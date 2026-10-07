(** Search - Iterative deepening principal variation search

    Negamax alpha-beta with a principal variation search window, transposition
    table, check extension, quiescence search (with check evasions), null move
    pruning, reverse futility pruning, razoring, futility pruning, late move
    pruning and late move reductions. Selective pruning is only applied at
    non-PV nodes and never when in check.

    Draws by repetition (against the game history and the search path), the
    fifty-move rule and insufficient material are detected inside the search.
    Time and stop requests are polled every 2048 nodes.

    All tables live in a [state] value that persists between moves of a game.
*)

open Chessml_core

(** Transposition table entry types *)
type tt_entry_type =
  | Exact
  | LowerBound
  | UpperBound

type tt_entry =
  { key : int64
  ; score : int
  ; depth : int
  ; best_move : Move.t option
  ; entry_type : tt_entry_type
  }

(** Transposition table: direct-mapped, keeps the deeper entry for the same key *)
module TranspositionTable = struct
  type t = { mutable table : tt_entry option array }

  let create size = { table = Array.make (max 1 size) None }

  let index tt key =
    Int64.to_int (Int64.unsigned_rem key (Int64.of_int (Array.length tt.table)))
  ;;

  let store tt key depth score best_move entry_type =
    let idx = index tt key in
    match tt.table.(idx) with
    | Some e when Int64.equal e.key key && e.depth > depth && entry_type <> Exact -> ()
    | _ -> tt.table.(idx) <- Some { key; score; depth; best_move; entry_type }
  ;;

  let lookup tt key =
    match tt.table.(index tt key) with
    | Some entry when Int64.equal entry.key key -> Some entry
    | _ -> None
  ;;

  let clear tt = Array.fill tt.table 0 (Array.length tt.table) None
  let resize tt size = tt.table <- Array.make (max 1 size) None
end

(** Approximate memory per TT entry in bytes (boxed record and key) *)
let tt_entry_bytes = 80

type state =
  { tt : TranspositionTable.t
  ; killers : Killers.killer_table
  ; history : History.t
  ; countermoves : Countermoves.t
  }

let create_state ?(hash_mb = 16) () =
  { tt = TranspositionTable.create (hash_mb * 1024 * 1024 / tt_entry_bytes)
  ; killers = Killers.create Score.max_ply
  ; history = History.create ()
  ; countermoves = Countermoves.create ()
  }
;;

(** Engine-wide default state used by the protocols *)
let default_state = create_state ()

(** Forget everything learned (new game) *)
let new_game ?(state = default_state) () =
  TranspositionTable.clear state.tt;
  Killers.clear state.killers;
  History.clear state.history;
  Countermoves.clear state.countermoves
;;

(** Resize (and clear) the transposition table *)
let set_hash_size_mb ?(state = default_state) mb =
  TranspositionTable.resize state.tt (max 1 mb * 1024 * 1024 / tt_entry_bytes)
;;

type search_result =
  { best_move : Move.t option
  ; score : int
  ; nodes : int64
  ; depth : int
  ; pv : Move.t list
  }

exception Stop

(** Per-search context *)
type ctx =
  { st : state
  ; stop : bool Atomic.t (** set by another thread to stop the search *)
  ; mutable nodes : int
  ; deadline : float option
  ; mutable can_stop : bool (** false until one iteration has completed *)
  ; keys : int64 array (** game history then search path, by ply *)
  ; root_index : int (** index of the root position in [keys] *)
  ; mutable root_best : Move.t option
  }

let poll ctx =
  ctx.nodes <- ctx.nodes + 1;
  if ctx.can_stop && ctx.nodes land 2047 = 0
  then
    if
      Atomic.get ctx.stop
      ||
      match ctx.deadline with
      | Some d -> Unix.gettimeofday () > d
      | None -> false
    then raise Stop
;;

(** Has the position at [ply] occurred before since the last irreversible move? *)
let is_repetition ctx pos ply =
  let idx = ctx.root_index + ply in
  let key = Position.key pos in
  let oldest = max 0 (idx - Position.halfmove pos) in
  let rec scan i = i >= oldest && (Int64.equal ctx.keys.(i) key || scan (i - 2)) in
  scan (idx - 4)
;;

let is_draw ctx pos ply =
  Position.halfmove pos >= 100
  || is_repetition ctx pos ply
  || Position.has_insufficient_material pos
;;

(** Quiescence search: resolve captures (and checks at the first ply) so the
    static evaluation is only taken in quiet positions *)
let rec quiescence ctx pos alpha beta ply qdepth =
  poll ctx;
  if ply >= Score.max_ply - 1
  then Eval.evaluate pos
  else if Movegen.in_check pos
  then (
    (* No stand pat in check: every evasion must be searched *)
    match Movegen.generate_moves pos with
    | [] -> Score.mated_in ply
    | moves ->
      search_quiescence_moves ctx pos moves (-Score.infinity) alpha beta ply qdepth)
  else (
    let stand_pat = Eval.evaluate pos in
    if stand_pat >= beta || qdepth >= Config.get_max_quiescence_depth ()
    then stand_pat
    else
      search_quiescence_moves
        ctx
        pos
        (Search_common.tactical_moves ~include_checks:(qdepth = 0) pos)
        stand_pat
        (max alpha stand_pat)
        beta
        ply
        qdepth)

and search_quiescence_moves ctx pos moves best alpha beta ply qdepth =
  let ordered =
    Search_common.Ordering.order_moves
      pos
      moves
      ~tt_move:None
      ~killer_check:(fun _ -> false)
      ~countermove_check:(fun _ -> false)
  in
  let rec loop best alpha = function
    | [] -> best
    | mv :: rest ->
      let score =
        -quiescence ctx (Position.make_move pos mv) (-beta) (-alpha) (ply + 1) (qdepth + 1)
      in
      if score >= beta then score else loop (max best score) (max alpha score) rest
  in
  loop best alpha ordered
;;

let store_tt ctx key depth score ply best_move entry_type =
  if Config.get_use_transposition_table ()
  then
    TranspositionTable.store
      ctx.st.tt
      key
      depth
      (Score.to_tt score ply)
      best_move
      entry_type
;;

(** Negamax alpha-beta (fail-soft) with principal variation search *)
let rec alphabeta ctx pos ~alpha ~beta ~depth ~ply ~prev_move ~null_ok =
  ctx.keys.(ctx.root_index + ply) <- Position.key pos;
  if ply > 0 && is_draw ctx pos ply
  then Score.draw
  else if ply >= Score.max_ply - 1
  then Eval.evaluate pos
  else (
    let in_check = Movegen.in_check pos in
    (* Check extension *)
    let depth = if in_check then depth + 1 else depth in
    if depth <= 0
    then
      if Config.get_use_quiescence ()
      then quiescence ctx pos alpha beta ply 0
      else Eval.evaluate pos
    else (
      poll ctx;
      (* Mate distance pruning *)
      let alpha = max alpha (Score.mated_in ply) in
      let beta = min beta (Score.mate - ply - 1) in
      if alpha >= beta
      then alpha
      else search_node ctx pos ~alpha ~beta ~depth ~ply ~prev_move ~null_ok ~in_check))

and search_node ctx pos ~alpha ~beta ~depth ~ply ~prev_move ~null_ok ~in_check =
  let pv_node = beta - alpha > 1 in
  let key = Position.key pos in
  let entry =
    if Config.get_use_transposition_table ()
    then TranspositionTable.lookup ctx.st.tt key
    else None
  in
  let tt_move = Option.bind entry (fun e -> e.best_move) in
  let tt_cutoff =
    match entry with
    | Some e when (not pv_node) && ply > 0 && e.depth >= depth ->
      let score = Score.of_tt e.score ply in
      (match e.entry_type with
       | Exact -> Some score
       | LowerBound when score >= beta -> Some score
       | UpperBound when score <= alpha -> Some score
       | _ -> None)
    | _ -> None
  in
  match tt_cutoff with
  | Some score -> score
  | None ->
    let side = Position.side_to_move pos in
    let static_eval = if in_check then -Score.infinity else Eval.evaluate pos in
    let can_prune = (not pv_node) && (not in_check) && abs beta < Score.mate_bound in
    (* Reverse futility pruning *)
    if
      can_prune
      && depth <= 4
      && static_eval - Search_common.reverse_futility_margin depth >= beta
    then static_eval
    else (
      (* Null move pruning: if passing still fails high, the position is too good *)
      let null_score =
        if
          can_prune
          && null_ok
          && depth >= 3
          && static_eval >= beta
          && Position.count_non_pawn_material pos side > 0
        then (
          let r = if depth > 6 then 3 else 2 in
          let score =
            -alphabeta
               ctx
               (Position.make_null_move pos)
               ~alpha:(-beta)
               ~beta:(-beta + 1)
               ~depth:(depth - 1 - r)
               ~ply:(ply + 1)
               ~prev_move:None
               ~null_ok:false
          in
          if score >= beta
          then Some (if Score.is_mate score then beta else score)
          else None)
        else None
      in
      match null_score with
      | Some score -> score
      | None ->
        (* Razoring: hopeless positions drop straight to quiescence *)
        let razored =
          if
            can_prune
            && depth <= 3
            && static_eval + Search_common.razor_margin depth < alpha
            && Config.get_use_quiescence ()
          then (
            let q = quiescence ctx pos alpha (alpha + 1) ply 0 in
            if q < alpha then Some q else None)
          else None
        in
        (match razored with
         | Some score -> score
         | None ->
           search_moves
             ctx
             pos
             ~alpha
             ~beta
             ~depth
             ~ply
             ~prev_move
             ~in_check
             ~pv_node
             ~static_eval
             ~tt_move
             ~key))

and search_moves
      ctx
      pos
      ~alpha
      ~beta
      ~depth
      ~ply
      ~prev_move
      ~in_check
      ~pv_node
      ~static_eval
      ~tt_move
      ~key
  =
  match Movegen.generate_moves pos with
  | [] -> if in_check then Score.mated_in ply else Score.draw
  | moves ->
    let st = ctx.st in
    let is_killer = Killers.is_killer st.killers ply in
    let ordered =
      Search_common.Ordering.order_moves
        ~history:st.history
        pos
        moves
        ~tt_move
        ~killer_check:is_killer
        ~countermove_check:(Countermoves.is_countermove st.countermoves prev_move)
    in
    let original_alpha = alpha in
    let futility =
      (not pv_node)
      && (not in_check)
      && depth <= 3
      && abs alpha < Score.mate_bound
      && static_eval + Search_common.futility_margin depth <= alpha
    in
    let search child ~alpha ~beta ~depth mv =
      -alphabeta
         ctx
         child
         ~alpha:(-beta)
         ~beta:(-alpha)
         ~depth
         ~ply:(ply + 1)
         ~prev_move:(Some mv)
         ~null_ok:true
    in
    let rec loop best_score best_move alpha move_count = function
      | [] -> best_score, best_move
      | mv :: rest ->
        let move_count = move_count + 1 in
        let child = Position.make_move pos mv in
        let quiet = not (Search_common.LMR.is_tactical_move mv) in
        let gives_check = quiet && Movegen.in_check child in
        let prunable = quiet && (not gives_check) && move_count > 1 && not in_check in
        if
          prunable
          && (not pv_node)
          && (futility || move_count > Search_common.late_move_count depth)
        then loop best_score best_move alpha move_count rest
        else (
          let score =
            if move_count = 1
            then search child ~alpha ~beta ~depth:(depth - 1) mv
            else (
              let reduction =
                if
                  depth >= 3
                  && (move_count > if pv_node then 5 else 3)
                  && prunable
                  && not (is_killer mv)
                then
                  Search_common.LMR.reduction
                    depth
                    move_count
                    ~is_pv:pv_node
                    ~history_score:(History.get_score st.history mv)
                else 0
              in
              let reduced_depth = max 0 (depth - 1 - reduction) in
              (* Null window search, possibly reduced *)
              let s = search child ~alpha ~beta:(alpha + 1) ~depth:reduced_depth mv in
              let s =
                if s > alpha && reduction > 0
                then search child ~alpha ~beta:(alpha + 1) ~depth:(depth - 1) mv
                else s
              in
              (* Full window re-search at PV nodes *)
              if s > alpha && s < beta && pv_node
              then search child ~alpha ~beta ~depth:(depth - 1) mv
              else s)
          in
          let best_score, best_move =
            if score > best_score then score, Some mv else best_score, best_move
          in
          if ply = 0 && score > alpha then ctx.root_best <- Some mv;
          if score >= beta
          then (
            if quiet
            then (
              Killers.store_killer st.killers ply mv;
              History.record_cutoff st.history mv depth;
              Option.iter (fun pm -> Countermoves.update st.countermoves pm mv) prev_move);
            best_score, best_move)
          else loop best_score best_move (max alpha score) move_count rest)
    in
    let best_score, best_move = loop (-Score.infinity) None alpha 0 ordered in
    let entry_type =
      if best_score >= beta
      then LowerBound
      else if best_score > original_alpha
      then Exact
      else UpperBound
    in
    store_tt ctx key depth best_score ply best_move entry_type;
    best_score
;;

(** Principal variation from the transposition table *)
let extract_pv st pos max_len =
  let rec follow pos n seen =
    let key = Position.key pos in
    if n = 0 || List.mem key seen
    then []
    else (
      match TranspositionTable.lookup st.tt key with
      | Some { best_move = Some mv; _ } when List.mem mv (Movegen.generate_moves pos) ->
        mv :: follow (Position.make_move pos mv) (n - 1) (key :: seen)
      | _ -> [])
  in
  follow pos max_len []
;;

(** Find the best move with iterative deepening up to [depth] plies.
    @param max_time_ms hard time limit; no new iteration starts after half of it
    @param on_iteration called after every completed iteration *)
let find_best_move
      ?(verbose = true)
      ?max_time_ms
      ?(state = default_state)
      ?(stop = Atomic.make false)
      ?(on_iteration = fun (_ : search_result) -> ())
      (game : Game.t)
      (depth : int)
  : search_result
  =
  let pos = Game.position game in
  let start = Unix.gettimeofday () in
  let history = Array.of_list (List.rev (Game.history game)) in
  let ctx =
    { st = state
    ; stop
    ; nodes = 0
    ; deadline = Option.map (fun ms -> start +. (float_of_int ms /. 1000.0)) max_time_ms
    ; can_stop = false
    ; keys = Array.append history (Array.make (Score.max_ply + 1) 0L)
    ; root_index = Array.length history - 1
    ; root_best = None
    }
  in
  Killers.clear state.killers;
  History.age state.history;
  let max_depth = min depth (Config.get_max_search_depth ()) in
  let half_time_passed () =
    match max_time_ms with
    | Some ms -> (Unix.gettimeofday () -. start) *. 1000.0 > float_of_int ms /. 2.0
    | None -> false
  in
  let rec iterate d best =
    if d > max_depth || (d > 1 && (half_time_passed () || Atomic.get ctx.stop))
    then best
    else (
      match
        alphabeta
          ctx
          pos
          ~alpha:(-Score.infinity)
          ~beta:Score.infinity
          ~depth:d
          ~ply:0
          ~prev_move:None
          ~null_ok:false
      with
      | exception Stop -> best
      | score ->
        ctx.can_stop <- true;
        let best_move =
          match ctx.root_best with
          | Some _ as mv -> mv
          | None -> List.nth_opt (Movegen.generate_moves pos) 0
        in
        let result =
          { best_move
          ; score
          ; nodes = Int64.of_int ctx.nodes
          ; depth = d
          ; pv =
              (match best_move with
               | Some mv -> mv :: extract_pv state (Position.make_move pos mv) (d - 1)
               | None -> [])
          }
        in
        if verbose
        then
          Printf.eprintf
            "Depth %d: %s, Score: %d, Nodes: %d, Time: %.3fs\n%!"
            d
            (Option.fold ~none:"none" ~some:Move.to_uci best_move)
            score
            ctx.nodes
            (Unix.gettimeofday () -. start);
        on_iteration result;
        (* A mate found within the searched depth cannot get shorter *)
        if Score.is_mate score && Score.mate - abs score <= d
        then result
        else iterate (d + 1) result)
  in
  match Movegen.generate_moves pos with
  | [] ->
    { best_move = None
    ; score = (if Movegen.in_check pos then Score.mated_in 0 else Score.draw)
    ; nodes = 0L
    ; depth = 0
    ; pv = []
    }
  | _ -> iterate 1 { best_move = None; score = 0; nodes = 0L; depth = 0; pv = [] }
;;
