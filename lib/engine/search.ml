(** Search - Alpha-beta minimax search with pruning and extensions
    
    Implements iterative deepening alpha-beta search with modern chess programming
    techniques: transposition tables, move ordering (hash/killer/history heuristics),
    quiescence search, null move pruning, late move reductions (LMR), futility pruning,
    and razoring. Supports time management and mate distance detection.
    
    Key features: PVS, aspiration windows (via TT), history heuristic with caps,
    killer moves, countermoves, and configurable search parameters
*)

open Chessml_core
open Types

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

(** Transposition table module *)
module TranspositionTable = struct
  type t =
    { table : tt_entry option array
    ; size : int
    }

  let create size = { table = Array.make size None; size }
  let index tt key = Int64.to_int (Int64.unsigned_rem key (Int64.of_int tt.size))

  let store tt key depth score best_move entry_type =
    let idx = index tt key in
    let entry = { key; score; depth; best_move; entry_type } in
    tt.table.(idx) <- Some entry
  ;;

  let lookup tt key =
    let idx = index tt key in
    match tt.table.(idx) with
    | Some entry when Int64.equal entry.key key -> Some entry
    | _ -> None
  ;;

  let clear tt = Array.fill tt.table 0 tt.size None
end

(** Global transposition table *)
let tt = TranspositionTable.create 1048576 (* 1M entries *)

(** Global countermove table *)
let countermove_table = Countermoves.create ()

type search_result =
  { best_move : Move.t option
  ; score : int
  ; nodes : int64
  ; depth : int
  }

(** Mate score constants *)
let mate_score = 100000

let mate_threshold = 90000

(** Check if a position is terminal (checkmate or stalemate) *)
let is_terminal (pos : Position.t) : bool * int =
  let moves = Movegen.generate_moves pos in
  if List.length moves = 0
  then (
    (* No legal moves - either checkmate or stalemate *)
    let side = Position.side_to_move pos in
    let opponent = Color.opponent side in
    let king_sq =
      match Movegen.find_king pos side with
      | Some sq -> sq
      | None -> 0 (* Should never happen *)
    in
    let attackers = Movegen.compute_attackers_to pos king_sq opponent in
    if Bitboard.is_empty attackers
    then true, 0 (* Stalemate - draw *)
    else true, -mate_score
    (* Checkmate - we lost *))
  else false, 0
;;

(** Helper function to increment int64 ref *)
let incr_int64 = Search_common.incr_int64

(** Quiescence search - search captures and checks to avoid horizon effect *)
let rec quiescence
          (pos : Position.t)
          (alpha : int)
          (beta : int)
          (nodes : int64 ref)
          ?(depth = 0)
          ()
  : int * Move.t option
  =
  incr_int64 nodes;
  (* Limit quiescence search depth based on configuration *)
  if depth >= Config.get_max_quiescence_depth ()
  then Eval.evaluate pos, None
  else (
    (* Stand pat evaluation *)
    let eval = Eval.evaluate pos in
    (* Beta cutoff *)
    if eval >= beta
    then beta, None
    else (
      (* Update alpha *)
      let alpha = max alpha eval in
      (* Generate captures, checks, and promotions using unified function *)
      let moves = Search_common.Quiescence.generate_tactical_moves pos in
      (* If no tactical moves, return stand-pat score *)
      if moves = []
      then eval, None
      else (
        (* Search tactical moves - use unified ordering *)
        let ordered_moves =
          Search_common.Ordering.order_moves
            pos
            moves
            ~tt_move:None
            ~killer_check:(fun _ -> false)
            ~countermove_check:(fun _ -> false)
        in
        let rec search_quiescence_moves moves alpha best_move =
          match moves with
          | [] -> alpha, best_move
          | mv :: rest ->
            let new_pos = Position.make_move pos mv in
            (* Use safe negation to avoid overflow *)
            let safe_neg_beta = if beta > 50000 then -50000 else -beta in
            let safe_neg_alpha = if alpha < -50000 then 50000 else -alpha in
            let score, _ =
              quiescence new_pos safe_neg_beta safe_neg_alpha nodes ~depth:(depth + 1) ()
            in
            let score = -score in
            if score >= beta
            then
              (* Beta cutoff *)
              score, Some mv
            else (
              let new_alpha = max alpha score in
              let new_best = if score > alpha then Some mv else best_move in
              search_quiescence_moves rest new_alpha new_best)
        in
        search_quiescence_moves ordered_moves alpha None)))
;;

(** Alpha-beta search implementation with node counting and transposition table *)
let rec alphabeta
          (pos : Position.t)
          (alpha : int)
          (beta : int)
          (depth : int)
          (nodes : int64 ref)
          (prev_move : Move.t option)
  : int * Move.t option
  =
  (* Increment node counter *)
  incr_int64 nodes;
  (* Get position hash for transposition table *)
  let pos_hash = Zobrist.compute pos in
  (* Check transposition table if enabled *)
  if Config.get_use_transposition_table ()
  then (
    let tt_move = ref None in
    match TranspositionTable.lookup tt pos_hash with
    | Some entry when entry.depth >= depth ->
      tt_move := entry.best_move;
      (match entry.entry_type with
       | Exact ->
         (* Exact score, return immediately *)
         entry.score, entry.best_move
       | LowerBound when entry.score >= beta ->
         (* Beta cutoff *)
         entry.score, entry.best_move
       | UpperBound when entry.score <= alpha ->
         (* Alpha cutoff *)
         entry.score, entry.best_move
       | _ ->
         (* Continue search but use TT move for ordering *)
         search_position pos depth alpha beta nodes pos_hash !tt_move prev_move)
    | Some entry ->
      (* Entry exists but insufficient depth, use move for ordering *)
      tt_move := entry.best_move;
      search_position pos depth alpha beta nodes pos_hash !tt_move prev_move
    | None ->
      (* No entry found *)
      search_position pos depth alpha beta nodes pos_hash None prev_move)
  else
    (* Transposition table disabled *)
    search_position pos depth alpha beta nodes pos_hash None prev_move

(** Helper function for the main search logic *)
and search_position pos depth alpha beta nodes pos_hash tt_move prev_move =
  (* Check for terminal position *)
  let moves = Movegen.generate_moves pos in
  match Search_common.Terminal.check pos moves with
  | Some score -> score, None
  | None ->
    if depth = 0
    then
      (* Leaf node - use quiescence search if enabled *)
      if Config.get_use_quiescence ()
      then quiescence pos alpha beta nodes ()
      else Eval.evaluate pos, None
    else (
      (* Null move pruning - try passing turn to opponent *)
      let in_check = Search_common.is_in_check pos in
      let null_move_result =
        if Search_common.NullMove.can_prune pos depth beta in_check
        then (
          let null_pos = Search_common.NullMove.make_null_move pos in
          let null_depth = Search_common.NullMove.search_depth depth in
          let safe_neg_beta = if beta > 50000 then -50000 else -beta in
          let safe_neg_alpha = if beta - 1 < -50000 then 50000 else -(beta - 1) in
          let null_score, _ =
            alphabeta null_pos safe_neg_beta safe_neg_alpha null_depth nodes None
          in
          let null_score = -null_score in
          if null_score >= beta then Some beta (* Null move cutoff *) else None)
        else None
      in
      (* Return if null move caused cutoff *)
      match null_move_result with
      | Some score -> score, None
      | None ->
        (* Evaluate position once for pruning decisions *)
        let static_eval = Eval.evaluate pos in
        (* Reverse futility pruning: if we're way ahead, prune *)
        if Search_common.ReverseFutility.can_prune pos depth beta static_eval in_check
        then beta, None
        else (
          (* Razoring: if eval + margin < alpha, drop to quiescence *)
          let razor_result =
            if Search_common.Razoring.can_try_razor pos depth alpha in_check
            then
              if Search_common.Razoring.should_razor static_eval depth alpha
              then
                (* Try quiescence to see if we're really this bad *)
                if Config.get_use_quiescence ()
                then (
                  let qscore, _ = quiescence pos alpha (alpha + 1) nodes () in
                  if qscore < alpha
                  then Some (qscore, None) (* Confirmed bad, return early *)
                  else None (* Not as bad as expected, continue search *))
                else None
              else None
            else None
          in
          match razor_result with
          | Some result -> result
          | None ->
            (* Futility pruning using shared logic *)
            let can_futility_prune =
              Search_common.can_futility_prune depth static_eval alpha
            in
            (* Generate and order moves *)
            let moves = Movegen.generate_moves pos in
            let moves =
              order_moves_with_tt_and_killers pos moves tt_move depth prev_move
            in
            (* Search through moves *)
            let original_alpha = alpha in
            let move_count = ref 0 in
            let rec search_moves moves alpha best_move =
              match moves with
              | [] ->
                (* Store in transposition table if enabled *)
                if Config.get_use_transposition_table ()
                then (
                  let entry_type =
                    if alpha <= original_alpha
                    then UpperBound
                    else if alpha >= beta
                    then LowerBound
                    else Exact
                  in
                  TranspositionTable.store tt pos_hash depth alpha best_move entry_type);
                alpha, best_move
              | mv :: rest ->
                move_count := !move_count + 1;
                (* Futility pruning: skip quiet moves if position is hopeless *)
                let move_gives_check = Search_common.Ordering.gives_check_fast pos mv in
                let should_prune_futility =
                  Search_common.should_prune_move
                    can_futility_prune
                    mv
                    ~gives_check:move_gives_check
                in
                (* Late Move Pruning: skip late quiet moves *)
                let is_tactical =
                  Search_common.LMR.is_tactical_move mv || move_gives_check
                in
                let improving = false in
                (* TODO: Track eval change from previous ply *)
                let should_prune_lmp =
                  Search_common.LateMove.should_prune
                    depth
                    !move_count
                    improving
                    in_check
                    is_tactical
                in
                if should_prune_futility || should_prune_lmp
                then
                  (* Skip this move, continue with rest *)
                  search_moves rest alpha best_move
                else (
                  (* Late Move Reduction (LMR) using logarithmic formula *)
                  let can_reduce =
                    Search_common.LMR.can_reduce depth !move_count is_tactical alpha
                  in
                  (* Get history score for this move *)
                  let history_score = History.get_score mv in
                  (* Determine if this is a PV node (approximation: alpha has been raised) *)
                  let is_pv = alpha <> original_alpha in
                  let reduction =
                    if can_reduce
                    then
                      Search_common.LMR.reduction
                        depth
                        !move_count
                        ~is_pv
                        ~improving
                        ~gives_check:move_gives_check
                        ~history_score
                    else 0
                  in
                  (* Make move and search *)
                  let new_pos = Position.make_move pos mv in
                  (* Use safe negation to avoid overflow *)
                  let safe_neg_beta = if beta > 50000 then -50000 else -beta in
                  let safe_neg_alpha = if alpha < -50000 then 50000 else -alpha in
                  (* Try reduced depth search first *)
                  let score, re_search =
                    if reduction > 0
                    then (
                      let reduced_depth = max 1 (depth - 1 - reduction) in
                      let reduced_score, _ =
                        alphabeta
                          new_pos
                          safe_neg_beta
                          safe_neg_alpha
                          reduced_depth
                          nodes
                          (Some mv)
                      in
                      let reduced_score = -reduced_score in
                      (* If reduced search fails high, re-search at full depth *)
                      if reduced_score > alpha
                      then reduced_score, true
                      else reduced_score, false)
                    else alpha, true
                    (* No reduction, do full search *)
                  in
                  (* Full depth search if needed *)
                  let final_score =
                    if re_search
                    then (
                      let full_score, _ =
                        alphabeta
                          new_pos
                          safe_neg_beta
                          safe_neg_alpha
                          (depth - 1)
                          nodes
                          (Some mv)
                      in
                      -full_score)
                    else score
                  in
                  if final_score >= beta
                  then (
                    (* Beta cutoff - store as lower bound if TT enabled *)
                    if Config.get_use_transposition_table ()
                    then
                      TranspositionTable.store
                        tt
                        pos_hash
                        depth
                        final_score
                        (Some mv)
                        LowerBound;
                    (* Store as killer move if it's not a capture *)
                    Search_common.AlphaBeta.handle_beta_cutoff
                      mv
                      depth
                      Killers.store_global_killer
                      History.record_cutoff;
                    (* Store countermove if we have a previous move *)
                    (match prev_move with
                     | Some pm -> Countermoves.update countermove_table pm mv
                     | None -> ());
                    final_score, Some mv)
                  else (
                    let new_alpha, new_best =
                      Search_common.AlphaBeta.update_alpha_and_best
                        alpha
                        final_score
                        best_move
                        mv
                    in
                    search_moves rest new_alpha new_best))
            in
            search_moves moves alpha None))

(** Order moves with transposition table move first, then killers, then others *)
and order_moves_with_tt_and_killers
      (pos : Position.t)
      (moves : Move.t list)
      (tt_move : Move.t option)
      (depth : int)
      (prev_move : Move.t option)
  : Move.t list
  =
  (* Use shared move ordering logic from Search_common.Ordering for consistency *)
  let is_killer mv = Killers.is_global_killer depth mv in
  let is_countermove mv = Countermoves.is_countermove countermove_table prev_move mv in
  Search_common.Ordering.order_moves
    pos
    moves
    ~tt_move
    ~killer_check:is_killer
    ~countermove_check:is_countermove

(** Order moves with transposition table move first (legacy function for compatibility) *)
and order_moves_with_tt (pos : Position.t) (moves : Move.t list) (tt_move : Move.t option)
  : Move.t list
  =
  order_moves_with_tt_and_killers pos moves tt_move 0 None
;;

(** Find the best move for the current position *)
let find_best_move ?(verbose = true) ?max_time_ms (game : Game.t) (depth : int)
  : search_result
  =
  let pos = Game.position game in
  let start_time = Unix.gettimeofday () in
  let total_nodes = ref 0L in
  (* Convert max_time_ms to seconds for comparison *)
  let time_limit =
    match max_time_ms with
    | Some ms -> Some (float_of_int ms /. 1000.0)
    | None -> None
  in
  (* Clear transposition table for new search *)
  (* Note: In a real engine, you might want to preserve entries between moves *)

  (* Clear killer moves for new search *)
  Killers.clear_global ();
  (* Age history table (don't clear completely - keep accumulated knowledge) *)
  History.age_table ();
  (* Iterative deepening: search from depth 1 to target depth *)
  let best_result = ref None in
  (try
     for current_depth = 1 to min depth (Config.get_max_search_depth ()) do
       let depth_nodes = ref 0L in
       let depth_start = Unix.gettimeofday () in
       let score, best_move =
         alphabeta
           pos
           Search_common.AlphaBeta.initial_alpha
           Search_common.AlphaBeta.initial_beta
           current_depth
           depth_nodes
           None
       in
       let depth_end = Unix.gettimeofday () in
       let depth_elapsed = depth_end -. depth_start in
       (* Update total node count *)
       total_nodes := Int64.add !total_nodes !depth_nodes;
       (* Store result for this depth *)
       let current_result =
         { best_move; score; nodes = !total_nodes; depth = current_depth }
       in
       best_result := Some current_result;
       (* Check time limit *)
       (match time_limit with
        | Some limit ->
          let current_time = Unix.gettimeofday () in
          let elapsed = current_time -. start_time in
          if elapsed >= limit
          then (
            if verbose
            then (
              Printf.eprintf
                "Time limit reached (%.3fs), stopping search at depth %d\n"
                elapsed
                current_depth;
              flush stderr);
            raise Exit)
        | None -> ());
       if verbose
       then (
         Printf.eprintf
           "Depth %d: %s, Score: %d, Nodes: %Ld, Time: %.3fs\n"
           current_depth
           (match best_move with
            | Some mv -> Move.to_uci mv
            | None -> "none")
           score
           !depth_nodes
           depth_elapsed;
         flush stderr);
       (* Early termination for mate *)
       if abs score >= mate_threshold
       then (
         if verbose
         then (
           Printf.eprintf "Mate found at depth %d, stopping search\n" current_depth;
           flush stderr);
         (* Convert mate score to mate-in-X *)
         let mate_in = ((mate_score - abs score) / 2) + 1 in
         if verbose
         then (
           Printf.eprintf "Mate in %d moves\n" mate_in;
           flush stderr);
         (* Return result with mate score *)
         let final_result = { current_result with score } in
         best_result := Some final_result;
         (* Break out of loop early *)
         raise Exit)
     done
   with
   | Exit -> ());
  let end_time = Unix.gettimeofday () in
  let total_elapsed = end_time -. start_time in
  match !best_result with
  | Some result ->
    if verbose
    then (
      Printf.eprintf "Search completed in %.3f seconds\n" total_elapsed;
      Printf.eprintf
        "Total nodes: %Ld (%.0f nodes/sec)\n"
        !total_nodes
        (Int64.to_float !total_nodes /. total_elapsed);
      flush stderr);
    result
  | None ->
    (* This should never happen, but provide a fallback *)
    { best_move = None; score = 0; nodes = !total_nodes; depth = 0 }
;;
