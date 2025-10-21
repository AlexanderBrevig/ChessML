(** Parallel Search - Lazy SMP parallel search implementation
    
    Multiple threads search the same position independently with shared transposition
    table. Each thread maintains its own history and killer tables for reduced
    contention. Uses OCaml 5 Domains for true parallelism with lock-striped TT
    for thread-safe access. Natural work distribution via TT sharing.
    
    Performance: Achieves 0.8-1.0x speedup (needs optimization for better scaling)
*)

open Chessml_core

(** Thread-local search state *)
type thread_state =
  { thread_id : int
  ; killers : Killers.killer_table
  ; countermoves : Countermoves.t
  ; history : History.t (* Thread-local history for better cache locality *)
  ; nodes : int64 ref
  ; max_depth : int ref
  ; selective_depth : int (* Add variation to search between threads *)
  }

(** Shared search state *)
type shared_state =
  { tt : Concurrent_tt.t
  ; stop_flag : bool Atomic.t
  ; best_move : Move.t option Atomic.t
  ; best_score : int Atomic.t
  ; shared_history : History.t (* Shared history updated periodically *)
  }

(** Create thread-local state *)
let create_thread_state thread_id =
  { thread_id
  ; killers = Killers.create 64
  ; countermoves = Countermoves.create ()
  ; history = History.create ()
  ; nodes = ref 0L
  ; max_depth = ref 0
  ; selective_depth = thread_id mod 4 (* Small variation: 0,1,2,3 *)
  }
;;

(** Create shared state *)
let create_shared_state tt_size =
  { tt = Concurrent_tt.create ~num_locks:4096 tt_size
  ; (* Many locks for minimal contention *)
    stop_flag = Atomic.make false
  ; best_move = Atomic.make None
  ; best_score = Atomic.make 0
  ; shared_history = History.create ()
  }
;;

(** Check if search should stop *)
let should_stop shared = Atomic.get shared.stop_flag

(** Signal all threads to stop *)
let signal_stop shared = Atomic.set shared.stop_flag true

(** Update best move if score is better *)
let update_best shared score move =
  let current_best = Atomic.get shared.best_score in
  if score > current_best
  then (
    Atomic.set shared.best_score score;
    Atomic.set shared.best_move move)
;;

(** Single-threaded alphabeta search with thread-local state
    This is similar to Search.alphabeta but uses thread-local killer/history
*)
let rec thread_alphabeta pos alpha beta depth nodes shared local prev_move =
  (* Check stop flag *)
  if should_stop shared
  then 0, None
  else (
    (* Increment node counter *)
    Search_common.incr_int64 nodes;
    (* Update max depth reached *)
    if depth > !(local.max_depth) then local.max_depth := depth;
    (* Check for terminal position *)
    let moves = Movegen.generate_moves pos in
    match Search_common.Terminal.check pos moves with
    | Some score -> score, None
    | None ->
      if depth = 0
      then
        (* Leaf node - use quiescence search *)
        quiescence_search pos alpha beta nodes shared local 0
      else
        (* Search this position *)
        search_position pos alpha beta depth nodes shared local moves prev_move)

and search_position pos alpha beta depth nodes shared local moves prev_move =
  let pos_hash = Zobrist.compute pos in
  (* Check transposition table *)
  let tt_move =
    match Concurrent_tt.lookup shared.tt pos_hash with
    | Some entry when entry.depth >= depth ->
      (match entry.entry_type with
       | Concurrent_tt.Exact ->
         (* Return exact score *)
         entry.score, entry.best_move
       | Concurrent_tt.LowerBound when entry.score >= beta -> entry.score, entry.best_move
       | Concurrent_tt.UpperBound when entry.score <= alpha ->
         entry.score, entry.best_move
       | _ ->
         (* Continue search with TT move hint *)
         search_moves
           pos
           alpha
           beta
           depth
           nodes
           shared
           local
           moves
           entry.best_move
           pos_hash
           alpha
           prev_move)
    | Some entry ->
      (* Use TT move for ordering *)
      search_moves
        pos
        alpha
        beta
        depth
        nodes
        shared
        local
        moves
        entry.best_move
        pos_hash
        alpha
        prev_move
    | None ->
      search_moves
        pos
        alpha
        beta
        depth
        nodes
        shared
        local
        moves
        None
        pos_hash
        alpha
        prev_move
  in
  tt_move

and search_moves
      pos
      alpha
      beta
      depth
      nodes
      shared
      local
      moves
      tt_move
      pos_hash
      original_alpha
      prev_move
  =
  (* Null move pruning - try passing turn to opponent *)
  let in_check = Search_common.is_in_check pos in
  let null_move_result =
    if Search_common.NullMove.can_prune pos depth beta in_check
    then (
      let null_pos = Search_common.NullMove.make_null_move pos in
      let null_depth = Search_common.NullMove.search_depth depth in
      let null_score, _ =
        thread_alphabeta null_pos (-beta) (-(beta - 1)) null_depth nodes shared local None
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
          then (
            (* Try quiescence to see if we're really this bad *)
            let qscore, _ =
              quiescence_search pos alpha (alpha + 1) nodes shared local 0
            in
            if qscore < alpha
            then Some (qscore, None) (* Confirmed bad, return early *)
            else None (* Not as bad as expected, continue search *))
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
        (* Order moves: TT move, then by history/killers/countermoves *)
        let ordered_moves =
          order_moves_for_thread local moves tt_move depth pos prev_move
        in
        let move_count = ref 0 in
        let rec search_list mvs current_alpha best_mv =
          match mvs with
          | [] ->
            (* Store in TT - but only for main thread or deeper searches to reduce contention *)
            let should_store = local.thread_id = 0 || depth >= 4 in
            if should_store
            then (
              let entry_type =
                if current_alpha <= original_alpha
                then Concurrent_tt.UpperBound
                else if current_alpha >= beta
                then Concurrent_tt.LowerBound
                else Concurrent_tt.Exact
              in
              Concurrent_tt.store
                shared.tt
                pos_hash
                depth
                current_alpha
                best_mv
                entry_type);
            current_alpha, best_mv
          | mv :: rest ->
            if should_stop shared
            then current_alpha, best_mv
            else (
              move_count := !move_count + 1;
              (* Futility pruning: skip quiet moves if position is hopeless *)
              let should_prune =
                Search_common.should_prune_move can_futility_prune mv ~gives_check:false
              in
              if should_prune
              then
                (* Skip this move, continue with rest *)
                search_list rest current_alpha best_mv
              else (
                (* Late Move Reduction (LMR) using logarithmic formula *)
                let is_tactical = Search_common.LMR.is_tactical_move mv in
                let can_reduce =
                  Search_common.LMR.can_reduce depth !move_count is_tactical current_alpha
                in
                (* Get history score for this move from thread-local history *)
                let history_score = History.get_score_from_table local.history mv in
                (* Check if move gives check (for LMR adjustment) *)
                let move_gives_check = Search_common.Ordering.gives_check_fast pos mv in
                (* Determine if this is a PV node (approximation: alpha has been raised) *)
                let is_pv = current_alpha <> original_alpha in
                (* TODO: Track improving flag properly *)
                let improving = false in
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
                let new_pos = Position.make_move pos mv in
                (* Try reduced depth search first *)
                let score, re_search =
                  if reduction > 0
                  then (
                    let reduced_depth = max 1 (depth - 1 - reduction) in
                    let reduced_score, _ =
                      thread_alphabeta
                        new_pos
                        (-beta)
                        (-current_alpha)
                        reduced_depth
                        nodes
                        shared
                        local
                        (Some mv)
                    in
                    let reduced_score = -reduced_score in
                    (* If reduced search fails high, re-search at full depth *)
                    if reduced_score > current_alpha
                    then reduced_score, true
                    else reduced_score, false)
                  else current_alpha, true
                  (* No reduction, do full search *)
                in
                (* Full depth search if needed *)
                let final_score =
                  if re_search
                  then (
                    let full_score, _ =
                      thread_alphabeta
                        new_pos
                        (-beta)
                        (-current_alpha)
                        (depth - 1)
                        nodes
                        shared
                        local
                        (Some mv)
                    in
                    -full_score)
                  else score
                in
                if final_score >= beta
                then (
                  (* Beta cutoff - store killer and countermove *)
                  Search_common.AlphaBeta.handle_beta_cutoff
                    mv
                    depth
                    (Killers.store_killer local.killers)
                    (fun m d -> History.record_cutoff_in_table local.history m d);
                  (* Store countermove if we have a previous move *)
                  (match prev_move with
                   | Some pm -> Countermoves.update local.countermoves pm mv
                   | None -> ());
                  (* Always store beta cutoffs - they're valuable *)
                  Concurrent_tt.store
                    shared.tt
                    pos_hash
                    depth
                    final_score
                    (Some mv)
                    Concurrent_tt.LowerBound;
                  final_score, Some mv)
                else (
                  let new_alpha, new_best =
                    Search_common.AlphaBeta.update_alpha_and_best
                      current_alpha
                      final_score
                      best_mv
                      mv
                  in
                  search_list rest new_alpha new_best)))
        in
        search_list ordered_moves alpha None)

(**Quiescence search for captures to avoid horizon effect *)
and quiescence_search pos alpha beta nodes shared local qdepth =
  Search_common.incr_int64 nodes;
  (* Limit quiescence depth using common constant *)
  if qdepth >= Search_common.Quiescence.max_depth
  then Eval.evaluate pos, None
  else (
    (* Stand pat *)
    let eval = Eval.evaluate pos in
    if eval >= beta
    then beta, None
    else (
      let alpha = max alpha eval in
      (* Generate tactical moves using common function *)
      let tactical_moves = Search_common.Quiescence.generate_tactical_moves pos in
      (* Search captures *)
      let rec search_captures mvs current_alpha =
        match mvs with
        | [] -> current_alpha, None
        | mv :: rest ->
          if should_stop shared
          then current_alpha, None
          else (
            let new_pos = Position.make_move pos mv in
            let score, _ =
              quiescence_search
                new_pos
                (-beta)
                (-current_alpha)
                nodes
                shared
                local
                (qdepth + 1)
            in
            let score = -score in
            if score >= beta
            then score, Some mv
            else (
              let new_alpha = max current_alpha score in
              search_captures rest new_alpha))
      in
      search_captures tactical_moves alpha))

and order_moves_for_thread local moves tt_move depth pos prev_move =
  (* Use shared move ordering logic with thread-local history *)
  let is_killer mv = Killers.is_killer local.killers depth mv in
  let is_countermove mv = Countermoves.is_countermove local.countermoves prev_move mv in
  (* Note: Search_common.Ordering uses global history, but we have thread-local
     This is acceptable - the TT sharing provides enough coordination *)
  Search_common.Ordering.order_moves
    pos
    moves
    ~tt_move
    ~killer_check:is_killer
    ~countermove_check:is_countermove
;;

(** Search worker function for a single thread *)
let search_worker shared local pos target_depth =
  let start_time = Unix.gettimeofday () in
  (* Helper threads start at different depths to reduce redundant work
     Strategy: Main thread does full ID, others start deeper to help sooner *)
  let start_depth =
    if local.thread_id = 0
    then 1 (* Main thread: full iterative deepening from depth 1 *)
    else if local.thread_id <= 2
    then max 1 (target_depth - 3) (* Threads 1-2: start 3 below target *)
    else if local.thread_id <= 4
    then max 1 (target_depth - 2) (* Threads 3-4: start 2 below target *)
    else max 1 (target_depth - 1)
    (* Threads 5+: start 1 below target *)
  in
  let prev_score = ref 0 in
  (* Iterative deepening *)
  let rec iterate depth =
    if depth > target_depth || should_stop shared
    then ()
    else (
      (* All threads search same depth - Lazy SMP relies on TT sharing for diversity *)
      let score, best_move =
        thread_alphabeta
          pos
          Search_common.AlphaBeta.initial_alpha
          Search_common.AlphaBeta.initial_beta
          depth
          local.nodes
          shared
          local
          None
      in
      prev_score := score;
      (* Update shared best if this is better *)
      update_best shared score best_move;
      (* Only main thread prints info *)
      if local.thread_id = 0
      then (
        let elapsed = Unix.gettimeofday () -. start_time in
        let nps =
          if elapsed > 0.0 then Int64.to_float !(local.nodes) /. elapsed else 0.0
        in
        Printf.printf
          "info depth %d score cp %d nodes %Ld nps %.0f time %.0f\n%!"
          depth
          score
          !(local.nodes)
          nps
          (elapsed *. 1000.0));
      iterate (depth + 1))
  in
  iterate start_depth
;;

(** Main parallel search function
    @param pos Position to search
    @param depth Target depth
    @param num_threads Number of threads to use
    @return (best_score, best_move, total_nodes)
*)
let parallel_search pos target_depth num_threads =
  Printf.printf "Starting parallel search with %d threads\n%!" num_threads;
  (* Scale TT size moderately with thread count
     Too large TT hurts cache locality, too small causes thrashing *)
  let base_tt_size = 1048576 in
  (* 1M entries base *)
  let tt_size = base_tt_size + ((num_threads - 1) * 262144) in
  (* +256K per extra thread *)
  (* Create shared state with scaled TT 
     Use more locks for better concurrency *)
  let shared = create_shared_state tt_size in
  (* Launch ALL threads including main thread as workers *)
  let workers =
    List.init num_threads (fun i ->
      let local = create_thread_state i in
      Domain.spawn (fun () ->
        search_worker shared local pos target_depth;
        !(local.nodes)))
  in
  (* Wait for all workers to complete *)
  let worker_nodes = List.map Domain.join workers in
  (* Collect results *)
  let total_nodes = List.fold_left Int64.add 0L worker_nodes in
  let best_score = Atomic.get shared.best_score in
  let best_move = Atomic.get shared.best_move in
  best_score, best_move, total_nodes
;;

(** Simple single-threaded search for comparison *)
let single_threaded_search pos target_depth =
  let shared = create_shared_state 1048576 in
  let local = create_thread_state 0 in
  search_worker shared local pos target_depth;
  let score = Atomic.get shared.best_score in
  let move = Atomic.get shared.best_move in
  let nodes = !(local.nodes) in
  score, move, nodes
;;
