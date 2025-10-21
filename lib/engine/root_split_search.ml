(** Root Split Search - Parallel search via root move distribution
    
    Divides root moves among threads for embarrassingly parallel search.
    Each thread searches its assigned moves with independent TT and heuristic
    tables (zero contention). Main thread aggregates results to find best move.
    
    Advantages: Simple, no synchronization overhead, perfect for OCaml 5 Domains
    Tradeoff: Less sharing of search information between threads vs Lazy SMP
    - Embarrassingly parallel
    - Perfect cache locality
    - Expected 1.8-1.9x speedup with 2 threads, 3.5-3.8x with 4 threads
*)

open Chessml_core

type search_result =
  { best_move : Move.t
  ; best_score : int
  ; nodes : int64
  ; depth : int
  }

(** Thread-local search state - each thread has its own infrastructure *)
type thread_local_state =
  { thread_id : int
  ; tt : Search.TranspositionTable.t (* Private TT - no contention! *)
  ; killers : Killers.killer_table
  ; countermoves : Countermoves.t
  ; history : History.t
  ; nodes : int64 ref
  }

(** Create thread-local state with private TT *)
let create_thread_state thread_id tt_size =
  { thread_id
  ; tt = Search.TranspositionTable.create tt_size
  ; killers = Killers.create 64
  ; countermoves = Countermoves.create ()
  ; history = History.create ()
  ; nodes = ref 0L
  }
;;

(** Thread-local alphabeta search - similar to parallel_search but simpler *)
let rec thread_alphabeta pos alpha beta depth local prev_move =
  (* Increment node counter *)
  Search_common.incr_int64 local.nodes;
  (* Check transposition table *)
  let pos_hash = Zobrist.compute pos in
  match Search.TranspositionTable.lookup local.tt pos_hash with
  | Some entry when entry.depth >= depth ->
    (match entry.entry_type with
     | Search.Exact -> entry.score, entry.best_move
     | Search.LowerBound when entry.score >= beta -> entry.score, entry.best_move
     | Search.UpperBound when entry.score <= alpha -> entry.score, entry.best_move
     | _ -> search_position pos alpha beta depth local pos_hash entry.best_move prev_move)
  | Some entry ->
    search_position pos alpha beta depth local pos_hash entry.best_move prev_move
  | None -> search_position pos alpha beta depth local pos_hash None prev_move

and search_position pos alpha beta depth local pos_hash tt_move prev_move =
  (* Check for terminal position *)
  let moves = Movegen.generate_moves pos in
  match Search_common.Terminal.check pos moves with
  | Some score -> score, None
  | None ->
    if depth = 0
    then
      (* Leaf node - use quiescence search *)
      quiescence_search pos alpha beta local 0
    else (
      (* Null move pruning *)
      let in_check = Search_common.is_in_check pos in
      let null_move_result =
        if Search_common.NullMove.can_prune pos depth beta in_check
        then (
          let null_pos = Search_common.NullMove.make_null_move pos in
          let null_depth = Search_common.NullMove.search_depth depth in
          let null_score, _ =
            thread_alphabeta null_pos (-beta) (-(beta - 1)) null_depth local None
          in
          let null_score = -null_score in
          if null_score >= beta then Some beta else None)
        else None
      in
      match null_move_result with
      | Some score -> score, None
      | None ->
        (* Evaluate for pruning *)
        let static_eval = Eval.evaluate pos in
        (* Reverse futility pruning *)
        if Search_common.ReverseFutility.can_prune pos depth beta static_eval in_check
        then beta, None
        else
          (* Order and search moves *)
          search_moves
            pos
            alpha
            beta
            depth
            local
            moves
            tt_move
            pos_hash
            prev_move
            static_eval
            in_check)

and search_moves
      pos
      alpha
      beta
      depth
      local
      moves
      tt_move
      pos_hash
      prev_move
      static_eval
      _in_check
  =
  let original_alpha = alpha in
  (* Futility pruning setup *)
  let can_futility_prune = Search_common.can_futility_prune depth static_eval alpha in
  (* Order moves *)
  let is_killer mv = Killers.is_killer local.killers depth mv in
  let is_countermove mv = Countermoves.is_countermove local.countermoves prev_move mv in
  let ordered_moves =
    Search_common.Ordering.order_moves
      pos
      moves
      ~tt_move
      ~killer_check:is_killer
      ~countermove_check:is_countermove
  in
  let move_count = ref 0 in
  let rec search_list mvs current_alpha best_mv =
    match mvs with
    | [] ->
      (* Store in TT *)
      let entry_type =
        if current_alpha <= original_alpha
        then Search.UpperBound
        else if current_alpha >= beta
        then Search.LowerBound
        else Search.Exact
      in
      Search.TranspositionTable.store
        local.tt
        pos_hash
        depth
        current_alpha
        best_mv
        entry_type;
      current_alpha, best_mv
    | mv :: rest ->
      move_count := !move_count + 1;
      (* Futility pruning *)
      let should_prune =
        Search_common.should_prune_move can_futility_prune mv ~gives_check:false
      in
      if should_prune
      then search_list rest current_alpha best_mv
      else (
        (* Late Move Reduction *)
        let is_tactical = Search_common.LMR.is_tactical_move mv in
        let can_reduce =
          Search_common.LMR.can_reduce depth !move_count is_tactical current_alpha
        in
        let history_score = History.get_score_from_table local.history mv in
        let move_gives_check = Search_common.Ordering.gives_check_fast pos mv in
        let is_pv = current_alpha <> original_alpha in
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
        (* Try reduced search *)
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
                local
                (Some mv)
            in
            let reduced_score = -reduced_score in
            if reduced_score > current_alpha
            then reduced_score, true
            else reduced_score, false)
          else current_alpha, true
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
                local
                (Some mv)
            in
            -full_score)
          else score
        in
        if final_score >= beta
        then (
          (* Beta cutoff *)
          Search_common.AlphaBeta.handle_beta_cutoff
            mv
            depth
            (Killers.store_killer local.killers)
            (fun m d -> History.record_cutoff_in_table local.history m d);
          (match prev_move with
           | Some pm -> Countermoves.update local.countermoves pm mv
           | None -> ());
          Search.TranspositionTable.store
            local.tt
            pos_hash
            depth
            final_score
            (Some mv)
            Search.LowerBound;
          final_score, Some mv)
        else (
          let new_alpha, new_best =
            Search_common.AlphaBeta.update_alpha_and_best
              current_alpha
              final_score
              best_mv
              mv
          in
          search_list rest new_alpha new_best))
  in
  search_list ordered_moves alpha None

and quiescence_search pos alpha beta local qdepth =
  Search_common.incr_int64 local.nodes;
  if qdepth >= Search_common.Quiescence.max_depth
  then Eval.evaluate pos, None
  else (
    let eval = Eval.evaluate pos in
    if eval >= beta
    then beta, None
    else (
      let alpha = max alpha eval in
      let tactical_moves = Search_common.Quiescence.generate_tactical_moves pos in
      let rec search_captures mvs current_alpha =
        match mvs with
        | [] -> current_alpha, None
        | mv :: rest ->
          let new_pos = Position.make_move pos mv in
          let score, _ =
            quiescence_search new_pos (-beta) (-current_alpha) local (qdepth + 1)
          in
          let score = -score in
          if score >= beta
          then score, Some mv
          else (
            let new_alpha = max current_alpha score in
            search_captures rest new_alpha)
      in
      search_captures tactical_moves alpha))
;;

(** Search a subset of root moves with iterative deepening *)
let search_root_moves pos moves target_depth thread_id : search_result =
  (* Create thread-local state with private TT (512K entries per thread for better hit rate) *)
  let local = create_thread_state thread_id 524288 in
  (* Store move scores for ordering *)
  let move_scores = Hashtbl.create (List.length moves) in
  (* Iterative deepening from depth 1 to target_depth *)
  let rec iterate_depth current_depth best_move best_score =
    if current_depth > target_depth
    then best_move, best_score
    else (
      (* Order moves based on previous iteration scores *)
      let ordered_moves =
        if current_depth = 1
        then
          (* First iteration: use static move ordering *)
          moves
        else
          (* Use scores from previous iteration *)
          List.sort
            (fun m1 m2 ->
               let s1 =
                 try Hashtbl.find move_scores m1 with
                 | Not_found -> Search_common.AlphaBeta.initial_alpha
               in
               let s2 =
                 try Hashtbl.find move_scores m2 with
                 | Not_found -> Search_common.AlphaBeta.initial_alpha
               in
               compare s2 s1 (* Higher scores first *))
            moves
      in
      (* Search all moves at current depth - simple full-window search *)
      let rec search_moves remaining_moves curr_best_move curr_best_score =
        match remaining_moves with
        | [] -> curr_best_move, curr_best_score
        | mv :: rest ->
          let new_pos = Position.make_move pos mv in
          (* Always use full window search for simplicity and consistency *)
          let score, _ =
            thread_alphabeta
              new_pos
              Search_common.AlphaBeta.initial_alpha
              Search_common.AlphaBeta.initial_beta
              (current_depth - 1)
              local
              (Some mv)
          in
          let score = -score in
          (* Store score for next iteration *)
          Hashtbl.replace move_scores mv score;
          (* Update best *)
          let new_best_move, new_best_score =
            if score > curr_best_score then mv, score else curr_best_move, curr_best_score
          in
          search_moves rest new_best_move new_best_score
      in
      let new_best_move, new_best_score =
        search_moves
          ordered_moves
          (List.hd ordered_moves)
          Search_common.AlphaBeta.initial_alpha
      in
      iterate_depth (current_depth + 1) new_best_move new_best_score)
  in
  let best_move, best_score =
    iterate_depth 1 (List.hd moves) Search_common.AlphaBeta.initial_alpha
  in
  { best_move; best_score; nodes = !(local.nodes); depth = target_depth }
;;

(** Distribute moves among threads *)
let distribute_moves moves num_threads =
  let total = List.length moves in
  let base_size = total / num_threads in
  let remainder = total mod num_threads in
  let rec distribute idx thread_num acc =
    if thread_num >= num_threads
    then List.rev acc
    else (
      (* Give extra moves to first threads *)
      let size = if thread_num < remainder then base_size + 1 else base_size in
      let thread_moves = List.filteri (fun i _ -> i >= idx && i < idx + size) moves in
      distribute (idx + size) (thread_num + 1) (thread_moves :: acc))
  in
  distribute 0 0 []
;;

(** Root splitting parallel search *)
let root_split_search pos target_depth num_threads =
  (* Disabled for performance: Printf.printf "Starting root-split search with %d threads\n%!" num_threads; *)

  (* Generate root moves *)
  let moves = Movegen.generate_moves pos in
  let move_list =
    List.filter
      (fun mv ->
         let new_pos = Position.make_move pos mv in
         not (Search_common.is_in_check new_pos))
      moves
  in
  let num_moves = List.length move_list in
  (* Disabled for performance: Printf.printf "Root moves: %d\n%!" num_moves; *)
  if num_moves = 0
  then
    (* Disabled for performance: Printf.printf "No legal moves!\n"; *)
    0, None, 0L
  else if num_moves = 1 || num_threads = 1
  then (
    (* Only one move or one thread - no parallelism needed *)
    (* Disabled for performance: Printf.printf "Single move or single thread, searching serially\n%!"; *)
    let result = search_root_moves pos move_list target_depth 0 in
    result.best_score, Some result.best_move, result.nodes)
  else (
    (* Distribute moves among threads *)
    let move_groups = distribute_moves move_list num_threads in
    (* Disabled for performance:
    Printf.printf "Move distribution: %s\n%!" 
      (String.concat ", " (List.map (fun g -> string_of_int (List.length g)) move_groups));
    *)
    let start_time = Unix.gettimeofday () in
    (* Launch threads *)
    let domains =
      List.mapi
        (fun i moves_for_thread ->
           if List.length moves_for_thread = 0
           then None
           else
             Some
               (Domain.spawn (fun () ->
                  (* Disabled for performance: Printf.printf "Thread %d searching %d moves\n%!" i (List.length moves_for_thread); *)
                  search_root_moves pos moves_for_thread target_depth i)))
        move_groups
    in
    (* Collect results *)
    let results : search_result list =
      List.filter_map
        (fun domain_opt ->
           match domain_opt with
           | Some domain ->
             let res : search_result = Domain.join domain in
             (* Disabled for performance:
          Printf.printf "Thread result: score=%d, move=%s, nodes=%Ld\n%!" 
            res.best_score
            (Move.to_uci res.best_move)
            res.nodes;
             *)
             Some res
           | None -> None)
        domains
    in
    let elapsed = Unix.gettimeofday () -. start_time in
    (* Find best result *)
    let best_result =
      List.fold_left
        (fun best res -> if res.best_score > best.best_score then res else best)
        (List.hd results)
        (List.tl results)
    in
    (* Sum all nodes *)
    let total_nodes =
      List.fold_left
        (fun (sum : int64) (res : search_result) -> Int64.add sum res.nodes)
        0L
        results
    in
    let _nps = Int64.to_float total_nodes /. elapsed in
    (* Disabled for performance:
    Printf.printf "Root split completed: best_score=%d, best_move=%s, total_nodes=%Ld, time=%.3fs, nps=%.0f\n%!"
      best_result.best_score
      (Move.to_uci best_result.best_move)
      total_nodes
      elapsed
      nps;
    *)
    best_result.best_score, Some best_result.best_move, total_nodes)
;;

(** Single-threaded search for comparison *)
let single_threaded_root_search pos target_depth =
  let moves = Movegen.generate_moves pos in
  let move_list =
    List.filter
      (fun mv ->
         let new_pos = Position.make_move pos mv in
         not (Search_common.is_in_check new_pos))
      moves
  in
  let result = search_root_moves pos move_list target_depth 0 in
  result.best_score, Some result.best_move, result.nodes
;;
