(** Comprehensive parallel search benchmark across various depths *)

let test_positions =
  [ "Start", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  ; "Italian", "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4"
  ; ( "Complex Middle"
    , "r1bq1rk1/pp1nbppp/2p1pn2/3p4/2PP4/2NBPN2/PP3PPP/R1BQ1RK1 w - - 0 10" )
  ]
;;

let benchmark_depth name fen depth num_threads =
  let pos = Chessml.Position.of_fen fen in
  (* Single-threaded *)
  let start = Unix.gettimeofday () in
  let _s_score, _s_move, s_nodes =
    Chessml.Parallel_search.single_threaded_search pos depth
  in
  let s_time = Unix.gettimeofday () -. start in
  (* Multi-threaded *)
  let start = Unix.gettimeofday () in
  let _p_score, _p_move, p_nodes =
    Chessml.Parallel_search.parallel_search pos depth num_threads
  in
  let p_time = Unix.gettimeofday () -. start in
  let speedup = s_time /. p_time in
  let nps_single = Int64.to_float s_nodes /. s_time in
  let nps_parallel = Int64.to_float p_nodes /. p_time in
  Printf.printf
    "| %s d%d | %6.2fs | %6.2fs | %.2fx | %8.0f | %8.0f |\n"
    name
    depth
    s_time
    p_time
    speedup
    nps_single
    nps_parallel;
  speedup
;;

let () =
  Printf.printf "╔═══════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║         Parallel Search Speedup Analysis                     ║\n";
  Printf.printf "╚═══════════════════════════════════════════════════════════════╝\n\n";
  (* Test with 4 threads *)
  let num_threads = 4 in
  Printf.printf "Testing with %d threads\n\n" num_threads;
  Printf.printf "┌──────────┬─────────┬─────────┬─────────┬──────────┬──────────┐\n";
  Printf.printf
    "│ Position │ Single  │ Parallel│ Speedup │  NPS-1   │  NPS-%d   │\n"
    num_threads;
  Printf.printf "├──────────┼─────────┼─────────┼─────────┼──────────┼──────────┤\n";
  let speedups = ref [] in
  (* Test each position at various depths *)
  List.iter
    (fun (name, fen) ->
       (* Depth 5 - shallow *)
       let sp5 = benchmark_depth name fen 5 num_threads in
       speedups := sp5 :: !speedups;
       (* Depth 7 - medium *)
       let sp7 = benchmark_depth name fen 7 num_threads in
       speedups := sp7 :: !speedups;
       let sp9 = benchmark_depth name fen 9 num_threads in
       speedups := sp9 :: !speedups;
       Printf.printf "├──────────┼─────────┼─────────┼─────────┼──────────┼──────────┤\n")
    (List.rev (List.tl (List.rev test_positions)));
  (* Skip complex for now *)
  Printf.printf "└──────────┴─────────┴─────────┴─────────┴──────────┴──────────┘\n\n";
  let avg_speedup =
    List.fold_left ( +. ) 0.0 !speedups /. float_of_int (List.length !speedups)
  in
  Printf.printf "Average speedup: %.2fx\n" avg_speedup;
  Printf.printf "\n📊 Analysis:\n";
  Printf.printf "  • Depth 5-6:  Overhead dominates, minimal speedup expected\n";
  Printf.printf "  • Depth 7-8:  Should see 1.2-1.5x speedup\n";
  Printf.printf "  • Depth 9+:   Should see 1.5-2.5x speedup\n";
  Printf.printf "  • TT hits:    Shared TT helps all threads find cutoffs faster\n\n";
  if avg_speedup < 1.0
  then Printf.printf "⚠️  Poor speedup - threads may be too many for shallow search\n"
  else if avg_speedup < 1.2
  then Printf.printf "⚠️  Marginal speedup - need deeper search or fewer threads\n"
  else if avg_speedup < 1.8
  then Printf.printf "✓  Decent speedup - typical for Lazy SMP\n"
  else Printf.printf "✓✓ Excellent speedup - parallel search working well!\n"
;;
