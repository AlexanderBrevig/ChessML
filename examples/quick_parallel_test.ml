(** Quick test to verify optimizations are working *)

let () =
  Printf.printf "Testing Parallel Search Optimizations\n\n";
  (* Test position with tactical elements *)
  let depth = 9 in
  let fen = "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4" in
  let pos = Chessml.Position.of_fen fen in
  (* Quick depth 5 test *)
  Printf.printf "Running single-threaded (depth %d)...\n%!" depth;
  let start = Unix.gettimeofday () in
  let score1, move1, nodes1 = Chessml.Parallel_search.single_threaded_search pos depth in
  let time1 = Unix.gettimeofday () -. start in
  Printf.printf
    "Single: score=%d, move=%s, nodes=%Ld, time=%.3fs\n\n"
    score1
    (match move1 with
     | Some m -> Chessml.Move.to_uci m
     | None -> "none")
    nodes1
    time1;
  (* Parallel with 4 threads *)
  Printf.printf "Running parallel 4 threads (depth %d)...\n%!" depth;
  let start = Unix.gettimeofday () in
  let score2, move2, nodes2 = Chessml.Parallel_search.parallel_search pos depth 4 in
  let time2 = Unix.gettimeofday () -. start in
  Printf.printf
    "Parallel: score=%d, move=%s, nodes=%Ld, time=%.3fs\n\n"
    score2
    (match move2 with
     | Some m -> Chessml.Move.to_uci m
     | None -> "none")
    nodes2
    time2;
  let speedup = time1 /. time2 in
  Printf.printf "Speedup: %.2fx\n" speedup;
  if speedup > 1.2
  then Printf.printf "✓ Good speedup!\n"
  else if speedup > 1.0
  then Printf.printf "⚠ Marginal speedup\n"
  else Printf.printf "✗ No speedup (slowdown)\n"
;;
