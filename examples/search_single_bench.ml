(** Simple single-threaded benchmark for perf profiling *)

let () =
  (* Standard starting position *)
  let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" in
  let depth = 9 in
  let pos = Chessml.Position.of_fen fen in
  Printf.printf "Single-threaded Search Benchmark\n";
  Printf.printf "=================================\n\n";
  Printf.printf "Position: %s\n\n" fen;
  (* Run search at depth 5 multiple times to get good perf samples *)
  for i = 1 to 3 do
    Printf.printf "Run %d:\n" i;
    let start_time = Unix.gettimeofday () in
    let score, move, nodes = Chessml.Parallel_search.single_threaded_search pos depth in
    let end_time = Unix.gettimeofday () in
    let elapsed = end_time -. start_time in
    let nps = Int64.to_float nodes /. elapsed in
    Printf.printf "  depth=%d, nodes=%Ld, time=%.3fs, NPS=%.0f\n" depth nodes elapsed nps;
    Printf.printf
      "  best_move=%s, score=%d\n\n"
      (match move with
       | Some m -> Chessml.Move.to_uci m
       | None -> "none")
      score
  done;
  Printf.printf "Benchmark complete. Use 'perf report' to analyze.\n"
;;
