(** Single-threaded benchmark using Search module (not Parallel_search) *)

let () =
  (* Standard starting position *)
  let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" in
  let depth = 9 in
  let game = Chessml.Game.default () in
  Printf.printf "Search Module Benchmark\n";
  Printf.printf "=======================\n\n";
  Printf.printf "Position: %s\n\n" fen;
  (* Run search at specified depth multiple times to get good perf samples *)
  for i = 1 to 3 do
    Printf.printf "Run %d:\n" i;
    let start_time = Unix.gettimeofday () in
    let result = Chessml.Search.find_best_move ~verbose:false game depth in
    let end_time = Unix.gettimeofday () in
    let elapsed = end_time -. start_time in
    let nps = Int64.to_float result.nodes /. elapsed in
    Printf.printf
      "  depth=%d, nodes=%Ld, time=%.3fs, NPS=%.0f\n"
      depth
      result.nodes
      elapsed
      nps;
    Printf.printf
      "  best_move=%s, score=%d\n\n"
      (match result.best_move with
       | Some m -> Chessml.Move.to_uci m
       | None -> "none")
      result.score
  done;
  Printf.printf "Benchmark complete. Use 'perf report' to analyze.\n"
;;
