(** Profiling test for performance analysis *)

open Chessml

let () =
  Printf.printf "Starting performance profiling...\n";
  flush stdout;
  (* Italian Game position - good mix of move generation and search *)
  let fen = "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4" in
  let game = Game.from_fen fen in
  Printf.printf "Testing position: %s\n" fen;
  Printf.printf "Running search at depth 6...\n";
  flush stdout;
  let start = Unix.gettimeofday () in
  let result = Search.find_best_move ~verbose:false game 6 in
  let elapsed = Unix.gettimeofday () -. start in
  Printf.printf "\nResults:\n";
  Printf.printf
    "Best move: %s\n"
    (match result.Search.best_move with
     | Some mv -> Move.to_uci mv
     | None -> "None");
  Printf.printf "Score: %d\n" result.Search.score;
  Printf.printf "Nodes: %Ld\n" result.Search.nodes;
  Printf.printf "Time: %.3f seconds\n" elapsed;
  Printf.printf "NPS: %.0f\n" (Int64.to_float result.Search.nodes /. elapsed);
  Printf.printf "\nProfile complete.\n"
;;
