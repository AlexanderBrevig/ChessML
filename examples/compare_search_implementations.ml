(** Compare Search module vs Parallel_search single-threaded performance *)

let test_position name fen depth =
  Printf.printf "\n=== Testing: %s (depth %d) ===\n" name depth;
  (* Test Search module *)
  Printf.printf "\n[Search module]\n";
  let game = Chessml.Game.of_fen fen in
  let start1 = Unix.gettimeofday () in
  let result1 = Chessml.Search.find_best_move ~verbose:false game depth in
  let end1 = Unix.gettimeofday () in
  let time1 = end1 -. start1 in
  let nps1 = Int64.to_float result1.nodes /. time1 in
  Printf.printf "  Time: %.3fs, Nodes: %Ld, NPS: %.0f\n" time1 result1.nodes nps1;
  Printf.printf
    "  Move: %s, Score: %d\n"
    (match result1.best_move with
     | Some m -> Chessml.Move.to_uci m
     | None -> "none")
    result1.score;
  (* Test Parallel_search single-threaded *)
  Printf.printf "\n[Parallel_search single-threaded]\n";
  let pos = Chessml.Position.of_fen fen in
  let start2 = Unix.gettimeofday () in
  let score2, move2, nodes2 = Chessml.Parallel_search.single_threaded_search pos depth in
  let end2 = Unix.gettimeofday () in
  let time2 = end2 -. start2 in
  let nps2 = Int64.to_float nodes2 /. time2 in
  Printf.printf "  Time: %.3fs, Nodes: %Ld, NPS: %.0f\n" time2 nodes2 nps2;
  Printf.printf
    "  Move: %s, Score: %d\n"
    (match move2 with
     | Some m -> Chessml.Move.to_uci m
     | None -> "none")
    score2;
  (* Compare *)
  Printf.printf "\n[Comparison]\n";
  Printf.printf "  Search module:               %.0f NPS\n" nps1;
  Printf.printf "  Parallel_search (1 thread):  %.0f NPS\n" nps2;
  let ratio = nps1 /. nps2 in
  Printf.printf
    "  Ratio (Search/Parallel):     %.2fx %s\n"
    ratio
    (if ratio > 1.0 then "(Search is faster)" else "(Parallel is faster)")
;;

let () =
  Printf.printf "╔══════════════════════════════════════════════════════════╗\n";
  Printf.printf "║  Search Module vs Parallel_search Performance Test      ║\n";
  Printf.printf "╚══════════════════════════════════════════════════════════╝\n";
  (* Starting position *)
  test_position
    "Starting Position"
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    9;
  (* Italian game position *)
  test_position
    "Italian Game"
    "r1bqkbnr/pppp1ppp/2n5/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 3 3"
    8;
  Printf.printf "\n✓ Test complete!\n"
;;
