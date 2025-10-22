(** Test quiescence search *)

open Chessml

let test_quiescence_position name fen depth =
  Printf.printf "\n=== %s ===\n" name;
  Printf.printf "FEN: %s\n" fen;
  Printf.printf "Testing quiescence search vs static eval at depth %d\n" depth;
  let game = Game.of_fen fen in
  let pos = Game.position game in
  (* Get static evaluation *)
  let static_eval = Eval.evaluate pos in
  Printf.printf "Static evaluation: %d centipawns\n" static_eval;
  (* Get search result with quiescence *)
  let result = Search.find_best_move ~verbose:false game depth in
  Printf.printf "Search evaluation: %d centipawns\n" result.score;
  Printf.printf
    "Best move: %s\n"
    (match result.best_move with
     | Some mv -> Move.to_uci mv
     | None -> "none");
  Printf.printf "Nodes searched: %Ld\n" result.nodes;
  let diff = result.score - static_eval in
  Printf.printf "Difference (search - static): %+d centipawns\n" diff;
  if abs diff > 10
  then Printf.printf "→ Quiescence search found tactical improvements!\n"
  else Printf.printf "→ Position appears tactically quiet\n"
;;

let () =
  Printf.printf "=== Quiescence Search Tests ===\n";
  (* Test 1: Starting position (should be quiet) *)
  test_quiescence_position
    "Starting Position"
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    2;
  (* Test 2: Position with hanging piece *)
  test_quiescence_position
    "Hanging Bishop"
    "rnbqk1nr/pppp2pp/5p2/2b5/4Pp2/2P5/PP1P2PP/RNBQKBNR w KQkq - 0 1"
    2;
  (* Test 3: Position with tactical shot *)
  test_quiescence_position
    "Fork Available"
    "rnbqkb1r/pppp1ppp/5n2/4p3/2B1P3/8/PPPP1PPP/RNBQK1NR w KQkq - 4 4"
    2;
  (* Test 4: Quiet endgame position *)
  test_quiescence_position "Quiet Endgame" "8/8/8/3k4/8/3K4/8/8 w - - 0 1" 2;
  Printf.printf "\n=== Quiescence Tests Complete ===\n"
;;
