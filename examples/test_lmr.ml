(** Test Late Move Reductions effectiveness *)

open Chessml

let test_position_with_lmr pos_name fen depth =
  Printf.printf "\n=== Testing %s ===\n" pos_name;
  Printf.printf "Position: %s\n" fen;
  Printf.printf "Search depth: %d\n\n" depth;
  let game = Game.of_fen fen in
  let start = Unix.gettimeofday () in
  let result = Search.find_best_move game depth in
  let elapsed = Unix.gettimeofday () -. start in
  Printf.printf
    "Best move: %s\n"
    (match result.Search.best_move with
     | Some mv -> Move.to_uci mv
     | None -> "None");
  Printf.printf "Score: %d\n" result.Search.score;
  Printf.printf "Nodes: %Ld\n" result.Search.nodes;
  Printf.printf "Time: %.3f seconds\n" elapsed;
  Printf.printf "NPS: %.0f\n" (Int64.to_float result.Search.nodes /. elapsed);
  Printf.printf "\n"
;;

let () =
  Printf.printf "Testing Late Move Reductions\n";
  Printf.printf "=============================\n";
  (* Test 1: Starting position - many moves to order *)
  test_position_with_lmr
    "Starting Position"
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    7;
  (* Test 2: Middlegame position with many candidate moves *)
  test_position_with_lmr
    "Complex Middlegame"
    "r1bqkb1r/pppp1ppp/2n2n2/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4"
    7;
  (* Test 3: Tactical position - LMR should avoid reducing good moves *)
  test_position_with_lmr
    "Tactical Position"
    "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4"
    7;
  (* Test 4: Open position with many captures *)
  test_position_with_lmr
    "Open Position"
    "rnbqkb1r/ppp2ppp/5n2/3pp3/3PP3/5N2/PPP2PPP/RNBQKB1R w KQkq - 0 4"
    7;
  Printf.printf "\nLMR Test Complete\n";
  Printf.printf "\nKey observations:\n";
  Printf.printf "- Tactical moves (captures, promotions) should NOT be reduced\n";
  Printf.printf "- First 3 moves should NOT be reduced\n";
  Printf.printf "- Later quiet moves should be reduced by 1-2 plies\n";
  Printf.printf "- Re-search happens when reduced search fails high\n";
  Printf.printf "- Overall node count should be significantly lower\n"
;;
