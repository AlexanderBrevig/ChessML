(** Test Null Move Pruning effectiveness *)

open Chessml

let test_null_move_position pos_name fen depth =
  Printf.printf "\n=== Testing %s ===\n" pos_name;
  Printf.printf "Position: %s\n" fen;
  Printf.printf "Search depth: %d\n" depth;
  flush stdout;
  Printf.printf "Creating game...\n";
  flush stdout;
  let game = Game.from_fen fen in
  Printf.printf "Starting search...\n";
  flush stdout;
  let start = Unix.gettimeofday () in
  let result = Search.find_best_move ~verbose:false game depth in
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
  Printf.printf "Testing Null Move Pruning\n";
  Printf.printf "=========================\n";
  flush stdout;
  Printf.printf
    "\nNull move pruning should significantly reduce nodes in quiet positions\n";
  Printf.printf "by allowing early cutoffs when position is too good.\n";
  flush stdout;
  (* Test 1: Simple tactical position at depth 6 *)
  test_null_move_position
    "Tactical Position"
    "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4"
    6;
  Printf.printf "\nNull Move Pruning Test Complete\n";
  Printf.printf "\nExpected behavior:\n";
  Printf.printf "- Quiet positions: 30-50%% node reduction\n";
  Printf.printf "- Winning positions: Larger reduction (more cutoffs)\n";
  Printf.printf "- Endgames: Less reduction (zugzwang risk)\n";
  Printf.printf "- Tactical: Still effective (most moves still searched)\n";
  Printf.printf "\nNote: Null move is disabled in:\n";
  Printf.printf "- Positions with only pawns (zugzwang risk)\n";
  Printf.printf "- When in check (can't pass turn)\n";
  Printf.printf "- Depths < 3 (not worth overhead)\n";
  Printf.printf "- Mate searches (beta near mate value)\n"
;;
