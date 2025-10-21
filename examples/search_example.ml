(** Example of basic search operations *)

open Chessml

let test_position name fen depth =
  Printf.printf "\n=== %s ===\n" name;
  Printf.printf "FEN: %s\n" fen;
  Printf.printf "Searching at depth %d...\n" depth;
  let game = Game.from_fen fen in
  let result = Search.find_best_move game depth in
  match result.Search.best_move with
  | Some mv ->
    Printf.printf "Best move: %s\n" (Move.to_uci mv);
    Printf.printf "Evaluation: %d centipawns\n" result.Search.score;
    if abs result.Search.score > 90000 then Printf.printf "Mate found!\n"
  | None -> Printf.printf "No legal moves available!\n"
;;

let () =
  Printf.printf "=== ChessML Search with MVV-LVA & Human-like Ordering ===\n";
  Printf.printf "Move ordering priority:\n";
  Printf.printf "  1. Checks\n";
  Printf.printf "  2. Captures (MVV-LVA)\n";
  Printf.printf "  3. Threats\n";
  Printf.printf "  4. Quiet moves\n\n";
  (* Starting position *)
  test_position "Starting Position" Position.fen_startpos 4;
  (* Tactical position with captures *)
  test_position
    "Tactical Position"
    "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4"
    5;
  (* Position with checks available *)
  test_position
    "Check Available"
    "r1bqkb1r/pppp1Bpp/2n2n2/4p3/4P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 0 4"
    4;
  (* Mate in 1 (Scholar's mate setup) *)
  test_position
    "Mate in 1"
    "r1bqkb1r/pppp1Qpp/2n2n2/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4"
    3;
  (* Queen sacrifice mate *)
  test_position "Forced Mate Sequence" "6k1/5ppp/8/8/8/8/5PPP/4Q1K1 w - - 0 1" 5;
  (* Tactical win - free bishop after checks *)
  Printf.printf "\n=== Tactical Puzzle: Win the Bishop ===\n";
  Printf.printf "Position: Black has an exposed bishop on c5\n";
  Printf.printf "Expected: Qh5+ followed by Qxc5 winning the bishop\n";
  let fen = "rnbqk1nr/pppp2pp/5p2/2b5/4Pp2/2P5/PP1P2PP/RNBQKBNR w KQkq - 0 1" in
  test_position "Tactical Win" fen 5;
  (* Let's also verify the moves are what we expect *)
  Printf.printf "\nDetailed analysis:\n";
  let game = Game.from_fen fen in
  let pos = Game.position game in
  let moves = Movegen.generate_moves pos in
  (* Check if Qh5+ is available *)
  let qh5_check = List.exists (fun mv -> Move.to_uci mv = "d1h5") moves in
  Printf.printf "Qh5+ available: %b\n" qh5_check;
  (* Show what the engine finds *)
  Printf.printf "\nTop moves considered by engine:\n";
  let result = Search.find_best_move game 5 in
  match result.Search.best_move with
  | Some mv ->
    Printf.printf "Engine choice: %s (score: %d)\n" (Move.to_uci mv) result.Search.score;
    (* Make the move and see what happens *)
    let new_pos = Position.make_move pos mv in
    Printf.printf
      "\nAfter %s, position evaluation: %d centipawns\n"
      (Move.to_uci mv)
      (Eval.evaluate new_pos);
    (* If it's Qh5+, check Black's options *)
    if Move.to_uci mv = "d1h5"
    then (
      Printf.printf "This is a check! Black must respond.\n";
      let black_moves = Movegen.generate_moves new_pos in
      Printf.printf "Black has %d legal moves: " (List.length black_moves);
      List.iter (fun mv -> Printf.printf "%s " (Move.to_uci mv)) black_moves;
      Printf.printf "\n\n";
      (* Analyze g7g6 response *)
      let g6_move = List.find_opt (fun mv -> Move.to_uci mv = "g7g6") black_moves in
      match g6_move with
      | Some g6 ->
        Printf.printf "If Black plays g6 (blocking):\n";
        let pos_after_g6 = Position.make_move new_pos g6 in
        let white_moves2 = Movegen.generate_moves pos_after_g6 in
        (* Look for Qxc5 *)
        let qxc5 = List.find_opt (fun mv -> Move.to_uci mv = "h5c5") white_moves2 in
        (match qxc5 with
         | Some capture ->
           Printf.printf "  White plays Qxc5, winning the bishop!\n";
           let final_pos = Position.make_move pos_after_g6 capture in
           let final_eval = Eval.evaluate final_pos in
           Printf.printf "  Final evaluation: %+d centipawns\n" final_eval;
           Printf.printf "  Material gained: ~330 (bishop)\n"
         | None -> Printf.printf "  Qxc5 not found (unexpected)\n");
        Printf.printf "\nOther Black moves also lose the bishop:\n";
        let other_moves = List.filter (fun mv -> Move.to_uci mv <> "g7g6") black_moves in
        List.iter
          (fun mv ->
             let pos_after = Position.make_move new_pos mv in
             let white_resp = Movegen.generate_moves pos_after in
             let can_take_bishop =
               List.exists (fun m -> Move.to_uci m = "h5c5") white_resp
             in
             if can_take_bishop
             then Printf.printf "  %s -> Qxc5 wins bishop\n" (Move.to_uci mv))
          other_moves
      | None -> ())
  | None -> Printf.printf "No move found\n"
;;
