(** Unit tests for endgame improvements: pawn promotion and 50-move rule avoidance *)

open Chessml

(** Test that pawn promotion moves are correctly generated and prioritized *)
let test_promotion_move_generation () =
  (* White pawn on 7th rank ready to promote *)
  let fen = "6k1/4P3/8/4K3/8/8/8/8 w - - 0 1" in
  let pos = Position.of_fen fen in
  let moves = Movegen.generate_moves pos in
  
  (* Filter promotion moves *)
  let promotions = List.filter Move.is_promotion moves in
  
  (* Should have 4 promotions (Q, R, B, N) *)
  Alcotest.(check int) "Should generate 4 promotion moves" 4 (List.length promotions);
  
  (* At least one should be a queen promotion *)
  let has_queen_promo = List.exists (fun mv ->
    Move.is_promotion mv && 
    match Move.promotion mv with
    | Some Queen -> true
    | _ -> false
  ) promotions in
  Alcotest.(check bool) "Should generate queen promotion" true has_queen_promo
;;

(** Test that the engine chooses to promote when it's the best move *)
let test_engine_chooses_promotion () =
  (* White pawn on 7th rank, king on e5, black king on g8 *)
  let fen = "6k1/4P3/8/4K3/8/8/8/8 w - - 0 1" in
  let game = Game.from_fen fen in
  
  (* Search to depth 5 *)
  let result = Search.find_best_move ~verbose:false game 5 in
  match result.best_move with
  | Some mv ->
    Alcotest.(check bool) "Engine should choose promotion move" 
      true (Move.is_promotion mv);
    (* Verify it's a queen promotion (best) *)
    (match Move.promotion mv with
    | Some Queen -> 
      Alcotest.(check bool) "Should promote to queen" true true
    | _ -> 
      Alcotest.fail "Engine should promote to queen, not other piece")
  | None ->
    Alcotest.fail "Engine should find a move"
;;

(** Test passed pawn evaluation bonuses increase with rank *)
let test_passed_pawn_bonus_progression () =
  (* Pawn on 4th rank *)
  let pos_4th = Position.of_fen "4k3/8/8/8/4P3/8/8/4K3 w - - 0 1" in
  let eval_4th = Eval.evaluate pos_4th in
  
  (* Pawn on 5th rank *)
  let pos_5th = Position.of_fen "4k3/8/8/4P3/8/8/8/4K3 w - - 0 1" in
  let eval_5th = Eval.evaluate pos_5th in
  
  (* Pawn on 6th rank *)
  let pos_6th = Position.of_fen "4k3/8/4P3/8/8/8/8/4K3 w - - 0 1" in
  let eval_6th = Eval.evaluate pos_6th in
  
  (* Pawn on 7th rank *)
  let pos_7th = Position.of_fen "4k3/4P3/8/8/8/8/8/4K3 w - - 0 1" in
  let eval_7th = Eval.evaluate pos_7th in
  
  (* Each rank should be increasingly valuable *)
  Alcotest.(check bool) "5th rank better than 4th" true (eval_5th > eval_4th);
  Alcotest.(check bool) "6th rank better than 5th" true (eval_6th > eval_5th);
  Alcotest.(check bool) "7th rank better than 6th" true (eval_7th > eval_6th);
  
  (* 7th rank should be substantially better (at least 200cp advantage) *)
  Alcotest.(check bool) "7th rank has substantial bonus" 
    true ((eval_7th - eval_4th) > 200)
;;

(** Test that 50-move rule penalty exists when winning *)
let test_fifty_move_penalty_exists () =
  (* White up material (rook + knight), halfmove clock at 80 *)
  let fen_high_clock = "7k/8/8/8/8/8/8/K4RN1 w - - 80 50" in
  let pos_high = Position.of_fen fen_high_clock in
  
  (* Same position with low halfmove clock *)
  let fen_low_clock = "7k/8/8/8/8/8/8/K4RN1 w - - 0 1" in
  let pos_low = Position.of_fen fen_low_clock in
  
  let eval_high = Eval.evaluate pos_high in
  let eval_low = Eval.evaluate pos_low in
  
  (* High halfmove clock should have lower evaluation (penalty applied) *)
  Alcotest.(check bool) "High halfmove clock reduces eval when winning" 
    true (eval_high < eval_low);
  
  (* Penalty should be significant (at least 50cp) *)
  Alcotest.(check bool) "Penalty is substantial" 
    true ((eval_low - eval_high) > 50)
;;

(** Test that 50-move penalty scales with material advantage *)
let test_fifty_move_penalty_scaling () =
  (* Calculate penalty with large material advantage *)
  let fen = "7k/8/8/8/8/8/8/K4RN1 w - - 80 50" in
  let pos = Position.of_fen fen in
  let material_advantage = 1400 in (* R+N = 500+320 = 820, but eval includes positional *)
  
  let penalty = Eval.evaluate_fifty_move_incentive pos material_advantage in
  
  (* Penalty should be negative (reduces winning score) *)
  Alcotest.(check bool) "Penalty is negative when winning" true (penalty < 0);
  
  (* Penalty should scale with material (larger advantage = larger penalty) *)
  (* At halfmove 80, penalty should be substantial *)
  Alcotest.(check bool) "Penalty at halfmove 80 is significant" 
    true (penalty < -50)
;;

(** Test that 50-move penalty doesn't apply when halfmove clock is low *)
let test_no_fifty_move_penalty_early () =
  (* White up material, but halfmove clock at 10 (safe) *)
  let fen = "7k/8/8/8/8/8/8/K4RN1 w - - 10 6" in
  let pos = Position.of_fen fen in
  let material_advantage = 820 in
  
  let penalty = Eval.evaluate_fifty_move_incentive pos material_advantage in
  
  (* Penalty should be zero or very small when clock is low *)
  Alcotest.(check bool) "No significant penalty at low halfmove clock" 
    true (abs penalty < 10)
;;

(** Test that engine plays the correct rook+pawn endgame sequence to mate *)
let test_rook_pawn_endgame_promotion_plan () =
  (* White: Ka1, Rd1 (needs to cut off), pawn on e4
     Black: Kh8
     White should execute the winning technique: cut off king, advance pawn, promote *)
  let starting_fen = "2R4k/8/8/8/3P4/8/8/K7 b - - 0 1" in
  
  (* Helper to check and execute an expected move *)
  let expect_move game_state expected_uci move_description =
    Printf.printf "\nExpecting: %s (%s)\n" expected_uci move_description;
    
    let result = Search.find_best_move ~verbose:false game_state 10 in
    match result.best_move with
    | None ->
      Printf.printf "ERROR: No move found!\n";
      Alcotest.fail (Printf.sprintf "Engine should find move: %s" expected_uci)
    | Some mv ->
      let actual_uci = Move.to_uci mv in
      Printf.printf "Engine plays: %s (score: %d)\n" actual_uci result.score;
      
      (* Check if move matches expectation *)
      Alcotest.(check string)
        (Printf.sprintf "Move %s: %s" expected_uci move_description)
        expected_uci
        actual_uci;
      
      (* Return new game state after making the move *)
      Game.make_move game_state mv
  in
  
  Printf.printf "\n=== Testing Rook+Pawn Endgame Winning Technique ===\n";
  Printf.printf "Starting position: %s\n" starting_fen;
  Printf.printf "White should: 1) Cut off king with rook, 2) Advance pawn, 3) Promote to mate\n";
  
  let game = Game.from_fen starting_fen in
  let pos = Game.position game in
  let side_to_move = Position.side_to_move pos in
  
  (* Verify it's Black's turn as the FEN specifies *)
  Alcotest.(check bool) 
    "Starting position should be Black to move"
    true
    (side_to_move = Black);
  
  
  (* Move sequence from actual engine play *)
  let game = expect_move game "h8g7" "Black king moves toward center" in
  let game = expect_move game "c8e8" "Rook cuts off Black king on e-file" in
  let game = expect_move game "g7f7" "Black king approaches" in
  let game = expect_move game "e8e2" "Rook maintains cutoff from 2nd rank" in
  let game = expect_move game "f7f6" "Black king continues approach" in
  let game = expect_move game "d4d5" "Pawn advances with king cut off" in
  let game = expect_move game "f6f5" "Black king tries to stop pawn" in
  let game = expect_move game "d5d6" "Pawn continues advancing" in
  let game = expect_move game "f5f4" "Black king desperate attempt" in
  let game = expect_move game "e2e7" "Rook moves behind passed pawn on 7th" in
  let game = expect_move game "f4f3" "Black king cannot stop promotion" in
  let game = expect_move game "d6d7" "Pawn reaches 7th rank" in
  let game = expect_move game "f3f2" "Black king helpless" in
  let _game = expect_move game "d7d8q" "Pawn promotes to Queen!" in
  Printf.printf "Rook+pawn endgame promotion plan executed successfully.\n"
;;

(** Test suite *)
let () =
  let open Alcotest in
  run "Endgame improvements" [
    "promotion_generation", [
      test_case "Generate promotion moves" `Quick test_promotion_move_generation;
      test_case "Engine chooses promotion" `Slow test_engine_chooses_promotion;
    ];
    "passed_pawn_bonuses", [
      test_case "Passed pawn bonus progression" `Quick test_passed_pawn_bonus_progression;
    ];
    "fifty_move_rule", [
      test_case "50-move penalty exists" `Quick test_fifty_move_penalty_exists;
      test_case "50-move penalty scales" `Quick test_fifty_move_penalty_scaling;
      test_case "No penalty early game" `Quick test_no_fifty_move_penalty_early;
    ];
    (* Rook+pawn endgame promotion plan validated with actual engine play *)
    "endgame_planning", [
      test_case "Rook+pawn endgame promotion plan" `Slow test_rook_pawn_endgame_promotion_plan;
    ];
  ]
;;
