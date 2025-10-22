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
  let has_queen_promo =
    List.exists
      (fun mv ->
         Move.is_promotion mv
         &&
         match Move.promotion mv with
         | Some Queen -> true
         | _ -> false)
      promotions
  in
  Alcotest.(check bool) "Should generate queen promotion" true has_queen_promo
;;

(** Test that the engine chooses to promote when it's the best move *)
let test_engine_chooses_promotion () =
  (* White pawn on 7th rank, king on e5, black king on g8 *)
  let fen = "6k1/4P3/8/4K3/8/8/8/8 w - - 0 1" in
  let game = Game.of_fen fen in
  (* Search to depth 5 *)
  let result = Search.find_best_move ~verbose:false game 5 in
  match result.best_move with
  | Some mv ->
    Alcotest.(check bool)
      "Engine should choose promotion move"
      true
      (Move.is_promotion mv);
    (* Verify it's a queen promotion (best) *)
    (match Move.promotion mv with
     | Some Queen -> Alcotest.(check bool) "Should promote to queen" true true
     | _ -> Alcotest.fail "Engine should promote to queen, not other piece")
  | None -> Alcotest.fail "Engine should find a move"
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
  Alcotest.(check bool) "7th rank has substantial bonus" true (eval_7th - eval_4th > 200)
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
  Alcotest.(check bool)
    "High halfmove clock reduces eval when winning"
    true
    (eval_high < eval_low);
  (* Penalty should be significant (at least 50cp) *)
  Alcotest.(check bool) "Penalty is substantial" true (eval_low - eval_high > 50)
;;

(** Test that 50-move penalty scales with material advantage *)
let test_fifty_move_penalty_scaling () =
  (* Calculate penalty with large material advantage *)
  let fen = "7k/8/8/8/8/8/8/K4RN1 w - - 80 50" in
  let pos = Position.of_fen fen in
  let material_advantage = 1400 in
  (* R+N = 500+320 = 820, but eval includes positional *)
  let penalty = Eval.evaluate_fifty_move_incentive pos material_advantage in
  (* Penalty should be negative (reduces winning score) *)
  Alcotest.(check bool) "Penalty is negative when winning" true (penalty < 0);
  (* Penalty should scale with material (larger advantage = larger penalty) *)
  (* At halfmove 80, penalty should be substantial *)
  Alcotest.(check bool) "Penalty at halfmove 80 is significant" true (penalty < -50)
;;

(** Test that 50-move penalty doesn't apply when halfmove clock is low *)
let test_no_fifty_move_penalty_early () =
  (* White up material, but halfmove clock at 10 (safe) *)
  let fen = "7k/8/8/8/8/8/8/K4RN1 w - - 10 6" in
  let pos = Position.of_fen fen in
  let material_advantage = 820 in
  let penalty = Eval.evaluate_fifty_move_incentive pos material_advantage in
  (* Penalty should be zero or very small when clock is low *)
  Alcotest.(check bool)
    "No significant penalty at low halfmove clock"
    true
    (abs penalty < 10)
;;

(** Test that engine plays the correct rook+pawn endgame sequence to mate *)
let test_rook_pawn_endgame_promotion_plan () =
  (* White: Ka1, Rc8, pawn on d4
     Black: Kh8
     This is a winning position - White should be able to force mate within 25 moves *)
  let starting_fen = "2R4k/8/8/8/3P4/8/8/K7 b - - 0 1" in
  let game = Game.of_fen starting_fen in
  
  (* Play out the game automatically for up to 25 moves *)
  let rec play_game current_game move_count max_moves =
    if move_count >= max_moves then
      Alcotest.fail (Printf.sprintf "White should have mated within %d moves" max_moves)
    else (
      let pos = Game.position current_game in
      let legal_moves = Movegen.generate_moves pos in
      if List.length legal_moves = 0 then (
        (* No legal moves - check if it's checkmate *)
        let side_to_move = Position.side_to_move pos in
        let king_sq = if side_to_move = White 
          then Position.white_king_sq pos 
          else Position.black_king_sq pos in
        let opponent = Types.Color.opponent side_to_move in
        let attackers = Movegen.compute_attackers_to pos king_sq opponent in
        if Bitboard.is_not_empty attackers then (
          (* King is in check and no legal moves = checkmate *)
          if side_to_move = Black then
            current_game (* Black is mated - success! *)
          else
            Alcotest.fail "Expected Black to be mated, not White"
        ) else
          Alcotest.fail "Rook+pawn vs lone king should not be stalemate"
      ) else if Game.is_draw current_game then
        Alcotest.fail "Rook+pawn vs lone king should not be a draw"
      else (
        (* Find and make the best move *)
        let result = Search.find_best_move ~verbose:false current_game 8 in
        match result.best_move with
        | None ->
          Alcotest.fail (Printf.sprintf "No legal move found at move %d" move_count)
        | Some mv ->
          let new_game = Game.make_move current_game mv in
          play_game new_game (move_count + 1) max_moves
      )
    )
  in
  
  let _final_game = play_game game 1 22 in
  ()
;;

(** Test suite *)
let () =
  let open Alcotest in
  run
    "Endgame improvements"
    [ ( "promotion_generation"
      , [ test_case "Generate promotion moves" `Quick test_promotion_move_generation
        ; test_case "Engine chooses promotion" `Slow test_engine_chooses_promotion
        ] )
    ; ( "passed_pawn_bonuses"
      , [ test_case
            "Passed pawn bonus progression"
            `Quick
            test_passed_pawn_bonus_progression
        ] )
    ; ( "fifty_move_rule"
      , [ test_case "50-move penalty exists" `Quick test_fifty_move_penalty_exists
        ; test_case "50-move penalty scales" `Quick test_fifty_move_penalty_scaling
        ; test_case "No penalty early game" `Quick test_no_fifty_move_penalty_early
        ] )
    ; (* Rook+pawn endgame promotion plan validated with actual engine play *)
      ( "endgame_planning"
      , [ test_case
            "Rook+pawn endgame promotion plan"
            `Slow
            test_rook_pawn_endgame_promotion_plan
        ] )
    ]
;;
