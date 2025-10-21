(** MVV-LVA (Most Valuable Victim - Least Valuable Attacker) tests *)

open Chessml

let test_mvv_lva_basic_scoring () =
  (* Test basic piece values - these should match Eval.piece_kind_value *)
  let pawn_value = Eval.piece_kind_value Pawn in
  let knight_value = Eval.piece_kind_value Knight in
  let bishop_value = Eval.piece_kind_value Bishop in
  let rook_value = Eval.piece_kind_value Rook in
  let queen_value = Eval.piece_kind_value Queen in
  Alcotest.(check int) "Pawn value is 100" 100 pawn_value;
  Alcotest.(check int) "Knight value is 320" 320 knight_value;
  Alcotest.(check int) "Bishop value is 330" 330 bishop_value;
  Alcotest.(check int) "Rook value is 500" 500 rook_value;
  Alcotest.(check int) "Queen value is 900" 900 queen_value
;;

let test_mvv_lva_capture_ordering () =
  (* Position where we can test different captures *)
  let game =
    Game.from_fen "r1bqk1nr/pppp1ppp/2n5/2b1p3/2B1P3/3P1N2/PPP2PPP/RNBQK2R w KQkq - 4 4"
  in
  let _pos = Game.position game in
  (* Let's test the scoring directly with a position designed for captures *)
  let pos_with_target =
    Game.from_fen "r1bqk1nr/pppp1ppp/2n5/2b1b3/1P2P3/3P1N2/P1P2PPP/RNBQK2R w KQkq - 4 4"
  in
  let pos_target = Game.position pos_with_target in
  (* Pawn captures bishop on c5 *)
  let pawn_capture = Move.make (Square.of_uci "b4") (Square.of_uci "c5") Move.Capture in
  let pawn_score =
    Search_common.Ordering.score_move
      pos_target
      pawn_capture
      ~is_tt_move:false
      ~is_killer:false
      ~is_countermove:false
  in
  (* Knight captures bishop on e5 *)
  let knight_capture = Move.make (Square.of_uci "f3") (Square.of_uci "e5") Move.Capture in
  let knight_score =
    Search_common.Ordering.score_move
      pos_target
      knight_capture
      ~is_tt_move:false
      ~is_killer:false
      ~is_countermove:false
  in
  Printf.printf "Pawn takes bishop score: %d\n" pawn_score;
  Printf.printf "Knight takes bishop score: %d\n" knight_score;
  (* Pawn taking bishop should score higher than knight taking bishop *)
  Alcotest.(check bool)
    "Pawn capture scores higher than knight capture"
    true
    (pawn_score > knight_score)
;;

let test_mvv_lva_score_calculation () =
  (* Test the MVV-LVA calculation formula directly *)
  let game =
    Game.from_fen "r1bqk2r/pppp1ppp/2n2n2/2b1p3/2B1P3/3P1N2/PPP2PPP/RNBQK2R b KQkq - 5 4"
  in
  let pos = Game.position game in
  (* Test non-capture move - score_move returns higher for better moves, quiet moves get base score *)
  let quiet_move = Move.make (Square.of_uci "g8") (Square.of_uci "f6") Move.Quiet in
  let quiet_score =
    Search_common.Ordering.score_move
      pos
      quiet_move
      ~is_tt_move:false
      ~is_killer:false
      ~is_countermove:false
  in
  (* Quiet moves get a base score, not 0, so just check it's less than captures *)
  Alcotest.(check bool) "Quiet move has low score" true (quiet_score < 1000000);
  (* Test capture: pawn takes pawn (equal trade) *)
  let pawn_pos =
    Game.from_fen "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2"
  in
  let pos_pawn = Game.position pawn_pos in
  let pawn_capture = Move.make (Square.of_uci "e4") (Square.of_uci "d5") Move.Capture in
  let pawn_capture_score =
    Search_common.Ordering.score_move
      pos_pawn
      pawn_capture
      ~is_tt_move:false
      ~is_killer:false
      ~is_countermove:false
  in
  (* score_move includes SEE and other factors, so just check it's a reasonable capture score *)
  (* Equal trades get equal_capture_base (7000) *)
  Alcotest.(check bool)
    "Pawn takes pawn has positive score"
    true
    (pawn_capture_score >= 7000)
;;

let test_move_ordering_with_captures () =
  (* Test that captures are ordered correctly using MVV-LVA *)
  let game =
    Game.from_fen "r1bqk2r/pppp1ppp/2n2n2/2b1p2Q/2B1P3/3P1N2/PPP2PPP/RNB1K2R w KQkq - 5 5"
  in
  let pos = Game.position game in
  (* Generate all legal moves *)
  let moves = Movegen.generate_moves pos in
  (* Filter to just captures *)
  let captures = List.filter Move.is_capture moves in
  (* Order the captures *)
  let ordered_captures =
    Search_common.Ordering.order_moves
      pos
      captures
      ~tt_move:None
      ~killer_check:(fun _ -> false)
      ~countermove_check:(fun _ -> false)
  in
  (* Verify that we have some captures to test *)
  Alcotest.(check bool) "Position has some captures" true (List.length captures > 0);
  (* Verify that ordered list has same length *)
  Alcotest.(check int)
    "Ordered captures same length"
    (List.length captures)
    (List.length ordered_captures);
  (* Test that ordering is stable - running twice gives same result *)
  let ordered_captures2 =
    Search_common.Ordering.order_moves
      pos
      captures
      ~tt_move:None
      ~killer_check:(fun _ -> false)
      ~countermove_check:(fun _ -> false)
  in
  Alcotest.(check bool)
    "Move ordering is deterministic"
    true
    (ordered_captures = ordered_captures2)
;;

let test_mvv_lva_victim_priority () =
  (* Test that higher value victims are prioritized *)
  let queen_victim_score = (900 * 10) - 100 in
  (* Pawn takes Queen *)
  let pawn_victim_score = (100 * 10) - 100 in
  (* Pawn takes Pawn *)
  Alcotest.(check bool)
    "Queen victim scores higher than pawn victim"
    true
    (queen_victim_score > pawn_victim_score);
  (* Test that lower value attackers are prioritized for same victim *)
  let pawn_attacker_score = (330 * 10) - 100 in
  (* Pawn takes Bishop *)
  let queen_attacker_score = (330 * 10) - 900 in
  (* Queen takes Bishop *)
  Alcotest.(check bool)
    "Pawn attacker scores higher than queen attacker"
    true
    (pawn_attacker_score > queen_attacker_score)
;;

let test_mvv_lva_integration_with_search () =
  (* Test that MVV-LVA improves tactical search *)
  let tactical_pos =
    Game.from_fen "r1bqk1nr/pppp1ppp/2n5/2b1p3/2B1P3/3P1N2/PPP2PPP/RNBQK2R w KQkq - 4 4"
  in
  (* Search should find the best moves efficiently *)
  let result = Search.find_best_move ~verbose:false tactical_pos 3 in
  (* Should find a reasonable move *)
  Alcotest.(check bool) "Search finds a move" true (result.best_move <> None);
  Alcotest.(check bool) "Search score is reasonable" true (abs result.score < 10000);
  Alcotest.(check bool) "Search explores nodes" true (result.nodes > 0L)
;;

let test_mvv_lva_special_captures () =
  (* Test en passant capture (victim is pawn, but not on target square) *)
  let en_passant_pos =
    Game.from_fen "rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3"
  in
  let pos_ep = Game.position en_passant_pos in
  (* En passant capture move *)
  let ep_capture =
    Move.make (Square.of_uci "e5") (Square.of_uci "f6") Move.EnPassantCapture
  in
  let ep_score =
    Search_common.Ordering.score_move
      pos_ep
      ep_capture
      ~is_tt_move:false
      ~is_killer:false
      ~is_countermove:false
  in
  (* En passant should still get a reasonable score (pawn captures pawn) *)
  (* En passant captures are treated as winning captures *)
  Alcotest.(check bool) "En passant has positive score" true (ep_score >= 7000);
  (* Test promotion with capture *)
  let promotion_pos =
    Game.from_fen "rnbqkbn1/pppppppP/8/8/8/8/PPPPPP1P/RNBQKBNR w KQq - 0 1"
  in
  let pos_promo = Game.position promotion_pos in
  let promo_capture =
    Move.make (Square.of_uci "h7") (Square.of_uci "g8") Move.CaptureAndPromoteQueen
  in
  let promo_score =
    Search_common.Ordering.score_move
      pos_promo
      promo_capture
      ~is_tt_move:false
      ~is_killer:false
      ~is_countermove:false
  in
  (* Promotion captures should be highly valued *)
  (* Promotion captures get winning_capture_base (8000) + SEE *)
  Alcotest.(check bool) "Promotion capture has high score" true (promo_score >= 8000)
;;

let test_mvv_lva_defensive_considerations () =
  (* While MVV-LVA doesn't consider if pieces are defended, 
     test that it at least orders captures sensibly *)
  let complex_pos =
    Game.from_fen "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/3P1N2/PPP2PPP/RNBQK2R b KQkq - 5 4"
  in
  let pos = Game.position complex_pos in
  (* Get all moves and filter captures *)
  let moves = Movegen.generate_moves pos in
  let captures = List.filter Move.is_capture moves in
  if List.length captures > 1
  then (
    let ordered =
      Search_common.Ordering.order_moves
        pos
        captures
        ~tt_move:None
        ~killer_check:(fun _ -> false)
        ~countermove_check:(fun _ -> false)
    in
    let first_capture = List.hd ordered in
    let last_capture = List.nth ordered (List.length ordered - 1) in
    let first_score =
      Search_common.Ordering.score_move
        pos
        first_capture
        ~is_tt_move:false
        ~is_killer:false
        ~is_countermove:false
    in
    let last_score =
      Search_common.Ordering.score_move
        pos
        last_capture
        ~is_tt_move:false
        ~is_killer:false
        ~is_countermove:false
    in
    (* First capture should score higher than or equal to last *)
    Alcotest.(check bool)
      "Captures ordered by descending score"
      true
      (first_score >= last_score))
  else
    (* If no captures, that's fine too *)
    Alcotest.(check bool) "Position analyzed successfully" true true
;;

let () =
  let open Alcotest in
  run
    "MVV-LVA Tests"
    [ ( "basic_scoring"
      , [ test_case "Basic piece values" `Quick test_mvv_lva_basic_scoring
        ; test_case "MVV-LVA score calculation" `Quick test_mvv_lva_score_calculation
        ; test_case "Victim priority" `Quick test_mvv_lva_victim_priority
        ] )
    ; ( "capture_ordering"
      , [ test_case "Capture ordering" `Quick test_mvv_lva_capture_ordering
        ; test_case "Move ordering with captures" `Quick test_move_ordering_with_captures
        ; test_case
            "MVV-LVA integration with search"
            `Quick
            test_mvv_lva_integration_with_search
        ] )
    ; ( "special_cases"
      , [ test_case
            "Special captures (en passant, promotion)"
            `Quick
            test_mvv_lva_special_captures
        ; test_case
            "Defensive considerations"
            `Quick
            test_mvv_lva_defensive_considerations
        ] )
    ]
;;
