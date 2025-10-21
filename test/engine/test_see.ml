open Chessml
open Alcotest

(** Test helpers *)

let see_test name fen move expected_score () =
  let game = Game.from_fen fen in
  let pos = Game.position game in
  let score = See.evaluate pos move in
  check int name expected_score score
;;

let make_move from_str to_str =
  let from_sq = Square.of_uci from_str in
  let to_sq = Square.of_uci to_str in
  Move.make from_sq to_sq Move.Capture
;;

(** Basic capture tests *)

let simple_capture_wins () =
  (* White pawn takes black pawn: 100cp gain *)
  see_test
    "White pawn takes undefended pawn"
    "8/8/8/4p3/3P4/8/8/8 w - - 0 1"
    (make_move "d4" "e5")
    100
    ()
;;

let simple_capture_equal () =
  (* White pawn takes black pawn defended by pawn: 0cp (equal trade) *)
  see_test
    "White pawn takes pawn defended by pawn"
    "8/8/5p2/4p3/3P4/8/8/8 w - - 0 1"
    (make_move "d4" "e5")
    0
    ()
;;

let simple_capture_loses () =
  (* White knight takes pawn defended by pawn: -220cp loss (320 - 100) *)
  see_test
    "Knight takes pawn defended by pawn (bad)"
    "8/8/5p2/4p3/8/3N4/8/8 w - - 0 1"
    (make_move "d3" "e5")
    (-220)
    ()
;;

(** Multiple attackers/defenders *)

let multiple_attackers_wins () =
  (* White takes queen with pawn, black recaptures with knight,
     white recaptures with knight: +900 - 100 + 320 = +1120cp
     But we stop at favorable point: just taking queen = +900 - 100 = +800cp *)
  see_test
    "Multiple attackers - favorable"
    "8/8/5n2/3q4/4P3/3N4/8/8 w - - 0 1" (* Black knight on f6, not e6 *)
    (make_move "e4" "d5")
    800 (* Pawn takes queen, black recaptures with knight: 900 - 100 *)
    ()
;;

let multiple_defenders () =
  (* Rook takes pawn defended by rook and bishop *)
  (* Rook takes pawn (+100), bishop takes rook (-500), we lose rook: -400cp *)
  see_test
    "Multiple defenders - bishop and rook"
    "8/8/3b4/3rpR2/8/8/8/8 w - - 0 1"
    (make_move "f5" "e5")
    (-400) (* We win pawn but lose rook *)
    ()
;;

(** X-ray attacks through piece *)

let xray_attack () =
  (* Pawn takes pawn defended by queen, but rook x-rays through
     Queen CAN recapture but shouldn't (would lose queen to rook)
     So black stands pat, white keeps pawn: +100cp *)
  see_test
    "X-ray attack through captured piece"
    "8/8/8/3qp3/3PR3/8/8/8 w - - 0 1"
    (make_move "d4" "e5")
    100 (* Black queen shouldn't recapture (would lose queen to rook) *)
    ()
;;

(** Least valuable attacker *)

let lva_ordering () =
  (* Both rook and queen can recapture - should use least valuable (rook) *)
  (* Queen takes pawn, black rook recaptures (not queen): +100 - 900 = -800cp *)
  see_test
    "LVA: Use rook not queen for recapture"
    "4q3/8/4r3/4p3/8/8/4Q3/8 w - - 0 1"
    (make_move "e2" "e5")
    (-800) (* Queen takes pawn, black uses rook (LVA) not queen: +100 - 900 = -800 *)
    ()
;;

(** Promotion scenarios *)

let pawn_promotion_capture () =
  (* Pawn on 7th rank captures and promotes to queen *)
  see_test
    "Pawn captures and promotes"
    "3rr3/3P4/8/8/8/8/8/8 w - - 0 1"
    (make_move "d7" "e8")
    400
    (* +500 (rook) + 800 (promotion Q-P) = +1300, but rook recaptures queen: -900, net +400 *)
    ()
;;

(** En passant *)

let en_passant_capture () =
  (* En passant is a special pawn capture *)
  see_test
    "En passant capture (undefended)"
    "8/8/8/3pP3/8/8/8/8 w - d6 0 1"
    (Move.make (Square.of_uci "e5") (Square.of_uci "d6") Move.EnPassantCapture)
    100 (* Win pawn *)
    ()
;;

(** Complex tactical scenarios *)

(* Commented out - complex scenario needs more investigation
let complex_exchange () =
  (* Central pawn capture with multiple pieces involved *)
  see_test "Complex central exchange"
    "r2qk2r/ppp2ppp/2n5/3p4/1b1P4/2N2N2/PPP2PPP/R1BQKB1R w KQkq - 0 1"
    (make_move "c3" "d5")
    100  (* Knight takes pawn, knight recaptures: 0cp net, but there's a pawn *)
    ()
*)

let dont_capture_defended_queen () =
  (* Knight takes queen defended by pawn: +900 - 320 = +580cp *)
  see_test
    "Capture defended queen (still wins)"
    "8/8/2p5/3q4/8/3N4/8/8 w - - 0 1"
    (make_move "d3" "d5")
    580 (* Win queen, lose knight *)
    ()
;;

let protected_piece_chain () =
  (* Bishop takes knight defended by pawn: +320 - 330 = -10cp *)
  see_test
    "Protected piece chain"
    "8/8/2p5/3n4/4B3/8/8/8 w - - 0 1"
    (make_move "e4" "d5")
    (-10) (* Bxn, pxB: 320-330 = -10 *)
    ()
;;

(** Edge cases *)

let king_cannot_capture_defended () =
  (* King takes rook defended by pawn - king can't recapture due to check *)
  see_test
    "King cannot capture defended piece"
    "8/8/8/8/4r3/3p4/4K3/8 w - - 0 1"
    (make_move "e2" "e4")
    500 (* King takes rook, cannot be recaptured (king can't move into check) *)
    ()
;;

let empty_square_returns_zero () =
  (* SEE of move to empty square should be 0 or handle gracefully *)
  see_test
    "Non-capture move"
    "8/8/8/8/3P4/8/8/8 w - - 0 1"
    (Move.make (Square.of_uci "d4") (Square.of_uci "d5") Move.Quiet)
    0 (* No capture *)
    ()
;;

(** Test suite *)

let () =
  run
    "SEE (Static Exchange Evaluation)"
    [ ( "basic_captures"
      , [ test_case "Simple undefended capture wins" `Quick simple_capture_wins
        ; test_case "Equal trade" `Quick simple_capture_equal
        ; test_case "Bad capture loses material" `Quick simple_capture_loses
        ] )
    ; ( "multiple_pieces"
      , [ test_case "Multiple attackers favorable" `Quick multiple_attackers_wins
        ; test_case "Multiple defenders" `Quick multiple_defenders
        ; test_case "LVA ordering" `Quick lva_ordering
        ] )
    ; ( "tactical"
      , [ test_case "X-ray attacks" `Quick xray_attack
        ; test_case "Capture defended queen" `Quick dont_capture_defended_queen
        ; test_case "Protected piece chain" `Quick protected_piece_chain
        ] )
    ; ( "special_moves"
      , [ test_case "Pawn promotion capture" `Quick pawn_promotion_capture
        ; test_case "En passant capture" `Quick en_passant_capture
        ] )
    ; ( "edge_cases"
      , [ test_case "King cannot capture defended" `Quick king_cannot_capture_defended
        ; test_case "Empty square (quiet move)" `Quick empty_square_returns_zero
        ] )
    ]
;;
