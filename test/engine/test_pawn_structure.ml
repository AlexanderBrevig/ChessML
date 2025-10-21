(** Pawn structure evaluation tests *)

open Chessml

let test_passed_pawn_white () =
  (* White has a passed pawn on e5, no black pawns can stop it *)
  let fen = "8/8/8/4P3/8/8/8/8 w - - 0 1" in
  let pos = Position.of_fen fen in
  let eval = Eval.evaluate pos in
  (* Should have significant bonus for passed pawn *)
  Alcotest.(check bool) "Passed pawn has positive evaluation" true (eval > 100)
;;

let test_passed_pawn_black () =
  (* Black has a passed pawn on d4 *)
  let fen = "8/8/8/8/3p4/8/8/8 b - - 0 1" in
  let pos = Position.of_fen fen in
  let eval = Eval.evaluate pos in
  (* Should have significant bonus for black's passed pawn *)
  Alcotest.(check bool) "Black passed pawn has positive evaluation" true (eval > 100)
;;

let test_advanced_passed_pawn () =
  (* Passed pawn on 6th rank is worth more than one on 4th rank *)
  let fen1 = "8/8/4P3/8/8/8/8/8 w - - 0 1" in
  (* e6 - 6th rank *)
  let fen2 = "8/8/8/8/4P3/8/8/8 w - - 0 1" in
  (* e4 - 4th rank *)
  let pos1 = Position.of_fen fen1 in
  let pos2 = Position.of_fen fen2 in
  let eval1 = Eval.evaluate pos1 in
  let eval2 = Eval.evaluate pos2 in
  Alcotest.(check bool) "Advanced passed pawn worth more" true (eval1 > eval2)
;;

let test_doubled_pawns_penalty () =
  (* Doubled pawns on e-file should have penalty *)
  let fen1 = "8/8/8/4P3/4P3/8/8/8 w - - 0 1" in
  (* Doubled *)
  let fen2 = "8/8/8/4P3/3P4/8/8/8 w - - 0 1" in
  (* Not doubled *)
  let pos1 = Position.of_fen fen1 in
  let pos2 = Position.of_fen fen2 in
  let eval1 = Eval.evaluate pos1 in
  let eval2 = Eval.evaluate pos2 in
  Alcotest.(check bool) "Doubled pawns have penalty" true (eval1 < eval2)
;;

let test_tripled_pawns_worse () =
  (* Tripled pawns are even worse - but compare per-pawn value *)
  let fen1 = "8/4P3/4P3/4P3/8/8/8/8 w - - 0 1" in
  (* Tripled = 3 pawns *)
  let fen2 = "8/8/8/3PP3/8/8/8/8 w - - 0 1" in
  (* Two separate = 2 pawns *)
  let pos1 = Position.of_fen fen1 in
  let pos2 = Position.of_fen fen2 in
  let eval1 = Eval.evaluate pos1 in
  let eval2 = Eval.evaluate pos2 in
  (* 3 tripled pawns should be worth less per pawn than 2 connected pawns *)
  let per_pawn1 = eval1 / 3 in
  let per_pawn2 = eval2 / 2 in
  Alcotest.(check bool) "Tripled pawns worse per-pawn value" true (per_pawn1 < per_pawn2)
;;

let test_isolated_pawn_penalty () =
  (* Isolated pawn has no friendly pawns on adjacent files *)
  let fen1 = "8/8/8/4P3/8/8/8/8 w - - 0 1" in
  (* Isolated on e-file *)
  let fen2 = "8/8/8/3PP3/8/8/8/8 w - - 0 1" in
  (* Connected pawns *)
  let pos1 = Position.of_fen fen1 in
  let pos2 = Position.of_fen fen2 in
  let eval1 = Eval.evaluate pos1 in
  let eval2 = Eval.evaluate pos2 in
  Alcotest.(check bool) "Isolated pawn has penalty" true (eval1 < eval2)
;;

let test_backward_pawn_penalty () =
  (* Backward pawn: can't advance safely and no support *)
  let fen = "8/8/8/3ppp2/4P3/8/8/8 w - - 0 1" in
  (* e4 is backward *)
  let pos = Position.of_fen fen in
  let eval = Eval.evaluate pos in
  (* Backward pawn should have negative evaluation *)
  Alcotest.(check bool) "Backward pawn has penalty" true (eval < 0)
;;

let test_pawn_chain_bonus () =
  (* Diagonal pawn chain is strong *)
  let fen1 = "8/8/8/3P4/4P3/8/8/8 w - - 0 1" in
  (* Chain: d5-e4 *)
  let fen2 = "8/8/8/3P4/8/4P3/8/8 w - - 0 1" in
  (* Disconnected *)
  let pos1 = Position.of_fen fen1 in
  let pos2 = Position.of_fen fen2 in
  let eval1 = Eval.evaluate pos1 in
  let eval2 = Eval.evaluate pos2 in
  Alcotest.(check bool) "Pawn chain has bonus" true (eval1 > eval2)
;;

let test_connected_pawns_bonus () =
  (* Side-by-side pawns are also good *)
  let fen = "8/8/8/3PP3/8/8/8/8 w - - 0 1" in
  let pos = Position.of_fen fen in
  let eval = Eval.evaluate pos in
  Alcotest.(check bool) "Connected pawns have positive evaluation" true (eval > 0)
;;

let test_passed_pawn_race () =
  (* Both sides have passed pawns - closer one is more valuable *)
  let fen1 = "8/8/P7/8/8/8/7p/8 w - - 0 1" in
  (* White a6 closer *)
  let fen2 = "8/8/7P/8/8/8/p7/8 w - - 0 1" in
  (* Mirrored: White h6, Black a2 - same distance *)
  let pos1 = Position.of_fen fen1 in
  let pos2 = Position.of_fen fen2 in
  let eval1 = Eval.evaluate pos1 in
  let eval2 = Eval.evaluate pos2 in
  (* White's pawn on a6 should give similar evaluation to h6 (symmetric) *)
  (* This tests that passed pawn evaluation is working *)
  Alcotest.(check bool)
    "Symmetric passed pawns give similar eval"
    true
    (abs (eval1 - eval2) < 20)
;;

let test_protected_passed_pawn () =
  (* Protected passed pawn is stronger than unprotected *)
  let fen1 = "8/8/3P4/4P3/8/8/8/8 w - - 0 1" in
  (* d6 protected by e5 *)
  let fen2 = "8/8/3P4/8/8/8/8/8 w - - 0 1" in
  (* d6 unprotected *)
  let pos1 = Position.of_fen fen1 in
  let pos2 = Position.of_fen fen2 in
  let eval1 = Eval.evaluate pos1 in
  let eval2 = Eval.evaluate pos2 in
  Alcotest.(check bool) "Protected passed pawn stronger" true (eval1 > eval2)
;;

let test_central_pawns_bonus () =
  (* Central pawns (d,e files) are more valuable *)
  let fen1 = "8/8/8/3PP3/8/8/8/8 w - - 0 1" in
  (* Central d5, e5 *)
  let fen2 = "8/8/8/P5P1/8/8/8/8 w - - 0 1" in
  (* Wing a5, h5 *)
  let pos1 = Position.of_fen fen1 in
  let pos2 = Position.of_fen fen2 in
  let eval1 = Eval.evaluate pos1 in
  let eval2 = Eval.evaluate pos2 in
  Alcotest.(check bool) "Central pawns more valuable" true (eval1 > eval2)
;;

let test_complex_pawn_structure () =
  (* Compare structures: connected vs doubled, same material count *)
  let fen1 = "8/8/8/2PPP3/8/8/8/8 w - - 0 1" in
  (* 3 connected pawns *)
  let fen2 = "8/8/8/3P4/3P4/3P4/8/8 w - - 0 1" in
  (* 3 tripled (doubled+) pawns *)
  let pos1 = Position.of_fen fen1 in
  let pos2 = Position.of_fen fen2 in
  let eval1 = Eval.evaluate pos1 in
  let eval2 = Eval.evaluate pos2 in
  (* Connected pawns should be better than tripled pawns *)
  Alcotest.(check bool) "Connected pawns better than tripled" true (eval1 > eval2)
;;

let test_no_pawns_neutral () =
  (* Position with no pawns should not crash *)
  let fen = "8/8/8/8/8/8/8/8 w - - 0 1" in
  let pos = Position.of_fen fen in
  let eval = Eval.evaluate pos in
  Alcotest.(check int) "No pawns gives zero pawn eval" 0 eval
;;

let () =
  let open Alcotest in
  run
    "Pawn Structure Tests"
    [ ( "passed_pawns"
      , [ test_case "White passed pawn bonus" `Quick test_passed_pawn_white
        ; test_case "Black passed pawn bonus" `Quick test_passed_pawn_black
        ; test_case "Advanced passed pawn worth more" `Quick test_advanced_passed_pawn
        ; test_case "Protected passed pawn stronger" `Quick test_protected_passed_pawn
        ; test_case "Passed pawn race evaluation" `Quick test_passed_pawn_race
        ] )
    ; ( "doubled_pawns"
      , [ test_case "Doubled pawns penalty" `Quick test_doubled_pawns_penalty
        ; test_case "Tripled pawns worse" `Quick test_tripled_pawns_worse
        ] )
    ; ( "isolated_pawns"
      , [ test_case "Isolated pawn penalty" `Quick test_isolated_pawn_penalty ] )
    ; ( "backward_pawns"
      , [ test_case "Backward pawn penalty" `Quick test_backward_pawn_penalty ] )
    ; ( "pawn_chains"
      , [ test_case "Pawn chain bonus" `Quick test_pawn_chain_bonus
        ; test_case "Connected pawns bonus" `Quick test_connected_pawns_bonus
        ; test_case "Central pawns bonus" `Quick test_central_pawns_bonus
        ] )
    ; ( "complex_structures"
      , [ test_case "Complex pawn structure" `Quick test_complex_pawn_structure
        ; test_case "No pawns neutral" `Quick test_no_pawns_neutral
        ] )
    ]
;;
