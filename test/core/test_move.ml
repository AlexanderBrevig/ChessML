(** Unit tests for Move module *)

open Chessml.Core

let test_move_uci () =
  let e2e4 = Move.make Square.e2 Square.e4 Move.Quiet in
  Alcotest.(check string) "e2e4 to UCI" "e2e4" (Move.to_uci e2e4)
;;

let test_move_quiet () =
  let e2e4 = Move.make Square.e2 Square.e4 Move.Quiet in
  Alcotest.(check bool) "e2e4 is quiet" true (Move.is_quiet e2e4);
  Alcotest.(check bool) "e2e4 is not capture" false (Move.is_capture e2e4)
;;

let test_move_capture () =
  let exd5 = Move.make Square.e4 Square.d5 Move.Capture in
  Alcotest.(check bool) "exd5 is capture" true (Move.is_capture exd5);
  Alcotest.(check bool) "exd5 is not quiet" false (Move.is_quiet exd5)
;;

let test_move_from_to () =
  let mv = Move.make Square.e2 Square.e4 Move.Quiet in
  Alcotest.(check int) "from is e2" 12 (Square.to_int (Move.from mv));
  Alcotest.(check int) "to is e4" 28 (Square.to_int (Move.to_square mv))
;;

let test_move_of_uci () =
  let mv = Move.of_uci "e2e4" in
  Alcotest.(check string) "roundtrip" "e2e4" (Move.to_uci mv)
;;

let test_promotion () =
  let e7e8q = Move.of_uci "e7e8q" in
  Alcotest.(check bool) "is promotion" true (Move.is_promotion e7e8q);
  Alcotest.(check string) "to uci includes q" "e7e8q" (Move.to_uci e7e8q)
;;

let () =
  let open Alcotest in
  run
    "Move"
    [ ( "basic"
      , [ test_case "UCI conversion" `Quick test_move_uci
        ; test_case "Quiet move" `Quick test_move_quiet
        ; test_case "Capture" `Quick test_move_capture
        ; test_case "From/To" `Quick test_move_from_to
        ; test_case "of_uci" `Quick test_move_of_uci
        ] )
    ; "special", [ test_case "Promotion" `Quick test_promotion ]
    ]
;;
