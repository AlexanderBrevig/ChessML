(** Unit tests for Square module *)

open Chessml.Core

let test_square_make () =
  let e4 = Square.make Types.FileE Types.Rank4 in
  Alcotest.(check string) "e4 to UCI" "e4" (Square.to_uci e4)
;;

let test_square_of_uci () =
  let a1 = Square.of_uci "a1" in
  Alcotest.(check int) "a1 index" 0 (Square.to_int a1);
  let h8 = Square.of_uci "h8" in
  Alcotest.(check int) "h8 index" 63 (Square.to_int h8)
;;

let test_square_e4 () =
  let e4 = Square.of_uci "e4" in
  Alcotest.(check int) "e4 is square 28" 28 (Square.to_int e4)
;;

let test_square_roundtrip () =
  for i = 0 to 63 do
    let sq = Square.of_int i in
    let sq_str = Square.to_uci sq in
    let sq2 = Square.of_uci sq_str in
    Alcotest.(check int) (Printf.sprintf "Square %d roundtrip" i) i (Square.to_int sq2)
  done
;;

let () =
  let open Alcotest in
  run
    "Square"
    [ ( "basic"
      , [ test_case "make" `Quick test_square_make
        ; test_case "of_uci" `Quick test_square_of_uci
        ; test_case "e4" `Quick test_square_e4
        ; test_case "roundtrip" `Quick test_square_roundtrip
        ] )
    ]
;;
