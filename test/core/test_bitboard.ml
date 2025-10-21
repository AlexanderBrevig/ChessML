(** Unit tests for Bitboard module *)

open Chessml.Core

let test_empty () =
  let bb = Bitboard.empty in
  Alcotest.(check bool) "empty is empty" true (Bitboard.is_empty bb);
  let count = List.length (Bitboard.to_list bb) in
  Alcotest.(check int) "empty count" 0 count
;;

let test_set_and_contains () =
  let bb = Bitboard.empty in
  let bb = Bitboard.set bb Square.e4 in
  Alcotest.(check bool) "contains e4" true (Bitboard.contains bb Square.e4);
  let count = List.length (Bitboard.to_list bb) in
  Alcotest.(check int) "count is 1" 1 count
;;

let test_clear () =
  let bb = Bitboard.of_square Square.e4 in
  let bb = Bitboard.clear bb Square.e4 in
  Alcotest.(check bool) "is empty after clear" true (Bitboard.is_empty bb)
;;

let test_multiple_squares () =
  let bb = Bitboard.empty in
  let bb = Bitboard.set bb Square.e4 in
  let bb = Bitboard.set bb Square.d4 in
  let bb = Bitboard.set bb Square.e5 in
  let count = List.length (Bitboard.to_list bb) in
  Alcotest.(check int) "count is 3" 3 count;
  Alcotest.(check bool) "contains e4" true (Bitboard.contains bb Square.e4);
  Alcotest.(check bool) "contains d4" true (Bitboard.contains bb Square.d4);
  Alcotest.(check bool) "contains e5" true (Bitboard.contains bb Square.e5);
  Alcotest.(check bool) "doesn't contain a1" false (Bitboard.contains bb Square.a1)
;;

let test_to_list () =
  let bb = Bitboard.of_square Square.e4 in
  let squares = Bitboard.to_list bb in
  Alcotest.(check int) "list length is 1" 1 (List.length squares);
  Alcotest.(check bool) "list contains e4" true (List.mem Square.e4 squares)
;;

let () =
  let open Alcotest in
  run
    "Bitboard"
    [ ( "operations"
      , [ test_case "empty" `Quick test_empty
        ; test_case "set and contains" `Quick test_set_and_contains
        ; test_case "clear" `Quick test_clear
        ; test_case "multiple squares" `Quick test_multiple_squares
        ; test_case "to_list" `Quick test_to_list
        ] )
    ]
;;
