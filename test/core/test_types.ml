(** Unit tests for Types module *)

open Chessml.Core

let test_color_is_white () =
  Alcotest.(check bool) "White is white" true (Types.Color.is_white Types.White)
;;

let test_color_is_black () =
  Alcotest.(check bool) "Black is black" true (Types.Color.is_black Types.Black)
;;

let test_color_opponent () =
  let opponent_white = Types.Color.opponent Types.White in
  Alcotest.(check bool) "Opponent of white is black" true (opponent_white = Types.Black);
  let opponent_black = Types.Color.opponent Types.Black in
  Alcotest.(check bool) "Opponent of black is white" true (opponent_black = Types.White)
;;

let test_piece_creation () =
  let white_pawn = { Types.color = Types.White; kind = Types.Pawn } in
  Alcotest.(check bool) "White pawn is white" true (Types.Color.is_white white_pawn.color);
  Alcotest.(check bool) "Pawn kind matches" true (white_pawn.kind = Types.Pawn)
;;

let () =
  let open Alcotest in
  run
    "Types"
    [ ( "color"
      , [ test_case "White is white" `Quick test_color_is_white
        ; test_case "Black is black" `Quick test_color_is_black
        ; test_case "Color opponent" `Quick test_color_opponent
        ] )
    ; "piece", [ test_case "Piece creation" `Quick test_piece_creation ]
    ]
;;
