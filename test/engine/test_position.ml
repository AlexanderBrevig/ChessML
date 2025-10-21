(** Unit tests for Position module *)

open Chessml.Engine

let test_default_position () =
  let pos = Position.default () in
  Alcotest.(check bool)
    "Side to move is white"
    true
    (Chessml.Types.Color.is_white (Position.side_to_move pos));
  Alcotest.(check int) "Halfmove is 0" 0 (Position.halfmove pos);
  Alcotest.(check int) "Fullmove is 1" 1 (Position.fullmove pos)
;;

let test_fen_parsing () =
  let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" in
  let pos = Position.of_fen fen in
  let fen2 = Position.to_fen pos in
  Alcotest.(check string) "FEN roundtrip" fen fen2
;;

let test_piece_at () =
  let pos = Position.default () in
  let piece_e2 = Position.piece_at pos Chessml.Square.e2 in
  match piece_e2 with
  | Some p ->
    Alcotest.(check bool)
      "e2 has white pawn"
      true
      (p.Chessml.Types.color = Chessml.Types.White && p.kind = Chessml.Types.Pawn)
  | None -> Alcotest.fail "e2 should have a piece"
;;

let () =
  let open Alcotest in
  run
    "Position"
    [ ( "basic"
      , [ test_case "Default position" `Quick test_default_position
        ; test_case "FEN parsing" `Quick test_fen_parsing
        ; test_case "piece_at" `Quick test_piece_at
        ] )
    ]
;;
