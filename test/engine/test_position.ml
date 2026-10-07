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

let test_fen_roundtrip_rights () =
  List.iter
    (fun fen -> Alcotest.(check string) fen fen (Position.to_fen (Position.of_fen fen)))
    [ "4k3/8/8/8/3pP3/8/8/4K3 b - e3 0 1"
    ; "r3k2r/8/8/8/8/8/8/R3K2R w Kq - 12 40"
    ; "r3k2r/8/8/8/8/8/8/R3K2R b Qk - 0 1"
    ]
;;

let test_halfmove_clock () =
  let pos = Position.of_fen "4k3/8/8/3p4/4P3/8/8/R3K3 w - - 7 20" in
  let after mv_str =
    let mv =
      List.find (fun mv -> Chessml.Move.to_uci mv = mv_str) (Movegen.generate_moves pos)
    in
    Position.halfmove (Position.make_move pos mv)
  in
  Alcotest.(check int) "quiet move increments" 8 (after "a1a2");
  Alcotest.(check int) "capture resets" 0 (after "e4d5");
  Alcotest.(check int) "pawn push resets" 0 (after "e4e5")
;;

let () =
  let open Alcotest in
  run
    "Position"
    [ ( "basic"
      , [ test_case "Default position" `Quick test_default_position
        ; test_case "FEN parsing" `Quick test_fen_parsing
        ; test_case "piece_at" `Quick test_piece_at
        ; test_case "FEN roundtrip castling and ep" `Quick test_fen_roundtrip_rights
        ; test_case "Halfmove clock" `Quick test_halfmove_clock
        ] )
    ]
;;
