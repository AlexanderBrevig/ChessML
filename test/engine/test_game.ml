(** Unit tests for Game module *)

open Chessml.Engine

let test_default_game () =
  let game = Game.default () in
  let pos = Game.position game in
  Alcotest.(check bool)
    "Starting position white to move"
    true
    (Chessml.Types.Color.is_white (Position.side_to_move pos))
;;

let test_game_moves () =
  let game = Game.default () in
  let moves = Game.legal_moves game in
  Alcotest.(check bool) "Has legal moves" true (List.length moves > 0);
  (* Starting position should have 20 legal moves *)
  Alcotest.(check int) "Exactly 20 moves" 20 (List.length moves)
;;

let test_make_move () =
  let game = Game.default () in
  let moves = Game.legal_moves game in
  match moves with
  | mv :: _ ->
    let game2 = Game.make_move game mv in
    let pos2 = Game.position game2 in
    Alcotest.(check bool)
      "Side switched after move"
      true
      (Chessml.Types.Color.is_black (Position.side_to_move pos2))
  | [] -> Alcotest.fail "Should have moves"
;;

let test_draw_starting_position () =
  let game = Game.default () in
  Alcotest.(check bool) "Starting position is not a draw" false (Game.is_draw game)
;;

let test_draw_insufficient_material () =
  (* K vs K *)
  let game = Game.of_fen "8/8/8/4k3/8/8/4K3/8 w - - 0 1" in
  Alcotest.(check bool) "K vs K is a draw" true (Game.is_draw game);
  (* KN vs K *)
  let game = Game.of_fen "8/8/8/4k3/8/8/4KN2/8 w - - 0 1" in
  Alcotest.(check bool) "KN vs K is a draw" true (Game.is_draw game);
  (* KB vs K *)
  let game = Game.of_fen "8/8/8/4k3/8/8/4KB2/8 w - - 0 1" in
  Alcotest.(check bool) "KB vs K is a draw" true (Game.is_draw game)
;;

let test_draw_fifty_move_rule () =
  (* Fifty-move rule: halfmove clock >= 100 (50 moves = 100 half-moves) *)
  let game = Game.of_fen "8/8/8/4k3/8/8/4K3/8 w - - 100 1" in
  Alcotest.(check bool) "Fifty-move rule at 100" true (Game.is_draw game)
;;

let test_draw_stalemate () =
  (* Classic stalemate: King on a8, white king on c8, white queen on d7, black to move *)
  let game = Game.of_fen "k1K5/3Q4/8/8/8/8/8/8 b - - 0 1" in
  Alcotest.(check bool) "Stalemate is a draw" true (Game.is_draw game)
;;

let test_not_draw_checkmate () =
  (* Checkmate position *)
  let game = Game.of_fen "7k/6Q1/6K1/8/8/8/8/8 b - - 0 1" in
  Alcotest.(check bool) "Checkmate is not a draw" false (Game.is_draw game)
;;

(* Castling tests *)
let test_castling_fen_loading () =
  (* Test that FEN positions with castling rights load correctly *)
  let game = Game.of_fen "r3k2r/1pp3p1/8/p4p2/5q2/4p1pP/PPP1P1R1/RQ2KB2 b kq - 1 29" in
  let pos = Game.position game in
  let fen_output = Position.to_fen pos in
  (* Check that position loaded correctly *)
  Alcotest.(check bool)
    "Black to move after loading"
    true
    (Chessml.Types.Color.is_black (Position.side_to_move pos));
  (* Check that FEN serialization works *)
  Alcotest.(check bool) "FEN contains black king" true (String.contains fen_output 'k');
  Alcotest.(check bool) "FEN contains rooks" true (String.contains fen_output 'r');
  (* Check side to move in FEN *)
  Alcotest.(check bool) "FEN shows black to move" true (String.contains fen_output 'b')
;;

let test_castling_move_creation () =
  (* Test creating castling moves *)
  let white_kingside =
    Chessml.Move.make
      (Chessml.Square.of_uci "e1")
      (Chessml.Square.of_uci "g1")
      Chessml.Move.ShortCastle
  in
  let white_queenside =
    Chessml.Move.make
      (Chessml.Square.of_uci "e1")
      (Chessml.Square.of_uci "c1")
      Chessml.Move.LongCastle
  in
  let black_kingside =
    Chessml.Move.make
      (Chessml.Square.of_uci "e8")
      (Chessml.Square.of_uci "g8")
      Chessml.Move.ShortCastle
  in
  let black_queenside =
    Chessml.Move.make
      (Chessml.Square.of_uci "e8")
      (Chessml.Square.of_uci "c8")
      Chessml.Move.LongCastle
  in
  (* Verify move types *)
  Alcotest.(check bool)
    "White kingside is castle"
    true
    (Chessml.Move.is_castle white_kingside);
  Alcotest.(check bool)
    "White queenside is castle"
    true
    (Chessml.Move.is_castle white_queenside);
  Alcotest.(check bool)
    "Black kingside is castle"
    true
    (Chessml.Move.is_castle black_kingside);
  Alcotest.(check bool)
    "Black queenside is castle"
    true
    (Chessml.Move.is_castle black_queenside)
;;

let test_castling_turn_switching () =
  (* Test that castling properly switches turns *)
  let game = Game.of_fen "r3k2r/1pp3p1/8/p4p2/5q2/4p1pP/PPP1P1R1/RQ2KB2 b kq - 1 29" in
  let pos_before = Game.position game in
  (* Verify black to move initially *)
  Alcotest.(check bool)
    "Initially black to move"
    true
    (Chessml.Types.Color.is_black (Position.side_to_move pos_before));
  (* Create and apply castling move *)
  let castling_move =
    Chessml.Move.make
      (Chessml.Square.of_uci "e8")
      (Chessml.Square.of_uci "c8")
      Chessml.Move.LongCastle
  in
  let game_after = Game.make_move game castling_move in
  let pos_after = Game.position game_after in
  (* Verify white to move after castling *)
  Alcotest.(check bool)
    "White to move after castling"
    true
    (Chessml.Types.Color.is_white (Position.side_to_move pos_after))
;;

let test_castling_piece_positions () =
  (* Test that castling moves pieces correctly *)
  let game = Game.of_fen "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1" in
  (* White kingside castling *)
  let wk_castle =
    Chessml.Move.make
      (Chessml.Square.of_uci "e1")
      (Chessml.Square.of_uci "g1")
      Chessml.Move.ShortCastle
  in
  let game_after_wk = Game.make_move game wk_castle in
  let pos_after_wk = Game.position game_after_wk in
  (* Check that king moved to g1 *)
  let king_on_g1 = Position.piece_at pos_after_wk (Chessml.Square.of_uci "g1") in
  Alcotest.(check bool)
    "King on g1 after O-O"
    true
    (match king_on_g1 with
     | Some piece -> piece.kind = Chessml.Types.King && piece.color = Chessml.Types.White
     | None -> false);
  (* Check that rook moved to f1 *)
  let rook_on_f1 = Position.piece_at pos_after_wk (Chessml.Square.of_uci "f1") in
  Alcotest.(check bool)
    "Rook on f1 after O-O"
    true
    (match rook_on_f1 with
     | Some piece -> piece.kind = Chessml.Types.Rook && piece.color = Chessml.Types.White
     | None -> false);
  (* Check that e1 and h1 are empty *)
  let e1_empty = Position.piece_at pos_after_wk (Chessml.Square.of_uci "e1") in
  let h1_empty = Position.piece_at pos_after_wk (Chessml.Square.of_uci "h1") in
  Alcotest.(check bool) "e1 empty after O-O" true (e1_empty = None);
  Alcotest.(check bool) "h1 empty after O-O" true (h1_empty = None)
;;

let test_castling_move_counter () =
  (* Test that move counters update correctly after castling *)
  let game = Game.of_fen "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1" in
  let pos_before = Game.position game in
  let castling_move =
    Chessml.Move.make
      (Chessml.Square.of_uci "e1")
      (Chessml.Square.of_uci "c1")
      Chessml.Move.LongCastle
  in
  let game_after = Game.make_move game castling_move in
  let pos_after = Game.position game_after in
  (* Check that fullmove counter incremented (since black to move now) *)
  let fullmove_before = Position.fullmove pos_before in
  let fullmove_after = Position.fullmove pos_after in
  (* Move counter should stay the same since it was white's move *)
  Alcotest.(check int)
    "Fullmove stays same after white move"
    fullmove_before
    fullmove_after;
  (* Halfmove should increment *)
  let halfmove_before = Position.halfmove pos_before in
  let halfmove_after = Position.halfmove pos_after in
  Alcotest.(check int) "Halfmove increments" (halfmove_before + 1) halfmove_after
;;

let () =
  let open Alcotest in
  run
    "Game"
    [ ( "basic"
      , [ test_case "Default game" `Quick test_default_game
        ; test_case "Legal moves" `Quick test_game_moves
        ; test_case "Make move" `Quick test_make_move
        ] )
    ; ( "draw"
      , [ test_case "Starting position is not a draw" `Quick test_draw_starting_position
        ; test_case "Insufficient material" `Quick test_draw_insufficient_material
        ; test_case "Fifty-move rule" `Quick test_draw_fifty_move_rule
        ; test_case "Stalemate" `Quick test_draw_stalemate
        ; test_case "Checkmate is not draw" `Quick test_not_draw_checkmate
        ] )
    ; ( "castling"
      , [ test_case "FEN loading" `Quick test_castling_fen_loading
        ; test_case "Move creation" `Quick test_castling_move_creation
        ; test_case "Turn switching" `Quick test_castling_turn_switching
        ; test_case "Piece positions" `Quick test_castling_piece_positions
        ; test_case "Move counter" `Quick test_castling_move_counter
        ] )
    ]
;;
