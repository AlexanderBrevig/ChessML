(** Integration tests for ChessML *)

open Chessml

let test_color () =
  Alcotest.(check bool) "White is white" true (Types.Color.is_white Types.White);
  Alcotest.(check bool) "Black is black" true (Types.Color.is_black Types.Black);
  let opponent_white = Types.Color.opponent Types.White in
  Alcotest.(check bool) "Opponent of white is black" true (opponent_white = Types.Black)
;;

let test_square () =
  let e4 = Square.make Types.FileE Types.Rank4 in
  Alcotest.(check string) "e4 to UCI" "e4" (Square.to_uci e4);
  let a1 = Square.of_uci "a1" in
  Alcotest.(check int) "a1 index" 0 (Square.to_int a1);
  let h8 = Square.of_uci "h8" in
  Alcotest.(check int) "h8 index" 63 (Square.to_int h8)
;;

let test_bitboard () =
  let empty = Bitboard.empty in
  Alcotest.(check bool) "Empty bitboard is empty" true (Bitboard.is_empty empty);
  let bb = Bitboard.of_square Square.e4 in
  Alcotest.(check bool) "Contains e4" true (Bitboard.contains bb Square.e4);
  Alcotest.(check bool) "Doesn't contain a1" false (Bitboard.contains bb Square.a1);
  Alcotest.(check int) "Population is 1" 1 (Bitboard.population bb)
;;

let test_piece () =
  let white_pawn = Types.Piece.white_pawn in
  Alcotest.(check bool) "White pawn is white" true (Types.Piece.is_white white_pawn);
  Alcotest.(check bool) "White pawn is a pawn" true (Types.Piece.is_pawn white_pawn);
  Alcotest.(check string) "White pawn char" "P" (Types.Piece.to_string white_pawn)
;;

let test_move () =
  let e2e4 = Move.make Square.e2 Square.e4 Move.Quiet in
  Alcotest.(check string) "e2e4 to UCI" "e2e4" (Move.to_uci e2e4);
  Alcotest.(check bool) "e2e4 is quiet" true (Move.is_quiet e2e4);
  Alcotest.(check bool) "e2e4 is not capture" false (Move.is_capture e2e4)
;;

let test_position () =
  let pos = Position.default () in
  Alcotest.(check bool)
    "Side to move is white"
    true
    (Types.Color.is_white (Position.side_to_move pos));
  Alcotest.(check int) "Halfmove is 0" 0 (Position.halfmove pos);
  Alcotest.(check int) "Fullmove is 1" 1 (Position.fullmove pos)
;;

let test_game () =
  let game = Game.default () in
  Alcotest.(check string) "Game FEN" Position.fen_startpos (Game.to_fen game)
;;

let test_draw_starting_position () =
  let game = Game.default () in
  Alcotest.(check bool) "Starting position is not a draw" false (Game.is_draw game)
;;

let test_draw_insufficient_material_k_vs_k () =
  let fen = "8/8/8/4k3/8/8/4K3/8 w - - 0 1" in
  let game = Game.from_fen fen in
  Alcotest.(check bool) "King vs King is a draw" true (Game.is_draw game)
;;

let test_draw_insufficient_material_kn_vs_k () =
  let fen = "8/8/8/4k3/8/8/4KN2/8 w - - 0 1" in
  let game = Game.from_fen fen in
  Alcotest.(check bool) "King+Knight vs King is a draw" true (Game.is_draw game)
;;

let test_draw_insufficient_material_kb_vs_k () =
  let fen = "8/8/8/4k3/8/8/4KB2/8 w - - 0 1" in
  let game = Game.from_fen fen in
  Alcotest.(check bool) "King+Bishop vs King is a draw" true (Game.is_draw game)
;;

let test_draw_insufficient_material_k_vs_kn () =
  let fen = "8/8/8/4kn2/8/8/4K3/8 w - - 0 1" in
  let game = Game.from_fen fen in
  Alcotest.(check bool) "King vs King+Knight is a draw" true (Game.is_draw game)
;;

let test_draw_insufficient_material_kb_vs_kb () =
  let fen = "8/8/8/4kb2/8/8/4KB2/8 w - - 0 1" in
  let game = Game.from_fen fen in
  Alcotest.(check bool) "King+Bishop vs King+Bishop is a draw" true (Game.is_draw game)
;;

let test_draw_sufficient_material_with_pawn () =
  let fen = "8/8/8/4k3/8/8/4KP2/8 w - - 0 1" in
  let game = Game.from_fen fen in
  Alcotest.(check bool) "King+Pawn vs King is not a draw" false (Game.is_draw game)
;;

let test_draw_sufficient_material_with_rook () =
  let fen = "8/8/8/4k3/8/8/4KR2/8 w - - 0 1" in
  let game = Game.from_fen fen in
  Alcotest.(check bool) "King+Rook vs King is not a draw" false (Game.is_draw game)
;;

let test_draw_fifty_move_rule () =
  let fen = "8/8/8/4k3/8/8/4K3/8 w - - 100 1" in
  let game = Game.from_fen fen in
  Alcotest.(check bool) "Fifty-move rule triggers draw" true (Game.is_draw game)
;;

let test_draw_not_fifty_move_rule () =
  let fen = "8/8/8/4k3/8/8/4KP2/8 w - - 99 1" in
  let game = Game.from_fen fen in
  Alcotest.(check bool) "49.5 moves is not a draw" false (Game.is_draw game)
;;

let test_draw_stalemate () =
  (* Classic stalemate: King on a8, white king on c8, white queen on d7, black to move *)
  let fen = "k1K5/3Q4/8/8/8/8/8/8 b - - 0 1" in
  let game = Game.from_fen fen in
  Alcotest.(check bool) "Stalemate is a draw" true (Game.is_draw game)
;;

let test_not_draw_checkmate () =
  let fen = "7k/6Q1/6K1/8/8/8/8/8 b - - 0 1" in
  let game = Game.from_fen fen in
  Alcotest.(check bool) "Checkmate is not a draw" false (Game.is_draw game)
;;

let test_zobrist_starting_position () =
  let pos = Position.default () in
  let key = Zobrist.compute pos in
  Alcotest.(check bool) "Starting position hash is non-zero" true (key <> 0L)
;;

let test_zobrist_different_positions () =
  let pos1 = Position.default () in
  let pos2 =
    Position.of_fen "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
  in
  let key1 = Zobrist.compute pos1 in
  let key2 = Zobrist.compute pos2 in
  Alcotest.(check bool) "Different positions have different hashes" true (key1 <> key2)
;;

let test_zobrist_same_position () =
  let pos1 = Position.default () in
  let pos2 = Position.default () in
  let key1 = Zobrist.compute pos1 in
  let key2 = Zobrist.compute pos2 in
  Alcotest.(check bool) "Same positions have same hash" true (key1 = key2)
;;

let test_zobrist_side_to_move () =
  let pos_white =
    Position.of_fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  in
  let pos_black =
    Position.of_fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1"
  in
  let key_white = Zobrist.compute pos_white in
  let key_black = Zobrist.compute pos_black in
  Alcotest.(check bool)
    "Different side to move yields different hash"
    true
    (key_white <> key_black)
;;

let test_zobrist_hash_piece () =
  let key = 0L in
  let key_with_piece = Zobrist.hash_piece key Square.e4 Types.Piece.white_pawn in
  Alcotest.(check bool) "Hashing a piece changes the key" true (key <> key_with_piece);
  (* Hashing the same piece again should return to original *)
  let key_unhashed = Zobrist.hash_piece key_with_piece Square.e4 Types.Piece.white_pawn in
  Alcotest.(check bool)
    "Hashing same piece twice returns to original"
    true
    (key = key_unhashed)
;;

let test_zobrist_hash_side_to_move () =
  let key = 0L in
  let key_black = Zobrist.hash_side_to_move key Types.Black in
  Alcotest.(check bool) "Hashing Black changes the key" true (key <> key_black);
  let key_white = Zobrist.hash_side_to_move key Types.White in
  Alcotest.(check bool) "Hashing White doesn't change the key" true (key = key_white)
;;

let test_repetition_none () =
  let game = Game.default () in
  Alcotest.(check bool)
    "Starting position is not a repetition"
    false
    (Game.is_repetition game)
;;

let test_repetition_simple () =
  (* Play Nb1-a3, Nb8-a6, Na3-b1, Na6-b8 to return to start *)
  let game = Game.default () in
  let mv1 = Move.of_uci "b1a3" in
  let game = Game.make_move game mv1 in
  Alcotest.(check bool) "After 1 move, no repetition" false (Game.is_repetition game);
  let mv2 = Move.of_uci "b8a6" in
  let game = Game.make_move game mv2 in
  Alcotest.(check bool) "After 2 moves, no repetition" false (Game.is_repetition game);
  let mv3 = Move.of_uci "a3b1" in
  let game = Game.make_move game mv3 in
  Alcotest.(check bool) "After 3 moves, no repetition" false (Game.is_repetition game);
  let mv4 = Move.of_uci "a6b8" in
  let game = Game.make_move game mv4 in
  Alcotest.(check bool)
    "After returning to start, it's a repetition"
    true
    (Game.is_repetition game)
;;

let test_threefold_repetition () =
  (* Play moves to create a threefold repetition *)
  let game = Game.default () in
  let mv1 = Move.of_uci "b1a3" in
  let mv2 = Move.of_uci "b8a6" in
  let mv3 = Move.of_uci "a3b1" in
  let mv4 = Move.of_uci "a6b8" in
  let game = Game.make_move game mv1 in
  let game = Game.make_move game mv2 in
  let game = Game.make_move game mv3 in
  let game = Game.make_move game mv4 in
  Alcotest.(check bool)
    "After 1st cycle, not threefold"
    false
    (Game.is_threefold_repetition game);
  (* Repeat the sequence *)
  let game = Game.make_move game mv1 in
  let game = Game.make_move game mv2 in
  let game = Game.make_move game mv3 in
  let game = Game.make_move game mv4 in
  Alcotest.(check bool)
    "After 2nd cycle, not threefold yet"
    false
    (Game.is_threefold_repetition game);
  (* Repeat once more *)
  let game = Game.make_move game mv1 in
  let game = Game.make_move game mv2 in
  let game = Game.make_move game mv3 in
  let game = Game.make_move game mv4 in
  Alcotest.(check bool)
    "After 3rd cycle, threefold repetition"
    true
    (Game.is_threefold_repetition game)
;;

let test_draw_threefold_repetition () =
  (* Threefold repetition should trigger a draw *)
  let game = Game.default () in
  let mv1 = Move.of_uci "b1a3" in
  let mv2 = Move.of_uci "b8a6" in
  let mv3 = Move.of_uci "a3b1" in
  let mv4 = Move.of_uci "a6b8" in
  (* Repeat 3 times *)
  let game = Game.make_move game mv1 in
  let game = Game.make_move game mv2 in
  let game = Game.make_move game mv3 in
  let game = Game.make_move game mv4 in
  let game = Game.make_move game mv1 in
  let game = Game.make_move game mv2 in
  let game = Game.make_move game mv3 in
  let game = Game.make_move game mv4 in
  let game = Game.make_move game mv1 in
  let game = Game.make_move game mv2 in
  let game = Game.make_move game mv3 in
  let game = Game.make_move game mv4 in
  Alcotest.(check bool) "Threefold repetition is a draw" true (Game.is_draw game)
;;

let () =
  let open Alcotest in
  run
    "Chessml"
    [ ( "types"
      , [ test_case "Color" `Quick test_color
        ; test_case "Square" `Quick test_square
        ; test_case "Bitboard" `Quick test_bitboard
        ; test_case "Piece" `Quick test_piece
        ] )
    ; "moves", [ test_case "Move" `Quick test_move ]
    ; "position", [ test_case "Position" `Quick test_position ]
    ; "game", [ test_case "Game" `Quick test_game ]
    ; ( "zobrist"
      , [ test_case
            "Starting position has non-zero hash"
            `Quick
            test_zobrist_starting_position
        ; test_case
            "Different positions have different hashes"
            `Quick
            test_zobrist_different_positions
        ; test_case "Same positions have same hash" `Quick test_zobrist_same_position
        ; test_case "Side to move affects hash" `Quick test_zobrist_side_to_move
        ; test_case "hash_piece is reversible" `Quick test_zobrist_hash_piece
        ; test_case
            "hash_side_to_move works correctly"
            `Quick
            test_zobrist_hash_side_to_move
        ] )
    ; ( "repetition"
      , [ test_case "Starting position is not a repetition" `Quick test_repetition_none
        ; test_case "Simple repetition detection" `Quick test_repetition_simple
        ; test_case "Threefold repetition detection" `Quick test_threefold_repetition
        ] )
    ; ( "draw"
      , [ test_case "Starting position is not a draw" `Quick test_draw_starting_position
        ; test_case "K vs K is a draw" `Quick test_draw_insufficient_material_k_vs_k
        ; test_case "KN vs K is a draw" `Quick test_draw_insufficient_material_kn_vs_k
        ; test_case "KB vs K is a draw" `Quick test_draw_insufficient_material_kb_vs_k
        ; test_case "K vs KN is a draw" `Quick test_draw_insufficient_material_k_vs_kn
        ; test_case "KB vs KB is a draw" `Quick test_draw_insufficient_material_kb_vs_kb
        ; test_case "KP vs K is not a draw" `Quick test_draw_sufficient_material_with_pawn
        ; test_case "KR vs K is not a draw" `Quick test_draw_sufficient_material_with_rook
        ; test_case "Fifty-move rule" `Quick test_draw_fifty_move_rule
        ; test_case "Not fifty-move rule at 99" `Quick test_draw_not_fifty_move_rule
        ; test_case "Stalemate is a draw" `Quick test_draw_stalemate
        ; test_case "Checkmate is not a draw" `Quick test_not_draw_checkmate
        ; test_case "Threefold repetition is a draw" `Quick test_draw_threefold_repetition
        ] )
    ]
;;
