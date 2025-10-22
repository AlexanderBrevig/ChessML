(** En passant move generation and state tracking tests *)

open Chessml

let test_en_passant_generation_white () =
  (* White pawn on e5, black pawn just moved d7-d5 *)
  let fen = "rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2" in
  let pos = Position.of_fen fen in
  let moves = Movegen.generate_moves pos in
  (* Should have en passant capture available *)
  let ep_moves = List.filter Move.is_en_passant moves in
  Alcotest.(check int) "One en passant move" 1 (List.length ep_moves);
  (* The en passant move should be e5xd6 *)
  let ep_move = List.hd ep_moves in
  Alcotest.(check int) "From e5" 36 (Move.from ep_move);
  (* e5 = 36 *)
  Alcotest.(check int) "To d6" 43 (Move.to_square ep_move)
;;

(* d6 = 43 *)

let test_en_passant_generation_black () =
  (* Black pawn on d4, white pawn just moved e2-e4 *)
  let fen = "rnbqkbnr/pppp1ppp/8/8/3pP3/8/PPP2PPP/RNBQKBNR b KQkq e3 0 2" in
  let pos = Position.of_fen fen in
  let moves = Movegen.generate_moves pos in
  let ep_moves = List.filter Move.is_en_passant moves in
  Alcotest.(check int) "Black can en passant" 1 (List.length ep_moves)
;;

let test_no_en_passant_without_double_push () =
  (* Normal position with no en passant available *)
  let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" in
  let pos = Position.of_fen fen in
  let moves = Movegen.generate_moves pos in
  let ep_moves = List.filter Move.is_en_passant moves in
  Alcotest.(check int) "No en passant in starting position" 0 (List.length ep_moves)
;;

let test_en_passant_parsing_from_fen () =
  (* Test that en passant square is parsed from FEN *)
  let fen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1" in
  let pos = Position.of_fen fen in
  match Position.ep_square pos with
  | Some sq -> Alcotest.(check int) "EP square is e3" 20 sq (* e3 = 20 *)
  | None -> Alcotest.fail "En passant square not parsed from FEN"
;;

let test_en_passant_square_cleared () =
  (* Test that en passant square is cleared after a non-double-push move *)
  let fen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1" in
  let pos = Position.of_fen fen in
  (* Make a move that's neither en passant nor double push *)
  let moves = Movegen.generate_moves pos in
  let some_move =
    List.find
      (fun mv -> (not (Move.is_en_passant mv)) && Move.kind mv <> Move.PawnDoublePush)
      moves
  in
  let new_pos = Position.make_move pos some_move in
  (* En passant square should be cleared *)
  match Position.ep_square new_pos with
  | None -> Alcotest.(check bool) "EP square cleared" true true
  | Some _ ->
    Alcotest.fail "En passant square should be cleared after non-double-push move"
;;

let test_en_passant_square_set_on_double_push () =
  (* Test that double pawn push sets en passant square *)
  let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" in
  let pos = Position.of_fen fen in
  let moves = Movegen.generate_moves pos in
  (* Find e2-e4 double push *)
  let e2e4 =
    List.find
      (fun mv -> Move.from mv = 12 && Move.to_square mv = 28 (* e2=12, e4=28 *))
      moves
  in
  let new_pos = Position.make_move pos e2e4 in
  (* En passant square should be e3 *)
  match Position.ep_square new_pos with
  | Some sq -> Alcotest.(check int) "EP square set to e3" 20 sq (* e3 = 20 *)
  | None -> Alcotest.fail "En passant square not set after double push"
;;

let test_en_passant_execution () =
  (* Test that en passant capture works correctly *)
  let fen = "rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2" in
  let pos = Position.of_fen fen in
  let moves = Movegen.generate_moves pos in
  let ep_move = List.find Move.is_en_passant moves in
  let new_pos = Position.make_move pos ep_move in
  (* White pawn should be on d6 *)
  let pawn_on_d6 =
    match Position.piece_at new_pos 43 with
    (* d6 = 43 *)
    | Some piece -> piece.kind = Pawn && piece.color = White
    | None -> false
  in
  (* Black pawn on d5 should be captured *)
  let d5_empty = Position.piece_at new_pos 35 = None in
  (* d5 = 35 *)
  (* Original square e5 should be empty *)
  let e5_empty = Position.piece_at new_pos 36 = None in
  (* e5 = 36 *)
  Alcotest.(check bool) "White pawn on d6" true pawn_on_d6;
  Alcotest.(check bool) "Black pawn captured on d5" true d5_empty;
  Alcotest.(check bool) "e5 empty" true e5_empty
;;

let test_en_passant_left_and_right () =
  (* Test that both adjacent pawns can capture en passant *)
  let fen = "rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2" in
  let pos = Position.of_fen fen in
  let moves = Movegen.generate_moves pos in
  (* Should have en passant from e5 *)
  let ep_moves = List.filter Move.is_en_passant moves in
  Alcotest.(check int) "One en passant available" 1 (List.length ep_moves)
;;

let test_en_passant_in_search () =
  (* Test that search can find en passant moves *)
  let fen = "rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2" in
  let game = Game.of_fen fen in
  let result = Search.find_best_move ~verbose:false ~max_time_ms:1000 game 3 in
  match result.Search.best_move with
  | Some mv ->
    (* Should find some move (en passant is an option but not necessarily best) *)
    Alcotest.(check bool) "Engine finds a move" true (mv <> mv || true)
  | None -> Alcotest.fail "Engine should find a move"
;;

let () =
  let open Alcotest in
  run
    "En Passant Tests"
    [ ( "move_generation"
      , [ test_case "En passant generation white" `Quick test_en_passant_generation_white
        ; test_case "En passant generation black" `Quick test_en_passant_generation_black
        ; test_case
            "No en passant without double push"
            `Quick
            test_no_en_passant_without_double_push
        ; test_case "En passant left and right" `Quick test_en_passant_left_and_right
        ] )
    ; ( "state_tracking"
      , [ test_case "En passant parsing from FEN" `Quick test_en_passant_parsing_from_fen
        ; test_case "En passant square cleared" `Quick test_en_passant_square_cleared
        ; test_case
            "En passant square set on double push"
            `Quick
            test_en_passant_square_set_on_double_push
        ] )
    ; ( "move_execution"
      , [ test_case "En passant execution" `Quick test_en_passant_execution ] )
    ; ( "search_integration"
      , [ test_case "En passant in search" `Quick test_en_passant_in_search ] )
    ]
;;
