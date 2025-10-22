(** Castling move generation and evaluation tests *)

open Chessml

let test_castling_move_generation_basic () =
  (* Test basic castling move generation from starting position *)
  let fen = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1" in
  let pos = Position.of_fen fen in
  let moves = Movegen.generate_moves pos in
  (* Filter castling moves *)
  let castling_moves = List.filter Move.is_castle moves in
  Alcotest.(check int) "Two castling moves available" 2 (List.length castling_moves);
  (* Check that both short and long castling are present *)
  let has_short_castle =
    List.exists
      (fun mv ->
         match mv.Move.kind with
         | Move.ShortCastle -> true
         | _ -> false)
      castling_moves
  in
  let has_long_castle =
    List.exists
      (fun mv ->
         match mv.Move.kind with
         | Move.LongCastle -> true
         | _ -> false)
      castling_moves
  in
  Alcotest.(check bool) "Short castle available" true has_short_castle;
  Alcotest.(check bool) "Long castle available" true has_long_castle
;;

let test_castling_blocked_by_pieces () =
  (* Test that castling is not generated when pieces block the path *)
  let fen = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/RN2KB1R w KQkq - 0 1" in
  let pos = Position.of_fen fen in
  let moves = Movegen.generate_moves pos in
  let castling_moves = List.filter Move.is_castle moves in
  Alcotest.(check int) "No castling moves when blocked" 0 (List.length castling_moves)
;;

let test_castling_king_in_check () =
  (* Test that castling is not generated when king is in check *)
  let fen = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R2qK2R w KQkq - 0 1" in
  let pos = Position.of_fen fen in
  let moves = Movegen.generate_moves pos in
  let castling_moves = List.filter Move.is_castle moves in
  Alcotest.(check int) "No castling when in check" 0 (List.length castling_moves)
;;

let test_castling_through_attack () =
  (* Test that castling is not generated when king would pass through attack *)
  let fen = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K1qR w KQkq - 0 1" in
  let pos = Position.of_fen fen in
  let moves = Movegen.generate_moves pos in
  let castling_moves = List.filter Move.is_castle moves in
  (* Short castling should be blocked, long castling might be available *)
  let has_short_castle =
    List.exists
      (fun mv ->
         match mv.Move.kind with
         | Move.ShortCastle -> true
         | _ -> false)
      castling_moves
  in
  Alcotest.(check bool) "No short castle through attack" false has_short_castle
;;

let test_castling_rights_lost () =
  (* Test positions where castling rights are lost *)
  let fen_no_rights = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w - - 0 1" in
  let pos = Position.of_fen fen_no_rights in
  let moves = Movegen.generate_moves pos in
  let castling_moves = List.filter Move.is_castle moves in
  Alcotest.(check int) "No castling without rights" 0 (List.length castling_moves)
;;

let test_castling_only_kingside () =
  (* Test position with only kingside castling rights *)
  let fen = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w K - 0 1" in
  let pos = Position.of_fen fen in
  let moves = Movegen.generate_moves pos in
  let castling_moves = List.filter Move.is_castle moves in
  Alcotest.(check int) "One castling move with K rights" 1 (List.length castling_moves);
  let has_short_castle =
    List.exists
      (fun mv ->
         match mv.Move.kind with
         | Move.ShortCastle -> true
         | _ -> false)
      castling_moves
  in
  Alcotest.(check bool) "Only short castle available" true has_short_castle
;;

let test_castling_only_queenside () =
  (* Test position with only queenside castling rights *)
  let fen = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w Q - 0 1" in
  let pos = Position.of_fen fen in
  let moves = Movegen.generate_moves pos in
  let castling_moves = List.filter Move.is_castle moves in
  Alcotest.(check int) "One castling move with Q rights" 1 (List.length castling_moves);
  let has_long_castle =
    List.exists
      (fun mv ->
         match mv.Move.kind with
         | Move.LongCastle -> true
         | _ -> false)
      castling_moves
  in
  Alcotest.(check bool) "Only long castle available" true has_long_castle
;;

let test_castling_black_moves () =
  (* Test that black can also castle *)
  let fen = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R b kq - 0 1" in
  let pos = Position.of_fen fen in
  let moves = Movegen.generate_moves pos in
  let castling_moves = List.filter Move.is_castle moves in
  Alcotest.(check int) "Black has two castling moves" 2 (List.length castling_moves)
;;

let test_castling_move_execution () =
  (* Test that castling moves can be executed properly *)
  let fen = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1" in
  let pos = Position.of_fen fen in
  let moves = Movegen.generate_moves pos in
  let castling_moves = List.filter Move.is_castle moves in
  (* Find short castle move *)
  let short_castle =
    List.find
      (fun mv ->
         match mv.Move.kind with
         | Move.ShortCastle -> true
         | _ -> false)
      castling_moves
  in
  (* Execute the move *)
  let new_pos = Position.make_move pos short_castle in
  (* Check that king and rook moved correctly *)
  let king_on_g1 =
    match Position.piece_at new_pos 6 with
    (* g1 *)
    | Some piece -> piece.kind = King && piece.color = White
    | None -> false
  in
  let rook_on_f1 =
    match Position.piece_at new_pos 5 with
    (* f1 *)
    | Some piece -> piece.kind = Rook && piece.color = White
    | None -> false
  in
  let king_not_on_e1 = Position.piece_at new_pos 4 = None in
  (* e1 *)
  let rook_not_on_h1 = Position.piece_at new_pos 7 = None in
  (* h1 *)
  Alcotest.(check bool) "King moved to g1" true king_on_g1;
  Alcotest.(check bool) "Rook moved to f1" true rook_on_f1;
  Alcotest.(check bool) "King not on e1" true king_not_on_e1;
  Alcotest.(check bool) "Rook not on h1" true rook_not_on_h1
;;

let test_castling_priority_in_search () =
  (* Test that castling gets high priority in search *)
  let fen = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1" in
  let game = Game.of_fen fen in
  (* Search for best move with shallow depth *)
  let result = Search.find_best_move ~verbose:false ~max_time_ms:1000 game 3 in
  match result.Search.best_move with
  | Some mv when Move.is_castle mv ->
    Alcotest.(check bool) "Engine chooses to castle" true true
  | Some mv ->
    (* Print what move was chosen for debugging *)
    Printf.printf "Engine chose: %s instead of castling\n" (Move.to_uci mv);
    Alcotest.(check bool) "Engine should prefer castling" true (Move.is_castle mv)
  | None -> Alcotest.fail "Engine should find a move"
;;

let () =
  let open Alcotest in
  run
    "Castling Tests"
    [ ( "move_generation"
      , [ test_case "Basic castling generation" `Quick test_castling_move_generation_basic
        ; test_case "Castling blocked by pieces" `Quick test_castling_blocked_by_pieces
        ; test_case "No castling when king in check" `Quick test_castling_king_in_check
        ; test_case "No castling through attack" `Quick test_castling_through_attack
        ; test_case "No castling without rights" `Quick test_castling_rights_lost
        ; test_case "Only kingside castling" `Quick test_castling_only_kingside
        ; test_case "Only queenside castling" `Quick test_castling_only_queenside
        ; test_case "Black castling moves" `Quick test_castling_black_moves
        ] )
    ; ( "move_execution"
      , [ test_case "Castling move execution" `Quick test_castling_move_execution ] )
    ; ( "search_integration"
      , [ test_case "Castling priority in search" `Slow test_castling_priority_in_search ]
      )
    ]
;;
