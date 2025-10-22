(** Promotion move generation and handling tests *)

open Chessml

let test_basic_promotion_generation () =
  (* White pawn on 7th rank ready to promote *)
  let fen = "8/P7/8/8/8/8/8/8 w - - 0 1" in
  let pos = Position.of_fen fen in
  let moves = Movegen.generate_moves pos in
  (* Should generate 4 promotion moves (Q, R, B, N) *)
  let promotion_moves = List.filter Move.is_promotion moves in
  Alcotest.(check int) "Four promotion moves" 4 (List.length promotion_moves);
  (* Check all promotion types are present *)
  let has_queen =
    List.exists
      (fun mv ->
         match Move.promotion mv with
         | Some Queen -> true
         | _ -> false)
      promotion_moves
  in
  let has_rook =
    List.exists
      (fun mv ->
         match Move.promotion mv with
         | Some Rook -> true
         | _ -> false)
      promotion_moves
  in
  let has_bishop =
    List.exists
      (fun mv ->
         match Move.promotion mv with
         | Some Bishop -> true
         | _ -> false)
      promotion_moves
  in
  let has_knight =
    List.exists
      (fun mv ->
         match Move.promotion mv with
         | Some Knight -> true
         | _ -> false)
      promotion_moves
  in
  Alcotest.(check bool) "Queen promotion" true has_queen;
  Alcotest.(check bool) "Rook promotion" true has_rook;
  Alcotest.(check bool) "Bishop promotion" true has_bishop;
  Alcotest.(check bool) "Knight promotion (underpromotion)" true has_knight
;;

let test_black_promotion_generation () =
  (* Black pawn on 2nd rank ready to promote *)
  let fen = "8/8/8/8/8/8/p7/8 b - - 0 1" in
  let pos = Position.of_fen fen in
  let moves = Movegen.generate_moves pos in
  let promotion_moves = List.filter Move.is_promotion moves in
  Alcotest.(check int) "Black gets four promotion moves" 4 (List.length promotion_moves)
;;

let test_promotion_with_capture () =
  (* White pawn can capture and promote *)
  let fen = "1r6/P7/8/8/8/8/8/8 w - - 0 1" in
  let pos = Position.of_fen fen in
  let moves = Movegen.generate_moves pos in
  let promotion_moves = List.filter Move.is_promotion moves in
  (* Should have 4 non-capture promotions (a8) + 4 capture promotions (b8) = 8 total *)
  Alcotest.(check int)
    "Eight promotion moves (4 normal + 4 capture)"
    8
    (List.length promotion_moves);
  (* Check capture promotions exist *)
  let capture_promos =
    List.filter (fun mv -> Move.is_promotion mv && Move.is_capture mv) promotion_moves
  in
  Alcotest.(check int) "Four capture promotions" 4 (List.length capture_promos)
;;

let test_no_premature_promotion () =
  (* Pawn on 6th rank should NOT generate promotions *)
  let fen = "8/8/P7/8/8/8/8/8 w - - 0 1" in
  let pos = Position.of_fen fen in
  let moves = Movegen.generate_moves pos in
  let promotion_moves = List.filter Move.is_promotion moves in
  Alcotest.(check int) "No promotions from 6th rank" 0 (List.length promotion_moves)
;;

let test_promotion_move_execution () =
  (* Test that promotion moves can be executed properly *)
  let fen = "8/P7/8/8/8/8/8/8 w - - 0 1" in
  let pos = Position.of_fen fen in
  let moves = Movegen.generate_moves pos in
  let promotion_moves = List.filter Move.is_promotion moves in
  (* Find queen promotion *)
  let queen_promo =
    List.find
      (fun mv ->
         match Move.promotion mv with
         | Some Queen -> true
         | _ -> false)
      promotion_moves
  in
  (* Execute the move *)
  let new_pos = Position.make_move pos queen_promo in
  (* Check that a queen is now on a8 *)
  let queen_on_a8 =
    match Position.piece_at new_pos 56 with
    (* a8 = square 56 *)
    | Some piece -> piece.kind = Queen && piece.color = White
    | None -> false
  in
  let pawn_gone = Position.piece_at new_pos 48 = None in
  (* a7 = square 48 *)
  Alcotest.(check bool) "Queen on a8 after promotion" true queen_on_a8;
  Alcotest.(check bool) "Pawn gone from a7" true pawn_gone
;;

let test_underpromotion_to_knight () =
  (* Test underpromotion to knight specifically *)
  let fen = "8/P7/8/8/8/8/8/8 w - - 0 1" in
  let pos = Position.of_fen fen in
  let moves = Movegen.generate_moves pos in
  let promotion_moves = List.filter Move.is_promotion moves in
  (* Find knight promotion *)
  let knight_promo =
    List.find
      (fun mv ->
         match Move.promotion mv with
         | Some Knight -> true
         | _ -> false)
      promotion_moves
  in
  (* Execute the move *)
  let new_pos = Position.make_move pos knight_promo in
  (* Check that a knight is now on a8 *)
  let knight_on_a8 =
    match Position.piece_at new_pos 56 with
    | Some piece -> piece.kind = Knight && piece.color = White
    | None -> false
  in
  Alcotest.(check bool) "Knight on a8 after underpromotion" true knight_on_a8
;;

let test_promotion_uci_notation () =
  (* Test that promotions are correctly converted to UCI notation *)
  let from_sq = Square.of_uci "a7" in
  let to_sq = Square.of_uci "a8" in
  let queen_promo = Move.make from_sq to_sq Move.PromoteQueen in
  let knight_promo = Move.make from_sq to_sq Move.PromoteKnight in
  let rook_promo = Move.make from_sq to_sq Move.PromoteRook in
  let bishop_promo = Move.make from_sq to_sq Move.PromoteBishop in
  Alcotest.(check string) "Queen promotion UCI" "a7a8q" (Move.to_uci queen_promo);
  Alcotest.(check string) "Knight promotion UCI" "a7a8n" (Move.to_uci knight_promo);
  Alcotest.(check string) "Rook promotion UCI" "a7a8r" (Move.to_uci rook_promo);
  Alcotest.(check string) "Bishop promotion UCI" "a7a8b" (Move.to_uci bishop_promo)
;;

let test_promotion_parsing_from_uci () =
  (* Test parsing UCI promotion notation *)
  let queen_mv = Move.of_uci "a7a8q" in
  let knight_mv = Move.of_uci "a7a8n" in
  let rook_mv = Move.of_uci "e7e8r" in
  let bishop_mv = Move.of_uci "h7h8b" in
  Alcotest.(check bool) "Parsed queen promotion" true (Move.is_promotion queen_mv);
  Alcotest.(check bool) "Parsed knight promotion" true (Move.is_promotion knight_mv);
  Alcotest.(check bool) "Parsed rook promotion" true (Move.is_promotion rook_mv);
  Alcotest.(check bool) "Parsed bishop promotion" true (Move.is_promotion bishop_mv);
  (* Check specific promotion types *)
  let check_promo mv expected =
    match Move.promotion mv with
    | Some piece_kind -> piece_kind = expected
    | None -> false
  in
  Alcotest.(check bool) "Queen promo type" true (check_promo queen_mv Queen);
  Alcotest.(check bool) "Knight promo type" true (check_promo knight_mv Knight);
  Alcotest.(check bool) "Rook promo type" true (check_promo rook_mv Rook);
  Alcotest.(check bool) "Bishop promo type" true (check_promo bishop_mv Bishop)
;;

let test_promotion_in_search () =
  (* Test that search can find promotion moves *)
  let fen = "8/P7/8/8/8/8/8/7k w - - 0 1" in
  let game = Game.of_fen fen in
  (* Search should find the promotion *)
  let result = Search.find_best_move ~verbose:false ~max_time_ms:1000 game 3 in
  match result.Search.best_move with
  | Some mv when Move.is_promotion mv ->
    (* Should prefer queen promotion in this simple position *)
    let promotes_to_queen =
      match Move.promotion mv with
      | Some Queen -> true
      | _ -> false
    in
    Alcotest.(check bool) "Engine promotes to queen" true promotes_to_queen
  | Some mv ->
    Printf.printf "Engine chose: %s (not a promotion)\n" (Move.to_uci mv);
    Alcotest.fail "Engine should find promotion move"
  | None -> Alcotest.fail "Engine should find a move"
;;

let () =
  let open Alcotest in
  run
    "Promotion Tests"
    [ ( "move_generation"
      , [ test_case "Basic promotion generation" `Quick test_basic_promotion_generation
        ; test_case "Black promotion generation" `Quick test_black_promotion_generation
        ; test_case "Promotion with capture" `Quick test_promotion_with_capture
        ; test_case "No premature promotion" `Quick test_no_premature_promotion
        ] )
    ; ( "move_execution"
      , [ test_case "Promotion move execution" `Quick test_promotion_move_execution
        ; test_case "Underpromotion to knight" `Quick test_underpromotion_to_knight
        ] )
    ; ( "notation"
      , [ test_case "Promotion UCI notation" `Quick test_promotion_uci_notation
        ; test_case "Promotion parsing from UCI" `Quick test_promotion_parsing_from_uci
        ] )
    ; ( "search_integration"
      , [ test_case "Promotion in search" `Slow test_promotion_in_search ] )
    ]
;;
