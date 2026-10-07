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

(* Bitboards, occupancy and the incremental key must match the board array at
   every node of a full tree walk *)
let test_incremental_state () =
  let open Chessml in
  let check_consistent pos =
    if Position.key pos <> Position.compute_key pos
    then Alcotest.failf "key mismatch in %s" (Position.to_fen pos);
    let occ = ref 0L in
    for sq = 0 to 63 do
      match Position.piece_at pos sq with
      | Some p ->
        occ := Bitboard.set !occ sq;
        if not (Bitboard.contains (Position.get_pieces pos p.color p.kind) sq)
        then Alcotest.failf "piece bitboard missing %d in %s" sq (Position.to_fen pos)
      | None -> ()
    done;
    let all =
      List.fold_left
        (fun acc kind ->
           Int64.logor
             acc
             (Int64.logor
                (Position.get_pieces pos White kind)
                (Position.get_pieces pos Black kind)))
        0L
        [ Pawn; Knight; Bishop; Rook; Queen; King ]
    in
    if !occ <> Position.occupied pos || !occ <> all
    then Alcotest.failf "occupancy mismatch in %s" (Position.to_fen pos)
  in
  let rec walk pos depth =
    check_consistent pos;
    if depth > 0
    then
      List.iter
        (fun mv -> walk (Position.make_move pos mv) (depth - 1))
        (Movegen.generate_moves pos)
  in
  List.iter
    (fun fen -> walk (Position.of_fen fen) 3)
    [ Position.fen_startpos
    ; "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
    ; "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1"
    ; "rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3"
    ];
  check_consistent (Position.make_null_move (Position.of_fen Position.fen_startpos))
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
        ; test_case "Incremental state" `Quick test_incremental_state
        ] )
    ]
;;
