(** Regression tests for illegal move bugs found in actual games *)

open Chessml_core
open Chessml_engine

let test_position_after_rxg2_check () =
  (* Position from results_skill_0.pgn where engine made illegal move c4d6
     FEN: 4k3/3b1pp1/3bp1r1/2p4p/2N1QP2/4Pq2/PPPPR1rP/R1B3K1 w - - 0 24
     White king on g1 is in check from black rook on g2 *)
  let fen = "4k3/3b1pp1/3bp1r1/2p4p/2N1QP2/4Pq2/PPPPR1rP/R1B3K1 w - - 0 24" in
  Printf.printf "\n=== Position After Rxg2+ ===\n";
  Printf.printf "FEN: %s\n\n" fen;
  let pos = Position.of_fen fen in
  let legal_moves = Movegen.generate_moves pos in
  Printf.printf "Legal moves: %d\n" (List.length legal_moves);
  List.iter (fun mv -> Printf.printf "  %s\n" (Move.to_uci mv)) legal_moves;
  (* Verify we have legal moves *)
  assert (List.length legal_moves > 0);
  (* Test that engine suggests a legal move *)
  let game = Game.of_fen fen in
  let result = Search.find_best_move ~verbose:false game 4 in
  match result.best_move with
  | None ->
    Printf.printf "\n❌ Engine returned no move\n";
    assert false
  | Some best_move ->
    Printf.printf "\nEngine suggests: %s\n" (Move.to_uci best_move);
    let is_legal =
      List.exists
        (fun mv ->
           Move.from mv = Move.from best_move
           && Move.to_square mv = Move.to_square best_move
           && Move.kind mv = Move.kind best_move)
        legal_moves
    in
    if is_legal
    then Printf.printf "✅ Move is legal\n"
    else (
      Printf.printf "❌ ENGINE PROPOSED ILLEGAL MOVE\n";
      assert false)
;;

let test_knight_c4_cannot_move () =
  (* Verify knight on c4 cannot move when king is in check
     (previously generated illegal Nc4xd6) *)
  Printf.printf "\n=== Knight on c4 Legality ===\n";
  let fen = "4k3/3b1pp1/3bp1r1/2p4p/2N1QP2/4Pq2/PPPPR1rP/R1B3K1 w - - 0 24" in
  let pos = Position.of_fen fen in
  let c4 = Square.c4 in
  let d6 = Square.d6 in
  let legal_moves = Movegen.generate_moves pos in
  let knight_moves = List.filter (fun mv -> Move.from mv = c4) legal_moves in
  let can_go_to_d6 = List.exists (fun mv -> Move.to_square mv = d6) knight_moves in
  Printf.printf "Knight on c4 has %d legal moves\n" (List.length knight_moves);
  if can_go_to_d6
  then (
    Printf.printf "❌ BUG: Nc4xd6 allowed (leaves king in check)\n";
    raise (Failure "Move generator allows illegal Nc4xd6"))
  else Printf.printf "✅ Nc4xd6 correctly rejected\n"
;;

let test_castling_after_castled () =
  (* Bug: Engine castled O-O earlier, then tried O-O-O later
     Position: 8/8/6k1/Q6p/3N4/3P3P/PPP2P2/R4RK1 w - - 1 35
     King on g1 means we already castled kingside, but engine tried O-O-O *)
  Printf.printf "\n=== Castling After Already Castled ===\n";
  (* Position after White castled kingside earlier in the game *)
  let fen = "8/8/6k1/Q6p/3N4/3P3P/PPP2P2/R4RK1 w - - 1 35" in
  let pos = Position.of_fen fen in
  Printf.printf "FEN: %s\n" fen;
  Printf.printf "King position: g1 (already castled)\n";
  Printf.printf "Castling rights from FEN: - (none)\n\n";
  let legal_moves = Movegen.generate_moves pos in
  let castle_moves = List.filter (fun mv -> Move.is_castle mv) legal_moves in
  Printf.printf "Castling moves available: %d\n" (List.length castle_moves);
  if List.length castle_moves > 0
  then (
    Printf.printf "❌ BUG: Castling still allowed after already castled\n";
    List.iter
      (fun mv -> Printf.printf "  Illegal castle: %s\n" (Move.to_uci mv))
      castle_moves;
    raise (Failure "Castling allowed after already castled"))
  else Printf.printf "✅ Castling correctly prevented\n";
  (* Also test that castling rights are cleared after making a castling move *)
  Printf.printf "\n=== Castling Rights Cleared After Castle ===\n";
  let start_pos = Position.default () in
  (* Make some moves to allow castling: 1.e4 e5 2.Nf3 Nf6 3.Bc4 Bc5 *)
  let pos1 =
    Position.make_move start_pos (Move.make Square.e2 Square.e4 Move.PawnDoublePush)
  in
  let pos2 =
    Position.make_move pos1 (Move.make Square.e7 Square.e5 Move.PawnDoublePush)
  in
  let pos3 = Position.make_move pos2 (Move.make Square.g1 Square.f3 Move.Quiet) in
  let pos4 = Position.make_move pos3 (Move.make Square.g8 Square.f6 Move.Quiet) in
  let pos5 = Position.make_move pos4 (Move.make Square.f1 Square.c4 Move.Quiet) in
  let pos6 = Position.make_move pos5 (Move.make Square.f8 Square.c5 Move.Quiet) in
  (* White castles kingside *)
  let pos7 = Position.make_move pos6 (Move.make Square.e1 Square.g1 Move.ShortCastle) in
  Printf.printf "After White castles O-O:\n";
  (* Now make a Black move and check if White can castle on next turn *)
  let pos8 = Position.make_move pos7 (Move.make Square.d7 Square.d6 Move.Quiet) in
  let white_moves = Movegen.generate_moves pos8 in
  let white_castles = List.filter (fun mv -> Move.is_castle mv) white_moves in
  Printf.printf "White castling moves after castling: %d\n" (List.length white_castles);
  if List.length white_castles > 0
  then (
    Printf.printf "❌ BUG: White can castle again after already castling!\n";
    List.iter (fun mv -> Printf.printf "  Illegal: %s\n" (Move.to_uci mv)) white_castles;
    raise (Failure "Castling rights not cleared after castling"))
  else Printf.printf "✅ White cannot castle again (rights cleared)\n"
;;

let () =
  Printf.printf "╔═══════════════════════════════════════════════════════╗\n";
  Printf.printf "║  Illegal Move Regression Tests                       ║\n";
  Printf.printf "╚═══════════════════════════════════════════════════════╝\n";
  try
    test_position_after_rxg2_check ();
    test_knight_c4_cannot_move ();
    test_castling_after_castled ();
    Printf.printf "\n╔═══════════════════════════════════════════════════════╗\n";
    Printf.printf "║  ✅ ALL TESTS PASSED                                  ║\n";
    Printf.printf "╚═══════════════════════════════════════════════════════╝\n";
    exit 0
  with
  | Failure msg ->
    Printf.printf "\n╔═══════════════════════════════════════════════════════╗\n";
    Printf.printf "║  ❌ TEST FAILED: %s\n" msg;
    Printf.printf "╚═══════════════════════════════════════════════════════╝\n";
    exit 1
  | Assert_failure (file, line, _) ->
    Printf.printf "\n╔═══════════════════════════════════════════════════════╗\n";
    Printf.printf "║  ❌ TEST FAILED at %s:%d\n" file line;
    Printf.printf "╚═══════════════════════════════════════════════════════╝\n";
    exit 1
  | e ->
    Printf.printf "\n╔═══════════════════════════════════════════════════════╗\n";
    Printf.printf "║  ❌ ERROR: %s\n" (Printexc.to_string e);
    Printf.printf "╚═══════════════════════════════════════════════════════╝\n";
    exit 1
;;
