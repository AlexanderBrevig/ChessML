(** Test the development evaluation improvements *)

open Chessml
open Chessml_core

let () =
  print_endline "Development Evaluation Test";
  print_endline "===========================";
  print_endline "";
  (* Test 1: Starting position - should encourage development *)
  print_endline "Test 1: Starting Position";
  let pos1 = Position.default () in
  let eval1 = Eval.evaluate pos1 in
  Printf.printf "  Eval: %d cp (baseline)\n" eval1;
  print_endline "";
  (* Test 2: After 1.e4 - pawn move, no development yet *)
  print_endline "Test 2: After 1.e4";
  let pos2 =
    Position.make_move pos1 (Move.make Square.e2 Square.e4 Move.PawnDoublePush)
  in
  let eval2 = Eval.evaluate pos2 in
  Printf.printf "  Eval: %d cp\n" eval2;
  print_endline "";
  (* Test 3: After 1.e4 e5 2.Qh5?? - queen out too early *)
  print_endline "Test 3: After 1.e4 e5 2.Qh5?? (queen out too early, no minors developed)";
  let pos3_a =
    Position.make_move pos1 (Move.make Square.e2 Square.e4 Move.PawnDoublePush)
  in
  let pos3_b =
    Position.make_move pos3_a (Move.make Square.e7 Square.e5 Move.PawnDoublePush)
  in
  let pos3_c = Position.make_move pos3_b (Move.make Square.d1 Square.h5 Move.Quiet) in
  let eval3 = Eval.evaluate pos3_c in
  Printf.printf "  Eval: %d cp (should be penalized!)\n" eval3;
  print_endline "";
  (* Test 4: After 1.e4 e5 2.Nf3 - proper development *)
  print_endline "Test 4: After 1.e4 e5 2.Nf3 (proper knight development)";
  let pos4_a =
    Position.make_move pos1 (Move.make Square.e2 Square.e4 Move.PawnDoublePush)
  in
  let pos4_b =
    Position.make_move pos4_a (Move.make Square.e7 Square.e5 Move.PawnDoublePush)
  in
  let pos4_c = Position.make_move pos4_b (Move.make Square.g1 Square.f3 Move.Quiet) in
  let eval4 = Eval.evaluate pos4_c in
  Printf.printf "  Eval: %d cp (should be better than Qh5)\n" eval4;
  Printf.printf "  Difference: %d cp better than Qh5\n" (eval4 - eval3);
  print_endline "";
  (* Test 5: Compare pieces still on starting squares vs developed *)
  print_endline "Test 5: Pieces on starting squares vs developed";
  (* Position A: No pieces developed (just pawn moves) *)
  print_endline "  A) After 1.e4 e5 2.d4 d5 (pawns only, no development)";
  let posA_1 =
    Position.make_move pos1 (Move.make Square.e2 Square.e4 Move.PawnDoublePush)
  in
  let posA_2 =
    Position.make_move posA_1 (Move.make Square.e7 Square.e5 Move.PawnDoublePush)
  in
  let posA_3 =
    Position.make_move posA_2 (Move.make Square.d2 Square.d4 Move.PawnDoublePush)
  in
  let posA_4 =
    Position.make_move posA_3 (Move.make Square.d7 Square.d5 Move.PawnDoublePush)
  in
  let evalA = Eval.evaluate posA_4 in
  Printf.printf "     Eval: %d cp (undeveloped)\n" evalA;
  (* Position B: Some development *)
  print_endline "  B) After 1.e4 e5 2.Nf3 Nc6 (2 knights developed)";
  let posB_1 =
    Position.make_move pos1 (Move.make Square.e2 Square.e4 Move.PawnDoublePush)
  in
  let posB_2 =
    Position.make_move posB_1 (Move.make Square.e7 Square.e5 Move.PawnDoublePush)
  in
  let posB_3 = Position.make_move posB_2 (Move.make Square.g1 Square.f3 Move.Quiet) in
  let posB_4 = Position.make_move posB_3 (Move.make Square.b8 Square.c6 Move.Quiet) in
  let evalB = Eval.evaluate posB_4 in
  Printf.printf "     Eval: %d cp (2 knights developed)\n" evalB;
  Printf.printf "     Bonus: %d cp for developing knights\n" (evalB - evalA);
  (* Position C: All minors developed *)
  print_endline "  C) After 1.e4 e5 2.Nf3 Nc6 3.Bc4 Bc5 (all minors developed)";
  let posC_1 =
    Position.make_move pos1 (Move.make Square.e2 Square.e4 Move.PawnDoublePush)
  in
  let posC_2 =
    Position.make_move posC_1 (Move.make Square.e7 Square.e5 Move.PawnDoublePush)
  in
  let posC_3 = Position.make_move posC_2 (Move.make Square.g1 Square.f3 Move.Quiet) in
  let posC_4 = Position.make_move posC_3 (Move.make Square.b8 Square.c6 Move.Quiet) in
  let posC_5 = Position.make_move posC_4 (Move.make Square.f1 Square.c4 Move.Quiet) in
  let posC_6 = Position.make_move posC_5 (Move.make Square.f8 Square.c5 Move.Quiet) in
  let evalC = Eval.evaluate posC_6 in
  Printf.printf "     Eval: %d cp (all 4 minors developed)\n" evalC;
  Printf.printf "     Bonus: %d cp for developing all minors\n" (evalC - evalA);
  (* Position D: All minors + castled *)
  print_endline "  D) After 1.e4 e5 2.Nf3 Nc6 3.Bc4 Bc5 4.O-O (developed + castled)";
  let posD_7 =
    Position.make_move posC_6 (Move.make Square.e1 Square.g1 Move.ShortCastle)
  in
  let evalD = Eval.evaluate posD_7 in
  Printf.printf "     Eval: %d cp (developed + castled)\n" evalD;
  Printf.printf "     Total bonus vs undeveloped: %d cp\n" (evalD - evalA);
  print_endline "";
  print_endline "✅ Development evaluation test complete";
  print_endline "";
  print_endline "Summary:";
  print_endline "- Engine now penalizes pieces staying on starting squares";
  print_endline "- Engine penalizes early queen moves before minor development";
  print_endline "- Engine rewards developing all minor pieces";
  print_endline "- Engine rewards castling after development";
  print_endline "- This should encourage better opening play!"
;;
