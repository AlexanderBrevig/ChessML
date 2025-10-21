(** Test castling incentives - castle when path is clear *)

open Chessml
open Chessml_core

let () =
  print_endline "Castling Incentive Test";
  print_endline "======================";
  print_endline "";
  (* Test 1: Italian Game - can castle immediately *)
  print_endline "Test 1: Italian Game - 1.e4 e5 2.Nf3 Nc6 3.Bc4 Bc5";
  let pos1 = Position.default () in
  let pos1 =
    Position.make_move pos1 (Move.make Square.e2 Square.e4 Move.PawnDoublePush)
  in
  let pos1 =
    Position.make_move pos1 (Move.make Square.e7 Square.e5 Move.PawnDoublePush)
  in
  let pos1 = Position.make_move pos1 (Move.make Square.g1 Square.f3 Move.Quiet) in
  let pos1 = Position.make_move pos1 (Move.make Square.b8 Square.c6 Move.Quiet) in
  let pos1 = Position.make_move pos1 (Move.make Square.f1 Square.c4 Move.Quiet) in
  let pos1 = Position.make_move pos1 (Move.make Square.f8 Square.c5 Move.Quiet) in
  let eval_before = Eval.evaluate pos1 in
  Printf.printf "  Eval before castling: %d cp\n" eval_before;
  (* Now castle *)
  let pos1_castled =
    Position.make_move pos1 (Move.make Square.e1 Square.g1 Move.ShortCastle)
  in
  let eval_after = Eval.evaluate pos1_castled in
  Printf.printf "  Eval after castling:  %d cp\n" eval_after;
  Printf.printf "  Castling bonus: %d cp\n" (eval_after - eval_before);
  print_endline "";
  (* Test 2: Path clear vs path blocked *)
  print_endline "Test 2: Castling incentive when path is clear";
  (* Position with path clear to castle *)
  let pos_clear = Position.default () in
  let pos_clear =
    Position.make_move pos_clear (Move.make Square.e2 Square.e4 Move.PawnDoublePush)
  in
  let pos_clear =
    Position.make_move pos_clear (Move.make Square.e7 Square.e5 Move.PawnDoublePush)
  in
  let pos_clear =
    Position.make_move pos_clear (Move.make Square.g1 Square.f3 Move.Quiet)
  in
  let pos_clear =
    Position.make_move pos_clear (Move.make Square.b8 Square.c6 Move.Quiet)
  in
  let pos_clear =
    Position.make_move pos_clear (Move.make Square.f1 Square.c4 Move.Quiet)
  in
  let pos_clear =
    Position.make_move pos_clear (Move.make Square.f8 Square.c5 Move.Quiet)
  in
  let eval_clear = Eval.evaluate pos_clear in
  Printf.printf "  A) Path clear to castle: %d cp\n" eval_clear;
  (* Position with path blocked (bishop still on f1) *)
  let pos_blocked = Position.default () in
  let pos_blocked =
    Position.make_move pos_blocked (Move.make Square.e2 Square.e4 Move.PawnDoublePush)
  in
  let pos_blocked =
    Position.make_move pos_blocked (Move.make Square.e7 Square.e5 Move.PawnDoublePush)
  in
  let pos_blocked =
    Position.make_move pos_blocked (Move.make Square.g1 Square.f3 Move.Quiet)
  in
  let pos_blocked =
    Position.make_move pos_blocked (Move.make Square.b8 Square.c6 Move.Quiet)
  in
  (* Don't move bishop - path still blocked *)
  let eval_blocked = Eval.evaluate pos_blocked in
  Printf.printf "  B) Path blocked (bishop on f1): %d cp\n" eval_blocked;
  Printf.printf "  Difference: %d cp incentive to clear path\n" (eval_clear - eval_blocked);
  print_endline "";
  (* Test 3: Compare castling now vs developing more pieces *)
  print_endline "Test 3: Castle now vs develop queen knight first";
  (* Can castle now *)
  let pos_can_castle = Position.default () in
  let pos_can_castle =
    Position.make_move pos_can_castle (Move.make Square.e2 Square.e4 Move.PawnDoublePush)
  in
  let pos_can_castle =
    Position.make_move pos_can_castle (Move.make Square.e7 Square.e5 Move.PawnDoublePush)
  in
  let pos_can_castle =
    Position.make_move pos_can_castle (Move.make Square.g1 Square.f3 Move.Quiet)
  in
  let pos_can_castle =
    Position.make_move pos_can_castle (Move.make Square.b8 Square.c6 Move.Quiet)
  in
  let pos_can_castle =
    Position.make_move pos_can_castle (Move.make Square.f1 Square.c4 Move.Quiet)
  in
  let pos_can_castle =
    Position.make_move pos_can_castle (Move.make Square.f8 Square.c5 Move.Quiet)
  in
  let eval_can = Eval.evaluate pos_can_castle in
  Printf.printf "  A) Can castle (2 pieces developed each): %d cp\n" eval_can;
  (* Develop queen knight instead *)
  let pos_develop =
    Position.make_move pos_can_castle (Move.make Square.b1 Square.c3 Move.Quiet)
  in
  let eval_develop = Eval.evaluate pos_develop in
  Printf.printf "  B) Develop Nc3 (3 pieces developed): %d cp\n" eval_develop;
  Printf.printf "  Difference: %d cp\n" (eval_can - eval_develop);
  if eval_can > eval_develop
  then Printf.printf "  ✅ Engine prefers castling now!\n"
  else Printf.printf "  ⚠️  Engine still prefers more development\n";
  print_endline "";
  print_endline "✅ Castling incentive test complete"
;;
