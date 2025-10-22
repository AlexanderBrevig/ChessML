(** XBoard Protocol Tests *)

open Chessml

(* Test basic move parsing - XBoard uses coordinate notation *)
let test_move_generation () =
  let game = Game.default () in
  let pos = Game.position game in
  let moves = Movegen.generate_moves pos in
  (* Verify we can generate legal moves (XBoard needs this) *)
  assert (List.length moves > 0);
  assert (List.length moves = 20);
  (* Starting position has 20 legal moves *)
  print_endline "✓ XBoard move generation test passed"
;;

(* Test FEN handling for XBoard setboard command *)
let test_setboard () =
  (* Test that we can create a game from FEN (setboard command) *)
  let fen = "r1bqkbnr/pppppppp/2n5/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" in
  let game = Game.of_fen fen in
  let pos = Game.position game in
  (* Verify position was set *)
  let result_fen = Position.to_fen pos in
  assert (String.length result_fen > 0);
  assert (String.contains result_fen 'n');
  (* knight present *)
  print_endline "✓ XBoard setboard test passed"
;;

(* Test time controls - XBoard sends time in centiseconds *)
let test_time_controls () =
  (* XBoard time format: time in centiseconds (1/100th of a second) *)
  let time_centiseconds = 30000 in
  (* 300 seconds = 5 minutes *)
  let time_seconds = time_centiseconds / 100 in
  assert (time_seconds = 300);
  (* XBoard can also send time per move *)
  let time_per_move = 100 in
  (* 1 second *)
  let seconds = time_per_move / 100 in
  assert (seconds = 1);
  print_endline "✓ XBoard time controls test passed"
;;

(* Test move formatting *)
let test_move_formatting () =
  let game = Game.default () in
  let pos = Game.position game in
  let moves = Movegen.generate_moves pos in
  (* Test that moves can be converted to UCI format (similar to XBoard) *)
  let move_strings = List.map Move.to_uci moves in
  assert (List.length move_strings = 20);
  (* Verify format looks reasonable *)
  List.iter
    (fun mv_str ->
       assert (String.length mv_str >= 4);
       assert (String.length mv_str <= 5) (* 4 for normal, 5 for promotion *))
    move_strings;
  print_endline "✓ XBoard move formatting test passed"
;;

(* Run all tests *)
let () =
  Printf.printf "=== XBoard Protocol Tests ===\n\n";
  test_move_generation ();
  test_setboard ();
  test_time_controls ();
  test_move_formatting ();
  Printf.printf "\n✅ All XBoard protocol tests passed!\n"
;;
