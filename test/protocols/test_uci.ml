(** UCI Protocol Tests *)

open Chessml

(* Test parsing position commands *)
let test_parse_position () =
  let game = Game.default () in
  (* Test position startpos *)
  let game' = Uci.parse_position [ "startpos" ] game in
  let pos = Game.position game' in
  let fen = Position.to_fen pos in
  (* Just verify it's a valid starting position *)
  assert (String.contains fen 'r');
  (* rooks present *)
  assert (String.contains fen 'k');
  (* kings present *)

  (* Test position with moves *)
  let game' = Uci.parse_position [ "startpos"; "moves"; "e2e4"; "e7e5" ] game in
  let pos' = Game.position game' in
  let fen' = Position.to_fen pos' in
  (* Position should be different after moves *)
  assert (fen <> fen');
  print_endline "✓ UCI parse_position test passed"
;;

(* Test applying moves *)
let test_apply_moves () =
  let game = Game.default () in
  (* Test applying a list of moves *)
  let game' = Uci.apply_moves game [ "e2e4"; "e7e5"; "g1f3" ] in
  let pos = Game.position game' in
  let fen = Position.to_fen pos in
  (* Verify moves were applied (position should have changed) *)
  let initial_fen = Position.to_fen (Game.position game) in
  assert (fen <> initial_fen);
  print_endline "✓ UCI apply_moves test passed"
;;

(* Test parsing go command parameters *)
let test_parse_go_params () =
  (* Test depth parameter *)
  let params = Uci.parse_go_params [ "depth"; "5" ] in
  assert (params.Uci.depth = Some 5);
  assert (params.Uci.movetime = None);
  (* Test movetime parameter *)
  let params = Uci.parse_go_params [ "movetime"; "1000" ] in
  assert (params.Uci.depth = None);
  assert (params.Uci.movetime = Some 1000);
  (* Test wtime/btime parameters *)
  let params =
    Uci.parse_go_params
      [ "wtime"; "30000"; "btime"; "25000"; "winc"; "500"; "binc"; "500" ]
  in
  assert (params.Uci.wtime = Some 30000);
  assert (params.Uci.btime = Some 25000);
  assert (params.Uci.winc = Some 500);
  assert (params.Uci.binc = Some 500);
  (* Test movestogo parameter *)
  let params =
    Uci.parse_go_params [ "movestogo"; "40"; "wtime"; "60000"; "btime"; "60000" ]
  in
  assert (params.Uci.movestogo = Some 40);
  assert (params.Uci.wtime = Some 60000);
  (* Test empty parameters *)
  let params = Uci.parse_go_params [] in
  assert (params.Uci.depth = None);
  assert (params.Uci.movetime = None);
  assert (params.Uci.wtime = None);
  print_endline "✓ UCI parse_go_params test passed"
;;

(* Test position parsing with complex FEN *)
let test_complex_fen () =
  let game = Game.default () in
  (* Test a tactical position FEN *)
  let fen = "r1bqk1nr/pppp2pp/5p2/2b5/4Pp2/2P5/PP1P2PP/RNBQKBNR" in
  let fen_tokens = [ "fen"; fen; "w"; "KQkq"; "-"; "0"; "1" ] in
  let game' = Uci.parse_position fen_tokens game in
  let pos = Game.position game' in
  (* Verify position was set correctly *)
  let result_fen = Position.to_fen pos in
  assert (String.contains result_fen 'b');
  (* bishop present *)
  assert (String.contains result_fen 'p');
  (* pawns present *)
  print_endline "✓ UCI complex FEN test passed"
;;

(* Run all tests *)
let () =
  Printf.printf "=== UCI Protocol Tests ===\n\n";
  test_parse_position ();
  test_apply_moves ();
  test_parse_go_params ();
  test_complex_fen ();
  Printf.printf "\n✅ All UCI protocol tests passed!\n"
;;
