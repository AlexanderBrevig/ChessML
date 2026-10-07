(** Unit tests for Pawn_cache *)

open Chessml.Engine
open Chessml_core

let pawns pos =
  ( Position.get_pieces pos Types.White Types.Pawn
  , Position.get_pieces pos Types.Black Types.Pawn )
;;

let probe cache pos =
  let white_pawns, black_pawns = pawns pos in
  Pawn_cache.probe cache ~white_pawns ~black_pawns
;;

let store cache pos score =
  let white_pawns, black_pawns = pawns pos in
  Pawn_cache.store cache ~white_pawns ~black_pawns score
;;

let test_cache_creation () =
  let hits, misses, hit_rate = Pawn_cache.stats (Pawn_cache.create 1000) in
  Alcotest.(check int) "Initial hits" 0 hits;
  Alcotest.(check int) "Initial misses" 0 misses;
  Alcotest.(check (float 0.001)) "Initial hit rate" 0.0 hit_rate
;;

let test_store_and_probe () =
  let cache = Pawn_cache.create 1024 in
  let pos = Position.default () in
  Alcotest.(check (option int)) "Miss before store" None (probe cache pos);
  store cache pos 42;
  Alcotest.(check (option int)) "Hit after store" (Some 42) (probe cache pos);
  store cache pos 7;
  Alcotest.(check (option int)) "Replaced" (Some 7) (probe cache pos);
  let hits, misses, _ = Pawn_cache.stats cache in
  Alcotest.(check (pair int int)) "Stats" (2, 1) (hits, misses)
;;

let test_ignores_other_pieces () =
  let cache = Pawn_cache.create 1024 in
  store cache (Position.default ()) 5;
  let pos = Position.of_fen "r1bqkb1r/pppppppp/8/8/8/8/PPPPPPPP/R1BQKB1R w KQkq - 0 1" in
  Alcotest.(check (option int)) "Same pawns share entry" (Some 5) (probe cache pos)
;;

let test_colors_not_confused () =
  (* Same pawn squares with colors swapped must not share an entry *)
  let cache = Pawn_cache.create 1024 in
  store cache (Position.of_fen "4k3/8/8/3p4/4P3/8/8/4K3 w - - 0 1") 10;
  let swapped = Position.of_fen "4k3/8/8/3P4/4p3/8/8/4K3 w - - 0 1" in
  Alcotest.(check (option int)) "Swapped colors miss" None (probe cache swapped)
;;

let test_clear () =
  let cache = Pawn_cache.create 1024 in
  let pos = Position.default () in
  store cache pos 1;
  Pawn_cache.clear cache;
  Alcotest.(check (option int)) "Cleared" None (probe cache pos)
;;

let test_eval_cold_equals_warm () =
  (* The cached and uncached pawn evaluation must give the same total eval *)
  List.iter
    (fun fen ->
       let pos = Position.of_fen fen in
       Pawn_cache.clear_global ();
       let cold = Eval.evaluate pos in
       let warm = Eval.evaluate pos in
       Alcotest.(check int) fen cold warm)
    [ Position.fen_startpos
    ; "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2"
    ; "8/5k2/3p4/1p1Pp2p/pP2Pp1P/P4P1K/8/8 b - - 99 50"
    ; "4k3/8/8/3P4/4p3/8/8/4K3 b - - 0 1"
    ]
;;

let () =
  let open Alcotest in
  run
    "Pawn_cache"
    [ ( "cache"
      , [ test_case "Create cache" `Quick test_cache_creation
        ; test_case "Store and probe" `Quick test_store_and_probe
        ; test_case "Ignores non-pawns" `Quick test_ignores_other_pieces
        ; test_case "Colors not confused" `Quick test_colors_not_confused
        ; test_case "Clear" `Quick test_clear
        ] )
    ; "eval", [ test_case "Cold equals warm" `Quick test_eval_cold_equals_warm ]
    ]
;;
