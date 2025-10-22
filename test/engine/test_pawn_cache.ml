(** Unit tests for Pawn_cache *)

open Chessml.Engine
open Chessml_core

(* Helper function to get pawn bitboards from a position *)
let get_pawn_bitboards pos =
  let white_pawns = Position.get_pieces pos Types.White Types.Pawn in
  let black_pawns = Position.get_pieces pos Types.Black Types.Pawn in
  (white_pawns, black_pawns)
;;

(* Test cache creation *)
let test_cache_creation () =
  let cache = Pawn_cache.create 1024 in
  let hits, misses, hit_rate = Pawn_cache.stats cache in
  Alcotest.(check int) "Initial hits" 0 hits;
  Alcotest.(check int) "Initial misses" 0 misses;
  Alcotest.(check (float 0.001)) "Initial hit rate" 0.0 hit_rate
;;

(* Test default cache creation *)
let test_default_cache_creation () =
  let cache = Pawn_cache.create_default () in
  let hits, misses, _ = Pawn_cache.stats cache in
  Alcotest.(check int) "Default cache starts empty" 0 (hits + misses)
;;

(* Test pawn hash computation *)
let test_pawn_hash_computation () =
  let pos = Position.default () in
  let white_pawns, black_pawns = get_pawn_bitboards pos in
  let hash1 = Pawn_cache.compute_pawn_hash white_pawns black_pawns in
  let hash2 = Pawn_cache.compute_pawn_hash white_pawns black_pawns in
  Alcotest.(check bool) "Same pawns produce same hash" true (hash1 = hash2)
;;

(* Test that different pawn structures produce different hashes *)
let test_different_pawn_structures () =
  let pos1 = Position.default () in
  let pos2 = Position.of_fen "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1" in
  let white_pawns1, black_pawns1 = get_pawn_bitboards pos1 in
  let white_pawns2, black_pawns2 = get_pawn_bitboards pos2 in
  let hash1 = Pawn_cache.compute_pawn_hash white_pawns1 black_pawns1 in
  let hash2 = Pawn_cache.compute_pawn_hash white_pawns2 black_pawns2 in
  Alcotest.(check bool) "Different pawn structures produce different hashes" true (hash1 <> hash2)
;;

(* Test cache miss on first probe *)
let test_cache_miss () =
  let cache = Pawn_cache.create 1024 in
  let pos = Position.default () in
  let white_pawns, black_pawns = get_pawn_bitboards pos in
  let hash = Pawn_cache.compute_pawn_hash white_pawns black_pawns in
  let result = Pawn_cache.probe cache hash in
  Alcotest.(check bool) "First probe is a miss" true (result = None);
  let _, misses, _ = Pawn_cache.stats cache in
  Alcotest.(check int) "Miss counter incremented" 1 misses
;;

(* Test storing and retrieving from cache *)
let test_cache_store_and_probe () =
  let cache = Pawn_cache.create 1024 in
  let pos = Position.default () in
  let white_pawns, black_pawns = get_pawn_bitboards pos in
  let hash = Pawn_cache.compute_pawn_hash white_pawns black_pawns in
  let score = 42 in
  
  (* Store evaluation *)
  Pawn_cache.store cache hash score;
  
  (* Probe should return the stored value *)
  let result = Pawn_cache.probe cache hash in
  match result with
  | Some s -> Alcotest.(check int) "Stored score retrieved" score s
  | None -> Alcotest.fail "Expected cache hit"
;;

(* Test cache hit statistics *)
let test_cache_hit_statistics () =
  let cache = Pawn_cache.create 1024 in
  let pos = Position.default () in
  let white_pawns, black_pawns = get_pawn_bitboards pos in
  let hash = Pawn_cache.compute_pawn_hash white_pawns black_pawns in
  
  (* First probe - miss *)
  let _ = Pawn_cache.probe cache hash in
  
  (* Store value *)
  Pawn_cache.store cache hash 100;
  
  (* Second probe - hit *)
  let _ = Pawn_cache.probe cache hash in
  
  let hits, misses, hit_rate = Pawn_cache.stats cache in
  Alcotest.(check int) "One hit recorded" 1 hits;
  Alcotest.(check int) "One miss recorded" 1 misses;
  Alcotest.(check (float 0.001)) "Hit rate is 50%" 0.5 hit_rate
;;

(* Test cache replacement *)
let test_cache_replacement () =
  let cache = Pawn_cache.create 1024 in
  let pos1 = Position.default () in
  let white_pawns1, black_pawns1 = get_pawn_bitboards pos1 in
  let hash1 = Pawn_cache.compute_pawn_hash white_pawns1 black_pawns1 in
  
  (* Store first value *)
  Pawn_cache.store cache hash1 100;
  
  (* Verify it's there *)
  let result1 = Pawn_cache.probe cache hash1 in
  Alcotest.(check bool) "First value stored" true (result1 = Some 100);
  
  (* Store second value with same hash (overwrites) *)
  Pawn_cache.store cache hash1 200;
  
  (* Verify it's updated *)
  let result2 = Pawn_cache.probe cache hash1 in
  Alcotest.(check bool) "Value replaced" true (result2 = Some 200)
;;

(* Test cache clearing *)
let test_cache_clear () =
  let cache = Pawn_cache.create 1024 in
  let pos = Position.default () in
  let white_pawns, black_pawns = get_pawn_bitboards pos in
  let hash = Pawn_cache.compute_pawn_hash white_pawns black_pawns in
  
  (* Store a value *)
  Pawn_cache.store cache hash 42;
  
  (* Verify it's there *)
  let result1 = Pawn_cache.probe cache hash in
  Alcotest.(check bool) "Value stored before clear" true (result1 = Some 42);
  
  (* Clear cache *)
  Pawn_cache.clear cache;
  
  (* Verify it's gone *)
  let result2 = Pawn_cache.probe cache hash in
  Alcotest.(check bool) "Cache cleared" true (result2 = None);
  
  (* Check statistics are reset *)
  let hits, misses, _ = Pawn_cache.stats cache in
  Alcotest.(check int) "Hits reset after clear" 0 hits;
  Alcotest.(check int) "Misses reset after clear (after new probe)" 1 misses
;;

(* Test multiple different pawn structures *)
let test_multiple_structures () =
  let cache = Pawn_cache.create 65536 in (* Larger cache to avoid collisions *)
  
  (* Three positions with clearly different pawn structures *)
  let positions = 
    [ Position.default () (* All pawns on ranks 2 and 7 *)
    ; Position.of_fen "rnbqkbnr/ppp1pppp/8/3p4/3P4/8/PPP1PPPP/RNBQKBNR w KQkq - 0 2" (* d4 d5 *)
    ; Position.of_fen "rnbqkb1r/pppp1ppp/5n2/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 4 3" (* e4 e5, Nf3 Nf6 *)
    ]
  in
  
  (* Compute hashes first *)
  let hashes = List.map (fun pos ->
    let white_pawns, black_pawns = get_pawn_bitboards pos in
    Pawn_cache.compute_pawn_hash white_pawns black_pawns
  ) positions in
  
  (* Verify all hashes are different *)
  let h0 = List.nth hashes 0 in
  let h1 = List.nth hashes 1 in
  let h2 = List.nth hashes 2 in
  Alcotest.(check bool) "Hash 0 != Hash 1" true (h0 <> h1);
  Alcotest.(check bool) "Hash 1 != Hash 2" true (h1 <> h2);
  Alcotest.(check bool) "Hash 0 != Hash 2" true (h0 <> h2);
  
  (* Store evaluations *)
  List.iteri (fun i hash ->
    Pawn_cache.store cache hash (i * 100)
  ) hashes;
  
  (* Verify all can be retrieved *)
  List.iteri (fun i hash ->
    match Pawn_cache.probe cache hash with
    | Some score -> Alcotest.(check int) ("Position " ^ string_of_int i ^ " score") (i * 100) score
    | None -> Alcotest.fail ("Position " ^ string_of_int i ^ " not found")
  ) hashes
;;

(* Test global cache functions *)
let test_global_cache () =
  (* Clear global cache first *)
  Pawn_cache.clear_global ();
  
  let global = Pawn_cache.get_global () in
  let hits, misses, _ = Pawn_cache.stats global in
  Alcotest.(check int) "Global cache starts clean" 0 (hits + misses);
  
  (* Use global cache *)
  let pos = Position.default () in
  let white_pawns, black_pawns = get_pawn_bitboards pos in
  let hash = Pawn_cache.compute_pawn_hash white_pawns black_pawns in
  Pawn_cache.store global hash 999;
  
  (* Check global stats *)
  let hits_g, misses_g, _ = Pawn_cache.global_stats () in
  Alcotest.(check int) "Can access global stats" 0 hits_g;
  Alcotest.(check int) "Global stats track activity" 0 misses_g
;;

(* Test position with no pawns *)
let test_no_pawns () =
  let cache = Pawn_cache.create 1024 in
  (* Position with no pawns (kings only endgame) *)
  let pos = Position.of_fen "4k3/8/8/8/8/8/8/4K3 w - - 0 1" in
  let white_pawns, black_pawns = get_pawn_bitboards pos in
  let hash = Pawn_cache.compute_pawn_hash white_pawns black_pawns in
  
  Pawn_cache.store cache hash 0;
  let result = Pawn_cache.probe cache hash in
  Alcotest.(check bool) "Can cache no-pawn position" true (result = Some 0)
;;

(* Test that only pawn positions matter for hash *)
let test_hash_ignores_other_pieces () =
  (* Two positions with same pawns but different pieces *)
  let pos1 = Position.of_fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" in
  let pos2 = Position.of_fen "r1bqkb1r/pppppppp/8/8/8/8/PPPPPPPP/R1BQKB1R w KQkq - 0 1" in
  
  let white_pawns1, black_pawns1 = get_pawn_bitboards pos1 in
  let white_pawns2, black_pawns2 = get_pawn_bitboards pos2 in
  
  let hash1 = Pawn_cache.compute_pawn_hash white_pawns1 black_pawns1 in
  let hash2 = Pawn_cache.compute_pawn_hash white_pawns2 black_pawns2 in
  
  Alcotest.(check bool) "Same pawn structure = same hash" true (hash1 = hash2)
;;

let () =
  let open Alcotest in
  run
    "Pawn_cache"
    [ ( "creation"
      , [ test_case "Create cache" `Quick test_cache_creation
        ; test_case "Create default cache" `Quick test_default_cache_creation
        ] )
    ; ( "hashing"
      , [ test_case "Compute hash" `Quick test_pawn_hash_computation
        ; test_case "Different structures" `Quick test_different_pawn_structures
        ; test_case "Hash ignores non-pawns" `Quick test_hash_ignores_other_pieces
        ; test_case "No pawns" `Quick test_no_pawns
        ] )
    ; ( "probing"
      , [ test_case "Cache miss" `Quick test_cache_miss
        ; test_case "Store and probe" `Quick test_cache_store_and_probe
        ; test_case "Cache replacement" `Quick test_cache_replacement
        ] )
    ; ( "statistics"
      , [ test_case "Hit statistics" `Quick test_cache_hit_statistics
        ; test_case "Multiple structures" `Quick test_multiple_structures
        ] )
    ; ( "management"
      , [ test_case "Clear cache" `Quick test_cache_clear
        ; test_case "Global cache" `Quick test_global_cache
        ] )
    ]
;;
