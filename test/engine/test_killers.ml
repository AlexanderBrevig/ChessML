(** Killer move heuristic tests *)

open Chessml

let test_killer_table_basic () =
  let table = Killers.create 10 in
  let move1 = Move.make (Square.of_uci "e2") (Square.of_uci "e4") Move.Quiet in
  let move2 = Move.make (Square.of_uci "d2") (Square.of_uci "d4") Move.Quiet in
  (* Initially no killers *)
  Alcotest.(check int) "No initial killers" 0 (List.length (Killers.get_killers table 0));
  (* Store first killer *)
  Killers.store_killer table 0 move1;
  let killers = Killers.get_killers table 0 in
  Alcotest.(check int) "One killer stored" 1 (List.length killers);
  Alcotest.(check bool) "Move1 is killer" true (Killers.is_killer table 0 move1);
  Alcotest.(check bool) "Move2 is not killer" false (Killers.is_killer table 0 move2);
  (* Store second killer *)
  Killers.store_killer table 0 move2;
  let killers = Killers.get_killers table 0 in
  Alcotest.(check int) "Two killers stored" 2 (List.length killers);
  Alcotest.(check bool) "Move1 is still killer" true (Killers.is_killer table 0 move1);
  Alcotest.(check bool) "Move2 is killer" true (Killers.is_killer table 0 move2)
;;

let test_killer_table_replacement () =
  let table = Killers.create 10 in
  let move1 = Move.make (Square.of_uci "e2") (Square.of_uci "e4") Move.Quiet in
  let move2 = Move.make (Square.of_uci "d2") (Square.of_uci "d4") Move.Quiet in
  let move3 = Move.make (Square.of_uci "c2") (Square.of_uci "c4") Move.Quiet in
  (* Store two killers *)
  Killers.store_killer table 0 move1;
  Killers.store_killer table 0 move2;
  (* Store third killer - should replace oldest *)
  Killers.store_killer table 0 move3;
  let killers = Killers.get_killers table 0 in
  Alcotest.(check int) "Still two killers after replacement" 2 (List.length killers);
  Alcotest.(check bool) "Move1 replaced" false (Killers.is_killer table 0 move1);
  Alcotest.(check bool) "Move2 is killer" true (Killers.is_killer table 0 move2);
  Alcotest.(check bool) "Move3 is killer" true (Killers.is_killer table 0 move3)
;;

let test_killer_table_depth_separation () =
  let table = Killers.create 10 in
  let move1 = Move.make (Square.of_uci "e2") (Square.of_uci "e4") Move.Quiet in
  let move2 = Move.make (Square.of_uci "d2") (Square.of_uci "d4") Move.Quiet in
  (* Store killers at different depths *)
  Killers.store_killer table 0 move1;
  Killers.store_killer table 1 move2;
  (* Check depth separation *)
  Alcotest.(check bool)
    "Move1 is killer at depth 0"
    true
    (Killers.is_killer table 0 move1);
  Alcotest.(check bool)
    "Move1 is not killer at depth 1"
    false
    (Killers.is_killer table 1 move1);
  Alcotest.(check bool)
    "Move2 is killer at depth 1"
    true
    (Killers.is_killer table 1 move2);
  Alcotest.(check bool)
    "Move2 is not killer at depth 0"
    false
    (Killers.is_killer table 0 move2)
;;

let test_killer_table_clear () =
  let table = Killers.create 10 in
  let move1 = Move.make (Square.of_uci "e2") (Square.of_uci "e4") Move.Quiet in
  (* Store killer and verify *)
  Killers.store_killer table 0 move1;
  Alcotest.(check bool)
    "Move1 is killer before clear"
    true
    (Killers.is_killer table 0 move1);
  (* Clear and verify *)
  Killers.clear table;
  Alcotest.(check bool)
    "Move1 is not killer after clear"
    false
    (Killers.is_killer table 0 move1);
  Alcotest.(check int)
    "No killers after clear"
    0
    (List.length (Killers.get_killers table 0))
;;

let test_killer_duplicate_prevention () =
  let table = Killers.create 10 in
  let move1 = Move.make (Square.of_uci "e2") (Square.of_uci "e4") Move.Quiet in
  (* Store same killer twice *)
  Killers.store_killer table 0 move1;
  Killers.store_killer table 0 move1;
  (* Should only have one instance *)
  let killers = Killers.get_killers table 0 in
  Alcotest.(check int) "Only one instance of duplicate killer" 1 (List.length killers)
;;

let test_global_killer_operations () =
  let move1 = Move.make (Square.of_uci "e2") (Square.of_uci "e4") Move.Quiet in
  let move2 = Move.make (Square.of_uci "d2") (Square.of_uci "d4") Move.Quiet in
  (* Clear global table *)
  Killers.clear_global ();
  (* Store global killers *)
  Killers.store_global_killer 0 move1;
  Killers.store_global_killer 1 move2;
  (* Check global operations *)
  Alcotest.(check bool)
    "Move1 is global killer at depth 0"
    true
    (Killers.is_global_killer 0 move1);
  Alcotest.(check bool)
    "Move2 is global killer at depth 1"
    true
    (Killers.is_global_killer 1 move2);
  Alcotest.(check bool)
    "Move1 is not global killer at depth 1"
    false
    (Killers.is_global_killer 1 move1);
  let global_killers_0 = Killers.get_global_killers 0 in
  let global_killers_1 = Killers.get_global_killers 1 in
  Alcotest.(check int) "One killer at depth 0" 1 (List.length global_killers_0);
  Alcotest.(check int) "One killer at depth 1" 1 (List.length global_killers_1)
;;

let test_search_with_killers_performance () =
  (* Test a position where killers should help reduce nodes *)
  let game =
    Game.of_fen "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3"
  in
  (* Search the same position twice to allow killers to be established *)
  let result1 = Search.find_best_move ~verbose:false game 4 in
  let result2 = Search.find_best_move ~verbose:false game 4 in
  (* Both searches should complete successfully *)
  Alcotest.(check bool) "First search completes" true (result1.best_move <> None);
  Alcotest.(check bool) "Second search completes" true (result2.best_move <> None);
  (* Node counts should be positive *)
  Alcotest.(check bool) "First search has positive nodes" true (result1.nodes > 0L);
  Alcotest.(check bool) "Second search has positive nodes" true (result2.nodes > 0L);
  (* Scores should be reasonable *)
  Alcotest.(check bool) "First search score reasonable" true (abs result1.score < 10000);
  Alcotest.(check bool) "Second search score reasonable" true (abs result2.score < 10000)
;;

let test_killer_integration_with_search () =
  (* Test that killers are actually stored during search *)
  let game = Game.default () in
  (* Clear global killers *)
  Killers.clear_global ();
  (* Perform a search that should generate some killers *)
  let _result = Search.find_best_move ~verbose:false game 3 in
  (* Check if any killers were stored (depth 0, 1, 2 should have some activity) *)
  let killers_depth_0 = Killers.get_global_killers 0 in
  let killers_depth_1 = Killers.get_global_killers 1 in
  let killers_depth_2 = Killers.get_global_killers 2 in
  let total_killers =
    List.length killers_depth_0
    + List.length killers_depth_1
    + List.length killers_depth_2
  in
  (* We should have some killers stored during search *)
  Alcotest.(check bool) "Search generates killer moves" true (total_killers > 0)
;;

let test_killer_bounds_safety () =
  let table = Killers.create 5 in
  let move1 = Move.make (Square.of_uci "e2") (Square.of_uci "e4") Move.Quiet in
  (* Test bounds - negative depth *)
  Killers.store_killer table (-1) move1;
  Alcotest.(check bool)
    "Negative depth ignored"
    false
    (Killers.is_killer table (-1) move1);
  (* Test bounds - depth too large *)
  Killers.store_killer table 10 move1;
  Alcotest.(check bool) "Excessive depth ignored" false (Killers.is_killer table 10 move1);
  (* Test valid bounds *)
  Killers.store_killer table 2 move1;
  Alcotest.(check bool) "Valid depth works" true (Killers.is_killer table 2 move1)
;;

let () =
  let open Alcotest in
  run
    "Killer Moves"
    [ ( "basic_operations"
      , [ test_case "Basic killer table operations" `Quick test_killer_table_basic
        ; test_case "Killer replacement" `Quick test_killer_table_replacement
        ; test_case "Depth separation" `Quick test_killer_table_depth_separation
        ; test_case "Table clearing" `Quick test_killer_table_clear
        ; test_case "Duplicate prevention" `Quick test_killer_duplicate_prevention
        ] )
    ; ( "global_operations"
      , [ test_case "Global killer operations" `Quick test_global_killer_operations
        ; test_case "Search integration" `Quick test_killer_integration_with_search
        ; test_case "Bounds safety" `Quick test_killer_bounds_safety
        ] )
    ; ( "performance"
      , [ test_case
            "Search with killers performance"
            `Quick
            test_search_with_killers_performance
        ] )
    ]
;;
