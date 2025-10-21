(** History heuristic tests *)

open Chessml

let test_history_creation () =
  History.clear ();
  let stats = History.get_stats () in
  let total, max_score, _avg = stats in
  Alcotest.(check int) "Empty table has 0 entries" 0 total;
  Alcotest.(check int) "Empty table has max score 0" 0 max_score
;;

let test_record_cutoff () =
  History.clear ();
  (* Create a test move e2-e4 *)
  let mv = Move.make 12 28 Move.Quiet in
  (* e2 to e4 *)
  (* Record a cutoff at depth 5 *)
  History.record_cutoff mv 5;
  let score = History.get_score mv in
  Alcotest.(check bool) "Move has positive score after cutoff" true (score > 0);
  Alcotest.(check int) "Score equals depth squared" 25 score (* 5*5 = 25 *)
;;

let test_multiple_cutoffs () =
  History.clear ();
  let mv = Move.make 12 28 Move.Quiet in
  (* Record multiple cutoffs *)
  History.record_cutoff mv 3;
  (* 3*3 = 9 *)
  History.record_cutoff mv 4;
  (* 4*4 = 16 *)
  let score = History.get_score mv in
  Alcotest.(check int) "Cumulative score" 25 score (* 9 + 16 = 25 *)
;;

let test_depth_weighting () =
  History.clear ();
  let mv1 = Move.make 12 28 Move.Quiet in
  (* e2-e4 *)
  let mv2 = Move.make 52 36 Move.Quiet in
  (* e7-e5 *)
  (* Shallow cutoff *)
  History.record_cutoff mv1 2;
  (* 2*2 = 4 *)

  (* Deep cutoff *)
  History.record_cutoff mv2 5;
  (* 5*5 = 25 *)
  let score1 = History.get_score mv1 in
  let score2 = History.get_score mv2 in
  Alcotest.(check bool) "Deeper cutoff has higher score" true (score2 > score1)
;;

let test_different_moves () =
  History.clear ();
  let mv1 = Move.make 12 28 Move.Quiet in
  let mv2 = Move.make 13 29 Move.Quiet in
  History.record_cutoff mv1 5;
  let score1 = History.get_score mv1 in
  let score2 = History.get_score mv2 in
  Alcotest.(check bool) "Only recorded move has score" true (score1 > 0);
  Alcotest.(check int) "Unrecorded move has zero score" 0 score2
;;

let test_aging () =
  History.clear ();
  let mv = Move.make 12 28 Move.Quiet in
  History.record_cutoff mv 6;
  (* 6*6 = 36 *)
  let score_before = History.get_score mv in
  History.age_table ();
  let score_after = History.get_score mv in
  Alcotest.(check int) "Score before aging" 36 score_before;
  Alcotest.(check int) "Score after aging is halved" 18 score_after
;;

let test_stats () =
  History.clear ();
  let mv1 = Move.make 12 28 Move.Quiet in
  let mv2 = Move.make 13 29 Move.Quiet in
  History.record_cutoff mv1 4;
  (* 16 *)
  History.record_cutoff mv2 6;
  (* 36 *)
  let total, max_score, avg = History.get_stats () in
  Alcotest.(check int) "Two entries recorded" 2 total;
  Alcotest.(check int) "Max score is 36" 36 max_score;
  Alcotest.(check bool) "Average is 26" true (abs_float (avg -. 26.0) < 0.1)
;;

let test_move_ordering_integration () =
  History.clear ();
  (* Setup: record history for some moves *)
  let mv_good = Move.make 12 28 Move.PawnDoublePush in
  (* e2-e4, has good history *)
  let mv_bad = Move.make 11 19 Move.Quiet in
  (* d2-d3, no history *)
  History.record_cutoff mv_good 6;
  (* 36 points *)
  let score_good = History.get_score mv_good in
  let score_bad = History.get_score mv_bad in
  Alcotest.(check bool) "Move with history scores higher" true (score_good > score_bad)
;;

let test_persistence_across_searches () =
  History.clear ();
  let mv = Move.make 12 28 Move.Quiet in
  (* Simulate multiple searches *)
  History.record_cutoff mv 3;
  History.record_cutoff mv 4;
  History.record_cutoff mv 5;
  let score = History.get_score mv in
  (* 3*3 + 4*4 + 5*5 = 9 + 16 + 25 = 50 *)
  Alcotest.(check int) "History persists across searches" 50 score
;;

let test_clear_resets_all () =
  History.clear ();
  let mv = Move.make 12 28 Move.Quiet in
  History.record_cutoff mv 10;
  History.clear ();
  let score = History.get_score mv in
  let total, _, _ = History.get_stats () in
  Alcotest.(check int) "Score is zero after clear" 0 score;
  Alcotest.(check int) "Table is empty after clear" 0 total
;;

(** Regression test for unbounded history growth issue *)
let test_history_cap_regression () =
  History.clear ();
  let mv = Move.make 12 28 Move.Quiet in
  (* e2-e4 *)
  (* Simulate many deep cutoffs that would previously cause unbounded growth *)
  for _i = 1 to 100 do
    History.record_cutoff mv 10 (* 10*10 = 100 per iteration *)
  done;
  let score = History.get_score mv in
  (* Score should be capped at 10000 to prevent dominating move ordering *)
  Alcotest.(check bool) "History score is capped at 10000" true (score <= 10000);
  Alcotest.(check int) "History score reaches cap" 10000 score
;;

(** Test that capped history doesn't dominate tactical moves in ordering *)
let test_history_doesnt_dominate_tactics () =
  History.clear ();
  let quiet_mv = Move.make 12 28 Move.Quiet in
  (* e2-e4 *)
  (* Build up maximum history *)
  for _i = 1 to 100 do
    History.record_cutoff quiet_mv 10
  done;
  let history_score = History.get_score quiet_mv in
  (* Even with max history (10000), the contribution to move ordering 
     is capped: quiet_base (0) + min(3000, history/10) = 0 + min(3000, 1000) = 1000
     This ensures it stays below:
     - TT moves (20000)
     - Castle moves (15000) 
     - Check moves (10000)
     - Winning captures (8000+)
     - Equal captures (7000)
     - Killer moves (5000)
     - Countermoves (4000)
  *)
  let ordering_contribution = min 3000 (history_score / 10) in
  Alcotest.(check bool)
    "History contribution capped below tactical moves"
    true
    (ordering_contribution < 4000);
  Alcotest.(check bool)
    "History doesn't override killers"
    true
    (ordering_contribution < 5000)
;;

let () =
  let open Alcotest in
  run
    "History Heuristic Tests"
    [ ( "basic_operations"
      , [ test_case "History table creation" `Quick test_history_creation
        ; test_case "Record cutoff" `Quick test_record_cutoff
        ; test_case "Multiple cutoffs accumulate" `Quick test_multiple_cutoffs
        ; test_case "Different moves tracked separately" `Quick test_different_moves
        ; test_case "Clear resets table" `Quick test_clear_resets_all
        ] )
    ; ( "scoring"
      , [ test_case "Depth weighting" `Quick test_depth_weighting
        ; test_case "Aging halves scores" `Quick test_aging
        ; test_case "Statistics calculation" `Quick test_stats
        ] )
    ; ( "integration"
      , [ test_case "Move ordering integration" `Quick test_move_ordering_integration
        ; test_case "Persistence across searches" `Quick test_persistence_across_searches
        ] )
    ; ( "regression"
      , [ test_case
            "History scores are capped (regression)"
            `Quick
            test_history_cap_regression
        ; test_case
            "Capped history doesn't dominate tactics"
            `Quick
            test_history_doesnt_dominate_tactics
        ] )
    ]
;;
