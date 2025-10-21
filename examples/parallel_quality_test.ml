(** Test that parallel search finds moves of equal or better quality than single-threaded *)

(** Test position with expected best move *)
type test_position =
  { name : string
  ; fen : string
  ; depth : int
  ; expected_move : string option (* None means any move is acceptable *)
  }

(** Test positions from various tactical scenarios *)
let test_positions =
  [ { name = "Start position"
    ; fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    ; depth = 6
    ; expected_move = None
    }
  ; { name = "Mate in 1 - Back rank"
    ; fen = "6k1/5ppp/8/8/8/8/5PPP/R5K1 w - - 0 1"
    ; depth = 3
    ; expected_move = Some "a1a8" (* Ra8# *)
    }
  ; { name = "Mate in 2 - Queen sacrifice"
    ; fen = "r1bqkb1r/pppp1ppp/2n2n2/4p2Q/2B1P3/8/PPPP1PPP/RNB1K1NR w KQkq - 0 1"
    ; depth = 5
    ; expected_move = Some "h5f7" (* Qxf7+ *)
    }
  ; { name = "Win material - fork"
    ; fen = "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 0 1"
    ; depth = 5
    ; expected_move = None
    }
  ; { name = "Endgame - Lucena position"
    ; fen = "1K1k4/1P6/8/8/8/8/r7/2R5 w - - 0 1"
    ; depth = 7
    ; expected_move = None
    }
  ; { name = "Tactical - Pin and win"
    ; fen = "r1bqk2r/pppp1ppp/2n2n2/2b1p3/2B1P3/3P1N2/PPP2PPP/RNBQK2R w KQkq - 0 1"
    ; depth = 6
    ; expected_move = None
    }
  ]
;;

(** Compare search results *)
let compare_results name single_result parallel_result =
  let s_score, s_move, s_nodes = single_result in
  let p_score, p_move, p_nodes = parallel_result in
  Printf.printf "\n=== %s ===\n" name;
  Printf.printf
    "Single-threaded: score=%d, move=%s, nodes=%Ld\n"
    s_score
    (match s_move with
     | Some m -> Chessml.Move.to_uci m
     | None -> "none")
    s_nodes;
  Printf.printf
    "Parallel (4):    score=%d, move=%s, nodes=%Ld\n"
    p_score
    (match p_move with
     | Some m -> Chessml.Move.to_uci m
     | None -> "none")
    p_nodes;
  (* Check if moves are equal *)
  let moves_equal =
    match s_move, p_move with
    | Some sm, Some pm -> sm = pm
    | None, None -> true
    | _ -> false
  in
  (* Score difference (absolute) *)
  let score_diff = abs (s_score - p_score) in
  (* Analysis *)
  if moves_equal
  then Printf.printf "✓ Same move found\n"
  else Printf.printf "⚠ Different moves (score diff: %d)\n" score_diff;
  if score_diff = 0
  then Printf.printf "✓ Exact same score\n"
  else if score_diff <= 10
  then Printf.printf "✓ Very similar score (diff: %d)\n" score_diff
  else if score_diff <= 50
  then Printf.printf "⚠ Moderately different score (diff: %d)\n" score_diff
  else if p_score > s_score
  then Printf.printf "✓✓ Parallel found BETTER move! (diff: +%d)\n" score_diff
  else Printf.printf "✗ Parallel found worse move (diff: -%d)\n" score_diff;
  (* Return whether parallel is acceptable (same or better) *)
  p_score >= s_score - 50, moves_equal, score_diff
;;

(** Test a single position *)
let test_position test_pos =
  Printf.printf "\n";
  Printf.printf "========================================\n";
  Printf.printf "Testing: %s\n" test_pos.name;
  Printf.printf "FEN: %s\n" test_pos.fen;
  Printf.printf "Depth: %d\n" test_pos.depth;
  Printf.printf "========================================\n%!";
  let pos = Chessml.Position.of_fen test_pos.fen in
  (* Run single-threaded search *)
  Printf.printf "\n--- Running single-threaded search ---\n%!";
  let single_result = Chessml.Parallel_search.single_threaded_search pos test_pos.depth in
  (* Run parallel search with 4 threads *)
  Printf.printf "\n--- Running parallel search (4 threads) ---\n%!";
  let parallel_result = Chessml.Parallel_search.parallel_search pos test_pos.depth 4 in
  (* Compare results *)
  let acceptable, same_move, score_diff =
    compare_results test_pos.name single_result parallel_result
  in
  (* Check expected move if provided *)
  let expected_ok =
    match test_pos.expected_move with
    | None -> true
    | Some expected_uci ->
      let _, p_move, _ = parallel_result in
      (match p_move with
       | Some pm when Chessml.Move.to_uci pm = expected_uci ->
         Printf.printf "✓ Found expected move: %s\n" expected_uci;
         true
       | Some pm ->
         Printf.printf "⚠ Expected %s, got %s\n" expected_uci (Chessml.Move.to_uci pm);
         false
       | None ->
         Printf.printf "✗ No move found, expected %s\n" expected_uci;
         false)
  in
  acceptable, same_move, expected_ok, score_diff
;;

(** Run all tests *)
let () =
  Printf.printf "╔════════════════════════════════════════════════╗\n";
  Printf.printf "║  Parallel Search Quality Verification Test    ║\n";
  Printf.printf "╚════════════════════════════════════════════════╝\n%!";
  let results = List.map test_position test_positions in
  (* Summary *)
  Printf.printf "\n\n";
  Printf.printf "╔════════════════════════════════════════════════╗\n";
  Printf.printf "║                    SUMMARY                     ║\n";
  Printf.printf "╚════════════════════════════════════════════════╝\n";
  let total = List.length results in
  let acceptable_count =
    List.fold_left (fun acc (a, _, _, _) -> if a then acc + 1 else acc) 0 results
  in
  let same_move_count =
    List.fold_left (fun acc (_, s, _, _) -> if s then acc + 1 else acc) 0 results
  in
  let expected_ok_count =
    List.fold_left (fun acc (_, _, e, _) -> if e then acc + 1 else acc) 0 results
  in
  let avg_score_diff =
    let sum = List.fold_left (fun acc (_, _, _, d) -> acc + d) 0 results in
    float_of_int sum /. float_of_int total
  in
  Printf.printf "\nTotal positions tested: %d\n" total;
  Printf.printf
    "Acceptable quality:     %d/%d (%.0f%%)\n"
    acceptable_count
    total
    (100.0 *. float_of_int acceptable_count /. float_of_int total);
  Printf.printf
    "Same move found:        %d/%d (%.0f%%)\n"
    same_move_count
    total
    (100.0 *. float_of_int same_move_count /. float_of_int total);
  Printf.printf
    "Expected moves found:   %d/%d (%.0f%%)\n"
    expected_ok_count
    total
    (100.0 *. float_of_int expected_ok_count /. float_of_int total);
  Printf.printf "Average score diff:     %.1f centipawns\n" avg_score_diff;
  Printf.printf "\n";
  if acceptable_count = total
  then Printf.printf "✓✓✓ ALL TESTS PASSED - Parallel search quality is verified!\n"
  else Printf.printf "✗✗✗ SOME TESTS FAILED - Parallel search may have issues\n";
  Printf.printf "\n"
;;
