open Chessml

(* Demonstrate the history heuristic improving move ordering over multiple searches *)

let print_search_result result =
  let total_moves, total_score, avg_score = History.get_stats () in
  Printf.printf "Nodes: %Ld, Depth: %d\n" result.Search.nodes result.Search.depth;
  Printf.printf
    "History stats - Tracked moves: %d, Total score: %d, Avg: %.1f\n\n"
    total_moves
    total_score
    avg_score
;;

let () =
  Printf.printf "=== History Heuristic Demo ===\n\n";
  Printf.printf
    "This demo shows how the history heuristic learns from previous searches.\n";
  Printf.printf
    "Moves that cause beta cutoffs get higher scores (weighted by depth²).\n\n";
  (* Create a tactical position *)
  let game =
    Game.of_fen "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 0 1"
  in
  Printf.printf "Position: Italian Game opening\n\n";
  (* First search - fresh history table *)
  History.clear ();
  Printf.printf "Search 1 (depth 4, empty history):\n";
  let result1 = Search.find_best_move ~verbose:false game 4 in
  (match result1.Search.best_move with
   | Some mv ->
     Printf.printf "Best move: %s (score: %d)\n" (Move.to_string mv) result1.Search.score
   | None -> Printf.printf "No move found\n");
  print_search_result result1;
  (* Second search - uses history from first search *)
  Printf.printf "Search 2 (depth 4, with learned history):\n";
  let result2 = Search.find_best_move ~verbose:false game 4 in
  (match result2.Search.best_move with
   | Some mv ->
     Printf.printf "Best move: %s (score: %d)\n" (Move.to_string mv) result2.Search.score
   | None -> Printf.printf "No move found\n");
  print_search_result result2;
  (* Third search - more accumulated history *)
  Printf.printf "Search 3 (depth 4, more accumulated history):\n";
  let result3 = Search.find_best_move ~verbose:false game 4 in
  (match result3.Search.best_move with
   | Some mv ->
     Printf.printf "Best move: %s (score: %d)\n" (Move.to_string mv) result3.Search.score
   | None -> Printf.printf "No move found\n");
  print_search_result result3;
  (* Try a different position to show history persists *)
  Printf.printf "=== Different position (Ruy Lopez) ===\n\n";
  let game2 =
    Game.of_fen "r1bqkbnr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 0 1"
  in
  Printf.printf "Search (depth 4, history persists across positions):\n";
  let result4 = Search.find_best_move ~verbose:false game2 4 in
  (match result4.Search.best_move with
   | Some mv ->
     Printf.printf "Best move: %s (score: %d)\n" (Move.to_string mv) result4.Search.score
   | None -> Printf.printf "No move found\n");
  print_search_result result4;
  Printf.printf "=== Key observations ===\n";
  Printf.printf "- History table learns moves that cause beta cutoffs\n";
  Printf.printf "- Scoring is weighted by depth² (deeper = more valuable)\n";
  Printf.printf "- Table is aged (halved) between searches to prevent staleness\n";
  Printf.printf "- History persists across different positions (global learning)\n";
  Printf.printf
    "- Complements killer moves: killers are depth-specific, history is global\n";
  Printf.printf "- Move ordering: TT move > Killers > History > MVV-LVA > Quiet moves\n"
;;
