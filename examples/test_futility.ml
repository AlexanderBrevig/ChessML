(** Test futility pruning effectiveness *)

let test_position name fen depth =
  Printf.printf "\n=== %s (depth %d) ===\n" name depth;
  let pos = Chessml.Position.of_fen fen in
  (* Single-threaded search with futility pruning *)
  Printf.printf "Searching...\n%!";
  let start = Unix.gettimeofday () in
  let score, move, nodes = Chessml.Parallel_search.single_threaded_search pos depth in
  let elapsed = Unix.gettimeofday () -. start in
  Printf.printf "Score: %d\n" score;
  Printf.printf
    "Best move: %s\n"
    (match move with
     | Some m -> Chessml.Move.to_uci m
     | None -> "none");
  Printf.printf "Nodes: %Ld\n" nodes;
  Printf.printf "Time: %.3fs\n" elapsed;
  Printf.printf "NPS: %.0f\n" (Int64.to_float nodes /. elapsed);
  nodes, elapsed
;;

let () =
  Printf.printf "╔════════════════════════════════════════════════╗\n";
  Printf.printf "║       Futility Pruning Test Suite             ║\n";
  Printf.printf "╚════════════════════════════════════════════════╝\n";
  let positions =
    [ "Start position", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 6
    ; ( "Italian Game"
      , "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4"
      , 6 )
    ; ( "Quiet middlegame"
      , "r1bq1rk1/pp1nbppp/2p1pn2/3p4/2PP4/2NBPN2/PP3PPP/R1BQ1RK1 w - - 0 10"
      , 6 )
    ]
  in
  let results =
    List.map (fun (name, fen, depth) -> test_position name fen depth) positions
  in
  let total_nodes = List.fold_left (fun acc (n, _) -> Int64.add acc n) 0L results in
  let total_time = List.fold_left (fun acc (_, t) -> acc +. t) 0.0 results in
  Printf.printf "\n╔════════════════════════════════════════════════╗\n";
  Printf.printf "║                   SUMMARY                      ║\n";
  Printf.printf "╚════════════════════════════════════════════════╝\n";
  Printf.printf "Total nodes: %Ld\n" total_nodes;
  Printf.printf "Total time: %.3fs\n" total_time;
  Printf.printf "Average NPS: %.0f\n" (Int64.to_float total_nodes /. total_time);
  Printf.printf "\n✓ Futility pruning should reduce nodes at depths 1-3\n";
  Printf.printf "  by skipping quiet moves in hopeless positions.\n"
;;
