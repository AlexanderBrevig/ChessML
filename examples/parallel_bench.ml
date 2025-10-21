(** Parallel search benchmark *)

let () =
  Printf.printf "=== Parallel Search Benchmark ===\n%!";
  (* Test position: Start position *)
  let pos =
    Chessml.Position.of_fen
      "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4"
  in
  (*Chessml.Position.fen_startpos in*)
  let test_depth = 7 in
  (* Single-threaded search *)
  Printf.printf "\n--- Single-threaded search (depth %d) ---\n%!" test_depth;
  let start_time = Unix.gettimeofday () in
  let score1, move1, nodes1 =
    Chessml.Parallel_search.single_threaded_search pos test_depth
  in
  let elapsed1 = Unix.gettimeofday () -. start_time in
  Printf.printf
    "Score: %d, Move: %s, Nodes: %Ld, Time: %.3fs, NPS: %.0f\n%!"
    score1
    (match move1 with
     | Some m -> Chessml.Move.to_uci m
     | None -> "none")
    nodes1
    elapsed1
    (Int64.to_float nodes1 /. elapsed1);
  (* 2-threaded search *)
  Printf.printf "\n--- 4-threaded search (depth %d) ---\n%!" test_depth;
  let start_time = Unix.gettimeofday () in
  let score2, move2, nodes2 = Chessml.Parallel_search.parallel_search pos test_depth 4 in
  let elapsed2 = Unix.gettimeofday () -. start_time in
  Printf.printf
    "Score: %d, Move: %s, Nodes: %Ld, Time: %.3fs, NPS: %.0f\n%!"
    score2
    (match move2 with
     | Some m -> Chessml.Move.to_uci m
     | None -> "none")
    nodes2
    elapsed2
    (Int64.to_float nodes2 /. elapsed2);
  let speedup2 = elapsed1 /. elapsed2 in
  Printf.printf "Speedup: %.2fx\n%!" speedup2;
  (* 4-threaded search *)
  Printf.printf "\n--- 8-threaded search (depth %d) ---\n%!" test_depth;
  let start_time = Unix.gettimeofday () in
  let score4, move4, nodes4 = Chessml.Parallel_search.parallel_search pos test_depth 8 in
  let elapsed4 = Unix.gettimeofday () -. start_time in
  Printf.printf
    "Score: %d, Move: %s, Nodes: %Ld, Time: %.3fs, NPS: %.0f\n%!"
    score4
    (match move4 with
     | Some m -> Chessml.Move.to_uci m
     | None -> "none")
    nodes4
    elapsed4
    (Int64.to_float nodes4 /. elapsed4);
  let speedup4 = elapsed1 /. elapsed4 in
  Printf.printf "Speedup: %.2fx\n%!" speedup4;
  Printf.printf "\n=== Benchmark Complete ===\n%!"
;;
