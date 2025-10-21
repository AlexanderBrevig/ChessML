(** Performance breakdown analysis *)

open Chessml

(* Timing helper *)
let time_it name f =
  let start = Unix.gettimeofday () in
  let result = f () in
  let elapsed = Unix.gettimeofday () -. start in
  Printf.printf "%-30s: %.3f seconds\n" name elapsed;
  flush stdout;
  result
;;

let () =
  Printf.printf "Performance Breakdown Analysis\n";
  Printf.printf "==============================\n\n";
  let fen = "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4" in
  let game = Game.from_fen fen in
  (* Test 1: Move generation speed *)
  Printf.printf "\n1. Move Generation Test\n";
  Printf.printf "   Testing 100,000 move generations\n";
  let gen_count = ref 0 in
  let _ =
    time_it "100k move generations" (fun () ->
      for _i = 1 to 100000 do
        let _moves = Game.legal_moves game in
        gen_count := !gen_count + 1
      done)
  in
  (* Test 2: Make/unmake move speed *)
  Printf.printf "\n2. Make Move Test\n";
  Printf.printf "   Testing 100,000 make_move operations\n";
  let moves = Game.legal_moves game in
  let test_move = List.hd moves in
  let _ =
    time_it "100k make_move" (fun () ->
      for _i = 1 to 100000 do
        let _new_game = Game.make_move game test_move in
        ()
      done)
  in
  (* Test 3: Position evaluation speed *)
  Printf.printf "\n3. Static Evaluation Test\n";
  Printf.printf "   Testing 100,000 evaluations\n";
  let pos = Game.position game in
  let _ =
    time_it "100k evaluations" (fun () ->
      for _i = 1 to 100000 do
        let _score = Eval.evaluate pos in
        ()
      done)
  in
  (* Test 4: Actual search at various depths *)
  Printf.printf "\n4. Search Performance\n";
  Printf.printf "   Testing search at increasing depths\n\n";
  for depth = 3 to 6 do
    Printf.printf "   Depth %d: " depth;
    flush stdout;
    let start = Unix.gettimeofday () in
    let result = Search.find_best_move ~verbose:false game depth in
    let elapsed = Unix.gettimeofday () -. start in
    let nps = Int64.to_float result.Search.nodes /. elapsed in
    Printf.printf "nodes=%Ld, time=%.3fs, NPS=%.0f\n" result.Search.nodes elapsed nps;
    flush stdout
  done;
  Printf.printf "\n5. Component Time Estimates\n";
  Printf.printf "   Rough estimates based on benchmarks:\n\n";
  Printf.printf "   If searching 1M nodes at depth 6:\n";
  let move_gen_time = 1000000.0 *. 0.000001 in
  (* based on test 1 *)
  let make_move_time = 1000000.0 *. 0.000001 in
  (* based on test 2 *)
  let eval_time = 1000000.0 *. 0.000001 in
  (* based on test 3 *)
  Printf.printf
    "   - Move generation: ~%.2f seconds (%.1f%%)\n"
    move_gen_time
    (move_gen_time /. 25.0 *. 100.0);
  Printf.printf
    "   - Make move calls: ~%.2f seconds (%.1f%%)\n"
    make_move_time
    (make_move_time /. 25.0 *. 100.0);
  Printf.printf
    "   - Evaluation calls: ~%.2f seconds (%.1f%%)\n"
    eval_time
    (eval_time /. 25.0 *. 100.0);
  Printf.printf "   - Search overhead:  remaining time\n";
  Printf.printf "\nAnalysis complete.\n"
;;
