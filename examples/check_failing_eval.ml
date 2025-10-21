open Chessml

let () =
  let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/R1BQKBNR w KQkq - 0 1" in
  let pos = Position.of_fen fen in
  let eval = Eval.evaluate pos in
  Printf.printf "FEN: %s\n" fen;
  Printf.printf "Evaluation: %d\n" eval;
  Printf.printf "Is eval < -200? %b\n" (eval < -200);
  Printf.printf "Is eval > -420? %b\n" (eval > -420);
  Printf.printf "\n";
  Printf.printf "Test expects: eval < -200 AND eval > -420\n";
  Printf.printf "Test passes: %b\n" (eval < -200 && eval > -420)
;;
