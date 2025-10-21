(** Example of running perft tests to verify move generation *)

open Chessml

let rec perft game depth =
  if depth = 0
  then 1
  else
    Game.legal_moves game
    |> List.fold_left (fun acc mv -> acc + perft (Game.make_move game mv) (depth - 1)) 0
;;

let () =
  Printf.printf "ChessML - Perft Example\n";
  Printf.printf "==============================\n\n";
  let game = Game.default () in
  Printf.printf "Testing starting position:\n";
  Printf.printf "FEN: %s\n\n" (Game.to_fen game);
  for depth = 1 to 4 do
    let nodes = perft game depth in
    Printf.printf "Depth %d: %d nodes\n" depth nodes
  done;
  Printf.printf "\nExpected values:\n";
  Printf.printf "Depth 1: 20 nodes\n";
  Printf.printf "Depth 2: 400 nodes\n";
  Printf.printf "Depth 3: 8902 nodes\n";
  Printf.printf "Depth 4: 197281 nodes\n"
;;
