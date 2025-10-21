(** Perft - Performance testing and move generation verification
    
    Counts leaf nodes at a given depth to verify move generation correctness.
    Standard debugging tool in chess programming for validating legal move
    generation, special moves, and ensuring no illegal moves are generated.
    
    Used for: Testing, debugging, benchmarking move generator performance
*)

let rec perft game depth =
  if depth = 0
  then 1
  else
    Game.legal_moves game
    |> List.fold_left (fun acc mv -> acc + perft (Game.make_move game mv) (depth - 1)) 0
;;
