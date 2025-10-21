(** Countermoves - Refutation move heuristic for move ordering
    
    Tracks the best refutation (countermove) for each opponent move. When the
    opponent plays move X, we remember which of our moves Y caused a beta cutoff.
    Next time opponent plays X, we try Y first. Indexed by [from_square][to_square].
    
    Used by: Search move ordering (score: 4000, between killers and quiet moves)
*)

open Chessml_core

(** Countermove table: [from][to] -> countermove *)
type t = Move.t option array array

(** Create a new countermove table *)
let create () : t = Array.init 64 (fun _ -> Array.make 64 None)

(** Get the countermove for a given move *)
let get (table : t) (mv : Move.t) : Move.t option =
  let from = Move.from mv in
  let to_sq = Move.to_square mv in
  if from >= 0 && from < 64 && to_sq >= 0 && to_sq < 64
  then table.(from).(to_sq)
  else None
;;

(** Update the countermove table *)
let update (table : t) (prev_move : Move.t) (best_move : Move.t) : unit =
  let from = Move.from prev_move in
  let to_sq = Move.to_square prev_move in
  if from >= 0 && from < 64 && to_sq >= 0 && to_sq < 64
  then table.(from).(to_sq) <- Some best_move
;;

(** Check if a move is the countermove for a previous move *)
let is_countermove (table : t) (prev_move_opt : Move.t option) (mv : Move.t) : bool =
  match prev_move_opt with
  | None -> false
  | Some prev_move ->
    (match get table prev_move with
     | None -> false
     | Some counter ->
       Move.from mv = Move.from counter
       && Move.to_square mv = Move.to_square counter
       && Move.kind mv = Move.kind counter)
;;

(** Clear the countermove table *)
let clear (table : t) : unit =
  for i = 0 to 63 do
    for j = 0 to 63 do
      table.(i).(j) <- None
    done
  done
;;
