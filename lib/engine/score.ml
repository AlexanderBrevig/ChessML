(** Score - Search score conventions shared by search, TT and protocols

    Scores are centipawns from the side to move's perspective. Mate scores encode
    the distance to mate: being mated at ply [p] from the root scores
    [-mate + p], so shorter mates score higher. Transposition table entries store
    mate scores relative to the node instead of the root ({!to_tt}/{!of_tt}).
*)

let infinity = 32000
let mate = 31000
let draw = 0
let max_ply = 256

(** Any score beyond this is a mate score *)
let mate_bound = mate - max_ply

let is_mate score = abs score >= mate_bound

(** Score for the side to move being checkmated at [ply] from the root *)
let mated_in ply = -mate + ply

(** Convert a root-relative score to a node-relative one before storing in the TT *)
let to_tt score ply =
  if score >= mate_bound
  then score + ply
  else if score <= -mate_bound
  then score - ply
  else score
;;

(** Convert a node-relative TT score back to root-relative at [ply] *)
let of_tt score ply =
  if score >= mate_bound
  then score - ply
  else if score <= -mate_bound
  then score + ply
  else score
;;

(** Moves to mate, positive if the side to move mates, negative if it is mated *)
let mate_in_moves score =
  if score > 0 then (mate - score + 1) / 2 else -((mate + score) / 2)
;;

(** UCI "score" field: "cp N" or "mate N" *)
let to_uci score =
  if is_mate score
  then Printf.sprintf "mate %d" (mate_in_moves score)
  else Printf.sprintf "cp %d" score
;;
