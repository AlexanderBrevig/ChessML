(** Killer move heuristic interface *)

open Chessml_core

(** Killer move table type *)
type killer_table

(** Create a new killer move table with given max ply *)
val create : int -> killer_table

(** Clear all killer moves in table *)
val clear : killer_table -> unit

(** Store a killer move at given ply *)
val store_killer : killer_table -> int -> Move.t -> unit

(** Get killer moves for a given ply *)
val get_killers : killer_table -> int -> Move.t list

(** Check if a move is a killer move at given ply *)
val is_killer : killer_table -> int -> Move.t -> bool
