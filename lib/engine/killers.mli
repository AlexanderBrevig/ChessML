(** Killer move heuristic interface *)

open Chessml_core

(** Killer move table type *)
type killer_table

(** Create a new killer move table with given max depth *)
val create : int -> killer_table

(** Clear all killer moves in table *)
val clear : killer_table -> unit

(** Store a killer move at given depth *)
val store_killer : killer_table -> int -> Move.t -> unit

(** Get killer moves for a given depth *)
val get_killers : killer_table -> int -> Move.t list

(** Check if a move is a killer move at given depth *)
val is_killer : killer_table -> int -> Move.t -> bool

(** Global killer table operations *)
val clear_global : unit -> unit

val store_global_killer : int -> Move.t -> unit
val get_global_killers : int -> Move.t list
val is_global_killer : int -> Move.t -> bool
