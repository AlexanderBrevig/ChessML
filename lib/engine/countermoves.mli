(** Countermove heuristic: track best refutations for each move *)

open Chessml_core

(** Countermove table type *)
type t

(** Create a new countermove table *)
val create : unit -> t

(** Get the countermove for a given move *)
val get : t -> Move.t -> Move.t option

(** Update the countermove table with a new countermove *)
val update : t -> Move.t -> Move.t -> unit

(** Check if a move is the countermove for a previous move *)
val is_countermove : t -> Move.t option -> Move.t -> bool

(** Clear the countermove table *)
val clear : t -> unit
