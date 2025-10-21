(** Concurrent Transposition Table Interface *)

open Chessml_core

type entry_type =
  | Exact
  | LowerBound
  | UpperBound

type tt_entry =
  { key : Int64.t
  ; score : int
  ; depth : int
  ; best_move : Move.t option
  ; entry_type : entry_type
  }

type t

val create : ?num_locks:int -> int -> t
val store : t -> Int64.t -> int -> int -> Move.t option -> entry_type -> unit
val lookup : t -> Int64.t -> tt_entry option
val clear : t -> unit
val stats : t -> string
