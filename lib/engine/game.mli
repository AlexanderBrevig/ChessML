(** Game module interface *)

open Chessml_core

type t

val make : Position.t -> t
val default : unit -> t
val of_fen : string -> t
val position : t -> Position.t
val history : t -> Zobrist.t list
val make_move : t -> Move.t -> t
val legal_moves : t -> Move.t list
val legal_moves_from : t -> Bitboard.t -> Move.t list
val to_fen : t -> string
val is_repetition : t -> bool
val is_threefold_repetition : t -> bool
val is_draw : t -> bool
