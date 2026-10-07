(** Piece-Square Tables and Value Functions *)

open Chessml_core

(** Get piece-square table bonus for a piece at a square *)
val piece_square_value : Types.piece -> Square.t -> int
