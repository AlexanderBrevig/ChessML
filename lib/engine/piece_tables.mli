(** Piece-Square Tables and Value Functions *)

open Chessml_core

(** Get piece-square table bonus for a piece at a square *)
val piece_square_value : Types.piece -> Square.t -> int

(** Get total value of a piece on a square: material + positional bonus *)
val piece_total_value : Types.piece -> Square.t -> int

(** Get total value for a piece kind on a square (requires color for PST lookup) *)
val piece_kind_total_value : Types.piece_kind -> Types.color -> Square.t -> int
