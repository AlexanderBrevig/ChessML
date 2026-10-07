(** Legal move generation and attack detection *)

open Chessml_core
open Types

val knight_attacks : Square.t -> Bitboard.t
val king_attacks : Square.t -> Bitboard.t

(** Squares attacked by a pawn of the given color on a square *)
val pawn_attacks : Square.t -> color -> Bitboard.t

(** Slider attacks given the occupancy *)
val rook_attacks : Square.t -> Bitboard.t -> Bitboard.t

val bishop_attacks : Square.t -> Bitboard.t -> Bitboard.t
val queen_attacks : Square.t -> Bitboard.t -> Bitboard.t

(** Squares attacked by a piece on a square given the occupancy *)
val attacks_for : piece -> Square.t -> Bitboard.t -> Bitboard.t

(** [attackers_to pos sq by occupied]: pieces of color [by] attacking [sq], with
    sliders blocked by [occupied] (which may differ from the position's, e.g. for
    x-rays in SEE) *)
val attackers_to : Position.t -> Square.t -> color -> Bitboard.t -> Bitboard.t

(** Pieces of a color attacking a square in the current position *)
val compute_attackers_to : Position.t -> Square.t -> color -> Bitboard.t

val is_square_attacked : Position.t -> Square.t -> color -> bool
val king_square : Position.t -> color -> Square.t

(** Is the side to move in check? *)
val in_check : Position.t -> bool

(** All legal moves for the side to move *)
val generate_moves : Position.t -> Move.t list
