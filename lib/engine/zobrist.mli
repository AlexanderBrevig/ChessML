(** Zobrist hashing for position keys *)

open Chessml_core
open Types

type t = Int64.t

(** Compute zobrist from raw components (for Position.of_fen) *)
val compute_from_raw
  :  piece option array
  -> color
  -> Position.castling_rights array
  -> Square.t option
  -> t

(** Compute the Zobrist hash for a position *)
val compute : Position.t -> t

(** Hash/unhash a piece at a square *)
val hash_piece : t -> Square.t -> piece -> t

(** Hash/unhash an en passant square *)
val hash_ep_square : t -> Square.t -> t

(** Hash/unhash castling rights *)
val hash_castling_rights : t -> Position.castling_rights array -> t

(** Hash/unhash side to move *)
val hash_side_to_move : t -> color -> t
