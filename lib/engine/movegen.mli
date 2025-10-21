(** Move generation interface *)

open Chessml_core
open Types

(** Magic bitboard entry for sliding piece move generation *)
type magic_entry =
  { mask : Int64.t
  ; magic : Int64.t
  ; shift : int
  ; offset : int
  }

(** Get knight attacks for a square *)
val knight_attacks : Square.t -> Bitboard.t

(** Get king attacks for a square *)
val king_attacks : Square.t -> Bitboard.t

(** Get pawn attacks for a square and color *)
val pawn_attacks : Square.t -> color -> Bitboard.t

(** Get pawn pushes for a square and color *)
val pawn_pushes : Square.t -> color -> Bitboard.t

(** Get rook attacks with blockers *)
val rook_attacks : Square.t -> Bitboard.t -> Bitboard.t

(** Get bishop attacks with blockers *)
val bishop_attacks : Square.t -> Bitboard.t -> Bitboard.t

(** Get queen attacks with blockers *)
val queen_attacks : Square.t -> Bitboard.t -> Bitboard.t

(** Get attacks for any piece type *)
val attacks_for : piece -> Square.t -> Bitboard.t -> Bitboard.t

(** Compute all squares attacked by a color *)
val compute_attacks_by : Position.t -> color -> Bitboard.t

(** Compute which pieces of a color attack a given square *)
val compute_attackers_to : Position.t -> Square.t -> color -> Bitboard.t

(** Compute occupied squares *)
val compute_occupied : Position.t -> Bitboard.t

(** Find king square for a color *)
val find_king : Position.t -> color -> Square.t option

(** Compute ray between two squares (exclusive) *)
val ray_between : Square.t -> Square.t -> Bitboard.t

(** Compute checkers, pinned pieces, and checkmask *)
val compute_legal_masks : Position.t -> color -> Bitboard.t * Bitboard.t * Bitboard.t

(** Generate all legal moves for the current position *)
val generate_moves : Position.t -> Move.t list

(** Generate moves only from squares in the given bitboard mask *)
val generate_moves_from : Position.t -> Bitboard.t -> Move.t list
