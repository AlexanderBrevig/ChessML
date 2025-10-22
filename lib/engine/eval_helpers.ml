(** Eval_helpers - Low-level utility functions for evaluation
    
    This module provides basic helper functions used across the evaluation system:
    - Bitboard manipulation helpers
    - Square checking utilities
    - Pawn counting on files
*)

open Chessml_core
open Types

(** Helper: check if a square is empty using bitboards *)
let is_square_empty pos sq = not (Bitboard.contains (Position.occupied pos) sq)

(** Get pawn bitboard for a color *)
let get_pawns pos color = Position.get_pieces pos color Pawn

(** Check if there's a pawn of given color on a specific square *)
let has_pawn_on pos color sq = Bitboard.contains (get_pawns pos color) sq

(** Count pawns on a file for a color using bitboards *)
let count_pawns_on_file_bb pos file_int color =
  let pawns = get_pawns pos color in
  (* Create file mask by setting all squares on that file *)
  let file_mask = ref Bitboard.empty in
  for rank = 0 to 7 do
    file_mask := Bitboard.set !file_mask (file_int + (rank * 8))
  done;
  Bitboard.population (Int64.logand pawns !file_mask)
;;

(** Piece values in centipawns (by kind) - delegate to Types module *)
let piece_kind_value (kind : piece_kind) : int = PieceKind.value kind

(** Get piece value (convenience wrapper) *)
let piece_value (piece : piece) : int = piece_kind_value piece.kind

(** Get piece-square table value for a piece at a square *)
let piece_square_value (piece : piece) (sq : int) : int =
  Piece_tables.piece_square_value piece sq
;;

(** Get total value of a piece on a square: material + positional bonus 
    This gives the true value of the piece considering its position. *)
let piece_total_value (piece : piece) (sq : int) : int =
  Piece_tables.piece_total_value piece sq
;;

(** Get total value for a piece kind on a square (requires color for PST lookup) *)
let piece_kind_total_value (kind : piece_kind) (color : color) (sq : int) : int =
  Piece_tables.piece_kind_total_value kind color sq
;;

(** Count material for a given color *)
let count_material (pos : Position.t) (color : color) : int =
  let material = ref 0 in
  let pieces = Position.get_color_pieces pos color in
  Bitboard.iter
    (fun sq ->
       match Position.piece_at pos sq with
       | Some piece when piece.color = color ->
         material := !material + piece_kind_value piece.kind
       | _ -> ())
    pieces;
  !material
;;
