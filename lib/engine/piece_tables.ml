(** Piece-Square Tables and Value Functions
    
    Provides piece values and positional bonuses for evaluation.
    This module is used by both Eval and See to avoid circular dependencies.
*)

open Chessml_core
open Types

(** Piece-square tables for positional evaluation (from white's perspective) *)

let pawn_table =
  [| 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 50
   ; 50
   ; 50
   ; 50
   ; 50
   ; 50
   ; 50
   ; 50
   ; 10
   ; 10
   ; 20
   ; 30
   ; 30
   ; 20
   ; 10
   ; 10
   ; 5
   ; 5
   ; 10
   ; 25
   ; 25
   ; 10
   ; 5
   ; 5
   ; 0
   ; 0
   ; 0
   ; 20
   ; 20
   ; 0
   ; 0
   ; 0
   ; 5
   ; -5
   ; -10
   ; 0
   ; 0
   ; -10
   ; -5
   ; 5
   ; 5
   ; 10
   ; 10
   ; -20
   ; -20
   ; 10
   ; 10
   ; 5
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
  |]
;;

let knight_table =
  [| -50
   ; -40
   ; -30
   ; -30
   ; -30
   ; -30
   ; -40
   ; -50
   ; -40
   ; -20
   ; 0
   ; 0
   ; 0
   ; 0
   ; -20
   ; -40
   ; -30
   ; 0
   ; 10
   ; 15
   ; 15
   ; 10
   ; 0
   ; -30
   ; -30
   ; 5
   ; 15
   ; 20
   ; 20
   ; 15
   ; 5
   ; -30
   ; -30
   ; 0
   ; 15
   ; 20
   ; 20
   ; 15
   ; 0
   ; -30
   ; -30
   ; 5
   ; 10
   ; 15
   ; 15
   ; 10
   ; 5
   ; -30
   ; -40
   ; -20
   ; 0
   ; 5
   ; 5
   ; 0
   ; -20
   ; -40
   ; -50
   ; -40
   ; -30
   ; -30
   ; -30
   ; -30
   ; -40
   ; -50
  |]
;;

let bishop_table =
  [| -20
   ; -10
   ; -10
   ; -10
   ; -10
   ; -10
   ; -10
   ; -20
   ; -10
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; -10
   ; -10
   ; 0
   ; 5
   ; 10
   ; 10
   ; 5
   ; 0
   ; -10
   ; -10
   ; 5
   ; 5
   ; 10
   ; 10
   ; 5
   ; 5
   ; -10
   ; -10
   ; 0
   ; 10
   ; 10
   ; 10
   ; 10
   ; 0
   ; -10
   ; -10
   ; 10
   ; 10
   ; 10
   ; 10
   ; 10
   ; 10
   ; -10
   ; -10
   ; 5
   ; 0
   ; 0
   ; 0
   ; 0
   ; 5
   ; -10
   ; -20
   ; -10
   ; -10
   ; -10
   ; -10
   ; -10
   ; -10
   ; -20
  |]
;;

let rook_table =
  [| 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 5
   ; 10
   ; 10
   ; 10
   ; 10
   ; 10
   ; 10
   ; 5
   ; -5
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; -5
   ; -5
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; -5
   ; -5
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; -5
   ; -5
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; -5
   ; -5
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; -5
   ; 0
   ; 0
   ; 0
   ; 5
   ; 5
   ; 0
   ; 0
   ; 0
  |]
;;

let queen_table =
  [| -20
   ; -10
   ; -10
   ; -5
   ; -5
   ; -10
   ; -10
   ; -20
   ; -10
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; -10
   ; -10
   ; 0
   ; 5
   ; 5
   ; 5
   ; 5
   ; 0
   ; -10
   ; -5
   ; 0
   ; 5
   ; 5
   ; 5
   ; 5
   ; 0
   ; -5
   ; 0
   ; 0
   ; 5
   ; 5
   ; 5
   ; 5
   ; 0
   ; -5
   ; -10
   ; 5
   ; 5
   ; 5
   ; 5
   ; 5
   ; 0
   ; -10
   ; -10
   ; 0
   ; 5
   ; 0
   ; 0
   ; 0
   ; 0
   ; -10
   ; -20
   ; -10
   ; -10
   ; -5
   ; -5
   ; -10
   ; -10
   ; -20
  |]
;;

let king_middlegame_table =
  [| -30
   ; -40
   ; -40
   ; -50
   ; -50
   ; -40
   ; -40
   ; -30
   ; -30
   ; -40
   ; -40
   ; -50
   ; -50
   ; -40
   ; -40
   ; -30
   ; -30
   ; -40
   ; -40
   ; -50
   ; -50
   ; -40
   ; -40
   ; -30
   ; -30
   ; -40
   ; -40
   ; -50
   ; -50
   ; -40
   ; -40
   ; -30
   ; -20
   ; -30
   ; -30
   ; -40
   ; -40
   ; -30
   ; -30
   ; -20
   ; -10
   ; -20
   ; -20
   ; -20
   ; -20
   ; -20
   ; -20
   ; -10
   ; 20
   ; 20
   ; 0
   ; 0
   ; 0
   ; 0
   ; 20
   ; 20
   ; 20
   ; 30
   ; 10
   ; 0
   ; 0
   ; 10
   ; 30
   ; 20
  |]
;;

(** Get piece-square table bonus for a piece at a square *)
let piece_square_value (piece : piece) (sq : Square.t) : int =
  (* Flip square for black pieces *)
  let table_sq = if piece.color = White then sq else sq lxor 56 in
  match piece.kind with
  | Pawn -> pawn_table.(table_sq)
  | Knight -> knight_table.(table_sq)
  | Bishop -> bishop_table.(table_sq)
  | Rook -> rook_table.(table_sq)
  | Queen -> queen_table.(table_sq)
  | King -> king_middlegame_table.(table_sq)
;;

(** Get total value of a piece on a square: material + positional bonus *)
let piece_total_value (piece : piece) (sq : Square.t) : int =
  PieceKind.value piece.kind + piece_square_value piece sq
;;

(** Get total value for a piece kind on a square (requires color for PST lookup) *)
let piece_kind_total_value (kind : piece_kind) (color : color) (sq : Square.t) : int =
  let piece = { kind; color } in
  piece_total_value piece sq
;;
