(** Piece-Square Tables and Value Functions
    
    Provides piece values and positional bonuses for evaluation.
    This module is used by both Eval and See to avoid circular dependencies.
*)

open Chessml_core
open Types

(** Piece-square tables for positional evaluation, from White's perspective and laid
    out as printed: index 0 is a8, index 63 is h1 (square index [sq lxor 56]). *)

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
  (* Tables are a8-first, squares are a1-first: flip for White, Black reads it mirrored *)
  let table_sq = if piece.color = White then sq lxor 56 else sq in
  match piece.kind with
  | Pawn -> pawn_table.(table_sq)
  | Knight -> knight_table.(table_sq)
  | Bishop -> bishop_table.(table_sq)
  | Rook -> rook_table.(table_sq)
  | Queen -> queen_table.(table_sq)
  | King -> king_middlegame_table.(table_sq)
;;
