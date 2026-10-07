(** Zobrist - Position hash keys in the Polyglot scheme

    The keys are those of the Polyglot opening book format, so a position's hash
    serves transposition tables, repetition detection and book lookups alike.
    Position maintains its key incrementally from these tables. As in Polyglot,
    the en passant file is only hashed when a pawn of the side to move can
    actually capture en passant.

    Reference: http://hgm.nubati.net/book_format.html
*)

open Chessml_core
open Types

type t = Int64.t

let random64 = Polyglot_random.random64

(** Polyglot piece kind: black pawn = 0, white pawn = 1, black knight = 2, ... *)
let polyglot_piece_kind piece =
  let kind =
    match piece.kind with
    | Pawn -> 0
    | Knight -> 1
    | Bishop -> 2
    | Rook -> 3
    | Queen -> 4
    | King -> 5
  in
  (2 * kind) + if piece.color = White then 1 else 0
;;

let piece_key piece sq = random64.((64 * polyglot_piece_kind piece) + sq)

let castling_key ~color ~short =
  random64.(768 + (if color = White then 0 else 2) + if short then 0 else 1)
;;

let ep_file_key file = random64.(772 + file)
let white_to_move_key = random64.(780)
