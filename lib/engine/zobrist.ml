(** Zobrist - Position hashing with the Polyglot key scheme

    Keys are computed exactly as in the Polyglot opening book format, so the same
    key serves transposition tables, repetition detection and book lookups. As in
    Polyglot, the en passant file is only hashed when a pawn of the side to move
    can actually capture en passant, so positions that differ only in an unusable
    en passant square hash equal (which repetition detection requires).

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

(** Key for [piece] standing on [sq] *)
let piece_key piece sq = random64.((64 * polyglot_piece_kind piece) + sq)

(** Castling keys: white short, white long, black short, black long *)
let castling_key ~color ~short =
  random64.(768 + (if color = White then 0 else 2) + if short then 0 else 1)
;;

(** Key for an en passant capture being possible on [file] *)
let ep_file_key file = random64.(772 + file)

(** Key XORed in when White is to move *)
let white_to_move_key = random64.(780)

let castling_rights_key (rights : Position.castling_rights array) =
  let key color =
    let r = rights.(Color.to_int color) in
    Int64.logxor
      (if Option.is_some r.short then castling_key ~color ~short:true else 0L)
      (if Option.is_some r.long then castling_key ~color ~short:false else 0L)
  in
  Int64.logxor (key White) (key Black)
;;

(** En passant key, only if a pawn of the side to move stands next to the pawn
    that just made a double push *)
let ep_key pos =
  match Position.ep_square pos with
  | None -> 0L
  | Some ep_sq ->
    let side = Position.side_to_move pos in
    let pushed_pawn_sq = if side = White then ep_sq - 8 else ep_sq + 8 in
    let file = ep_sq mod 8 in
    let our_pawns = Position.get_pieces pos side Pawn in
    let adjacent f =
      f >= 0 && f <= 7 && Bitboard.contains our_pawns (pushed_pawn_sq - file + f)
    in
    if adjacent (file - 1) || adjacent (file + 1) then ep_file_key file else 0L
;;

(** Compute the key of a position from scratch *)
let compute pos =
  let key = ref 0L in
  Array.iteri
    (fun sq piece_opt ->
       match piece_opt with
       | Some piece -> key := Int64.logxor !key (piece_key piece sq)
       | None -> ())
    (Position.board pos);
  let key = Int64.logxor !key (castling_rights_key (Position.castling_rights pos)) in
  let key = Int64.logxor key (ep_key pos) in
  if Position.side_to_move pos = White then Int64.logxor key white_to_move_key else key
;;
