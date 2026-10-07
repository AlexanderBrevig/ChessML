(** Polyglot - Binary opening book file format parser
    
    Low-level parser for Polyglot opening book binary format. Handles move encoding/
    decoding, file I/O, and binary structure. Works with Opening_book module which
    provides high-level query interface. Supports big-endian format with 16-byte entries.
    
    Format: [8B: Polyglot key][2B: move][2B: weight][4B: learn data], sorted by
    key as an unsigned integer.
*)

open Chessml_core
open Types

(** Polyglot book entry (16 bytes) *)
type entry =
  { key : Int64.t (* 8 bytes: zobrist hash *)
  ; move : int (* 2 bytes: encoded move *)
  ; weight : int (* 2 bytes: move weight/popularity *)
  ; learn : int (* 4 bytes: learning data *)
  }

(** {1 Move Encoding/Decoding} *)

(** Encode a move as in the Polyglot format: to square in bits 0-5, from square in
    bits 6-11, promotion piece (1=N, 2=B, 3=R, 4=Q) in bits 12-14. Castling is
    encoded as the king capturing its own rook (e1h1, e1a1, e8h8, e8a8). *)
let encode_move (mv : Move.t) : int =
  let from_sq = Move.from mv in
  let to_sq =
    match Move.kind mv with
    | Move.ShortCastle -> from_sq + 3
    | Move.LongCastle -> from_sq - 4
    | _ -> Move.to_square mv
  in
  let promo =
    match Move.promotion mv with
    | None -> 0
    | Some Knight -> 1
    | Some Bishop -> 2
    | Some Rook -> 3
    | Some Queen -> 4
    | Some (Pawn | King) -> 0
  in
  to_sq lor (from_sq lsl 6) lor (promo lsl 12)
;;

(** Decode a Polyglot move by matching it against the legal moves, so the result
    always carries the right kind and is never illegal *)
let decode_move (pos : Position.t) (encoded : int) : Move.t option =
  List.find_opt (fun mv -> encode_move mv = encoded) (Movegen.generate_moves pos)
;;

(** {1 Binary I/O Helpers} *)

(** Read 16-bit big-endian unsigned integer *)
let read_u16_be (ch : in_channel) : int =
  let b1 = input_byte ch in
  let b2 = input_byte ch in
  (b1 lsl 8) lor b2
;;

(** Read 32-bit big-endian unsigned integer as Int64 *)
let read_u32_be (ch : in_channel) : Int64.t =
  let w1 = read_u16_be ch in
  let w2 = read_u16_be ch in
  Int64.logor (Int64.shift_left (Int64.of_int w1) 16) (Int64.of_int w2)
;;

(** Read 64-bit big-endian unsigned integer *)
let read_u64_be (ch : in_channel) : Int64.t =
  let d1 = read_u32_be ch in
  let d2 = read_u32_be ch in
  Int64.logor (Int64.shift_left d1 32) d2
;;

(** Write 16-bit big-endian unsigned integer *)
let write_u16_be (ch : out_channel) (v : int) : unit =
  output_byte ch ((v lsr 8) land 0xFF);
  output_byte ch (v land 0xFF)
;;

(** Write 32-bit big-endian unsigned integer *)
let write_u32_be (ch : out_channel) (v : Int32.t) : unit =
  let hi = Int32.to_int (Int32.shift_right_logical v 16) in
  let lo = Int32.to_int (Int32.logand v 0xFFFFl) in
  write_u16_be ch hi;
  write_u16_be ch lo
;;

(** Write 64-bit big-endian unsigned integer *)
let write_u64_be (ch : out_channel) (v : Int64.t) : unit =
  for i = 0 to 7 do
    let byte = Int64.to_int (Int64.shift_right_logical v ((7 - i) * 8)) land 0xFF in
    output_byte ch byte
  done
;;

(** {1 Entry I/O} *)

(** Read a single book entry from channel *)
let read_entry (ch : in_channel) : entry option =
  try
    let key = read_u64_be ch in
    let move = read_u16_be ch in
    let weight = read_u16_be ch in
    let learn_w1 = read_u16_be ch in
    let learn_w2 = read_u16_be ch in
    let learn = (learn_w1 lsl 16) lor learn_w2 in
    Some { key; move; weight; learn }
  with
  | End_of_file -> None
;;

(** Write a single book entry to channel *)
let write_entry (ch : out_channel) (entry : entry) : unit =
  write_u64_be ch entry.key;
  write_u16_be ch entry.move;
  write_u16_be ch entry.weight;
  write_u32_be ch (Int32.of_int entry.learn)
;;

(** Create an entry for [move] in the position with key [key] *)
let make_entry key move weight = { key; move = encode_move move; weight; learn = 0 }
