(** Polyglot - Binary opening book file format parser
    
    Low-level parser for Polyglot opening book binary format. Handles move encoding/
    decoding, file I/O, and binary structure. Works with Opening_book module which
    provides high-level query interface. Supports big-endian format with 16-byte entries.
    
    Format: [8B: Zobrist key][2B: move][2B: weight][4B: learn data]
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

(** Encode move in Polyglot format: from_sq | (to_sq << 6) | (promo << 12) *)
let encode_move from_sq to_sq promotion = from_sq lor (to_sq lsl 6) lor (promotion lsl 12)

(** Decode polyglot move encoding to our Move.t *)
let decode_move (pos : Position.t) (encoded : int) : Move.t option =
  let from_sq = encoded land 0x3F in
  let to_sq = (encoded lsr 6) land 0x3F in
  let promo = (encoded lsr 12) land 0x7 in
  (* Polyglot promotion encoding: 1=Knight, 2=Bishop, 3=Rook, 4=Queen *)
  let move_kind =
    if promo > 0
    then (
      match promo with
      | 1 -> Some Move.PromoteKnight
      | 2 -> Some Move.PromoteBishop
      | 3 -> Some Move.PromoteRook
      | 4 -> Some Move.PromoteQueen
      | _ -> None)
    else (
      (* Determine move kind based on position *)
      match Position.piece_at pos from_sq with
      | None -> None
      | Some piece ->
        let target = Position.piece_at pos to_sq in
        (match piece.kind with
         | King ->
           (* Check for castling *)
           let file_diff = abs ((to_sq mod 8) - (from_sq mod 8)) in
           if file_diff > 1
           then if to_sq > from_sq then Some Move.ShortCastle else Some Move.LongCastle
           else if target <> None
           then Some Move.Capture
           else Some Move.Quiet
         | Pawn ->
           let from_rank = from_sq / 8 in
           let to_rank = to_sq / 8 in
           let rank_diff = abs (to_rank - from_rank) in
           (* Check for double push *)
           if rank_diff = 2
           then Some Move.PawnDoublePush (* Check for en passant *)
           else if target = None && from_sq mod 8 <> to_sq mod 8
           then Some Move.EnPassantCapture
           else if target <> None
           then Some Move.Capture
           else Some Move.Quiet
         | _ -> if target <> None then Some Move.Capture else Some Move.Quiet))
  in
  match move_kind with
  | Some kind -> Some (Move.make from_sq to_sq kind)
  | None -> None
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
  | _ -> None
;;

(** Write a single book entry to channel *)
let write_entry (ch : out_channel) (entry : entry) : unit =
  write_u64_be ch entry.key;
  write_u16_be ch entry.move;
  write_u16_be ch entry.weight;
  write_u32_be ch (Int32.of_int entry.learn)
;;

(** Create an entry from components *)
let make_entry key from_sq to_sq weight =
  let move = encode_move from_sq to_sq 0 in
  { key; move; weight; learn = 0 }
;;
