(** Polyglot opening book binary format *)

open Chessml_core

(** Polyglot book entry (16 bytes) *)
type entry =
  { key : Int64.t (** 8 bytes: zobrist hash *)
  ; move : int (** 2 bytes: encoded move *)
  ; weight : int (** 2 bytes: move weight/popularity *)
  ; learn : int (** 4 bytes: learning data *)
  }

(** {1 Move Encoding/Decoding} *)

(** Encode a move in Polyglot format.
    @param from_sq Source square (0-63)
    @param to_sq Destination square (0-63)
    @param promotion Promotion piece (0=none, 1=knight, 2=bishop, 3=rook, 4=queen)
    @return 16-bit encoded move *)
val encode_move : int -> int -> int -> int

(** Decode a Polyglot move encoding to a Move.t.
    @param pos Current position (needed to determine move kind)
    @param encoded 16-bit encoded move
    @return Some move if valid, None if invalid *)
val decode_move : Position.t -> int -> Move.t option

(** {1 Entry I/O} *)

(** Read a single 16-byte entry from a channel.
    @param ch Input channel positioned at entry start
    @return Some entry if successful, None on EOF or error *)
val read_entry : in_channel -> entry option

(** Write a single 16-byte entry to a channel.
    @param ch Output channel
    @param entry Entry to write *)
val write_entry : out_channel -> entry -> unit

(** Create an entry from components.
    @param key Zobrist hash
    @param from_sq Source square
    @param to_sq Destination square
    @param weight Move weight
    @return entry *)
val make_entry : Int64.t -> int -> int -> int -> entry
