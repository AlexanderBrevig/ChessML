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

(** Encode a move in Polyglot format: to square in bits 0-5, from square in bits
    6-11, promotion (1=N, 2=B, 3=R, 4=Q) in bits 12-14; castling as king takes rook *)
val encode_move : Move.t -> int

(** Decode a Polyglot move to the matching legal move in [pos], [None] if the
    move is not legal there *)
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

(** Create an entry for a move in the position with the given key and weight *)
val make_entry : Int64.t -> Move.t -> int -> entry
