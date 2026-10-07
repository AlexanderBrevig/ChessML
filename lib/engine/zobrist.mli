(** Position hashing with the Polyglot key scheme, shared by the transposition
    table, repetition detection and the opening book *)

open Chessml_core
open Types

type t = Int64.t

(** Compute the Polyglot key of a position *)
val compute : Position.t -> t

(** Key for a piece standing on a square *)
val piece_key : piece -> Square.t -> t

(** Key for one castling right *)
val castling_key : color:color -> short:bool -> t

(** Key for an en passant capture being possible on a file (0-7) *)
val ep_file_key : int -> t

(** Key XORed in when White is to move *)
val white_to_move_key : t
