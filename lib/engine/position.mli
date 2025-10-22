(** Position module interface *)

open Chessml_core

type castling_rights =
  { short : Square.t option
  ; long : Square.t option
  }

type board = Types.piece option array
type t

val fen_startpos : string
val empty_board : unit -> board
val piece_at : t -> Square.t -> Types.piece option
val set_piece : Square.t -> Types.piece -> t -> t
val clear_square : Square.t -> t -> t
val side_to_move : t -> Types.color
val ep_square : t -> Square.t option
val halfmove : t -> int
val fullmove : t -> int
val board : t -> board
val castling_rights : t -> castling_rights array
val white_king_sq : t -> Square.t
val black_king_sq : t -> Square.t
val occupied : t -> Bitboard.t
val key : t -> Int64.t

(** Piece bitboard accessors *)
val get_pieces : t -> Types.color -> Types.piece_kind -> Bitboard.t

val get_color_pieces : t -> Types.color -> Bitboard.t
val count_non_pawn_material : t -> Types.color -> int
val make_move : t -> Move.t -> t
val make_null_move : t -> t
val of_fen : string -> t
val default : unit -> t
val to_fen : t -> string

(** Draw ASCII board representation of the position *)
val draw_board : t -> unit
