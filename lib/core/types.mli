(** Type signature for types module *)

type color =
  | White
  | Black

type file =
  | FileA
  | FileB
  | FileC
  | FileD
  | FileE
  | FileF
  | FileG
  | FileH

type rank =
  | Rank1
  | Rank2
  | Rank3
  | Rank4
  | Rank5
  | Rank6
  | Rank7
  | Rank8

type piece_kind =
  | Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King

type piece =
  { color : color
  ; kind : piece_kind
  }

module Color : sig
  type t = color

  val all : t list
  val opponent : t -> t
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string
  val of_string : string -> t
  val is_white : t -> bool
  val is_black : t -> bool
  val negation_multiplier : t -> int
end

module File : sig
  type t = file

  val all : t list
  val to_int : t -> int
  val of_int : int -> t
  val to_char : t -> char
  val of_char : char -> t
  val to_string : t -> string
end

module Rank : sig
  type t = rank

  val all : t list
  val to_int : t -> int
  val of_int : int -> t
  val to_char : t -> char
  val of_char : char -> t
  val to_string : t -> string
  val first : color -> t
  val second : color -> t
  val seventh : color -> t
  val eighth : color -> t
end

module PieceKind : sig
  type t = piece_kind

  val all : t list
  val to_int : t -> int
  val of_int : int -> t
  val to_char : ?uppercase:bool -> t -> char
  val of_char : char -> t
  val to_string : t -> string
end

module Piece : sig
  type t = piece

  val make : color -> piece_kind -> t
  val white_pawn : t
  val white_knight : t
  val white_bishop : t
  val white_rook : t
  val white_queen : t
  val white_king : t
  val black_pawn : t
  val black_knight : t
  val black_bishop : t
  val black_rook : t
  val black_queen : t
  val black_king : t
  val color : t -> color
  val kind : t -> piece_kind
  val is_white : t -> bool
  val is_black : t -> bool
  val is_pawn : t -> bool
  val is_knight : t -> bool
  val is_bishop : t -> bool
  val is_rook : t -> bool
  val is_queen : t -> bool
  val is_king : t -> bool
  val to_char : t -> char
  val of_char : char -> t
  val to_string : t -> string
end
