(** Types - Core chess type definitions and utilities
    
    Defines fundamental chess types: colors, pieces, files, and ranks.
    Provides conversion functions, validation, and pretty-printing for all types.
    Used throughout the engine for type-safe chess logic.
*)

(** Color represents the color of a player, piece, or square *)
type color =
  | White
  | Black

module Color = struct
  type t = color

  let all = [ White; Black ]

  let opponent = function
    | White -> Black
    | Black -> White
  ;;

  let to_int = function
    | White -> 0
    | Black -> 1
  ;;

  let of_int = function
    | 0 -> White
    | 1 -> Black
    | _ -> invalid_arg "Color.of_int: invalid value"
  ;;

  let to_string = function
    | White -> "w"
    | Black -> "b"
  ;;

  let of_string = function
    | "w" | "W" -> White
    | "b" | "B" -> Black
    | s -> invalid_arg ("Color.of_string: " ^ s)
  ;;

  let is_white = function
    | White -> true
    | _ -> false
  ;;

  let is_black = function
    | Black -> true
    | _ -> false
  ;;

  let negation_multiplier = function
    | White -> 1
    | Black -> -1
  ;;
end

(** File represents a column on the chessboard (a-h) *)
type file =
  | FileA
  | FileB
  | FileC
  | FileD
  | FileE
  | FileF
  | FileG
  | FileH

module File = struct
  type t = file

  let all = [ FileA; FileB; FileC; FileD; FileE; FileF; FileG; FileH ]

  let to_int = function
    | FileA -> 0
    | FileB -> 1
    | FileC -> 2
    | FileD -> 3
    | FileE -> 4
    | FileF -> 5
    | FileG -> 6
    | FileH -> 7
  ;;

  let of_int = function
    | 0 -> FileA
    | 1 -> FileB
    | 2 -> FileC
    | 3 -> FileD
    | 4 -> FileE
    | 5 -> FileF
    | 6 -> FileG
    | 7 -> FileH
    | _ -> invalid_arg "File.of_int: invalid value"
  ;;

  let to_char = function
    | FileA -> 'a'
    | FileB -> 'b'
    | FileC -> 'c'
    | FileD -> 'd'
    | FileE -> 'e'
    | FileF -> 'f'
    | FileG -> 'g'
    | FileH -> 'h'
  ;;

  let of_char = function
    | 'a' | 'A' -> FileA
    | 'b' | 'B' -> FileB
    | 'c' | 'C' -> FileC
    | 'd' | 'D' -> FileD
    | 'e' | 'E' -> FileE
    | 'f' | 'F' -> FileF
    | 'g' | 'G' -> FileG
    | 'h' | 'H' -> FileH
    | c -> invalid_arg ("File.of_char: " ^ String.make 1 c)
  ;;

  let to_string f = String.make 1 (to_char f)
end

(** Rank represents a row on the chessboard (1-8) *)
type rank =
  | Rank1
  | Rank2
  | Rank3
  | Rank4
  | Rank5
  | Rank6
  | Rank7
  | Rank8

module Rank = struct
  type t = rank

  let all = [ Rank1; Rank2; Rank3; Rank4; Rank5; Rank6; Rank7; Rank8 ]

  let to_int = function
    | Rank1 -> 0
    | Rank2 -> 1
    | Rank3 -> 2
    | Rank4 -> 3
    | Rank5 -> 4
    | Rank6 -> 5
    | Rank7 -> 6
    | Rank8 -> 7
  ;;

  let of_int = function
    | 0 -> Rank1
    | 1 -> Rank2
    | 2 -> Rank3
    | 3 -> Rank4
    | 4 -> Rank5
    | 5 -> Rank6
    | 6 -> Rank7
    | 7 -> Rank8
    | _ -> invalid_arg "Rank.of_int: invalid value"
  ;;

  let to_char = function
    | Rank1 -> '1'
    | Rank2 -> '2'
    | Rank3 -> '3'
    | Rank4 -> '4'
    | Rank5 -> '5'
    | Rank6 -> '6'
    | Rank7 -> '7'
    | Rank8 -> '8'
  ;;

  let of_char = function
    | '1' -> Rank1
    | '2' -> Rank2
    | '3' -> Rank3
    | '4' -> Rank4
    | '5' -> Rank5
    | '6' -> Rank6
    | '7' -> Rank7
    | '8' -> Rank8
    | c -> invalid_arg ("Rank.of_char: " ^ String.make 1 c)
  ;;

  let to_string r = String.make 1 (to_char r)

  let first = function
    | White -> Rank1
    | Black -> Rank8
  ;;

  let second = function
    | White -> Rank2
    | Black -> Rank7
  ;;

  let seventh = function
    | White -> Rank7
    | Black -> Rank2
  ;;

  let eighth = function
    | White -> Rank8
    | Black -> Rank1
  ;;
end

(** Piece kind (Pawn, Knight, Bishop, Rook, Queen, King) *)
type piece_kind =
  | Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King

module PieceKind = struct
  type t = piece_kind

  let all = [ Pawn; Knight; Bishop; Rook; Queen; King ]

  let to_int = function
    | Pawn -> 0
    | Knight -> 1
    | Bishop -> 2
    | Rook -> 3
    | Queen -> 4
    | King -> 5
  ;;

  let of_int = function
    | 0 -> Pawn
    | 1 -> Knight
    | 2 -> Bishop
    | 3 -> Rook
    | 4 -> Queen
    | 5 -> King
    | _ -> invalid_arg "PieceKind.of_int: invalid value"
  ;;

  let to_char ?(uppercase = false) = function
    | Pawn -> if uppercase then 'P' else 'p'
    | Knight -> if uppercase then 'N' else 'n'
    | Bishop -> if uppercase then 'B' else 'b'
    | Rook -> if uppercase then 'R' else 'r'
    | Queen -> if uppercase then 'Q' else 'q'
    | King -> if uppercase then 'K' else 'k'
  ;;

  let of_char = function
    | 'p' | 'P' -> Pawn
    | 'n' | 'N' -> Knight
    | 'b' | 'B' -> Bishop
    | 'r' | 'R' -> Rook
    | 'q' | 'Q' -> Queen
    | 'k' | 'K' -> King
    | c -> invalid_arg ("PieceKind.of_char: " ^ String.make 1 c)
  ;;

  let to_string kind = String.make 1 (to_char kind)

  (** Piece values in centipawns (100 = one pawn) *)
  let value = function
    | Pawn -> 100
    | Knight -> 320
    | Bishop -> 330
    | Rook -> 500
    | Queen -> 900
    | King -> 20000 (* King is invaluable but give it a high value for MVV-LVA *)
  ;;
end

(** A chess piece with color and kind *)
type piece =
  { color : color
  ; kind : piece_kind
  }

module Piece = struct
  type t = piece

  let make color kind = { color; kind }
  let white_pawn = { color = White; kind = Pawn }
  let white_knight = { color = White; kind = Knight }
  let white_bishop = { color = White; kind = Bishop }
  let white_rook = { color = White; kind = Rook }
  let white_queen = { color = White; kind = Queen }
  let white_king = { color = White; kind = King }
  let black_pawn = { color = Black; kind = Pawn }
  let black_knight = { color = Black; kind = Knight }
  let black_bishop = { color = Black; kind = Bishop }
  let black_rook = { color = Black; kind = Rook }
  let black_queen = { color = Black; kind = Queen }
  let black_king = { color = Black; kind = King }
  let color p = p.color
  let kind p = p.kind
  let is_white p = Color.is_white p.color
  let is_black p = Color.is_black p.color
  let is_pawn p = p.kind = Pawn
  let is_knight p = p.kind = Knight
  let is_bishop p = p.kind = Bishop
  let is_rook p = p.kind = Rook
  let is_queen p = p.kind = Queen
  let is_king p = p.kind = King

  let to_char piece =
    let c = PieceKind.to_char piece.kind in
    if Color.is_white piece.color then Char.uppercase_ascii c else c
  ;;

  let of_char c =
    let color = if Char.uppercase_ascii c = c then White else Black in
    let kind = PieceKind.of_char c in
    { color; kind }
  ;;

  let to_string piece = String.make 1 (to_char piece)
end
