(** PGN Parser - Parse Portable Game Notation chess files *)

open Chessml_core

(** A parsed game with metadata and SAN moves *)
type game =
  { event : string option
  ; site : string option
  ; date : string option
  ; round : string option
  ; white : string option
  ; black : string option
  ; result : string option
  ; fen : string option (** FEN tag for games not starting from the standard position *)
  ; moves : string list
  }

(** Resolve a SAN move to the legal move it denotes; [None] if illegal or ambiguous *)
val parse_san_move : Position.t -> string -> Move.t option

(** Parse all games in PGN text *)
val parse_string : string -> game list

(** Parse all games in a PGN file *)
val parse_file : string -> game list

(** Starting position of a game (its FEN tag, or the standard position) *)
val start_position : game -> Position.t

(** The game's moves from its start position, up to the first unresolvable one *)
val game_to_moves : game -> Move.t list

(** Print statistics about a PGN file *)
val file_stats : string -> unit
