(** PGN Parser - Parse Portable Game Notation chess files *)

open Chessml_core

(** A parsed game with metadata and moves *)
type game =
  { event : string option
  ; site : string option
  ; date : string option
  ; round : string option
  ; white : string option
  ; black : string option
  ; result : string option
  ; moves : string list
  }

(** Parse a SAN move string to a Move.t given a position *)
val parse_san_move : Position.t -> string -> Move.t option

(** Parse multiple games from a PGN file *)
val parse_file : string -> game list

(** Convert a game's SAN moves to actual Move.t list *)
val game_to_moves : game -> Move.t list

(** Get statistics about a PGN file *)
val file_stats : string -> unit
