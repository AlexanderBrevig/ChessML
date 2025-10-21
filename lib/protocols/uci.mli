(** UCI (Universal Chess Interface) protocol implementation *)

open Chessml_engine

(** Parse a list of moves in UCI format and apply them to a game *)
val apply_moves : Game.t -> string list -> Game.t

(** Parse and execute a UCI position command *)
val parse_position : string list -> Game.t -> Game.t

(** Parse go command parameters *)
type search_params =
  { depth : int option
  ; movetime : int option (* milliseconds *)
  ; wtime : int option (* white time in ms *)
  ; btime : int option (* black time in ms *)
  ; winc : int option (* white increment *)
  ; binc : int option (* black increment *)
  ; movestogo : int option
  }

val parse_go_params : string list -> search_params

(** Main UCI loop - reads commands from stdin and responds on stdout *)
val main_loop : unit -> unit
