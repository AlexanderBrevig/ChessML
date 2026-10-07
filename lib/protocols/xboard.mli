(** XBoard/WinBoard protocol (CECP v2) implementation *)

open Chessml_engine

(** A protocol session: game, clocks, engine side and output *)
type session

(** Create a session writing lines with [send] (default: stdout) *)
val create_session : ?send:(string -> unit) -> ?book:Opening_book.book -> unit -> session

(** Handle one input line; the engine replies synchronously when it is to move *)
val handle_line : session -> string -> [ `Continue | `Quit ]

(** Read commands from stdin until "quit" or end of input *)
val main_loop : unit -> unit
