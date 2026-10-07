(** UCI (Universal Chess Interface) protocol implementation *)

open Chessml_engine

(** Parameters of a "go" command *)
type search_params =
  { depth : int option
  ; movetime : int option (** milliseconds *)
  ; wtime : int option (** white's remaining time in ms *)
  ; btime : int option (** black's remaining time in ms *)
  ; winc : int option (** white's increment in ms *)
  ; binc : int option (** black's increment in ms *)
  ; movestogo : int option
  ; infinite : bool
  }

(** A protocol session: current game, book and output *)
type session

(** Create a session writing lines with [send] (default: stdout) *)
val create_session : ?send:(string -> unit) -> ?book:Opening_book.book -> unit -> session

(** Apply moves in UCI notation, resolved against the legal moves.
    @raise Failure on an illegal move *)
val apply_moves : Game.t -> string list -> Game.t

(** Parse the arguments of a "position" command.
    @raise Failure on malformed input *)
val parse_position : string list -> Game.t

(** Parse the arguments of a "go" command *)
val parse_go_params : string list -> search_params

(** Time limit in ms for the side to move, [None] for an unlimited search *)
val time_limit_ms : search_params -> Chessml_core.Types.color -> int option

(** Handle one input line; errors are reported as "info string" lines *)
val handle_line : session -> string -> [ `Continue | `Quit ]

(** Wait for the running search (if any) to finish *)
val wait : session -> unit

(** Read commands from stdin until "quit" or end of input *)
val main_loop : unit -> unit
