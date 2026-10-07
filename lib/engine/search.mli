(** Iterative deepening principal variation search *)

open Chessml_core

(** Transposition table entry types *)
type tt_entry_type =
  | Exact
  | LowerBound
  | UpperBound

(** Transposition table entry; mate scores are stored relative to the node *)
type tt_entry =
  { key : int64
  ; score : int
  ; depth : int
  ; best_move : Move.t option
  ; entry_type : tt_entry_type
  }

module TranspositionTable : sig
  type t

  (** Create a table with the given number of entries *)
  val create : int -> t

  (** [store tt key depth score best_move entry_type] *)
  val store : t -> int64 -> int -> int -> Move.t option -> tt_entry_type -> unit

  val lookup : t -> int64 -> tt_entry option
  val clear : t -> unit

  (** Replace the table with an empty one of the given number of entries *)
  val resize : t -> int -> unit
end

(** Search tables that persist between the moves of a game *)
type state =
  { tt : TranspositionTable.t
  ; killers : Killers.killer_table
  ; history : History.t
  ; countermoves : Countermoves.t
  }

val create_state : ?hash_mb:int -> unit -> state

(** State used when none is passed explicitly (protocols use this one) *)
val default_state : state

(** Clear all tables (start of a new game) *)
val new_game : ?state:state -> unit -> unit

(** Resize and clear the transposition table *)
val set_hash_size_mb : ?state:state -> int -> unit

(** Search result *)
type search_result =
  { best_move : Move.t option
  ; score : int (** side to move's perspective; see {!Score} for mates *)
  ; nodes : int64
  ; depth : int (** last completed iteration *)
  ; pv : Move.t list (** principal variation starting with [best_move] *)
  }

(** Ask a running search to stop as soon as possible (safe from another thread) *)
val request_stop : unit -> unit

(** Find the best move with iterative deepening up to [depth] plies (capped by
    [Config.get_max_search_depth]).
    @param verbose print per-iteration statistics to stderr (default true)
    @param max_time_ms hard time limit; no new iteration starts after half of it
    @param state tables to use (default {!default_state})
    @param on_iteration called with the result of every completed iteration *)
val find_best_move
  :  ?verbose:bool
  -> ?max_time_ms:int
  -> ?state:state
  -> ?on_iteration:(search_result -> unit)
  -> Game.t
  -> int
  -> search_result
