(** Alpha-beta minimax search *)

open Chessml_core

(** Search result containing the best move and evaluation *)
type search_result =
  { best_move : Move.t option
  ; score : int
  ; nodes : int64 (* Number of nodes searched *)
  ; depth : int
  }

(** Find the best move for the current position using alpha-beta search.
    @param verbose Print search statistics to stderr (default: true)
    @param max_time_ms Maximum time to search in milliseconds (optional)
    @param game The current game state
    @param depth Maximum search depth (in plies/half-moves)
    @return Search result with best move and score *)
val find_best_move : ?verbose:bool -> ?max_time_ms:int -> Game.t -> int -> search_result

(** Alpha-beta search algorithm with transposition table.
    @param pos Current position
    @param depth Remaining depth to search
    @param alpha Alpha value (best score for maximizing player)
    @param beta Beta value (best score for minimizing player)
    @param nodes Reference to node counter for tracking search statistics
    @param prev_move Previous move (for countermove heuristic)
    @return (score, best_move_option) *)
val alphabeta
  :  Position.t
  -> int
  -> int
  -> int
  -> int64 ref
  -> Move.t option
  -> int * Move.t option

(** Quiescence search for captures and checks.
    @param pos Current position
    @param alpha Alpha value
    @param beta Beta value  
    @param nodes Reference to node counter
    @param depth Current quiescence depth (optional, default 0)
    @return (score, best_move_option) *)
val quiescence
  :  Position.t
  -> int
  -> int
  -> int64 ref
  -> ?depth:int
  -> unit
  -> int * Move.t option

(** Transposition table entry types *)
type tt_entry_type =
  | Exact
  | LowerBound
  | UpperBound

(** Transposition table entry *)
type tt_entry =
  { key : int64
  ; score : int
  ; depth : int
  ; best_move : Move.t option
  ; entry_type : tt_entry_type
  }

(** Transposition table operations *)
module TranspositionTable : sig
  type t

  (** Create a new transposition table with given size *)
  val create : int -> t

  (** Store an entry in the transposition table *)
  val store : t -> int64 -> int -> int -> Move.t option -> tt_entry_type -> unit

  (** Lookup an entry in the transposition table *)
  val lookup : t -> int64 -> tt_entry option

  (** Clear all entries from the transposition table *)
  val clear : t -> unit
end
