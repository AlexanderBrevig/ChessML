(** Common search logic shared between single-threaded and parallel search *)

open Chessml_core

(** Futility pruning margins by depth *)
val futility_margin : int -> int

(** Check if futility pruning can be applied *)
val can_futility_prune : int -> int -> int -> bool

(** Check if a move should be pruned by futility pruning *)
val should_prune_move : bool -> Move.t -> gives_check:bool -> bool

(** Razoring: Return quiescence if eval + margin < alpha *)
module Razoring : sig
  (** Razoring margins by depth *)
  val margin : int -> int

  (** Check if we can try razoring *)
  val can_try_razor : Position.t -> int -> int -> bool -> bool

  (** Try razoring: if eval + margin < alpha, return true to signal drop to qsearch *)
  val should_razor : int -> int -> int -> bool
end

(** Reverse Futility Pruning (Static Null Move): Prune if eval - margin > beta *)
module ReverseFutility : sig
  (** Reverse futility margins by depth *)
  val margin : int -> int

  (** Check if we can prune the node (return beta) *)
  val can_prune : Position.t -> int -> int -> int -> bool -> bool
end

(** Late Move Pruning: Skip late quiet moves at low depths *)
module LateMove : sig
  (** Number of moves to search before pruning *)
  val move_count_threshold : int -> bool -> int

  (** Should we prune this late quiet move? *)
  val should_prune : int -> int -> bool -> bool -> bool -> bool
end

(** Late Move Reduction parameters *)
module LMR : sig
  (** Pre-computed logarithmic reduction table *)
  val reduction_table : int array array

  (** Check if a move can be reduced *)
  val can_reduce : int -> int -> bool -> int -> bool

  (** Calculate reduction amount with logarithmic formula and adjustments 
      @param depth Current search depth
      @param move_count Number of moves searched so far (1-indexed)
      @param is_pv Whether this is a PV node
      @param improving Whether position is improving
      @param gives_check Whether move gives check
      @param history_score History heuristic score for this move
      @return Reduction amount (plies to reduce) *)
  val reduction
    :  int
    -> int
    -> is_pv:bool
    -> improving:bool
    -> gives_check:bool
    -> history_score:int
    -> int

  (** Simple reduction for compatibility (uses default parameters) *)
  val reduction_simple : int -> int -> int

  (** Determine if a move is tactical (should not be reduced) *)
  val is_tactical_move : Move.t -> bool
end

(** Move ordering scores *)
module Ordering : sig
  (** Base scores for different move types *)
  val tt_move_score : int

  val castle_score : int
  val check_score : int
  val winning_capture_base : int
  val equal_capture_base : int
  val killer_score : int
  val quiet_base : int

  (** Fast check detection without making the move *)
  val gives_check_fast : Position.t -> Move.t -> bool

  (** Check if a move gives check (simplified for ordering) *)
  val gives_check_simple : Position.t -> Move.t -> bool

  (** Score a move for ordering (higher is better) *)
  val score_move
    :  Position.t
    -> Move.t
    -> is_tt_move:bool
    -> is_killer:bool
    -> is_countermove:bool
    -> int

  (** Order moves by score (highest first) *)
  val order_moves
    :  Position.t
    -> Move.t list
    -> tt_move:Move.t option
    -> killer_check:(Move.t -> bool)
    -> countermove_check:(Move.t -> bool)
    -> Move.t list
end

(** Terminal position detection *)
module Terminal : sig
  (** Check if position is checkmate or stalemate, returns Some score or None *)
  val check : Position.t -> Move.t list -> int option
end

(** Quiescence search helpers *)
module Quiescence : sig
  (** Maximum quiescence search depth *)
  val max_depth : int

  (** Generate tactical moves for quiescence search *)
  val generate_tactical_moves : Position.t -> Move.t list

  (** Check if quiescence should prune a capture based on SEE *)
  val should_prune_capture : Position.t -> Move.t -> bool
end

(** Safe negation for alpha-beta bounds to avoid overflow *)
val safe_negate : int -> int

(** Check if the current side to move is in check *)
val is_in_check : Position.t -> bool

(** Increment int64 reference *)
val incr_int64 : int64 ref -> unit

(** Alpha-beta search constants and helpers *)
module AlphaBeta : sig
  (** Initial alpha/beta bounds *)
  val initial_alpha : int

  val initial_beta : int

  (** Mate score detection threshold *)
  val mate_threshold : int

  (** Update alpha and best move based on search score *)
  val update_alpha_and_best : int -> int -> Move.t option -> Move.t -> int * Move.t option

  (** Check if score triggers beta cutoff *)
  val is_beta_cutoff : int -> int -> bool

  (** Handle beta cutoff: store killers and history for quiet moves *)
  val handle_beta_cutoff
    :  Move.t
    -> int
    -> (int -> Move.t -> unit)
    -> (Move.t -> int -> unit)
    -> unit
end

(** Null Move Pruning parameters *)
module NullMove : sig
  (** Reduction amount based on depth *)
  val reduction : int -> int

  (** Check if position has non-pawn material to avoid zugzwang *)
  val has_non_pawn_material : Position.t -> bool

  (** Check if null move pruning can be applied *)
  val can_prune : Position.t -> int -> int -> bool -> bool

  (** Make a null move (pass turn to opponent) *)
  val make_null_move : Position.t -> Position.t

  (** Calculate search depth for null move *)
  val search_depth : int -> int
end
