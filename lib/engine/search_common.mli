(** Pruning parameters and move ordering for the search *)

open Chessml_core

(** Futility pruning margin by depth *)
val futility_margin : int -> int

(** Reverse futility pruning margin by depth *)
val reverse_futility_margin : int -> int

(** Razoring margin by depth *)
val razor_margin : int -> int

(** Number of quiet moves searched before late move pruning, by depth *)
val late_move_count : int -> int

(** Late Move Reduction parameters *)
module LMR : sig
  (** Pre-computed logarithmic reduction table *)
  val reduction_table : int array array

  (** [reduction depth move_count ~is_pv ~history_score] in plies *)
  val reduction : int -> int -> is_pv:bool -> history_score:int -> int

  (** Captures and promotions are never reduced *)
  val is_tactical_move : Move.t -> bool
end

(** Move ordering *)
module Ordering : sig
  val tt_move_score : int
  val castle_score : int
  val check_score : int
  val promotion_base : int
  val winning_capture_base : int
  val equal_capture_base : int
  val killer_score : int
  val countermove_score : int
  val quiet_base : int

  (** Does the move give check? (exact) *)
  val gives_check : Position.t -> Move.t -> bool

  (** Most Valuable Victim - Least Valuable Attacker score *)
  val mvv_lva_score : Position.t -> Move.t -> int

  (** Score a move for ordering (higher is better) *)
  val score_move
    :  ?history_score:int
    -> Position.t
    -> Move.t
    -> is_tt_move:bool
    -> is_killer:bool
    -> is_countermove:bool
    -> int

  (** Order moves by score, highest first (stable) *)
  val order_moves
    :  ?history:History.t
    -> Position.t
    -> Move.t list
    -> tt_move:Move.t option
    -> killer_check:(Move.t -> bool)
    -> countermove_check:(Move.t -> bool)
    -> Move.t list
end

(** Captures with SEE >= 0 and promotions, plus quiet checks if requested *)
val tactical_moves : ?include_checks:bool -> Position.t -> Move.t list
