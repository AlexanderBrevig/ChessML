(** Position evaluation *)

open Chessml_core

(** Evaluate a position from the perspective of the side to move.
    Positive scores favor the side to move, negative scores favor the opponent.
    Returns score in centipawns (100 = 1 pawn advantage); 0 for dead positions.
    Repetitions are handled by the search, not here. *)
val evaluate : Position.t -> int

(** Get the material value of a piece in centipawns *)
val piece_value : Types.piece -> int

(** Get piece value by kind only *)
val piece_kind_value : Types.piece_kind -> int

(** Check if a piece on a square is hanging (attacked and undefended, or bad trade).
    Returns true if the piece can be captured with material gain. *)
val is_piece_hanging : Position.t -> Square.t -> bool

(** Evaluate 50-move rule incentive/penalty.
    Returns negative penalty when approaching 50-move draw while winning.
    Scaled by material advantage - larger advantage = larger penalty to encourage progress.
    Takes position and material difference in centipawns. *)
val evaluate_fifty_move_incentive : Position.t -> int -> int
