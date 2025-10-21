(** Position evaluation *)

open Chessml_core

(** Evaluate a position from the perspective of the side to move.
    Positive scores favor the side to move, negative scores favor the opponent.
    Returns score in centipawns (100 = 1 pawn advantage).
    Optional history parameter allows repetition-aware evaluation. *)
val evaluate : ?history:int64 list -> Position.t -> int

(** Count repetitions of a position key in the game history.
    Returns number of times this position has appeared. *)
val count_repetitions : int64 -> int64 list -> int

(** Count material for a given color in centipawns *)
val count_material : Position.t -> Types.color -> int

(** Get the material value of a piece in centipawns *)
val piece_value : Types.piece -> int

(** Get piece value by kind only *)
val piece_kind_value : Types.piece_kind -> int

(** Get piece-square table bonus for a piece at a specific square *)
val piece_square_value : Types.piece -> Square.t -> int

(** Get total value of a piece on a square: material + positional bonus *)
val piece_total_value : Types.piece -> Square.t -> int

(** Get total value for a piece kind on a square (requires color for PST lookup) *)
val piece_kind_total_value : Types.piece_kind -> Types.color -> Square.t -> int

(** Check if a piece on a square is hanging (attacked and undefended, or bad trade).
    Returns true if the piece can be captured with material gain. *)
val is_piece_hanging : Position.t -> Square.t -> bool

(** Check if a piece is en prise (can be captured immediately with material gain).
    Similar to is_piece_hanging but focuses on immediate captures. *)
val is_piece_en_prise : Position.t -> Square.t -> bool

(** Evaluate threats to a specific piece on a square.
    Returns negative value if piece is under threat (more negative = worse).
    Returns 0 if piece is safe. *)
val evaluate_piece_threats : Position.t -> Square.t -> int
