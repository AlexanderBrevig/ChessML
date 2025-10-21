(** Static Exchange Evaluation (SEE)
    
    SEE evaluates the material outcome of a capture sequence on a square.
    It simulates all possible captures and recaptures to determine if a
    capture is winning, losing, or equal.
    
    This is crucial for:
    - Pruning bad captures in search
    - Better move ordering (winning captures first)
    - Tactical evaluation
*)

open Chessml_core

(** Evaluate static exchange for a move.
    
    Returns the expected material outcome (in centipawns) from the 
    perspective of the side making the initial capture.
    
    Positive values mean the capture wins material.
    Negative values mean the capture loses material.
    Zero means an equal trade.
    
    @param pos Current position
    @param move Move to evaluate (typically a capture)
    @return Material evaluation in centipawns
    
    Example:
    - Pawn takes undefended queen: +900cp (win queen)
    - Queen takes pawn defended by pawn: -800cp (win pawn, lose queen)
    - Knight takes bishop defended by knight: 0cp (equal trade)
*)
val evaluate : Position.t -> Move.t -> int

(** Check if a capture wins or breaks even on material (SEE >= 0).
    
    @param pos Current position
    @param move Move to evaluate
    @return true if capture wins or equals material, false if it loses
*)
val is_winning_capture : Position.t -> Move.t -> bool

(** Check if a capture loses material (SEE < 0).
    
    @param pos Current position  
    @param move Move to evaluate
    @return true if capture loses material, false otherwise
*)
val is_losing_capture : Position.t -> Move.t -> bool
