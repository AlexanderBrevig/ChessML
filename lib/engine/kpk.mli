(** Exact win/draw table for king and pawn against king, computed on first use *)

(** Does the side with the pawn win? Squares must be given with the pawn moving up
    the board (towards rank 8); flip the board first if the pawn is Black's. *)
val wins : strong_king:int -> weak_king:int -> pawn:int -> strong_to_move:bool -> bool
