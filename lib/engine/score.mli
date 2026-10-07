(** Search score conventions: centipawns from the side to move's perspective, with
    mate scores encoding distance to mate *)

(** Larger than any score; initial alpha-beta window is [-infinity, infinity] *)
val infinity : int

(** Score of delivering mate on the board (at ply 0) *)
val mate : int

val draw : int
val max_ply : int

(** Scores with absolute value at least this are mate scores *)
val mate_bound : int

val is_mate : int -> bool

(** Score for the side to move being checkmated at [ply] from the root *)
val mated_in : int -> int

(** Root-relative to node-relative mate score, for storing in the TT at [ply] *)
val to_tt : int -> int -> int

(** Node-relative TT score back to root-relative at [ply] *)
val of_tt : int -> int -> int

(** Moves to mate: positive if the side to move mates, negative if it is mated *)
val mate_in_moves : int -> int

(** UCI score field, "cp N" or "mate N" *)
val to_uci : int -> string
