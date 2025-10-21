(** Magic Bitboards for fast sliding piece attack generation *)

(** Initialize magic bitboards (must be called before use) *)
val init : unit -> unit

(** Fast rook attack lookup
    @param sq Square index (0-63)
    @param blockers Bitboard of occupied squares
    @return Bitboard of attacked squares
*)
val rook_attacks : int -> Int64.t -> Int64.t

(** Fast bishop attack lookup
    @param sq Square index (0-63)
    @param blockers Bitboard of occupied squares
    @return Bitboard of attacked squares
*)
val bishop_attacks : int -> Int64.t -> Int64.t

(** Queen attack lookup (combination of rook and bishop)
    @param sq Square index (0-63)
    @param blockers Bitboard of occupied squares
    @return Bitboard of attacked squares
*)
val queen_attacks : int -> Int64.t -> Int64.t
