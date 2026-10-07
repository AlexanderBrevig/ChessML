(** Pawn structure evaluation cache, keyed by the exact pawn bitboards *)

type t

(** Create a new pawn cache; size is rounded up to a power of two *)
val create : int -> t

(** Probe cache for a pawn structure. Scores are white minus black. *)
val probe : t -> white_pawns:Int64.t -> black_pawns:Int64.t -> int option

(** Store pawn structure score (white minus black) *)
val store : t -> white_pawns:Int64.t -> black_pawns:Int64.t -> int -> unit

(** Clear cache *)
val clear : t -> unit

(** Get cache statistics (hits, misses, hit_rate) *)
val stats : t -> int * int * float

(** Get global pawn cache *)
val get_global : unit -> t

(** Clear global pawn cache *)
val clear_global : unit -> unit
