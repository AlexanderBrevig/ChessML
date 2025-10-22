(** Pawn structure evaluation cache *)

type t

(** Compute pawn structure hash from pawn bitboards *)
val compute_pawn_hash : Int64.t -> Int64.t -> Int64.t

(** Create a new pawn cache with given size *)
val create : int -> t

(** Create default cache (64K entries) *)
val create_default : unit -> t

(** Probe cache for a pawn structure evaluation *)
val probe : t -> Int64.t -> int option

(** Store pawn structure evaluation in cache *)
val store : t -> Int64.t -> int -> unit

(** Clear cache *)
val clear : t -> unit

(** Get cache statistics (hits, misses, hit_rate) *)
val stats : t -> int * int * float

(** Get global pawn cache *)
val get_global : unit -> t

(** Clear global pawn cache *)
val clear_global : unit -> unit

(** Get global cache statistics *)
val global_stats : unit -> int * int * float
