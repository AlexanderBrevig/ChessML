(** Pawn_cache - Cache for pawn structure evaluations
    
    Caches pawn structure evaluations using a hash computed from pawn bitboards.
    This avoids expensive re-evaluation when pawns haven't moved (which is frequent).
    Uses a simple hash table with replacement policy.
*)

type cache_entry =
  { pawn_hash : Int64.t
  ; score : int
  }

type t =
  { mutable table : cache_entry option array
  ; mutable hits : int
  ; mutable misses : int
  }

(** Compute pawn structure hash from pawn bitboards *)
let compute_pawn_hash white_pawns black_pawns =
  (* XOR the pawn bitboards together to create a simple hash *)
  Int64.logxor white_pawns black_pawns
;;

(** Create a new pawn structure cache *)
let create size =
  { table = Array.make size None
  ; hits = 0
  ; misses = 0
  }
;;

(** Default cache size (must be power of 2 for fast modulo) *)
let default_size = 65536 (* 64K entries, ~1MB memory *)

(** Create default cache *)
let create_default () = create default_size

(** Get index in table from hash *)
let get_index pawn_hash table_size =
  Int64.(to_int (logand pawn_hash (of_int (table_size - 1))))
;;

(** Probe cache for a pawn structure *)
let probe cache pawn_hash =
  let idx = get_index pawn_hash (Array.length cache.table) in
  match cache.table.(idx) with
  | Some entry when entry.pawn_hash = pawn_hash ->
    cache.hits <- cache.hits + 1;
    Some entry.score
  | _ ->
    cache.misses <- cache.misses + 1;
    None
;;

(** Store evaluation in cache *)
let store cache pawn_hash score =
  let idx = get_index pawn_hash (Array.length cache.table) in
  cache.table.(idx) <- Some { pawn_hash; score }
;;

(** Clear cache *)
let clear cache =
  Array.fill cache.table 0 (Array.length cache.table) None;
  cache.hits <- 0;
  cache.misses <- 0
;;

(** Get cache statistics *)
let stats cache =
  let total = cache.hits + cache.misses in
  let hit_rate = if total > 0 then float_of_int cache.hits /. float_of_int total else 0.0 in
  (cache.hits, cache.misses, hit_rate)
;;

(** Global pawn cache *)
let global_cache = ref (create_default ())

(** Get global cache *)
let get_global () = !global_cache

(** Clear global cache *)
let clear_global () = clear !global_cache

(** Get global cache statistics *)
let global_stats () = stats !global_cache
