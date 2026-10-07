(** Pawn_cache - Cache for pawn structure evaluations

    Caches pawn structure scores keyed by the exact pair of pawn bitboards, so a
    probe can never return the score of a different pawn structure. Scores are
    stored from White's perspective (white minus black). Uses a direct-mapped
    table with always-replace policy.
*)

type cache_entry =
  { white_pawns : Int64.t
  ; black_pawns : Int64.t
  ; score : int
  }

type t =
  { table : cache_entry option array
  ; mutable hits : int
  ; mutable misses : int
  }

(** Create a new pawn structure cache; [size] is rounded up to a power of two *)
let create size =
  let rec pow2 n = if n >= size then n else pow2 (n * 2) in
  { table = Array.make (pow2 1) None; hits = 0; misses = 0 }
;;

(** Default cache size *)
let default_size = 65536

(** Mix both bitboards into a well-distributed table index *)
let index cache white_pawns black_pawns =
  let open Int64 in
  let h =
    mul (logxor white_pawns (mul black_pawns 0x9E3779B97F4A7C15L)) 0xBF58476D1CE4E5B9L
  in
  to_int (shift_right_logical h 32) land (Array.length cache.table - 1)
;;

(** Probe cache for a pawn structure *)
let probe cache ~white_pawns ~black_pawns =
  match cache.table.(index cache white_pawns black_pawns) with
  | Some entry when entry.white_pawns = white_pawns && entry.black_pawns = black_pawns ->
    cache.hits <- cache.hits + 1;
    Some entry.score
  | _ ->
    cache.misses <- cache.misses + 1;
    None
;;

(** Store evaluation in cache *)
let store cache ~white_pawns ~black_pawns score =
  cache.table.(index cache white_pawns black_pawns)
  <- Some { white_pawns; black_pawns; score }
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
  let hit_rate =
    if total > 0 then float_of_int cache.hits /. float_of_int total else 0.0
  in
  cache.hits, cache.misses, hit_rate
;;

(** Global pawn cache *)
let global_cache = create default_size

(** Get global cache *)
let get_global () = global_cache

(** Clear global cache *)
let clear_global () = clear global_cache
