(** History heuristic for move ordering: [from][to] scores of quiet moves that
    caused beta cutoffs *)

open Chessml_core

type t

(** Scores are capped at this value *)
val max_score : int

val create : unit -> t
val clear : t -> unit

(** Record a quiet move that caused a beta cutoff at the given depth *)
val record_cutoff : t -> Move.t -> int -> unit

val get_score : t -> Move.t -> int

(** Halve all scores (between searches) *)
val age : t -> unit

(** (non-zero entries, max score, average non-zero score) *)
val stats : t -> int * int * float
