(** History heuristic for move ordering *)

open Chessml_core

(** History table type *)
type t

(** Create a new history table *)
val create : unit -> t

(** Get the global history table *)
val get_global : unit -> t

(** Clear the history table *)
val clear : unit -> unit

(** Record a move that caused a beta cutoff *)
val record_cutoff : Move.t -> int -> unit

(** Record a move that failed to cause a cutoff *)
val record_failure : Move.t -> int -> unit

(** Get the history score for a move from the global table *)
val get_score : Move.t -> int

(** Get the history score for a move from a specific table *)
val get_score_from_table : t -> Move.t -> int

(** Record a cutoff in a specific table *)
val record_cutoff_in_table : t -> Move.t -> int -> unit

(** Age the history table (divide all scores by 2) *)
val age_table : unit -> unit

(** Get statistics about the history table (total_entries, max_score, avg_score) *)
val get_stats : unit -> int * int * float
