(** Parallel search using Lazy SMP *)

open Chessml_core

(** Thread-local search state *)
type thread_state

(** Shared state between threads *)
type shared_state

(** Main parallel search function.
    
    Launches multiple threads that search independently, sharing only
    the transposition table. Uses Lazy SMP algorithm for work distribution.
    
    @param pos Position to search
    @param depth Target search depth
    @param num_threads Number of threads to use (1 = single-threaded)
    @return (best_score, best_move, total_nodes)
*)
val parallel_search : Position.t -> int -> int -> int * Move.t option * int64

(** Single-threaded search for comparison and testing.
    
    @param pos Position to search
    @param depth Target search depth
    @return (best_score, best_move, total_nodes)
*)
val single_threaded_search : Position.t -> int -> int * Move.t option * int64
