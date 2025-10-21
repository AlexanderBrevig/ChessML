(** Root Splitting Parallel Search *)

open Chessml_core

(** Search result from root splitting *)
type search_result =
  { best_move : Move.t
  ; best_score : int
  ; nodes : int64
  ; depth : int
  }

(** Root splitting parallel search.
    Splits root moves among threads for embarrassingly parallel search.
    Each thread searches its own moves with zero contention.
    
    @param pos Position to search
    @param depth Target search depth
    @param num_threads Number of threads to use
    @return (best_score, best_move_option, total_nodes)
*)
val root_split_search : Position.t -> int -> int -> int * Move.t option * int64

(** Single-threaded root search for comparison *)
val single_threaded_root_search : Position.t -> int -> int * Move.t option * int64
