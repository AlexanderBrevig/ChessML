(** Opening book support - Polyglot format *)

open Chessml_core

(** Polyglot book file handle *)
type book

(** Open a polyglot format book file 
    @param filename Path to .bin book file
    @return Some book handle or None if file cannot be opened *)
val open_book : string -> book option

(** Close a book file *)
val close_book : book -> unit

(** Probe book for a position
    @param book Book handle (or None)
    @param pos Position to look up
    @return List of (move, weight) pairs for available book moves *)
val probe : book option -> Position.t -> (Move.t * int) list

(** Get best book move for position
    @param random If true, select weighted random move; if false, select highest weight
    @param book Book handle (or None)
    @param pos Position to look up
    @return Some move from book, or None if position not in book *)
val get_book_move : ?random:bool -> book option -> Position.t -> Move.t option

(** Get detailed book statistics for position
    @param book Book handle (or None)
    @param pos Position to look up
    @return List of (move, weight, percentage) tuples sorted by popularity *)
val get_book_stats : book option -> Position.t -> (Move.t * int * float) list
