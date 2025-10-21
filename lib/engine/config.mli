(** Engine configuration interface *)

type search_config =
  { max_search_depth : int
  ; max_quiescence_depth : int
  ; transposition_table_size : int
  ; use_quiescence : bool
  ; use_transposition_table : bool
  ; debug_output : bool
  }

(** Configuration setters *)
val set_max_search_depth : int -> unit

val set_max_quiescence_depth : int -> unit
val set_transposition_table_size : int -> unit
val set_use_quiescence : bool -> unit
val set_use_transposition_table : bool -> unit
val set_debug_output : bool -> unit

(** Configuration getters *)
val get_max_search_depth : unit -> int

val get_max_quiescence_depth : unit -> int
val get_transposition_table_size : unit -> int
val get_use_quiescence : unit -> bool
val get_use_transposition_table : unit -> bool
val get_debug_output : unit -> bool

(** Utility functions *)
val reset_to_defaults : unit -> unit

val print_config : unit -> unit

(** Get potential paths for opening book file
    
    Returns a list of paths to try for the opening book, in order of preference:
    1. Current directory (./book.bin)
    2. XDG_DATA_HOME/chessml/book.bin (typically ~/.local/share/chessml/book.bin)
    3. ~/.chessml/book.bin (fallback for non-XDG systems)
    4. /usr/local/share/chessml/book.bin (system-wide installation)
    5. /usr/share/chessml/book.bin (system-wide installation)
*)
val get_book_paths : unit -> string list
