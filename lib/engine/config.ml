(** Config - Engine configuration and runtime parameters
    
    Centralized configuration for search parameters including depth limits,
    quiescence settings, transposition table size, and feature toggles.
    Allows runtime adjustment of search behavior without recompilation.
    
    Used by: All search modules, UCI option handling
*)

type search_config =
  { max_search_depth : int
  ; max_quiescence_depth : int
  ; transposition_table_size : int
  ; use_quiescence : bool
  ; use_transposition_table : bool
  ; debug_output : bool
  }

(** Default configuration *)
let default_config =
  { max_search_depth = 20
  ; max_quiescence_depth = 8
  ; transposition_table_size = 1048576
  ; (* 1M entries *)
    use_quiescence = true
  ; use_transposition_table = true
  ; debug_output = false
  }
;;

(** Global configuration - mutable for runtime updates *)
let config = ref default_config

(** Update configuration *)
let set_max_search_depth depth =
  config := { !config with max_search_depth = max 1 (min depth 50) }
;;

let set_max_quiescence_depth depth =
  config := { !config with max_quiescence_depth = max 1 (min depth 20) }
;;

let set_transposition_table_size size =
  let size = max 1024 (min size 16777216) in
  (* 1K to 16M entries *)
  config := { !config with transposition_table_size = size }
;;

let set_use_quiescence enabled = config := { !config with use_quiescence = enabled }

let set_use_transposition_table enabled =
  config := { !config with use_transposition_table = enabled }
;;

let set_debug_output enabled = config := { !config with debug_output = enabled }

(** Getters *)
let get_max_search_depth () = !config.max_search_depth

let get_max_quiescence_depth () = !config.max_quiescence_depth
let get_transposition_table_size () = !config.transposition_table_size
let get_use_quiescence () = !config.use_quiescence
let get_use_transposition_table () = !config.use_transposition_table
let get_debug_output () = !config.debug_output

(** Reset to defaults *)
let reset_to_defaults () = config := default_config

(** Print current configuration *)
let print_config () =
  Printf.printf "=== Engine Configuration ===\n";
  Printf.printf "Max search depth: %d\n" !config.max_search_depth;
  Printf.printf "Max quiescence depth: %d\n" !config.max_quiescence_depth;
  Printf.printf "Transposition table size: %d entries\n" !config.transposition_table_size;
  Printf.printf "Use quiescence: %b\n" !config.use_quiescence;
  Printf.printf "Use transposition table: %b\n" !config.use_transposition_table;
  Printf.printf "Debug output: %b\n" !config.debug_output;
  Printf.printf "=============================\n"
;;

(** Get potential paths for opening book file
    
    Returns a list of paths to try for the opening book, in order of preference:
    1. Current directory (./book.bin)
    2. XDG_DATA_HOME/chessml/book.bin (typically ~/.local/share/chessml/book.bin)
    3. ~/.chessml/book.bin (fallback for non-XDG systems)
    4. /usr/local/share/chessml/book.bin (system-wide installation)
    5. /usr/share/chessml/book.bin (system-wide installation)
*)
let get_book_paths () =
  let home = Sys.getenv_opt "HOME" |> Option.value ~default:"" in
  let xdg_data_home =
    Sys.getenv_opt "XDG_DATA_HOME"
    |> Option.value ~default:(Filename.concat home ".local/share")
  in
  [ "book.bin" (* Current directory *)
  ; Filename.concat (Filename.concat xdg_data_home "chessml") "book.bin"
  ; Filename.concat (Filename.concat home ".chessml") "book.bin"
  ; "/usr/local/share/chessml/book.bin"
  ; "/usr/share/chessml/book.bin"
  ]
;;
