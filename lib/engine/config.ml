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
