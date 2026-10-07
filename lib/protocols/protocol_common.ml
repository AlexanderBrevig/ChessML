(** Protocol_common - Engine options, opening book and time management shared by
    the UCI and XBoard front ends *)

open Chessml_engine

type option_kind =
  | Spin of
      { default : int
      ; min : int
      ; max : int
      }
  | Check of bool

type engine_option =
  { name : string
  ; kind : option_kind
  ; apply : string -> unit (** @raise Failure on a bad value *)
  }

let own_book = ref true

let parse_bool value =
  match String.lowercase_ascii value with
  | "true" | "1" | "on" -> true
  | "false" | "0" | "off" -> false
  | _ -> failwith ("expected true/false, got " ^ value)
;;

let spin name ~default ~min ~max set =
  { name
  ; kind = Spin { default; min; max }
  ; apply =
      (fun value ->
        match int_of_string_opt (String.trim value) with
        | Some n when n >= min && n <= max -> set n
        | _ -> failwith (Printf.sprintf "expected %d..%d, got %s" min max value))
  }
;;

let check name ~default set =
  { name; kind = Check default; apply = (fun value -> set (parse_bool value)) }
;;

(** Options offered by both protocols *)
let options =
  [ spin "Hash" ~default:16 ~min:1 ~max:1024 (fun mb -> Search.set_hash_size_mb mb)
  ; spin
      "MaxDepth"
      ~default:(Config.get_max_search_depth ())
      ~min:1
      ~max:50
      Config.set_max_search_depth
  ; spin
      "QuiescenceDepth"
      ~default:(Config.get_max_quiescence_depth ())
      ~min:1
      ~max:20
      Config.set_max_quiescence_depth
  ; check
      "UseQuiescence"
      ~default:(Config.get_use_quiescence ())
      Config.set_use_quiescence
  ; check
      "UseTranspositionTable"
      ~default:(Config.get_use_transposition_table ())
      Config.set_use_transposition_table
  ; check "DebugOutput" ~default:(Config.get_debug_output ()) Config.set_debug_output
  ; check "OwnBook" ~default:true (fun b -> own_book := b)
  ]
;;

(** Set an option by (case-insensitive) name *)
let set_option name value =
  match
    List.find_opt
      (fun o -> String.lowercase_ascii o.name = String.lowercase_ascii (String.trim name))
      options
  with
  | None -> Error ("unknown option " ^ name)
  | Some o ->
    (match o.apply value with
     | () -> Ok ()
     | exception Failure msg -> Error (Printf.sprintf "%s: %s" o.name msg))
;;

(** Open the first opening book found in [Config.get_book_paths] *)
let load_book () =
  List.find_map
    (fun path -> Option.map (fun book -> book, path) (Opening_book.open_book path))
    (Config.get_book_paths ())
;;

(** Weighted random book move, if the book is enabled and has the position *)
let book_move book pos =
  if !own_book then Opening_book.get_book_move ~random:true book pos else None
;;

(** Milliseconds to spend on a move given the remaining clock, the increment and
    optionally the moves left until the next time control. Keeps a safety margin
    so the engine never flags. *)
let time_budget_ms ~remaining_ms ~increment_ms ~moves_to_go =
  let moves =
    match moves_to_go with
    | Some n when n > 0 -> n + 1
    | _ -> 30
  in
  let budget = (remaining_ms / moves) + (increment_ms * 3 / 4) in
  let safety = min 50 (remaining_ms / 10) in
  max 1 (min budget (remaining_ms - safety))
;;
