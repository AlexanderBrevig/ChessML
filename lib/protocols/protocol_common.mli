(** Engine options, opening book and time management shared by the UCI and XBoard
    front ends *)

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
  ; apply : string -> unit
  }

(** Options offered by both protocols *)
val options : engine_option list

(** Set an option by case-insensitive name; booleans accept true/false/1/0/on/off *)
val set_option : string -> string -> (unit, string) result

(** Open the first opening book found in [Config.get_book_paths] *)
val load_book : unit -> (Opening_book.book * string) option

(** Weighted random book move, if the OwnBook option is on and the book has one *)
val book_move : Opening_book.book option -> Position.t -> Chessml_core.Move.t option

(** Milliseconds to spend on a move, keeping a safety margin *)
val time_budget_ms : remaining_ms:int -> increment_ms:int -> moves_to_go:int option -> int
