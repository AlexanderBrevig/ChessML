(** Killers - Killer move heuristic for move ordering
    
    Stores quiet moves that caused beta cutoffs at each ply depth. These moves are
    likely to be good in sibling nodes at the same depth. Maintains up to 2 killer
    moves per depth level using a replacement scheme (most recent killers).
    
    Used by: Search move ordering to prioritize refutation moves (score: 5000)
    Note: FIXED bug where unbounded history was dominating killers
*)

open Chessml_core

(** Killer move table - stores up to 2 killer moves per depth level *)
type killer_table =
  { mutable killers : Move.t option array array (* [depth][slot] where slot 0,1 *)
  ; max_depth : int
  }

(** Create a new killer move table *)
let create max_depth = { killers = Array.make_matrix max_depth 2 None; max_depth }

(** Clear all killer moves *)
let clear table =
  for depth = 0 to table.max_depth - 1 do
    for slot = 0 to 1 do
      table.killers.(depth).(slot) <- None
    done
  done
;;

(** Store a killer move at given depth *)
let store_killer table depth move =
  if depth >= 0 && depth < table.max_depth
  then (
    (* Check if this move is already stored as killer *)
    let already_stored = ref false in
    for slot = 0 to 1 do
      match table.killers.(depth).(slot) with
      | Some existing_move
        when Move.from existing_move = Move.from move
             && Move.to_square existing_move = Move.to_square move
             && Move.kind existing_move = Move.kind move -> already_stored := true
      | _ -> ()
    done;
    (* If not already stored, add it (replacing oldest) *)
    if not !already_stored
    then (
      (* Shift killer 0 to killer 1, store new move as killer 0 *)
      table.killers.(depth).(1) <- table.killers.(depth).(0);
      table.killers.(depth).(0) <- Some move))
;;

(** Get killer moves for a given depth *)
let get_killers table depth =
  if depth >= 0 && depth < table.max_depth
  then Array.to_list table.killers.(depth) |> List.filter_map (fun x -> x)
  else []
;;

(** Check if a move is a killer move at given depth *)
let is_killer table depth move =
  if depth >= 0 && depth < table.max_depth
  then (
    let killers = get_killers table depth in
    List.exists
      (fun killer_move ->
         Move.from killer_move = Move.from move
         && Move.to_square killer_move = Move.to_square move
         && Move.kind killer_move = Move.kind move)
      killers)
  else false
;;

(** Global killer table - initialized with reasonable max depth *)
let global_killer_table = create 64

(** Clear global killer table (call at start of new search) *)
let clear_global () = clear global_killer_table

(** Store killer move in global table *)
let store_global_killer depth move = store_killer global_killer_table depth move

(** Get killer moves from global table *)
let get_global_killers depth = get_killers global_killer_table depth

(** Check if move is killer in global table *)
let is_global_killer depth move = is_killer global_killer_table depth move
