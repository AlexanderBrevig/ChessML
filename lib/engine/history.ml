(** History heuristic for move ordering
    
    Tracks moves that have historically caused beta cutoffs.
    Moves that caused cutoffs in previous searches are likely to be good in similar positions.
    
    Uses a [from_square][to_square] table to store scores.
    Scores are weighted by depth - deeper cutoffs are more valuable.
*)

open Chessml_core

(** History table: [from_square][to_square] -> score *)
type t = int array array

(** Global history table *)
let global_table : t option ref = ref None

(** Initialize the history table *)
let create () : t = Array.make_matrix 64 64 0

(** Get the global history table, creating it if necessary *)
let get_global () : t =
  match !global_table with
  | Some table -> table
  | None ->
    let table = create () in
    global_table := Some table;
    table
;;

(** Clear the history table *)
let clear () : unit =
  match !global_table with
  | Some table ->
    for from_sq = 0 to 63 do
      for to_sq = 0 to 63 do
        table.(from_sq).(to_sq) <- 0
      done
    done
  | None -> ()
;;

(** Record a move that caused a beta cutoff
    
    @param mv The move that caused the cutoff
    @param depth The depth at which the cutoff occurred (higher depth = more important)
*)
let record_cutoff (mv : Move.t) (depth : int) : unit =
  let table = get_global () in
  let from_sq = Move.from mv in
  let to_sq = Move.to_square mv in
  (* Increment score, weighted by depth squared (deeper cutoffs are exponentially more valuable) *)
  let bonus = depth * depth in
  let current = table.(from_sq).(to_sq) in
  (* Cap history scores at 10000 to prevent unbounded growth *)
  let new_score = min 10000 (current + bonus) in
  table.(from_sq).(to_sq) <- new_score
;;

(** Record a move that failed (didn't cause cutoff or was searched but didn't improve alpha)
    
    This is used for some advanced history heuristics that penalize quiet moves that don't work.
    For now, we don't penalize - just don't reward.
*)
let record_failure (_mv : Move.t) (_depth : int) : unit =
  () (* Could implement negative scoring here if desired *)
;;

(** Get the history score for a move from the global table
    
    @param mv The move to score
    @return The history score (0 if never caused a cutoff)
*)
let get_score (mv : Move.t) : int =
  let table = get_global () in
  let from_sq = Move.from mv in
  let to_sq = Move.to_square mv in
  table.(from_sq).(to_sq)
;;

(** Get the history score for a move from a specific table
    
    @param table The history table
    @param mv The move to score
    @return The history score (0 if never caused a cutoff)
*)
let get_score_from_table (table : t) (mv : Move.t) : int =
  let from_sq = Move.from mv in
  let to_sq = Move.to_square mv in
  table.(from_sq).(to_sq)
;;

(** Record a cutoff in a specific table
    
    @param table The history table
    @param mv The move that caused the cutoff
    @param depth The depth at which the cutoff occurred
*)
let record_cutoff_in_table (table : t) (mv : Move.t) (depth : int) : unit =
  let from_sq = Move.from mv in
  let to_sq = Move.to_square mv in
  let bonus = depth * depth in
  let current = table.(from_sq).(to_sq) in
  (* Cap history scores at 10000 to prevent unbounded growth *)
  let new_score = min 10000 (current + bonus) in
  table.(from_sq).(to_sq) <- new_score
;;

(** Age the history table by dividing all scores by 2
    
    This prevents old history from dominating and helps adapt to changing positions.
    Should be called periodically (e.g., every N nodes or between searches).
*)
let age_table () : unit =
  match !global_table with
  | Some table ->
    for from_sq = 0 to 63 do
      for to_sq = 0 to 63 do
        table.(from_sq).(to_sq) <- table.(from_sq).(to_sq) / 2
      done
    done
  | None -> ()
;;

(** Get statistics about the history table
    
    @return (total_entries, max_score, avg_score)
*)
let get_stats () : int * int * float =
  match !global_table with
  | Some table ->
    let total = ref 0 in
    let max_score = ref 0 in
    let sum = ref 0 in
    for from_sq = 0 to 63 do
      for to_sq = 0 to 63 do
        let score = table.(from_sq).(to_sq) in
        if score > 0
        then (
          incr total;
          sum := !sum + score;
          if score > !max_score then max_score := score)
      done
    done;
    let avg = if !total > 0 then float_of_int !sum /. float_of_int !total else 0.0 in
    !total, !max_score, avg
  | None -> 0, 0, 0.0
;;
