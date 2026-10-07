(** History heuristic for move ordering

    Quiet moves that caused beta cutoffs score higher, weighted by depth squared,
    in a [from_square][to_square] table. Scores are capped and halved between
    searches so old information fades.
*)

open Chessml_core

type t = int array array

let max_score = 10000
let create () : t = Array.make_matrix 64 64 0
let clear (table : t) = Array.iter (fun row -> Array.fill row 0 64 0) table

(** Record a quiet move that caused a beta cutoff at [depth] *)
let record_cutoff (table : t) (mv : Move.t) (depth : int) =
  let row = table.(Move.from mv) in
  let to_sq = Move.to_square mv in
  row.(to_sq) <- min max_score (row.(to_sq) + (depth * depth))
;;

let get_score (table : t) (mv : Move.t) = table.(Move.from mv).(Move.to_square mv)

(** Halve all scores *)
let age (table : t) =
  Array.iter (fun row -> Array.iteri (fun i s -> row.(i) <- s / 2) row) table
;;

(** (non-zero entries, max score, average non-zero score) *)
let stats (table : t) =
  let total, max_s, sum =
    Array.fold_left
      (Array.fold_left (fun (n, m, s) score ->
         if score > 0 then n + 1, max m score, s + score else n, m, s))
      (0, 0, 0)
      table
  in
  total, max_s, if total > 0 then float_of_int sum /. float_of_int total else 0.0
;;
