(** Search Common - Pruning parameters and move ordering for the search

    Margins and thresholds for the selective search (futility, reverse futility,
    razoring, late move pruning and reductions), move ordering, and tactical move
    generation for quiescence search.
*)

open Chessml_core
open Chessml_core.Types

(** Futility pruning: skip quiet moves at depth <= 3 when eval + margin <= alpha *)
let futility_margin depth = 150 * depth

(** Reverse futility pruning: return eval when eval - margin >= beta at depth <= 4 *)
let reverse_futility_margin depth = 50 + (150 * depth)

(** Razoring: drop to quiescence when eval + margin < alpha at depth <= 3 *)
let razor_margin depth = 100 + (200 * depth)

(** Late move pruning (depth 3-6): quiet moves searched before the rest are skipped *)
let late_move_count depth =
  match depth with
  | 3 -> 8
  | 4 -> 12
  | 5 -> 18
  | 6 -> 25
  | _ -> max_int
;;

(** Late Move Reduction parameters *)
module LMR = struct
  (** Pre-computed logarithmic reduction table: ln(depth) * ln(move_num) / 2.5 *)
  let reduction_table =
    Array.init 64 (fun depth ->
      Array.init 64 (fun move_num ->
        if depth >= 3 && move_num >= 4
        then (
          let r = log (float_of_int depth) *. log (float_of_int move_num) /. 2.5 in
          max 1 (int_of_float (r +. 0.5)))
        else 0))
  ;;

  (** Reduction in plies for the [move_count]th move at [depth]: one less at PV
      nodes and for moves with good history, at least 1 *)
  let reduction depth move_count ~is_pv ~history_score =
    let base = reduction_table.(min 63 depth).(min 63 move_count) in
    let adjust = (if is_pv then 1 else 0) + if history_score > 1000 then 1 else 0 in
    max 1 (base - adjust)
  ;;

  (** Captures and promotions are never reduced *)
  let is_tactical_move mv = Move.is_capture mv || Move.is_promotion mv
end

(** Move ordering scores *)
module Ordering = struct
  let tt_move_score = 20000
  let castle_score = 15000
  let check_score = 10000
  let promotion_base = 9000
  let winning_capture_base = 8000
  let equal_capture_base = 7000
  let killer_score = 5000
  let countermove_score = 4000
  let quiet_base = 0

  (** Does the move give check? Exact, including promotions, castling and
      discovered checks *)
  let gives_check pos mv = Movegen.in_check (Position.make_move pos mv)

  (** MVV-LVA (Most Valuable Victim - Least Valuable Attacker) score *)
  let mvv_lva_score pos mv =
    match
      Position.piece_at pos (Move.to_square mv), Position.piece_at pos (Move.from mv)
    with
    | Some victim, Some attacker ->
      (PieceKind.value victim.kind * 10) - PieceKind.value attacker.kind
    | _ -> 0
  ;;

  (** Score a move for ordering (higher is better). [history_score] orders the
      remaining quiet moves. *)
  let score_move ?(history_score = 0) pos mv ~is_tt_move ~is_killer ~is_countermove =
    if is_tt_move
    then tt_move_score
    else if Move.is_castle mv
    then castle_score
    else if gives_check pos mv
    then check_score
    else if Move.is_promotion mv
    then (
      match Move.promotion mv with
      | Some Queen -> promotion_base + 500
      | Some Rook -> promotion_base + 100
      | _ -> promotion_base + 50)
    else if Move.is_capture mv
    then (
      let see_score = See.evaluate pos mv in
      if see_score > 0
      then winning_capture_base + min see_score 1000
      else if see_score = 0
      then equal_capture_base
      else max (-5000) see_score)
    else if is_killer
    then killer_score
    else if is_countermove
    then countermove_score
    else quiet_base + min 3000 (history_score / 10)
  ;;

  (** Order moves by score (highest first) *)
  let order_moves ?history pos moves ~tt_move ~killer_check ~countermove_check =
    let history_score mv =
      Option.fold ~none:0 ~some:(fun h -> History.get_score h mv) history
    in
    List.map
      (fun mv ->
         ( mv
         , score_move
             ~history_score:(history_score mv)
             pos
             mv
             ~is_tt_move:(tt_move = Some mv)
             ~is_killer:(killer_check mv)
             ~is_countermove:(countermove_check mv) ))
      moves
    |> List.stable_sort (fun (_, s1) (_, s2) -> compare s2 s1)
    |> List.map fst
  ;;
end

(** Quiescence search moves: captures that do not lose material (SEE >= 0) and
    promotions, plus quiet checks when [include_checks] *)
let tactical_moves ?(include_checks = false) pos =
  List.filter
    (fun mv ->
       if Move.is_capture mv
       then See.evaluate pos mv >= 0
       else Move.is_promotion mv || (include_checks && Ordering.gives_check pos mv))
    (Movegen.generate_moves pos)
;;
