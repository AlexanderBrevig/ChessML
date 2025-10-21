(** Search Common - Shared search utilities and pruning logic
    
    Provides reusable search components including: pruning heuristics (futility,
    razoring, null move, LMR, LMP), move ordering logic with proper priority
    handling, quiescence search helpers, and terminal position detection.
    Ensures consistent behavior between single and parallel search implementations.
    
    Includes FIXED move ordering to prevent history from dominating tactics
*)

open Chessml_core
open Chessml_core.Types

(** Futility pruning margins by depth *)
let futility_margin depth =
  match depth with
  | 1 -> 150 (* 1.5 pawns - more aggressive *)
  | 2 -> 300 (* 3 pawns - more aggressive *)
  | 3 -> 450 (* 4.5 pawns - more aggressive *)
  | _ -> 10000 (* No pruning at higher depths *)
;;

(** Check if futility pruning can be applied *)
let can_futility_prune depth static_eval alpha =
  depth <= 3 && static_eval + futility_margin depth < alpha
;;

(** Check if a move should be pruned by futility pruning *)
let should_prune_move can_futility_prune mv ~gives_check =
  can_futility_prune
  && (not (Move.is_capture mv))
  && (not (Move.is_promotion mv))
  && not gives_check
;;

(** Razoring: Return quiescence if eval + margin < alpha *)
module Razoring = struct
  (** Razoring margins by depth *)
  let margin depth =
    match depth with
    | 1 -> 300
    | 2 -> 500
    | 3 -> 700
    | _ -> 10000
  ;;

  (** Check if we can try razoring *)
  let can_try_razor _pos depth alpha in_check =
    depth <= 3 && (not in_check) && alpha < 90000 (* Not in mate search *)
  ;;

  (** Try razoring: if eval + margin < alpha, return true to signal drop to qsearch *)
  let should_razor eval depth alpha = eval + margin depth < alpha
end

(** Reverse Futility Pruning (Static Null Move): Prune if eval - margin > beta *)
module ReverseFutility = struct
  (** Reverse futility margins by depth *)
  let margin depth =
    match depth with
    | 1 -> 200
    | 2 -> 350
    | 3 -> 500
    | 4 -> 650
    | _ -> 10000
  ;;

  (** Check if we can prune the node (return beta) *)
  let can_prune _pos depth beta eval in_check =
    depth <= 4 && (not in_check) && beta > -90000 && eval - margin depth >= beta
  ;;
end

(** Late Move Pruning: Skip late quiet moves at low depths *)
module LateMove = struct
  (** Number of moves to search before pruning (based on depth and improving flag) *)
  let move_count_threshold depth improving =
    let base =
      match depth with
      | 1 -> 3
      | 2 -> 5
      | 3 -> 8
      | 4 -> 12
      | 5 -> 18
      | 6 -> 25
      | _ -> 1000 (* No pruning at higher depths *)
    in
    if improving then base + 2 else base
  ;;

  (** Should we prune this late quiet move? *)
  let should_prune depth move_num improving in_check is_tactical =
    depth <= 6
    && (not in_check)
    && (not is_tactical)
    && move_num > move_count_threshold depth improving
  ;;
end

(** Late Move Reduction parameters *)
module LMR = struct
  (** Pre-computed logarithmic reduction table: ln(depth) * ln(move_num) / 2.5 *)
  let reduction_table =
    let table = Array.make_matrix 64 64 0 in
    for depth = 1 to 63 do
      for move_num = 1 to 63 do
        if depth >= 3 && move_num >= 4
        then (
          let ln_depth = log (float_of_int depth) in
          let ln_move = log (float_of_int move_num) in
          let reduction = ln_depth *. ln_move /. 2.5 in
          table.(depth).(move_num) <- max 1 (int_of_float (reduction +. 0.5)))
        else table.(depth).(move_num) <- 0
      done
    done;
    table
  ;;

  (** Check if a move can be reduced *)
  let can_reduce depth move_count is_tactical alpha =
    depth >= 3
    (* Only at reasonable depths *)
    && move_count >= 4
    (* Skip first few moves *)
    && (not is_tactical)
    &&
    (* Don't reduce tactical moves *)
    alpha > -90000 (* Not in check / mating positions *)
  ;;

  (** Calculate reduction amount with logarithmic formula and adjustments 
      @param depth Current search depth
      @param move_count Number of moves searched so far (1-indexed)
      @param is_pv Whether this is a PV node
      @param improving Whether position is improving
      @param gives_check Whether move gives check
      @param history_score History heuristic score for this move
      @return Reduction amount (plies to reduce) *)
  let reduction depth move_count ~is_pv ~improving ~gives_check ~history_score =
    (* Clamp to table bounds *)
    let d = min 63 depth in
    let m = min 63 move_count in
    (* Base logarithmic reduction *)
    let base_reduction = reduction_table.(d).(m) in
    (* Adjustments to reduce less in important situations *)
    let adjustment = ref 0 in
    if is_pv then decr adjustment;
    (* Reduce less in PV *)
    if improving then decr adjustment;
    (* Reduce less if position improving *)
    if gives_check then decr adjustment;
    (* Reduce less for checks *)
    if history_score > 1000 then decr adjustment;
    (* Reduce less for good history *)

    (* Never reduce to less than 1 ply *)
    max 1 (base_reduction + !adjustment)
  ;;

  (** Simple reduction for compatibility (uses default parameters) *)
  let reduction_simple depth move_count =
    let d = min 63 depth in
    let m = min 63 move_count in
    reduction_table.(d).(m)
  ;;

  (** Determine if a move is tactical (should not be reduced) *)
  let is_tactical_move mv = Move.is_capture mv || Move.is_promotion mv
end

(** Null Move Pruning parameters *)
module NullMove = struct
  (** Reduction amount based on depth *)
  let reduction depth = if depth > 6 then 3 else 2

  (** Check if position has non-pawn material to avoid zugzwang *)
  let has_non_pawn_material pos =
    let side = Position.side_to_move pos in
    let piece_count = Position.count_non_pawn_material pos side in
    piece_count >= 3 (* At least 3 non-pawn pieces *)
  ;;

  (** Check if null move pruning can be applied *)
  let can_prune pos depth beta in_check =
    depth >= 3
    (* Deep enough to be useful *)
    && (not in_check)
    (* Not in check (can't pass) *)
    && beta < 90000
    &&
    (* Not in mate search *)
    has_non_pawn_material pos (* Avoid zugzwang positions *)
  ;;

  (** Make a null move (pass turn to opponent) *)
  let make_null_move pos = Position.make_null_move pos

  (** Calculate search depth for null move (original depth - 1 - reduction) *)
  let search_depth depth = depth - 1 - reduction depth
end

(** Move ordering scores *)
module Ordering = struct
  (** Base scores for different move types *)
  let tt_move_score = 20000

  let castle_score = 15000
  let check_score = 10000
  let winning_capture_base = 8000
  let equal_capture_base = 7000
  let killer_score = 5000
  let countermove_score = 4000
  let quiet_base = 0 (* Changed from 50 to 0 - history should be the differentiator *)

  (** Fast check detection without making the move *)
  let gives_check_fast pos mv =
    let from_sq = Move.from mv in
    let to_sq = Move.to_square mv in
    match Position.piece_at pos from_sq with
    | None -> false
    | Some piece ->
      let opponent = Color.opponent piece.color in
      let opp_king_sq =
        match Movegen.find_king pos opponent with
        | Some sq -> sq
        | None -> 0
      in
      let occupied = Movegen.compute_occupied pos in
      (* Simulate the piece being on the destination square *)
      let new_occupied = Bitboard.set (Bitboard.clear occupied from_sq) to_sq in
      (* Check if moving piece gives direct check *)
      let direct_check =
        let attacks = Movegen.attacks_for piece to_sq new_occupied in
        Bitboard.contains attacks opp_king_sq
      in
      if direct_check
      then true
      else (
        (* Check for discovered check: did we unblock a slider? *)
        (* Check if there's a slider of our color that could give discovered check *)
        let bishops = Position.get_pieces pos piece.color Bishop in
        let rooks = Position.get_pieces pos piece.color Rook in
        let queens = Position.get_pieces pos piece.color Queen in
        let sliders = Int64.logor bishops (Int64.logor rooks queens) in
        (* For each slider, check if removing from_sq opens a line to enemy king *)
        let discovered_check = ref false in
        Bitboard.iter
          (fun slider_sq ->
             if slider_sq <> from_sq
             then (
               (* Skip the moving piece itself *)
               match Position.piece_at pos slider_sq with
               | Some sp ->
                 let attacks_before = Movegen.attacks_for sp slider_sq occupied in
                 let attacks_after = Movegen.attacks_for sp slider_sq new_occupied in
                 (* If enemy king wasn't attacked before but is now, it's discovered check *)
                 if
                   (not (Bitboard.contains attacks_before opp_king_sq))
                   && Bitboard.contains attacks_after opp_king_sq
                 then discovered_check := true
               | None -> ()))
          sliders;
        !discovered_check)
  ;;

  (** Check if a move gives check (simplified for ordering) - OLD, kept for compatibility *)
  let gives_check_simple pos mv =
    try
      let new_pos = Position.make_move pos mv in
      let opponent = Color.opponent (Position.side_to_move pos) in
      let king_sq =
        match Movegen.find_king new_pos opponent with
        | Some sq -> sq
        | None -> 0
      in
      let side = Position.side_to_move new_pos in
      let attackers = Movegen.compute_attackers_to new_pos king_sq side in
      not (Bitboard.is_empty attackers)
    with
    | _ -> false
  ;;

  (** Quick MVV-LVA (Most Valuable Victim - Least Valuable Attacker) score *)
  let mvv_lva_score pos mv =
    let victim_opt = Position.piece_at pos (Move.to_square mv) in
    let attacker_opt = Position.piece_at pos (Move.from mv) in
    match victim_opt, attacker_opt with
    | Some victim, Some attacker ->
      (* Victim value * 10 - attacker value to prefer PxQ over QxP *)
      let victim_val = Eval.piece_kind_value victim.kind in
      let attacker_val = Eval.piece_kind_value attacker.kind in
      (victim_val * 10) - attacker_val
    | _ -> 0
  ;;

  (** Score a move for ordering (higher is better) *)
  let score_move pos mv ~is_tt_move ~is_killer ~is_countermove =
    if is_tt_move
    then tt_move_score
    else if Move.is_castle mv
    then castle_score
    else if gives_check_fast pos mv
    then check_score
    else if Move.is_capture mv
    then (
      (* Use MVV-LVA for quick pre-filtering *)
      let quick_score = mvv_lva_score pos mv in
      if quick_score >= 800
      then (
        (* Very good capture by MVV-LVA (e.g., PxQ, NxQ, etc.) - confirm with SEE *)
        let see_score = See.evaluate pos mv in
        if see_score > 0
        then winning_capture_base + min see_score 1000
        else if see_score = 0
        then equal_capture_base
        else max (-5000) see_score)
      else (
        (* Potentially bad capture - always use SEE *)
        let see_score = See.evaluate pos mv in
        if see_score > 0
        then winning_capture_base + min see_score 1000
        else if see_score = 0
        then equal_capture_base
        else max (-5000) see_score))
    else if is_killer
    then killer_score
    else if is_countermove
    then countermove_score
    else (
      (* For quiet moves, use history but cap the contribution *)
      let history_score = History.get_score mv in
      (* Clamp history contribution to range [0, 3000] to prevent dominating *)
      quiet_base + min 3000 (history_score / 10))
  ;;

  (** Order moves by score (highest first) *)
  let order_moves pos moves ~tt_move ~killer_check ~countermove_check =
    let is_tt mv =
      match tt_move with
      | Some tm -> mv = tm
      | None -> false
    in
    let scored =
      List.map
        (fun mv ->
           let is_killer = killer_check mv in
           let is_countermove = countermove_check mv in
           let score =
             score_move pos mv ~is_tt_move:(is_tt mv) ~is_killer ~is_countermove
           in
           mv, score)
        moves
    in
    let sorted = List.sort (fun (_, s1) (_, s2) -> compare s2 s1) scored in
    List.map fst sorted
  ;;
end

(** Terminal position detection *)
module Terminal = struct
  (** Check if position is checkmate or stalemate *)
  let check pos moves =
    if moves = []
    then (
      let side = Position.side_to_move pos in
      let opponent = Color.opponent side in
      let king_sq =
        match Movegen.find_king pos side with
        | Some sq -> sq
        | None -> 0
      in
      let attackers = Movegen.compute_attackers_to pos king_sq opponent in
      if Bitboard.is_empty attackers then Some 0 (* Stalemate *) else Some (-20000)
      (* Checkmate - we lost *))
    else None
  ;;
end

(** Quiescence search helpers *)
module Quiescence = struct
  (** Maximum quiescence search depth *)
  let max_depth = 4

  (** Generate tactical moves for quiescence search (captures, promotions, and checks) *)
  let generate_tactical_moves pos =
    let all_moves = Movegen.generate_moves pos in
    (* Filter for captures, checks, and promotions *)
    let tactical_moves =
      List.filter
        (fun mv ->
           Move.is_capture mv || Move.is_promotion mv || Ordering.gives_check_fast pos mv)
        all_moves
    in
    (* Prune obviously losing captures using SEE *)
    List.filter
      (fun mv ->
         if Move.is_capture mv
         then
           (* Only include captures that don't lose material *)
           See.evaluate pos mv >= 0
         else
           (* Include all checks and promotions *)
           true)
      tactical_moves
  ;;

  (** Check if quiescence should prune a capture based on SEE *)
  let should_prune_capture pos mv =
    if Move.is_capture mv
    then See.evaluate pos mv < 0 (* Prune losing captures *)
    else false
  ;;
end

(** Safe negation for alpha-beta bounds to avoid overflow *)
let safe_negate x = if x > 50000 then -50000 else if x < -50000 then 50000 else -x

(** Check if the current side to move is in check *)
let is_in_check pos =
  let side = Position.side_to_move pos in
  let opponent = Color.opponent side in
  match Movegen.find_king pos side with
  | Some king_sq ->
    let attackers = Movegen.compute_attackers_to pos king_sq opponent in
    not (Bitboard.is_empty attackers)
  | None -> false (* Should never happen *)
;;

(** Increment int64 reference *)
let incr_int64 r = r := Int64.add !r 1L

(** Alpha-beta search constants *)
module AlphaBeta = struct
  (** Initial alpha/beta bounds *)
  let initial_alpha = -100000

  let initial_beta = 100000

  (** Mate score detection threshold *)
  let mate_threshold = 90000

  (** Update alpha and best move *)
  let update_alpha_and_best alpha score best_move mv =
    let new_alpha = max alpha score in
    let new_best = if score > alpha then Some mv else best_move in
    new_alpha, new_best
  ;;

  (** Check if score triggers beta cutoff *)
  let is_beta_cutoff score beta = score >= beta

  (** Handle beta cutoff: store killers and history for quiet moves *)
  let handle_beta_cutoff mv depth killer_store history_record =
    if not (Move.is_capture mv)
    then (
      killer_store depth mv;
      history_record mv depth)
  ;;
end
