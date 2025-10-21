(** Static Exchange Evaluation (SEE)
    
    Evaluates the material outcome of a capture sequence on a square.
    Uses a minimax-like approach to simulate all possible captures/recaptures.
    
    Algorithm:
    1. Find all attackers and defenders of the target square
    2. Simulate the capture sequence, alternating sides
    3. Always use the least valuable attacker (LVA)
    4. Account for x-ray attacks (pieces behind captured pieces)
    5. Return the net material gain/loss
*)

open Chessml_core
open Types

(** Get all attackers of a square for a given color *)
let get_attackers (pos : Position.t) (sq : Square.t) (color : color)
  : (Square.t * piece_kind) list
  =
  let attackers = ref [] in
  let blockers = Movegen.compute_occupied pos in
  (* Check all squares for pieces that can attack target square *)
  for from_sq = 0 to 63 do
    match Position.piece_at pos from_sq with
    | Some piece when piece.color = color ->
      let attacks = Movegen.attacks_for piece from_sq blockers in
      if Bitboard.contains attacks sq
      then attackers := (from_sq, piece.kind) :: !attackers
    | _ -> ()
  done;
  !attackers
;;

(** Find the least valuable attacker from a list *)
let find_least_valuable_attacker (attackers : (Square.t * piece_kind) list)
  : (Square.t * piece_kind) option
  =
  match attackers with
  | [] -> None
  | _ ->
    (* Sort by piece value and return the least valuable *)
    let sorted =
      List.sort
        (fun (_, k1) (_, k2) ->
           compare (PieceKind.value k1) (PieceKind.value k2))
        attackers
    in
    Some (List.hd sorted)
;;

(** Simulate capture sequence and return material balance
    
    Uses a simpler approach: build list of capture values, then negamax from the end.
    
    @param pos Current position
    @param target Target square being contested
    @param side_to_move Color of side making next capture
    @param target_value Value of piece on target square (what we gain if we capture)
    @param gains Accumulated list of piece values in capture sequence
    @return List of captured piece values (in order of capture)
*)
let rec see_recursive
          (pos : Position.t)
          (target : Square.t)
          (side_to_move : color)
          (target_value : int)
          (gains : int list)
  : int list
  =
  (* Safety check: limit recursion depth to avoid infinite loops *)
  if List.length gains >= 32
  then gains (* Max 32 captures in sequence is more than enough *)
  else (
    (* Find all attackers for current side *)
    let attackers = get_attackers pos target side_to_move in
    match find_least_valuable_attacker attackers with
    | None ->
      (* No more attackers - sequence ends *)
      gains
    | Some (attacker_sq, attacker_kind) ->
      (* Make the capture - we gain the target, but risk losing the attacker *)
      (* Remove attacker from position and update target square *)
      let new_pos = Position.clear_square attacker_sq pos in
      let new_pos =
        Position.set_piece target { color = side_to_move; kind = attacker_kind } new_pos
      in
      (* Add captured piece value to gains *)
      let new_gains = target_value :: gains in
      (* Handle promotion: if pawn reaches back rank, it becomes queen *)
      let promoted_value =
        if attacker_kind = Pawn
        then (
          let target_rank = target / 8 in
          if
            (side_to_move = White && target_rank = 7)
            || (side_to_move = Black && target_rank = 0)
          then Piece_tables.piece_kind_total_value Queen side_to_move target
          else 
            (* Pawn's value at the target square *)
            Piece_tables.piece_kind_total_value attacker_kind side_to_move target)
        else 
          (* Piece's value at the target square *)
          Piece_tables.piece_kind_total_value attacker_kind side_to_move target
      in
      (* Recurse with opponent's turn *)
      let opponent =
        match side_to_move with
        | White -> Black
        | Black -> White
      in
      see_recursive new_pos target opponent promoted_value new_gains)
;;

(** Negamax evaluation of gain list
    
    The gains list contains piece values captured at each step.
    gains[0] = initial piece captured by White
    gains[1] = piece captured by Black (recapture)
    gains[2] = piece captured by White (re-recapture)
    ...
    
    Working backwards from the end, each side decides whether to continue.
    At each step: score = gain - (opponent's best score from next move)
    But each side can also choose not to capture (stand pat = 0).
    
    @param gains List of captured piece values (oldest first)
    @return Net material evaluation from first attacker's perspective
*)
let negamax_gains (gains : int list) : int =
  (* Process from the end backwards *)
  let rec eval_from_end lst can_stand_pat =
    match lst with
    | [] -> 0 (* No captures *)
    | [ gain ] -> gain (* Only one capture, no recapture possible *)
    | gain :: rest ->
      (* I capture 'gain', but opponent might recapture *)
      (* Opponent can choose to stand pat (not recapture) if it's bad for them *)
      let opponent_score = eval_from_end rest true in
      (* My score: I gain this piece, but opponent may gain from recaptures *)
      let my_score = gain - opponent_score in
      (* I can only stand pat if I'm not the initial attacker *)
      if can_stand_pat then max 0 my_score else my_score
  in
  (* Initial attacker cannot stand pat - they committed to the capture *)
  eval_from_end gains false
;;

(** Evaluate static exchange for a move
    
    Returns the expected material outcome of the capture sequence
    from the perspective of the side making the initial capture.
    
    @param pos Current position
    @param move Move to evaluate (should be a capture)
    @return Material evaluation in centipawns (positive = good for attacker)
*)
let evaluate (pos : Position.t) (move : Move.t) : int =
  let to_sq = Move.to_square move in
  let from_sq = Move.from move in
  (* Get the moving piece *)
  let moving_piece =
    match Position.piece_at pos from_sq with
    | Some p -> p
    | None -> { color = White; kind = Pawn }
    (* Shouldn't happen *)
  in
  (* Get captured piece value including positional bonus (or 0 for quiet moves) *)
  let captured_value =
    match move with
    | _ when Move.is_capture move || Move.is_en_passant move ->
      (match Position.piece_at pos to_sq with
       | Some p -> Piece_tables.piece_total_value p to_sq  (* Include positional value *)
       | None ->
         (* En passant: captured pawn is not on target square *)
         if Move.is_en_passant move 
         then (
           let captured_pawn_sq =
             if moving_piece.color = White then to_sq - 8 else to_sq + 8
           in
           let opponent = if moving_piece.color = White then Black else White in
           Piece_tables.piece_kind_total_value Pawn opponent captured_pawn_sq)
         else 0)
    | _ -> 0 (* Quiet move *)
  in
  (* If no capture, SEE is 0 *)
  if captured_value = 0
  then 0
  else (
    (* Make the initial capture *)
    let new_pos = Position.clear_square from_sq pos in
    let new_pos = Position.set_piece to_sq moving_piece new_pos in
    (* Handle en passant: remove the captured pawn *)
    let new_pos =
      if Move.is_en_passant move
      then (
        let captured_pawn_sq =
          if moving_piece.color = White then to_sq - 8 else to_sq + 8
        in
        Position.clear_square captured_pawn_sq new_pos)
      else new_pos
    in
    (* Handle promotion - value at target square *)
    let moving_value =
      if Move.is_promotion move
      then (
        match Move.promotion move with
        | Some promo_piece -> Piece_tables.piece_kind_total_value promo_piece moving_piece.color to_sq
        | None -> Piece_tables.piece_total_value moving_piece to_sq)
      else Piece_tables.piece_total_value moving_piece to_sq  (* Piece value at target square *)
    in
    (* Initial gain: we captured target, but now our piece can be recaptured *)
    let initial_gain = captured_value in
    (* See if opponent can recapture *)
    let opponent =
      match moving_piece.color with
      | White -> Black
      | Black -> White
    in
    let gains = see_recursive new_pos to_sq opponent moving_value [ initial_gain ] in
    (* Reverse gains list since we built it backwards with :: *)
    let gains_forward = List.rev gains in
    (* Evaluate with negamax *)
    negamax_gains gains_forward)
;;

(** Check if a capture is winning (SEE >= 0) *)
let is_winning_capture (pos : Position.t) (move : Move.t) : bool = evaluate pos move >= 0

(** Check if a capture loses material (SEE < 0) *)
let is_losing_capture (pos : Position.t) (move : Move.t) : bool = evaluate pos move < 0
