(** Eval - Static position evaluation function (main orchestrator)

    Evaluates chess positions from the side-to-move perspective using multiple factors:
    - Material balance (piece values)
    - Piece-square tables (positional bonuses)
    - Pawn structure (delegated to Eval_pawn_structure)
    - King safety and castling (delegated to Eval_king_safety)
    - Piece development and bishop pair (delegated to Eval_pieces)
    - Endgame techniques (delegated to Eval_endgame)

    Returns centipawn score (100 = one pawn advantage)
*)

open Chessml_core
open Types

let piece_kind_value = PieceKind.value
let piece_value (piece : piece) = PieceKind.value piece.kind
let is_piece_hanging = Eval_pieces.is_piece_hanging
let evaluate_fifty_move_incentive = Eval_endgame.evaluate_fifty_move_incentive

(** Piece-square table total for a color *)
let positional pos color =
  List.fold_left
    (fun acc kind ->
       Bitboard.fold
         (fun sq acc -> acc + Piece_tables.piece_square_value { color; kind } sq)
         (Position.get_pieces pos color kind)
         acc)
    0
    [ Pawn; Knight; Bishop; Rook; Queen; King ]
;;

(** Sum of all evaluation components, from the side to move's perspective *)
let evaluate_position (pos : Position.t) : int =
  let side = Position.side_to_move pos in
  let opponent = Color.opponent side in
  (* [diff f] is f for us minus f for them *)
  let diff f = f side - f opponent in
  let our_material = Position.material pos side in
  let their_material = Position.material pos opponent in
  let material_diff = our_material - their_material in
  let total_material = our_material + their_material in
  let pawn_structure =
    let white_minus_black = Eval_pawn_structure.evaluate_pawn_structure pos in
    if side = White then white_minus_black else -white_minus_black
  in
  (* Trade incentive: the side ahead prefers fewer pieces on the board *)
  let trade_incentive =
    let pieces =
      Position.count_non_pawn_material pos side
      + Position.count_non_pawn_material pos opponent
    in
    if material_diff > 200
    then -(pieces * 5)
    else if material_diff < -200
    then pieces * 5
    else 0
  in
  material_diff
  + diff (positional pos)
  + pawn_structure
  + trade_incentive
  + diff (fun c -> Eval_king_safety.evaluate_king_safety pos c total_material)
  + diff (Eval_pieces.evaluate_development pos)
  + diff (Eval_pieces.evaluate_bishop_pair pos)
  + Eval_endgame.evaluate_fifty_move_incentive pos material_diff
  + diff (Eval_endgame.evaluate_rook_endgame pos)
  + diff (Eval_endgame.evaluate_ladder_mate pos)
;;

(** Score from the side to move's perspective in centipawns; 0 for dead positions *)
let evaluate (pos : Position.t) : int =
  if Position.has_insufficient_material pos then 0 else evaluate_position pos
;;
