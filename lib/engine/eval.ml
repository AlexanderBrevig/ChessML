(** Eval - Static position evaluation function (main orchestrator)
    
    Evaluates chess positions from the side-to-move perspective using multiple factors:
    - Material balance (piece values)
    - Piece-square tables (positional bonuses)
    - Pawn structure (delegated to Eval_pawn_structure)
    - King safety and castling (delegated to Eval_king_safety)
    - Piece development and safety (delegated to Eval_pieces)
    - Endgame techniques (delegated to Eval_endgame)
    
    Returns centipawn score (100 = one pawn advantage)
*)

open Chessml_core
open Types

(* Re-export helper functions for backward compatibility *)
let piece_kind_value = Eval_helpers.piece_kind_value
let piece_value = Eval_helpers.piece_value
let piece_square_value = Eval_helpers.piece_square_value
let piece_total_value = Eval_helpers.piece_total_value
let piece_kind_total_value = Eval_helpers.piece_kind_total_value
let count_material = Eval_helpers.count_material
let is_square_empty = Eval_helpers.is_square_empty

(* Re-export pawn structure functions for backward compatibility *)
let is_passed_pawn = Eval_pawn_structure.is_passed_pawn
let is_doubled_pawn = Eval_pawn_structure.is_doubled_pawn
let is_isolated_pawn = Eval_pawn_structure.is_isolated_pawn
let is_backward_pawn = Eval_pawn_structure.is_backward_pawn
let has_pawn_support = Eval_pawn_structure.has_pawn_support
let count_pawns_on_file = Eval_pawn_structure.count_pawns_on_file
let evaluate_pawn_structure = Eval_pawn_structure.evaluate_pawn_structure

(* Re-export piece functions for backward compatibility *)
let is_square_attacked = Eval_pieces.is_square_attacked
let is_piece_hanging = Eval_pieces.is_piece_hanging
let is_piece_en_prise pos sq = Eval_pieces.is_piece_en_prise pos sq piece_kind_value
let evaluate_piece_threats pos sq = Eval_pieces.evaluate_piece_threats pos sq piece_kind_value
let evaluate_bishop_pair = Eval_pieces.evaluate_bishop_pair
let evaluate_piece_safety = Eval_pieces.evaluate_piece_safety
let evaluate_development = Eval_pieces.evaluate_development

(* Re-export endgame functions for backward compatibility *)
let count_repetitions = Eval_endgame.count_repetitions
let evaluate_repetition_incentive = Eval_endgame.evaluate_repetition_incentive
let evaluate_fifty_move_incentive = Eval_endgame.evaluate_fifty_move_incentive

(** Main evaluation function - orchestrates all evaluation components *)
let evaluate ?(history = []) (pos : Position.t) : int =
  let side = Position.side_to_move pos in
  let opponent = Color.opponent side in
  
  (* Helper: sum positional values for a piece type bitboard *)
  let sum_position_values pieces kind color =
    let piece = { Types.color; kind } in
    let total = ref 0 in
    Bitboard.iter (fun sq -> total := !total + piece_square_value piece sq) pieces;
    !total
  in
  
  (* Count pieces (non-pawns, non-kings) for trade incentive *)
  let count_pieces color =
    Bitboard.population (Position.get_pieces pos color Knight)
    + Bitboard.population (Position.get_pieces pos color Bishop)
    + Bitboard.population (Position.get_pieces pos color Rook)
    + Bitboard.population (Position.get_pieces pos color Queen)
  in
  
  (* Material using optimized count_material from eval_helpers *)
  let our_material = count_material pos side in
  let their_material = count_material pos opponent in
  
  (* Positional bonuses - process each piece type's bitboard *)
  let our_position =
    sum_position_values (Position.get_pieces pos side Pawn) Pawn side
    + sum_position_values (Position.get_pieces pos side Knight) Knight side
    + sum_position_values (Position.get_pieces pos side Bishop) Bishop side
    + sum_position_values (Position.get_pieces pos side Rook) Rook side
    + sum_position_values (Position.get_pieces pos side Queen) Queen side
    + sum_position_values (Position.get_pieces pos side King) King side
  in
  let their_position =
    sum_position_values (Position.get_pieces pos opponent Pawn) Pawn opponent
    + sum_position_values (Position.get_pieces pos opponent Knight) Knight opponent
    + sum_position_values (Position.get_pieces pos opponent Bishop) Bishop opponent
    + sum_position_values (Position.get_pieces pos opponent Rook) Rook opponent
    + sum_position_values (Position.get_pieces pos opponent Queen) Queen opponent
    + sum_position_values (Position.get_pieces pos opponent King) King opponent
  in
  
  let our_pieces_count = count_pieces side in
  let their_pieces_count = count_pieces opponent in
  
  let material_diff = our_material - their_material in
  let position_diff = our_position - their_position in
  let total_material = our_material + their_material in
  
  (* Evaluate pawn structure *)
  let pawn_structure_bonus =
    let our_pawn_eval = evaluate_pawn_structure pos side in
    let their_pawn_eval = evaluate_pawn_structure pos opponent in
    our_pawn_eval - their_pawn_eval
  in
  
  (* Trade incentive - use already collected piece counts *)
  let trade_incentive =
    if abs material_diff > 200
    then (
      let total_pieces = our_pieces_count + their_pieces_count in
      if material_diff > 200
      then
        (* Ahead: prefer having fewer pieces total (encourage trades) *)
        -(total_pieces * 5)
      else
        (* Behind: prefer having more pieces total (avoid trades) *)
        total_pieces * 5)
    else 0
  in
  
  (* King safety and castling evaluation *)
  let king_safety_bonus =
    let our_bonus = Eval_king_safety.evaluate_king_safety pos side total_material in
    let their_penalty = Eval_king_safety.evaluate_king_safety pos opponent total_material in
    our_bonus - their_penalty
  in
  
  (* Development evaluation - encourage proper opening play *)
  let development_bonus =
    let our_dev = evaluate_development pos side in
    let their_dev = evaluate_development pos opponent in
    our_dev - their_dev
  in
  
  (* Bishop pair bonus *)
  let bishop_pair_bonus =
    let our_bishops = evaluate_bishop_pair pos side in
    let their_bishops = evaluate_bishop_pair pos opponent in
    our_bishops - their_bishops
  in
  
  (* Piece safety - penalize hanging/threatened pieces *)
  let safety_bonus =
    let our_safety = evaluate_piece_safety pos side in
    let their_safety = evaluate_piece_safety pos opponent in
    our_safety - their_safety
  in
  
  (* Repetition incentive - avoid when winning, seek when losing *)
  let repetition_incentive = evaluate_repetition_incentive pos history material_diff in
  
  (* 50-move rule incentive - avoid draws when winning *)
  let fifty_move_incentive = evaluate_fifty_move_incentive pos material_diff in
  
  (* Rook endgame evaluation - king cutoff and rook positioning *)
  let rook_endgame_bonus =
    let our_bonus = Eval_endgame.evaluate_rook_endgame pos side is_passed_pawn in
    let their_bonus = Eval_endgame.evaluate_rook_endgame pos opponent is_passed_pawn in
    our_bonus - their_bonus
  in
  
  (* Ladder mate evaluation - coordinate major pieces to push king to edge *)
  let ladder_mate_bonus =
    let our_bonus = Eval_endgame.evaluate_ladder_mate pos side piece_kind_value in
    let their_bonus = Eval_endgame.evaluate_ladder_mate pos opponent piece_kind_value in
    our_bonus - their_bonus
  in
  
  (* Return score from side-to-move perspective *)
  material_diff
  + position_diff
  + pawn_structure_bonus
  + trade_incentive
  + king_safety_bonus
  + development_bonus
  + bishop_pair_bonus
  + safety_bonus
  + repetition_incentive
  + fifty_move_incentive
  + rook_endgame_bonus
  + ladder_mate_bonus
;;
