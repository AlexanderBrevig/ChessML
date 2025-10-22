(** Eval_king_safety - King safety and castling evaluation
    
    This module handles king-related evaluation:
    - Castling incentives (encourage timely castling)
    - Castling path detection (clear vs. blocked)
    - King safety based on castling status
    - Material-dependent castling penalties
*)

open Chessml_core
open Types

(** Helper: check if a square is empty using bitboards *)
let is_square_empty pos sq = not (Bitboard.contains (Position.occupied pos) sq)

(** Evaluate king safety and castling for a given color
    Returns bonus/penalty in centipawns *)
let evaluate_king_safety 
      (pos : Position.t) 
      (color : color)
      (total_material : int)
  : int 
  =
  let castling_rights = Position.castling_rights pos in
  let our_rights = castling_rights.(if color = White then 0 else 1) in
  (* Use cached king positions instead of searching *)
  let our_king_sq =
    if color = White then Position.white_king_sq pos else Position.black_king_sq pos
  in
  (* Check if we've already castled (king on g1/g8 or c1/c8) *)
  let has_castled =
    if color = White
    then our_king_sq = 6 || our_king_sq = 2 (* g1 or c1 *)
    else our_king_sq = 62 || our_king_sq = 58 (* g8 or c8 *)
  in
  if has_castled
  then
    (* Already castled - excellent! *)
    120
  else (
    (* Not castled yet *)
    let can_castle_short = our_rights.short <> None in
    let can_castle_long = our_rights.long <> None in
    if can_castle_short || can_castle_long
    then (
      (* Can still castle - but should we castle NOW? *)
      (* Check if the path is clear (pieces between king and rook are developed) *)
      let kingside_clear =
        if color = White
        then
          (* White kingside: f1 and g1 should be empty *)
          is_square_empty pos 5 && is_square_empty pos 6
        else
          (* Black kingside: f8 and g8 should be empty *)
          is_square_empty pos 61 && is_square_empty pos 62
      in
      let queenside_clear =
        if color = White
        then
          (* White queenside: b1, c1, d1 should be empty *)
          is_square_empty pos 1 && is_square_empty pos 2 && is_square_empty pos 3
        else
          (* Black queenside: b8, c8, d8 should be empty *)
          is_square_empty pos 57 && is_square_empty pos 58 && is_square_empty pos 59
      in
      (* If castling path is clear, give strong incentive to castle NOW! *)
      if (can_castle_short && kingside_clear) || (can_castle_long && queenside_clear)
      then
        (* Path is clear - castle ASAP! *)
        80
      else
        (* Can castle but path blocked - work on development *)
        30)
    else if
      (* Lost castling rights - penalty, especially in opening/middlegame *)
      total_material > 6000
    then (* Still in opening/middlegame *)
      -60
    else -20)
;;
