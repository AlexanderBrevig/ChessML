(** Eval_pieces - Piece safety, threats, and development evaluation
    
    This module handles evaluation of non-pawn pieces:
    - Square attack detection (simplified)
    - Piece hanging/en prise detection (using SEE)
    - Piece threat evaluation
    - Bishop pair bonus
    - Piece safety penalties
    - Development bonuses (opening phase)
*)

open Chessml_core
open Types

(** Check if a square is attacked by a given color *)
let is_square_attacked (pos : Position.t) (sq : int) (by_color : color) : bool =
  (* Simple check: look for attacking pieces *)
  (* This is a simplified version - full implementation would need movegen *)

  (* Check for pawn attacks *)
  let file = sq mod 8 in
  let rank = sq / 8 in
  let pawn_attack_squares =
    if by_color = White
    then
      (* White pawns attack diagonally upward *)
      (if file > 0 && rank > 0 then [ sq - 9 ] else [])
      @ if file < 7 && rank > 0 then [ sq - 7 ] else []
    else
      (* Black pawns attack diagonally downward *)
      (if file > 0 && rank < 7 then [ sq + 7 ] else [])
      @ if file < 7 && rank < 7 then [ sq + 9 ] else []
  in
  List.exists
    (fun attack_sq ->
       match Position.piece_at pos attack_sq with
       | Some p when p.color = by_color && p.kind = Pawn -> true
       | _ -> false)
    pawn_attack_squares
  ||
  (* Check for knight attacks *)
  let knight_offsets = [ -17; -15; -10; -6; 6; 10; 15; 17 ] in
  List.exists
    (fun offset ->
       let target_sq = sq + offset in
       if target_sq >= 0 && target_sq < 64
       then (
         (* Check if move is valid (not wrapping around board) *)
         let file_diff = abs ((target_sq mod 8) - file) in
         let rank_diff = abs ((target_sq / 8) - rank) in
         if (file_diff = 2 && rank_diff = 1) || (file_diff = 1 && rank_diff = 2)
         then (
           match Position.piece_at pos target_sq with
           | Some p when p.color = by_color && p.kind = Knight -> true
           | _ -> false)
         else false)
       else false)
    knight_offsets
;;

(** Check if a piece is hanging (can be captured with material gain) using proper SEE *)
let is_piece_hanging (pos : Position.t) (sq : Square.t) : bool =
  match Position.piece_at pos sq with
  | None -> false
  | Some piece ->
    let opponent = Color.opponent piece.color in
    let opponent_pieces = Position.get_color_pieces pos opponent in
    let occupied = Position.occupied pos in
    let has_winning_capture = ref false in
    (* Check if any opponent piece can capture with a winning exchange using SEE *)
    Bitboard.iter
      (fun from_sq ->
         if not !has_winning_capture
         then (
           match Position.piece_at pos from_sq with
           | Some attacker when attacker.color = opponent ->
             let attacks = Movegen.attacks_for attacker from_sq occupied in
             if Bitboard.contains attacks sq
             then (
               (* Create a capture move and evaluate using proper SEE *)
               let capture_move = Move.make from_sq sq Move.Capture in
               let see_value = See.evaluate pos capture_move in
               (* Positive SEE means attacker wins material - piece is hanging *)
               if see_value > 0 then has_winning_capture := true)
           | _ -> ()))
      opponent_pieces;
    !has_winning_capture
;;

(** Check if piece is en prise (can be captured with immediate material gain) *)
let is_piece_en_prise 
      (pos : Position.t) 
      (sq : Square.t)
      (piece_kind_value : piece_kind -> int)
  : bool 
  =
  match Position.piece_at pos sq with
  | None -> false
  | Some piece ->
    if piece.kind = Pawn || piece.kind = King
    then false (* Don't check pawns/king with this function *)
    else (
      (* Check if attacked by cheaper piece - simplified but effective *)
      let piece_value = piece_kind_value piece.kind in
      let opponent = Color.opponent piece.color in
      let occupied = Position.occupied pos in
      let min_attacker_value = ref 10000 in
      let has_attacker = ref false in
      let opponent_pieces = Position.get_color_pieces pos opponent in
      Bitboard.iter
        (fun attacker_sq ->
           match Position.piece_at pos attacker_sq with
           | Some attacker when attacker.color = opponent ->
             let attacks = Movegen.attacks_for attacker attacker_sq occupied in
             if Bitboard.contains attacks sq
             then (
               has_attacker := true;
               let attacker_val = piece_kind_value attacker.kind in
               if attacker_val < !min_attacker_value
               then min_attacker_value := attacker_val)
           | _ -> ())
        opponent_pieces;
      (* En prise if attacked by significantly cheaper piece (at least pawn value difference) *)
      !has_attacker && !min_attacker_value + 100 < piece_value)
;;

(** Evaluate threats to a specific piece - returns negative if piece is at risk *)
let evaluate_piece_threats 
      (pos : Position.t) 
      (sq : Square.t)
      (piece_kind_value : piece_kind -> int)
  : int 
  =
  match Position.piece_at pos sq with
  | None -> 0
  | Some piece ->
    let piece_value = piece_kind_value piece.kind in
    let opponent = Color.opponent piece.color in
    let occupied = Position.occupied pos in
    let min_attacker_value = ref 10000 in
    let has_attacker = ref false in
    let opponent_pieces = Position.get_color_pieces pos opponent in
    Bitboard.iter
      (fun attacker_sq ->
         match Position.piece_at pos attacker_sq with
         | Some attacker when attacker.color = opponent ->
           let attacks = Movegen.attacks_for attacker attacker_sq occupied in
           if Bitboard.contains attacks sq
           then (
             has_attacker := true;
             let attacker_val = piece_kind_value attacker.kind in
             if attacker_val < !min_attacker_value
             then min_attacker_value := attacker_val)
         | _ -> ())
      opponent_pieces;
    (* Return the risk differential - negative if under attack by cheaper piece *)
    if !has_attacker && !min_attacker_value < piece_value
    then !min_attacker_value - piece_value
    else 0
;;

(** Evaluate bishop pair bonus *)
let evaluate_bishop_pair (pos : Position.t) (color : color) : int =
  let bishops = Position.get_pieces pos color Bishop in
  let bishop_count = Bitboard.population bishops in
  if bishop_count >= 2 then 50 (* Bishop pair bonus *) else 0
;;

(** Evaluate piece safety - penalize hanging pieces using proper SEE *)
let evaluate_piece_safety (pos : Position.t) (color : color) : int =
  let penalty = ref 0 in
  (* Check all our pieces if they're hanging *)
  let pieces = Position.get_color_pieces pos color in
  Bitboard.iter
    (fun sq ->
       match Position.piece_at pos sq with
       | Some piece when piece.color = color && piece.kind <> Pawn && piece.kind <> King
         ->
         (* Use the is_piece_hanging function which already does SEE *)
         if is_piece_hanging pos sq
         then (
           let piece_val = Piece_tables.piece_total_value piece sq in
           (* Penalize at full value - the piece is at risk of being lost *)
           penalty := !penalty - piece_val)
       | _ -> ())
    pieces;
  !penalty
;;

(** Evaluate piece development and protection for a color *)
let evaluate_development (pos : Position.t) (color : color) : int =
  let fullmove = Position.fullmove pos in
  let in_opening = fullmove <= 15 in
  (* First ~15 moves *)
  if not in_opening
  then 0
  else (
    let bonus = ref 0 in
    (* Starting squares for pieces *)
    let knight_start_sqs, bishop_start_sqs, queen_start_sq, rook_start_sqs =
      if color = White
      then [ 1; 6 ], [ 2; 5 ], 3, [ 0; 7 ]
      else [ 57; 62 ], [ 58; 61 ], 59, [ 56; 63 ]
    in
    let developed_minors = ref 0 in
    (* Check knight development *)
    List.iter
      (fun sq ->
         match Position.piece_at pos sq with
         | Some p when p.kind = Knight && p.color = color ->
           (* Knight still on starting square - penalty *)
           bonus := !bonus - 25
         | None | Some _ ->
           (* Knight has moved - good! *)
           incr developed_minors)
      knight_start_sqs;
    (* Check bishop development *)
    List.iter
      (fun sq ->
         match Position.piece_at pos sq with
         | Some p when p.kind = Bishop && p.color = color ->
           (* Bishop still on starting square - penalty *)
           bonus := !bonus - 25
         | None | Some _ ->
           (* Bishop has moved - good! *)
           incr developed_minors)
      bishop_start_sqs;
    (* Check if queen moved too early (before minors developed) *)
    let queen_on_start =
      match Position.piece_at pos queen_start_sq with
      | Some p when p.kind = Queen && p.color = color -> true
      | _ -> false
    in
    if (not queen_on_start) && !developed_minors < 2
    then
      (* Queen moved before developing at least 2 minor pieces - bad! *)
      bonus := !bonus - 40;
    (* Reward having developed pieces *)
    bonus := !bonus + (!developed_minors * 15);
    (* Check for connected rooks (both castled or on back rank with no pieces between) *)
    let rooks_connected =
      let rook_positions = ref [] in
      List.iter
        (fun sq ->
           match Position.piece_at pos sq with
           | Some p when p.kind = Rook && p.color = color ->
             rook_positions := sq :: !rook_positions
           | _ -> ())
        rook_start_sqs;
      (* Also check other squares for rooks using rook bitboard *)
      let rooks = Position.get_pieces pos color Rook in
      Bitboard.iter
        (fun sq ->
           if not (List.mem sq !rook_positions)
           then rook_positions := sq :: !rook_positions)
        rooks;
      (* Check if rooks can "see" each other (same rank, no pieces between) *)
      match !rook_positions with
      | [ sq1; sq2 ] ->
        let rank1 = sq1 / 8 in
        let rank2 = sq2 / 8 in
        if rank1 = rank2
        then (
          let file1 = sq1 mod 8 in
          let file2 = sq2 mod 8 in
          let min_file = min file1 file2 in
          let max_file = max file1 file2 in
          let blocked = ref false in
          for f = min_file + 1 to max_file - 1 do
            let check_sq = f + (rank1 * 8) in
            match Position.piece_at pos check_sq with
            | Some _ -> blocked := true
            | None -> ()
          done;
          not !blocked)
        else false
      | _ -> false
    in
    if rooks_connected then bonus := !bonus + 40;
    (* Check piece protection - pieces defended by pawns or other pieces *)
    let pieces = Position.get_color_pieces pos color in
    Bitboard.iter
      (fun sq ->
         match Position.piece_at pos sq with
         | Some piece when piece.kind <> Pawn && piece.kind <> King ->
           (* Check if this piece is protected *)
           if is_square_attacked pos sq color then bonus := !bonus + 5
         | _ -> ())
      pieces;
    !bonus)
;;
