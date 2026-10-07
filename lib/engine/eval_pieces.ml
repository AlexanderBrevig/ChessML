(** Eval_pieces - Bishop pair, development and hanging piece detection
    
    This module handles evaluation of non-pawn pieces:
    - Hanging piece detection (using SEE)
    - Bishop pair bonus
    - Development bonuses (opening phase)
*)

open Chessml_core
open Types

(** Is the piece on [sq] attacked by a capture that wins material (SEE > 0)? *)
let is_piece_hanging (pos : Position.t) (sq : Square.t) : bool =
  match Position.piece_at pos sq with
  | None -> false
  | Some piece ->
    Bitboard.fold
      (fun from_sq hanging ->
         hanging || See.evaluate pos (Move.make from_sq sq Move.Capture) > 0)
      (Movegen.compute_attackers_to pos sq (Color.opponent piece.color))
      false
;;

(** Evaluate bishop pair bonus *)
let evaluate_bishop_pair (pos : Position.t) (color : color) : int =
  let bishops = Position.get_pieces pos color Bishop in
  let bishop_count = Bitboard.population bishops in
  if bishop_count >= 2 then 50 (* Bishop pair bonus *) else 0
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
    (* Count from the pieces we still have, so a captured minor is neither a
       penalty nor a development bonus *)
    let developed_minors = ref 0 in
    let count_minor kind start_sqs =
      let start_mask = Bitboard.of_list start_sqs in
      let pieces = Position.get_pieces pos color kind in
      let undeveloped = Bitboard.population (Int64.logand pieces start_mask) in
      bonus := !bonus - (undeveloped * 25);
      developed_minors := !developed_minors + Bitboard.population pieces - undeveloped
    in
    count_minor Knight knight_start_sqs;
    count_minor Bishop bishop_start_sqs;
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
           if Movegen.is_square_attacked pos sq color then bonus := !bonus + 5
         | _ -> ())
      pieces;
    !bonus)
;;
