(** Eval - Static position evaluation function
    
    Evaluates chess positions from the side-to-move perspective using multiple factors:
    - Material balance (piece values)
    - Piece-square tables (positional bonuses)
    - Pawn structure (passed, doubled, isolated pawns)
    - King safety and castling incentives
    - Piece development (opening phase)
    - Trade incentives (when ahead/behind in material)
    
    Returns centipawn score (100 = one pawn advantage)
*)

open Chessml_core
open Types

(** Helper: check if a square is empty using bitboards *)
let is_square_empty pos sq = not (Bitboard.contains (Position.occupied pos) sq)

(** Get pawn bitboard for a color *)
let get_pawns pos color = Position.get_pieces pos color Pawn

(** Check if there's a pawn of given color on a specific square *)
let has_pawn_on pos color sq = Bitboard.contains (get_pawns pos color) sq

(** Count pawns on a file for a color using bitboards *)
let count_pawns_on_file_bb pos file_int color =
  let pawns = get_pawns pos color in
  (* Create file mask by setting all squares on that file *)
  let file_mask = ref Bitboard.empty in
  for rank = 0 to 7 do
    file_mask := Bitboard.set !file_mask (file_int + (rank * 8))
  done;
  Bitboard.population (Int64.logand pawns !file_mask)
;;

(** Piece values in centipawns (by kind) - delegate to Types module *)
let piece_kind_value (kind : piece_kind) : int = PieceKind.value kind

(** Get piece value (convenience wrapper) *)
let piece_value (piece : piece) : int = piece_kind_value piece.kind

(** Piece-square tables for positional evaluation (from white's perspective) *)

(** Values are in centipawns, bonuses for good squares *)

let pawn_table =
  [| 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 50
   ; 50
   ; 50
   ; 50
   ; 50
   ; 50
   ; 50
   ; 50
   ; 10
   ; 10
   ; 20
   ; 30
   ; 30
   ; 20
   ; 10
   ; 10
   ; 5
   ; 5
   ; 10
   ; 25
   ; 25
   ; 10
   ; 5
   ; 5
   ; 0
   ; 0
   ; 0
   ; 20
   ; 20
   ; 0
   ; 0
   ; 0
   ; 5
   ; -5
   ; -10
   ; 0
   ; 0
   ; -10
   ; -5
   ; 5
   ; 5
   ; 10
   ; 10
   ; -20
   ; -20
   ; 10
   ; 10
   ; 5
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
  |]
;;

let knight_table =
  [| -50
   ; -40
   ; -30
   ; -30
   ; -30
   ; -30
   ; -40
   ; -50
   ; -40
   ; -20
   ; 0
   ; 0
   ; 0
   ; 0
   ; -20
   ; -40
   ; -30
   ; 0
   ; 10
   ; 15
   ; 15
   ; 10
   ; 0
   ; -30
   ; -30
   ; 5
   ; 15
   ; 20
   ; 20
   ; 15
   ; 5
   ; -30
   ; -30
   ; 0
   ; 15
   ; 20
   ; 20
   ; 15
   ; 0
   ; -30
   ; -30
   ; 5
   ; 10
   ; 15
   ; 15
   ; 10
   ; 5
   ; -30
   ; -40
   ; -20
   ; 0
   ; 5
   ; 5
   ; 0
   ; -20
   ; -40
   ; -50
   ; -40
   ; -30
   ; -30
   ; -30
   ; -30
   ; -40
   ; -50
  |]
;;

let bishop_table =
  [| -20
   ; -10
   ; -10
   ; -10
   ; -10
   ; -10
   ; -10
   ; -20
   ; -10
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; -10
   ; -10
   ; 0
   ; 5
   ; 10
   ; 10
   ; 5
   ; 0
   ; -10
   ; -10
   ; 5
   ; 5
   ; 10
   ; 10
   ; 5
   ; 5
   ; -10
   ; -10
   ; 0
   ; 10
   ; 10
   ; 10
   ; 10
   ; 0
   ; -10
   ; -10
   ; 10
   ; 10
   ; 10
   ; 10
   ; 10
   ; 10
   ; -10
   ; -10
   ; 5
   ; 0
   ; 0
   ; 0
   ; 0
   ; 5
   ; -10
   ; -20
   ; -10
   ; -10
   ; -10
   ; -10
   ; -10
   ; -10
   ; -20
  |]
;;

let rook_table =
  [| 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 5
   ; 10
   ; 10
   ; 10
   ; 10
   ; 10
   ; 10
   ; 5
   ; -5
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; -5
   ; -5
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; -5
   ; -5
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; -5
   ; -5
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; -5
   ; -5
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; -5
   ; 0
   ; 0
   ; 0
   ; 5
   ; 5
   ; 0
   ; 0
   ; 0
  |]
;;

let queen_table =
  [| -20
   ; -10
   ; -10
   ; -5
   ; -5
   ; -10
   ; -10
   ; -20
   ; -10
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; 0
   ; -10
   ; -10
   ; 0
   ; 5
   ; 5
   ; 5
   ; 5
   ; 0
   ; -10
   ; -5
   ; 0
   ; 5
   ; 5
   ; 5
   ; 5
   ; 0
   ; -5
   ; 0
   ; 0
   ; 5
   ; 5
   ; 5
   ; 5
   ; 0
   ; -5
   ; -10
   ; 5
   ; 5
   ; 5
   ; 5
   ; 5
   ; 0
   ; -10
   ; -10
   ; 0
   ; 5
   ; 0
   ; 0
   ; 0
   ; 0
   ; -10
   ; -20
   ; -10
   ; -10
   ; -5
   ; -5
   ; -10
   ; -10
   ; -20
  |]
;;

let king_middlegame_table =
  [| -30
   ; -40
   ; -40
   ; -50
   ; -50
   ; -40
   ; -40
   ; -30
   ; -30
   ; -40
   ; -40
   ; -50
   ; -50
   ; -40
   ; -40
   ; -30
   ; -30
   ; -40
   ; -40
   ; -50
   ; -50
   ; -40
   ; -40
   ; -30
   ; -30
   ; -40
   ; -40
   ; -50
   ; -50
   ; -40
   ; -40
   ; -30
   ; -20
   ; -30
   ; -30
   ; -40
   ; -40
   ; -30
   ; -30
   ; -20
   ; -10
   ; -20
   ; -20
   ; -20
   ; -20
   ; -20
   ; -20
   ; -10
   ; 20
   ; 20
   ; 0
   ; 0
   ; 0
   ; 0
   ; 20
   ; 20
   ; 20
   ; 30
   ; 10
   ; 0
   ; 0
   ; 10
   ; 30
   ; 20
  |]
;;

(** Get piece-square table value for a piece at a square *)
let piece_square_value (piece : piece) (sq : int) : int =
  Piece_tables.piece_square_value piece sq
;;

(** Get total value of a piece on a square: material + positional bonus 
    This gives the true value of the piece considering its position. *)
let piece_total_value (piece : piece) (sq : int) : int =
  Piece_tables.piece_total_value piece sq
;;

(** Get total value for a piece kind on a square (requires color for PST lookup) *)
let piece_kind_total_value (kind : piece_kind) (color : color) (sq : int) : int =
  Piece_tables.piece_kind_total_value kind color sq
;;

(** Count material for a given color *)
let count_material (pos : Position.t) (color : color) : int =
  let material = ref 0 in
  let pieces = Position.get_color_pieces pos color in
  Bitboard.iter
    (fun sq ->
       match Position.piece_at pos sq with
       | Some piece -> material := !material + piece_value piece
       | None -> ())
    pieces;
  !material
;;

(** Pawn structure evaluation helpers *)

(** Check if a pawn is passed (no enemy pawns can stop it) *)
let is_passed_pawn (pos : Position.t) (sq : int) (color : color) : bool =
  let file = sq mod 8 in
  let rank = sq / 8 in
  let opponent = Color.opponent color in
  (* Check files: current, left, right *)
  let files_to_check =
    [ file ]
    @ (if file > 0 then [ file - 1 ] else [])
    @ if file < 7 then [ file + 1 ] else []
  in
  (* For white, check ranks ahead (rank+1 to 7) *)
  (* For black, check ranks ahead (0 to rank-1) *)
  let no_blockers = ref true in
  List.iter
    (fun f ->
       let start_rank, end_rank = if color = White then rank + 1, 7 else 0, rank - 1 in
       let min_r = min start_rank end_rank in
       let max_r = max start_rank end_rank in
       for r = min_r to max_r do
         let check_sq = f + (r * 8) in
         match Position.piece_at pos check_sq with
         | Some p when p.color = opponent && p.kind = Pawn -> no_blockers := false
         | _ -> ()
       done)
    files_to_check;
  !no_blockers
;;

(** Check if a pawn is doubled (another friendly pawn on same file) *)
let is_doubled_pawn (pos : Position.t) (sq : int) (color : color) : bool =
  let file = sq mod 8 in
  count_pawns_on_file_bb pos file color > 1
;;

(** Count pawns on a file for a color *)
let count_pawns_on_file (pos : Position.t) (file : int) (color : color) : int =
  count_pawns_on_file_bb pos file color
;;

(** Check if a pawn is isolated (no friendly pawns on adjacent files) *)
let is_isolated_pawn (pos : Position.t) (sq : int) (color : color) : bool =
  let file = sq mod 8 in
  let has_support =
    (file > 0 && count_pawns_on_file_bb pos (file - 1) color > 0)
    || (file < 7 && count_pawns_on_file_bb pos (file + 1) color > 0)
  in
  not has_support
;;

(** Check if a pawn is backward (can't safely advance, no support) *)
let is_backward_pawn (pos : Position.t) (sq : int) (color : color) : bool =
  let file = sq mod 8 in
  let rank = sq / 8 in
  (* Check if pawn can be defended by friendly pawns *)
  let has_defender = ref false in
  let files_to_check =
    (if file > 0 then [ file - 1 ] else []) @ if file < 7 then [ file + 1 ] else []
  in
  List.iter
    (fun f ->
       for r = 0 to 7 do
         let check_sq = f + (r * 8) in
         match Position.piece_at pos check_sq with
         | Some p when p.color = color && p.kind = Pawn ->
           (* Check if this pawn is behind or at same level *)
           let check_rank = check_sq / 8 in
           if
             (color = White && check_rank <= rank) || (color = Black && check_rank >= rank)
           then has_defender := true
         | _ -> ()
       done)
    files_to_check;
  not !has_defender
;;

(** Check if pawn has friendly pawn support (connected or chain) *)
let has_pawn_support (pos : Position.t) (sq : int) (color : color) : bool =
  let file = sq mod 8 in
  let rank = sq / 8 in
  (* Check for pawns on adjacent files at same or supporting rank *)
  let files_to_check =
    (if file > 0 then [ file - 1 ] else []) @ if file < 7 then [ file + 1 ] else []
  in
  List.exists
    (fun f ->
       (* Check same rank (connected) or one rank behind (supporting) *)
       let ranks_to_check =
         [ rank ]
         @ (if color = White && rank > 0 then [ rank - 1 ] else [])
         @ if color = Black && rank < 7 then [ rank + 1 ] else []
       in
       List.exists
         (fun r ->
            let check_sq = f + (r * 8) in
            match Position.piece_at pos check_sq with
            | Some p when p.color = color && p.kind = Pawn -> true
            | _ -> false)
         ranks_to_check)
    files_to_check
;;

(** Evaluate pawn structure for a given color *)
let evaluate_pawn_structure (pos : Position.t) (color : color) : int =
  let bonus = ref 0 in
  let pawns = get_pawns pos color in
  Bitboard.iter
    (fun sq ->
       let file = sq mod 8 in
       let rank = sq / 8 in
       let actual_rank = if color = White then rank else 7 - rank in
       (* Passed pawn bonus (increases with rank advancement) *)
       if is_passed_pawn pos sq color
       then (
         let passed_bonus =
           match actual_rank with
           | 0 | 1 -> 0 (* Not passed yet or just started *)
           | 2 -> 15 (* 3rd rank *)
           | 3 -> 30 (* 4th rank *)
           | 4 -> 60 (* 5th rank - doubled *)
           | 5 -> 150 (* 6th rank - doubled, very strong incentive *)
           | 6 -> 300 (* 7th rank - tripled, must push! *)
           | 7 -> 800 (* About to promote! - massive bonus *)
           | _ -> 0
         in
         bonus := !bonus + passed_bonus;
         (* Extra bonus if protected *)
         if has_pawn_support pos sq color then bonus := !bonus + 20);
       (* Doubled pawn penalty - apply only once per file *)
       if is_doubled_pawn pos sq color
       then (
         let pawns_on_file = count_pawns_on_file pos file color in
         (* Check if this is the first pawn we're processing on this file *)
         (* to avoid counting penalty multiple times *)
         let is_front_pawn =
           let front_rank = ref (if color = White then -1 else 8) in
           for r = 0 to 7 do
             let check_sq = file + (r * 8) in
             match Position.piece_at pos check_sq with
             | Some p when p.color = color && p.kind = Pawn ->
               if color = White
               then (if r > !front_rank then front_rank := r)
               else if r < !front_rank
               then front_rank := r
             | _ -> ()
           done;
           rank = !front_rank
         in
         (* Only apply penalty to the most advanced pawn *)
         if is_front_pawn
         then bonus := !bonus - ((pawns_on_file - 1) * 20 (* Increased penalty *)));
       (* Isolated pawn penalty *)
       if is_isolated_pawn pos sq color then bonus := !bonus - 20;
       (* Backward pawn penalty *)
       if is_backward_pawn pos sq color then bonus := !bonus - 10;
       (* Connected/supported pawns bonus *)
       if has_pawn_support pos sq color then bonus := !bonus + 5;
       (* Central pawns bonus (d and e files) *)
       if file = 3 || file = 4 then bonus := !bonus + 5)
    pawns;
  !bonus
;;

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
let is_piece_en_prise (pos : Position.t) (sq : Square.t) : bool =
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
let evaluate_piece_threats (pos : Position.t) (sq : Square.t) : int =
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

(** Check if position is approaching repetition by looking at history 
    Returns: 0 = no repetition, 1 = position seen once, 2+ = multiple repetitions *)
let count_repetitions (pos_key : int64) (history : int64 list) : int =
  let rec count key hist skip_next =
    match hist with
    | [] -> 0
    | h :: rest ->
      if skip_next
      then count key rest false (* Skip every other position - different side to move *)
      else if h = key
      then 1 + count key rest true
      else count key rest true
  in
  count pos_key history true
;;

(** Evaluate repetition bonus/penalty based on material situation 
    Returns adjustment in centipawns *)
let evaluate_repetition_incentive
      (pos : Position.t)
      (history : int64 list)
      (material_diff : int)
  : int
  =
  let pos_key = Zobrist.compute pos in
  let repetition_count = count_repetitions pos_key history in
  if repetition_count = 0
  then 0
  else (
    (* Position has occurred before - apply incentive based on material situation *)
    let base_penalty = 150 in
    (* Larger penalty/bonus for repetitions *)
    let scaling_factor = if repetition_count >= 2 then 2 else 1 in
    (* Stronger for 2nd repetition *)
    if material_diff > 200
    then
      (* We're winning - strongly avoid repetition *)
      -(base_penalty * scaling_factor)
    else if material_diff < -200
    then
      (* We're losing - seek repetition for a draw *)
      base_penalty * scaling_factor
    else if material_diff > 50
    then
      (* Slightly ahead - discourage repetition *)
      -(base_penalty / 2 * scaling_factor)
    else if material_diff < -50
    then
      (* Slightly behind - encourage repetition *)
      base_penalty / 2 * scaling_factor
    else
      (* Equal position - small penalty to avoid repetition *)
      -(base_penalty / 4 * scaling_factor))
;;

(** Evaluate rook endgames with passed pawns - king cutoff is critical!
    In rook+pawn vs rook or rook+pawn vs king endgames, the key is:
    1. Use rook to cut off enemy king from the pawn
    2. Keep rook behind passed pawn (or on the same file)
    3. Advanced passed pawns with rook support should be heavily rewarded *)
let evaluate_rook_endgame (pos : Position.t) (color : color) : int =
  let opponent = Color.opponent color in
  
  (* Count pieces to determine if this is a rook endgame *)
  let our_rooks = Bitboard.population (Position.get_pieces pos color Rook) in
  let their_pawns = Bitboard.population (Position.get_pieces pos opponent Pawn) in
  
  (* Only evaluate if it's a rook endgame (no queens, bishops, or knights) *)
  let total_queens = 
    Bitboard.population (Position.get_pieces pos color Queen) +
    Bitboard.population (Position.get_pieces pos opponent Queen) in
  let total_bishops =
    Bitboard.population (Position.get_pieces pos color Bishop) +
    Bitboard.population (Position.get_pieces pos opponent Bishop) in
  let total_knights =
    Bitboard.population (Position.get_pieces pos color Knight) +
    Bitboard.population (Position.get_pieces pos opponent Knight) in
  
  if total_queens > 0 || total_bishops > 0 || total_knights > 0
  then 0 (* Not a pure rook endgame *)
  else if our_rooks = 0
  then 0 (* We don't have a rook *)
  else (
    let bonus = ref 0 in
    
    (* Find our passed pawns *)
    let our_pawn_bb = Position.get_pieces pos color Pawn in
    let our_pawn_squares = Bitboard.to_list our_pawn_bb in
    
    (* Find enemy king square *)
    let enemy_king_sq = 
      if opponent = White 
      then Position.white_king_sq pos 
      else Position.black_king_sq pos in
    let enemy_king_file = Square.file enemy_king_sq |> File.to_int in
    let enemy_king_rank = Square.rank enemy_king_sq |> Rank.to_int in
    
    (* Find our rook *)
    let our_rook_bb = Position.get_pieces pos color Rook in
    let our_rook_squares = Bitboard.to_list our_rook_bb in
    
    List.iter (fun pawn_sq ->
      if is_passed_pawn pos pawn_sq color then (
        let pawn_file = Square.file pawn_sq |> File.to_int in
        let pawn_rank = Square.rank pawn_sq |> Rank.to_int in
        let relative_rank = if color = White then pawn_rank else 7 - pawn_rank in
        
        (* Check if our rook cuts off the enemy king from the pawn *)
        List.iter (fun rook_sq ->
          let rook_file = Square.file rook_sq |> File.to_int in
          let rook_rank = Square.rank rook_sq |> Rank.to_int in
          
          (* King cutoff: Rook is between pawn and enemy king on a file or rank *)
          let is_cutting_off_file = 
            (* Rook and pawn on different files, rook file is between king file and pawn file *)
            if pawn_file < enemy_king_file then
              rook_file > pawn_file && rook_file < enemy_king_file
            else if pawn_file > enemy_king_file then
              rook_file < pawn_file && rook_file > enemy_king_file
            else false in
          
          let is_cutting_off_rank =
            (* Similar logic for ranks *)
            if color = White then
              (* White wants to prevent black king from coming down *)
              rook_rank > pawn_rank && enemy_king_rank > rook_rank
            else
              (* Black wants to prevent white king from coming up *)
              rook_rank < pawn_rank && enemy_king_rank < rook_rank in
          
          if is_cutting_off_file || is_cutting_off_rank then (
            (* HUGE bonus for cutting off the king! *)
            bonus := !bonus + 150 + (relative_rank * 20);
            
            (* Extra bonus if pawn is far advanced *)
            if relative_rank >= 5 then
              bonus := !bonus + 200
          );
          
          (* Bonus for rook behind the passed pawn (classic technique) *)
          let rook_behind_pawn =
            rook_file = pawn_file && 
            ((color = White && rook_rank < pawn_rank) ||
             (color = Black && rook_rank > pawn_rank)) in
          
          if rook_behind_pawn then
            bonus := !bonus + 50 + (relative_rank * 10);
          
        ) our_rook_squares;
        
        (* Additional bonus if enemy has no pawns (easier to win) *)
        if their_pawns = 0 then
          bonus := !bonus + 100;
      )
    ) our_pawn_squares;
    
    !bonus
  )
;;

(** Evaluate ladder mate technique with two major pieces (rooks or queen+rook)
    Ladder mate pushes enemy king to edge by coordinating major pieces on adjacent ranks/files.
    Key patterns:
    1. Two rooks on adjacent ranks/files cutting off king
    2. Queen + rook coordinated similarly
    3. Enemy king distance from center (being pushed to edge)
    4. Our pieces maintaining safe distance from enemy king *)
let evaluate_ladder_mate (pos : Position.t) (color : color) : int =
  let opponent = Color.opponent color in
  
  (* Count major pieces *)
  let our_rooks = Position.get_pieces pos color Rook in
  let our_queens = Position.get_pieces pos color Queen in
  let our_rook_count = Bitboard.population our_rooks in
  let our_queen_count = Bitboard.population our_queens in
  
  (* Only evaluate if we have 2+ major pieces (2 rooks, or queen+rook, or 2 queens) *)
  let major_piece_count = our_rook_count + our_queen_count in
  if major_piece_count < 2
  then 0
  else (
    (* Check if opponent has few/no pieces (mating scenario) *)
    let their_material = ref 0 in
    let their_pieces = Position.get_color_pieces pos opponent in
    Bitboard.iter
      (fun sq ->
         match Position.piece_at pos sq with
         | Some piece when piece.color = opponent && piece.kind <> King ->
           their_material := !their_material + piece_kind_value piece.kind
         | _ -> ())
      their_pieces;
    
    (* Only apply ladder mate bonus if opponent is weak (< 500cp material, basically lone king or king+minor) *)
    if !their_material > 500
    then 0
    else (
      let bonus = ref 0 in
      
      (* Find enemy king *)
      let enemy_king_sq = 
        if opponent = White 
        then Position.white_king_sq pos 
        else Position.black_king_sq pos in
      let enemy_king_file = Square.file enemy_king_sq |> File.to_int in
      let enemy_king_rank = Square.rank enemy_king_sq |> Rank.to_int in
      
      (* Bonus for enemy king distance from center (being pushed to edge) *)
      let center_file = 3.5 in (* Between d and e files *)
      let center_rank = 3.5 in (* Between 4th and 5th ranks *)
      let file_dist = abs_float (float_of_int enemy_king_file -. center_file) in
      let rank_dist = abs_float (float_of_int enemy_king_rank -. center_rank) in
      let edge_distance = max file_dist rank_dist in
      (* Reward king being far from center (toward edges) *)
      bonus := !bonus + (int_of_float (edge_distance *. 40.0));
      
      (* Bonus if king is on edge (file 0, 7 or rank 0, 7) *)
      if enemy_king_file = 0 || enemy_king_file = 7 || enemy_king_rank = 0 || enemy_king_rank = 7
      then bonus := !bonus + 150; (* Getting close to mate! *)
      
      (* Extra bonus if king is in corner *)
      if (enemy_king_file = 0 || enemy_king_file = 7) 
         && (enemy_king_rank = 0 || enemy_king_rank = 7)
      then bonus := !bonus + 200; (* Corner = checkmate territory *)
      
      (* Note: We don't actively encourage king movement - K+R+R vs K is winnable
         without king help in most positions. The rooks do the work via ladder mate. *)
      
      (* Collect our major piece positions *)
      let major_piece_sqs = ref [] in
      let combined_major_pieces = Int64.logor our_rooks our_queens in
      Bitboard.iter
        (fun sq -> major_piece_sqs := sq :: !major_piece_sqs)
        combined_major_pieces;
      
      (* Check for coordinated pieces on DIFFERENT ranks AND different files *)
      let pieces = !major_piece_sqs in
      (match pieces with
      | sq1 :: sq2 :: _ ->
        let file1 = Square.file sq1 |> File.to_int in
        let rank1 = Square.rank sq1 |> Rank.to_int in
        let file2 = Square.file sq2 |> File.to_int in
        let rank2 = Square.rank sq2 |> Rank.to_int in
        
        (* Calculate differences *)
        let rank_diff = abs (rank1 - rank2) in
        let file_diff = abs (file1 - file2) in
        
        (* Check if pieces are on same rank *)
        let same_rank = rank1 = rank2 in
        (* Check if pieces are on same file *)
        let same_file = file1 = file2 in
        
        (* CRITICAL: For proper ladder mate, pieces must be ADJACENT (exactly 1 apart)
           on either rank OR file, but NOT both at once, and NOT same rank/file.
           This prevents "jumping over" the king. *)
        let proper_ladder_formation =
          (not same_rank) && (not same_file) &&  (* Different rank AND file *)
          ((rank_diff = 1 && file_diff = 1) ||    (* Diagonal adjacent *)
           (rank_diff = 1 && file_diff <= 2) ||   (* Close on rank, near on file *)
           (file_diff = 1 && rank_diff <= 2))     (* Close on file, near on rank *)
        in
        
        if proper_ladder_formation then (
          (* EXCELLENT! Proper adjacent ladder formation *)
          bonus := !bonus + 250;
          
          (* Extra bonus if EXACTLY adjacent diagonally (knight's move away or closer) *)
          if rank_diff = 1 && file_diff = 1 then
            bonus := !bonus + 100; (* Perfect diagonal adjacency! *)
          
          (* Check if pieces are cutting off king's escape *)
          let piece1_cuts_king_file = abs (file1 - enemy_king_file) = 1 in
          let piece2_cuts_king_file = abs (file2 - enemy_king_file) = 1 in
          let piece1_cuts_king_rank = abs (rank1 - enemy_king_rank) = 1 in
          let piece2_cuts_king_rank = abs (rank2 - enemy_king_rank) = 1 in
          
          if (piece1_cuts_king_file || piece2_cuts_king_file) &&
             (piece1_cuts_king_rank || piece2_cuts_king_rank) then
            bonus := !bonus + 150; (* Perfect trap - cutting off escape squares! *)
        ) else (
          (* Penalize if not in proper ladder formation *)
          if same_rank then
            bonus := !bonus - 200 (* Very bad - same rank allows king to escape *)
          else if same_file then
            bonus := !bonus - 200 (* Very bad - same file allows king to escape *)
          else if rank_diff > 2 || file_diff > 2 then
            bonus := !bonus - 150 (* Too far apart - not coordinating properly *)
          else
            () (* Other formations - neutral *)
        );
        
        (* Bonus for keeping pieces safe from enemy king *)
        List.iter (fun sq ->
          let file = Square.file sq |> File.to_int in
          let rank = Square.rank sq |> Rank.to_int in
          let file_dist = abs (file - enemy_king_file) in
          let rank_dist = abs (rank - enemy_king_rank) in
          let king_distance = max file_dist rank_dist in
          (* Penalize if too close to enemy king (can be captured) *)
          if king_distance = 1 then
            bonus := !bonus - 150 (* HUGE danger! King can capture *)
          else if king_distance = 2 then
            bonus := !bonus + 30 (* Good safe distance *)
          else if king_distance >= 3 then
            bonus := !bonus + 10 (* Safe *)
        ) pieces;
        
      | _ -> ());
      
      !bonus
    )
  )
;;

(** Evaluate 50-move rule incentive - avoid draws when winning
    Returns penalty in centipawns based on halfmove clock and material situation *)
let evaluate_fifty_move_incentive (pos : Position.t) (material_diff : int) : int =
  let halfmove_clock = Position.halfmove pos in
  (* Only apply penalty when clock is getting dangerous (>= 60) *)
  if halfmove_clock < 60
  then 0
  else (
    (* Calculate how close we are to the 50-move draw (100 halfmoves) *)
    let moves_until_draw = 100 - halfmove_clock in
    (* Base penalty increases as we approach the draw *)
    let base_penalty =
      if moves_until_draw <= 5
      then 500 (* Critical! Only 5 halfmoves left *)
      else if moves_until_draw <= 10
      then 300 (* Very dangerous, 10 halfmoves left *)
      else if moves_until_draw <= 20
      then 150 (* Getting close, 20 halfmoves left *)
      else 75 (* Somewhat close, 40 halfmoves left *)
    in
    (* Scale penalty by material advantage - stronger penalty when winning *)
    if material_diff > 300
    then
      (* Winning significantly - STRONGLY avoid the draw *)
      -(base_penalty * 3)
    else if material_diff > 100
    then
      (* Winning - avoid the draw *)
      -(base_penalty * 2)
    else if material_diff < -300
    then
      (* Losing significantly - actually WANT the draw! *)
      base_penalty * 2
    else if material_diff < -100
    then
      (* Losing - draw is better than losing *)
      base_penalty
    else
      (* Close to equal - mild penalty to avoid draw by repetition *)
      -base_penalty)
;;

(** Evaluate with material and piece-square tables - OPTIMIZED single-pass *)
let evaluate ?(history = []) (pos : Position.t) : int =
  let side = Position.side_to_move pos in
  (* Single pass: collect all data in one loop *)
  let our_material = ref 0 in
  let their_material = ref 0 in
  let our_position = ref 0 in
  let their_position = ref 0 in
  let our_pieces_count = ref 0 in
  let their_pieces_count = ref 0 in
  (* Count material and positional bonuses in single pass - iterate only occupied squares *)
  let occupied = Position.occupied pos in
  Bitboard.iter
    (fun sq ->
       match Position.piece_at pos sq with
       | Some piece ->
         let mat_value = piece_value piece in
         let pos_value = piece_square_value piece sq in
         if piece.color = side
         then (
           our_material := !our_material + mat_value;
           our_position := !our_position + pos_value;
           if piece.kind <> Pawn && piece.kind <> King then incr our_pieces_count)
         else (
           their_material := !their_material + mat_value;
           their_position := !their_position + pos_value;
           if piece.kind <> Pawn && piece.kind <> King then incr their_pieces_count)
       | None -> ())
    occupied;
  let material_diff = !our_material - !their_material in
  let position_diff = !our_position - !their_position in
  (* Evaluate pawn structure *)
  let pawn_structure_bonus =
    let our_pawn_eval = evaluate_pawn_structure pos side in
    let their_pawn_eval = evaluate_pawn_structure pos (Color.opponent side) in
    our_pawn_eval - their_pawn_eval
  in
  (* Trade incentive - use already collected piece counts *)
  let trade_incentive =
    if abs material_diff > 200
    then (
      let total_pieces = !our_pieces_count + !their_pieces_count in
      if material_diff > 200
      then
        (* Ahead: prefer having fewer pieces total (encourage trades) *)
        -(total_pieces * 5)
      else
        (* Behind: prefer having more pieces total (avoid trades) *)
        total_pieces * 5)
    else 0
  in
  (* King safety and castling evaluation - use cached king positions *)
  let king_safety_bonus =
    let castling_rights = Position.castling_rights pos in
    let our_rights = castling_rights.(if side = White then 0 else 1) in
    let their_rights = castling_rights.(if side = White then 1 else 0) in
    (* Use cached king positions instead of searching *)
    let our_king_sq =
      if side = White then Position.white_king_sq pos else Position.black_king_sq pos
    in
    let their_king_sq =
      if side = White then Position.black_king_sq pos else Position.white_king_sq pos
    in
    let our_bonus =
      (* Check if we've already castled (king on g1/g8 or c1/c8) *)
      let has_castled =
        if side = White
        then our_king_sq = 6 || our_king_sq = 2 (* g1 or c1 *)
        else our_king_sq = 62 || our_king_sq = 58 (* g8 or c8 *)
      in
      if has_castled
      then
        (* Already castled - excellent! *)
        120
        (* Increased from 100 *)
      else (
        (* Not castled yet *)
        let can_castle_short = our_rights.short <> None in
        let can_castle_long = our_rights.long <> None in
        if can_castle_short || can_castle_long
        then (
          (* Can still castle - but should we castle NOW? *)
          (* Check if the path is clear (pieces between king and rook are developed) *)
          let kingside_clear =
            if side = White
            then
              (* White kingside: f1 and g1 should be empty *)
              is_square_empty pos 5 && is_square_empty pos 6
            else
              (* Black kingside: f8 and g8 should be empty *)
              is_square_empty pos 61 && is_square_empty pos 62
          in
          let queenside_clear =
            if side = White
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
            (* Strong incentive - was 30 *)
          else
            (* Can castle but path blocked - work on development *)
            30)
        else if
          (* Lost castling rights - penalty, especially in opening/middlegame *)
          !our_material + !their_material > 6000
        then (* Still in opening/middlegame *)
          -60 (* Increased penalty from -30 *)
        else -20 (* Increased penalty from -10 *))
    in
    let their_penalty =
      (* Mirror evaluation for opponent *)
      let has_castled =
        if side = White
        then their_king_sq = 62 || their_king_sq = 58 (* g8 or c8 *)
        else their_king_sq = 6 || their_king_sq = 2 (* g1 or c1 *)
      in
      if has_castled
      then
        (* They've castled - bad for us *)
        -120
        (* Increased from -100 *)
      else (
        let can_castle_short = their_rights.short <> None in
        let can_castle_long = their_rights.long <> None in
        if can_castle_short || can_castle_long
        then (
          (* They can castle - check if path is clear *)
          let kingside_clear =
            if side = White
            then
              (* Black kingside from White's perspective *)
              is_square_empty pos 61 && is_square_empty pos 62
            else
              (* White kingside from Black's perspective *)
              is_square_empty pos 5 && is_square_empty pos 6
          in
          let queenside_clear =
            if side = White
            then
              (* Black queenside from White's perspective *)
              is_square_empty pos 57 && is_square_empty pos 58 && is_square_empty pos 59
            else
              (* White queenside from Black's perspective *)
              is_square_empty pos 1 && is_square_empty pos 2 && is_square_empty pos 3
          in
          if (can_castle_short && kingside_clear) || (can_castle_long && queenside_clear)
          then
            (* Opponent can castle now - bad! *)
            -80
            (* Was -30 *)
          else
            (* Opponent can castle but path blocked *)
            -30)
        else if
          (* They lost castling rights - good for us *)
          !our_material + !their_material > 6000
        then 60 (* Increased from 30 *)
        else 20 (* Increased from 10 *))
    in
    our_bonus + their_penalty
  in
  (* Development evaluation - encourage proper opening play *)
  let development_bonus =
    let our_dev = evaluate_development pos side in
    let their_dev = evaluate_development pos (Color.opponent side) in
    our_dev - their_dev
  in
  (* Bishop pair bonus *)
  let bishop_pair_bonus =
    let our_bishops = evaluate_bishop_pair pos side in
    let their_bishops = evaluate_bishop_pair pos (Color.opponent side) in
    our_bishops - their_bishops
  in
  (* Piece safety - penalize hanging/threatened pieces *)
  let safety_bonus =
    let our_safety = evaluate_piece_safety pos side in
    let their_safety = evaluate_piece_safety pos (Color.opponent side) in
    our_safety - their_safety
  in
  (* Repetition incentive - avoid when winning, seek when losing *)
  let repetition_incentive = evaluate_repetition_incentive pos history material_diff in
  (* 50-move rule incentive - avoid draws when winning *)
  let fifty_move_incentive = evaluate_fifty_move_incentive pos material_diff in
  (* Rook endgame evaluation - king cutoff and rook positioning *)
  let rook_endgame_bonus =
    let our_bonus = evaluate_rook_endgame pos side in
    let their_bonus = evaluate_rook_endgame pos (Color.opponent side) in
    our_bonus - their_bonus
  in
  (* Ladder mate evaluation - coordinate major pieces to push king to edge *)
  let ladder_mate_bonus =
    let our_bonus = evaluate_ladder_mate pos side in
    let their_bonus = evaluate_ladder_mate pos (Color.opponent side) in
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
