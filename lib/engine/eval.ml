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

(** Piece values in centipawns (by kind) *)
let piece_kind_value (kind : piece_kind) : int =
  match kind with
  | Pawn -> 100
  | Knight -> 320
  | Bishop -> 330
  | Rook -> 500
  | Queen -> 900
  | King -> 20000 (* King is invaluable but give it a high value for MVV-LVA *)
;;

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
  (* Flip square for black pieces *)
  let table_sq = if piece.color = White then sq else sq lxor 56 in
  match piece.kind with
  | Pawn -> pawn_table.(table_sq)
  | Knight -> knight_table.(table_sq)
  | Bishop -> bishop_table.(table_sq)
  | Rook -> rook_table.(table_sq)
  | Queen -> queen_table.(table_sq)
  | King -> king_middlegame_table.(table_sq)
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
           | 2 -> 10 (* 3rd rank *)
           | 3 -> 20 (* 4th rank *)
           | 4 -> 40 (* 5th rank *)
           | 5 -> 70 (* 6th rank *)
           | 6 -> 120 (* 7th rank *)
           | 7 -> 200 (* About to promote! *)
           | _ -> 0
         in
         bonus := !bonus + passed_bonus;
         (* Extra bonus if protected *)
         if has_pawn_support pos sq color then bonus := !bonus + 15);
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

(** Get all attackers of a square using move generation *)
let get_attackers_list (pos : Position.t) (sq : Square.t) (by_color : color)
  : (Square.t * piece_kind) list
  =
  let attackers = ref [] in
  let occupied = Position.occupied pos in
  (* Check all pieces of the attacking color *)
  let pieces = Position.get_color_pieces pos by_color in
  Bitboard.iter
    (fun from_sq ->
       match Position.piece_at pos from_sq with
       | Some piece when piece.color = by_color ->
         (* Check if this piece attacks the target square *)
         let attacks = Movegen.attacks_for piece from_sq occupied in
         if Bitboard.contains attacks sq
         then attackers := (from_sq, piece.kind) :: !attackers
       | _ -> ())
    pieces;
  !attackers
;;

(** Get all defenders of a square *)
let get_defenders_list (pos : Position.t) (sq : Square.t) (color : color)
  : (Square.t * piece_kind) list
  =
  get_attackers_list pos sq color
;;

(** Count attackers and defenders of a square *)
let count_attackers_defenders (pos : Position.t) (sq : Square.t) (piece_color : color)
  : int * int
  =
  let opponent = Color.opponent piece_color in
  let attackers = List.length (get_attackers_list pos sq opponent) in
  let defenders = List.length (get_defenders_list pos sq piece_color) in
  attackers, defenders
;;

(** Calculate the value at risk using SEE (Static Exchange Evaluation) *)
let calculate_exchange_value (pos : Position.t) (sq : Square.t) : int =
  match Position.piece_at pos sq with
  | None -> 0
  | Some piece ->
    let piece_val = piece_kind_value piece.kind in
    let opponent = Color.opponent piece.color in
    (* Get all attackers from opponent *)
    let attackers = get_attackers_list pos sq opponent in
    if attackers = []
    then 0 (* No threats *)
    else (
      (* Find the least valuable attacker *)
      let min_attacker =
        List.fold_left
          (fun acc (_, kind) ->
             let val_ = piece_kind_value kind in
             min acc val_)
          10000
          attackers
      in
      (* Simplified SEE: if attacked by lower value piece, we're in danger *)
      if min_attacker < piece_val
      then (
        (* Get defenders *)
        let defenders = get_defenders_list pos sq piece.color in
        if defenders = []
        then
          (* No defenders - piece is hanging! *)
          -piece_val
        else (
          (* Has defenders - but is it enough? *)
          let min_defender =
            List.fold_left
              (fun acc (_, kind) ->
                 let val_ = piece_kind_value kind in
                 min acc val_)
              10000
              defenders
          in
          (* If we defend with a more valuable piece, it's risky *)
          if min_defender > min_attacker
          then
            (* Bad trade: we'll lose more than we gain *)
            -(min_defender - min_attacker)
          else
            (* Reasonable trade or defended adequately *)
            0))
      else 0 (* Attacked by equal or higher value - okay *))
;;

(** Check if a piece is hanging (undefended or bad trade) *)
let is_piece_hanging (pos : Position.t) (sq : Square.t) : bool =
  match Position.piece_at pos sq with
  | None -> false
  | Some piece ->
    let opponent = Color.opponent piece.color in
    let attackers = get_attackers_list pos sq opponent in
    if attackers = []
    then false (* Not attacked *)
    else (
      let defenders = get_defenders_list pos sq piece.color in
      if defenders = []
      then true (* Attacked with no defenders = hanging *)
      else (
        (* Check if it's a bad trade *)
        let piece_val = piece_kind_value piece.kind in
        let min_attacker_val =
          List.fold_left
            (fun acc (_, kind) -> min acc (piece_kind_value kind))
            10000
            attackers
        in
        let min_defender_val =
          List.fold_left
            (fun acc (_, kind) -> min acc (piece_kind_value kind))
            10000
            defenders
        in
        (* Hanging if: attacked by cheaper piece AND no adequate defender *)
        min_attacker_val < piece_val && min_defender_val > min_attacker_val))
;;

(** Check if piece is en prise (can be captured with immediate gain) *)
let is_piece_en_prise (pos : Position.t) (sq : Square.t) : bool =
  match Position.piece_at pos sq with
  | None -> false
  | Some piece ->
    if piece.kind = Pawn || piece.kind = King
    then false (* Don't check pawns/king with this function *)
    else (
      let exchange_val = calculate_exchange_value pos sq in
      exchange_val < -100 (* Losing at least a pawn's worth *))
;;

(** Evaluate threats to a specific piece *)
let evaluate_piece_threats (pos : Position.t) (sq : Square.t) : int =
  calculate_exchange_value pos sq
;;

(** Evaluate bishop pair bonus *)
let evaluate_bishop_pair (pos : Position.t) (color : color) : int =
  let bishops = Position.get_pieces pos color Bishop in
  let bishop_count = Bitboard.population bishops in
  if bishop_count >= 2 then 50 (* Bishop pair bonus *) else 0
;;

(** Evaluate piece safety - penalize hanging pieces *)
let evaluate_piece_safety (pos : Position.t) (color : color) : int =
  let penalty = ref 0 in
  (* Check all our pieces for threats *)
  let pieces = Position.get_color_pieces pos color in
  Bitboard.iter
    (fun sq ->
       match Position.piece_at pos sq with
       | Some piece when piece.color = color && piece.kind <> Pawn && piece.kind <> King
         ->
         (* Use SEE-like logic: a hanging piece will likely be captured *)
         let exchange_val = calculate_exchange_value pos sq in
         if exchange_val <= -300
         then
           (* Piece is completely hanging (minor/major piece)
             Apply 3x penalty: strikes a balance between preventing hanging piece moves
             and not over-distorting eval. The 3x multiplier ensures search sees losing
             a hanging piece as very bad, while still maintaining reasonable eval comparisons *)
           penalty := !penalty + (exchange_val * 3)
         else if exchange_val < -200
         then
           (* Piece in serious danger *)
           penalty := !penalty + (exchange_val * 2)
         else if exchange_val < -50
         then
           (* Some threat exists *)
           penalty := !penalty + (exchange_val * 3 / 2)
         else
           (* Minor or no threat *)
           penalty := !penalty + exchange_val
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

(** Evaluate with material and piece-square tables - OPTIMIZED single-pass *)
let evaluate (pos : Position.t) : int =
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
  (* Return score from side-to-move perspective *)
  material_diff
  + position_diff
  + pawn_structure_bonus
  + trade_incentive
  + king_safety_bonus
  + development_bonus
  + bishop_pair_bonus
  + safety_bonus
;;
