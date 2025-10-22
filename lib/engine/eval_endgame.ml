(** Eval_endgame - Endgame-specific evaluation functions
    
    This module handles evaluation of endgame positions including:
    - Rook endgames (king cutoff, rook behind passed pawn)
    - Ladder mate technique (two major pieces coordinating)
    - Repetition incentives (based on material situation)
    - Fifty-move rule incentives (avoid/seek draws appropriately)
*)

open Chessml_core
open Types

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
let evaluate_rook_endgame
      (pos : Position.t)
      (color : color)
      (is_passed_pawn : Position.t -> int -> color -> bool)
  : int
  =
  let opponent = Color.opponent color in
  (* Count pieces to determine if this is a rook endgame *)
  let our_rooks = Bitboard.population (Position.get_pieces pos color Rook) in
  let their_pawns = Bitboard.population (Position.get_pieces pos opponent Pawn) in
  (* Only evaluate if it's a rook endgame (no queens, bishops, or knights) *)
  let total_queens =
    Bitboard.population (Position.get_pieces pos color Queen)
    + Bitboard.population (Position.get_pieces pos opponent Queen)
  in
  let total_bishops =
    Bitboard.population (Position.get_pieces pos color Bishop)
    + Bitboard.population (Position.get_pieces pos opponent Bishop)
  in
  let total_knights =
    Bitboard.population (Position.get_pieces pos color Knight)
    + Bitboard.population (Position.get_pieces pos opponent Knight)
  in
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
      if opponent = White then Position.white_king_sq pos else Position.black_king_sq pos
    in
    let enemy_king_file = Square.file enemy_king_sq |> File.to_int in
    let enemy_king_rank = Square.rank enemy_king_sq |> Rank.to_int in
    (* Find our rook *)
    let our_rook_bb = Position.get_pieces pos color Rook in
    let our_rook_squares = Bitboard.to_list our_rook_bb in
    List.iter
      (fun pawn_sq ->
         if is_passed_pawn pos pawn_sq color
         then (
           let pawn_file = Square.file pawn_sq |> File.to_int in
           let pawn_rank = Square.rank pawn_sq |> Rank.to_int in
           let relative_rank = if color = White then pawn_rank else 7 - pawn_rank in
           (* Check if our rook cuts off the enemy king from the pawn *)
           List.iter
             (fun rook_sq ->
                let rook_file = Square.file rook_sq |> File.to_int in
                let rook_rank = Square.rank rook_sq |> Rank.to_int in
                (* King cutoff: Rook is between pawn and enemy king on a file or rank *)
                let is_cutting_off_file =
                  (* Rook and pawn on different files, rook file is between king file and pawn file *)
                  if pawn_file < enemy_king_file
                  then rook_file > pawn_file && rook_file < enemy_king_file
                  else if pawn_file > enemy_king_file
                  then rook_file < pawn_file && rook_file > enemy_king_file
                  else false
                in
                let is_cutting_off_rank =
                  (* Similar logic for ranks *)
                  if color = White
                  then
                    (* White wants to prevent black king from coming down *)
                    rook_rank > pawn_rank && enemy_king_rank > rook_rank
                  else
                    (* Black wants to prevent white king from coming up *)
                    rook_rank < pawn_rank && enemy_king_rank < rook_rank
                in
                if is_cutting_off_file || is_cutting_off_rank
                then (
                  (* HUGE bonus for cutting off the king! *)
                  bonus := !bonus + 150 + (relative_rank * 20);
                  (* Extra bonus if pawn is far advanced *)
                  if relative_rank >= 5 then bonus := !bonus + 200);
                (* Bonus for rook behind the passed pawn (classic technique) *)
                let rook_behind_pawn =
                  rook_file = pawn_file
                  && ((color = White && rook_rank < pawn_rank)
                      || (color = Black && rook_rank > pawn_rank))
                in
                if rook_behind_pawn then bonus := !bonus + 50 + (relative_rank * 10))
             our_rook_squares;
           (* Additional bonus if enemy has no pawns (easier to win) *)
           if their_pawns = 0 then bonus := !bonus + 100))
      our_pawn_squares;
    !bonus)
;;

(** Evaluate ladder mate technique with two major pieces (rooks or queen+rook)
    Ladder mate pushes enemy king to edge by coordinating major pieces on adjacent ranks/files.
    Key patterns:
    1. Two rooks on adjacent ranks/files cutting off king
    2. Queen + rook coordinated similarly
    3. Enemy king distance from center (being pushed to edge)
    4. Our pieces maintaining safe distance from enemy king *)
let evaluate_ladder_mate
      (pos : Position.t)
      (color : color)
      (piece_kind_value : piece_kind -> int)
  : int
  =
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
        else Position.black_king_sq pos
      in
      let enemy_king_file = Square.file enemy_king_sq |> File.to_int in
      let enemy_king_rank = Square.rank enemy_king_sq |> Rank.to_int in
      (* Bonus for enemy king distance from center (being pushed to edge) *)
      let center_file = 3.5 in
      (* Between d and e files *)
      let center_rank = 3.5 in
      (* Between 4th and 5th ranks *)
      let file_dist = abs_float (float_of_int enemy_king_file -. center_file) in
      let rank_dist = abs_float (float_of_int enemy_king_rank -. center_rank) in
      let edge_distance = max file_dist rank_dist in
      (* Reward king being far from center (toward edges) *)
      bonus := !bonus + int_of_float (edge_distance *. 40.0);
      (* Bonus if king is on edge (file 0, 7 or rank 0, 7) *)
      if
        enemy_king_file = 0
        || enemy_king_file = 7
        || enemy_king_rank = 0
        || enemy_king_rank = 7
      then bonus := !bonus + 150;
      (* Getting close to mate! *)

      (* Extra bonus if king is in corner *)
      if
        (enemy_king_file = 0 || enemy_king_file = 7)
        && (enemy_king_rank = 0 || enemy_king_rank = 7)
      then bonus := !bonus + 200;
      (* Corner = checkmate territory *)

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
           (not same_rank)
           && (not same_file)
           &&
           (* Different rank AND file *)
           ((rank_diff = 1 && file_diff = 1)
            (* Diagonal adjacent *)
            || (rank_diff = 1 && file_diff <= 2)
            ||
            (* Close on rank, near on file *)
            (file_diff = 1 && rank_diff <= 2))
           (* Close on file, near on rank *)
         in
         if proper_ladder_formation
         then (
           (* EXCELLENT! Proper adjacent ladder formation *)
           bonus := !bonus + 250;
           (* Extra bonus if EXACTLY adjacent diagonally (knight's move away or closer) *)
           if rank_diff = 1 && file_diff = 1 then bonus := !bonus + 100;
           (* Perfect diagonal adjacency! *)

           (* Check if pieces are cutting off king's escape *)
           let piece1_cuts_king_file = abs (file1 - enemy_king_file) = 1 in
           let piece2_cuts_king_file = abs (file2 - enemy_king_file) = 1 in
           let piece1_cuts_king_rank = abs (rank1 - enemy_king_rank) = 1 in
           let piece2_cuts_king_rank = abs (rank2 - enemy_king_rank) = 1 in
           if
             (piece1_cuts_king_file || piece2_cuts_king_file)
             && (piece1_cuts_king_rank || piece2_cuts_king_rank)
           then bonus := !bonus + 150
           (* Perfect trap - cutting off escape squares! *))
         else if
           (* Penalize if not in proper ladder formation *)
           same_rank
         then bonus := !bonus - 200 (* Very bad - same rank allows king to escape *)
         else if same_file
         then bonus := !bonus - 200 (* Very bad - same file allows king to escape *)
         else if rank_diff > 2 || file_diff > 2
         then bonus := !bonus - 150 (* Too far apart - not coordinating properly *)
         else () (* Other formations - neutral *);
         (* Bonus for keeping pieces safe from enemy king *)
         List.iter
           (fun sq ->
              let file = Square.file sq |> File.to_int in
              let rank = Square.rank sq |> Rank.to_int in
              let file_dist = abs (file - enemy_king_file) in
              let rank_dist = abs (rank - enemy_king_rank) in
              let king_distance = max file_dist rank_dist in
              (* Penalize if too close to enemy king (can be captured) *)
              if king_distance = 1
              then bonus := !bonus - 150 (* HUGE danger! King can capture *)
              else if king_distance = 2
              then bonus := !bonus + 30 (* Good safe distance *)
              else if king_distance >= 3
              then bonus := !bonus + 10 (* Safe *))
           pieces
       | _ -> ());
      !bonus))
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
