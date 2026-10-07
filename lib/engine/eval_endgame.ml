(** Eval_endgame - Endgame-specific evaluation functions
    
    This module handles evaluation of endgame positions including:
    - Rook endgames (king cutoff, rook behind passed pawn)
    - Ladder mate technique (two major pieces coordinating)
    - Fifty-move rule incentives (avoid/seek draws appropriately)
*)

open Chessml_core
open Types

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
         if Eval_pawn_structure.is_passed_pawn pos pawn_sq color
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
    let their_material = Position.material pos opponent in
    (* Only apply ladder mate bonus if opponent is weak (< 500cp material, basically lone king or king+minor) *)
    if their_material > 500
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
      (* Formation bonus for a pair of major pieces: they must be adjacent on a
         rank or file but not share one, so the king cannot slip between them *)
      let pair_bonus sq1 sq2 =
        let file1 = Square.file sq1 |> File.to_int in
        let rank1 = Square.rank sq1 |> Rank.to_int in
        let file2 = Square.file sq2 |> File.to_int in
        let rank2 = Square.rank sq2 |> Rank.to_int in
        let rank_diff = abs (rank1 - rank2) in
        let file_diff = abs (file1 - file2) in
        let same_rank = rank1 = rank2 in
        let same_file = file1 = file2 in
        let proper_ladder_formation =
          (not same_rank)
          && (not same_file)
          && ((rank_diff = 1 && file_diff <= 2) || (file_diff = 1 && rank_diff <= 2))
        in
        if proper_ladder_formation
        then (
          let cuts_file =
            abs (file1 - enemy_king_file) = 1 || abs (file2 - enemy_king_file) = 1
          in
          let cuts_rank =
            abs (rank1 - enemy_king_rank) = 1 || abs (rank2 - enemy_king_rank) = 1
          in
          250
          + (if rank_diff = 1 && file_diff = 1 then 100 else 0)
          + if cuts_file && cuts_rank then 150 else 0)
        else if same_rank || same_file
        then -200
        else if rank_diff > 2 || file_diff > 2
        then -150
        else 0
      in
      (* Score the best-coordinated pair, so the result does not depend on the
         order in which pieces are found *)
      let pieces = !major_piece_sqs in
      let rec best_pair = function
        | [] -> min_int
        | sq :: rest ->
          List.fold_left
            (fun acc other -> max acc (pair_bonus sq other))
            (best_pair rest)
            rest
      in
      bonus := !bonus + best_pair pieces;
      (* Keep every major piece out of reach of the enemy king *)
      List.iter
        (fun sq ->
           let file_dist = abs ((Square.file sq |> File.to_int) - enemy_king_file) in
           let rank_dist = abs ((Square.rank sq |> Rank.to_int) - enemy_king_rank) in
           match max file_dist rank_dist with
           | 1 -> bonus := !bonus - 150
           | 2 -> bonus := !bonus + 30
           | _ -> bonus := !bonus + 10)
        pieces;
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
    (* Scale by material advantage. The term is antisymmetric (the same position
       scores the same from either side), so the side to move does not matter:
       the side that is ahead is pushed to make progress. *)
    let sign = if material_diff > 0 then -1 else 1 in
    if abs material_diff > 300
    then sign * base_penalty * 3
    else if abs material_diff > 100
    then sign * base_penalty * 2
    else 0)
;;
