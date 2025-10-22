(** Eval_pawn_structure - Pawn structure evaluation
    
    This module handles all pawn-related evaluation:
    - Passed pawns (no enemy pawns can stop them)
    - Doubled pawns (multiple pawns on same file)
    - Isolated pawns (no friendly pawns on adjacent files)
    - Backward pawns (can't advance safely, no support)
    - Connected/supported pawns (pawn chains)
    - Overall pawn structure scoring
*)

open Chessml_core
open Types

(** Count pawns on a file for a color using bitboards *)
let count_pawns_on_file_bb pos file_int color =
  let pawns = Position.get_pieces pos color Pawn in
  (* Create file mask by setting all squares on that file *)
  let file_mask = ref Bitboard.empty in
  for rank = 0 to 7 do
    file_mask := Bitboard.set !file_mask (file_int + (rank * 8))
  done;
  Bitboard.population (Int64.logand pawns !file_mask)
;;

(** Check if a pawn is passed (no enemy pawns can stop it) *)
let is_passed_pawn (pos : Position.t) (sq : int) (color : color) : bool =
  let file = sq mod 8 in
  let rank = sq / 8 in
  let opponent = Color.opponent color in
  (* First check: is this the most advanced pawn on this file for our color?
     A pawn blocked by a friendly pawn ahead is not a passed pawn! *)
  let is_most_advanced =
    let our_pawns = Position.get_pieces pos color Pawn in
    let pawns_on_file = ref [] in
    for r = 0 to 7 do
      let check_sq = file + (r * 8) in
      if Bitboard.contains our_pawns check_sq then pawns_on_file := r :: !pawns_on_file
    done;
    (* For white, most advanced = highest rank. For black, most advanced = lowest rank *)
    match !pawns_on_file with
    | [] -> false (* No pawns? Shouldn't happen *)
    | ranks ->
      if color = White
      then rank = List.fold_left max min_int ranks
      else rank = List.fold_left min max_int ranks
  in
  if not is_most_advanced
  then false (* Not the frontmost pawn - not passed *)
  else (
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
    !no_blockers)
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

(** Internal evaluation function - does the actual work *)
let evaluate_pawn_structure_inner (pos : Position.t) (color : color) : int =
  let bonus = ref 0 in
  let pawns = Position.get_pieces pos color Pawn in
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

(** Evaluate pawn structure for a given color with caching *)
let evaluate_pawn_structure (pos : Position.t) (color : color) : int =
  let white_pawns = Position.get_pieces pos White Pawn in
  let black_pawns = Position.get_pieces pos Black Pawn in
  let pawn_hash = Pawn_cache.compute_pawn_hash white_pawns black_pawns in
  let cache = Pawn_cache.get_global () in
  match Pawn_cache.probe cache pawn_hash with
  | Some score ->
    (* Cache hit - return stored score adjusted for color *)
    if color = White then score else -score
  | None ->
    (* Cache miss - compute score for both colors and cache *)
    let white_score = evaluate_pawn_structure_inner pos White in
    let black_score = evaluate_pawn_structure_inner pos Black in
    let combined_score = white_score - black_score in
    Pawn_cache.store cache pawn_hash combined_score;
    if color = White then white_score else black_score
;;
