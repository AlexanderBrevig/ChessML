(** Movegen - Fast legal move generation for all piece types
    
    Generates all legal moves for a position using magic bitboards for sliding pieces.
    Handles special moves (castling, en passant, promotions) and validates legality
    by checking for pins, checks, and king safety. Uses pre-computed attack tables
    for non-sliding pieces (pawns, knights, kings).
    
    Performance: O(1) attack lookup via magic bitboards, optimized for tactical positions
*)

open Chessml_core
open Types

(* Initialize magic bitboards at module load time *)
let () = Magic.init ()

(* Pre-computed attack tables for pieces *)
(* These would normally be loaded from files or generated at compile time *)
(* For now, we use simplified versions *)

(** Knight attack patterns *)
let knight_attacks_table = Array.make 64 Bitboard.empty

(** King attack patterns *)
let king_attacks_table = Array.make 64 Bitboard.empty

(** White pawn push patterns *)
let white_pawn_pushes_table = Array.make 64 Bitboard.empty

(** Black pawn push patterns *)
let black_pawn_pushes_table = Array.make 64 Bitboard.empty

(** White pawn attack patterns *)
let white_pawn_attacks_table = Array.make 64 Bitboard.empty

(** Black pawn attack patterns *)
let black_pawn_attacks_table = Array.make 64 Bitboard.empty

(** Rays between two squares *)
let ray_between_table = Array.make_matrix 64 64 Bitboard.empty

(** Rays containing two squares *)
let ray_containing_table = Array.make_matrix 64 64 Bitboard.empty

(** Magic entry for sliding piece move generation *)
type magic_entry =
  { mask : Int64.t
  ; magic : Int64.t
  ; shift : int
  ; offset : int
  }

(** Rook magic bitboards *)
let rook_magics : magic_entry array =
  Array.make 64 { mask = 0L; magic = 0L; shift = 0; offset = 0 }
;;

(** Bishop magic bitboards *)
let bishop_magics : magic_entry array =
  Array.make 64 { mask = 0L; magic = 0L; shift = 0; offset = 0 }
;;

(** Rook move database *)
let rook_moves : Int64.t array = Array.make 102400 0L

(** Bishop move database *)
let bishop_moves : Int64.t array = Array.make 5248 0L

(** Get magic index for sliding piece attacks *)
let magic_index (entry : magic_entry) (blockers : Bitboard.t) : int =
  let blockers_masked = Int64.logand blockers entry.mask in
  let hash = Int64.mul blockers_masked entry.magic in
  let index = Int64.to_int (Int64.shift_right_logical hash entry.shift) in
  entry.offset + index
;;

(** Initialize knight attacks *)
let init_knight_attacks () =
  for sq = 0 to 63 do
    let bb = ref Bitboard.empty in
    let file = sq mod 8 in
    let rank = sq / 8 in
    (* Knight can move in L-shape: 2 squares in one direction, 1 in perpendicular *)
    let moves = [ 2, 1; 2, -1; -2, 1; -2, -1; 1, 2; 1, -2; -1, 2; -1, -2 ] in
    List.iter
      (fun (df, dr) ->
         let new_file = file + df in
         let new_rank = rank + dr in
         if new_file >= 0 && new_file < 8 && new_rank >= 0 && new_rank < 8
         then (
           let target_sq = new_file + (new_rank * 8) in
           bb := Bitboard.set !bb target_sq))
      moves;
    knight_attacks_table.(sq) <- !bb
  done
;;

(** Initialize king attacks *)
let init_king_attacks () =
  for sq = 0 to 63 do
    let bb = ref Bitboard.empty in
    let file = sq mod 8 in
    let rank = sq / 8 in
    (* King can move one square in any direction *)
    let moves = [ 1, 0; -1, 0; 0, 1; 0, -1; 1, 1; 1, -1; -1, 1; -1, -1 ] in
    List.iter
      (fun (df, dr) ->
         let new_file = file + df in
         let new_rank = rank + dr in
         if new_file >= 0 && new_file < 8 && new_rank >= 0 && new_rank < 8
         then (
           let target_sq = new_file + (new_rank * 8) in
           bb := Bitboard.set !bb target_sq))
      moves;
    king_attacks_table.(sq) <- !bb
  done
;;

(** Initialize pawn attacks *)
let init_pawn_attacks () =
  for sq = 0 to 63 do
    let file = sq mod 8 in
    let rank = sq / 8 in
    (* White pawns attack diagonally forward *)
    let white_attacks = ref Bitboard.empty in
    if rank < 7
    then (
      if file > 0
      then white_attacks := Bitboard.set !white_attacks (file - 1 + ((rank + 1) * 8));
      if file < 7
      then white_attacks := Bitboard.set !white_attacks (file + 1 + ((rank + 1) * 8)));
    white_pawn_attacks_table.(sq) <- !white_attacks;
    (* Black pawns attack diagonally backward *)
    let black_attacks = ref Bitboard.empty in
    if rank > 0
    then (
      if file > 0
      then black_attacks := Bitboard.set !black_attacks (file - 1 + ((rank - 1) * 8));
      if file < 7
      then black_attacks := Bitboard.set !black_attacks (file + 1 + ((rank - 1) * 8)));
    black_pawn_attacks_table.(sq) <- !black_attacks
  done
;;

(** Initialize pawn pushes *)
let init_pawn_pushes () =
  for sq = 0 to 63 do
    let file = sq mod 8 in
    let rank = sq / 8 in
    (* White pawns push forward *)
    let white_pushes = ref Bitboard.empty in
    if rank < 7
    then (
      white_pushes := Bitboard.set !white_pushes (file + ((rank + 1) * 8));
      (* Double push from starting rank *)
      if rank = 1 then white_pushes := Bitboard.set !white_pushes (file + ((rank + 2) * 8)));
    white_pawn_pushes_table.(sq) <- !white_pushes;
    (* Black pawns push backward *)
    let black_pushes = ref Bitboard.empty in
    if rank > 0
    then (
      black_pushes := Bitboard.set !black_pushes (file + ((rank - 1) * 8));
      (* Double push from starting rank *)
      if rank = 6 then black_pushes := Bitboard.set !black_pushes (file + ((rank - 2) * 8)));
    black_pawn_pushes_table.(sq) <- !black_pushes
  done
;;

(** Initialize all lookup tables *)
let init_tables () =
  init_knight_attacks ();
  init_king_attacks ();
  init_pawn_attacks ();
  init_pawn_pushes ()
;;

(* Initialize tables on module load *)
let () = init_tables ()

(** Get knight attacks for a square *)
let knight_attacks (sq : Square.t) : Bitboard.t = knight_attacks_table.(sq)

(** Get king attacks for a square *)
let king_attacks (sq : Square.t) : Bitboard.t = king_attacks_table.(sq)

(** Get pawn attacks for a square and color *)
let pawn_attacks (sq : Square.t) (color : color) : Bitboard.t =
  match color with
  | White -> white_pawn_attacks_table.(sq)
  | Black -> black_pawn_attacks_table.(sq)
;;

(** Get pawn pushes for a square and color *)
let pawn_pushes (sq : Square.t) (color : color) : Bitboard.t =
  match color with
  | White -> white_pawn_pushes_table.(sq)
  | Black -> black_pawn_pushes_table.(sq)
;;

(** Fast rook attacks using magic bitboards *)
let rook_attacks (sq : Square.t) (blockers : Bitboard.t) : Bitboard.t =
  Magic.rook_attacks sq blockers
;;

(** Fast bishop attacks using magic bitboards *)
let bishop_attacks (sq : Square.t) (blockers : Bitboard.t) : Bitboard.t =
  Magic.bishop_attacks sq blockers
;;

(** Queen attacks (combination of rook and bishop) *)
let queen_attacks (sq : Square.t) (blockers : Bitboard.t) : Bitboard.t =
  Int64.logor (rook_attacks sq blockers) (bishop_attacks sq blockers)
;;

(** Get attacks for a piece *)
let attacks_for (piece : piece) (sq : Square.t) (blockers : Bitboard.t) : Bitboard.t =
  match piece.kind with
  | Pawn -> pawn_attacks sq piece.color
  | Knight -> knight_attacks sq
  | Bishop -> bishop_attacks sq blockers
  | Rook -> rook_attacks sq blockers
  | Queen -> queen_attacks sq blockers
  | King -> king_attacks sq
;;

(** Get pawn push moves *)
let pawn_push_moves (sq : Square.t) (color : color) (blockers : Bitboard.t) : Bitboard.t =
  let rank = sq / 8 in
  let file = sq mod 8 in
  let moves = ref Bitboard.empty in
  (match color with
   | White ->
     (* Single push *)
     if rank < 7
     then (
       let target = file + ((rank + 1) * 8) in
       if not (Bitboard.contains blockers target)
       then (
         moves := Bitboard.set !moves target;
         (* Double push from starting rank *)
         if rank = 1
         then (
           let target2 = file + ((rank + 2) * 8) in
           if not (Bitboard.contains blockers target2)
           then moves := Bitboard.set !moves target2)))
   | Black ->
     (* Single push *)
     if rank > 0
     then (
       let target = file + ((rank - 1) * 8) in
       if not (Bitboard.contains blockers target)
       then (
         moves := Bitboard.set !moves target;
         (* Double push from starting rank *)
         if rank = 6
         then (
           let target2 = file + ((rank - 2) * 8) in
           if not (Bitboard.contains blockers target2)
           then moves := Bitboard.set !moves target2))));
  !moves
;;

(** Compute occupied squares bitboard - uses cached value *)
let compute_occupied (pos : Position.t) : Bitboard.t = Position.occupied pos

(** Compute all squares attacked by color *)
let compute_attacks_by (pos : Position.t) (color : color) : Bitboard.t =
  let attacks = ref Bitboard.empty in
  let blockers = compute_occupied pos in
  (* Iterate through all squares *)
  for sq = 0 to 63 do
    match Position.piece_at pos sq with
    | Some piece when piece.color = color ->
      attacks := Int64.logor !attacks (attacks_for piece sq blockers)
    | _ -> ()
  done;
  !attacks
;;

(** Compute attackers to a square *)
let compute_attackers_to (pos : Position.t) (sq : Square.t) (color : color) : Bitboard.t =
  let attackers = ref Bitboard.empty in
  let blockers = compute_occupied pos in
  (* Check each piece type *)
  for from_sq = 0 to 63 do
    match Position.piece_at pos from_sq with
    | Some piece when piece.color = color ->
      let attacks = attacks_for piece from_sq blockers in
      if Bitboard.contains attacks sq then attackers := Bitboard.set !attackers from_sq
    | _ -> ()
  done;
  !attackers
;;

(** Find the king square for a color - uses cached position *)
let find_king (pos : Position.t) (color : color) : Square.t option =
  let sq =
    if color = White then Position.white_king_sq pos else Position.black_king_sq pos
  in
  Some sq
;;

(** Compute ray between two squares (exclusive) *)
let ray_between (from_sq : Square.t) (to_sq : Square.t) : Bitboard.t =
  let from_file = from_sq mod 8 in
  let from_rank = from_sq / 8 in
  let to_file = to_sq mod 8 in
  let to_rank = to_sq / 8 in
  let ray = ref Bitboard.empty in
  (* Check if squares are on same rank, file, or diagonal *)
  let file_diff = to_file - from_file in
  let rank_diff = to_rank - from_rank in
  if file_diff = 0 && rank_diff <> 0
  then (
    (* Same file *)
    let step = if rank_diff > 0 then 1 else -1 in
    let start_rank = from_rank + step in
    let end_rank = to_rank in
    for r = min start_rank (end_rank - step) to max start_rank (end_rank - step) do
      if r <> to_rank then ray := Bitboard.set !ray (from_file + (r * 8))
    done)
  else if rank_diff = 0 && file_diff <> 0
  then (
    (* Same rank *)
    let step = if file_diff > 0 then 1 else -1 in
    let start_file = from_file + step in
    let end_file = to_file in
    for f = min start_file (end_file - step) to max start_file (end_file - step) do
      if f <> to_file then ray := Bitboard.set !ray (f + (from_rank * 8))
    done)
  else if abs file_diff = abs rank_diff && file_diff <> 0
  then (
    (* Diagonal *)
    let file_step = if file_diff > 0 then 1 else -1 in
    let rank_step = if rank_diff > 0 then 1 else -1 in
    let steps = abs file_diff - 1 in
    for i = 1 to steps do
      let f = from_file + (i * file_step) in
      let r = from_rank + (i * rank_step) in
      ray := Bitboard.set !ray (f + (r * 8))
    done);
  !ray
;;

(** Compute checkers, pinned pieces, and checkmask for a position *)
let compute_legal_masks (pos : Position.t) (color : color)
  : Bitboard.t * Bitboard.t * Bitboard.t
  =
  let opponent = Color.opponent color in
  let occupied = compute_occupied pos in
  match find_king pos color with
  | None -> Bitboard.empty, Bitboard.empty, Bitboard.empty (* Should never happen *)
  | Some king_sq ->
    let checkers = ref Bitboard.empty in
    let pinned = ref Bitboard.empty in
    (* Find knight and pawn checkers *)
    let knight_check = knight_attacks king_sq in
    for sq = 0 to 63 do
      match Position.piece_at pos sq with
      | Some piece when piece.color = opponent && piece.kind = Knight ->
        if Bitboard.contains knight_check sq then checkers := Bitboard.set !checkers sq
      | _ -> ()
    done;
    let pawn_check = pawn_attacks king_sq color in
    for sq = 0 to 63 do
      match Position.piece_at pos sq with
      | Some piece when piece.color = opponent && piece.kind = Pawn ->
        if Bitboard.contains pawn_check sq then checkers := Bitboard.set !checkers sq
      | _ -> ()
    done;
    (* Find sliding checkers and pinned pieces *)
    let rook_ray = rook_attacks king_sq Bitboard.empty in
    let bishop_ray = bishop_attacks king_sq Bitboard.empty in
    (* Check rooks and queens on rook rays *)
    for sq = 0 to 63 do
      match Position.piece_at pos sq with
      | Some piece when piece.color = opponent && (piece.kind = Rook || piece.kind = Queen)
        ->
        if Bitboard.contains rook_ray sq
        then (
          let ray = ray_between king_sq sq in
          let blockers_count = Bitboard.population (Int64.logand ray occupied) in
          match blockers_count with
          | 0 -> checkers := Bitboard.set !checkers sq
          | 1 ->
            (* Check if the blocker is our piece (pinned) *)
            let blocking_piece_sq = ref None in
            Bitboard.iter
              (fun r_sq ->
                 match Position.piece_at pos r_sq with
                 | Some p when p.color = color -> blocking_piece_sq := Some r_sq
                 | _ -> ())
              ray;
            (match !blocking_piece_sq with
             | Some bsq -> pinned := Bitboard.set !pinned bsq
             | None -> ())
          | _ -> ())
      | _ -> ()
    done;
    (* Check bishops and queens on bishop rays *)
    for sq = 0 to 63 do
      match Position.piece_at pos sq with
      | Some piece
        when piece.color = opponent && (piece.kind = Bishop || piece.kind = Queen) ->
        if Bitboard.contains bishop_ray sq
        then (
          let ray = ray_between king_sq sq in
          let blockers_count = Bitboard.population (Int64.logand ray occupied) in
          match blockers_count with
          | 0 -> checkers := Bitboard.set !checkers sq
          | 1 ->
            (* Check if the blocker is our piece (pinned) *)
            let blocking_piece_sq = ref None in
            Bitboard.iter
              (fun r_sq ->
                 match Position.piece_at pos r_sq with
                 | Some p when p.color = color -> blocking_piece_sq := Some r_sq
                 | _ -> ())
              ray;
            (match !blocking_piece_sq with
             | Some bsq -> pinned := Bitboard.set !pinned bsq
             | None -> ())
          | _ -> ())
      | _ -> ()
    done;
    (* Compute checkmask *)
    let checkmask = ref Bitboard.empty in
    if Bitboard.is_empty !checkers
    then
      (* No check: can move anywhere except where opponent pieces are or our pieces *)
      for sq = 0 to 63 do
        match Position.piece_at pos sq with
        | Some piece when piece.color = opponent && piece.kind <> King ->
          checkmask := Bitboard.set !checkmask sq
        | None -> checkmask := Bitboard.set !checkmask sq
        | _ -> ()
      done
    else
      (* In check: must block or capture checker *)
      Bitboard.iter
        (fun checker_sq ->
           checkmask := Bitboard.set !checkmask checker_sq;
           let ray = ray_between king_sq checker_sq in
           checkmask := Int64.logor !checkmask ray)
        !checkers;
    !checkers, !pinned, !checkmask
;;

(** Check if a square is attacked by opponent after a hypothetical move *)
let is_square_attacked_after_move
      (pos : Position.t)
      (target_sq : Square.t)
      (from_sq : Square.t)
      (to_sq : Square.t)
      (by_color : color)
  : bool
  =
  (* Create a hypothetical occupied bitboard after the move *)
  let occupied = compute_occupied pos in
  let occupied = Bitboard.clear occupied from_sq in
  (* Remove piece from source *)
  let occupied = Bitboard.set occupied to_sq in
  (* Add piece to destination *)
  (* Helper to check if a square has an attacking piece of given type(s) *)
  let has_attacker sq kind_check =
    sq <> from_sq
    && sq <> to_sq
    &&
    (* Skip moved/captured squares *)
    match Position.piece_at pos sq with
    | Some piece -> piece.color = by_color && kind_check piece.kind
    | None -> false
  in
  (* Check knight attacks - only iterate over squares on knight attack rays *)
  let knight_attackers = knight_attacks target_sq in
  let knight_found = ref false in
  Bitboard.iter
    (fun sq ->
       if (not !knight_found) && has_attacker sq (fun k -> k = Knight)
       then knight_found := true)
    knight_attackers;
  if !knight_found
  then true
  else (
    (* Check pawn attacks - only iterate over squares on pawn attack rays *)
    let defender_color = Color.opponent by_color in
    let pawn_attackers = pawn_attacks target_sq defender_color in
    let pawn_found = ref false in
    Bitboard.iter
      (fun sq ->
         if (not !pawn_found) && has_attacker sq (fun k -> k = Pawn)
         then pawn_found := true)
      pawn_attackers;
    if !pawn_found
    then true
    else (
      (* Check king attacks - only iterate over squares adjacent to target *)
      let king_attackers = king_attacks target_sq in
      let king_found = ref false in
      Bitboard.iter
        (fun sq ->
           if (not !king_found) && has_attacker sq (fun k -> k = King)
           then king_found := true)
        king_attackers;
      if !king_found
      then true
      else (
        (* Check rook/queen attacks - only iterate over squares on rook rays *)
        let rook_attackers = rook_attacks target_sq occupied in
        let rook_found = ref false in
        Bitboard.iter
          (fun sq ->
             if (not !rook_found) && has_attacker sq (fun k -> k = Rook || k = Queen)
             then rook_found := true)
          rook_attackers;
        if !rook_found
        then true
        else (
          (* Check bishop/queen attacks - only iterate over squares on bishop rays *)
          let bishop_attackers = bishop_attacks target_sq occupied in
          let bishop_found = ref false in
          Bitboard.iter
            (fun sq ->
               if
                 (not !bishop_found) && has_attacker sq (fun k -> k = Bishop || k = Queen)
               then bishop_found := true)
            bishop_attackers;
          !bishop_found))))
;;

(** Check if a move is legal given pins and checkmask *)
let is_legal_move
      (pos : Position.t)
      (from_sq : Square.t)
      (to_sq : Square.t)
      (pinned : Bitboard.t)
      (checkmask : Bitboard.t)
      (king_sq : Square.t)
      (in_check : bool)
  : bool
  =
  let piece_opt = Position.piece_at pos from_sq in
  match piece_opt with
  | None -> false
  | Some piece ->
    let color = Position.side_to_move pos in
    let opponent = Color.opponent color in
    (* King moves are special - must move to non-attacked square *)
    if piece.kind = King
    then
      (* King can always move (except to attacked squares, checked later) *)
      (* But king moves ignore checkmask *)
      (* After king moves, check if the target square is attacked *)
      not (is_square_attacked_after_move pos to_sq from_sq to_sq opponent)
    else (
      (* First, do the basic legality checks *)
      let basic_legal =
        if in_check && not (Bitboard.contains checkmask to_sq)
        then false (* If pinned, can only move along pin ray *)
        else if Bitboard.contains pinned from_sq
        then (
          (* Pinned piece can only move along the ray to the king *)
          let ray = ray_between king_sq from_sq in
          let full_ray = Int64.logor ray (Bitboard.of_square from_sq) in
          let full_ray = Int64.logor full_ray (Bitboard.of_square king_sq) in
          (* Check if to_sq is on the ray *)
          Bitboard.contains full_ray to_sq
          ||
          (* Or if moving towards the attacker on the same ray *)
          let king_file = king_sq mod 8 in
          let king_rank = king_sq / 8 in
          let from_file = from_sq mod 8 in
          let from_rank = from_sq / 8 in
          let to_file = to_sq mod 8 in
          let to_rank = to_sq / 8 in
          (* Check if on same file, rank, or diagonal *)
          let on_same_line =
            (king_file = from_file && from_file = to_file)
            || (king_rank = from_rank && from_rank = to_rank)
            || (abs (king_file - from_file) = abs (king_rank - from_rank)
                && abs (from_file - to_file) = abs (from_rank - to_rank)
                && abs (king_file - to_file) = abs (king_rank - to_rank))
          in
          on_same_line)
        else true
      in
      (* If basic checks pass, verify the move doesn't leave king in check *)
      if basic_legal
      then not (is_square_attacked_after_move pos king_sq from_sq to_sq opponent)
      else false)
;;

(** Generate legal moves for a position *)
let generate_moves (pos : Position.t) : Move.t list =
  let color = Position.side_to_move pos in
  let opponent = Color.opponent color in
  (* Compute legal masks *)
  let checkers, pinned, checkmask = compute_legal_masks pos color in
  let in_check = not (Bitboard.is_empty checkers) in
  let checker_count = Bitboard.population checkers in
  (* Find king *)
  let king_sq =
    match find_king pos color with
    | Some sq -> sq
    | None -> 0 (* Should never happen *)
  in
  (* Calculate occupied squares *)
  let blockers = compute_occupied pos in
  let our_pieces = ref Bitboard.empty in
  for sq = 0 to 63 do
    match Position.piece_at pos sq with
    | Some piece when piece.color = color -> our_pieces := Bitboard.set !our_pieces sq
    | _ -> ()
  done;
  let our_pieces = !our_pieces in
  (* Compute opponent attacks (for king moves) *)
  (* We need to compute attacks with the king removed, since sliding pieces *)
  (* behind the king will attack squares the king might move to *)
  let blockers_without_king =
    Int64.logand blockers (Int64.lognot (Bitboard.of_square king_sq))
  in
  let opponent_attacks = ref Bitboard.empty in
  for sq = 0 to 63 do
    match Position.piece_at pos sq with
    | Some piece when piece.color = opponent ->
      opponent_attacks
      := Int64.logor !opponent_attacks (attacks_for piece sq blockers_without_king)
    | _ -> ()
  done;
  let opponent_attacks = !opponent_attacks in
  let moves = ref [] in
  (* If in double check, only king moves are legal *)
  if checker_count >= 2
  then (
    (* Only generate king moves *)
    match Position.piece_at pos king_sq with
    | Some _ ->
      let king_moves = king_attacks king_sq in
      let valid_king_moves = Int64.logand king_moves (Int64.lognot our_pieces) in
      Bitboard.iter
        (fun to_sq ->
           (* King cannot move to attacked square *)
           if not (Bitboard.contains opponent_attacks to_sq)
           then (
             let kind =
               match Position.piece_at pos to_sq with
               | Some _ -> Move.Capture
               | None -> Move.Quiet
             in
             moves := Move.make king_sq to_sq kind :: !moves))
        valid_king_moves
    | None -> ())
  else
    (* Generate moves for all pieces *)
    for from_sq = 0 to 63 do
      match Position.piece_at pos from_sq with
      | Some piece when piece.color = color ->
        if piece.kind = King
        then (
          (* King moves *)
          let king_moves = king_attacks from_sq in
          let valid_king_moves = Int64.logand king_moves (Int64.lognot our_pieces) in
          Bitboard.iter
            (fun to_sq ->
               (* King cannot move to attacked square *)
               if not (Bitboard.contains opponent_attacks to_sq)
               then (
                 let kind =
                   match Position.piece_at pos to_sq with
                   | Some _ -> Move.Capture
                   | None -> Move.Quiet
                 in
                 moves := Move.make from_sq to_sq kind :: !moves))
            valid_king_moves;
          (* Castling moves *)
          let castling_rights = Position.castling_rights pos in
          let our_rights = castling_rights.(if color = White then 0 else 1) in
          (* Check if we can castle (not in check) *)
          if not (Bitboard.contains opponent_attacks from_sq)
          then (
            (* Short castling (kingside) *)
            (match our_rights.short with
             | Some _rook_sq ->
               let king_dest = if color = White then 6 else 62 in
               (* g1 or g8 *)
               let _rook_dest = if color = White then 5 else 61 in
               (* f1 or f8 *)
               (* Check if path is clear *)
               let between_squares = if color = White then [ 5; 6 ] else [ 61; 62 ] in
               let path_clear =
                 List.for_all (fun sq -> Position.piece_at pos sq = None) between_squares
               in
               (* Check if king doesn't pass through or land on attacked square *)
               let path_safe =
                 List.for_all
                   (fun sq -> not (Bitboard.contains opponent_attacks sq))
                   between_squares
               in
               if path_clear && path_safe
               then moves := Move.make from_sq king_dest Move.ShortCastle :: !moves
             | None -> ());
            (* Long castling (queenside) *)
            match our_rights.long with
            | Some _rook_sq ->
              let king_dest = if color = White then 2 else 58 in
              (* c1 or c8 *)
              let _rook_dest = if color = White then 3 else 59 in
              (* d1 or d8 *)
              (* Check if path is clear (includes b1/b8 square) *)
              let between_squares =
                if color = White then [ 1; 2; 3 ] else [ 57; 58; 59 ]
              in
              let path_clear =
                List.for_all (fun sq -> Position.piece_at pos sq = None) between_squares
              in
              (* Check if king doesn't pass through or land on attacked square *)
              (* Note: b1/b8 square doesn't need to be safe, only king's path *)
              let king_path = if color = White then [ 2; 3 ] else [ 58; 59 ] in
              let path_safe =
                List.for_all
                  (fun sq -> not (Bitboard.contains opponent_attacks sq))
                  king_path
              in
              if path_clear && path_safe
              then moves := Move.make from_sq king_dest Move.LongCastle :: !moves
            | None -> ()))
        else (
          (* Non-king pieces *)
          let piece_moves =
            if piece.kind = Pawn
            then (
              (* Pawn moves *)
              let attacks = pawn_attacks from_sq piece.color in
              let pushes = pawn_push_moves from_sq piece.color blockers in
              (* Only include attacks that capture *)
              let captures = ref Bitboard.empty in
              Bitboard.iter
                (fun to_sq ->
                   match Position.piece_at pos to_sq with
                   | Some target when target.color = opponent ->
                     captures := Bitboard.set !captures to_sq
                   | _ -> ())
                attacks;
              Int64.logor !captures pushes)
            else attacks_for piece from_sq blockers
          in
          (* Remove moves to our own pieces *)
          let valid_moves = Int64.logand piece_moves (Int64.lognot our_pieces) in
          (* Filter by legality *)
          Bitboard.iter
            (fun to_sq ->
               if is_legal_move pos from_sq to_sq pinned checkmask king_sq in_check
               then (
                 let is_capture = Position.piece_at pos to_sq <> None in
                 (* Check if this is a pawn promotion *)
                 if piece.kind = Pawn
                 then (
                   let to_rank = to_sq / 8 in
                   let is_promotion =
                     (color = White && to_rank = 7)
                     ||
                     (* White promotes on rank 8 (index 7) *)
                     (color = Black && to_rank = 0)
                     (* Black promotes on rank 1 (index 0) *)
                   in
                   if is_promotion
                   then (
                     (* Generate all four promotion moves *)
                     let promo_kinds =
                       if is_capture
                       then
                         [ Move.CaptureAndPromoteQueen
                         ; Move.CaptureAndPromoteRook
                         ; Move.CaptureAndPromoteBishop
                         ; Move.CaptureAndPromoteKnight
                         ]
                       else
                         [ Move.PromoteQueen
                         ; Move.PromoteRook
                         ; Move.PromoteBishop
                         ; Move.PromoteKnight
                         ]
                     in
                     List.iter
                       (fun kind -> moves := Move.make from_sq to_sq kind :: !moves)
                       promo_kinds)
                   else (
                     (* Regular pawn move - check if it's a double push *)
                     let from_rank = from_sq / 8 in
                     let to_rank = to_sq / 8 in
                     let is_double_push =
                       (color = White && from_rank = 1 && to_rank = 3)
                       || (color = Black && from_rank = 6 && to_rank = 4)
                     in
                     let kind =
                       if is_capture
                       then Move.Capture
                       else if is_double_push
                       then Move.PawnDoublePush
                       else Move.Quiet
                     in
                     moves := Move.make from_sq to_sq kind :: !moves))
                 else (
                   (* Non-pawn piece *)
                   let kind = if is_capture then Move.Capture else Move.Quiet in
                   moves := Move.make from_sq to_sq kind :: !moves)))
            valid_moves)
      | _ -> ()
    done;
  (* Generate en passant captures *)
  (match Position.ep_square pos with
   | Some ep_sq ->
     let color = Position.side_to_move pos in
     (* Find pawns that can capture en passant *)
     let ep_file = ep_sq mod 8 in
     (* Check left attacking pawn *)
     if ep_file > 0
     then (
       let attacker_sq =
         if color = White
         then ep_sq - 9 (* One square down-left for white *)
         else ep_sq + 7 (* One square up-left for black *)
       in
       match Position.piece_at pos attacker_sq with
       | Some piece when piece.color = color && piece.kind = Pawn ->
         if is_legal_move pos attacker_sq ep_sq pinned checkmask king_sq in_check
         then moves := Move.make attacker_sq ep_sq Move.EnPassantCapture :: !moves
       | _ -> ());
     (* Check right attacking pawn *)
     if ep_file < 7
     then (
       let attacker_sq =
         if color = White
         then ep_sq - 7 (* One square down-right for white *)
         else ep_sq + 9 (* One square up-right for black *)
       in
       match Position.piece_at pos attacker_sq with
       | Some piece when piece.color = color && piece.kind = Pawn ->
         if is_legal_move pos attacker_sq ep_sq pinned checkmask king_sq in_check
         then moves := Move.make attacker_sq ep_sq Move.EnPassantCapture :: !moves
       | _ -> ())
   | None -> ());
  List.rev !moves
;;

(** Generate moves only from specific squares *)
let generate_moves_from (pos : Position.t) (mask : Bitboard.t) : Move.t list =
  let moves = ref [] in
  let color = Position.side_to_move pos in
  Bitboard.iter
    (fun from_sq ->
       match Position.piece_at pos from_sq with
       | Some piece when piece.color = color ->
         let blockers = Bitboard.empty in
         let attacks = attacks_for piece from_sq blockers in
         Bitboard.iter
           (fun to_sq ->
              let kind =
                match Position.piece_at pos to_sq with
                | Some _ -> Move.Capture
                | None -> Move.Quiet
              in
              moves := Move.make from_sq to_sq kind :: !moves)
           attacks
       | _ -> ())
    mask;
  List.rev !moves
;;
