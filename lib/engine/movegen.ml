(** Movegen - Legal move generation and attack detection

    Attacks come from precomputed tables (knights, kings, pawns) and magic
    bitboards (sliders). [attackers_to] finds all pieces attacking a square with
    the reverse-lookup trick and is the single attack primitive used by move
    generation, check detection, SEE and evaluation.

    Moves are generated pseudo-legally from the piece bitboards and kept only if
    the king is safe on the resulting occupancy. That one test covers pins,
    discovered checks through an en passant capture, evasions and double check.
*)

open Chessml_core
open Types

(* Initialize magic bitboards at module load time *)
let () = Magic.init ()
let knight_attacks_table = Array.make 64 Bitboard.empty
let king_attacks_table = Array.make 64 Bitboard.empty
let white_pawn_attacks_table = Array.make 64 Bitboard.empty
let black_pawn_attacks_table = Array.make 64 Bitboard.empty

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

let () =
  init_knight_attacks ();
  init_king_attacks ();
  init_pawn_attacks ()
;;

let knight_attacks (sq : Square.t) : Bitboard.t = knight_attacks_table.(sq)
let king_attacks (sq : Square.t) : Bitboard.t = king_attacks_table.(sq)

(** Squares attacked by a pawn of [color] standing on [sq] *)
let pawn_attacks (sq : Square.t) (color : color) : Bitboard.t =
  match color with
  | White -> white_pawn_attacks_table.(sq)
  | Black -> black_pawn_attacks_table.(sq)
;;

let rook_attacks (sq : Square.t) (occupied : Bitboard.t) : Bitboard.t =
  Magic.rook_attacks sq occupied
;;

let bishop_attacks (sq : Square.t) (occupied : Bitboard.t) : Bitboard.t =
  Magic.bishop_attacks sq occupied
;;

let queen_attacks (sq : Square.t) (occupied : Bitboard.t) : Bitboard.t =
  Int64.logor (rook_attacks sq occupied) (bishop_attacks sq occupied)
;;

(** Squares attacked by [piece] on [sq] given the occupancy *)
let attacks_for (piece : piece) (sq : Square.t) (occupied : Bitboard.t) : Bitboard.t =
  match piece.kind with
  | Pawn -> pawn_attacks sq piece.color
  | Knight -> knight_attacks sq
  | Bishop -> bishop_attacks sq occupied
  | Rook -> rook_attacks sq occupied
  | Queen -> queen_attacks sq occupied
  | King -> king_attacks sq
;;

(** Pieces of color [by] attacking [sq], with sliders blocked by [occupied] *)
let attackers_to pos (sq : Square.t) (by : color) (occupied : Bitboard.t) : Bitboard.t =
  let pieces kind = Position.get_pieces pos by kind in
  let queens = pieces Queen in
  List.fold_left
    Int64.logor
    0L
    [ Int64.logand (pawn_attacks sq (Color.opponent by)) (pieces Pawn)
    ; Int64.logand (knight_attacks sq) (pieces Knight)
    ; Int64.logand (king_attacks sq) (pieces King)
    ; Int64.logand (rook_attacks sq occupied) (Int64.logor (pieces Rook) queens)
    ; Int64.logand (bishop_attacks sq occupied) (Int64.logor (pieces Bishop) queens)
    ]
;;

(** Pieces of color [by] attacking [sq] in the current position *)
let compute_attackers_to pos (sq : Square.t) (by : color) : Bitboard.t =
  attackers_to pos sq by (Position.occupied pos)
;;

let is_square_attacked pos (sq : Square.t) (by : color) : bool =
  compute_attackers_to pos sq by <> 0L
;;

let king_square pos color =
  if color = White then Position.white_king_sq pos else Position.black_king_sq pos
;;

(** Is the side to move in check? *)
let in_check pos =
  let side = Position.side_to_move pos in
  is_square_attacked pos (king_square pos side) (Color.opponent side)
;;

(** Is the king of [side] safe after moving [from] to [to_sq], removing the piece
    on [captured_sq] (the target square, or the en passant victim)? *)
let king_safe_after pos side ~king_sq ~from ~to_sq ~captured_sq =
  let occupied =
    Bitboard.set
      (Bitboard.clear (Bitboard.clear (Position.occupied pos) from) captured_sq)
      to_sq
  in
  let king = if from = king_sq then to_sq else king_sq in
  let attackers = attackers_to pos king (Color.opponent side) occupied in
  Int64.logand attackers (Int64.lognot (Bitboard.of_square captured_sq)) = 0L
;;

let promotions =
  [ Move.PromoteQueen; Move.PromoteRook; Move.PromoteBishop; Move.PromoteKnight ]
;;

let capture_promotions =
  [ Move.CaptureAndPromoteQueen
  ; Move.CaptureAndPromoteRook
  ; Move.CaptureAndPromoteBishop
  ; Move.CaptureAndPromoteKnight
  ]
;;

(** Generate all legal moves for the side to move *)
let generate_moves (pos : Position.t) : Move.t list =
  let side = Position.side_to_move pos in
  let opponent = Color.opponent side in
  let us = Position.get_color_pieces pos side in
  let them = Position.get_color_pieces pos opponent in
  let occupied = Position.occupied pos in
  let king_sq = king_square pos side in
  let moves = ref [] in
  let add ?(captured_sq = -1) from to_sq kind =
    let captured_sq = if captured_sq < 0 then to_sq else captured_sq in
    if king_safe_after pos side ~king_sq ~from ~to_sq ~captured_sq
    then moves := Move.make from to_sq kind :: !moves
  in
  let add_targets from targets =
    Bitboard.iter
      (fun to_sq ->
         add
           from
           to_sq
           (if Bitboard.contains them to_sq then Move.Capture else Move.Quiet))
      (Int64.logand targets (Int64.lognot us))
  in
  (* Pawns *)
  let forward = if side = White then 8 else -8 in
  let start_rank = if side = White then 1 else 6 in
  let promotion_rank = if side = White then 7 else 0 in
  Bitboard.iter
    (fun from ->
       let one = from + forward in
       if not (Bitboard.contains occupied one)
       then
         if one / 8 = promotion_rank
         then List.iter (add from one) promotions
         else (
           add from one Move.Quiet;
           let two = one + forward in
           if from / 8 = start_rank && not (Bitboard.contains occupied two)
           then add from two Move.PawnDoublePush);
       Bitboard.iter
         (fun to_sq ->
            if to_sq / 8 = promotion_rank
            then List.iter (add from to_sq) capture_promotions
            else add from to_sq Move.Capture)
         (Int64.logand (pawn_attacks from side) them);
       match Position.ep_square pos with
       | Some ep when Bitboard.contains (pawn_attacks from side) ep ->
         add ~captured_sq:(ep - forward) from ep Move.EnPassantCapture
       | _ -> ())
    (Position.get_pieces pos side Pawn);
  (* Pieces *)
  Bitboard.iter
    (fun from -> add_targets from (knight_attacks from))
    (Position.get_pieces pos side Knight);
  Bitboard.iter
    (fun from -> add_targets from (bishop_attacks from occupied))
    (Position.get_pieces pos side Bishop);
  Bitboard.iter
    (fun from -> add_targets from (rook_attacks from occupied))
    (Position.get_pieces pos side Rook);
  Bitboard.iter
    (fun from -> add_targets from (queen_attacks from occupied))
    (Position.get_pieces pos side Queen);
  add_targets king_sq (king_attacks king_sq);
  (* Castling: right still held, rook on its square, path empty, and the king is
     not in check and does not pass through or land on an attacked square *)
  let rights = (Position.castling_rights pos).(Color.to_int side) in
  let can_castle rook_sq ~empty ~safe =
    Position.piece_at pos rook_sq = Some { color = side; kind = Rook }
    && List.for_all (fun sq -> not (Bitboard.contains occupied sq)) empty
    && List.for_all (fun sq -> not (is_square_attacked pos sq opponent)) (king_sq :: safe)
  in
  Option.iter
    (fun rook_sq ->
       if
         can_castle
           rook_sq
           ~empty:[ king_sq + 1; king_sq + 2 ]
           ~safe:[ king_sq + 1; king_sq + 2 ]
       then moves := Move.make king_sq (king_sq + 2) Move.ShortCastle :: !moves)
    rights.short;
  Option.iter
    (fun rook_sq ->
       if
         can_castle
           rook_sq
           ~empty:[ king_sq - 1; king_sq - 2; king_sq - 3 ]
           ~safe:[ king_sq - 1; king_sq - 2 ]
       then moves := Move.make king_sq (king_sq - 2) Move.LongCastle :: !moves)
    rights.long;
  List.rev !moves
;;
