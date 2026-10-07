(** Kpk - Exact win/draw table for king and pawn against king

    The table is computed backwards from positions whose result is obvious, the
    way endgame tablebases are built, but small enough to compute on first use.

    Positions are stored from the side with the pawn ("White", pawn moving up the
    board), with the pawn on files a-d (other files are mirrored):
    24 pawn squares x 64 x 64 king squares x 2 sides to move.

    - Immediate win: White to move can promote and the new queen cannot be taken.
    - Immediate draw: Black to move is stalemated, or can take an undefended pawn.
    - Then repeatedly: with White to move, a position is won if some move leads to a
      win and drawn if every move leads to a draw; with Black to move, it is drawn
      if some move leads to a draw and won if every move leads to a win.
    - Whatever is still undecided when nothing changes any more is a draw.
*)

open Chessml_core

let unknown = '\000'
let invalid = '\001'
let draw = '\002'
let win = '\003'

(** Pawn on file 0-3 and rank 1-6 (a2-d7) *)
let pawn_index sq = (sq mod 8) + (4 * ((sq / 8) - 1))

let pawn_square idx = (idx mod 4) + (8 * ((idx / 4) + 1))

let index ~pawn ~wk ~bk ~white_to_move =
  (((((pawn_index pawn * 64) + wk) * 64) + bk) * 2) + if white_to_move then 0 else 1
;;

let size = 24 * 64 * 64 * 2
let king_attacks = Movegen.king_attacks
let pawn_attacks sq = Movegen.pawn_attacks sq Types.White

(** Squares the black king may move to: not next to the white king, not attacked
    by the pawn (taking a defended pawn is excluded by the first rule) *)
let black_targets ~pawn ~wk ~bk =
  Int64.logand
    (king_attacks bk)
    (Int64.lognot (Int64.logor (king_attacks wk) (pawn_attacks pawn)))
;;

let initial ~pawn ~wk ~bk ~white_to_move =
  if
    wk = bk
    || wk = pawn
    || bk = pawn
    || Square.distance wk bk <= 1
    || (white_to_move && Bitboard.contains (pawn_attacks pawn) bk)
  then invalid
  else if white_to_move
  then (
    let queen = pawn + 8 in
    if
      pawn / 8 = 6
      && queen <> wk
      && queen <> bk
      && (Square.distance bk queen > 1 || Square.distance wk queen = 1)
    then win
    else unknown)
  else (
    let targets = black_targets ~pawn ~wk ~bk in
    if targets = 0L
    then if Bitboard.contains (pawn_attacks pawn) bk then win (* mate *) else draw
    else if Bitboard.contains targets pawn
    then draw
    else unknown)
;;

let compute () =
  let table = Bytes.make size unknown in
  let get ~pawn ~wk ~bk ~white_to_move =
    Bytes.get table (index ~pawn ~wk ~bk ~white_to_move)
  in
  let each f =
    for p = 0 to 23 do
      let pawn = pawn_square p in
      for wk = 0 to 63 do
        for bk = 0 to 63 do
          f ~pawn ~wk ~bk ~white_to_move:true;
          f ~pawn ~wk ~bk ~white_to_move:false
        done
      done
    done
  in
  each (fun ~pawn ~wk ~bk ~white_to_move ->
    Bytes.set
      table
      (index ~pawn ~wk ~bk ~white_to_move)
      (initial ~pawn ~wk ~bk ~white_to_move));
  (* Results of the children of an undecided position *)
  let classify ~pawn ~wk ~bk ~white_to_move =
    let results = ref [] in
    let add r = results := r :: !results in
    if white_to_move
    then (
      Bitboard.iter
        (fun to_sq -> add (get ~pawn ~wk:to_sq ~bk ~white_to_move:false))
        (Int64.logand
           (king_attacks wk)
           (Int64.lognot (Int64.logor (king_attacks bk) (Bitboard.of_square pawn))));
      let one = pawn + 8 in
      if one <> wk && one <> bk
      then
        if pawn / 8 = 6
        then add draw (* promotion that was not an immediate win loses the queen *)
        else (
          add (get ~pawn:one ~wk ~bk ~white_to_move:false);
          let two = one + 8 in
          if pawn / 8 = 1 && two <> wk && two <> bk
          then add (get ~pawn:two ~wk ~bk ~white_to_move:false));
      if List.mem win !results
      then win
      else if List.for_all (fun r -> r = draw || r = invalid) !results
      then draw
      else unknown)
    else (
      Bitboard.iter
        (fun to_sq -> add (get ~pawn ~wk ~bk:to_sq ~white_to_move:true))
        (black_targets ~pawn ~wk ~bk);
      if List.mem draw !results
      then draw
      else if List.for_all (fun r -> r = win || r = invalid) !results
      then win
      else unknown)
  in
  let changed = ref true in
  while !changed do
    changed := false;
    each (fun ~pawn ~wk ~bk ~white_to_move ->
      let i = index ~pawn ~wk ~bk ~white_to_move in
      if Bytes.get table i = unknown
      then (
        let r = classify ~pawn ~wk ~bk ~white_to_move in
        if r <> unknown
        then (
          Bytes.set table i r;
          changed := true)))
  done;
  table
;;

let table = lazy (compute ())

(** Does the side with the pawn win? Squares are given with the pawn moving up the
    board (from rank 2 towards rank 8); any file. *)
let wins ~strong_king ~weak_king ~pawn ~strong_to_move =
  (* mirror files e-h onto a-d *)
  let flip sq = if pawn mod 8 > 3 then sq lxor 7 else sq in
  Bytes.get
    (Lazy.force table)
    (index
       ~pawn:(flip pawn)
       ~wk:(flip strong_king)
       ~bk:(flip weak_king)
       ~white_to_move:strong_to_move)
  = win
;;
