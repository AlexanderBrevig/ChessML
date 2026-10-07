(** Endgame - Specialized evaluation for endgames recognized by their material

    When a material signature is recognized, its evaluator replaces the general
    evaluation. The search stays in charge of the moves; these functions only tell
    it which direction is progress (or that a position is a draw), so the usual
    tactical safety, including avoiding stalemate, still comes from the search.

    - Lone king against a queen or rook (plus anything): drive the king to the
      edge and bring the attacking king close.
    - King and pawn against king: exact result from the {!Kpk} table; a won
      position also rewards pushing the pawn.
    - King, bishop and knight against king: mate is only possible in a corner of
      the bishop's color, so drive the king towards one of those two corners.
*)

open Chessml_core
open Types

(** Far above any normal evaluation, far below mate scores *)
let known_win = 10000

(** Pieces other than the king *)
let non_king_pieces pos color =
  Int64.logxor (Position.get_color_pieces pos color) (Position.get_pieces pos color King)
;;

let count pos color kind = Bitboard.population (Position.get_pieces pos color kind)

let king_square pos color =
  if color = White then Position.white_king_sq pos else Position.black_king_sq pos
;;

(** 0 in the four center squares up to 6 in the corners *)
let center_distance sq =
  let file = sq mod 8
  and rank = sq / 8 in
  let from_center x = if x < 4 then 3 - x else x - 4 in
  from_center file + from_center rank
;;

(** Bonus for the defending king being near the edge *)
let push_to_edge sq = 20 * center_distance sq

(** Bonus for the kings being close to each other *)
let push_close a b = 10 * (7 - Square.distance a b)

(** Lone king against a queen or rook: score for [strong] *)
let kxk pos ~strong =
  let weak = Color.opponent strong in
  let weak_king = king_square pos weak in
  known_win
  + Position.material pos strong
  + push_to_edge weak_king
  + push_close (king_square pos strong) weak_king
;;

(** King, bishop and knight against king: score for [strong]. The defending king
    has to be driven into a corner the bishop can reach: a1/h8 for a dark-squared
    bishop, a8/h1 for a light-squared one. *)
let kbnk pos ~strong =
  let weak = Color.opponent strong in
  let weak_king = king_square pos weak in
  let light_bishop =
    Int64.logand (Position.get_pieces pos strong Bishop) Bitboard.light_squares <> 0L
  in
  let corners =
    if light_bishop then [ Square.a8; Square.h1 ] else [ Square.a1; Square.h8 ]
  in
  (* Files plus ranks to the nearer right corner (0-14). Every step along the edge
     from a wrong corner towards a right one shortens it, so driving the king
     there always shows progress. It must dominate the other terms. *)
  let corner_distance =
    List.fold_left
      (fun d c ->
         min d (abs ((weak_king mod 8) - (c mod 8)) + abs ((weak_king / 8) - (c / 8))))
      14
      corners
  in
  known_win
  + Position.material pos strong
  + (100 * (14 - corner_distance))
  + push_close (king_square pos strong) weak_king
;;

(** King and pawn against king: score for [strong] *)
let kpk pos ~strong =
  let weak = Color.opponent strong in
  (* the table wants the pawn moving up the board *)
  let orient sq = if strong = White then sq else sq lxor 56 in
  let pawn = orient (Bitboard.lsb (Position.get_pieces pos strong Pawn) |> Option.get) in
  if
    Kpk.wins
      ~strong_king:(orient (king_square pos strong))
      ~weak_king:(orient (king_square pos weak))
      ~pawn
      ~strong_to_move:(Position.side_to_move pos = strong)
  then known_win + Types.PieceKind.value Pawn + (10 * (pawn / 8))
  else Score.draw
;;

(** Which side, if any, has only its king left *)
let bare_king pos =
  if non_king_pieces pos Black = 0L
  then Some Black
  else if non_king_pieces pos White = 0L
  then Some White
  else None
;;

(** Score of a recognized endgame from the side to move's perspective, [None] if
    the general evaluation should be used *)
let evaluate pos =
  let from_side_to_move strong score =
    if Position.side_to_move pos = strong then score else -score
  in
  match bare_king pos with
  | Some weak ->
    let strong = Color.opponent weak in
    if count pos strong Queen + count pos strong Rook > 0
    then Some (from_side_to_move strong (kxk pos ~strong))
    else if
      non_king_pieces pos strong = Position.get_pieces pos strong Pawn
      && count pos strong Pawn = 1
    then Some (from_side_to_move strong (kpk pos ~strong))
    else if
      count pos strong Bishop = 1
      && count pos strong Knight = 1
      && Bitboard.population (non_king_pieces pos strong) = 2
    then Some (from_side_to_move strong (kbnk pos ~strong))
    else None
  | None -> None
;;
