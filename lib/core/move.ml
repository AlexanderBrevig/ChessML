(** Move - Chess move representation and utilities
    
    Encodes chess moves efficiently using 16 bits: from square, to square, and move type.
    Supports all move types including quiet moves, captures, castling, en passant,
    and promotions. Provides conversions to/from UCI notation and algebraic notation.
    
    Move encoding: bits 0-5: from, 6-11: to, 12-15: move kind
*)

open Types

type move_kind =
  | Quiet
  | PawnDoublePush
  | ShortCastle
  | LongCastle
  | Capture
  | EnPassantCapture
  | PromoteKnight
  | PromoteBishop
  | PromoteRook
  | PromoteQueen
  | CaptureAndPromoteKnight
  | CaptureAndPromoteBishop
  | CaptureAndPromoteRook
  | CaptureAndPromoteQueen

type t =
  { from : Square.t
  ; to_sq : Square.t
  ; kind : move_kind
  }

let make from to_sq kind = { from; to_sq; kind }
let from mv = mv.from
let to_square mv = mv.to_sq
let kind mv = mv.kind
let is_quiet mv = mv.kind = Quiet

let is_capture mv =
  match mv.kind with
  | Capture
  | EnPassantCapture
  | CaptureAndPromoteKnight
  | CaptureAndPromoteBishop
  | CaptureAndPromoteRook
  | CaptureAndPromoteQueen -> true
  | _ -> false
;;

let is_promotion mv =
  match mv.kind with
  | PromoteKnight
  | PromoteBishop
  | PromoteRook
  | PromoteQueen
  | CaptureAndPromoteKnight
  | CaptureAndPromoteBishop
  | CaptureAndPromoteRook
  | CaptureAndPromoteQueen -> true
  | _ -> false
;;

let is_castle mv =
  match mv.kind with
  | ShortCastle | LongCastle -> true
  | _ -> false
;;

let is_en_passant mv = mv.kind = EnPassantCapture

let promotion mv =
  match mv.kind with
  | PromoteKnight | CaptureAndPromoteKnight -> Some Knight
  | PromoteBishop | CaptureAndPromoteBishop -> Some Bishop
  | PromoteRook | CaptureAndPromoteRook -> Some Rook
  | PromoteQueen | CaptureAndPromoteQueen -> Some Queen
  | _ -> None
;;

let of_uci str =
  if String.length str < 4
  then invalid_arg ("Move.of_uci: invalid UCI string: " ^ str)
  else (
    let from = Square.of_uci (String.sub str 0 2) in
    let to_sq = Square.of_uci (String.sub str 2 2) in
    let promo =
      if String.length str > 4 then Some (PieceKind.of_char str.[4]) else None
    in
    (* Default to quiet move; needs position context for accurate kind *)
    let kind =
      match promo with
      | Some Knight -> PromoteKnight
      | Some Bishop -> PromoteBishop
      | Some Rook -> PromoteRook
      | Some Queen -> PromoteQueen
      | Some _ -> invalid_arg "Invalid promotion piece"
      | None -> Quiet
    in
    { from; to_sq; kind })
;;

let to_uci mv =
  let from_str = Square.to_uci mv.from in
  let to_str = Square.to_uci mv.to_sq in
  let promo_str =
    match promotion mv with
    | Some piece -> String.make 1 (PieceKind.to_char piece)
    | None -> ""
  in
  from_str ^ to_str ^ promo_str
;;

let to_string = to_uci
