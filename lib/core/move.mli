(** Move module interface *)

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

val make : Square.t -> Square.t -> move_kind -> t
val from : t -> Square.t
val to_square : t -> Square.t
val kind : t -> move_kind
val is_quiet : t -> bool
val is_capture : t -> bool
val is_promotion : t -> bool
val is_castle : t -> bool
val is_en_passant : t -> bool
val promotion : t -> piece_kind option
val of_uci : string -> t
val to_uci : t -> string
val move_kind_to_string : move_kind -> string
val to_string : t -> string
