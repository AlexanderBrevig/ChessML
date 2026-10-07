(** Static Exchange Evaluation (SEE)

    Material outcome of the capture sequence started by a move, assuming both
    sides always recapture with their least valuable attacker and may stop
    whenever continuing would lose material. Works on occupancy bitboards: each
    capture removes the attacker from the occupancy, which uncovers x-ray
    attackers behind it. Promotions (by the initial move or a pawn recapture on
    the last rank) count the queen's extra value.
*)

open Chessml_core
open Types

let promotion_bonus = PieceKind.value Queen - PieceKind.value Pawn

(** Least valuable attacker of [side] among [attackers]: (square, kind) *)
let least_valuable pos side attackers =
  List.find_map
    (fun kind ->
       let bb = Int64.logand attackers (Position.get_pieces pos side kind) in
       if bb = 0L then None else Option.map (fun sq -> sq, kind) (Bitboard.lsb bb))
    [ Pawn; Knight; Bishop; Rook; Queen; King ]
;;

let evaluate (pos : Position.t) (move : Move.t) : int =
  let from = Move.from move in
  let to_sq = Move.to_square move in
  match Position.piece_at pos from with
  | None -> 0
  | Some mover when Move.is_capture move ->
    let victim_sq, victim_value =
      if Move.is_en_passant move
      then (if mover.color = White then to_sq - 8 else to_sq + 8), PieceKind.value Pawn
      else
        ( to_sq
        , match Position.piece_at pos to_sq with
          | Some p -> PieceKind.value p.kind
          | None -> 0 )
    in
    let first_gain, on_square =
      match Move.promotion move with
      | Some kind ->
        victim_value + PieceKind.value kind - PieceKind.value Pawn, PieceKind.value kind
      | None -> victim_value, PieceKind.value mover.kind
    in
    let gain = Array.make 34 0 in
    gain.(0) <- first_gain;
    let occupied =
      Bitboard.clear (Bitboard.clear (Position.occupied pos) from) victim_sq
    in
    (* Simulate recaptures; [on_square] is the value of the piece that can be taken *)
    let rec swap depth side occupied on_square =
      let attackers =
        Int64.logand (Movegen.attackers_to pos to_sq side occupied) occupied
      in
      match least_valuable pos side attackers with
      | None -> depth
      | Some (sq, kind) ->
        let occupied' = Bitboard.clear occupied sq in
        let opponent = Color.opponent side in
        (* A king may only recapture if the square is no longer defended *)
        if
          kind = King
          && Int64.logand (Movegen.attackers_to pos to_sq opponent occupied') occupied'
             <> 0L
        then depth
        else (
          let promotes = kind = Pawn && to_sq / 8 = if side = White then 7 else 0 in
          let depth = depth + 1 in
          gain.(depth)
          <- on_square + (if promotes then promotion_bonus else 0) - gain.(depth - 1);
          let value = if promotes then PieceKind.value Queen else PieceKind.value kind in
          if depth >= 33 then depth else swap depth opponent occupied' value)
    in
    let depth = swap 0 (Color.opponent mover.color) occupied on_square in
    (* Each side can decline to continue the exchange *)
    for d = depth downto 1 do
      gain.(d - 1) <- -(max (-gain.(d - 1)) gain.(d))
    done;
    gain.(0)
  | Some _ -> 0
;;
