(** Position - Complete chess position representation and manipulation

    Immutable position: a mailbox board plus per-piece, per-color and occupancy
    bitboards, castling rights, en passant square, move counters and a Polyglot
    Zobrist key. [make_move] updates bitboards, occupancy and key incrementally
    through a single primitive ({!toggle}) so they can never drift apart.
*)

open Chessml_core
open Types

type castling_rights =
  { short : Square.t option
  ; long : Square.t option
  }

type board = piece option array

type t =
  { board : board
  ; side_to_move : color
  ; castling_rights : castling_rights array
  ; ep_square : Square.t option
  ; halfmove : int
  ; fullmove : int
  ; key : Int64.t
  ; white_king_sq : Square.t
  ; black_king_sq : Square.t
  ; occupied : Bitboard.t
  ; white_pawns : Bitboard.t
  ; white_knights : Bitboard.t
  ; white_bishops : Bitboard.t
  ; white_rooks : Bitboard.t
  ; white_queens : Bitboard.t
  ; white_king : Bitboard.t
  ; black_pawns : Bitboard.t
  ; black_knights : Bitboard.t
  ; black_bishops : Bitboard.t
  ; black_rooks : Bitboard.t
  ; black_queens : Bitboard.t
  ; black_king : Bitboard.t
  ; white_pieces : Bitboard.t
  ; black_pieces : Bitboard.t
  }

let fen_startpos = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
let empty_board () = Array.make 64 None
let piece_at pos sq = pos.board.(sq)
let side_to_move pos = pos.side_to_move
let ep_square pos = pos.ep_square
let halfmove pos = pos.halfmove
let fullmove pos = pos.fullmove
let board pos = pos.board
let castling_rights pos = pos.castling_rights
let white_king_sq pos = pos.white_king_sq
let black_king_sq pos = pos.black_king_sq
let occupied pos = pos.occupied
let key pos = pos.key

(** Piece bitboard accessors *)
let get_pieces pos color kind =
  match color, kind with
  | White, Pawn -> pos.white_pawns
  | White, Knight -> pos.white_knights
  | White, Bishop -> pos.white_bishops
  | White, Rook -> pos.white_rooks
  | White, Queen -> pos.white_queens
  | White, King -> pos.white_king
  | Black, Pawn -> pos.black_pawns
  | Black, Knight -> pos.black_knights
  | Black, Bishop -> pos.black_bishops
  | Black, Rook -> pos.black_rooks
  | Black, Queen -> pos.black_queens
  | Black, King -> pos.black_king
;;

let get_color_pieces pos color =
  if color = White then pos.white_pieces else pos.black_pieces
;;

(** Count non-pawn material pieces for a given color *)
let count_non_pawn_material pos color =
  Bitboard.population (get_pieces pos color Knight)
  + Bitboard.population (get_pieces pos color Bishop)
  + Bitboard.population (get_pieces pos color Rook)
  + Bitboard.population (get_pieces pos color Queen)
;;

(** Material of a color in centipawns (kings excluded) *)
let material pos color =
  List.fold_left
    (fun acc kind ->
       acc + (Bitboard.population (get_pieces pos color kind) * PieceKind.value kind))
    0
    [ Pawn; Knight; Bishop; Rook; Queen ]
;;

(** XOR [piece] on [sq] into its piece and color bitboards, the occupancy and the
    key. Adding and removing a piece are the same operation. Does not touch the
    board array. *)
let toggle pos piece sq =
  let bit = Bitboard.of_square sq in
  let x = Int64.logxor in
  let occupied = x pos.occupied bit in
  let key = x pos.key (Zobrist.piece_key piece sq) in
  match piece.color, piece.kind with
  | White, kind ->
    let pos = { pos with occupied; key; white_pieces = x pos.white_pieces bit } in
    (match kind with
     | Pawn -> { pos with white_pawns = x pos.white_pawns bit }
     | Knight -> { pos with white_knights = x pos.white_knights bit }
     | Bishop -> { pos with white_bishops = x pos.white_bishops bit }
     | Rook -> { pos with white_rooks = x pos.white_rooks bit }
     | Queen -> { pos with white_queens = x pos.white_queens bit }
     | King -> { pos with white_king = x pos.white_king bit })
  | Black, kind ->
    let pos = { pos with occupied; key; black_pieces = x pos.black_pieces bit } in
    (match kind with
     | Pawn -> { pos with black_pawns = x pos.black_pawns bit }
     | Knight -> { pos with black_knights = x pos.black_knights bit }
     | Bishop -> { pos with black_bishops = x pos.black_bishops bit }
     | Rook -> { pos with black_rooks = x pos.black_rooks bit }
     | Queen -> { pos with black_queens = x pos.black_queens bit }
     | King -> { pos with black_king = x pos.black_king bit })
;;

(** Key contribution of the castling rights *)
let castling_key (rights : castling_rights array) =
  let key color =
    let r = rights.(Color.to_int color) in
    Int64.logxor
      (if Option.is_some r.short then Zobrist.castling_key ~color ~short:true else 0L)
      (if Option.is_some r.long then Zobrist.castling_key ~color ~short:false else 0L)
  in
  Int64.logxor (key White) (key Black)
;;

(** Key contribution of the en passant square: only hashed (as in Polyglot) when a
    pawn of the side to move stands next to the pawn that just double-pushed *)
let ep_key pos =
  match pos.ep_square with
  | None -> 0L
  | Some ep_sq ->
    let side = pos.side_to_move in
    let pushed_pawn_sq = if side = White then ep_sq - 8 else ep_sq + 8 in
    let file = ep_sq mod 8 in
    let our_pawns = get_pieces pos side Pawn in
    let adjacent f =
      f >= 0 && f <= 7 && Bitboard.contains our_pawns (pushed_pawn_sq - file + f)
    in
    if adjacent (file - 1) || adjacent (file + 1) then Zobrist.ep_file_key file else 0L
;;

let side_key side = if side = White then Zobrist.white_to_move_key else 0L

(** Compute the key from scratch (the incremental key must always equal this) *)
let compute_key pos =
  let key = ref 0L in
  Array.iteri
    (fun sq -> function
       | Some piece -> key := Int64.logxor !key (Zobrist.piece_key piece sq)
       | None -> ())
    pos.board;
  List.fold_left
    Int64.logxor
    !key
    [ castling_key pos.castling_rights; ep_key pos; side_key pos.side_to_move ]
;;

(** Remove the castling right tied to a rook's home square, if [sq] is one *)
let clear_castling_on (rights : castling_rights array) sq =
  let w = rights.(0)
  and b = rights.(1) in
  if sq = Square.h1
  then rights.(0) <- { w with short = None }
  else if sq = Square.a1
  then rights.(0) <- { w with long = None }
  else if sq = Square.h8
  then rights.(1) <- { b with short = None }
  else if sq = Square.a8
  then rights.(1) <- { b with long = None }
;;

let make_move pos mv =
  let from = Move.from mv in
  let to_sq = Move.to_square mv in
  match pos.board.(from) with
  | None -> pos
  | Some piece ->
    let side = pos.side_to_move in
    let board = Array.copy pos.board in
    let cur = ref { pos with board } in
    let remove sq p =
      board.(sq) <- None;
      cur := toggle !cur p sq
    in
    let put sq p =
      board.(sq) <- Some p;
      cur := toggle !cur p sq
    in
    (* Capture (the en passant victim is behind the target square) *)
    let captured_sq =
      if Move.is_en_passant mv
      then if side = White then to_sq - 8 else to_sq + 8
      else to_sq
    in
    Option.iter (remove captured_sq) board.(captured_sq);
    (* Move the piece, promoting if needed *)
    remove from piece;
    put
      to_sq
      (match Move.promotion mv with
       | Some kind -> { piece with kind }
       | None -> piece);
    (* Castling also moves the rook *)
    (match Move.kind mv with
     | Move.ShortCastle | Move.LongCastle ->
       let short = Move.kind mv = Move.ShortCastle in
       let rook_from = if short then from + 3 else from - 4 in
       let rook_to = if short then from + 1 else from - 1 in
       Option.iter
         (fun rook ->
            remove rook_from rook;
            put rook_to rook)
         board.(rook_from)
     | _ -> ());
    (* Castling rights: lost when the king moves or a rook leaves or is captured
       on its home square *)
    let castling_rights = Array.copy pos.castling_rights in
    if piece.kind = King
    then castling_rights.(Color.to_int side) <- { short = None; long = None };
    clear_castling_on castling_rights from;
    clear_castling_on castling_rights to_sq;
    let ep_square =
      if Move.kind mv = Move.PawnDoublePush then Some ((from + to_sq) / 2) else None
    in
    let next =
      { !cur with
        side_to_move = Color.opponent side
      ; castling_rights
      ; ep_square
      ; halfmove =
          (if piece.kind = Pawn || Move.is_capture mv then 0 else pos.halfmove + 1)
      ; fullmove = (if side = Black then pos.fullmove + 1 else pos.fullmove)
      ; white_king_sq =
          (if piece.kind = King && side = White then to_sq else pos.white_king_sq)
      ; black_king_sq =
          (if piece.kind = King && side = Black then to_sq else pos.black_king_sq)
      }
    in
    let key =
      List.fold_left
        Int64.logxor
        next.key
        [ castling_key pos.castling_rights
        ; castling_key castling_rights
        ; ep_key pos
        ; Zobrist.white_to_move_key
        ]
    in
    { next with key = Int64.logxor key (ep_key next) }
;;

(** Make a null move: pass the turn without moving *)
let make_null_move pos =
  let key =
    List.fold_left Int64.logxor pos.key [ ep_key pos; Zobrist.white_to_move_key ]
  in
  { pos with
    side_to_move = Color.opponent pos.side_to_move
  ; ep_square = None
  ; halfmove = pos.halfmove + 1
  ; key
  }
;;

let empty =
  { board = empty_board ()
  ; side_to_move = White
  ; castling_rights = [| { short = None; long = None }; { short = None; long = None } |]
  ; ep_square = None
  ; halfmove = 0
  ; fullmove = 1
  ; key = 0L
  ; white_king_sq = 0
  ; black_king_sq = 0
  ; occupied = 0L
  ; white_pawns = 0L
  ; white_knights = 0L
  ; white_bishops = 0L
  ; white_rooks = 0L
  ; white_queens = 0L
  ; white_king = 0L
  ; black_pawns = 0L
  ; black_knights = 0L
  ; black_bishops = 0L
  ; black_rooks = 0L
  ; black_queens = 0L
  ; black_king = 0L
  ; white_pieces = 0L
  ; black_pieces = 0L
  }
;;

(** Parse a FEN string. Missing trailing fields default to "w KQkq - 0 1".
    @raise Invalid_argument on an unknown piece character *)
let of_fen fen =
  let parts = String.split_on_char ' ' (String.trim fen) |> List.filter (( <> ) "") in
  let field i default = Option.value ~default (List.nth_opt parts i) in
  let pos = ref empty in
  let board = empty_board () in
  List.iteri
    (fun rank_idx rank_str ->
       let rank = 7 - rank_idx in
       let file = ref 0 in
       String.iter
         (fun c ->
            if c >= '1' && c <= '8'
            then file := !file + (Char.code c - Char.code '0')
            else (
              let color = if Char.uppercase_ascii c = c then White else Black in
              let piece = { color; kind = PieceKind.of_char c } in
              let sq = !file + (rank * 8) in
              if !file > 7 || rank < 0 then invalid_arg ("Position.of_fen: " ^ fen);
              board.(sq) <- Some piece;
              pos := toggle !pos piece sq;
              if piece.kind = King
              then
                if color = White
                then pos := { !pos with white_king_sq = sq }
                else pos := { !pos with black_king_sq = sq };
              incr file))
         rank_str)
    (String.split_on_char '/' (field 0 ""));
  let castling = field 2 "KQkq" in
  let right c sq = if String.contains castling c then Some sq else None in
  let ep = field 3 "-" in
  let pos =
    { !pos with
      board
    ; side_to_move = (if field 1 "w" = "b" then Black else White)
    ; castling_rights =
        [| { short = right 'K' Square.h1; long = right 'Q' Square.a1 }
         ; { short = right 'k' Square.h8; long = right 'q' Square.a8 }
        |]
    ; ep_square = (if ep = "-" then None else Some (Square.of_uci ep))
    ; halfmove = int_of_string (field 4 "0")
    ; fullmove = int_of_string (field 5 "1")
    }
  in
  { pos with key = compute_key pos }
;;

let default () = of_fen fen_startpos

let to_fen pos =
  (* Convert position back to FEN - proper implementation *)
  let board_str =
    let ranks = Array.make 8 "" in
    for rank = 7 downto 0 do
      let rank_str = ref "" in
      let empty_count = ref 0 in
      for file = 0 to 7 do
        let sq = file + (rank * 8) in
        match pos.board.(sq) with
        | None -> incr empty_count
        | Some piece ->
          if !empty_count > 0
          then (
            rank_str := !rank_str ^ string_of_int !empty_count;
            empty_count := 0);
          let piece_char = PieceKind.to_char piece.kind in
          let final_char =
            if piece.color = White
            then Char.uppercase_ascii piece_char
            else Char.lowercase_ascii piece_char
          in
          rank_str := !rank_str ^ String.make 1 final_char
      done;
      if !empty_count > 0 then rank_str := !rank_str ^ string_of_int !empty_count;
      ranks.(7 - rank) <- !rank_str
    done;
    String.concat "/" (Array.to_list ranks)
  in
  let side_char = if pos.side_to_move = White then "w" else "b" in
  let castling_str =
    let w = pos.castling_rights.(0)
    and b = pos.castling_rights.(1) in
    let flag opt c = if Option.is_some opt then c else "" in
    match flag w.short "K" ^ flag w.long "Q" ^ flag b.short "k" ^ flag b.long "q" with
    | "" -> "-"
    | s -> s
  in
  let ep_str =
    match pos.ep_square with
    | Some sq -> Square.to_uci sq
    | None -> "-"
  in
  Printf.sprintf
    "%s %s %s %s %d %d"
    board_str
    side_char
    castling_str
    ep_str
    pos.halfmove
    pos.fullmove
;;

(** Draw ASCII board representation of the position *)
let draw_board pos =
  Printf.printf "  +---+---+---+---+---+---+---+---+\n";
  for rank = 7 downto 0 do
    Printf.printf "%d |" (rank + 1);
    for file = 0 to 7 do
      let sq = (rank * 8) + file in
      let piece_char =
        match piece_at pos sq with
        | None -> " "
        | Some p ->
          let c =
            match p.kind with
            | Pawn -> "P"
            | Knight -> "N"
            | Bishop -> "B"
            | Rook -> "R"
            | Queen -> "Q"
            | King -> "K"
          in
          if p.color = White then c else String.lowercase_ascii c
      in
      Printf.printf " %s |" piece_char
    done;
    Printf.printf "\n";
    Printf.printf "  +---+---+---+---+---+---+---+---+\n"
  done;
  Printf.printf "    a   b   c   d   e   f   g   h\n"
;;

(** Dead position by material (FIDE 9.6): bare kings, a single minor piece, or only
    bishops that all stand on squares of the same color *)
let has_insufficient_material pos =
  let pawns_rooks_queens =
    List.fold_left
      Int64.logor
      0L
      [ pos.white_pawns
      ; pos.black_pawns
      ; pos.white_rooks
      ; pos.black_rooks
      ; pos.white_queens
      ; pos.black_queens
      ]
  in
  if pawns_rooks_queens <> 0L
  then false
  else (
    let knights = Int64.logor pos.white_knights pos.black_knights in
    let bishops = Int64.logor pos.white_bishops pos.black_bishops in
    let light_squares = 0x55AA55AA55AA55AAL in
    Bitboard.population (Int64.logor knights bishops) <= 1
    || (knights = 0L
        && (Int64.logand bishops light_squares = 0L
            || Int64.logand bishops (Int64.lognot light_squares) = 0L)))
;;
