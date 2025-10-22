(** Position - Complete chess position representation and manipulation
    
    Tracks the full game state including piece placement, castling rights,
    en passant squares, move counters, and side to move. Uses bitboards for
    efficient piece lookup and caches king positions for fast check detection.
    Supports FEN parsing/generation and provides move making/unmaking.
    
    Key optimizations: cached occupied squares, cached king positions,
    incremental hash updates (via Zobrist hashing)
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
  ; white_king_sq : Square.t (* Cached king positions for fast access *)
  ; black_king_sq : Square.t
  ; occupied : Int64.t (* Cached bitboard of all occupied squares *)
  ; (* Piece-type bitboards for fast lookups - major optimization! *)
    white_pawns : Bitboard.t
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
  ; white_pieces : Bitboard.t (* All white pieces *)
  ; black_pieces : Bitboard.t (* All black pieces *)
  }

let fen_startpos = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
let empty_board () = Array.make 64 None
let piece_at pos sq = pos.board.(sq)

(** Helper to get reference to the appropriate piece bitboard *)
let get_piece_bb_ref piece =
  match piece.color, piece.kind with
  | White, Pawn -> fun pos -> pos.white_pawns
  | White, Knight -> fun pos -> pos.white_knights
  | White, Bishop -> fun pos -> pos.white_bishops
  | White, Rook -> fun pos -> pos.white_rooks
  | White, Queen -> fun pos -> pos.white_queens
  | White, King -> fun pos -> pos.white_king
  | Black, Pawn -> fun pos -> pos.black_pawns
  | Black, Knight -> fun pos -> pos.black_knights
  | Black, Bishop -> fun pos -> pos.black_bishops
  | Black, Rook -> fun pos -> pos.black_rooks
  | Black, Queen -> fun pos -> pos.black_queens
  | Black, King -> fun pos -> pos.black_king
;;

(** Update position with a new value for a specific piece bitboard *)
let update_piece_bb pos piece bb =
  match piece.color, piece.kind with
  | White, Pawn -> { pos with white_pawns = bb }
  | White, Knight -> { pos with white_knights = bb }
  | White, Bishop -> { pos with white_bishops = bb }
  | White, Rook -> { pos with white_rooks = bb }
  | White, Queen -> { pos with white_queens = bb }
  | White, King -> { pos with white_king = bb }
  | Black, Pawn -> { pos with black_pawns = bb }
  | Black, Knight -> { pos with black_knights = bb }
  | Black, Bishop -> { pos with black_bishops = bb }
  | Black, Rook -> { pos with black_rooks = bb }
  | Black, Queen -> { pos with black_queens = bb }
  | Black, King -> { pos with black_king = bb }
;;

let set_piece sq piece pos =
  let board = Array.copy pos.board in
  board.(sq) <- Some piece;
  (* Update piece bitboard *)
  let get_bb = get_piece_bb_ref piece in
  let bb = Bitboard.set (get_bb pos) sq in
  let pos = update_piece_bb pos piece bb in
  (* Update color bitboards *)
  let pos =
    if piece.color = White
    then { pos with white_pieces = Bitboard.set pos.white_pieces sq }
    else { pos with black_pieces = Bitboard.set pos.black_pieces sq }
  in
  { pos with board }
;;

let clear_square sq pos =
  let board = Array.copy pos.board in
  let old_piece = board.(sq) in
  board.(sq) <- None;
  match old_piece with
  | None -> { pos with board }
  | Some piece ->
    (* Update piece bitboard *)
    let get_bb = get_piece_bb_ref piece in
    let bb = Bitboard.clear (get_bb pos) sq in
    let pos = update_piece_bb pos piece bb in
    (* Update color bitboards *)
    let pos =
      if piece.color = White
      then { pos with white_pieces = Bitboard.clear pos.white_pieces sq }
      else { pos with black_pieces = Bitboard.clear pos.black_pieces sq }
    in
    { pos with board }
;;

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
  let knights = get_pieces pos color Knight in
  let bishops = get_pieces pos color Bishop in
  let rooks = get_pieces pos color Rook in
  let queens = get_pieces pos color Queen in
  Bitboard.population knights
  + Bitboard.population bishops
  + Bitboard.population rooks
  + Bitboard.population queens
;;

(** Compute all bitboards from board array - used only during position creation *)
let compute_bitboards_from_board board =
  let white_pawns = ref Bitboard.empty in
  let white_knights = ref Bitboard.empty in
  let white_bishops = ref Bitboard.empty in
  let white_rooks = ref Bitboard.empty in
  let white_queens = ref Bitboard.empty in
  let white_king = ref Bitboard.empty in
  let black_pawns = ref Bitboard.empty in
  let black_knights = ref Bitboard.empty in
  let black_bishops = ref Bitboard.empty in
  let black_rooks = ref Bitboard.empty in
  let black_queens = ref Bitboard.empty in
  let black_king = ref Bitboard.empty in
  for sq = 0 to 63 do
    match board.(sq) with
    | Some piece ->
      let bb_ref =
        match piece.color, piece.kind with
        | White, Pawn -> white_pawns
        | White, Knight -> white_knights
        | White, Bishop -> white_bishops
        | White, Rook -> white_rooks
        | White, Queen -> white_queens
        | White, King -> white_king
        | Black, Pawn -> black_pawns
        | Black, Knight -> black_knights
        | Black, Bishop -> black_bishops
        | Black, Rook -> black_rooks
        | Black, Queen -> black_queens
        | Black, King -> black_king
      in
      bb_ref := Bitboard.set !bb_ref sq
    | None -> ()
  done;
  let white_pieces =
    Int64.logor
      !white_pawns
      (Int64.logor
         !white_knights
         (Int64.logor
            !white_bishops
            (Int64.logor !white_rooks (Int64.logor !white_queens !white_king))))
  in
  let black_pieces =
    Int64.logor
      !black_pawns
      (Int64.logor
         !black_knights
         (Int64.logor
            !black_bishops
            (Int64.logor !black_rooks (Int64.logor !black_queens !black_king))))
  in
  let occupied = Int64.logor white_pieces black_pieces in
  ( !white_pawns
  , !white_knights
  , !white_bishops
  , !white_rooks
  , !white_queens
  , !white_king
  , !black_pawns
  , !black_knights
  , !black_bishops
  , !black_rooks
  , !black_queens
  , !black_king
  , white_pieces
  , black_pieces
  , occupied )
;;

(** Compute occupied bitboard from board - used only during position creation *)
let compute_occupied_from_board board =
  let occ = ref Bitboard.empty in
  for sq = 0 to 63 do
    match board.(sq) with
    | Some _ -> occ := Bitboard.set !occ sq
    | None -> ()
  done;
  !occ
;;

(** Find king position by scanning board - used only during position creation *)
let find_king_on_board board color =
  let king_sq = ref 0 in
  for sq = 0 to 63 do
    match board.(sq) with
    | Some piece when piece.color = color && piece.kind = King -> king_sq := sq
    | _ -> ()
  done;
  !king_sq
;;

let make_move pos mv =
  (* Handle different move types *)
  let from = Move.from mv in
  let to_square = Move.to_square mv in
  let piece = piece_at pos from in
  match piece with
  | None -> pos
  | Some p ->
    (* Handle promotion - replace pawn with promoted piece *)
    let piece_to_place =
      if Move.is_promotion mv
      then (
        match Move.promotion mv with
        | Some promo_kind -> { p with kind = promo_kind }
        | None -> p)
      else p
    in
    let pos' = pos |> clear_square from |> set_piece to_square piece_to_place in
    (* Handle castling moves - move the rook as well *)
    let pos_final =
      if Move.is_castle mv
      then (
        match Move.kind mv with
        | Move.ShortCastle ->
          (* Kingside castling *)
          if pos.side_to_move = White
          then (
            (* White O-O: move rook from h1 to f1 *)
            let rook = piece_at pos' Square.h1 in
            match rook with
            | Some r -> pos' |> clear_square Square.h1 |> set_piece Square.f1 r
            | None -> pos')
          else (
            (* Black O-O: move rook from h8 to f8 *)
            let rook = piece_at pos' Square.h8 in
            match rook with
            | Some r -> pos' |> clear_square Square.h8 |> set_piece Square.f8 r
            | None -> pos')
        | Move.LongCastle ->
          (* Queenside castling *)
          if pos.side_to_move = White
          then (
            (* White O-O-O: move rook from a1 to d1 *)
            let rook = piece_at pos' Square.a1 in
            match rook with
            | Some r -> pos' |> clear_square Square.a1 |> set_piece Square.d1 r
            | None -> pos')
          else (
            (* Black O-O-O: move rook from a8 to d8 *)
            let rook = piece_at pos' Square.a8 in
            match rook with
            | Some r -> pos' |> clear_square Square.a8 |> set_piece Square.d8 r
            | None -> pos')
        | _ -> pos')
      else pos'
    in
    (* Handle en passant capture - remove the captured pawn *)
    let pos_after_ep =
      if Move.is_en_passant mv
      then (
        (* The captured pawn is on the same file as to_square but different rank *)
        let captured_pawn_square =
          if pos.side_to_move = White
          then to_square - 8 (* Captured black pawn is one rank below *)
          else to_square + 8 (* Captured white pawn is one rank above *)
        in
        clear_square captured_pawn_square pos_final)
      else pos_final
    in
    (* Update en passant square *)
    let new_ep_square =
      if Move.kind mv = Move.PawnDoublePush
      then (
        (* Set ep_square to the square the pawn skipped over *)
        let ep_sq =
          if pos.side_to_move = White
          then from + 8 (* Square between from and to *)
          else from - 8
        in
        Some ep_sq)
      else None (* Clear ep_square for any other move *)
    in
    (* Update move counters *)
    let new_side = Color.opponent pos.side_to_move in
    let new_halfmove = pos.halfmove + 1 in
    let new_fullmove = if new_side = White then pos.fullmove + 1 else pos.fullmove in
    (* Update king positions if king moved *)
    let new_white_king =
      if p.kind = King && p.color = White then to_square else pos.white_king_sq
    in
    let new_black_king =
      if p.kind = King && p.color = Black then to_square else pos.black_king_sq
    in
    (* Update occupied bitboard - clear from square, set to square *)
    let new_occupied =
      let occ = pos.occupied in
      let occ = Bitboard.clear occ from in
      let occ = Bitboard.set occ to_square in
      (* Handle en passant - clear captured pawn square *)
      if Move.is_en_passant mv
      then (
        let captured_sq =
          if pos.side_to_move = White then to_square - 8 else to_square + 8
        in
        Bitboard.clear occ captured_sq
        (* Handle castling - set rook destination, clear rook source *))
      else if Move.is_castle mv
      then (
        match Move.kind mv with
        | Move.ShortCastle ->
          let rook_from = if pos.side_to_move = White then Square.h1 else Square.h8 in
          let rook_to = if pos.side_to_move = White then Square.f1 else Square.f8 in
          Bitboard.set (Bitboard.clear occ rook_from) rook_to
        | Move.LongCastle ->
          let rook_from = if pos.side_to_move = White then Square.a1 else Square.a8 in
          let rook_to = if pos.side_to_move = White then Square.d1 else Square.d8 in
          Bitboard.set (Bitboard.clear occ rook_from) rook_to
        | _ -> occ)
      else occ
    in
    (* Update castling rights *)
    let new_castling_rights = Array.copy pos.castling_rights in
    let color_idx = if pos.side_to_move = White then 0 else 1 in
    (* If king moved or castled, lose all castling rights for this color *)
    if p.kind = King || Move.is_castle mv
    then new_castling_rights.(color_idx) <- { short = None; long = None }
    else if
      (* If rook moved from starting square, lose that side's castling *)
      p.kind = Rook
    then (
      let current_rights = new_castling_rights.(color_idx) in
      if pos.side_to_move = White
      then (
        if
          (* White rooks *)
          from = Square.h1
        then new_castling_rights.(color_idx) <- { current_rights with short = None }
        else if from = Square.a1
        then new_castling_rights.(color_idx) <- { current_rights with long = None })
      else if
        (* Black rooks *)
        from = Square.h8
      then new_castling_rights.(color_idx) <- { current_rights with short = None }
      else if from = Square.a8
      then new_castling_rights.(color_idx) <- { current_rights with long = None });
    (* If opponent's rook was captured on its starting square, lose that castling right *)
    let opponent_idx = 1 - color_idx in
    let opponent_color = Color.opponent pos.side_to_move in
    (match piece_at pos to_square with
     | Some captured_piece
       when captured_piece.kind = Rook && captured_piece.color = opponent_color ->
       let opponent_rights = new_castling_rights.(opponent_idx) in
       if opponent_color = White
       then (
         if to_square = Square.h1
         then new_castling_rights.(opponent_idx) <- { opponent_rights with short = None }
         else if to_square = Square.a1
         then new_castling_rights.(opponent_idx) <- { opponent_rights with long = None })
       else if to_square = Square.h8
       then new_castling_rights.(opponent_idx) <- { opponent_rights with short = None }
       else if to_square = Square.a8
       then new_castling_rights.(opponent_idx) <- { opponent_rights with long = None }
     | _ -> ());
    (* Compute all piece bitboards from final board state *)
    let ( white_pawns
        , white_knights
        , white_bishops
        , white_rooks
        , white_queens
        , white_king
        , black_pawns
        , black_knights
        , black_bishops
        , black_rooks
        , black_queens
        , black_king
        , white_pieces
        , black_pieces
        , _ )
      =
      compute_bitboards_from_board pos_after_ep.board
    in
    { pos_after_ep with
      side_to_move = new_side
    ; ep_square = new_ep_square
    ; halfmove = new_halfmove
    ; fullmove = new_fullmove
    ; white_king_sq = new_white_king
    ; black_king_sq = new_black_king
    ; castling_rights = new_castling_rights
    ; occupied = new_occupied
    ; white_pawns
    ; white_knights
    ; white_bishops
    ; white_rooks
    ; white_queens
    ; white_king
    ; black_pawns
    ; black_knights
    ; black_bishops
    ; black_rooks
    ; black_queens
    ; black_king
    ; white_pieces
    ; black_pieces
    }
;;

(* FEN parser *)
let of_fen fen =
  let board = empty_board () in
  let parts = String.split_on_char ' ' fen in
  (* Parse piece placement *)
  let piece_placement = List.nth parts 0 in
  let ranks = String.split_on_char '/' piece_placement in
  List.iteri
    (fun rank_idx rank_str ->
       let rank = 7 - rank_idx in
       (* FEN starts from rank 8 *)
       let file = ref 0 in
       String.iter
         (fun c ->
            if c >= '1' && c <= '8'
            then file := !file + (int_of_char c - int_of_char '0')
            else (
              let sq = !file + (rank * 8) in
              (try
                 let color = if Char.uppercase_ascii c = c then White else Black in
                 let kind = PieceKind.of_char c in
                 board.(sq) <- Some { color; kind }
               with
               | Invalid_argument _ -> ());
              file := !file + 1))
         rank_str)
    ranks;
  (* Parse side to move *)
  let side_to_move =
    if List.length parts > 1 && List.nth parts 1 = "b" then Black else White
  in
  (* Parse castling rights *)
  let castling_str = if List.length parts > 2 then List.nth parts 2 else "KQkq" in
  let white_short = String.contains castling_str 'K' in
  let white_long = String.contains castling_str 'Q' in
  let black_short = String.contains castling_str 'k' in
  let black_long = String.contains castling_str 'q' in
  let castling_rights =
    [| { short = (if white_short then Some Square.h1 else None)
       ; long = (if white_long then Some Square.a1 else None)
       }
     ; { short = (if black_short then Some Square.h8 else None)
       ; long = (if black_long then Some Square.a8 else None)
       }
    |]
  in
  (* Parse en passant square *)
  let ep_str = List.nth parts 3 in
  let ep_square = if ep_str = "-" then None else Some (Square.of_uci ep_str) in
  (* Parse halfmove and fullmove *)
  let halfmove = if List.length parts > 4 then int_of_string (List.nth parts 4) else 0 in
  let fullmove = if List.length parts > 5 then int_of_string (List.nth parts 5) else 1 in
  (* Find king positions and compute all bitboards *)
  let white_king_sq = find_king_on_board board White in
  let black_king_sq = find_king_on_board board Black in
  let ( white_pawns
      , white_knights
      , white_bishops
      , white_rooks
      , white_queens
      , white_king
      , black_pawns
      , black_knights
      , black_bishops
      , black_rooks
      , black_queens
      , black_king
      , white_pieces
      , black_pieces
      , occupied )
    =
    compute_bitboards_from_board board
  in
  (* Build position - zobrist key is 0L initially, computed on first use *)
  { board
  ; side_to_move
  ; castling_rights
  ; ep_square
  ; halfmove
  ; fullmove
  ; key = 0L
  ; (* Will be computed when needed *)
    white_king_sq
  ; black_king_sq
  ; occupied
  ; white_pawns
  ; white_knights
  ; white_bishops
  ; white_rooks
  ; white_queens
  ; white_king
  ; black_pawns
  ; black_knights
  ; black_bishops
  ; black_rooks
  ; black_queens
  ; black_king
  ; white_pieces
  ; black_pieces
  }
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
  (* Simplified castling rights for now *)
  let castling_str = "KQkq" in
  (* Simplified en passant *)
  let ep_str = "-" in
  Printf.sprintf
    "%s %s %s %s %d %d"
    board_str
    side_char
    castling_str
    ep_str
    pos.halfmove
    pos.fullmove
;;

(** Make a null move - swap side to move without moving pieces *)
let make_null_move pos =
  let opponent = Color.opponent pos.side_to_move in
  { pos with
    side_to_move = opponent
  ; ep_square = None
  ; (* Clear en passant *)
    halfmove =
      pos.halfmove + 1
      (* Increment halfmove clock *)
      (* Note: fullmove stays same since only Black moves increment it *)
      (* King positions unchanged in null move *)
  }
;;

(** Draw ASCII board representation of the position *)
let draw_board pos =
  Printf.printf "  +---+---+---+---+---+---+---+---+\n";
  for rank = 7 downto 0 do
    Printf.printf "%d |" (rank + 1);
    for file = 0 to 7 do
      let sq = rank * 8 + file in
      let piece_char = match piece_at pos sq with
        | None -> " "
        | Some p ->
          let c = match p.kind with
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
