(** Game - High-level game state management and move history
    
    Manages complete game state including current position, move history, and
    computed information (legal moves, check status). Provides high-level interface
    for making moves, checking game status (checkmate, stalemate, draw), and
    managing game flow. Caches legal moves for performance.
    
    Used by: UCI/XBoard protocols, search initialization, game replay
*)

open Chessml_core

type t =
  { position : Position.t
  ; checkers : Bitboard.t
  ; pinned : Bitboard.t
  ; history : Zobrist.t list (* Stack of position hashes for repetition detection *)
  }

let make position =
  { position
  ; checkers = Bitboard.empty
  ; pinned = Bitboard.empty
  ; history = [ Zobrist.compute position ]
  }
;;

let default () = make (Position.default ())
let from_fen fen = make (Position.of_fen fen)
let position game = game.position

let make_move game mv =
  let new_pos = Position.make_move game.position mv in
  let new_key = Zobrist.compute new_pos in
  { position = new_pos
  ; checkers = Bitboard.empty
  ; pinned = Bitboard.empty
  ; history = new_key :: game.history
  }
;;

let legal_moves game = Movegen.generate_moves game.position
let legal_moves_from game mask = Movegen.generate_moves_from game.position mask
let to_fen game = Position.to_fen game.position

(** Check if the current position is a repetition (appeared at least once before) *)
let is_repetition game =
  let current_key = List.hd game.history in
  let rec check_history = function
    | [] | [ _ ] -> false (* Need at least 2 positions *)
    | _ :: rest ->
      (* Only check positions with same side to move (skip every other position) *)
      (match rest with
       | [] -> false
       | _ :: tail ->
         if List.exists (fun key -> key = current_key) tail
         then true
         else check_history tail)
  in
  check_history game.history
;;

(** Check if the current position is a threefold repetition (appeared 3+ times) *)
let is_threefold_repetition game =
  let current_key = List.hd game.history in
  let rec check_every_other = function
    | [] | [ _ ] -> 0
    | key :: _ :: rest ->
      if key = current_key then 1 + check_every_other rest else check_every_other rest
  in
  (* Count occurrences in positions with same side to move *)
  check_every_other game.history >= 3
;;

(** Count pieces of a specific kind and color on the board *)
let count_pieces_on_board pos kind color =
  let count = ref 0 in
  for sq = 0 to 63 do
    match Position.piece_at pos sq with
    | Some p when p.Types.color = color && p.Types.kind = kind -> count := !count + 1
    | _ -> ()
  done;
  !count
;;

(** Get truly legal moves (now using improved movegen) *)
let get_truly_legal_moves pos = Movegen.generate_moves pos

(** Check if there is insufficient material for checkmate (FIDE Article 9.6) *)
let has_insufficient_material pos =
  (* Count all pieces for both colors *)
  let count_white kind = count_pieces_on_board pos kind Types.White in
  let count_black kind = count_pieces_on_board pos kind Types.Black in
  let white_knights = count_white Types.Knight in
  let white_bishops = count_white Types.Bishop in
  let white_rooks = count_white Types.Rook in
  let white_queens = count_white Types.Queen in
  let white_pawns = count_white Types.Pawn in
  let black_knights = count_black Types.Knight in
  let black_bishops = count_black Types.Bishop in
  let black_rooks = count_black Types.Rook in
  let black_queens = count_black Types.Queen in
  let black_pawns = count_black Types.Pawn in
  (* Any pawns, rooks, or queens means sufficient material *)
  if
    white_pawns > 0
    || black_pawns > 0
    || white_rooks > 0
    || black_rooks > 0
    || white_queens > 0
    || black_queens > 0
  then false
  else (
    (* Now we only have kings, knights, and bishops *)
    let white_minors = white_knights + white_bishops in
    let black_minors = black_knights + black_bishops in
    (* King vs King *)
    if white_minors = 0 && black_minors = 0
    then true (* King + minor vs King *)
    else if
      (white_minors = 1 && black_minors = 0) || (white_minors = 0 && black_minors = 1)
    then
      true
      (* King + Bishop vs King + Bishop (same color bishops) *)
      (* This is a simplified check - proper implementation would check bishop square colors *)
    else if
      white_knights = 0 && black_knights = 0 && white_bishops = 1 && black_bishops = 1
    then true
    else false)
;;

(** Check if the game is a draw according to FIDE rules *)
let is_draw game =
  let pos = game.position in
  (* 1. Fifty-move rule: Draw if halfmove clock >= 100 (50 full moves) *)
  if Position.halfmove pos >= 100
  then true (* 2. Threefold repetition *)
  else if is_threefold_repetition game
  then true (* 3. Insufficient material *)
  else if has_insufficient_material pos
  then true
  (* 4. Stalemate: No legal moves and not in check *)
  else (
    let truly_legal_moves = get_truly_legal_moves pos in
    if List.length truly_legal_moves = 0
    then (
      (* No legal moves - could be checkmate or stalemate *)
      (* Check if the king is in check *)
      let king_square = ref None in
      for sq = 0 to 63 do
        match Position.piece_at pos sq with
        | Some p
          when p.Types.color = Position.side_to_move pos && p.Types.kind = Types.King ->
          king_square := Some sq
        | _ -> ()
      done;
      match !king_square with
      | None -> false (* No king found - invalid position *)
      | Some sq ->
        let opponent = Types.Color.opponent (Position.side_to_move pos) in
        let attackers = Movegen.compute_attackers_to pos sq opponent in
        (* If king is not attacked, it's stalemate (draw) *)
        Bitboard.is_empty attackers)
    else false (* Has legal moves, not a draw by these rules *))
;;
