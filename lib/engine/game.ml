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
  ; history = [ Position.key position ]
  }
;;

let default () = make (Position.default ())
let of_fen fen = make (Position.of_fen fen)
let position game = game.position
let history game = game.history

let make_move game mv =
  let new_pos = Position.make_move game.position mv in
  let new_key = Position.key new_pos in
  { position = new_pos
  ; checkers = Bitboard.empty
  ; pinned = Bitboard.empty
  ; history = new_key :: game.history
  }
;;

let legal_moves game = Movegen.generate_moves game.position
let legal_moves_from game mask = Movegen.generate_moves_from game.position mask
let to_fen game = Position.to_fen game.position

(** Resolve a move string (coordinate notation like "e2e4"/"e7e8q", or "O-O"/"O-O-O")
    to the matching legal move, so the move carries its correct kind (castle,
    en passant, double push, capture). Returns [None] for malformed or illegal moves. *)
let find_move game str =
  let legal = legal_moves game in
  match str with
  | "O-O" | "0-0" -> List.find_opt (fun mv -> Move.kind mv = Move.ShortCastle) legal
  | "O-O-O" | "0-0-0" -> List.find_opt (fun mv -> Move.kind mv = Move.LongCastle) legal
  | _ ->
    (match Move.of_uci str with
     | exception (Invalid_argument _ | Failure _) -> None
     | parsed ->
       List.find_opt
         (fun mv ->
            Move.from mv = Move.from parsed
            && Move.to_square mv = Move.to_square parsed
            && Move.promotion mv = Move.promotion parsed)
         legal)
;;

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

(** Check if the game is a draw according to FIDE rules: fifty-move rule, threefold
    repetition, insufficient material or stalemate *)
let is_draw game =
  let pos = game.position in
  Position.halfmove pos >= 100
  || is_threefold_repetition game
  || Position.has_insufficient_material pos
  || (legal_moves game = []
      &&
      let side = Position.side_to_move pos in
      let king_sq =
        if side = Types.White
        then Position.white_king_sq pos
        else Position.black_king_sq pos
      in
      Bitboard.is_empty
        (Movegen.compute_attackers_to pos king_sq (Types.Color.opponent side)))
;;
