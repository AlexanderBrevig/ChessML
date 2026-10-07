(** Game - High-level game state management and move history
    
    The current position plus the keys of all earlier positions (for repetition
    detection). Provides move parsing, legal moves and draw detection.
    
    Used by: UCI/XBoard protocols, search initialization, game replay
*)

open Chessml_core

type t =
  { position : Position.t
  ; history : Zobrist.t list (* Stack of position hashes for repetition detection *)
  }

let make position = { position; history = [ Position.key position ] }
let default () = make (Position.default ())
let of_fen fen = make (Position.of_fen fen)
let position game = game.position
let history game = game.history

let make_move game mv =
  let new_pos = Position.make_move game.position mv in
  let new_key = Position.key new_pos in
  { position = new_pos; history = new_key :: game.history }
;;

let legal_moves game = Movegen.generate_moves game.position
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

(** Number of times the current position has occurred in the game (keys include
    side to move, castling rights and usable en passant squares) *)
let occurrences game =
  match game.history with
  | [] -> 0
  | key :: _ -> List.length (List.filter (Int64.equal key) game.history)
;;

(** Has the current position occurred before? *)
let is_repetition game = occurrences game >= 2

(** Has the current position occurred at least three times? *)
let is_threefold_repetition game = occurrences game >= 3

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
