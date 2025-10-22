(** ChessML - Fast and modular chess engine library for OCaml
    
    A complete chess engine implementation featuring bitboard move generation,
    magic bitboards for sliding pieces, alpha-beta search with modern pruning
    techniques, parallel search support, and UCI/XBoard protocol implementations.
    
    Modules: Core (types, bitboards), Engine (search, eval), Protocols (UCI, XBoard)
*)

(* Re-export sublibraries as modules *)
module Core = Chessml_core
module Engine = Chessml_engine
module Protocols = Chessml_protocols

(* Core types - for convenience, re-export at top level *)
module Types = Core.Types
module Square = Core.Square
module Bitboard = Core.Bitboard
module Move = Core.Move
module Piece = Core.Types.Piece
module Color = Core.Types.Color
module File = Core.Types.File
module Rank = Core.Types.Rank
module PieceKind = Core.Types.PieceKind

(* Engine modules - for convenience, re-export at top level *)
module Position = Engine.Position
module Movegen = Engine.Movegen
module Zobrist = Engine.Zobrist
module Game = Engine.Game
module Perft = Engine.Perft
module Eval = Engine.Eval
module Search = Engine.Search
module Config = Engine.Config
module Killers = Engine.Killers
module History = Engine.History
module Pawn_cache = Engine.Pawn_cache
module See = Engine.See
module Piece_tables = Engine.Piece_tables
module Concurrent_tt = Engine.Concurrent_tt
module Parallel_search = Engine.Parallel_search
module Root_split_search = Engine.Root_split_search
module Search_common = Engine.Search_common
module Polyglot = Engine.Polyglot
module Opening_book = Engine.Opening_book
module Pgn_parser = Engine.Pgn_parser

(* Protocol modules - for convenience, re-export at top level *)
module Uci = Protocols.Uci
module Xboard = Protocols.Xboard

(* Re-export commonly used types *)
type color = Core.Types.color =
  | White
  | Black

type piece_kind = Core.Types.piece_kind =
  | Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King

type piece = Core.Types.piece =
  { color : color
  ; kind : piece_kind
  }

(* Common constants *)
let fen_startpos = Engine.Position.fen_startpos
