---
layout: default
title: Opening Books
parent: Chess Programming Guide
nav_order: 12
description: "My notes on the Polyglot book format and how ChessML builds and reads its book"
permalink: /docs/opening-books
---

# Opening Books

These are my notes on opening books, mostly the Polyglot format that ChessML reads and writes. The real references are the [Polyglot book format description](http://hgm.nubati.net/book_format.html) and the [Chess Programming Wiki](https://www.chessprogramming.org/Opening_Book).

## The idea

In the opening the engine has to search positions that have been played thousands of times. A book is a table from "position" to "moves that were played here, with how much to prefer each". While the current position is in the book, the engine plays a move from it without searching; once it is out, it searches as usual.

Two things make a book worth having for me. The engine saves its clock in the first moves, and it varies its play: from the start position it does not always go 1.e4 e5 2.Nf3 Nc6 3.Bb5 (the Ruy Lopez), because it picks among the book moves at random, weighted by preference.

## The Polyglot format

A Polyglot book is a file of 16-byte entries, all big-endian, sorted by key:

| Bytes | Field | Meaning |
| --- | --- | --- |
| 8 | key | hash of the position |
| 2 | move | the move, encoded as below |
| 2 | weight | how much to prefer this move in this position |
| 4 | learn | free for engines that learn; usually 0 |

A position with several book moves has several consecutive entries with the same key.

### The key

The key is a [Zobrist hash]({% link docs/zobrist-hashing.md %}) built from a fixed table of 781 published 64-bit random numbers. Using exactly that table is what lets any program read any Polyglot book. The key is the XOR of:

- **one number per piece**, at index `64 * kind + square`. `kind` is `2 * piece + (1 if white else 0)`, with pawn, knight, bishop, rook, queen, king = 0 to 5. So the black pawn is 0, the white pawn 1, and so on up to the white king at 11. Squares are a1 = 0 to h8 = 63, the same as ChessML.
- **one number per castling right** still held: 768 White short, 769 White long, 770 Black short, 771 Black long.
- **one number for the en passant file** (772 + file), but only if a pawn of the side to move can actually make the capture. A double push alone is not enough.
- **number 780 if White is to move.**

ChessML's `Zobrist` uses these tables for its normal position key, so `Position.key` is the book key as well as the transposition-table key. This is `Zobrist.polyglot_piece_kind`:

```ocaml
let polyglot_piece_kind piece =
  let kind =
    match piece.kind with
    | Pawn -> 0 | Knight -> 1 | Bishop -> 2 | Rook -> 3 | Queen -> 4 | King -> 5
  in
  (2 * kind) + if piece.color = White then 1 else 0
```

The format page lists test keys; the start position must hash to `0x463b96181691fc9c`, and `test/engine/test_zobrist.ml` checks it and the others.

### The move

The 16-bit move has the target square in bits 0-5, the origin square in bits 6-11 and the promotion piece in bits 12-14 (1 = knight up to 4 = queen, 0 = none). Castling is written as the king capturing its own rook: e1h1, e1a1, e8h8, e8a8. From `lib/engine/polyglot.ml`:

```ocaml
let encode_move (mv : Move.t) : int =
  let from_sq = Move.from mv in
  let to_sq =
    match Move.kind mv with
    | Move.ShortCastle -> from_sq + 3      (* e1 -> h1 *)
    | Move.LongCastle -> from_sq - 4       (* e1 -> a1 *)
    | _ -> Move.to_square mv
  in
  (* ... promo is 0 or 1-4 ... *)
  to_sq lor (from_sq lsl 6) lor (promo lsl 12)
```

For decoding, ChessML does not try to rebuild the move from the bits. `Polyglot.decode_move` generates the legal moves and returns the one that encodes to the same number, or `None`. That way a book move is always legal and carries the right move kind (castling, en passant, promotion).

### The weight

The weight only means something relative to the other moves of the same position: the engine picks a move with probability weight / sum of weights. It is 16 bits, so raw game counts do not fit; a popular position can be reached in more than 65535 games.

## How ChessML builds its book

`bin/create_book.ml` reads the PGNMentor collection with `Pgn_parser`, using `domainslib` workers, and counts how often each move was played in each position over the first 20 plies (`max_ply`) of every game. Then, per position:

- a move is kept if it was played in at least 3 games (`min_game_count`) and in at least 5% of the games that reached the position (`min_share`);
- its weight is `max 1 (count * 65535 / best)`, where `best` is the count of the most played move in that position. The top move gets 65535, the rest are linear in their count.

The entries are sorted with `Int64.unsigned_compare` and written with `learn = 0`.

Two measured notes. An earlier version used logarithmic weights, which flattened the choices so much that fringe moves like 1.b4 were picked often; the linear weights beat it 42-23 with 35 draws in a 100-game match at 10s+0.1s. And PGNMentor groups its files by opening, so the move frequencies reflect what is in that collection, not what is popular in general.

## How ChessML reads it

`Opening_book.open_book` only notes the file name and the number of entries. A probe binary-searches the file for the key, seeking on disk at every step and comparing keys with `Int64.unsigned_compare`, then walks back to the first matching entry and collects all entries with that key. `Opening_book.probe` decodes each move against the legal moves and drops the ones that do not match.

`Protocol_common.book_move` then picks one at random, weighted, whenever `OwnBook` is on and the position is in the book. Both the UCI and the XBoard front-end do this. There is no move-number cutoff: in practice the book ends by itself after 20 plies, because nothing deeper was counted.

ChessML looks for the book in the paths from `Config.get_book_paths`, in this order: `book.bin` in the current directory, `$XDG_DATA_HOME/chessml/book.bin` (with `~/.local/share` when the variable is unset), `~/.chessml/book.bin`, `/usr/local/share/chessml/book.bin` and `/usr/share/chessml/book.bin`. It reads the environment with `Sys.getenv_opt`; plain `Sys.getenv` raises `Not_found` when a variable is unset.

I checked the reader against python-chess's Polyglot reader on the same book.

## Pitfalls

- **Swapped move fields.** The target square is in the low bits, the origin above it. Getting this backwards still produces plausible numbers, but no move matches. ChessML had this bug.
- **Castling as e1g1.** Polyglot writes castling as king takes rook (e1h1); looking for e1g1 never finds it.
- **Signed keys.** The file is sorted by key as an unsigned 64-bit number. OCaml's `Int64.compare` is signed, so a binary search with it misses many positions. ChessML had this bug too.
- **Turn and en passant keys.** The turn number is XORed in when *White* is to move, and the en passant file only when a capture is really possible. Either mistake changes every key.
- **Playing the move string blindly.** Decoding a book move straight into a move without matching it against the legal moves loses castling and en passant semantics, and an unlucky key collision can produce an illegal move.

## Sources

- [Polyglot book format](http://hgm.nubati.net/book_format.html) by H.G. Muller, the specification ChessML follows (keys, move encoding, random numbers, test keys)
- [Chess Programming Wiki: PolyGlot](https://www.chessprogramming.org/PolyGlot)
- [Chess Programming Wiki: Opening Book](https://www.chessprogramming.org/Opening_Book)
- [python-chess](https://python-chess.readthedocs.io/en/latest/polyglot.html), used to check ChessML's reader
- ChessML's code: `lib/engine/polyglot.ml`, `lib/engine/opening_book.ml`, `lib/engine/zobrist.ml`, `lib/engine/config.ml`, `lib/protocols/protocol_common.ml`, `bin/create_book.ml`
