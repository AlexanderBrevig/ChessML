---
layout: default
title: Developer Guide
nav_order: 3
description: "How the ChessML code is organized and how I work on it"
permalink: /docs/developer-guide
---

# Developer guide

How the code is laid out, for when I come back to it after a break (and for anyone curious). The technique pages under [Chess Programming Guide]({% link docs/chess-programming-guide.md %}) explain the ideas; this page is only about where things live.

## Libraries

The code is split into three dune libraries, re-exported together as `Chessml`:

**`lib/core`**: the basic types

- `Types` (colors, pieces), `Square` (a1 = 0 … h8 = 63), `Move` (from, to and a move kind)
- `Bitboard`, with three C helpers in `bitboard_stubs.c` (lowest bit, highest bit, bit count)

**`lib/engine`**: everything about playing chess

| Module | What it does |
| --- | --- |
| `Position` | Immutable position: a 64-square array, bitboards per piece and color, and the hash key, all kept in sync by `make_move` |
| `Zobrist`, `Polyglot_random` | The hash keys (the published Polyglot ones, so the position key is also the book key) |
| `Magic`, `Movegen` | Attack tables and legal move generation; `Movegen.attackers_to` is the one function everything uses to ask "who attacks this square?" |
| `Game` | A position plus the keys of earlier positions (for repetitions); `Game.find_move` turns `"e2e4"` or `"O-O"` into a legal move |
| `Eval`, `Eval_pawn_structure`, `Eval_pieces`, `Eval_king_safety`, `Eval_endgame`, `Piece_tables`, `Pawn_cache` | The evaluation, split by topic |
| `See` | Static exchange evaluation |
| `Search`, `Search_common`, `Score`, `Killers`, `History`, `Countermoves` | The search and its helpers; mate scores live in `Score` |
| `Polyglot`, `Opening_book`, `Pgn_parser` | Reading books and parsing PGN (for building books) |
| `Config` | Search options that the protocols can change |

**`lib/protocols`**: `Uci` and `Xboard`, plus `Protocol_common` for what they share (options, book access, time budgeting). The UCI search runs on its own thread so `stop` works; XBoard thinks synchronously.

The programs in `bin/` are thin wrappers: the two engines, `create_book`, and `play_from`.

## The board: array and bitboards

`Position` stores the board twice: a 64-entry array answers "what is on e4?" in one lookup, and bitboards answer "where are all the white knights?" in one lookup. Keeping two copies is only safe if nothing can update one without the other, so every piece change in `make_move` goes through a single helper (`toggle`) that also updates the hash key. Positions are immutable: `make_move` returns a new position and copies the array.

## Tests

- `test/core`: bitboards, squares, moves, types
- `test/engine`: perft (move generation against published node counts), position and hash consistency, evaluation symmetry, SEE, search (mates, repetitions), opening book and PGN parsing, special moves
- `test/protocols`: UCI and XBoard sessions, fed one command at a time

Tests catch bugs, but they say little about strength. For that I play games, see "Strength" in the [README](https://github.com/AlexanderBrevig/ChessML#strength).

## Working on it

```bash
dune build                       # debug build
dune build --profile=release     # use this for playing and benchmarks
dune runtest                     # all tests, a few seconds
just format                      # ocamlformat 0.27.0
dune exec --profile=release examples/search_bench.exe
```

Contributions are welcome, see [CONTRIBUTING.md](https://github.com/AlexanderBrevig/ChessML/blob/main/CONTRIBUTING.md).

## Things I would like to try

- Generating pawn moves set-wise instead of one pawn at a time
- A tapered evaluation and some endgame technique
- Tuning the evaluation against real games
- Lazy SMP (several search threads sharing one table)

## Other resources

- [Chess Programming Wiki](https://www.chessprogramming.org/)
- [Stockfish](https://github.com/official-stockfish/Stockfish), the strongest open-source engine
- [Real World OCaml](https://dev.realworldocaml.org/) and the [OCaml manual](https://ocaml.org/manual/)
