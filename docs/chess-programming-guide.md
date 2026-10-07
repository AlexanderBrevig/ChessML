---
layout: default
title: Chess Programming Guide
nav_order: 2
has_children: true
description: "My study notes on the techniques used in ChessML"
permalink: /docs/chess-programming-guide
---

# Chess Programming Guide

These pages are my notes from building ChessML, a chess engine I write in OCaml for fun and to learn. Each page explains one technique the way I understand it and then shows what ChessML actually does, including its shortcuts. I am a novice at this, so treat them as study notes, not as a reference; the real reference is the [Chess Programming Wiki](https://www.chessprogramming.org/), and each page links the articles it leans on.

I have not measured how much each technique is worth in ChessML, so the pages do not put numbers on it. The only strength figure I have is the rough estimate in the [README](https://github.com/AlexanderBrevig/ChessML#strength).

## Representing the board

- [Bitboards]({% link docs/bitboards.md %}): the board as 64-bit integers, one bit per square, and the handful of bit tricks that make them useful.
- [Magic Bitboards]({% link docs/magic-bitboards.md %}): how rook and bishop attacks, which depend on what is in the way, become a single table lookup.
- [Zobrist Hashing]({% link docs/zobrist-hashing.md %}): a 64-bit key per position that is cheap to update move by move. ChessML uses the Polyglot keys, so the same key works for the opening book.

## Searching

- [Alpha-Beta Pruning]({% link docs/alpha-beta-pruning.md %}): searching the game tree while skipping moves that cannot change the result. With good move ordering it visits roughly the square root of the positions plain minimax would, which is why everything else on this list matters.
- [Transposition Tables]({% link docs/transposition-tables.md %}): remembering positions already searched, since the same position is often reached by different move orders.
- [Quiescence Search]({% link docs/quiescence-search.md %}): continuing with captures at the end of the search, so the engine does not stop counting in the middle of an exchange.
- [Null Move Pruning]({% link docs/null-move-pruning.md %}): if passing the turn still leaves me winning, the position is probably good enough to stop searching.
- [Late Move Reductions]({% link docs/late-move-reductions.md %}): searching the moves that come late in the ordering less deeply, and re-searching if one turns out to be good.

## Choosing which move to try first

- [Move Ordering]({% link docs/move-ordering.md %}): the hash move, captures, killer moves, history and countermoves, in that kind of order. Alpha-beta only prunes well if the best move tends to come first.
- [Static Exchange Evaluation]({% link docs/static-exchange-evaluation.md %}): working out whether a capture wins or loses material without searching it.

## Judging a position

- [Evaluation Function]({% link docs/evaluation-function.md %}): turning a position into a score: material, piece-square tables, pawn structure, king safety and some endgame knowledge.

## Openings

- [Opening Books]({% link docs/opening-books.md %}): the Polyglot book format and how ChessML builds its own book from a collection of games.

## The order I would read them in

If you are new to this too, this is the order that made sense to me:

1. [Bitboards]({% link docs/bitboards.md %})
2. [Alpha-Beta Pruning]({% link docs/alpha-beta-pruning.md %})
3. [Evaluation Function]({% link docs/evaluation-function.md %})
4. [Quiescence Search]({% link docs/quiescence-search.md %})
5. [Move Ordering]({% link docs/move-ordering.md %})
6. [Zobrist Hashing]({% link docs/zobrist-hashing.md %}) and [Transposition Tables]({% link docs/transposition-tables.md %})
7. [Magic Bitboards]({% link docs/magic-bitboards.md %})
8. [Static Exchange Evaluation]({% link docs/static-exchange-evaluation.md %})
9. [Null Move Pruning]({% link docs/null-move-pruning.md %}) and [Late Move Reductions]({% link docs/late-move-reductions.md %})
10. [Opening Books]({% link docs/opening-books.md %})

For how the code itself is organized, see the [Developer Guide]({% link docs/README.md %}).

## Sources

- [Chess Programming Wiki](https://www.chessprogramming.org/), where nearly everything in these notes comes from
- [Chess Programming Wiki: Alpha-Beta](https://www.chessprogramming.org/Alpha-Beta), for the square-root figure
- [ChessML on GitHub](https://github.com/AlexanderBrevig/ChessML)
