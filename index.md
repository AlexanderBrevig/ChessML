---
layout: home
title: Home
nav_order: 1
description: "Notes I wrote while learning chess programming by building ChessML in OCaml"
permalink: /
---

# ChessML notes

ChessML is a chess engine I am writing in OCaml for fun and to learn. These pages are my notes on the techniques it uses: what I understood about each one, how ChessML does it (including the parts it does crudely or not at all), and the mistakes worth warning others about.

They are not a reference. I am a novice at this, and where I explain something you should trust the [Chess Programming Wiki](https://www.chessprogramming.org/) and the engines it links to over me. Each page lists the sources it leans on.

## The pages

Representing the board:

- [Bitboards]({% link docs/bitboards.md %}): the board as 64-bit integers
- [Magic bitboards]({% link docs/magic-bitboards.md %}): attacks of sliding pieces by table lookup
- [Zobrist hashing]({% link docs/zobrist-hashing.md %}): a 64-bit key for every position

Searching:

- [Alpha-beta pruning]({% link docs/alpha-beta-pruning.md %}): the core search
- [Transposition tables]({% link docs/transposition-tables.md %}): remembering positions already searched
- [Quiescence search]({% link docs/quiescence-search.md %}): not stopping in the middle of a capture sequence
- [Null move pruning]({% link docs/null-move-pruning.md %}): passing to prove a position is good enough
- [Late move reductions]({% link docs/late-move-reductions.md %}): searching unlikely moves less deeply
- [Move ordering]({% link docs/move-ordering.md %}): why the order of moves matters so much

Judging positions:

- [Static exchange evaluation]({% link docs/static-exchange-evaluation.md %}): is this capture safe?
- [Evaluation function]({% link docs/evaluation-function.md %}): turning a position into a number
- [Opening books]({% link docs/opening-books.md %}): playing the first moves from a book
- [Basic endgames]({% link docs/endgames.md %}): queen, rook, bishop and knight mates, and an exact king and pawn table

The [overview page]({% link docs/chess-programming-guide.md %}) has a few lines on each, and the [developer guide]({% link docs/README.md %}) describes how the code is organized.

## How strong is it?

Not very, by engine standards: roughly 1850–2000 on Stockfish's limited-strength scale, measured with a few hundred fast games, so take it as a ballpark. The [README](https://github.com/AlexanderBrevig/ChessML#strength) explains how I measured it.

## Links

- [Source code](https://github.com/AlexanderBrevig/ChessML)
- [Contributing](https://github.com/AlexanderBrevig/ChessML/blob/main/CONTRIBUTING.md)
