---
layout: default
title: Basic Endgames
parent: Chess Programming Guide
nav_order: 13
description: "My notes on teaching ChessML the basic mates and king and pawn against king"
permalink: /docs/endgames
---

# Basic endgames

These are my notes on getting ChessML to win the simplest endgames. For the real reference, see the Chess Programming Wiki pages listed at the bottom.

## The problem

With a queen against a lone king, my engine used to wander around until the fifty-move rule ended the game. In a test from 20 random K+Q vs K positions it mated only 12 times, with 2 stalemates; with a rook it mated 4 times in 20, and with bishop and knight never.

The search was not the problem. The evaluation said "+9 pawns" in every position, so all moves looked equally good, and the mate was too far away for the search to see. What was missing was a sense of direction.

## Recognize, then evaluate differently

My first idea was to write a separate routine for each endgame that only considers the "right" moves. I dropped it: a rule like "keep the queen a knight's move from the king" walks straight into stalemate in some positions, and if it filters out the one move that works, no amount of searching can bring it back.

What ChessML does instead (`lib/engine/endgame.ml`): before the normal evaluation, it looks at the material. If it recognizes the endgame, a special evaluation replaces the normal one, and the search still picks the moves. The search keeps avoiding stalemate and blunders; the evaluation only says which positions are progress.

## King and queen (or rook) against king

The score is a large "known win" bonus, plus material, plus:

- a bonus for the lone king being near the edge (0 in the center, highest in the corners)
- a bonus for the two kings being close together

The search then finds the moves that push the king back. The known-win bonus (10000) is far above any normal evaluation and far below mate scores, so a real mate the search finds is still preferred.

From 100 random positions at depth 5, ChessML now mates with the queen every time (in 17 plies on average) and with the rook every time (24 plies).

## King, bishop and knight against king

This is the hard one. Mate is only possible in a corner of the same color as the bishop, and the defender runs to the other corners. ChessML scores the lone king by how many files plus ranks it is from the nearer right corner, with a large weight, plus king closeness.

The shape of that score mattered more than its size. From 100 random positions at 100 ms per move it now mates 96 times; the other 4 hit the fifty-move rule.

## King and pawn against king

Here rules of thumb (the opposition, the key squares, the rook pawn) are famous, but there is a simpler way: compute the answer for every position. There are only about 200,000 (pawn on files a to d, the rest mirrored), so ChessML builds a table of win or draw on first use (`lib/engine/kpk.ml`), working backwards:

1. Mark the obvious ones: a safe promotion is a win; stalemate, or the defender taking an undefended pawn, is a draw.
2. Repeat until nothing changes: with the pawn side to move, a position is won if some move reaches a won position; with the defender to move, it is drawn if some move reaches a drawn one.
3. Anything still undecided is a draw.

It takes about 0.3 seconds. I checked all 331,352 legal positions against the Syzygy tablebase with `scripts/verify_kpk.py`, and they all agree. The opposition comes out of the table by itself: with the white king on e3, pawn on e2 and black king on e6, White to move wins and Black to move draws.

## Pitfalls

- **A score that is flat where progress happens.** I first combined "near the edge" with "near the right diagonal" at equal weights, and along the edge from a wrong corner to a right one the two cancelled exactly, so the search saw no progress and the defender sat in the wrong corner. Check the score along the path you want the king to take.
- **Rewarding every corner in K+B+N vs K.** A general "push to the edge" bonus also rewards the two corners where mate is impossible.
- **Testing on positions that are not won.** A random position can hang the queen or be a draw. Filter those out (ChessML's test harness rejects pieces the lone king can take, and only uses K+P vs K positions the table says are won), or the numbers measure luck.
- **Mixing up "known win" and mate.** If the known-win bonus could reach mate scores, the engine could prefer a big evaluation over an actual mate.

## Sources

- [Chess Programming Wiki: KPK](https://www.chessprogramming.org/KPK) and [Retrograde Analysis](https://www.chessprogramming.org/Retrograde_Analysis)
- [Chess Programming Wiki: KBNK Endgame](https://www.chessprogramming.org/KBNK_Endgame)
- [Stockfish](https://github.com/official-stockfish/Stockfish)'s `endgame.cpp`, for the idea of recognizing endgames by material and evaluating them specially
- [Syzygy tablebases](https://syzygy-tables.info/), used to check the KPK table
- ChessML's code: `lib/engine/endgame.ml`, `lib/engine/kpk.ml`, `test/engine/test_endgame_technique.ml`, `test/engine/endgame_harness.ml`
