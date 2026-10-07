---
layout: default
title: Zobrist Hashing
parent: Chess Programming Guide
nav_order: 3
description: "My notes on giving each position a 64-bit key that is cheap to update"
permalink: /docs/zobrist-hashing
---

# Zobrist Hashing

These are my notes on how ChessML turns a position into a single 64-bit number. They describe how I understand it and what ChessML does; the real reference is the [Chess Programming Wiki](https://www.chessprogramming.org/Zobrist_Hashing).

## The idea

An engine often needs to ask "have I seen this position before?": in the [transposition table]({% link docs/transposition-tables.md %}), to spot repetitions, and to look the position up in an [opening book]({% link docs/opening-books.md %}). Comparing whole boards is slow, so each position gets a 64-bit key instead.

Zobrist's scheme: give every possible feature of a position its own random 64-bit number, and XOR together the numbers for the features that are present. The features are:

- a piece of a given kind and color on a given square (12 × 64 = 768 numbers),
- each castling right,
- the en passant file,
- whose turn it is.

What makes this nice is that XOR undoes itself: `k XOR r XOR r = k`. So when a move changes the position, you do not start over. You XOR out what disappeared and XOR in what appeared:

- a quiet move: XOR the piece out on its old square and in on its new one,
- a capture: also XOR the captured piece out on the square where it stood (for en passant that is not the target square, but the square behind it),
- a promotion: XOR the pawn out and the new piece in,
- castling: the king's move plus the rook's move,
- then fix up castling rights, en passant and the side to move the same way.

The key is not unique. There are far more positions than 64-bit numbers, so different positions can share a key. More on that below.

## What ChessML does

### The random numbers

ChessML does not generate its own random numbers. It uses the 781 published "Random64" values from the [Polyglot book format](http://hgm.nubati.net/book_format.html), copied into `lib/engine/polyglot_random.ml`. Because of that, a position's key is also its Polyglot book key, and one key serves the transposition table, repetition detection and the opening book.

The 781 values are laid out as 768 piece-square keys, 4 castling keys (one per right: white short, white long, black short, black long), 8 en passant file keys and 1 side-to-move key. `lib/engine/zobrist.ml` just indexes into that array:

```ocaml
let piece_key piece sq = random64.((64 * polyglot_piece_kind piece) + sq)

let castling_key ~color ~short =
  random64.(768 + (if color = White then 0 else 2) + if short then 0 else 1)
;;

let ep_file_key file = random64.(772 + file)
let white_to_move_key = random64.(780)
```

Two Polyglot details are easy to get backwards:

- The side-to-move key is XORed in when **White** is to move.
- The en passant file is only hashed when a pawn of the side to move stands next to the pawn that just made a double push, so a capture is at least possible. After 1. e4 there is an en passant square, but no black pawn can take, so it does not count. ChessML does this in `Position.ep_key`.

If you generate your own keys instead, use a fixed seed (`Random.init 42`) so the keys are the same on every run, which matters if you ever save anything keyed by them. `Random.bits64 ()` gives all 64 bits; `Random.int64 Int64.max_int` only gives 63.

### Keeping the key up to date

`Position` is immutable, and `make_move` builds the new position by removing and adding pieces through one helper, `toggle` (in `lib/engine/position.ml`). It flips the piece's bit in the bitboards and XORs its Zobrist key in the same step, so adding and removing are the same operation and the key cannot fall behind the board. Shortened:

```ocaml
let toggle pos piece sq =
  let bit = Bitboard.of_square sq in
  let x = Int64.logxor in
  let occupied = x pos.occupied bit in
  let key = x pos.key (Zobrist.piece_key piece sq) in
  (* ... then flip [bit] in the piece and color bitboards ... *)
```

Captures, en passant, promotions and castling are all just calls to `remove` and `put`, which use `toggle`. For example, the capture in `make_move` knows that an en passant victim stands behind the target square:

```ocaml
let captured_sq =
  if Move.is_en_passant mv
  then if side = White then to_sq - 8 else to_sq + 8
  else to_sq
in
Option.iter (remove captured_sq) board.(captured_sq);
```

At the end, `make_move` XORs out the old castling rights, en passant key and side to move and XORs in the new ones. `Position.key` returns the result.

There is also `Position.compute_key`, which builds the key from scratch by looping over the board. Apart from `Position.of_fen`, which uses it once to set up the first key, it is there so tests can check that the incremental key always agrees with it. `test/engine/test_position.ml` walks a whole move tree and compares the two at every node, and `test/engine/test_zobrist.ml` checks the keys against the test positions published with the Polyglot format (the start position must give `0x463b96181691fc9c`, and so on).

### Repetitions

`Game` keeps the list of keys of every position so far, and the search keeps the keys along the current path. A position is a repetition if its key appears earlier, looking back two plies at a time and no further than the last capture or pawn move (`is_repetition` in `lib/engine/search.ml`).

## Collisions

Two different positions with the same key will happen eventually. How rare is it? With n random 64-bit keys, the expected number of pairs that share a key is about n² / 2^65. For a billion positions that is 10^18 / 3.7 × 10^19, roughly 0.03. I find that reassuring, but it also means a long enough run will hit one.

ChessML stores the full key in each transposition table entry and only trusts an entry whose key matches, so a wrong hit needs a full 64-bit collision. Even then, the stored move is only used to order the moves the move generator produced, so it can never make the engine play an illegal move; the worst case is a wrong score for one node.

## Pitfalls

- **En passant hashed when no capture is possible.** Then the same position gets two keys depending on how it was reached, repetitions are missed and book lookups fail. ChessML had this bug.
- **Turn key for the wrong side.** Harmless for your own table, but every Polyglot book lookup misses.
- **Forgetting part of a move.** The en passant victim on the wrong square, the promoted piece never XORed in, the castling rook not moved, or a castling right lost when a rook is captured on its home square. A test comparing the incremental key against a from-scratch key over a perft tree finds these quickly.
- **Trusting a hash move blindly.** After a collision the stored move may not be legal in the current position; check it against the generated moves before playing it.
- **Applying moves from a GUI without matching them.** If "e1g1" is applied as an ordinary king move, the rook and castling keys are never updated. ChessML looks such strings up with `Game.find_move`.

## Sources

- [Chess Programming Wiki: Zobrist Hashing](https://www.chessprogramming.org/Zobrist_Hashing), for the idea
- [Polyglot opening book format](http://hgm.nubati.net/book_format.html), for the key layout, the en passant rule and the test positions
- ChessML's code: [`lib/engine/zobrist.ml`](https://github.com/AlexanderBrevig/ChessML/blob/main/lib/engine/zobrist.ml), [`lib/engine/polyglot_random.ml`](https://github.com/AlexanderBrevig/ChessML/blob/main/lib/engine/polyglot_random.ml), [`lib/engine/position.ml`](https://github.com/AlexanderBrevig/ChessML/blob/main/lib/engine/position.ml), [`test/engine/test_zobrist.ml`](https://github.com/AlexanderBrevig/ChessML/blob/main/test/engine/test_zobrist.ml)
