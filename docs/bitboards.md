---
layout: default
title: Bitboards
parent: Chess Programming Guide
nav_order: 1
description: "My notes on representing the board as 64-bit integers"
permalink: /docs/bitboards
---

# Bitboards

These are my notes from learning how chess engines store the board. They describe how I understand it and what ChessML does; for the real reference, see the [Chess Programming Wiki](https://www.chessprogramming.org/Bitboards).

## The idea

A chessboard has 64 squares and an `int64` has 64 bits, so one integer can answer one yes/no question about every square at once: "is there a white pawn here?", "is this square attacked?". That integer is called a bitboard.

ChessML numbers the squares a1 = 0, b1 = 1, … h1 = 7, a2 = 8, … h8 = 63, so bit `rank * 8 + file` is the square:

```
 8 | 56 57 58 59 60 61 62 63
 7 | 48 49 50 51 52 53 54 55
 6 | 40 41 42 43 44 45 46 47
 5 | 32 33 34 35 36 37 38 39
 4 | 24 25 26 27 28 29 30 31
 3 | 16 17 18 19 20 21 22 23
 2 |  8  9 10 11 12 13 14 15
 1 |  0  1  2  3  4  5  6  7
     a  b  c  d  e  f  g  h
```

One bitboard per piece type and color (12 in total) describes where everything stands. ChessML's `Position` keeps those 12, plus one per color, one for all occupied squares, and an ordinary 64-entry array for answering "what is on e4?". Keeping both means they must never disagree. In ChessML, `make_move` (`lib/engine/position.ml`) changes the array and routes every piece it adds or removes through one helper, `toggle`, which flips the bit in the piece, color and occupancy bitboards and updates the hash key in the same step. Having a single place for it is what keeps them in sync.

## Why bother

What sold me on it: a question about a whole set of squares becomes one or two integer operations instead of a loop over 64 squares. "Which of my pawns can capture something?" is "squares my pawns attack" AND "squares holding enemy pieces". I have not measured how much faster ChessML is because of this, so I won't put a number on it.

## The operations I use

OCaml has no operator syntax for `int64` bit operations, so the code uses the `Int64` functions. These are the real definitions from `lib/core/bitboard.ml`:

```ocaml
let of_square sq = Int64.shift_left 1L sq               (* a bitboard with one bit set *)
let set bb sq = Int64.logor bb (of_square sq)           (* put something on sq *)
let clear bb sq = Int64.logand bb (Int64.lognot (of_square sq))
let contains bb sq = Int64.logand bb (of_square sq) <> 0L
```

Combining sets is plain bit logic:

| Meaning | Operation |
| --- | --- |
| all pieces | `Int64.logor white_pieces black_pieces` |
| empty squares | `Int64.lognot occupied` |
| white pieces that are not pawns | `Int64.logand white_pieces (Int64.lognot white_pawns)` |
| pawn captures | `Int64.logand pawn_attacks black_pieces` |

## Walking over the set bits

To do something for each piece, take the lowest set bit, handle it, clear it, repeat. Two tricks make this cheap:

- `b land (b - 1)` clears the lowest set bit (the borrow ripples up to it and stops).
- "count trailing zeros" gives the index of the lowest set bit. Most CPUs have an instruction for it.

OCaml does not expose that instruction directly, so ChessML calls a few lines of C (`lib/core/bitboard_stubs.c`, using the compiler's `__builtin_ctzll`, `__builtin_clzll` and `__builtin_popcountll`). This is ChessML's loop, slightly shortened:

```ocaml
let iter f bb =
  let rec loop b =
    if b <> 0L then (
      f (find_lsb_fast b);                     (* index of the lowest set bit *)
      loop (Int64.logand b (Int64.sub b 1L)))  (* clear it and continue *)
  in
  loop bb
```

## Shifting a whole set

Moving every bit one rank up is `Int64.shift_left bb 8`. Moving one file sideways is a shift by 1, but then pieces on the edge wrap around to the other side of the board, so the result has to be masked. A shift east must not land on the a-file, and a shift west must not land on the h-file:

```ocaml
let east bb = Int64.logand (Int64.shift_left bb 1) not_file_a
let west bb = Int64.logand (Int64.shift_right_logical bb 1) not_file_h
```

Engines use this to generate, say, all single pawn pushes at once (`north pawns` AND `empty`). ChessML does not do that: its move generator loops over the pawns one by one and looks up each pawn's targets. Set-wise generation is on my list of things to try.

## Attack tables for knights and kings

A knight or king on a given square always attacks the same squares, whatever else is on the board. So ChessML computes all 64 attack sets once at startup and looks them up afterwards. It builds them the slow, obvious way, which is fine because it only runs once (shortened from `lib/engine/movegen.ml`):

```ocaml
(* for each square: try all eight knight jumps and keep those that stay on the board *)
List.iter
  (fun (df, dr) ->
     let f = file + df and r = rank + dr in
     if f >= 0 && f < 8 && r >= 0 && r < 8 then bb := Bitboard.set !bb (f + (r * 8)))
  [ 2, 1; 2, -1; -2, 1; -2, -1; 1, 2; 1, -2; -1, 2; -1, -2 ]
```

Sliding pieces (bishops, rooks, queens) are harder, because what they attack depends on what is in the way. That needs [magic bitboards]({% link docs/magic-bitboards.md %}).

## Pitfalls

- **Two orientations in one program.** Tables are usually typed in with a8 first (as you would print a board) while squares count from a1, so one side ends up reading the table upside down without any test failing. ChessML had exactly this bug. Write the numbering down and test one known square.
- **Sideways shifts wrap around.** Shifting by 1 moves an h-file piece onto the a-file of the next rank; mask the result.

## Sources

- [Chess Programming Wiki: Bitboards](https://www.chessprogramming.org/Bitboards), the starting point for nearly everything on this page
- [Chess Programming Wiki: Square Mapping Considerations](https://www.chessprogramming.org/Square_Mapping_Considerations), on why a1 = 0 is a choice and not a law
- ChessML's code: `lib/core/bitboard.ml`, `lib/core/bitboard_stubs.c`, `lib/engine/position.ml`, `lib/engine/movegen.ml`
