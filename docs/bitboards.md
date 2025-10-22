---
layout: default
title: Bitboards
parent: Chess Programming Guide
nav_order: 1
description: "64-bit board representation for efficient chess programming"
permalink: /docs/bitboards
---

# Bitboards

## What Are Bitboards?

A bitboard is a 64-bit integer that represents a chess board. Each bit corresponds to one square on the board. If a bit is `1`, it means something exists on that square (like a piece, or that the square is attacked). If it's `0`, the square is empty or doesn't have that property.

For example, to represent all white pawns on the board, you use one bitboard where each bit tells you whether there's a white pawn on that square.

## Why Use Bitboards?

**Speed.** Bitboards let you operate on all 64 squares simultaneously using a single CPU instruction. Instead of looping through squares one by one, you can:

- Find all pieces of a type in one operation
- Calculate all attacked squares using bitwise operations
- Check if squares are empty or occupied instantly

**Elo Impact:** ~100-200 Elo compared to array-based representations. The speed gain allows you to search deeper in the same amount of time.

## How They Work

### Basic Structure

```
A chess board (a1 = bit 0, h8 = bit 63):

 8  56 57 58 59 60 61 62 63
 7  48 49 50 51 52 53 54 55
 6  40 41 42 43 44 45 46 47
 5  32 33 34 35 36 37 38 39
 4  24 25 26 27 28 29 30 31
 3  16 17 18 19 20 21 22 23
 2   8  9 10 11 12 13 14 15
 1   0  1  2  3  4  5  6  7
    a  b  c  d  e  f  g  h
```

### Key Operations

**Setting a bit (place a piece):**

```ocaml
bitboard ||| (1L <<< square_index)
```

**Clearing a bit (remove a piece):**

```ocaml
bitboard &&& ~~~(1L <<< square_index)
```

**Testing a bit (is there a piece?):**

```ocaml
(bitboard &&& (1L <<< square_index)) <> 0L
```

**Finding pieces (iterate set bits):**

```ocaml
(* Find the least significant bit *)
let lsb = bitboard &&& (-bitboard)
```

### Shifts for Move Generation

Bitboards shine when generating moves. For example, to find all squares white pawns can move to:

```ocaml
(* Single push: shift north by 8 *)
let single_push = (white_pawns <<< 8) &&& empty_squares

(* Double push: shift north by 16, only from rank 2 *)
let double_push =
  ((white_pawns &&& rank_2) <<< 16) &&& empty_squares &&& (empty_squares <<< 8)
```

Attacks are similar:

```ocaml
(* Pawn attacks northeast: shift by 9, but not from h-file *)
let attacks_ne = (white_pawns <<< 9) &&& not_file_a

(* Pawn attacks northwest: shift by 7, but not from a-file *)
let attacks_nw = (white_pawns <<< 7) &&& not_file_h
```

## Implementation Tips

### 1. Create Constant Bitboards

Pre-compute commonly used patterns:

```ocaml
let rank_1 = 0x00000000000000FFL
let rank_8 = 0xFF00000000000000L
let file_a = 0x0101010101010101L
let file_h = 0x8080808080808080L
```

### 2. Use Multiple Bitboards

A complete position needs ~12 bitboards:

- 6 for white pieces (pawn, knight, bishop, rook, queen, king)
- 6 for black pieces
- Often helpful: composite bitboards for "all white pieces", "all pieces", "empty squares"

### 3. Population Count

Modern CPUs have a `popcnt` instruction to count set bits instantly:

```ocaml
(* In OCaml, often exposed via C bindings *)
external popcount : int64 -> int = "popcount_stub"
```

### 4. Bit Scanning

Find the index of the first/last set bit:

```ocaml
(* Find least significant bit index *)
external ctz : int64 -> int = "count_trailing_zeros"

(* Find most significant bit index *)
external clz : int64 -> int = "count_leading_zeros"
```

## Common Patterns

### Intersection (AND)

```ocaml
(* Find white pawns that can capture black pieces *)
let capturable = white_pawn_attacks &&& black_pieces
```

### Union (OR)

```ocaml
(* All pieces *)
let occupied = white_pieces ||| black_pieces
```

### Complement (NOT)

```ocaml
(* Empty squares *)
let empty = ~~~occupied
```

### Subtraction (AND NOT)

```ocaml
(* White pieces that aren't pawns *)
let white_non_pawns = white_pieces &&& ~~~white_pawns
```

## Real-World Example: Knight Moves

Knights are perfect for bitboards because their moves follow a fixed pattern:

```ocaml
(* Pre-computed knight attack table *)
let knight_attacks = Array.init 64 (fun sq ->
  let bb = 1L <<< sq in
  let l1 = (bb >>> 1) &&& not_file_h in
  let l2 = (bb >>> 2) &&& (not_file_h &&& (not_file_h <<< 1)) in
  let r1 = (bb <<< 1) &&& not_file_a in
  let r2 = (bb <<< 2) &&& (not_file_a &&& (not_file_a >>> 1)) in

  let h1 = l1 ||| r1 in
  let h2 = l2 ||| r2 in

  (h1 <<< 16) ||| (h1 >>> 16) ||| (h2 <<< 8) ||| (h2 >>> 8)
)

(* Generate knight moves *)
let generate_knight_moves pos knight_square =
  let attacks = knight_attacks.(knight_square) in
  let targets = attacks &&& (~~~friendly_pieces) in
  (* targets now contains all legal knight destinations *)
  targets
```

## Performance Benefits

1. **Parallel operations:** Check 64 squares in one instruction
2. **Cache-friendly:** Small memory footprint (12-16 bytes for piece positions vs 64+ bytes for arrays)
3. **Branch-free:** Many operations avoid conditionals, helping CPU pipelining
4. **Hardware support:** Modern CPUs have specialized instructions (popcnt, bsf, bsr) for bit operations

## Further Reading

- [Chess Programming Wiki - Bitboards](https://www.chessprogramming.org/Bitboards)
- [Magic Bitboards](magic-bitboards.md) - Advanced technique for sliding pieces
- `lib/core/bitboard.ml` - See the implementation in action
