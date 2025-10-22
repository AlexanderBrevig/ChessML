---
layout: default
title: Magic Bitboards
parent: Chess Programming Guide
nav_order: 2
description: "Fast sliding piece attack generation using magic numbers"
permalink: /docs/magic-bitboards
---

# Magic Bitboards

## What Are Magic Bitboards?

Magic bitboards are a technique for rapidly generating sliding piece (rook, bishop, queen) attacks using pre-computed lookup tables. They use "magic numbers" and bit manipulation to map occupancy patterns to attack bitboards in constant time.

**Elo Impact:** ~50-100 Elo compared to slower attack generation methods. The speed gain allows searching more nodes per second.

## Why They're Needed

Sliding pieces (rooks, bishops, queens) can move different distances depending on blockers:

```
Rook on e4, no blockers:
████████
░░░░█░░░  Can move to any square
░░░░█░░░  on rank or file
░░░░█░░░
███▓████  (█ = can reach)
░░░░█░░░
░░░░█░░░
░░░░█░░░
████████

Rook on e4, with blockers:
░░░░░░░░
░░░░█░░░  Blocked by pieces on
░░░░█░░░  c4, e6, e2, g4
░░░░█░░░
░░█▓████  (█ = can reach, ░ = blocked)
░░░░█░░░
░░░░░░░░
░░░░░░░░
```

Computing attacks on-the-fly is slow. You need to:

1. Check each direction
2. Stop at first blocker
3. Build attack bitboard

Magic bitboards let you do this in **one memory lookup** instead!

## The Core Idea

For each square and each piece type:

1. Pre-compute **all possible blocker configurations**
2. Pre-compute **attack bitboard** for each configuration
3. Find a **magic number** that maps configurations → unique indices
4. Store attacks in a **lookup table**

At runtime:

```ocaml
let attacks = magic_table.(square).(hash(blockers))
```

**One lookup, instant result!**

## How Magic Numbers Work

A magic number `M` and shift `S` transform a blocker pattern into a table index:

```ocaml
let index blockers magic shift =
  let product = Int64.mul blockers magic in
  let shifted = Int64.shift_right_logical product shift in
  Int64.to_int shifted
```

The magic number is chosen so that **different blocker patterns produce different indices** (no collisions in the table).

## Relevant Occupancy

Not all squares matter for blocking. For a rook on e4:

```
Relevant squares (affect attacks):
░░░░░░░░
░░░░█░░░  Only squares between
░░░░█░░░  the rook and board edges
░░░░█░░░  matter as blockers
░░█▓█░█░  (edges don't block)
░░░░█░░░
░░░░█░░░
░░░░░░░░
░░░░░░░░
```

**Why exclude edges?** A piece on the edge is already at the limit—doesn't matter if there's another blocker beyond it.

This reduces table size significantly!

## Pre-Computation Process

### 1. Generate Relevant Occupancy Mask

```ocaml
let rook_mask square =
  let rank = square / 8 in
  let file = square mod 8 in
  let mask = ref 0L in

  (* North (exclude rank 7) *)
  for r = rank + 1 to 6 do
    mask := Bitboard.set !mask (r * 8 + file)
  done;

  (* South (exclude rank 0) *)
  for r = 1 to rank - 1 do
    mask := Bitboard.set !mask (r * 8 + file)
  done;

  (* East (exclude file 7) *)
  for f = file + 1 to 6 do
    mask := Bitboard.set !mask (rank * 8 + f)
  done;

  (* West (exclude file 0) *)
  for f = 1 to file - 1 do
    mask := Bitboard.set !mask (rank * 8 + f)
  done;

  !mask
```

### 2. Generate All Blocker Configurations

```ocaml
let generate_blockers mask =
  let bits = Bitboard.to_list mask in
  let n = List.length bits in
  let configs = ref [] in

  (* Generate all 2^n subsets *)
  for i = 0 to (1 lsl n) - 1 do
    let blockers = ref 0L in
    List.iteri (fun j bit ->
      if (i land (1 lsl j)) <> 0 then
        blockers := Bitboard.set !blockers bit
    ) bits;
    configs := !blockers :: !configs
  done;

  !configs
```

For a rook, typically 10-12 relevant squares → 1024-4096 configurations.

### 3. Compute Attacks for Configuration

```ocaml
let compute_rook_attacks square blockers =
  let attacks = ref 0L in
  let rank = square / 8 in
  let file = square mod 8 in

  (* North *)
  for r = rank + 1 to 7 do
    let sq = r * 8 + file in
    attacks := Bitboard.set !attacks sq;
    if Bitboard.contains blockers sq then break
  done;

  (* South, East, West similarly... *)

  !attacks
```

### 4. Find Magic Number

This is the hard part! You need to find a magic number that maps all blocker configurations to unique indices without collisions.

```ocaml
let find_magic square mask shift =
  let blockers = generate_blockers mask in
  let attempts = ref 0 in

  while !attempts < 100_000_000 do
    attempts := !attempts + 1;
    let magic = random_sparse_64bit () in  (* Random with few bits *)
    let table = Array.make (1 lsl shift) None in
    let success = ref true in

    List.iter (fun blocker_config ->
      let attacks = compute_attacks square blocker_config in
      let idx = magic_index blocker_config magic shift in

      match table.(idx) with
      | None -> table.(idx) <- Some attacks
      | Some existing_attacks ->
        if attacks <> existing_attacks then
          success := false  (* Collision! *)
    ) blockers;

    if !success then
      return (magic, table)
  done;

  failwith "Couldn't find magic number"
```

**Random sparse numbers** (few 1-bits) work better as magics. This is empirically discovered.

### 5. Build Lookup Tables

```ocaml
type magic_entry = {
  mask: bitboard;
  magic: int64;
  shift: int;
  attacks: bitboard array;
}

let rook_magics = Array.init 64 (fun sq ->
  let mask = rook_mask sq in
  let shift = 64 - (Bitboard.population mask) in
  let magic, attacks = find_magic sq mask shift in
  { mask; magic; shift; attacks }
)
```

## Runtime Usage

At runtime, attack generation is trivial:

```ocaml
let rook_attacks square occupied =
  let magic = rook_magics.(square) in

  (* Get relevant blockers *)
  let blockers = Int64.logand occupied magic.mask in

  (* Hash to index *)
  let product = Int64.mul blockers magic.magic in
  let index = Int64.to_int (Int64.shift_right_logical product magic.shift) in

  (* Lookup attacks *)
  magic.attacks.(index)
```

**Total: ~5 operations!** Compare this to checking each square in each direction.

## Bishop Magic Bitboards

Bishops work identically, just with diagonal masks:

```ocaml
let bishop_mask square =
  let rank = square / 8 in
  let file = square mod 8 in
  let mask = ref 0L in

  (* Northeast diagonal (exclude edges) *)
  let r, f = ref (rank + 1), ref (file + 1) in
  while !r <= 6 && !f <= 6 do
    mask := Bitboard.set !mask (!r * 8 + !f);
    incr r; incr f
  done;

  (* Northwest, Southeast, Southwest similarly... *)

  !mask
```

Bishops typically have 7-9 relevant squares → 128-512 configurations.

## Queen Attacks

Queens combine rook and bishop attacks:

```ocaml
let queen_attacks square occupied =
  let rook_atk = rook_attacks square occupied in
  let bishop_atk = bishop_attacks square occupied in
  Int64.logor rook_atk bishop_atk
```

No separate magic needed—just combine the results!

## Pre-Computed Magic Numbers

Finding magics is slow (~minutes), so engines **hard-code** them:

```ocaml
let rook_magics = [|
  0x0080001020400080L;  (* a1 *)
  0x0040001000200040L;  (* b1 *)
  0x0080081000200080L;  (* c1 *)
  (* ... 61 more ... *)
|]

let bishop_magics = [|
  0x0002020202020200L;  (* a1 *)
  0x0002020202020000L;  (* b1 *)
  (* ... 61 more ... *)
|]
```

These are discovered once during development and never change.

## Memory Usage

**Rook tables:** ~800 KB (64 squares × ~12 KB per square)
**Bishop tables:** ~40 KB (64 squares × ~0.6 KB per square)
**Total:** ~1 MB

This is tiny by modern standards and fits in L2/L3 cache!

## Performance Comparison

| Method                 | Operations per Lookup | Relative Speed |
| ---------------------- | --------------------- | -------------- |
| Classical (loops)      | ~20-40                | 1x             |
| Magic bitboards        | ~5-8                  | 5-8x faster    |
| Hyperbola Quintessence | ~15-20                | 2-3x faster    |

Magic bitboards are the fastest general-purpose method.

## Complete Implementation

```ocaml
module Magic = struct
  type magic_entry = {
    mask: int64;
    magic: int64;
    shift: int;
    attacks: int64 array;
  }

  let rook_magics = Array.make 64 {
    mask = 0L; magic = 0L; shift = 0; attacks = [||]
  }

  let bishop_magics = Array.make 64 {
    mask = 0L; magic = 0L; shift = 0; attacks = [||]
  }

  (* Initialize at startup *)
  let init () =
    load_precomputed_magics ();
    generate_attack_tables ()

  (* Fast attack lookup *)
  let rook_attacks square occupied =
    let entry = rook_magics.(square) in
    let blockers = Int64.logand occupied entry.mask in
    let hash = Int64.mul blockers entry.magic in
    let index = Int64.to_int (Int64.shift_right_logical hash entry.shift) in
    entry.attacks.(index)

  let bishop_attacks square occupied =
    let entry = bishop_magics.(square) in
    let blockers = Int64.logand occupied entry.mask in
    let hash = Int64.mul blockers entry.magic in
    let index = Int64.to_int (Int64.shift_right_logical hash entry.shift) in
    entry.attacks.(index)

  let queen_attacks square occupied =
    Int64.logor
      (rook_attacks square occupied)
      (bishop_attacks square occupied)
end
```

## Common Pitfalls

### 1. Including Edge Squares in Mask

```ocaml
(* Wrong: Include rank 7 *)
for r = rank + 1 to 7 do ...

(* Correct: Exclude rank 7 *)
for r = rank + 1 to 6 do ...
```

Edge squares don't change attacks, so excluding them reduces table size.

### 2. Wrong Shift Calculation

```ocaml
(* Wrong: *)
let shift = 64 - num_relevant_squares in

(* Correct: Shift is table size dependent *)
let shift = 64 - (Bitboard.population mask) in
```

### 3. Not Checking for Collisions

When finding magics, verify no collisions occur:

```ocaml
(* Wrong: Assume magic works *)
let magic = random_number () in

(* Correct: Test all configurations *)
let is_valid = test_all_blocker_configs magic mask in
if is_valid then use_magic else try_another
```

### 4. Forgetting to Initialize

```ocaml
(* Wrong: Use before init *)
let attacks = Magic.rook_attacks square occupied in

(* Correct: Initialize first *)
let () = Magic.init () in
let attacks = Magic.rook_attacks square occupied in
```

## Alternative: Fancy Magic Bitboards

"Fancy" magic bitboards don't separate tables per square—they use one giant shared table:

```ocaml
let shared_attacks = Array.make 107648 0L  (* Total of all entries *)

let rook_offsets = [| 0; 4096; 6144; ... |]  (* Where each square starts *)
```

**Pros:** Slightly faster (one array lookup)
**Cons:** More complex initialization

Most engines use regular magic bitboards for simplicity.

## Further Reading

- [Bitboards](bitboards.md) - Foundation for magic bitboards
- [Chess Programming Wiki - Magic Bitboards](https://www.chessprogramming.org/Magic_Bitboards)
- [Finding Magic Numbers](https://www.chessprogramming.org/Looking_for_Magics)
- `lib/engine/magic.ml` - See implementation
