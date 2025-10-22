---
layout: default
title: Transposition Tables
parent: Chess Programming Guide
nav_order: 5
description: "Cache position evaluations to avoid redundant search"
permalink: /docs/transposition-tables
---

# Transposition Tables

## What Is a Transposition Table?

A transposition table (TT) is a hash table that caches position evaluations during [search](alpha-beta-pruning.md). When you encounter a position you've already analyzed, you can reuse the previous result instead of searching it again.

**Elo Impact:** ~200-300 Elo. This is one of the biggest single improvements you can make to a chess engine.

## Why They're Essential

### Transpositions Are Common

The same position can be reached through different move orders:

```
1. e4 e5 2. Nf3 Nc6  ← Same position
1. Nf3 Nc6 2. e4 e5  ← Different move order
```

At depth 6+, roughly 30-40% of positions searched are transpositions. Without caching, you search each one multiple times—wasting enormous amounts of computation.

### The Math

Consider a position reached by 3 different move sequences:

- Without TT: Search 3 times (e.g., 3 × 10,000 nodes = 30,000 nodes)
- With TT: Search once, lookup twice (e.g., 10,000 + 2 = 10,002 nodes)

This compounds exponentially with search depth!

## How They Work

### Basic Structure

```ocaml
type entry_type =
  | Exact       (* Exact score: alpha < score < beta *)
  | LowerBound  (* Beta cutoff: score >= beta *)
  | UpperBound  (* Alpha cutoff: score <= alpha *)

type tt_entry = {
  key: int64;          (* Zobrist hash of position *)
  score: int;          (* Evaluation score *)
  depth: int;          (* Depth searched *)
  best_move: move option;  (* Best move found *)
  entry_type: entry_type;  (* Type of bound *)
}

let table = Array.make size None
```

See [Zobrist Hashing](zobrist-hashing.md) for how to generate position keys.

### Storing Entries

After searching a position:

```ocaml
let store tt pos_hash depth score best_move entry_type =
  let index = Int64.to_int (Int64.unsigned_rem pos_hash (Int64.of_int tt.size)) in
  let entry = {
    key = pos_hash;
    score = score;
    depth = depth;
    best_move = best_move;
    entry_type = entry_type;
  } in
  tt.table.(index) <- Some entry
```

### Looking Up Entries

Before searching a position:

```ocaml
let lookup tt pos_hash =
  let index = Int64.to_int (Int64.unsigned_rem pos_hash (Int64.of_int tt.size)) in
  match tt.table.(index) with
  | Some entry when entry.key = pos_hash -> Some entry
  | _ -> None
```

## Entry Types Explained

### Exact Score

You searched with a window `[alpha, beta]` and found a score inside that window:

```ocaml
if alpha < score && score < beta then
  store tt pos_hash depth score best_move Exact
```

This is the "gold standard" - you can reuse this score directly.

### Lower Bound (Beta Cutoff)

You found a move so good that it caused a beta cutoff:

```ocaml
if score >= beta then
  store tt pos_hash depth score best_move LowerBound
```

**Meaning:** The true score is _at least_ this good (possibly better). You stopped searching because the opponent won't let you reach this position.

### Upper Bound (Alpha Cutoff)

You searched all moves and none improved alpha:

```ocaml
if score <= alpha then
  store tt pos_hash depth score best_move UpperBound
```

**Meaning:** The true score is _at most_ this good (possibly worse). This position doesn't help us.

## Using Stored Entries

When you find a cached entry:

```ocaml
match TranspositionTable.lookup tt pos_hash with
| Some entry when entry.depth >= depth ->
  (* Entry is deep enough to use *)
  (match entry.entry_type with
   | Exact ->
     (* Perfect! Return the cached score *)
     entry.score, entry.best_move
   | LowerBound when entry.score >= beta ->
     (* Score is at least this good, and that's enough for beta cutoff *)
     entry.score, entry.best_move
   | UpperBound when entry.score <= alpha ->
     (* Score is at most this good, and that's below alpha *)
     entry.score, entry.best_move
   | _ ->
     (* Can't use the score, but try the move first! *)
     search_with_move_ordering entry.best_move)
| Some entry ->
  (* Entry exists but not deep enough - still use the move hint *)
  search_with_move_ordering entry.best_move
| None ->
  (* No entry, search normally *)
  normal_search ()
```

### The Move Hint

{: .important }

> Even when you can't use the cached score, always try the hash move first—it's the most effective move ordering heuristic!

Even when you can't use the cached score, the `best_move` is extremely valuable! Try it first—it's very likely to cause a beta cutoff, improving [alpha-beta pruning](alpha-beta-pruning.md) efficiency. This is the most important component of [move ordering](move-ordering.md).

## Implementation Details

### Table Size

Typical sizes: 16 MB to 1024 MB (configurable by user)

```ocaml
(* Size in MB -> number of entries *)
let entries_for_size_mb mb =
  (mb * 1024 * 1024) / (size_of_entry)

(* Example: 64 MB, 24 bytes per entry = ~2.8M entries *)
```

### Hash Collisions

Two different positions might hash to the same index (collision). Always verify the full hash:

```ocaml
match tt.table.(index) with
| Some entry when entry.key = pos_hash -> (* Correct position *)
| Some entry -> (* Collision - different position *)
| None -> (* Empty slot *)
```

With good hashing (Zobrist), collisions are rare enough that you can use a simple "always replace" strategy for simplicity.

### Replacement Strategy

When a collision occurs, should you keep the old entry or replace it? Common strategies:

**Always Replace** (simplest):

```ocaml
tt.table.(index) <- Some new_entry
```

**Depth-Preferred** (keep deeper searches):

```ocaml
match tt.table.(index) with
| Some old when old.depth > new_entry.depth -> ()  (* Keep old *)
| _ -> tt.table.(index) <- Some new_entry         (* Replace *)
```

**Age-Based** (prefer recent entries):

```ocaml
(* Add age field, increment each search, replace old entries *)
```

### Mate Scores

Mate scores need special handling because they're depth-dependent:

```ocaml
(* When storing *)
let adjusted_score =
  if abs score > mate_threshold then
    if score > 0 then score - ply_from_root
    else score + ply_from_root
  else score
in
store tt ... adjusted_score ...

(* When retrieving *)
let adjusted_score =
  if abs entry.score > mate_threshold then
    if entry.score > 0 then entry.score + ply_from_root
    else entry.score - ply_from_root
  else entry.score
```

This ensures "mate in 3" remains accurate regardless of where in the tree you are.

## Zobrist Hashing

Transposition tables require fast, high-quality position hashing. See [Zobrist Hashing](zobrist-hashing.md) for details on generating position keys.

Quick version:

```ocaml
(* XOR together random numbers for each piece on each square *)
let pos_hash =
  hash_pieces pos
  |> hash_castling_rights
  |> hash_en_passant
  |> hash_side_to_move
```

## Concurrent Access

For parallel search, you need thread-safe transposition tables:

```ocaml
(* Simple approach: lock per entry *)
type concurrent_tt = {
  table: tt_entry option array;
  locks: Mutex.t array;
}

let store_concurrent tt pos_hash ... =
  let idx = index tt pos_hash in
  let lock_idx = idx mod (Array.length tt.locks) in
  Mutex.lock tt.locks.(lock_idx);
  tt.table.(idx) <- Some entry;
  Mutex.unlock tt.locks.(lock_idx)
```

Or use lock-free atomic operations for even better performance.

## Common Pitfalls

{: .warning }

> **Common Pitfall:** Using `>` instead of `>=` for depth comparison—entries at equal depth are valid!

### 1. Wrong Depth Comparison

```ocaml
(* Wrong: *)
if entry.depth > depth then ...

(* Correct: >=, not > *)
if entry.depth >= depth then ...
```

An entry at equal depth is still valid!

### 2. Forgetting to Check Hash

```ocaml
(* Wrong: *)
match tt.table.(index) with
| Some entry -> return entry.score  (* Could be wrong position! *)

(* Correct: *)
match tt.table.(index) with
| Some entry when entry.key = pos_hash -> return entry.score
```

### 3. Not Using Move Hints

Even when the score isn't usable, always try the stored move first:

```ocaml
match entry with
| Some e -> search_move_first e.best_move (other_moves pos)
| None -> search_all_moves pos
```

## Measuring Effectiveness

Track these statistics:

```ocaml
type stats = {
  lookups: int;          (* Total lookups *)
  hits: int;             (* Found valid entry *)
  cutoffs: int;          (* Used cached score *)
  move_hints: int;       (* Used move but not score *)
}
```

Good TT performance:

- **Hit rate:** 40-60% at depth 6+
- **Cutoff rate:** 20-40% (entries with usable scores)
- **Move hint usage:** 50-80% (when score not usable)

## Memory vs. Performance

Larger tables = more hits = fewer nodes = deeper search = stronger play

But there are diminishing returns:

- 16 MB → 64 MB: ~50 Elo gain
- 64 MB → 256 MB: ~30 Elo gain
- 256 MB → 1024 MB: ~20 Elo gain

The sweet spot for most engines is 64-256 MB.

## Further Reading

- [Zobrist Hashing](zobrist-hashing.md) - How to hash positions
- [Alpha-Beta Pruning](alpha-beta-pruning.md) - The search algorithm
- [Chess Programming Wiki - Transposition Table](https://www.chessprogramming.org/Transposition_Table)
- `lib/engine/concurrent_tt.ml` - Thread-safe implementation
