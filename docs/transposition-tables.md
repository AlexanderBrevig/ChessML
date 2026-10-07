---
layout: default
title: Transposition Tables
parent: Chess Programming Guide
nav_order: 5
description: "My notes on remembering search results by position"
permalink: /docs/transposition-tables
---

# Transposition Tables

These are my notes on transposition tables (TT), written while building ChessML. They describe how I understand them and what ChessML does; for the real reference, see the [Chess Programming Wiki](https://www.chessprogramming.org/Transposition_Table).

## The idea

Different move orders often lead to the same position: 1. e4 e5 2. Nf3 Nc6 and 1. Nf3 Nc6 2. e4 e5 end up in the same place. A plain search would analyze that position once per path. A transposition table is a big hash table, keyed by the position's [Zobrist key]({% link docs/zobrist-hashing.md %}), where the search writes down what it found about each position so it can reuse it when it gets there again.

Because of alpha-beta, a search result is often not an exact score but a bound, so each entry says which kind it is:

- **Exact**: the score fell inside the window `(alpha, beta)`; it is the true value at that depth.
- **Lower bound**: some move reached beta and the search stopped early; the true value is at least this.
- **Upper bound**: no move beat alpha; the true value is at most this.

An entry also stores its search depth (the score only stands in for a search at most that deep) and the best move, which is worth trying first even when the score is not usable (see [Move Ordering]({% link docs/move-ordering.md %}#tt-move)).

## What ChessML does

The table is a module inside `lib/engine/search.ml`: an array of `tt_entry option`, indexed by the key modulo the array length. Each slot holds one entry (direct-mapped, no buckets):

```ocaml
type tt_entry =
  { key : int64
  ; score : int
  ; depth : int
  ; best_move : Move.t option
  ; entry_type : tt_entry_type   (* Exact | LowerBound | UpperBound *)
  }
```

### Collisions

Many positions map to each slot, so index collisions are normal; that is what replacement is about. To tell them apart the entry keeps the full 64-bit key, and `lookup` treats any other key as a miss. Two positions with the same full key are possible but very rare.

### Replacement

When storing, ChessML keeps the existing entry only if it is for the same position, was searched deeper, and the new result is not Exact. In every other case the new entry overwrites the slot, including when the slot holds a different position:

```ocaml
let store tt key depth score best_move entry_type =
  let idx = index tt key in
  match tt.table.(idx) with
  | Some e when Int64.equal e.key key && e.depth > depth && entry_type <> Exact -> ()
  | _ -> tt.table.(idx) <- Some { key; score; depth; best_move; entry_type }
```

Many engines use small buckets of entries and an age field instead; ChessML does not.

### Using an entry

In `search_node` the TT move is always taken for ordering. The score is only used to end the search of a node when the node is not a PV node (`beta - alpha > 1`), is not the root, and the entry is deep enough (shortened):

```ocaml
match entry with
| Some e when (not pv_node) && ply > 0 && e.depth >= depth ->
  let score = Score.of_tt e.score ply in
  (match e.entry_type with
   | Exact -> Some score
   | LowerBound when score >= beta -> Some score
   | UpperBound when score <= alpha -> Some score
   | _ -> None)
| _ -> None
```

Not cutting at PV nodes keeps the principal variation complete.

### Mate scores

ChessML's [`Score`](https://github.com/AlexanderBrevig/ChessML/blob/main/lib/engine/score.ml) module counts mate distance from the root: being mated at ply `p` scores `-mate + p`. A TT entry, though, may be read at a different ply than it was written, so a root-relative score would be wrong there. Before storing, `Score.to_tt score ply` makes mate scores relative to the node: a winning mate score (at or above `mate_bound`) gets `+ ply`, a losing one gets `- ply`. On the way out, `Score.of_tt score ply` does the opposite at the ply where it is read. As a check: a mate found at ply 9, seen from a node at ply 5, is a mate four plies away from that node, and `mate - 9 + 5 = mate - 4`.

### Size and lifetime

ChessML estimates a boxed entry at about 80 bytes (`tt_entry_bytes`), so the default UCI `Hash` of 16 MB is about 210,000 entries; changing `Hash` reallocates and empties the table. The table lives in `Search.state`, so it survives from one move to the next during a game; `Search.new_game` (UCI `ucinewgame`, XBoard `new`) clears it.

ChessML has no parallel search, so there is no shared or lock-free table.

## Pitfalls

- **Mate scores stored without ply adjustment.** The engine then reports wrong mate distances or prefers a longer mate. The store and the load must apply opposite corrections at their own ply.
- **Comparing only the index.** Many positions map to each slot; without comparing the stored key, the search reads another position's score.
- **Using a shallow entry's score.** An entry is only as good as the depth it was searched to; use `e.depth >= depth`, but the move can still be used for ordering.

## Sources

- [Chess Programming Wiki: Transposition Table](https://www.chessprogramming.org/Transposition_Table)
- ChessML's code: `lib/engine/search.ml` (`TranspositionTable`, `search_node`, `store_tt`), `lib/engine/score.ml`, `lib/protocols/protocol_common.ml` (the `Hash` option)
