---
layout: default
title: Alpha-Beta Pruning
parent: Chess Programming Guide
nav_order: 4
description: "The foundation of modern chess search algorithms"
permalink: /docs/alpha-beta-pruning
---

# Alpha-Beta Pruning

## What Is Alpha-Beta Pruning?

Alpha-beta pruning is an optimization of the minimax algorithm that lets you skip searching parts of the game tree that provably can't affect the final decision. It's the foundation of every competitive chess engine.

**Elo Impact:** ~1000+ Elo compared to basic minimax. It typically reduces the number of positions you need to evaluate by 50-90%, letting you search much deeper.

## Why It Matters

Chess has an enormous branching factor (~35 legal moves per position on average). Without pruning:

- Depth 1: ~35 positions
- Depth 2: ~1,225 positions
- Depth 3: ~42,875 positions
- Depth 4: ~1.5 million positions

With alpha-beta, you can effectively search 2x deeper in the same time, which is worth roughly 200 Elo per extra ply.

## The Core Idea

During search, you maintain two values:

- **Alpha (α):** The best score the maximizing player (you) can guarantee so far
- **Beta (β):** The best score the minimizing player (opponent) can guarantee so far

**Key insight:** If you find a move where the opponent's best response is worse than what they can already achieve elsewhere in the tree, stop searching that branch—it will never be played.

## How It Works

### Simple Example

```
Your turn, considering two moves:
  Move A: Opponent can reply with -5 (bad for you)
  Move B: You're evaluating opponent's responses...
    - Response 1: -3
    - Response 2: -1
    - Response 3: ... (we can stop here!)
```

Why stop at Response 3? You've already seen that Move A gives -5. In Move B, the opponent already has a response giving them -1 (better for them than -5). Even if Response 3 is terrible for the opponent (say, -10), they'll just play Response 2 instead. So you don't need to evaluate Response 3—Move B is already worse than Move A.

### The Algorithm

```ocaml
let rec alphabeta pos depth alpha beta maximizing_player =
  if depth = 0 then
    evaluate pos
  else if maximizing_player then
    (* We're trying to maximize score *)
    let rec search_moves moves alpha best_move =
      match moves with
      | [] -> alpha, best_move
      | mv :: rest ->
        let new_pos = make_move pos mv in
        let score, _ = alphabeta new_pos (depth - 1) alpha beta false in
        if score >= beta then
          (* Beta cutoff: opponent won't let us reach this position *)
          score, Some mv
        else
          let new_alpha = max alpha score in
          let new_best = if score > alpha then Some mv else best_move in
          search_moves rest new_alpha new_best
    in
    search_moves (generate_moves pos) alpha None
  else
    (* Opponent trying to minimize score *)
    let rec search_moves moves beta best_move =
      match moves with
      | [] -> beta, best_move
      | mv :: rest ->
        let new_pos = make_move pos mv in
        let score, _ = alphabeta new_pos (depth - 1) alpha beta true in
        if score <= alpha then
          (* Alpha cutoff: we won't let opponent reach this position *)
          score, Some mv
        else
          let new_beta = min beta score in
          let new_best = if score < beta then Some mv else best_move in
          search_moves rest new_beta new_best
    in
    search_moves (generate_moves pos) beta None
```

## Types of Cutoffs

### Beta Cutoff (Fail-High)

Occurs when the maximizing player finds a move too good for the position—the opponent won't allow the position to be reached.

```
Alpha = 10, Beta = 20
You find a move scoring 25 → Beta cutoff!
Opponent has better options earlier in the tree (scoring < 20)
```

### Alpha Cutoff (Fail-Low)

Occurs when the minimizing player finds a move too good for them—you won't allow that position.

```
Alpha = 10, Beta = 20
Opponent finds a response scoring 5 → Alpha cutoff!
You have better options earlier (scoring > 10)
```

## Negamax Formulation

Most engines use "negamax" - a simplified version where you always maximize from the current player's perspective:

```ocaml
let rec alphabeta pos depth alpha beta =
  if depth = 0 then
    evaluate pos
  else
    let rec search_moves moves alpha best_move =
      match moves with
      | [] -> alpha, best_move
      | mv :: rest ->
        let new_pos = make_move pos mv in
        (* Note the negation and swapped alpha/beta *)
        let score, _ = alphabeta new_pos (depth - 1) (-beta) (-alpha) in
        let score = -score in
        if score >= beta then
          score, Some mv  (* Beta cutoff *)
        else
          let new_alpha = max alpha score in
          let new_best = if score > alpha then Some mv else best_move in
          search_moves rest new_alpha new_best
    in
    search_moves (generate_moves pos) alpha None
```

This works because what's good for you (positive score) is bad for your opponent (negative score).

## Move Ordering: The Secret Sauce

{: .highlight }
> **Performance Tip:** Good move ordering can effectively double your search depth!

Alpha-beta's effectiveness depends heavily on **[move ordering](move-ordering.md)**. The algorithm works best when you search the best moves first.

**Perfect ordering:** Only ~2√N positions searched (vs N for minimax)
**Random ordering:** ~3N/4 positions searched

### Basic ordering strategy:

1. **[Hash move](transposition-tables.md)** (from transposition table)
2. **Winning captures** (pawn takes queen)
3. **Equal captures** (bishop takes bishop)
4. **[Killer moves](move-ordering.md#killer-move-heuristic)** (quiet moves that caused cutoffs recently)
5. **[History heuristic](move-ordering.md#history-heuristic)** (moves that historically caused cutoffs)
6. **Losing captures** (queen takes pawn defended by pawn)
7. **Remaining quiet moves**

Good move ordering can effectively double your search depth!

## Principal Variation Search (PVS)

An enhancement where you assume the first move is best:

```ocaml
(* Search first move with full window *)
let score1, _ = alphabeta pos1 (depth - 1) (-beta) (-alpha) in
let score1 = -score1 in

(* Search remaining moves with zero window *)
let score2, _ = alphabeta pos2 (depth - 1) (-alpha - 1) (-alpha) in
let score2 = -score2 in

if alpha < score2 && score2 < beta then
  (* Re-search with full window if assumption was wrong *)
  let score2, _ = alphabeta pos2 (depth - 1) (-beta) (-score2) in
  ...
```

This saves time because zero-window searches are very fast and often confirm your assumption. Often combined with [Late Move Reductions](late-move-reductions.md) for maximum efficiency.

## Common Pitfalls

{: .warning }
> **Common Pitfall:** Forgetting to use `max` when updating alpha loses information!

### 1. Incorrect Window Updates

```ocaml
(* Wrong: *)
let alpha = score  (* Loses information *)

(* Correct: *)
let alpha = max alpha score  (* Keep best lower bound *)
```

### 2. Forgetting to Negate

```ocaml
(* Wrong in negamax: *)
let score, _ = alphabeta new_pos (depth - 1) (-beta) (-alpha) in

(* Correct: *)
let score, _ = alphabeta new_pos (depth - 1) (-beta) (-alpha) in
let score = -score in
```

### 3. Not Checking for Cutoffs

Every move must check if `score >= beta` for maximizer or `score <= alpha` for minimizer.

## Implementation Tips

### 1. Start Simple

Begin with basic alphabeta before adding PVS or aspiration windows.

### 2. Track Nodes

Count positions evaluated to measure pruning effectiveness:

```ocaml
let nodes = ref 0L in
(* ... *)
nodes := Int64.add !nodes 1L
```

### 3. Use Mate Scores Carefully

```ocaml
let mate_score = 100000 in
let is_mate score = abs score > 90000 in

(* Be careful with overflow when negating! *)
if score > 50000 then -50000 else -score
```

### 4. Depth Reduction Checks

Always verify `depth > 0` before recursing to prevent infinite loops.

## Measuring Success

A well-implemented alpha-beta should:

- Achieve **50-90% pruning** at depth 4+
- Show **exponentially decreasing** node counts with better move ordering
- Produce **identical results** to minimax (verify with shallow searches)

Example node counts (same position):

- Minimax depth 5: ~5 million nodes
- Basic alpha-beta: ~500k nodes (90% reduction)
- Alpha-beta + good ordering: ~50k nodes (99% reduction)

## Further Reading

- [Move Ordering](move-ordering.md) - Critical for alpha-beta efficiency
- [Transposition Tables](transposition-tables.md) - Cache results for identical positions
- [Quiescence Search](quiescence-search.md) - Extend search for tactical stability
- [Chess Programming Wiki - Alpha-Beta](https://www.chessprogramming.org/Alpha-Beta)
