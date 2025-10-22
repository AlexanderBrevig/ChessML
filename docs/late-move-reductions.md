---
layout: default
title: Late Move Reductions
parent: Chess Programming Guide
nav_order: 8
description: "Search later moves at reduced depth for efficiency"
permalink: /docs/late-move-reductions
---

# Late Move Reductions (LMR)

## What Is Late Move Reductions?

Late Move Reductions (LMR) is a technique where you search later moves in the move list at reduced depth, based on the assumption that good [move ordering](move-ordering.md) means later moves are unlikely to be best. If a reduced search proves this assumption wrong, you re-search at full depth.

**Elo Impact:** ~100-200 Elo. This is one of the most effective search techniques in modern chess engines, often allowing you to effectively search 1-2 plies deeper.

## The Core Idea

With good [move ordering](move-ordering.md):

- First few moves (10-20%) contain the best move ~90% of the time
- Later moves (80-90%) are usually inferior

Instead of searching all moves to the same depth, search "late" moves at reduced depth. If one looks good at reduced depth, re-search it at full depth to verify.

**Key insight:** Most moves are bad. Spending full effort on all moves wastes time. Search promising moves deeply, questionable moves shallowly.

## Basic Algorithm

```ocaml
let rec search_moves moves depth alpha beta move_count =
  match moves with
  | [] -> alpha, None
  | mv :: rest ->
    move_count := !move_count + 1;

    (* Determine if we can reduce this move *)
    let can_reduce =
      !move_count >= 3           (* Don't reduce first 2-3 moves *)
      && depth >= 3              (* Need sufficient depth *)
      && not (is_capture mv)     (* Don't reduce captures *)
      && not (gives_check mv)    (* Don't reduce checks *)
      && not (is_killer mv)      (* Don't reduce killers *)
    in

    let reduction =
      if can_reduce then
        calculate_reduction depth !move_count
      else 0
    in

    (* Try reduced search *)
    let score =
      if reduction > 0 then
        let reduced_depth = max 1 (depth - 1 - reduction) in
        let s, _ = alphabeta new_pos reduced_depth (-beta) (-alpha) in
        let s = -s in

        (* If reduced search beats alpha, re-search at full depth *)
        if s > alpha then
          let full_s, _ = alphabeta new_pos (depth - 1) (-beta) (-alpha) in
          -full_s
        else
          s
      else
        (* No reduction, search at full depth *)
        let s, _ = alphabeta new_pos (depth - 1) (-beta) (-alpha) in
        -s
    in

    if score >= beta then
      score, Some mv  (* Beta cutoff *)
    else
      let new_alpha = max alpha score in
      search_moves rest depth new_alpha beta move_count
```

## When to Reduce

### Always safe to reduce:

**1. Late in move list** (move number >= 3-4)

```ocaml
let move_threshold = 3 in
if move_count >= move_threshold then ...
```

**2. Sufficient depth** (depth >= 3)

```ocaml
if depth >= 3 then ...
```

**Why:** Need enough depth for reduction to be meaningful and for re-search to catch mistakes.

### Never reduce:

**1. Tactical moves** (captures, promotions)

```ocaml
if is_capture mv || is_promotion mv then
  reduction = 0
```

**Why:** Tactical moves can dramatically change evaluation. Missing tactics is catastrophic.

**2. Moves giving check**

```ocaml
if gives_check pos mv then
  reduction = 0
```

**Why:** Checks are forcing and can lead to tactics or checkmate.

**3. Killer moves**

```ocaml
if is_killer mv depth then
  reduction = 0
```

**Why:** [Killers](move-ordering.md#killer-move-heuristic) are quiet moves that previously caused cutoffs—they're likely strong.

**4. PV nodes** (where alpha has been raised)

```ocaml
let is_pv = alpha != original_alpha in
if is_pv then
  reduction = 0  (* Or reduced reduction *)
```

**Why:** PV nodes are on the main variation—these moves are critical to search accurately.

## Calculating Reduction

### Simple Fixed Reduction

```ocaml
let reduction depth move_count =
  if depth >= 3 && move_count >= 3 then 1
  else 0
```

Reduce by 1 ply for late moves at sufficient depth.

### Adaptive Reduction

```ocaml
let reduction depth move_count =
  if depth < 3 || move_count < 3 then 0
  else if depth >= 6 && move_count >= 6 then 2
  else 1
```

Deeper searches and later moves get more aggressive reduction.

### Logarithmic Formula (Tournament Strength)

```ocaml
let reduction depth move_count =
  if depth < 3 || move_count < 3 then 0
  else
    (* Base reduction using logarithmic formula *)
    let base =
      int_of_float (log (float depth) *. log (float move_count) /. 2.0) in
    max 1 (min base (depth - 2))
```

**Why logarithmic?**

- Move 3 at depth 6: reduction = 1
- Move 10 at depth 6: reduction = 2
- Move 20 at depth 10: reduction = 3

Scales naturally with both depth and move number.

### Adjustments Based on Context

```ocaml
let reduction depth move_count ~is_pv ~improving ~gives_check ~history_score =
  let base = base_reduction depth move_count in

  let base = base - (if is_pv then 1 else 0) in
  let base = base - (if improving then 1 else 0) in
  let base = base - (if gives_check then 2 else 0) in
  let base = base - (if history_score > 1000 then 1 else 0) in
  let base = base + (if history_score < -1000 then 1 else 0) in

  max 0 base  (* Don't reduce below 0 *)
```

**Factors:**

- **PV node:** Reduce less (or not at all)
- **Improving position:** Reduce less (evaluation increasing)
- **Gives check:** Reduce much less (or not at all)
- **High history score:** Reduce less (move has been good before)
- **Low history score:** Reduce more (move has failed before)

## Re-Search Logic

Critical: If reduced search shows move might be good, re-search at full depth:

```ocaml
(* Try reduced search *)
let score_reduced = search_reduced new_pos reduced_depth (-beta) (-alpha) in

if score_reduced > alpha then
  (* Reduced search suggests this move is good - verify! *)
  let score_full = search_full new_pos (depth - 1) (-beta) (-alpha) in
  score_full
else
  (* Reduced search confirms move is bad, accept result *)
  score_reduced
```

**Cost:** Some moves need two searches (reduced + full)
**Benefit:** Most moves only need one (reduced) search
**Net result:** Massive time savings (~50% node reduction)

## Principal Variation Search (PVS) + LMR

Combine [PVS](alpha-beta-pruning.md#principal-variation-search-pvs) with LMR for maximum efficiency:

```ocaml
if move_count = 1 then
  (* First move: full window, no reduction *)
  let score, _ = alphabeta new_pos (depth - 1) (-beta) (-alpha) in
  -score
else
  (* Later moves: try zero window + reduction *)
  let reduction = calculate_reduction depth move_count in
  let reduced_depth = max 1 (depth - 1 - reduction) in

  (* Zero window search at reduced depth *)
  let score, _ = alphabeta new_pos reduced_depth (-alpha - 1) (-alpha) in
  let score = -score in

  if score > alpha then
    (* Failed high on reduced zero window - re-search *)
    if score < beta then
      (* Re-search with full window at full depth *)
      let score, _ = alphabeta new_pos (depth - 1) (-beta) (-alpha) in
      -score
    else
      score  (* Beta cutoff *)
  else
    score  (* Move is bad as expected *)
```

This combines three optimizations:

1. PVS zero window for non-PV nodes
2. LMR reduction for late moves
3. Re-search only when necessary

## Complete Implementation

```ocaml
let rec search_moves moves depth alpha beta move_count =
  match moves with
  | [] -> alpha, None
  | mv :: rest ->
    move_count := !move_count + 1;
    let is_first = !move_count = 1 in

    (* Calculate reduction *)
    let is_tactical = is_capture mv || is_promotion mv in
    let gives_check = gives_check_fast pos mv in
    let is_killer = is_killer mv depth in
    let can_reduce =
      not is_first
      && depth >= 3
      && !move_count >= 3
      && not is_tactical
      && not gives_check
      && not is_killer
    in

    let reduction =
      if can_reduce then
        let is_pv = alpha != original_alpha in
        let history = History.get_score mv in
        calculate_reduction depth !move_count ~is_pv ~history
      else 0
    in

    (* Make move *)
    let new_pos = make_move pos mv in

    (* Search with appropriate depth and window *)
    let score =
      if is_first then
        (* First move: full window, full depth *)
        let s, _ = alphabeta new_pos (depth - 1) (-beta) (-alpha) in
        -s
      else if reduction > 0 then
        (* Late move with reduction: reduced depth + zero window *)
        let reduced_depth = max 1 (depth - 1 - reduction) in
        let s, _ = alphabeta new_pos reduced_depth (-alpha - 1) (-alpha) in
        let s = -s in

        (* Re-search if necessary *)
        if s > alpha && s < beta then
          let s, _ = alphabeta new_pos (depth - 1) (-beta) (-alpha) in
          -s
        else
          s
      else
        (* Late move without reduction: zero window *)
        let s, _ = alphabeta new_pos (depth - 1) (-alpha - 1) (-alpha) in
        let s = -s in

        (* Re-search if necessary *)
        if s > alpha && s < beta then
          let s, _ = alphabeta new_pos (depth - 1) (-beta) (-alpha) in
          -s
        else
          s
    in

    (* Check for cutoff *)
    if score >= beta then
      score, Some mv
    else
      let new_alpha = max alpha score in
      search_moves rest depth new_alpha beta move_count
```

## Common Pitfalls

### 1. Reducing Tactical Moves

```ocaml
(* Wrong: *)
let reduction = calculate_reduction depth move_count in

(* Correct: *)
let reduction =
  if is_capture mv || gives_check mv then 0
  else calculate_reduction depth move_count
```

### 2. Not Re-Searching

```ocaml
(* Wrong: Trust reduced search *)
let score = search_reduced new_pos reduced_depth alpha beta in

(* Correct: Re-search if promising *)
let score_reduced = search_reduced ... in
if score_reduced > alpha then
  let score_full = search_full ... in
  score_full
else
  score_reduced
```

### 3. Reducing Too Aggressively

```ocaml
(* Wrong: Reduce by too much *)
let reduction = depth / 2 in

(* Correct: Conservative reduction *)
let reduction = log(depth) * log(move_count) / 2.0 in
let reduction = max 1 (min reduction (depth - 2)) in
```

### 4. Reducing First Moves

```ocaml
(* Wrong: *)
for all moves do
  let reduction = calculate_reduction ... in

(* Correct: *)
if move_count >= 3 then
  let reduction = calculate_reduction ... in
else
  let reduction = 0 in
```

## Measuring Effectiveness

Track LMR statistics:

```ocaml
type lmr_stats = {
  reductions: int;        (* Times LMR applied *)
  re_searches: int;       (* Times re-search needed *)
  reduction_total: int;   (* Sum of all reductions *)
}

let avg_reduction stats =
  float stats.reduction_total /. float stats.reductions

let re_search_rate stats =
  float stats.re_searches /. float stats.reductions
```

Good LMR performance:

- **Reduction rate:** 40-60% of moves reduced
- **Average reduction:** 1.5-2.5 plies
- **Re-search rate:** 10-20% (most reduced searches confirm bad moves)
- **Node reduction:** 40-60% fewer nodes searched
- **Strength:** No measurable loss (or slight gain from deeper search)

## Tuning Reduction Formula

The logarithmic formula is standard but can be tuned:

```ocaml
(* Conservative: *)
let reduction = log(depth) * log(move_count) / 3.0

(* Aggressive: *)
let reduction = log(depth) * log(move_count) / 1.5

(* Custom: *)
let reduction =
  (log(depth) * 0.5) + (log(move_count) * 0.4)
```

Run test matches to find optimal values for your engine. Start conservative and increase aggression as other parts improve.

## LMR Synergy

LMR works best with:

- **Good [move ordering](move-ordering.md):** Hash moves, killers, history
- **[Transposition tables](transposition-tables.md):** Avoid re-searching transpositions
- **[Null move pruning](null-move-pruning.md):** Double reduction effect
- **[PVS](alpha-beta-pruning.md#principal-variation-search-pvs):** Zero window searches are faster

Combined, these techniques can make your engine 5-10x faster!

## Further Reading

- [Move Ordering](move-ordering.md) - Critical for LMR effectiveness
- [Alpha-Beta Pruning](alpha-beta-pruning.md) - Base search algorithm
- [Null Move Pruning](null-move-pruning.md) - Complementary pruning
- [Chess Programming Wiki - Late Move Reductions](https://www.chessprogramming.org/Late_Move_Reductions)
- `lib/engine/search.ml` - See LMR implementation
