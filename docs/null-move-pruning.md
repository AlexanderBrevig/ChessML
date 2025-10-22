---
layout: default
title: Null Move Pruning
parent: Chess Programming Guide
nav_order: 7
description: "Aggressive pruning by giving opponent a free move"
permalink: /docs/null-move-pruning
---

# Null Move Pruning

## What Is Null Move Pruning?

Null move pruning is a technique where you let the opponent move twice in a row. If they still can't beat your [beta threshold](alpha-beta-pruning.md) even with this huge advantage, the position is so good for you that you can safely prune it.

**Elo Impact:** ~100-150 Elo. This aggressive pruning technique dramatically reduces search time with minimal accuracy loss.

## The Core Idea

In chess, having the move is valuable—roughly 15-50 centipawns. If you give your opponent an extra move (pass your turn, make a "null move"), and they _still_ can't beat beta, then the position is so good that you can stop searching and return beta.

**Key insight:** If even giving the opponent a free move doesn't help them, you must have overwhelming advantage. Opponent won't let the game reach this position, so you can prune this branch.

## How It Works

```ocaml
let rec alphabeta pos depth alpha beta =
  (* ... existing checks ... *)

  (* Try null move pruning *)
  if can_do_null_move pos depth beta then begin
    (* Make null move: swap sides without moving pieces *)
    let null_pos = make_null_move pos in

    (* Search at reduced depth *)
    let null_depth = depth - 1 - null_reduction in
    let score, _ = alphabeta null_pos null_depth (-beta) (-beta + 1) in
    let score = -score in

    (* If null move causes beta cutoff, prune this branch *)
    if score >= beta then
      return beta  (* Null move cutoff *)
  end;

  (* Continue normal search *)
  ...
```

## When to Use Null Move

### Don't use null move when:

**1. In check**

```ocaml
let in_check = is_king_attacked pos in
if in_check then
  (* Skip null move - can't pass when in check *)
```

**Why:** It's illegal to stay in check. Also, being in check often means you're in trouble, not ahead.

**2. In zugzwang positions**

```ocaml
let zugzwang_likely =
  only_king_and_pawns pos || endgame pos in
if zugzwang_likely then
  (* Skip null move - passing might actually help *)
```

**Why:** In some positions (especially king and pawn endgames), passing your turn _helps_ you. Example: If you have to move but any move loses, passing would be better.

**3. Already did null move recently**

```ocaml
(* Track if parent node used null move *)
if parent_used_null_move then
  (* Skip null move - prevents double null move *)
```

**Why:** Can't give opponent two free moves in a row—leads to invalid positions.

**4. Low depth**

```ocaml
if depth < 3 then
  (* Skip null move - not enough depth to be useful *)
```

**Why:** Need enough depth to get meaningful verification. At depth 2, reduced depth becomes too shallow.

## Null Move Reduction

How much to reduce depth? Common formulas:

**Fixed reduction (simple):**

```ocaml
let null_reduction = 2 in
let null_depth = depth - 1 - null_reduction in
(* depth 6 -> null depth 3 *)
```

**Adaptive reduction (better):**

```ocaml
let null_reduction depth =
  if depth >= 6 then 3
  else 2
in
(* Deeper searches use more aggressive reduction *)
```

**Formula-based (tournament engines):**

```ocaml
let null_reduction depth eval beta =
  let base_reduction = 3 in
  let depth_bonus = max 0 ((depth - 6) / 3) in
  let eval_bonus = if eval - beta > 200 then 1 else 0 in
  base_reduction + depth_bonus + eval_bonus
in
```

Typical values: **R = 2 to 3** (reduce by 2-3 plies)

## Making the Null Move

```ocaml
let make_null_move pos =
  { pos with
    side_to_move = Color.opponent pos.side_to_move;
    en_passant_square = None;  (* Clear en passant *)
    halfmove_clock = pos.halfmove_clock + 1;
    fullmove_number =
      if pos.side_to_move = Black
      then pos.fullmove_number + 1
      else pos.fullmove_number;
  }
```

**Important:**

- Swap side to move
- Clear en passant (can't capture en passant after null move)
- Don't move any pieces!
- Update move counters

## Verification Search

To avoid zugzwang problems, some engines do **verification search**:

```ocaml
if score >= beta then
  (* Null move suggests cutoff, but verify *)
  if likely_zugzwang pos then
    (* Do reduced depth search to verify *)
    let verify_depth = depth - null_reduction in
    let verify_score, _ = alphabeta pos verify_depth alpha beta in
    if verify_score >= beta then
      return beta  (* Verified cutoff *)
    else
      (* False cutoff, continue normal search *)
      continue_search ()
  else
    return beta  (* Not zugzwang risk, trust cutoff *)
```

Most engines **skip verification** for performance—zugzwang is rare enough that the speed gain outweighs occasional errors.

## Complete Implementation

```ocaml
let rec alphabeta pos depth alpha beta ~prev_used_null =
  (* Check for terminal conditions *)
  if depth = 0 then quiescence pos alpha beta
  else
    (* Null move pruning *)
    let in_check = is_king_attacked pos in
    let null_move_result =
      if not prev_used_null          (* Don't do double null *)
         && not in_check              (* Can't null in check *)
         && depth >= 3                (* Need sufficient depth *)
         && has_non_pawn_material pos (* Avoid zugzwang *)
         && score > beta              (* Position is promising *)
      then begin
        let null_pos = make_null_move pos in
        let null_reduction = if depth >= 6 then 3 else 2 in
        let null_depth = depth - 1 - null_reduction in

        let null_score, _ =
          alphabeta null_pos null_depth (-beta) (-beta + 1) ~prev_used_null:true in
        let null_score = -null_score in

        if null_score >= beta then
          Some beta  (* Null move cutoff *)
        else
          None
      end
      else None
    in

    match null_move_result with
    | Some score -> score, None
    | None ->
      (* Continue normal search *)
      normal_search pos depth alpha beta ~prev_used_null:false
```

## Zugzwang Detection

Positions where null move can fail:

### King and Pawn Endgames

```
Example: Opposition
White: Kd5, pawns on e5,f5
Black: Kd7

White to move: draw (must move king, loses opposition)
Black to move: White wins (Black must give up opposition)

Null move gives Black extra move -> fails to find White's win
```

### Detection heuristics:

```ocaml
let zugzwang_likely pos =
  (* Only kings and pawns *)
  let non_pawn_material side =
    count_material pos side - (count_pawns pos side * 100) in

  non_pawn_material White <= 300 && non_pawn_material Black <= 300
```

Or simpler:

```ocaml
let has_non_pawn_material pos =
  let side = pos.side_to_move in
  let knights = popcount (pieces pos side Knight) in
  let bishops = popcount (pieces pos side Bishop) in
  let rooks = popcount (pieces pos side Rook) in
  let queens = popcount (pieces pos side Queen) in
  knights + bishops + rooks + queens > 0
```

## Common Pitfalls

### 1. Null Move in Check

```ocaml
(* Wrong: *)
let null_pos = make_null_move pos in

(* Correct: *)
if not (is_in_check pos) then
  let null_pos = make_null_move pos in
```

### 2. Double Null Move

```ocaml
(* Wrong: Can do null move again in child *)
alphabeta null_pos depth' alpha beta

(* Correct: Track null move usage *)
alphabeta null_pos depth' alpha beta ~used_null:true
```

### 3. Not Clearing En Passant

```ocaml
(* Wrong: *)
make_null_move pos  (* Keeps en_passant_square *)

(* Correct: *)
{ pos with en_passant_square = None }
```

### 4. Using with Low Depth

```ocaml
(* Wrong: *)
if depth >= 1 then try_null_move ()

(* Correct: Need at least depth 3 *)
if depth >= 3 then try_null_move ()
```

## Measuring Effectiveness

Track null move statistics:

```ocaml
type null_move_stats = {
  attempts: int;        (* Times null move tried *)
  cutoffs: int;         (* Times null move caused cutoff *)
  nodes_saved: int64;   (* Estimated nodes saved *)
}

let cutoff_rate stats =
  float stats.cutoffs /. float stats.attempts
```

Good null move pruning:

- **Cutoff rate:** 60-80% of attempts
- **Nodes saved:** 40-60% reduction in total nodes
- **Speed improvement:** 2-3x faster search

## Advanced: Mate Threat Extensions

If null move search shows opponent has a mate threat, extend the search:

```ocaml
let null_score, _ = alphabeta null_pos null_depth (-beta) (-beta + 1) in
let null_score = -null_score in

if null_score >= beta then
  Some beta  (* Normal null move cutoff *)
else if null_score <= -mate_threshold then
  (* Opponent has mate threat! Extend search *)
  let extended_depth = depth + 1 in
  normal_search pos extended_depth alpha beta
else
  None  (* Continue normal search *)
```

This catches positions where opponent has strong threats.

## Null Move vs Other Techniques

| Technique                                          | Elo Gain | Risk                | Complexity |
| -------------------------------------------------- | -------- | ------------------- | ---------- |
| Null Move                                          | +100-150 | Zugzwang errors     | Low        |
| [Late Move Reductions](late-move-reductions.md)   | +100-200 | Missed tactics      | Medium     |
| Futility Pruning                                   | +30-50   | Tactical oversights | Low        |

Null move is high reward, low risk, easy to implement—one of the best Elo-per-line improvements!

## Further Reading

- [Late Move Reductions](late-move-reductions.md) - Another powerful pruning technique
- [Alpha-Beta Pruning](alpha-beta-pruning.md) - Base search algorithm
- [Chess Programming Wiki - Null Move Pruning](https://www.chessprogramming.org/Null_Move_Pruning)
- [Zugzwang](https://www.chessprogramming.org/Zugzwang)
- `lib/engine/search.ml` - See null move implementation
