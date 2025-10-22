---
layout: default
title: Quiescence Search
parent: Chess Programming Guide
nav_order: 6
description: "Extend search to resolve tactical sequences"
permalink: /docs/quiescence-search
---

# Quiescence Search

## What Is Quiescence Search?

Quiescence search extends your main [alpha-beta search](alpha-beta-pruning.md) to resolve tactical sequences (captures, checks, promotions) until reaching a "quiet" position where no immediate tactics exist. This prevents the **horizon effect** where the engine misjudges positions because it stops searching in the middle of a tactical sequence.

**Elo Impact:** ~150-250 Elo. This is essential for tactical accuracy and prevents catastrophic blunders.

## The Horizon Effect Problem

{: .warning }
> **Common Pitfall:** Without quiescence search, your engine will miss tactics just beyond its search depth and make catastrophic blunders!

Without quiescence, this happens:

```
Position: Your queen is attacked by a pawn
Main search depth: 6

Ply 6: You move queen to safety (looks safe)
Ply 7: Opponent plays discovered check, your king is in danger
Ply 8: Forced king move
Ply 9: Opponent captures your rook

But search stopped at ply 6, so you don't see the rook loss!
```

The engine thinks it saved the queen, missing that it led to losing a rook. This is **catastrophic** and happens frequently at tactical positions.

## How Quiescence Search Works

After reaching depth 0 in your main search, instead of immediately calling the evaluation function, you continue searching captures (and sometimes checks):

```ocaml
let rec alphabeta pos depth alpha beta =
  if depth = 0 then
    quiescence pos alpha beta  (* Don't stop yet! *)
  else
    (* Normal search *)
    ...

and quiescence pos alpha beta =
  (* Stand-pat: assume we can at least maintain current position *)
  let eval = evaluate pos in

  (* Beta cutoff: position is already too good *)
  if eval >= beta then
    beta
  else
    (* Update alpha with stand-pat *)
    let alpha = max alpha eval in

    (* Try captures and checks *)
    let tactical_moves = generate_captures_and_checks pos in
    search_tactical_moves tactical_moves alpha beta
```

## Stand-Pat Evaluation

{: .note }
> The stand-pat score represents choosing to "do nothing"—you can always decline to make a capture.

The key insight: **You can always choose to do nothing** (pass). The "do nothing" score is the static evaluation:

```ocaml
let stand_pat = Eval.evaluate pos in

(* If stand-pat is already >= beta, we're too good *)
if stand_pat >= beta then
  return beta  (* Beta cutoff *)

(* If stand-pat improves alpha, update it *)
let alpha = max alpha stand_pat in
```

This is called **stand-pat** because you're standing with the current position.

## What Moves to Search

### Captures (Always)

```ocaml
let captures = generate_captures pos in
```

**Why:** Captures can dramatically change material balance. Must search until no more captures exist.

### Checks (Sometimes)

```ocaml
let checks = generate_checks pos in
```

**Why:** Checks are forcing moves that can lead to tactics. But they significantly increase the search tree size, so many engines:

- Only search checks at depth 0 of quiescence (first ply)
- Skip checks entirely and rely on captures

### Promotions (Always)

```ocaml
let promotions = generate_promotions pos in
```

**Why:** A pawn promoting to queen changes evaluation by ~800 centipawns. Critical to search.

### Typical Configuration

```ocaml
(* Conservative: captures only *)
let tactical_moves = generate_captures pos

(* Aggressive: captures + checks + promotions *)
let tactical_moves =
  generate_captures pos
  @ generate_checks pos
  @ generate_promotions pos
```

Most strong engines use **captures only** for performance reasons.

## Delta Pruning

An optimization: skip captures that can't possibly improve alpha:

```ocaml
let delta_margin = 900 in  (* Queen value + buffer *)

let quiescence pos alpha beta =
  let stand_pat = evaluate pos in
  if stand_pat >= beta then beta
  else
    let alpha = max alpha stand_pat in
    let captures = generate_captures pos in

    (* Delta pruning *)
    let viable_captures =
      captures |> List.filter (fun capture ->
        let captured_value = piece_value (captured_piece capture) in
        stand_pat + captured_value + delta_margin > alpha
      ) in

    search_captures viable_captures alpha beta
```

**Logic:** If you're behind by 500 centipawns (stand_pat + 500 < alpha), and the best capture only wins a pawn (100 centipawns), it can't possibly raise your score above alpha. Skip it!

**Exception:** Don't prune pawn promotions—they can swing evaluation by 800+ centipawns.

## Depth Limiting

Quiescence can go very deep in tactical positions (20+ plies). Limit it:

```ocaml
let max_quiescence_depth = 6 in

let rec quiescence pos alpha beta depth =
  if depth >= max_quiescence_depth then
    evaluate pos
  else
    (* Normal quiescence logic *)
    ...
```

Typical values: 4-10 plies. Deeper = more accurate but slower.

## Move Ordering in Quiescence

Move ordering matters even more in quiescence because you want quick cutoffs. See [move-ordering.md](move-ordering.md) for general strategies.

```ocaml
let order_captures pos captures =
  captures
  |> List.map (fun m -> (SEE.evaluate pos m, mvv_lva m, m))
  |> List.sort (fun (see1, mvv1, _) (see2, mvv2, _) ->
      compare (see2, mvv2) (see1, mvv1))
  |> List.map (fun (_, _, m) -> m)
```

**Order:**

1. Winning captures ([SEE](static-exchange-evaluation.md) > 0), best victim first
2. Equal trades (SEE = 0)
3. Losing captures (SEE < 0), least-bad first

**Critical:** Skip losing captures entirely in many cases:

```ocaml
let captures =
  generate_captures pos
  |> List.filter (fun m -> SEE.evaluate pos m >= 0)
```

This prunes bad captures that almost never improve the position.

## Complete Implementation

```ocaml
let rec quiescence pos alpha beta ?(depth = 0) () =
  (* Depth limit *)
  if depth >= Config.max_quiescence_depth () then
    Eval.evaluate pos
  else
    (* Stand-pat *)
    let stand_pat = Eval.evaluate pos in

    (* Beta cutoff *)
    if stand_pat >= beta then
      beta
    else
      (* Update alpha *)
      let alpha = max alpha stand_pat in

      (* Generate and order tactical moves *)
      let captures = Movegen.generate_captures pos in

      (* SEE pruning: skip losing captures *)
      let good_captures =
        captures |> List.filter (fun m ->
          SEE.evaluate pos m >= 0) in

      (* Delta pruning *)
      let delta_margin = 900 in
      let viable_captures =
        good_captures |> List.filter (fun m ->
          let victim_value = piece_value (captured_piece m) in
          let is_promotion = is_pawn_promotion m in
          is_promotion ||
          stand_pat + victim_value + delta_margin > alpha) in

      (* Order moves *)
      let ordered_captures = order_captures pos viable_captures in

      (* Search captures *)
      let rec search_moves moves alpha =
        match moves with
        | [] -> alpha
        | mv :: rest ->
          let new_pos = make_move pos mv in
          let score = -quiescence new_pos (-beta) (-alpha) ~depth:(depth + 1) () in
          if score >= beta then
            beta  (* Beta cutoff *)
          else
            let new_alpha = max alpha score in
            search_moves rest new_alpha
      in
      search_moves ordered_captures alpha
```

## SEE (Static Exchange Evaluation)

Critical for quiescence! [SEE](static-exchange-evaluation.md) evaluates capture sequences:

```ocaml
(* Example: Knight takes bishop on e4, defended by pawn *)
let see_score = SEE.evaluate pos (Knight, e4) in
(* Returns: 300 (bishop) - 300 (knight) = 0 (equal trade) *)
```

See [Static Exchange Evaluation](static-exchange-evaluation.md) for implementation details.

## Check Extensions

Some engines search checks in quiescence:

```ocaml
let tactical_moves =
  let captures = generate_captures pos in
  let checks = generate_checks pos in
  captures @ checks
```

**Tradeoff:**

- **Pro:** Better tactical accuracy, find more checkmates
- **Con:** 2-3x more nodes searched, slower overall

Most modern engines **skip checks** in quiescence for speed, relying on main search extensions to handle checking sequences.

## Common Pitfalls

### 1. Infinite Recursion

Without depth limit, perpetual checks or repetitive captures cause infinite loops:

```ocaml
(* Always have a depth limit! *)
if depth >= max_depth then evaluate pos
```

### 2. Not Using Stand-Pat

```ocaml
(* Wrong: Must allow standing pat *)
let alpha = stand_pat in  (* Lost the option to not move *)

(* Correct: *)
let alpha = max alpha stand_pat in
```

### 3. Searching Quiet Moves

```ocaml
(* Wrong: Searching ALL moves in quiescence *)
let moves = generate_all_moves pos in

(* Correct: Only tactical moves *)
let moves = generate_captures pos in
```

Searching quiet moves defeats the purpose—you'll search forever!

### 4. Poor Move Ordering

Trying losing captures first wastes time:

```ocaml
(* Bad: Random order *)
let captures = generate_captures pos in

(* Good: SEE ordering *)
let captures =
  generate_captures pos
  |> List.filter (fun m -> SEE.evaluate pos m >= 0)
  |> List.sort (fun m1 m2 ->
      compare (mvv_lva m2) (mvv_lva m1))
```

## Measuring Effectiveness

Track quiescence statistics:

```ocaml
type qsearch_stats = {
  calls: int;              (* Times quiescence called *)
  max_depth: int;          (* Deepest quiescence search *)
  cutoffs: int;            (* Beta cutoffs *)
  stand_pat_cutoffs: int;  (* Stand-pat caused cutoff *)
}
```

Good quiescence:

- **Avg depth:** 1-3 plies
- **Max depth:** 6-10 plies
- **Stand-pat cutoff rate:** 40-60%
- **Nodes:** 30-50% of total nodes

## Quiescence vs Main Search

| Aspect       | Main Search     | Quiescence              |
| ------------ | --------------- | ----------------------- |
| Moves        | All legal moves | Captures only           |
| Depth        | Fixed (e.g., 6) | Variable (until quiet)  |
| Stand-pat    | No              | Yes                     |
| Beta cutoff  | Yes             | Yes                     |
| Alpha update | From moves only | From stand-pat OR moves |

## Performance Impact

Quiescence typically:

- Adds 30-50% more nodes to search
- Prevents 90%+ of tactical blunders
- Increases average search depth by 3-5 plies

The tradeoff is hugely favorable—without it, your engine will blunder constantly in tactical positions.

## Further Reading

- [Static Exchange Evaluation](static-exchange-evaluation.md) - Essential for quiescence
- [Move Ordering](move-ordering.md) - Order captures for quick cutoffs
- [Alpha-Beta Pruning](alpha-beta-pruning.md) - Main search algorithm
- [Chess Programming Wiki - Quiescence Search](https://www.chessprogramming.org/Quiescence_Search)
- `lib/engine/search.ml` - See the `quiescence` function
