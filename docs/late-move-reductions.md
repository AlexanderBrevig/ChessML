---
layout: default
title: Late Move Reductions
parent: Chess Programming Guide
nav_order: 8
description: "My notes on searching late, quiet moves less deeply"
permalink: /docs/late-move-reductions
---

# Late Move Reductions

These are my notes on late move reductions (LMR), written while adding them to ChessML. They describe how I understand the technique and what ChessML does; for the real reference, see the [Chess Programming Wiki](https://www.chessprogramming.org/Late_Move_Reductions).

## The idea

If [move ordering]({% link docs/move-ordering.md %}) works, the best move in a position is usually among the first few that get searched. The quiet moves at the end of the list rarely turn out to be best. LMR bets on that: those late moves get a shallower search than the early ones.

A bet can be lost, so a reduced search that comes back better than expected is not trusted. The move is searched again at the normal depth, and only that result counts. When ordering is good, most late moves fail low at the reduced depth and are never looked at again, and the depth saved goes into the moves that matter. People report that LMR is one of the techniques that matters most; I have not measured what it gives ChessML.

## How it fits with PVS

ChessML's search is principal variation search (see [Alpha-Beta Pruning]({% link docs/alpha-beta-pruning.md %})): the first move gets the full window, every later move first gets a null window `(alpha, alpha + 1)` that only answers "is this better than alpha?". LMR slots into that null-window probe. For every move after the first, `search_moves` in `lib/engine/search.ml` does up to three searches (shortened):

```ocaml
let reduced_depth = max 0 (depth - 1 - reduction) in
(* 1. null window, possibly reduced *)
let s = search child ~alpha ~beta:(alpha + 1) ~depth:reduced_depth mv in
(* 2. it beat alpha at reduced depth: same null window at full depth *)
let s =
  if s > alpha && reduction > 0
  then search child ~alpha ~beta:(alpha + 1) ~depth:(depth - 1) mv
  else s
in
(* 3. at a PV node, a score inside the window needs the exact value *)
if s > alpha && s < beta && pv_node
then search child ~alpha ~beta ~depth:(depth - 1) mv
else s
```

Step 2 is the important one. Any score above alpha from the reduced search gets verified at full depth, including one that reaches beta. At a non-PV node beta is already alpha + 1, so "above alpha" and "fails high" are the same thing there, and without step 2 the reduced result would cause the cutoff. Step 3 only ever runs at PV nodes, because only there is the window wider than one point.

A PV node is simply one with an open window, `beta - alpha > 1`. Whether alpha has been raised yet in this node is a different question.

## When ChessML reduces

A move is reduced only if all of these hold:

- the remaining depth is at least 3;
- it is not among the first few moves: `move_count > 3`, or `move_count > 5` at a PV node;
- it is quiet: not a capture and not a promotion (`LMR.is_tactical_move`);
- it does not give check, and the side to move is not in check;
- it is not a killer move for this ply.

How much to reduce comes from a table in `lib/engine/search_common.ml`, filled at startup with `ln(depth) * ln(move_number) / 2.5`, rounded, and at least 1 (entries for depth below 3 or move number below 4 are 0). Some values:

| depth \ move | 4 | 6 | 10 | 20 | 40 |
| --- | --- | --- | --- | --- | --- |
| 3 | 1 | 1 | 1 | 1 | 2 |
| 6 | 1 | 1 | 2 | 2 | 3 |
| 12 | 1 | 2 | 2 | 3 | 4 |
| 20 | 2 | 2 | 3 | 4 | 4 |

The table value is then adjusted a little (this is the real function):

```ocaml
let reduction depth move_count ~is_pv ~history_score =
  let base = reduction_table.(min 63 depth).(min 63 move_count) in
  let adjust = (if is_pv then 1 else 0) + if history_score > 1000 then 1 else 0 in
  max 1 (base - adjust)
```

So PV nodes and moves with a good [history score]({% link docs/move-ordering.md %}#history-heuristic) (`History.get_score st.history mv`) are reduced one ply less, but every move that qualifies is reduced by at least one ply. The reduced depth is clamped at 0, not 1, so a late move near the leaves can go straight into [quiescence search]({% link docs/quiescence-search.md %}).

## What ChessML does not do

Stronger engines adjust the reduction by more things: whether the static eval is improving compared to two plies ago, whether a move has a bad history (reduce more), whether the TT move is a capture, and so on. ChessML has none of that; history can only lower its reductions, never raise them. It also has late move pruning and futility pruning next to LMR (in the same loop, at non-PV nodes only), which skip some late quiet moves outright instead of reducing them.

## Pitfalls

- **Trusting a reduced fail-high.** If the reduced null-window search beats alpha, search again at full depth before using the score. Testing `score < beta` instead does nothing at non-PV nodes, where beta is alpha + 1.
- **Reducing tactical moves.** Captures, promotions and checks can change the evaluation a lot in one move, so they are normally not reduced. Forgetting promotions in the "tactical" test is easy.
- **Reducing while in check.** In check there are few legal moves and all of them are forced; reducing them hides mates.
- **Reducing too early.** The first moves (TT move, good captures, killers) are where the best move usually is; reducing them throws away the benefit of ordering.

## Sources

- [Chess Programming Wiki: Late Move Reductions](https://www.chessprogramming.org/Late_Move_Reductions)
- ChessML's code: `lib/engine/search.ml` (`search_moves`), `lib/engine/search_common.ml` (`LMR`), `lib/engine/history.ml`
