---
layout: default
title: Alpha-Beta Pruning
parent: Chess Programming Guide
nav_order: 4
description: "My notes on alpha-beta, negamax and principal variation search"
permalink: /docs/alpha-beta-pruning
---

# Alpha-Beta Pruning

These are my notes on the search algorithm at the center of ChessML. They describe how I understand it and what ChessML does; for the real reference, see the [Chess Programming Wiki](https://www.chessprogramming.org/Alpha-Beta).

## The idea

Minimax looks at every move, every reply, every reply to that, down to a fixed depth, and assumes both sides pick their best option. Alpha-beta gets the same score while skipping branches that cannot change it.

The smallest example I could come up with. It is my move and I compare two candidates:

```
Move A: after the opponent's best reply I am at +2.
Move B: the first reply I look at already leaves me at -1.
```

I do not need the other replies to B. The opponent can always choose that -1 reply (or something even better for them), so B is worth at most -1 to me, and A already guarantees +2. B is worse than A whatever the other replies are, so I stop looking at B. That stop is a cutoff.

The search carries two numbers down the tree to make this work:

- **alpha**: the score I am already sure of somewhere else (here +2, from A).
- **beta**: the score the opponent is already sure of; if I find something at or above it, they will avoid this line, and I can stop.

Anything between them is the window of scores that still matters.

## Negamax

Instead of writing a "max" side and a "min" side, nearly every engine (ChessML too) uses negamax: a score is always from the point of view of the side to move, and a child's score is negated on the way up. For this to work, the evaluation must also score from the side to move's perspective. ChessML's `Eval.evaluate` does.

A simplified sketch (not ChessML's code; `evaluate`, `legal_moves`, `make_move` and `in_check` stand for the obvious functions):

```ocaml
let rec negamax pos ~depth ~ply ~alpha ~beta =
  if depth = 0
  then evaluate pos
  else (
    match legal_moves pos with
    | [] -> if in_check pos then Score.mated_in ply else Score.draw
    | moves ->
      let rec loop best alpha = function
        | [] -> best
        | mv :: rest ->
          let score =
            -negamax (make_move pos mv) ~depth:(depth - 1) ~ply:(ply + 1)
               ~alpha:(-beta) ~beta:(-alpha)
          in
          if score >= beta
          then score (* cutoff: the opponent will not allow this position *)
          else loop (max best score) (max alpha score) rest
      in
      loop (-Score.infinity) alpha moves)
```

Two details I got wrong at first:

- **No legal moves is not "return alpha".** It is checkmate if the side to move is in check, otherwise stalemate, which is a draw.
- **Fail-soft vs fail-hard.** This sketch returns the best score it saw, even when that is outside the window ("fail-soft"). A fail-hard version clamps the result to alpha or beta. Both give the same move at the root; fail-soft gives the caller (and the transposition table) a slightly tighter bound. ChessML is fail-soft.

Plain alpha-beta returns the same score as minimax at the same depth. It does not always return the same move, since two moves with equal scores can come out in a different order. Once a transposition table and pruning are added, even the score can differ.

## Why move order matters

How much alpha-beta saves depends entirely on the order moves are tried. If the best move always comes first, the search visits roughly 2·b^(d/2) leaf positions instead of b^d (b = moves per position, d = depth), which is like searching twice as deep for the same work. With bad order it saves much less. So most of what an engine does around the search is about trying good moves early; see [Move Ordering]({% link docs/move-ordering.md %}).

ChessML's order, from `Search_common.Ordering.score_move` (higher first):

| Score | Moves |
| --- | --- |
| 20000 | the move stored in the [transposition table]({% link docs/transposition-tables.md %}) |
| 15000 | castling |
| 10000 | moves that give check |
| 9000+ | promotions (queen highest) |
| 8000 + SEE | captures that win material by [SEE]({% link docs/static-exchange-evaluation.md %}) |
| 7000 | captures that trade evenly |
| 5000 | killer moves |
| 4000 | the countermove to the opponent's last move |
| history / 10 | other quiet moves, by history score |
| negative | captures that lose material |

## Principal variation search

With good ordering the first move is usually the best one. Principal variation search (PVS) uses that: it searches the first move with the full window, and every later move with a "null window" (alpha, alpha + 1), which only answers "is this better than alpha, yes or no?". Null-window searches cut off much more. If one does come back above alpha, the guess was wrong and that move is searched again with the full window.

This is ChessML's version, shortened from `search_moves` in `lib/engine/search.ml` (the reduction is [late move reductions]({% link docs/late-move-reductions.md %})):

```ocaml
let score =
  if move_count = 1
  then search child ~alpha ~beta ~depth:(depth - 1) mv
  else (
    let reduced_depth = max 0 (depth - 1 - reduction) in
    (* Null window search, possibly reduced *)
    let s = search child ~alpha ~beta:(alpha + 1) ~depth:reduced_depth mv in
    let s =
      if s > alpha && reduction > 0
      then search child ~alpha ~beta:(alpha + 1) ~depth:(depth - 1) mv
      else s
    in
    (* Full window re-search at PV nodes *)
    if s > alpha && s < beta && pv_node
    then search child ~alpha ~beta ~depth:(depth - 1) mv
    else s)
```

A node is a "PV node" when its window is wider than one (`beta - alpha > 1`). Only those can produce an exact score, and ChessML does its selective pruning only at the other nodes.

## What else ChessML's search does

- **Mate scores with distance.** Mate is `Score.mate` (31000) minus the ply, built with `Score.mated_in ply`, so a mate in 2 scores higher than a mate in 5. `Score.infinity` (32000) is the starting window, and `Score.is_mate` tests against `Score.mate_bound` (`mate - max_ply`). The code never writes mate values by hand.
- **Mate distance pruning.** At ply `p` the best possible result is mating on the next move and the worst is being mated right now, so the window is clamped to those two scores. If a shorter mate is already known elsewhere, the clamped window is empty and the node returns at once:

  ```ocaml
  let alpha = max alpha (Score.mated_in ply) in
  let beta = min beta (Score.mate - ply - 1) in
  if alpha >= beta then alpha else ...
  ```

- **Check extension.** When the side to move is in check, the node gets one extra ply (`if in_check then depth + 1`).
- **Draws inside the tree.** `is_draw` returns a draw for the fifty-move rule, insufficient material, and a repetition of any earlier position, both from the game so far and from the current search path. Without this ChessML shuffled won endings into threefold repetition.
- **Iterative deepening.** `find_best_move` searches depth 1, then 2, then 3, and so on. Each iteration fills the transposition table and the ordering tables, which makes the next one cheaper. It stops at the depth limit, when a mate is found within the searched depth, or when half the time is used (no new iteration is started).
- **Time checks inside the search.** Once the first iteration has finished, every 2048 nodes `poll` checks the clock and the `stop` flag, and raises an exception that abandons the iteration in progress. The result of the last completed iteration is used.
- **Quiescence at the leaves.** At depth 0 it does not call the evaluation directly but runs a [quiescence search]({% link docs/quiescence-search.md %}).

There is also [null move pruning]({% link docs/null-move-pruning.md %}) and a few cheaper pruning tricks (reverse futility, razoring, futility, late move pruning) whose margins live in [`search_common.ml`](https://github.com/AlexanderBrevig/ChessML/blob/main/lib/engine/search_common.ml). ChessML does not use aspiration windows: every iteration starts with the full window.

## Pitfalls

- **Negamax with a White-perspective eval.** If the evaluation returns "good for White" instead of "good for the side to move", Black plays the worst moves it can find.
- **Flat mate scores.** If every mate scores the same, the engine has no reason to prefer the short mate and can keep postponing it. ChessML had this bug. Subtract the ply, and adjust mate scores when storing them in the transposition table.
- **Treating "no moves" as a normal leaf.** Without the checkmate/stalemate test the engine does not know it is mated, or happily walks into stalemate.
- **Repetitions not detected in the search.** The engine cannot see that a line repeats, so it misses draws it could save and repeats its way out of won positions.
- **Time only checked between iterations.** One deep iteration can take several times longer than the previous one; ChessML overran its time by about 2x before it polled inside the search.

## Sources

- [Chess Programming Wiki: Alpha-Beta](https://www.chessprogramming.org/Alpha-Beta)
- [Chess Programming Wiki: Principal Variation Search](https://www.chessprogramming.org/Principal_Variation_Search)
- [Chess Programming Wiki: Mate Distance Pruning](https://www.chessprogramming.org/Mate_Distance_Pruning)
- Donald Knuth and Ronald Moore, "An Analysis of Alpha-Beta Pruning" (1975), for the size of the minimal tree
- ChessML's code: `lib/engine/search.ml`, `lib/engine/search_common.ml`, `lib/engine/score.ml`
