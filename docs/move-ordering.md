---
layout: default
title: Move Ordering
parent: Chess Programming Guide
nav_order: 9
description: "My notes on choosing which move to search first"
permalink: /docs/move-ordering
---

# Move Ordering

These are my notes on move ordering, written while building ChessML. They describe how I understand it and what ChessML does; for the real reference, see the [Chess Programming Wiki](https://www.chessprogramming.org/Move_Ordering).

## The idea

[Alpha-beta]({% link docs/alpha-beta-pruning.md %}) gets its savings from cutoffs: once one move in a node is shown to be good enough, the remaining moves of that node are skipped. The sooner a strong move is tried, the sooner that happens. With a perfect order, alpha-beta looks at roughly the square root of the nodes plain minimax would; with a bad order it saves little. So before searching, an engine scores each move with cheap guesses about how good it is and tries the best-scored moves first.

## What ChessML does

`Ordering.score_move` in `lib/engine/search_common.ml` gives every move one number, and `order_moves` sorts by it with `List.stable_sort`. The first rule that matches decides the score:

| Move | Score |
| --- | --- |
| TT move (best move stored for this position) | 20000 |
| castling | 15000 |
| gives check | 10000 |
| promotion | 9000 + 500 (queen), 100 (rook) or 50 (minor) |
| capture that wins material (SEE > 0) | 8000 + SEE, at most 1000 extra |
| capture that breaks even (SEE = 0) | 7000 |
| killer move | 5000 |
| countermove | 4000 |
| other quiet moves | history / 10, at most 3000 |
| capture that loses material (SEE < 0) | the SEE value (negative, at least -5000) |

Captures are ranked by [static exchange evaluation]({% link docs/static-exchange-evaluation.md %}), which plays out the exchanges on the target square and returns the material result. The file also has an MVV-LVA function (most valuable victim, least valuable attacker), the cheaper and more common way to order captures, but nothing calls it.

Putting all checks and all castling moves this high is my own choice, not a standard recipe. "Gives check" is found by playing the move and asking whether the opponent is in check, which is exact (it sees discovered checks) but costs a `make_move` per move.

The three tables below live in `Search.state` and are updated only when a **quiet** move causes a beta cutoff; captures and promotions never update them. This is the update in `search_moves`:

```ocaml
if score >= beta
then (
  if quiet
  then (
    Killers.store_killer st.killers ply mv;
    History.record_cutoff st.history mv depth;
    Option.iter (fun pm -> Countermoves.update st.countermoves pm mv) prev_move);
  best_score, best_move)
```

### TT move

If the [transposition table]({% link docs/transposition-tables.md %}) has an entry for this position, its best move goes first, even when the entry is too shallow for its score to be used. It is usually what the previous iteration of iterative deepening found best.

### Killer move heuristic

A killer is a quiet move that caused a cutoff at the same ply in a sibling node. Positions at the same distance from the root often share a threat, so the refutation tends to work again. `lib/engine/killers.ml` keeps two per ply, most recent first; storing a move that is already there does nothing. The killers are cleared at the start of each search.

### Countermove heuristic

A countermove is indexed by the opponent's previous move instead of by ply: "last time they played this, that reply cut off". `lib/engine/countermoves.ml` keeps one reply per from/to square of the previous move. `Countermoves.is_countermove table prev_move mv` takes the previous move as a `Move.t option`, since the root and the node after a null move have none.

### History heuristic

History collects evidence across the whole tree. `lib/engine/history.ml` keeps a 64 × 64 table indexed by from and to square. `History.record_cutoff table mv depth` adds `depth * depth` (deep cutoffs count more), capped at 10000, and `History.get_score table mv` reads it back. `History.age` halves every entry at the start of each search, so knowledge from earlier moves of the game fades instead of piling up. ChessML only rewards cutoffs; it does not punish quiet moves that were tried and failed.

The history score is also used by [LMR]({% link docs/late-move-reductions.md %}): a quiet move with history above 1000 is reduced one ply less.

## What ChessML does not do

ChessML scores and sorts the whole list before searching. A cheaper alternative is incremental selection: pick the best remaining move, search it, and only then look for the next one. If the first move cuts off, the rest never get scored, which saves the SEE and `make_move` calls.

Other ideas I have not tried: penalizing quiet moves that did not cut off, history indexed by piece instead of from-square, and continuation history (history keyed by the previous move as well).

## Pitfalls

- **Letting captures into the quiet-move tables.** Captures are already ordered by what they win; storing them as killers or in history pushes out the quiet moves those tables are for.
- **Sorting losing captures with the winning ones.** A queen taking a defended pawn looks attractive by victim value but usually loses; ordering by SEE (or putting SEE < 0 captures last) fixes that.
- **History that only grows.** Without a cap and some aging, old scores from a different part of the game dominate the ordering.

## Sources

- [Chess Programming Wiki: Move Ordering](https://www.chessprogramming.org/Move_Ordering)
- [Chess Programming Wiki: Killer Heuristic](https://www.chessprogramming.org/Killer_Heuristic)
- [Chess Programming Wiki: History Heuristic](https://www.chessprogramming.org/History_Heuristic)
- [Chess Programming Wiki: Countermove Heuristic](https://www.chessprogramming.org/Countermove_Heuristic)
- ChessML's code: `lib/engine/search_common.ml` (`Ordering`), `lib/engine/search.ml`, `lib/engine/killers.ml`, `lib/engine/history.ml`, `lib/engine/countermoves.ml`
