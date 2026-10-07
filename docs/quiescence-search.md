---
layout: default
title: Quiescence Search
parent: Chess Programming Guide
nav_order: 6
description: "My notes on searching captures until the position is quiet"
permalink: /docs/quiescence-search
---

# Quiescence Search

These are my notes on quiescence search. They describe how I understand it and what ChessML does; for the real reference, see the [Chess Programming Wiki](https://www.chessprogramming.org/Quiescence_Search).

## The idea

The [main search]({% link docs/alpha-beta-pruning.md %}) stops at a fixed depth and asks the evaluation function for a score. The evaluation only counts what is on the board. So if the depth runs out right after my queen takes a defended pawn (QxP), the evaluation sees "a pawn up", and without anything more the engine never sees the reply ...PxQ. It will happily play into lines like that. This is called the horizon effect.

Quiescence search is a small extra search at the leaves that only plays the forcing moves, mostly captures, until nothing is hanging any more. Only then is the evaluation trusted.

## Stand pat

In quiescence the side to move is not forced to capture. It can "stand pat": take the static evaluation as its score, on the assumption that it has some quiet move that keeps at least that much. So:

- the evaluation is a lower bound for the side to move;
- if it is already at or above beta, return at once;
- otherwise it becomes the score to beat, and captures are tried to see if any of them does better.

The assumption fails when the side to move is **in check**. Then there is no quiet "do nothing" option: every legal move must get out of check, all of them have to be searched, and if there are none it is checkmate. ChessML handles this now; for a while it took stand pat in check too.

## What ChessML does

This is the whole function, from `lib/engine/search.ml` (comments mine, shortened):

```ocaml
let rec quiescence ctx pos alpha beta ply qdepth =
  poll ctx;
  if ply >= Score.max_ply - 1
  then Eval.evaluate pos
  else if Movegen.in_check pos
  then (
    (* no stand pat in check: every evasion must be searched *)
    match Movegen.generate_moves pos with
    | [] -> Score.mated_in ply
    | moves ->
      search_quiescence_moves ctx pos moves (-Score.infinity) alpha beta ply qdepth)
  else (
    let stand_pat = Eval.evaluate pos in
    if stand_pat >= beta || qdepth >= Config.get_max_quiescence_depth ()
    then stand_pat
    else
      search_quiescence_moves ctx pos
        (Search_common.tactical_moves ~include_checks:(qdepth = 0) pos)
        stand_pat (max alpha stand_pat) beta ply qdepth)
```

`search_quiescence_moves` orders the moves with the same scoring as the main search (without TT move, killers or countermoves) and runs an ordinary fail-soft alpha-beta loop over them, with stand pat as the starting best score.

### Which moves

`Search_common.tactical_moves` decides what is searched when not in check:

```ocaml
let tactical_moves ?(include_checks = false) pos =
  List.filter
    (fun mv ->
       if Move.is_capture mv
       then See.evaluate pos mv >= 0
       else Move.is_promotion mv || (include_checks && Ordering.gives_check pos mv))
    (Movegen.generate_moves pos)
```

So ChessML searches:

- captures that do not lose material according to [static exchange evaluation]({% link docs/static-exchange-evaluation.md %}) (`See.evaluate pos mv >= 0`). With ChessML's values (knight 320, bishop 330), knight takes a bishop that a pawn defends is 330 - 320 = +10, so it is kept; queen takes a pawn defended by a pawn is 100 - 900 and is skipped;
- all promotions, captures or not;
- quiet moves that give check, but only at the first quiescence ply (`qdepth = 0`);
- all evasions when in check.

Engines differ here. Some search only captures; others also try checks for a ply or two, as ChessML does. The checks let quiescence notice simple checking tactics; I have not measured whether they are worth their cost in ChessML.

### A depth limit

Captures alone cannot go on forever, since each one removes a piece. Checks can (a perpetual check is exactly that), which is one reason ChessML only adds quiet checks at the first quiescence ply. Even so, long capture chains can blow up the search, so ChessML has a limit: when `qdepth` reaches `Config.get_max_quiescence_depth ()` (8 by default, the `QuiescenceDepth` option) it returns stand pat. The limit is only checked when not in check. The `ply` guard on `Score.max_ply` is a last safety net.

## Things ChessML does not do

- **Delta pruning.** The idea: if stand pat plus the value of the captured piece plus a safety margin (a couple of pawns, say) still cannot reach alpha, skip that capture. Promotions should be exempt. ChessML relies on the SEE filter instead; delta pruning is on my list.
- **Transposition table probes in quiescence.** ChessML neither reads nor writes the TT here.

## Pitfalls

- **Stand pat while in check.** The side to move has no free "pass", so quiescence returns a fake score and misses mates. ChessML had this bug.
- **Searching every capture.** Without the SEE filter (or a similar cut), quiescence spends most of its time on obviously losing captures like queen takes a defended pawn.
- **Checks at every quiescence ply.** Check sequences can repeat or go very deep; limit them to the first ply or two, and keep a depth limit.
- **Evaluation terms that do quiescence's job.** A "hanging piece" penalty in the evaluation counts threats that quiescence already resolves, and pruning based on that evaluation then cuts real tactics. ChessML's evaluation still has such a term (it once penalized the wrong side), so this is one I keep an eye on.

## Sources

- [Chess Programming Wiki: Quiescence Search](https://www.chessprogramming.org/Quiescence_Search)
- [Chess Programming Wiki: Horizon Effect](https://www.chessprogramming.org/Horizon_Effect)
- [Chess Programming Wiki: Delta Pruning](https://www.chessprogramming.org/Delta_Pruning)
- ChessML's code: `lib/engine/search.ml` (`quiescence`), `lib/engine/search_common.ml` (`tactical_moves`), `lib/engine/see.ml`, `lib/engine/config.ml`
