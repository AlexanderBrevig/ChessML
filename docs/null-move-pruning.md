---
layout: default
title: Null Move Pruning
parent: Chess Programming Guide
nav_order: 7
description: "My notes on pruning by letting the opponent move twice"
permalink: /docs/null-move-pruning
---

# Null Move Pruning

These are my notes on null move pruning. They describe how I understand it and what ChessML does; for the real reference, see the [Chess Programming Wiki](https://www.chessprogramming.org/Null_Move_Pruning).

## The idea

In almost every chess position, having the move is an advantage: there is nearly always something useful to do. Null move pruning turns that into a test. Before searching a node properly, pretend the side to move passes (a "null move") and let the opponent move again, with a shallower search. If my position is still at or above beta even after giving the opponent a free move, then with a real move it will very likely be at least as good, so the node can be cut off without searching any real moves.

The test is cheap because it uses a null window (only "is it at least beta?") and a reduced depth. The reduction is called R: the null move is searched to `depth - 1 - R` instead of `depth - 1`.

## When passing would help: zugzwang

The assumption "a move is never worse than passing" is false in zugzwang, where every legal move makes things worse. It mostly happens in endgames with few pieces, typically king and pawn endings. A classic example is the trébuchet, a mutual zugzwang: the two pawns block each other, each king stands guarding its own pawn while attacking the other one, and whoever has to move must step away and lose their pawn (and usually the game). In a position like that, the null move test says "even if I pass, I am fine", while in fact the side to move is lost precisely because it cannot pass.

The usual protection, and the one ChessML uses, is to skip the null move when the side to move has nothing but its king and pawns. Pieces give it spare moves, so zugzwang is rarer then (not impossible).

## What ChessML does

From `search_node` in `lib/engine/search.ml` (shortened; `can_prune` is defined just above it):

```ocaml
let can_prune = (not pv_node) && (not in_check) && abs beta < Score.mate_bound in
...
if can_prune
   && null_ok
   && depth >= 3
   && static_eval >= beta
   && Position.count_non_pawn_material pos side > 0
then (
  let r = if depth > 6 then 3 else 2 in
  let score =
    -alphabeta ctx (Position.make_null_move pos)
       ~alpha:(-beta) ~beta:(-beta + 1)
       ~depth:(depth - 1 - r) ~ply:(ply + 1)
       ~prev_move:None ~null_ok:false
  in
  if score >= beta
  then Some (if Score.is_mate score then beta else score)
  else None)
```

So a null move is only tried when:

- the node is not a PV node (null-window nodes only);
- the side to move is not in check (passing out of check is not a legal position);
- beta is not a mate score;
- `null_ok` is set, which it is not right after another null move;
- at least 3 plies are left;
- the static evaluation is already at or above beta, so a cutoff is plausible;
- the side to move has at least one knight, bishop, rook or queen (`Position.count_non_pawn_material`), against zugzwang.

R is 2, or 3 when more than 6 plies are left. If the null search fails high, ChessML returns its score (fail-soft), except when that score is a mate: a mate found after a pass is not a real mate, so it returns plain beta then.

The child is called with `~null_ok:false`. Two null moves in a row would not break anything, but they cancel out: the same position comes back with the same side to move, just shallower, so the search would only waste depth on it.

### Making the null move

`Position.make_null_move` in `lib/engine/position.ml` flips the side to move, clears the en passant square and bumps the halfmove clock. It also has to keep the hash key right, by XORing out the en passant key (if one was hashed) and flipping the side-to-move key:

```ocaml
let make_null_move pos =
  let key =
    List.fold_left Int64.logxor pos.key [ ep_key pos; Zobrist.white_to_move_key ]
  in
  { pos with
    side_to_move = Color.opponent pos.side_to_move
  ; ep_square = None
  ; halfmove = pos.halfmove + 1
  ; key
  }
```

Without the key update, the position after a null move shares its key with the position before it, and the transposition table mixes up the two.

## Things ChessML does not do

- **Verification search.** Some engines, when the null move fails high, run a reduced normal search to confirm it before cutting, as extra protection against zugzwang.
- **Mate threat detection.** If the null search shows that the opponent mates after a pass, the position contains a serious threat; some engines extend the search there. ChessML just carries on with the normal search.
- **A larger R based on how far the eval is above beta.** ChessML only looks at depth.

The other pruning margins (futility, reverse futility, razoring, late move pruning) live in [`search_common.ml`](https://github.com/AlexanderBrevig/ChessML/blob/main/lib/engine/search_common.ml).

## Pitfalls

- **Null move while in check.** The resulting position has the side not to move in check, which cannot happen in a legal game, and the search returns nonsense. Test for check first.
- **Two null moves in a row.** Not wrong, but useless: the second one undoes the first and the remaining depth is wasted. Pass a flag to the child.
- **Forgetting the hash key.** The side-to-move key and the en passant key must be updated like in a real move.
- **Zugzwang in pawn endings.** Without a material condition the engine misjudges king and pawn endings, where zugzwang is common.
- **Trusting mate scores from the null search.** Returning a "mate" that depends on a pass gives wrong mate scores; return beta instead.

## Sources

- [Chess Programming Wiki: Null Move Pruning](https://www.chessprogramming.org/Null_Move_Pruning)
- [Chess Programming Wiki: Zugzwang](https://www.chessprogramming.org/Zugzwang)
- ChessML's code: `lib/engine/search.ml` (`search_node`), `lib/engine/position.ml` (`make_null_move`, `count_non_pawn_material`)
