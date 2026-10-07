---
layout: default
title: Evaluation Function
parent: Chess Programming Guide
nav_order: 11
description: "My notes on turning a position into a number, and what ChessML scores"
permalink: /docs/evaluation-function
---

# Evaluation Function

These are my notes on how an engine puts a number on a position, and what ChessML happens to count. They are not a guide to good evaluation; for that, see the [Chess Programming Wiki](https://www.chessprogramming.org/Evaluation).

## The idea

The search cannot play every game to the end, so at the leaves it stops and asks: "how good does this look?". The evaluation answers with a single integer in centipawns (100 = one pawn). It is static: it looks at the board as it stands and does not try moves. Tactics are the search's job, in particular [quiescence search]({% link docs/quiescence-search.md %}), which keeps resolving captures before the evaluation is asked.

ChessML's `Eval.evaluate` scores from the point of view of the side to move: positive means good for whoever is about to move. That is what negamax wants, because each level just negates the child's score. It returns 0 when neither side has enough material to mate.

Almost every evaluation I have read starts with the same two parts, material and piece-square tables, and then adds smaller terms on top.

## Material

Count the pieces and multiply by a value. ChessML uses `PieceKind.value` in `lib/core/types.ml`: pawn 100, knight 320, bishop 330, rook 500, queen 900. `Position.material` adds these up for one color with the king left out, so each side starts with 8 × 100 + 2 × 320 + 2 × 330 + 2 × 500 + 900 = 4000. Counting is one `Bitboard.population` per piece type ([bitboards]({% link docs/bitboards.md %})).

## Piece-square tables

A knight in the middle of the board does more than one in the corner. A piece-square table (PST) gives a small bonus or penalty for each piece type on each of the 64 squares, and the evaluation adds up the entries for every piece on the board.

The part that tripped me up is orientation. The tables are easiest to type in as you would draw a board, with a8 in the top left, so index 0 is a8. ChessML's squares count from a1 = 0. The two layouts differ only in the rank, so `sq lxor 56` (flip the rank bits, keep the file) converts one to the other. A White piece therefore reads `table.(sq lxor 56)`. A Black piece should see the table mirrored top to bottom, which happens to be exactly what you get by reading `table.(sq)` directly. This is `Piece_tables.piece_square_value`:

```ocaml
let piece_square_value (piece : piece) (sq : Square.t) : int =
  (* Tables are a8-first, squares are a1-first: flip for White, Black reads it mirrored *)
  let table_sq = if piece.color = White then sq lxor 56 else sq in
  match piece.kind with
  | Pawn -> pawn_table.(table_sq)
  | Knight -> knight_table.(table_sq)
  (* ... bishop, rook, queen ... *)
  | King -> king_middlegame_table.(table_sq)
```

A common way to mirror for Black is `63 - sq`. That flips the files too, which makes no difference while a table is the same on both wings, but turns an asymmetric table (a king that likes g1 more than b1, for example) into nonsense for one side.

## Tapered evaluation (not in ChessML)

A king should hide in the middlegame and walk to the center in the endgame, so one table cannot be right for both. The usual answer is to keep two scores, a middlegame one and an endgame one, and blend them by a game phase computed from the remaining non-pawn material: all middlegame at the start, all endgame when the pieces are gone, a weighted average in between.

ChessML does not do this. It has a single `king_middlegame_table`, and its endgame knowledge comes from separate terms instead (below). Tapering is on my list.

## Pawn structure

Pawns move slowly and cannot go back, so their weaknesses last. ChessML scores each pawn in `lib/engine/eval_pawn_structure.ml`:

| Term | ChessML's rule | Score |
| --- | --- | --- |
| passed | no enemy pawn ahead on its own or adjacent files, and it is the frontmost friendly pawn on its file | by relative rank (0-based from its own side): 0, 0, 15, 30, 60, 150, 300; +20 more if supported |
| doubled | more than one friendly pawn on the file | -20 per extra pawn, charged once per file |
| isolated | no friendly pawn on either adjacent file | -20 |
| backward | there are friendly pawns on adjacent files, but all of them are ahead of it (so none can defend it); not counted if isolated | -10 |
| supported | a friendly pawn beside it or diagonally behind it | +5 |
| central | on the d- or e-file | +5 |

The list runs from relative rank 0 (its own back rank, where a pawn never stands) to 6 (the seventh rank). There is no entry for the last rank, because a pawn that gets there has already promoted.

Many engines also call a pawn backward only when the square in front of it is attacked by an enemy pawn. ChessML does not check that, so it penalizes some pawns that could safely advance.

Pawn structure depends only on where the pawns are, and the same pawn structure appears over and over in a search. So ChessML caches the result in `Pawn_cache`, keyed by both pawn bitboards (White's and Black's, compared exactly on a hit), and stores the score as White minus Black. `Eval` negates it when Black is to move.

## King safety

Real king safety looks at the pawn shield, open files near the king and enemy pieces aiming at it. ChessML's version (`lib/engine/eval_king_safety.ml`) is only about castling, and has no pawn shield term:

- king on g1/c1 (g8/c8 for Black), counted as castled: +120
- not castled, still has a castling right and the squares between king and rook on that side are empty: +80
- has a right but the path is blocked: +30
- lost all castling rights without castling: -60 while total material is above 6000, otherwise -20

## Development

For the first 15 moves (`Position.fullmove <= 15`), `Eval_pieces.evaluate_development` nudges the engine to get its pieces out:

- -25 for each knight or bishop still on its starting square, +15 for each one that has left
- -40 if the queen has left d1/d8 before two minor pieces are out
- +40 if the two rooks are on the same rank with nothing between them
- +5 for each piece (not pawn or king) that is defended

There is also a bishop pair bonus of +50 in `Eval_pieces.evaluate_bishop_pair`.

## Trade incentive

When you are ahead, trading pieces makes the extra material count for more. An earlier version of these notes penalized the material difference itself, which does nothing for trades: the difference stays the same when equal pieces come off. What ChessML does is look at how many pieces are left:

```ocaml
(* from Eval.evaluate_position *)
let trade_incentive =
  let pieces =
    Position.count_non_pawn_material pos side
    + Position.count_non_pawn_material pos opponent
  in
  if material_diff > 200
  then -(pieces * 5)
  else if material_diff < -200
  then pieces * 5
  else 0
in
```

`count_non_pawn_material` counts knights, bishops, rooks and queens (pieces, not centipawns). The side more than two pawns ahead loses 5 for every piece still on the board, so each trade raises its score; the side behind gets the opposite.

## Putting it together

`Eval.evaluate_position` adds up, each as "us minus them" from the side to move's view:

1. material
2. piece-square tables
3. pawn structure (cached)
4. trade incentive
5. king safety
6. development
7. bishop pair
8. fifty-move incentive (when the halfmove clock is high, the side ahead is pushed to make progress)
9. rook endgame (`Eval_endgame.evaluate_rook_endgame`: cutting off the enemy king, rook behind a passed pawn)
10. ladder mate (`Eval_endgame.evaluate_ladder_mate`: two major pieces against a nearly bare king)

`Eval.evaluate` wraps this and returns 0 first if `Position.has_insufficient_material`.

### Things ChessML does not have

Mobility (how many squares each piece can reach), rooks on open files, knight outposts and king tropism are standard in other engines and absent here. People report that mobility in particular matters a lot; I have not tried it in ChessML.

## Testing it

Two tests I found worth having (`test/engine/test_eval.ml`):

- **Color symmetry.** Flip the board vertically, swap the colors of all pieces, swap the side to move and the castling rights. Because the score is from the side to move's view, the flipped position must get the *same* score, not the negated one. `test_color_symmetry` checks this on a handful of FENs.
- **Known positions.** The start position should be close to 0, a side a piece up clearly positive, bare kings exactly 0. `test_pst_orientation` pins a few table lookups down (a White pawn on e4 must score more than one on e2), which is the test that would have caught the orientation bug below.

Whether a new term helps can only be found out with games. Small differences need many games: a 55% score over 200 games is still within the noise. How I run matches is in the [README](https://github.com/AlexanderBrevig/ChessML/blob/main/README.md#strength) and in [CLAUDE.md](https://github.com/AlexanderBrevig/ChessML/blob/main/CLAUDE.md).

## Pitfalls

- **Tables read upside down.** Tables typed a8-first but indexed a1-first make one or both colors read them upside down, with no crash and no failing test unless you test symmetry. ChessML had this bug: a pawn on e2 got the bonus meant for e7.
- **Mirroring with `63 - sq`.** It also swaps the files, which breaks every table that is not symmetric left to right.
- **Wrong perspective.** Mixing "White minus Black" and "side to move" scores, for example caching one and returning it as the other, gives a score with the wrong sign half of the time.
- **A cache keyed on too little.** The pawn cache must be keyed on everything the cached value depends on. A key built by XOR-ing the two pawn bitboards maps a structure and its color swap to the same entry; ChessML had that bug and now compares both bitboards.
- **Doing the search's work in the evaluation.** ChessML once subtracted the value of every attacked piece. That duplicated quiescence search, was wrong for the side to move (it can simply move the piece away), and the inflated static scores let pruning cut real tactics. It was removed.

## Sources

- [Chess Programming Wiki: Evaluation](https://www.chessprogramming.org/Evaluation)
- [Chess Programming Wiki: Piece-Square Tables](https://www.chessprogramming.org/Piece-Square_Tables)
- [Chess Programming Wiki: Pawn Structure](https://www.chessprogramming.org/Pawn_Structure)
- [Chess Programming Wiki: Tapered Eval](https://www.chessprogramming.org/Tapered_Eval)
- ChessML's code: `lib/engine/eval.ml`, `eval_pawn_structure.ml`, `eval_pieces.ml`, `eval_king_safety.ml`, `eval_endgame.ml`, `piece_tables.ml`, `pawn_cache.ml`
