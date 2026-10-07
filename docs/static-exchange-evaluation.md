---
layout: default
title: Static Exchange Evaluation
parent: Chess Programming Guide
nav_order: 10
description: "My notes on working out what a capture wins without searching it"
permalink: /docs/static-exchange-evaluation
---

# Static Exchange Evaluation (SEE)

These are my notes on static exchange evaluation and how ChessML implements it. The real reference is the [Chess Programming Wiki](https://www.chessprogramming.org/Static_Exchange_Evaluation).

## The idea

Before searching a capture, it helps to know roughly whether it wins or loses material. SEE answers that by looking at one square only: it lets both sides take turns capturing on it, each time with their cheapest piece that attacks it, and adds up what changes hands. No moves are made on a real board and nothing else on the board is considered, hence "static".

A small example. A white knight takes a pawn on d5, and a black queen defends d5:

1. Nxd5: White wins the pawn, +100.
2. Qxd5: Black wins the knight back. From White's side the exchange is now 100 - 320 = -220.

Black would certainly recapture here, so the SEE of Nxd5 is -220.

The important detail is that every side may stop. After a capture, the opponent recaptures only if that is good for them. So when the sequence is folded back up, each recapture counts as "the better of capturing and not capturing" for the side that makes it. The first capture is different: it is the move being asked about, so it always happens. Clamping the first step to 0 as well would make every capture look at least even.

Two more examples with ChessML's values (pawn 100, knight 320, bishop 330, queen 900):

- Knight takes a bishop that a pawn defends: +330, then the pawn takes the knight, 330 - 320 = +10. The pawn will recapture, so SEE = +10.
- Queen takes a pawn that a pawn defends: +100, then pawn takes queen. Black gains 900 - 100 = 800 from its own view, and wants it, so SEE = -800.

## X-rays

Pieces lined up on the same line attack "through" each other. With a white queen on a1, a white bishop on b2 and a black piece on c3, the queen does not attack c3 at first, but once the bishop has captured there, it does. An implementation that computes every attacker once at the start never sees the queen. ChessML handles this by keeping its own occupancy bitboard: each capture removes the capturing piece from it, and the attackers are recomputed from that occupancy before each step, so sliders behind it become visible.

## What ChessML does

`See.evaluate pos move` in `lib/engine/see.ml` uses the swap-list scheme:

- `gain.(0)` is the value of the first victim.
- `gain.(d)` is the value of the piece now standing on the square (the one about to be captured) minus `gain.(d-1)`. Each entry is from the point of view of the side making that capture.
- After every capture, that attacker's square is cleared from `occupied`, and the next side's attackers are `Movegen.attackers_to pos sq side occupied`, masked with `occupied` so pieces already used are not counted again.
- When nobody can capture any more, the list is folded back from the end, letting each side decline: `gain.(d-1) <- -(max (-gain.(d-1)) gain.(d))`. The answer is `gain.(0)`.

Shortened from `lib/engine/see.ml`:

```ocaml
let rec swap depth side occupied on_square =
  let attackers =
    Int64.logand (Movegen.attackers_to pos to_sq side occupied) occupied
  in
  match least_valuable pos side attackers with
  | None -> depth
  | Some (sq, kind) ->
    let occupied' = Bitboard.clear occupied sq in
    let opponent = Color.opponent side in
    (* A king may only recapture if the square is no longer defended *)
    if kind = King
       && Int64.logand (Movegen.attackers_to pos to_sq opponent occupied') occupied' <> 0L
    then depth
    else (
      let depth = depth + 1 in
      gain.(depth) <- on_square - gain.(depth - 1);   (* promotion bonus left out here *)
      swap depth opponent occupied' (PieceKind.value kind))
in
let depth = swap 0 (Color.opponent mover.color) occupied on_square in
for d = depth downto 1 do
  gain.(d - 1) <- -(max (-gain.(d - 1)) gain.(d))
done;
gain.(0)
```

Some details:

- `least_valuable` tries pawn, knight, bishop, rook, queen, king in that order and takes the first attacker found.
- **En passant**: the victim is the pawn behind the target square, and that square is cleared from the occupancy too.
- **Promotions**: if the first move promotes, or a pawn recaptures on the last rank, the gain includes the difference between the new piece and a pawn, and the piece left on the square is worth the new piece.
- **Pins are ignored.** A pinned piece is counted as an attacker even though it may not really be able to capture. This is the usual trade-off; SEE is meant to be cheap and roughly right.
- It uses only plain material values, never piece-square bonuses.

## Where ChessML uses it

- **Move ordering** (`Search_common.Ordering`): captures with positive SEE are tried first, then equal ones, and losing captures go after the quiet moves. ChessML orders captures by SEE alone; it has no MVV-LVA ordering. See [Move Ordering]({% link docs/move-ordering.md %}).
- **Quiescence search** (`Search_common.tactical_moves`): only captures with SEE >= 0 and promotions are searched, so the search does not wander into exchanges that clearly lose. See [Quiescence Search]({% link docs/quiescence-search.md %}).

## A cheap shortcut (not in ChessML)

If the victim is worth at least as much as the attacker, the capture cannot lose: at worst the opponent recaptures and you stop, ending at victim minus attacker, which is then 0 or more. So victim - attacker is a lower bound on SEE, and engines skip the full calculation when only "is this capture >= 0?" is needed and that bound already answers it. Pawn takes knight never needs the swap list. ChessML always runs the full calculation.

## Pitfalls

- **Stale occupancy.** Computing the attackers once, or not removing each capturer from the occupancy, misses x-ray attackers behind it. ChessML had this problem.
- **Mixing in positional values.** If the gains include piece-square bonuses, equal trades no longer come out as 0 and the attacker order no longer matches the values. ChessML once did this; it now uses plain material.
- **Clamping the first capture.** Only recaptures are optional. Taking the max with 0 at the root makes every losing capture look even.
- **King recaptures.** The king may only take if the square is not defended any more, otherwise the swap list contains an illegal capture.
- **Forgetting en passant.** The captured pawn is not on the target square, so both the victim and the occupancy are wrong if it is treated as a normal capture.

## Sources

- [Chess Programming Wiki: Static Exchange Evaluation](https://www.chessprogramming.org/Static_Exchange_Evaluation)
- [Chess Programming Wiki: SEE - The Swap Algorithm](https://www.chessprogramming.org/SEE_-_The_Swap_Algorithm), the swap-list scheme ChessML follows
- ChessML's code: `lib/engine/see.ml`, `lib/engine/search_common.ml`, `lib/engine/movegen.ml` (`attackers_to`)
