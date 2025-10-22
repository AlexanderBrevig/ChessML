---
layout: default
title: Static Exchange Evaluation
parent: Chess Programming Guide
nav_order: 10
description: "Calculate material outcome of capture sequences"
permalink: /docs/static-exchange-evaluation
---

# Static Exchange Evaluation (SEE)

## What Is Static Exchange Evaluation?

Static Exchange Evaluation (SEE) calculates the material outcome of a capture sequence on a square. It simulates all captures and recaptures to determine if a capture wins, loses, or trades equally—without actually searching.

**Elo Impact:** ~30-80 Elo. Essential for move ordering and quiescence search pruning. Helps avoid searching obviously bad captures.

## Why It Matters

Not all captures are good:

```
Position: Knight on e4, Queen on e4, Pawn on d5

If you play Nxd5:
- You capture pawn (+100)
- Opponent captures Qxd5 (-300)
- Net: -200 centipawns (bad trade!)

SEE tells you this capture loses material.
```

Without SEE:

- Try all captures in quiescence
- Search bad captures deeply
- Waste time on losing captures

With SEE:

- Skip bad captures (SEE < 0)
- Order winning captures first (SEE > 0)
- Faster search, better move ordering

## How It Works

Simulate the capture sequence:

1. Make initial capture
2. Find least valuable attacker who can recapture
3. Simulate that recapture
4. Repeat until no more captures
5. Return material balance

### Key Insight

Use **least valuable attacker first** (you don't sacrifice your queen to recapture a pawn if a pawn can do it).

## Basic Algorithm

```ocaml
let rec see pos square attacker_color =
  (* Find least valuable attacker *)
  match find_least_valuable_attacker pos square attacker_color with
  | None -> 0  (* No attacker, capture sequence ends *)
  | Some (attacker_square, attacker_piece) ->
    (* Value of piece we're capturing *)
    let captured_value =
      match piece_at pos square with
      | Some p -> piece_value p
      | None -> 0
    in

    (* Make the capture *)
    let new_pos = make_capture pos attacker_square square in

    (* Recursively evaluate opponent's recapture *)
    let opponent = Color.opponent attacker_color in
    let opponent_gain = see new_pos square opponent in

    (* Our gain: capture value minus opponent's best response *)
    max 0 (captured_value - opponent_gain)
```

### Why Max 0?

You can always choose **not to recapture** if it loses material:

```
Knight takes pawn (guarded by queen):
- Capture pawn: +100
- Queen recaptures: -300
- Don't recapture! Just take the pawn and run

Net: +100 (not -200)
```

## Finding Attackers

Identify all pieces that can attack a square:

```ocaml
let find_attackers pos square color =
  let attackers = ref Bitboard.empty in

  (* Pawns *)
  let pawn_attacks = pawn_attacks_to_square square (Color.opponent color) in
  attackers := !attackers ||| (pawn_attacks &&& get_pawns pos color);

  (* Knights *)
  let knight_attacks = knight_attacks_pattern square in
  attackers := !attackers ||| (knight_attacks &&& get_knights pos color);

  (* Bishops and queens (diagonal) *)
  let diag_attacks = bishop_attacks square (occupied pos) in
  let diag_pieces = get_bishops pos color ||| get_queens pos color in
  attackers := !attackers ||| (diag_attacks &&& diag_pieces);

  (* Rooks and queens (straight) *)
  let straight_attacks = rook_attacks square (occupied pos) in
  let straight_pieces = get_rooks pos color ||| get_queens pos color in
  attackers := !attackers ||| (straight_attacks &&& straight_pieces);

  (* Kings *)
  let king_attacks = king_attacks_pattern square in
  attackers := !attackers ||| (king_attacks &&& get_king pos color);

  !attackers
```

### Least Valuable Attacker

```ocaml
let find_least_valuable_attacker pos square color =
  let attackers = find_attackers pos square color in

  (* Check pieces in value order *)
  let check_piece kind bb =
    let pieces = bb &&& attackers in
    if Bitboard.is_not_empty pieces then
      let sq = Bitboard.lsb pieces in
      Some (sq, { kind; color })
    else None
  in

  match
    check_piece Pawn (get_pawns pos color)
  with Some x -> Some x | None ->
  match
    check_piece Knight (get_knights pos color)
  with Some x -> Some x | None ->
  match
    check_piece Bishop (get_bishops pos color)
  with Some x -> Some x | None ->
  match
    check_piece Rook (get_rooks pos color)
  with Some x -> Some x | None ->
  match
    check_piece Queen (get_queens pos color)
  with Some x -> Some x | None ->
  check_piece King (get_king pos color)
```

## Optimized Algorithm

The recursive version is clear but slow. Most engines use an iterative approach:

```ocaml
let see_optimized pos move =
  let target_square = move.to_ in
  let moving_piece = move.piece in

  (* Initialize with captured piece value *)
  let gain = Array.make 32 0 in
  gain.(0) <-
    match move.captured with
    | Some p -> piece_value p
    | None -> 0;

  (* Track material balance alternating sides *)
  let depth = ref 1 in
  let attacker_color = ref (Color.opponent moving_piece.color) in
  let attacker_value = ref (piece_value moving_piece) in

  (* Simulate captures until no more attackers *)
  while !depth < 32 do
    (* Gain for this ply *)
    gain.(!depth) <- !attacker_value - gain.(!depth - 1);

    (* Find next attacker *)
    match find_least_valuable_attacker pos target_square !attacker_color with
    | None -> break  (* No more attackers *)
    | Some (_, piece) ->
      attacker_value := piece_value piece;
      attacker_color := Color.opponent !attacker_color;
      depth := !depth + 1
  done;

  (* Minimax evaluation of the gain array *)
  let rec minimax d =
    if d = !depth then gain.(d)
    else max gain.(d) (-minimax (d + 1))
  in

  if !depth = 1 then gain.(0)  (* Only initial capture *)
  else minimax 1
```

## X-Ray Attacks

After a piece moves, it might reveal attackers behind it:

```
Initial: Rook-a1, Bishop-b2, target-c3
After Bishop captures c3: Rook can now attack c3!
```

Handle this by updating the occupancy bitboard:

```ocaml
let occupied = ref (Position.occupied pos) in

(* Remove piece that just moved *)
occupied := Bitboard.clear !occupied attacker_square;

(* Add piece to target square (if recapturing) *)
occupied := Bitboard.set !occupied target_square;

(* Recalculate sliding attacks with new occupancy *)
let new_attackers = find_attackers_with_occupancy pos target_square color !occupied in
```

## Usage in Move Ordering

### Filter Bad Captures

```ocaml
let good_captures moves =
  moves
  |> List.filter is_capture
  |> List.filter (fun m -> SEE.evaluate pos m >= 0)
```

### Order by SEE Score

```ocaml
let order_captures moves =
  moves
  |> List.filter is_capture
  |> List.map (fun m -> (SEE.evaluate pos m, m))
  |> List.sort (fun (s1, _) (s2, _) -> compare s2 s1)  (* High to low *)
  |> List.map snd
```

## Usage in Quiescence Search

Skip losing captures:

```ocaml
let quiescence pos alpha beta =
  let captures = generate_captures pos in

  (* Only consider non-losing captures *)
  let good_captures =
    captures |> List.filter (fun m -> SEE.evaluate pos m >= 0) in

  search_moves good_captures alpha beta
```

This dramatically reduces quiescence search size!

## Example Calculations

### Simple Capture

```
White pawn takes black knight (undefended):
- Gain: +300 (knight value)
- No recapture possible
SEE = +300
```

### Equal Trade

```
White knight takes black bishop (defended by pawn):
- Capture: +325 (bishop)
- Recapture: -300 (knight)
- Net: +25
- But opponent won't recapture (loses pawn)
SEE = +325 (opponent stops sequence)
```

### Losing Capture

```
White queen takes black pawn (defended by pawn):
- Capture: +100 (pawn)
- Recapture: -900 (queen)
- Net: -800
SEE = +100 (we stop after taking pawn, don't recapture)

Actually: SEE returns +100 because we can take pawn and leave.
But from opponent's view, they get queen for pawn: +800 for them.

So move is bad even though SEE > 0!

Better formulation: Always simulate from initial mover's view:
- We capture pawn: +100
- They capture queen: +900 for them = -900 for us
- Net: +100 - 900 = -800
SEE = -800 (correctly identifies bad capture)
```

**Correct SEE accounts for forced recaptures!**

## Performance Tips

### 1. Early Exit

```ocaml
(* If capture wins material without recapture, done *)
if captured_value > attacker_value && no_defenders then
  return captured_value
```

### 2. Lazy Evaluation

```ocaml
(* Only compute if needed for move ordering *)
let see_lazy pos move =
  match move.captured with
  | None -> 0  (* Not a capture *)
  | Some victim when piece_value victim < piece_value move.piece ->
    (* Obviously bad: queen takes pawn *)
    let defenders = count_defenders pos move.to_ in
    if defenders > 0 then -piece_value move.piece else piece_value victim
  | Some victim ->
    (* Might be good, compute full SEE *)
    see_full pos move
```

### 3. Cache Results

```ocaml
(* Store SEE scores in move structure *)
type move = {
  from: square;
  to_: square;
  piece: piece;
  captured: piece option;
  mutable see_score: int option;  (* Cached *)
}
```

## Common Pitfalls

### 1. Forgetting X-Ray Attacks

```ocaml
(* Wrong: Don't update occupancy *)
let attackers = find_attackers pos square color in

(* Correct: Update after each capture *)
let attackers = find_attackers_with_occupancy pos square color occupancy in
```

### 2. Not Using Least Valuable Attacker

```ocaml
(* Wrong: Use any attacker *)
let attacker = first_attacker pos square color in

(* Correct: Use cheapest attacker *)
let attacker = least_valuable_attacker pos square color in
```

### 3. Forgetting King Can Attack

```ocaml
(* Wrong: Skip king *)
check_piece Pawn || check_piece Knight || ... || check_piece Queen

(* Correct: Include king *)
... || check_piece Queen || check_piece King
```

King can capture, especially in endgames!

### 4. Wrong Stopping Condition

```ocaml
(* Wrong: Always recurse *)
let rec see pos sq color =
  captured_value - see pos sq (opponent color)

(* Correct: Stop if no attackers *)
match find_attacker pos sq color with
| None -> 0
| Some attacker -> captured_value - see new_pos sq (opponent color)
```

## Integration with Search

SEE complements other techniques:

| Technique  | Uses SEE? | Purpose                     |
| ---------- | --------- | --------------------------- |
| MVV-LVA    | No        | Quick capture ordering      |
| SEE        | Yes       | Accurate capture evaluation |
| Quiescence | Yes       | Prune bad captures          |
| History    | No        | Quiet move ordering         |
| Killers    | No        | Non-capture refutations     |

Use MVV-LVA for quick approximation, SEE for accuracy when needed.

## Measuring Effectiveness

Track SEE statistics:

```ocaml
type see_stats = {
  captures_evaluated: int;
  winning_captures: int;  (* SEE > 0 *)
  equal_captures: int;    (* SEE = 0 *)
  losing_captures: int;   (* SEE < 0 *)
  pruned: int;           (* Moves skipped due to SEE *)
}
```

Good SEE usage:

- **Prune rate:** 30-50% of captures have negative SEE
- **Quiescence speedup:** 2-3x faster with SEE pruning
- **Accuracy:** >95% correct about winning/losing captures

## Further Reading

- [Move Ordering](move-ordering.md) - Uses SEE for capture ordering
- [Quiescence Search](quiescence-search.md) - Uses SEE to prune bad captures
- [Chess Programming Wiki - SEE](https://www.chessprogramming.org/Static_Exchange_Evaluation)
- `lib/engine/see.ml` - See implementation
