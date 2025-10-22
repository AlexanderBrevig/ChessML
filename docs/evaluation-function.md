---
layout: default
title: Evaluation Function
parent: Chess Programming Guide
nav_order: 11
description: "Assign numerical scores to chess positions"
permalink: /docs/evaluation-function
---

# Evaluation Function

## What Is an Evaluation Function?

The evaluation function assigns a numerical score to a chess position, indicating who is winning and by how much. Scores are in **centipawns** (cp), where 100 = one pawn advantage. Positive scores favor the side to move, negative scores favor the opponent.

**Elo Impact:** ~200-400 Elo for going from material-only to a well-tuned evaluation. Evaluation quality directly determines playing strength when [search](alpha-beta-pruning.md) reaches similar depths.

## Why It Matters

The evaluation function is the "eyes" of your engine. When search reaches leaf nodes (or quiescence), evaluation determines which positions look good. A better evaluation function means:

- Better move selection at equal search depth
- Correct positional judgment (know when to trade, push pawns, etc.)
- Understanding of plans and strategy

**Example:** Material-only evaluation thinks "rook = 500, knight = 300" but misses that a knight on an outpost is worth more than a rook trapped in a corner.

## Core Components

### 1. Material Count

The foundation—count piece values:

```ocaml
let piece_values = [
  (Pawn, 100);
  (Knight, 300);
  (Bishop, 325);  (* Slightly better than knight *)
  (Rook, 500);
  (Queen, 900);
  (King, 0);  (* Can't be captured *)
]

let count_material pos color =
  let total = ref 0 in
  List.iter (fun (kind, value) ->
    let count = popcount (Position.get_pieces pos color kind) in
    total := !total + (count * value)
  ) piece_values;
  !total
```

This gives you the baseline: "I have 2400 centipawns of material, opponent has 2100—I'm ahead by 3 pawns."

### 2. Piece-Square Tables (PST)

Not all squares are equal. A knight in the center is worth more than a knight on the rim. See [bitboards](bitboards.md) for efficient board representation.

```ocaml
(* Knight table: center squares get big bonuses *)
let knight_table = [|
  -50; -40; -30; -30; -30; -30; -40; -50;  (* Rank 1: rim is bad *)
  -40; -20;   0;   0;   0;   0; -20; -40;
  -30;   0;  10;  15;  15;  10;   0; -30;
  -30;   5;  15;  20;  20;  15;   5; -30;  (* Center is great *)
  -30;   0;  15;  20;  20;  15;   0; -30;
  -30;   5;  10;  15;  15;  10;   5; -30;
  -40; -20;   0;   5;   5;   0; -20; -40;
  -50; -40; -30; -30; -30; -30; -40; -50;  (* Rank 8: rim is bad *)
|]

let piece_square_value piece square =
  let table = get_table_for_piece piece in
  let sq_index =
    if piece.color = White then square
    else 63 - square  (* Flip for black *)
  in
  table.(sq_index)
```

**Why flip for black?** Tables are from white's perspective. Black's 8th rank is like white's 1st rank.

### 3. Total Evaluation

```ocaml
let evaluate pos =
  let side = Position.side_to_move pos in
  let opponent = Color.opponent side in

  (* Material *)
  let our_material = count_material pos side in
  let their_material = count_material pos opponent in

  (* Piece-square bonuses *)
  let our_pst = sum_piece_square_bonuses pos side in
  let their_pst = sum_piece_square_bonuses pos opponent in

  (* Total score *)
  (our_material + our_pst) - (their_material + their_pst)
```

This gives you a score from the side-to-move's perspective.

## Advanced Evaluation Terms

### Pawn Structure

**Passed Pawns** (no enemy pawns can stop them):

```ocaml
let passed_pawn_bonus rank =
  (* More valuable as they advance *)
  match rank with
  | 1 -> 0    (* On starting square *)
  | 2 -> 10
  | 3 -> 20
  | 4 -> 40
  | 5 -> 80
  | 6 -> 120  (* Almost promoting! *)
  | 7 -> 200  (* Unstoppable *)
  | _ -> 0

let is_passed_pawn pos color sq =
  let file = sq mod 8 in
  let rank = sq / 8 in
  let opponent = Color.opponent color in

  (* Check if any opponent pawns can stop it *)
  let front_span = if color = White
    then squares_ahead sq
    else squares_behind sq in
  let adjacent_files = [file - 1; file; file + 1] in

  not (exists_enemy_pawn pos opponent front_span adjacent_files)
```

**Doubled Pawns** (two pawns same file, bad):

```ocaml
let doubled_pawn_penalty = -10 in

let count_doubled_pawns pos color =
  let doubled = ref 0 in
  for file = 0 to 7 do
    let pawns_on_file = count_pawns_on_file pos file color in
    if pawns_on_file > 1 then
      doubled := !doubled + (pawns_on_file - 1)
  done;
  !doubled * doubled_pawn_penalty
```

**Isolated Pawns** (no friendly pawns on adjacent files, bad):

```ocaml
let isolated_pawn_penalty = -20 in

let is_isolated_pawn pos color sq =
  let file = sq mod 8 in
  let left_file = file - 1 in
  let right_file = file + 1 in

  (left_file < 0 || not (has_pawn_on_file pos color left_file)) &&
  (right_file > 7 || not (has_pawn_on_file pos color right_file))
```

### King Safety

**Castling Bonus** (castled king is safer):

```ocaml
let castling_bonus pos color =
  let has_castled = Position.has_castled pos color in
  if has_castled then 30 else 0

let can_castle_bonus pos color =
  let rights = Position.castling_rights pos color in
  if rights.kingside || rights.queenside then 10 else 0
```

**Pawn Shield** (pawns in front of king):

```ocaml
let pawn_shield_bonus pos color king_sq =
  let shield_squares = squares_in_front_of_king king_sq color in
  let shield_pawns =
    List.filter (has_pawn_on pos color) shield_squares in
  List.length shield_pawns * 10
```

### Piece Development

**Penalize unmoved pieces in opening:**

```ocaml
let development_penalty pos =
  if Position.fullmove_number pos > 10 then 0  (* Past opening *)
  else
    let penalty = ref 0 in
    (* Check if knights still on back rank *)
    if has_piece_on pos White Knight 1 then penalty := !penalty - 20;
    if has_piece_on pos White Knight 6 then penalty := !penalty - 20;
    (* Check if bishops still on back rank *)
    if has_piece_on pos White Bishop 2 then penalty := !penalty - 20;
    if has_piece_on pos White Bishop 5 then penalty := !penalty - 20;
    !penalty
```

### Mobility

**Count number of legal moves** (more options = better):

```ocaml
let mobility_bonus pos color =
  let move_count = List.length (Movegen.generate_moves pos) in
  move_count * 2  (* 2 centipawns per legal move *)
```

**Note:** This is expensive to calculate, so many engines approximate it or skip it. See [move generation](bitboards.md) for implementation details.

### Trade Incentives

**When ahead, trade pieces (not pawns):**

```ocaml
let trade_bonus pos side =
  let our_material = count_material pos side in
  let their_material = count_material pos (Color.opponent side) in
  let material_diff = our_material - their_material in

  if material_diff > 0 then
    (* Ahead: prefer fewer pieces *)
    let our_pieces = count_non_pawn_pieces pos side in
    let their_pieces = count_non_pawn_pieces pos (Color.opponent side) in
    -10 * (our_pieces - their_pieces)  (* Negative = good when behind *)
  else
    0
```

### Piece-Specific Terms

**Bishop pair bonus:**

```ocaml
let bishop_pair_bonus pos color =
  let bishop_count = popcount (Position.get_pieces pos color Bishop) in
  if bishop_count >= 2 then 50 else 0
```

**Rook on open file:**

```ocaml
let rook_open_file_bonus pos color rook_sq =
  let file = rook_sq mod 8 in
  let has_any_pawn =
    has_pawn_on_file pos White file || has_pawn_on_file pos Black file in
  if not has_any_pawn then 25  (* Open file *)
  else if not (has_pawn_on_file pos color file) then 15  (* Semi-open *)
  else 0
```

## Complete Evaluation

```ocaml
let evaluate pos =
  let side = Position.side_to_move pos in
  let opponent = Color.opponent side in

  (* Material *)
  let material_us = count_material pos side in
  let material_them = count_material pos opponent in
  let material = material_us - material_them in

  (* Piece-square tables *)
  let pst_us = piece_square_bonus pos side in
  let pst_them = piece_square_bonus pos opponent in
  let positional = pst_us - pst_them in

  (* Pawn structure *)
  let pawns_us = evaluate_pawns pos side in
  let pawns_them = evaluate_pawns pos opponent in
  let pawn_structure = pawns_us - pawns_them in

  (* King safety *)
  let king_us = evaluate_king_safety pos side in
  let king_them = evaluate_king_safety pos opponent in
  let king_safety = king_us - king_them in

  (* Mobility (optional - expensive) *)
  (* let mobility = calculate_mobility pos side - calculate_mobility pos opponent in *)

  (* Total *)
  material + positional + pawn_structure + king_safety
```

## Tuning Evaluation

### Start Simple

Begin with material + PST. Add terms one at a time, testing each:

```ocaml
(* Version 1: Material only *)
let eval_v1 pos = count_material pos White - count_material pos Black

(* Version 2: + Piece-square tables *)
let eval_v2 pos = eval_v1 pos + pst_diff pos

(* Version 3: + Pawn structure *)
let eval_v3 pos = eval_v2 pos + pawn_structure_diff pos
```

### Test Each Addition

Run matches to measure Elo gain:

```bash
cutechess-cli \
  -engine name=V1 cmd=engine_v1 \
  -engine name=V2 cmd=engine_v2 \
  -rounds 100 \
  -games 2
```

If V2 wins 55-60%, it's ~+35-70 Elo improvement. Keep it.

### Avoid Over-Tuning

Don't add every possible term. Each term:

- Makes evaluation slower
- Might not help (or hurt!) if poorly calibrated

**Good rule:** Each term should gain 10+ Elo or don't include it.

## Common Pitfalls

### 1. Forgetting Perspective

```ocaml
(* Wrong: Always returns from white's perspective *)
let eval pos =
  count_material pos White - count_material pos Black

(* Correct: Returns from side-to-move perspective *)
let eval pos =
  let side = Position.side_to_move pos in
  let us = count_material pos side in
  let them = count_material pos (Color.opponent side) in
  us - them
```

### 2. Expensive Mobility

```ocaml
(* Wrong: Generates moves twice! *)
let eval pos =
  let our_mobility = List.length (Movegen.generate_moves pos) in
  let new_pos = switch_sides pos in
  let their_mobility = List.length (Movegen.generate_moves new_pos) in
  ...
```

This doubles evaluation time! Either skip mobility or approximate it.

### 3. Ignoring Game Phase

```ocaml
(* Wrong: Same evaluation opening and endgame *)
let eval pos = material + king_safety

(* Correct: Different emphasis by phase *)
let eval pos =
  let phase = game_phase pos in
  if phase = Opening then
    material + development + king_safety
  else if phase = Endgame then
    material + king_activity + pawn_advancement
  else
    material + pst + king_safety
```

## Evaluation Debugging

### Test Positions

```ocaml
(* Should be ~+100 (pawn advantage) *)
let test1 = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPP1/RNBQKBNR w KQkq - 0 1"

(* Should be ~+900 (queen advantage) *)
let test2 = "rnb1kbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

(* Should be ~0 (equal) *)
let test3 = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
```

### Sanity Checks

```ocaml
(* Evaluation should never be crazy *)
assert (abs (evaluate pos) < 50000);  (* Unless it's mate *)

(* Should be symmetric *)
assert (evaluate pos = -(evaluate (flip_position pos)));

(* Material count should match *)
let mat = count_material pos White + count_material pos Black in
assert (mat >= 0 && mat <= 7800);  (* Max material ~78 points *)
```

## Further Reading

- [Piece-Square Tables](https://www.chessprogramming.org/Piece-Square_Tables)
- [Pawn Structure](https://www.chessprogramming.org/Pawn_Structure)
- [King Safety](https://www.chessprogramming.org/King_Safety)
- [Tuning](https://www.chessprogramming.org/Automated_Tuning)
- `lib/engine/eval.ml` - See complete implementation
