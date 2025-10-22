---
layout: default
title: Move Ordering
parent: Chess Programming Guide
nav_order: 9
description: "Try promising moves first to maximize alpha-beta efficiency"
permalink: /docs/move-ordering
---

# Move Ordering

## What Is Move Ordering?

Move ordering is the art of sorting moves before searching them, trying the most promising moves first. This dramatically improves [alpha-beta pruning](alpha-beta-pruning.md) efficiency.

**Elo Impact:** ~100-200 Elo. Good move ordering can effectively double your search depth by causing more beta cutoffs earlier.

## Why It Matters

Alpha-beta pruning works best when you search good moves first:

{: .highlight }

> **Performance Tip:** Good move ordering can reduce nodes by 20x at depth 6!

**Perfect ordering:** Search ~2√N positions instead of N (exponential savings!)
**Random ordering:** Search ~3N/4 positions (still better than minimax, but not much)

Example at depth 6:

- Random ordering: ~1,000,000 nodes
- Good ordering: ~50,000 nodes (20x faster!)
- Perfect ordering: ~10,000 nodes (100x faster - theoretical limit)

## The Ordering Strategy

Most engines use this priority order:

### 1. Hash Move (from Transposition Table)

See [transposition-tables.md](transposition-tables.md).

```ocaml
match tt_entry with
| Some e -> [e.best_move] @ other_moves
| None -> other_moves
```

**Why:** If we've already searched this position deeply, the best move found is very likely still best. This is the single most effective ordering heuristic (~30% cutoff rate).

### 2. Winning Captures (SEE > 0)

See [Static Exchange Evaluation](static-exchange-evaluation.md) for SEE details.

```ocaml
(* Captures that win material *)
let winning_captures =
  captures |> List.filter (fun m -> SEE.evaluate pos m > 0)
  |> List.sort (fun m1 m2 -> compare (mvv_lva m2) (mvv_lva m1))
```

**MVV-LVA** (Most Valuable Victim - Least Valuable Attacker): Prefer "pawn takes queen" over "queen takes pawn".

**Why:** Winning material is usually the best move. Order by victim value since taking valuable pieces is more likely to cause cutoffs.

### 3. Killer Moves

```ocaml
let killers = Killers.get_killers killer_table depth in
```

**Why:** Non-capturing moves that recently caused [beta cutoffs](alpha-beta-pruning.md) at the same depth. These are often strong positional moves (like discovered attacks or forks) that work in similar positions.

Typically store 2 killers per depth:

```ocaml
(* Store when a quiet move causes beta cutoff *)
if is_beta_cutoff && not (is_capture move) then
  Killers.store killer_table depth move
```

### 4. Countermoves

```ocaml
let countermove = Countermoves.get countermove_table prev_move in
```

**Why:** Best response to the opponent's last move. If they played "knight to f3", and "pawn to e5" was a good reply last time, try it first.

```ocaml
(* Store countermove when it causes cutoff *)
if is_beta_cutoff then
  match prev_move with
  | Some pm -> Countermoves.update table pm current_move
  | None -> ()
```

### 5. History Heuristic

```ocaml
let history_score = History.get_score move in
List.sort (fun m1 m2 ->
  compare (History.get_score m2) (History.get_score m1))
```

**Why:** Moves that historically caused beta cutoffs are likely to be good. This captures long-term patterns across the whole search tree.

```ocaml
(* Update history when move causes cutoff *)
if is_beta_cutoff then
  History.record_cutoff move depth

(* Also record failures to learn bad moves *)
if no_cutoff then
  History.record_failure move depth
```

### 6. Equal Captures (SEE = 0)

```ocaml
let equal_captures =
  captures |> List.filter (fun m -> SEE.evaluate pos m = 0)
```

**Why:** Trades (knight takes knight, etc.) are often reasonable but less forcing than winning captures.

### 7. Quiet Moves

```ocaml
let quiet_moves = all_moves |> List.filter (fun m -> not (is_capture m))
```

**Why:** Non-captures ordered by history score.

### 8. Losing Captures (SEE < 0)

```ocaml
let losing_captures =
  captures |> List.filter (fun m -> SEE.evaluate pos m < 0)
  |> List.sort (fun m1 m2 -> compare (SEE.evaluate pos m2) (SEE.evaluate pos m1))
```

**Why:** Last resort. "Queen takes pawn protected by pawn" loses material. Try these last since they rarely cause cutoffs. Still order by least-bad first.

## Key Techniques

### Static Exchange Evaluation (SEE)

SEE calculates the material outcome of a capture sequence:

```ocaml
(* Example: Knight takes bishop on e4, defended by pawn *)
let see_score = SEE.evaluate pos (Knight, e4) in
(* Returns: 300 (bishop) - 300 (knight) = 0 (equal trade) *)
```

See [Static Exchange Evaluation](static-exchange-evaluation.md) for details.

### MVV-LVA

**Most Valuable Victim, Least Valuable Attacker:**

```ocaml
let mvv_lva_score move =
  let victim_value = piece_value (captured_piece move) in
  let attacker_value = piece_value (moving_piece move) in
  (* Multiply victim by large number to prioritize *)
  victim_value * 1000 - attacker_value
```

Examples:

- Pawn × Queen: 900 × 1000 - 100 = 899,900 (best)
- Knight × Queen: 900 × 1000 - 300 = 899,700
- Queen × Pawn: 100 × 1000 - 900 = 99,100 (worst)

### Killer Move Heuristic

Store moves that caused beta cutoffs:

```ocaml
type killer_table = move option array array  (* [depth][slot] *)

let store killers depth move =
  (* Keep 2 killers per depth *)
  if killers.(depth).(0) <> Some move then begin
    killers.(depth).(1) <- killers.(depth).(0);
    killers.(depth).(0) <- Some move
  end
```

Only store **quiet moves** (non-captures) since captures are already ordered well.

### History Heuristic

Maintain global statistics for all moves:

```ocaml
type history_table = int array array  (* [from_square][to_square] *)

let record_cutoff history move depth =
  let bonus = depth * depth in  (* Deeper searches matter more *)
  let from, to_ = move.from, move.to_ in
  history.(from).(to_) <- history.(from).(to_) + bonus

let record_failure history move depth =
  let penalty = depth * depth in
  let from, to_ = move.from, move.to_ in
  history.(from).(to_) <- max 0 (history.(from).(to_) - penalty)
```

{: .important }

> Cap history scores to prevent overflow and allow the engine to adapt to new positions!

**Important:** Cap history scores to prevent overflow and allow adaptation:

```ocaml
let max_history_score = 10000 in

let record_cutoff history move depth =
  let bonus = depth * depth in
  let new_score = history.(from).(to_) + bonus in
  history.(from).(to_) <- min max_history_score new_score
```

### Countermove Heuristic

Store best refutation for each move:

```ocaml
type countermove_table = move option array array  (* [from][to] *)

let update_countermove table prev_move refutation =
  let from, to_ = prev_move.from, prev_move.to_ in
  table.(from).(to_) <- Some refutation

let get_countermove table prev_move =
  let from, to_ = prev_move.from, prev_move.to_ in
  table.(from).(to_)
```

## Implementation Example

```ocaml
let order_moves pos moves tt_move killers prev_move =
  (* Separate captures and quiets *)
  let captures, quiets =
    List.partition is_capture moves in

  (* Score captures with SEE *)
  let scored_captures =
    captures |> List.map (fun m ->
      (SEE.evaluate pos m, mvv_lva m, m)) in
  let winning_caps =
    scored_captures |> List.filter (fun (see, _, _) -> see > 0)
    |> List.sort (fun (s1, mvv1, _) (s2, mvv2, _) ->
        compare (s2, mvv2) (s1, mvv1))
    |> List.map (fun (_, _, m) -> m) in
  let equal_caps =
    scored_captures |> List.filter (fun (see, _, _) -> see = 0)
    |> List.map (fun (_, _, m) -> m) in
  let losing_caps =
    scored_captures |> List.filter (fun (see, _, _) -> see < 0)
    |> List.sort (fun (s1, _, _) (s2, _, _) -> compare s2 s1)
    |> List.map (fun (_, _, m) -> m) in

  (* Score quiet moves *)
  let is_killer m = List.mem m killers in
  let is_countermove m =
    match prev_move with
    | Some pm -> Countermoves.is_countermove table pm m
    | None -> false in

  let killer_moves = quiets |> List.filter is_killer in
  let counter_moves = quiets |> List.filter is_countermove in
  let other_quiets =
    quiets
    |> List.filter (fun m -> not (is_killer m || is_countermove m))
    |> List.sort (fun m1 m2 ->
        compare (History.get_score m2) (History.get_score m1)) in

  (* Final order *)
  List.concat [
    Option.to_list tt_move;
    winning_caps;
    killer_moves;
    counter_moves;
    other_quiets;
    equal_caps;
    losing_caps;
  ]
```

## Measuring Effectiveness

Track move ordering statistics:

```ocaml
type ordering_stats = {
  beta_cutoffs: int;            (* Total cutoffs *)
  first_move_cutoffs: int;      (* Cutoff on first move tried *)
  hash_move_cutoffs: int;       (* Cutoff on hash move *)
  capture_cutoffs: int;         (* Cutoff on capture *)
  killer_cutoffs: int;          (* Cutoff on killer *)
}

let first_move_cutoff_rate stats =
  float stats.first_move_cutoffs /. float stats.beta_cutoffs
```

Good move ordering:

- **First move cutoff:** 60-70% (try best move first that often)
- **Hash move cutoff:** 30-40% when hash move exists
- **Killer cutoff:** 10-15% of quiet moves

## Common Pitfalls

### 1. Over-Complicating

Start simple:

1. Hash move
2. MVV-LVA captures
3. Killers
4. Other moves

Add history/countermoves/SEE once basics work.

### 2. Sorting Too Much

Don't sort the entire move list—use partial sorting:

```ocaml
(* Bad: Full sort *)
let sorted = List.sort compare_moves all_moves in

(* Good: Pick best move repeatedly *)
let rec search_best_first moves =
  match find_and_remove_best moves with
  | None -> ()
  | Some (best, rest) ->
    search_move best;
    search_best_first rest
```

### 3. Not Updating Heuristics

Killer and history tables must be updated during search:

```ocaml
(* After trying a move *)
if score >= beta then begin
  if not (is_capture move) then
    Killers.store killer_table depth move;
  History.record_cutoff move depth;
  Option.iter (fun pm ->
    Countermoves.update countermove_table pm move) prev_move
end else begin
  History.record_failure move depth
end
```

### 4. Forgetting to Exclude

Don't try the hash move twice:

```ocaml
let other_moves =
  all_moves |> List.filter (fun m ->
    not (Move.equal m hash_move))
```

## Advanced: Captures First?

Some engines do **captures in [quiescence](quiescence-search.md) only** and order all quiet moves by history in main search. This works because:

1. Winning captures are handled in quiescence
2. Losing captures are usually bad
3. Equal captures might not be best move

But traditionally, trying winning captures first in main search still works well.

## Further Reading

- [Static Exchange Evaluation](static-exchange-evaluation.md) - Evaluate capture sequences
- [Alpha-Beta Pruning](alpha-beta-pruning.md) - Why move ordering matters
- [Killer Moves](https://www.chessprogramming.org/Killer_Heuristic)
- [History Heuristic](https://www.chessprogramming.org/History_Heuristic)
- `lib/engine/history.ml`, `killers.ml`, `countermoves.ml`
