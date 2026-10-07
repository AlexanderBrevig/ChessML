---
layout: default
title: Magic Bitboards
parent: Chess Programming Guide
nav_order: 2
description: "My notes on looking up rook and bishop attacks with magic multiplication"
permalink: /docs/magic-bitboards
---

# Magic Bitboards

These are my notes on how ChessML finds the squares a rook, bishop or queen attacks. They describe how I understand the trick and what ChessML does; the real reference is the [Chess Programming Wiki](https://www.chessprogramming.org/Magic_Bitboards).

## The problem

A knight on a given square always attacks the same squares, so a 64-entry table is enough (see [Bitboards]({% link docs/bitboards.md %})). A rook is different: it slides until it hits something, so its attacks depend on what else is on the board.

Here is a rook on e4 with other pieces on c4, g4, e6 and e2 (`R` is the rook, `x` a piece in the way, `*` an empty square the rook reaches):

```
 8 | .  .  .  .  .  .  .  .
 7 | .  .  .  .  .  .  .  .
 6 | .  .  .  .  x  .  .  .
 5 | .  .  .  .  *  .  .  .
 4 | .  .  x  *  R  *  x  .
 3 | .  .  .  .  *  .  .  .
 2 | .  .  .  .  x  .  .  .
 1 | .  .  .  .  .  .  .  .
     a  b  c  d  e  f  g  h
```

The rook attacks the four `*` squares and also the four `x` squares: if the piece there is an enemy, it can be captured (if it is a friend, it is defended). Everything behind them (e7, e8, b4, a4, h4, e1) is out of reach.

The obvious way to compute this is to walk each of the four directions square by square and stop at the first piece. That works, and ChessML does exactly that, but only once, at startup, to fill a table. Magic bitboards are a way to turn "rook on e4, this occupancy" into an index into that table.

## Only some squares matter

For a rook on e4, only the pieces on the same file and rank can change its attacks, and not even all of those. A piece on e8 does not matter: the rook reaches e8 whether it is empty or occupied, and there is nothing behind it. The same holds for the other edge squares. So the squares that matter (the "relevant occupancy mask", `m` below) are e2 to e7 and b4 to g4, without e4 itself:

```
 8 | .  .  .  .  .  .  .  .
 7 | .  .  .  .  m  .  .  .
 6 | .  .  .  .  m  .  .  .
 5 | .  .  .  .  m  .  .  .
 4 | .  m  m  m  R  m  m  .
 3 | .  .  .  .  m  .  .  .
 2 | .  .  .  .  m  .  .  .
 1 | .  .  .  .  .  .  .  .
     a  b  c  d  e  f  g  h
```

That is 10 squares, so 2^10 = 1024 possible arrangements of pieces on them. A rook has 10 to 12 relevant squares depending on where it stands (12 in the corners), and a bishop 5 to 9, so a bishop needs between 32 and 512 entries per square.

## The magic part

AND the occupancy with the mask and you have a 64-bit number with at most 12 bits set, scattered over the board. To use it as an array index, those bits need to be squeezed together into a small number. The trick is to multiply by a carefully chosen 64-bit constant (the magic) and keep the top N bits of the product, where N is the number of relevant squares:

```
index = ((occupancy AND mask) * magic) >> (64 - N)
```

The multiplication shifts copies of each relevant bit to many places; a good magic is one where the top N bits end up different for any two arrangements that give different attacks. Two arrangements may share an index if they produce the same attack set (a piece behind a blocker, for example); that collision is harmless, and the Wiki calls it constructive. Nobody computes magics in closed form as far as I know: you try random candidates until one has no harmful collision for all arrangements.

## What ChessML does

ChessML does not search for magics. `lib/engine/magic.ml` has 64 hard-coded rook magics and 64 bishop magics, and builds everything else from them. For each square it stores a small record:

```ocaml
type magic_entry =
  { mask : Int64.t (* relevant occupancy bits (no edges) *)
  ; magic : Int64.t
  ; shift : int (* 64 - number of relevant bits *)
  ; offset : int (* where this square's slice starts in the shared table *)
  }
```

The lookup is the formula above plus the offset (from `magic.ml`):

```ocaml
let magic_index (entry : magic_entry) (blockers : Int64.t) : int =
  let relevant = Int64.logand blockers entry.mask in
  let hash = Int64.mul relevant entry.magic in
  let index = Int64.to_int (Int64.shift_right_logical hash entry.shift) in
  entry.offset + index
;;

let rook_attacks (sq : int) (blockers : Int64.t) : Int64.t =
  let idx = magic_index rook_magics.(sq) blockers in
  rook_attacks_table.(idx)
;;
```

Queen attacks are the rook and bishop attacks ORed together.

`Magic.init` fills the tables. For every square it computes the mask, enumerates every arrangement of pieces on it, works out the attacks with the slow direction walk and stores them at the magic index. You do not call it yourself: `lib/engine/movegen.ml` runs `let () = Magic.init ()` when the module is initialised, and everything else asks `Movegen` for attacks.

### Table layout

There are two common ways to lay out the tables:

- **Plain**: every square gets room for the worst case, 4096 entries for rooks and 512 for bishops, with the same shift everywhere. Simple, but 64 × (4096 + 512) entries of 8 bytes is about 2.3 MB.
- **Fancy**: each square gets exactly 2^N entries for its own N, and all squares share one packed array, with an offset saying where each square's slice starts. That needs 102400 rook entries and 5248 bishop entries, 107648 in total (about 841 KB). As far as I can tell from its source, Stockfish uses this layout, or the BMI2 `pext` instruction where the CPU has it.

ChessML uses the fancy layout: `rook_attacks_table` has 102400 entries and `bishop_attacks_table` 5248, and `init` advances the offset by `1 lsl bits` after each square.

### Finding magics yourself

If you want to generate your own, the usual recipe is: pick a random 64-bit number with few bits set (ANDing three random numbers together is the common way), and check it against every arrangement of the square. People report that this finds a full set in seconds. A simplified sketch of the check, not ChessML code:

```ocaml
(* simplified sketch: does [magic] index every arrangement without a harmful collision? *)
let magic_works ~bits ~configs ~attacks magic =
  let table = Array.make (1 lsl bits) None in
  let ok = ref true in
  Array.iteri
    (fun i occ ->
       let idx =
         Int64.to_int (Int64.shift_right_logical (Int64.mul occ magic) (64 - bits))
       in
       match table.(idx) with
       | None -> table.(idx) <- Some attacks.(i)
       | Some a -> if not (Int64.equal a attacks.(i)) then ok := false)
    configs;
  !ok
```

The table has `1 lsl bits` entries, not `1 lsl (64 - bits)`; mixing up the two allocates an absurd array.

## Pitfalls

- **Arithmetic instead of logical shift.** In OCaml, `Int64.shift_right` keeps the sign bit, so a product with the top bit set gives a negative index. Use `Int64.shift_right_logical`.
- **Forgetting the mask.** The magic only works on the relevant squares; multiplying the full occupancy gives garbage indices.
- **Edge squares in the mask.** Including them doubles or quadruples the arrangements, and published magics will not fit the larger index.
- **Magics from somewhere else.** A magic only works with the same square numbering (a1 = 0 or a8 = 0), the same masks and the same shift as where it came from.
- **No check after filling.** If a magic is wrong, `init` silently overwrites entries and some attacks are wrong in rare positions. ChessML's `init` does not check for collisions either; the [perft tests](https://github.com/AlexanderBrevig/ChessML/blob/main/test/engine/test_perft.ml) are what would catch a bad magic.

## Sources

- [Chess Programming Wiki: Magic Bitboards](https://www.chessprogramming.org/Magic_Bitboards), for the idea, the plain and fancy names and the table sizes
- [Chess Programming Wiki: Looking for Magics](https://www.chessprogramming.org/Looking_for_Magics), for how magics are found
- ChessML's code: [`lib/engine/magic.ml`](https://github.com/AlexanderBrevig/ChessML/blob/main/lib/engine/magic.ml), [`lib/engine/movegen.ml`](https://github.com/AlexanderBrevig/ChessML/blob/main/lib/engine/movegen.ml)
