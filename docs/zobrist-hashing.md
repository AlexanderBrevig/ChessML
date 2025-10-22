---
layout: default
title: Zobrist Hashing
parent: Chess Programming Guide
nav_order: 3
description: "Fast incremental position hashing for transposition tables"
permalink: /docs/zobrist-hashing
---

# Zobrist Hashing

## What Is Zobrist Hashing?

Zobrist hashing is a technique for generating unique (almost) 64-bit hash keys for chess positions. These keys are used by transposition tables to quickly identify identical positions reached through different move orders.

**Elo Impact:** Enables transposition tables (~200-300 Elo). Without Zobrist hashing, you can't efficiently implement transposition tables.

## Why It Matters

Chess positions can be reached through different move sequences:

```
1. e4 e5 2. Nf3 Nc6  ← Same position
1. Nf3 Nc6 2. e4 e5  ← Different moves
```

You need a way to:

1. **Quickly** identify if two positions are identical (microseconds)
2. **Uniquely** represent each position with a small key (64 bits)
3. **Incrementally** update the key as moves are made (no full recalculation)

Zobrist hashing does all three!

## The Core Idea

Pre-generate random 64-bit numbers for every possible board component:

- Each piece type on each square (e.g., white pawn on e4)
- Side to move (white or black)
- Castling rights (4 possibilities: KQkq)
- En passant square (8 files)

To compute a position's hash, **XOR together** the random numbers for all components present.

**Key property of XOR:** `A ⊕ A = 0` and `A ⊕ 0 = A`

This means you can **incrementally update** hashes by XORing in changes!

## Random Number Generation

Generate random 64-bit numbers **once at startup**:

```ocaml
(* Random numbers for pieces on squares *)
let piece_keys = Array.make_matrix 64 12 0L

(* Initialize with random numbers *)
let init_piece_keys () =
  Random.init 0x42;  (* Fixed seed for reproducibility *)
  for sq = 0 to 63 do
    for piece = 0 to 11 do  (* 6 piece types × 2 colors *)
      piece_keys.(sq).(piece) <- Random.int64 Int64.max_int
    done
  done

(* Other random values *)
let side_to_move_key = Random.int64 Int64.max_int
let castling_keys = Array.init 16 (fun _ -> Random.int64 Int64.max_int)
let en_passant_keys = Array.init 8 (fun _ -> Random.int64 Int64.max_int)
```

{: .important }
> **Important:** Use a **fixed seed** so the random numbers are the same every time the program runs. This ensures positions hash consistently across sessions.

## Computing Initial Hash

For a starting position, XOR together all components:

```ocaml
let compute_hash pos =
  let hash = ref 0L in

  (* XOR in all pieces *)
  for sq = 0 to 63 do
    match Position.piece_at pos sq with
    | Some piece ->
      let piece_index = piece_to_index piece in
      hash := Int64.logxor !hash piece_keys.(sq).(piece_index)
    | None -> ()
  done;

  (* XOR in side to move *)
  if Position.side_to_move pos = Black then
    hash := Int64.logxor !hash side_to_move_key;

  (* XOR in castling rights *)
  let castling_index = castling_rights_to_index pos in
  hash := Int64.logxor !hash castling_keys.(castling_index);

  (* XOR in en passant *)
  (match Position.en_passant_square pos with
   | Some sq ->
     let file = sq mod 8 in
     hash := Int64.logxor !hash en_passant_keys.(file)
   | None -> ());

  !hash
```

## Incremental Updates

**This is the magic!** When making a move, you don't recompute the entire hash. Instead:

1. **Remove** the piece from its old square: `hash ⊕ piece_key[from][piece]`
2. **Add** the piece to its new square: `hash ⊕ piece_key[to][piece]`
3. **Update** side to move: `hash ⊕ side_to_move_key`
4. **Update** castling/en passant as needed

### Example: Simple Move

```ocaml
(* Moving white pawn from e2 to e4 *)
let update_hash_for_move hash move =
  let from_sq = move.from in
  let to_sq = move.to_ in
  let piece = move.piece in

  (* Remove piece from source square *)
  let piece_idx = piece_to_index piece in
  hash := Int64.logxor !hash piece_keys.(from_sq).(piece_idx);

  (* Add piece to destination square *)
  hash := Int64.logxor !hash piece_keys.(to_sq).(piece_idx);

  (* Toggle side to move *)
  hash := Int64.logxor !hash side_to_move_key;

  !hash
```

### Example: Capture

```ocaml
(* If a capture, also remove captured piece *)
match move.captured with
| Some captured_piece ->
  let captured_idx = piece_to_index captured_piece in
  hash := Int64.logxor !hash piece_keys.(to_sq).(captured_idx)
| None -> ()
```

### Example: Castling

```ocaml
(* Update both king and rook *)
if move.is_castle then begin
  (* Remove king from e1 *)
  hash := Int64.logxor !hash piece_keys.(4).(white_king_idx);
  (* Add king to g1 or c1 *)
  hash := Int64.logxor !hash piece_keys.(move.to_).(white_king_idx);
  (* Remove rook from h1 or a1 *)
  hash := Int64.logxor !hash piece_keys.(rook_from).(white_rook_idx);
  (* Add rook to f1 or d1 *)
  hash := Int64.logxor !hash piece_keys.(rook_to).(white_rook_idx);
end
```

## Castling Rights Encoding

Castling rights have 16 possible states (4 bits: KQkq):

```ocaml
let castling_index rights =
  let index = ref 0 in
  if rights.white_kingside then index := !index lor 1;
  if rights.white_queenside then index := !index lor 2;
  if rights.black_kingside then index := !index lor 4;
  if rights.black_queenside then index := !index lor 8;
  !index
```

When castling rights change:

```ocaml
(* Remove old castling state *)
hash := Int64.logxor !hash castling_keys.(old_index);
(* Add new castling state *)
hash := Int64.logxor !hash castling_keys.(new_index);
```

## En Passant Encoding

Only the **file** matters (not the rank) since en passant squares are always on rank 3 or 6:

```ocaml
(* Clear old en passant *)
match old_ep_square with
| Some sq ->
  let file = sq mod 8 in
  hash := Int64.logxor !hash en_passant_keys.(file)
| None -> ()

(* Set new en passant *)
match new_ep_square with
| Some sq ->
  let file = sq mod 8 in
  hash := Int64.logxor !hash en_passant_keys.(file)
| None -> ()
```

## Piece Encoding

Map pieces to array indices:

```ocaml
let piece_to_index piece =
  let color_offset = if piece.color = White then 0 else 6 in
  let kind_offset = match piece.kind with
    | Pawn -> 0
    | Knight -> 1
    | Bishop -> 2
    | Rook -> 3
    | Queen -> 4
    | King -> 5
  in
  color_offset + kind_offset

(* 0-11: white pieces, black pieces *)
```

## Hash Collisions

With 64-bit hashes and ~10^43 possible positions, collisions are theoretically possible but extremely rare:

**Probability:** ~1 in 10^19 for any two positions

In practice:

- Search might examine ~10^9 positions
- Expected collisions: ~10^-10 (once per billion games)
- **Verify with full position comparison** if critical

Most engines **don't verify** because collisions are so rare they don't affect play.

## Verification

Test that your Zobrist implementation is correct:

```ocaml
let test_zobrist () =
  let pos = Position.from_fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" in

  (* Compute full hash *)
  let hash1 = Zobrist.compute pos in

  (* Make move and update incrementally *)
  let move = Move.make ~from:12 ~to_:28 ~piece:{color=White; kind=Pawn} in
  let pos2 = Position.make_move pos move in
  let hash2 = Position.hash pos2 in

  (* Recompute from scratch *)
  let hash3 = Zobrist.compute pos2 in

  (* Should match! *)
  assert (hash2 = hash3)
```

## Complete Implementation

```ocaml
module Zobrist = struct
  (* Random keys *)
  let piece_keys = Array.make_matrix 64 12 0L
  let side_key = ref 0L
  let castling_keys = Array.make 16 0L
  let ep_keys = Array.make 8 0L

  (* Initialize once at startup *)
  let init () =
    Random.init 0x1234567890ABCDEFL;
    for sq = 0 to 63 do
      for pc = 0 to 11 do
        piece_keys.(sq).(pc) <- Random.int64 Int64.max_int
      done
    done;
    side_key := Random.int64 Int64.max_int;
    for i = 0 to 15 do
      castling_keys.(i) <- Random.int64 Int64.max_int
    done;
    for i = 0 to 7 do
      ep_keys.(i) <- Random.int64 Int64.max_int
    done

  (* Compute hash from scratch *)
  let compute pos =
    let hash = ref 0L in

    (* Pieces *)
    for sq = 0 to 63 do
      match Position.piece_at pos sq with
      | Some p ->
        let idx = piece_to_index p in
        hash := Int64.logxor !hash piece_keys.(sq).(idx)
      | None -> ()
    done;

    (* Side to move *)
    if Position.side_to_move pos = Black then
      hash := Int64.logxor !hash !side_key;

    (* Castling *)
    let castle_idx = castling_index pos in
    hash := Int64.logxor !hash castling_keys.(castle_idx);

    (* En passant *)
    (match Position.en_passant_square pos with
     | Some sq ->
       let file = sq mod 8 in
       hash := Int64.logxor !hash ep_keys.(file)
     | None -> ());

    !hash

  (* Update hash for a piece move *)
  let hash_piece hash square piece =
    let idx = piece_to_index piece in
    Int64.logxor hash piece_keys.(square).(idx)

  (* Toggle side to move *)
  let hash_side hash =
    Int64.logxor hash !side_key

  (* Update castling *)
  let hash_castling hash old_idx new_idx =
    hash
    |> Int64.logxor castling_keys.(old_idx)
    |> Int64.logxor castling_keys.(new_idx)

  (* Update en passant *)
  let hash_ep hash old_sq new_sq =
    let hash = match old_sq with
      | Some sq -> Int64.logxor hash ep_keys.(sq mod 8)
      | None -> hash
    in
    match new_sq with
    | Some sq -> Int64.logxor hash ep_keys.(sq mod 8)
    | None -> hash
end
```

## Common Pitfalls

{: .warning }
> **Common Pitfall:** Not using a fixed seed leads to different hashes each run!

### 1. Not Using Fixed Seed

```ocaml
(* Wrong: Different hashes each run *)
Random.self_init ()

(* Correct: Reproducible hashes *)
Random.init 0x42
```

### 2. Forgetting to Toggle Side

```ocaml
(* Wrong: Same side to move *)
let new_hash = update_pieces old_hash move in

(* Correct: Toggle side *)
let new_hash =
  update_pieces old_hash move
  |> Int64.logxor side_to_move_key
```

### 3. Double-XORing

```ocaml
(* Wrong: XOR piece twice (removes it!) *)
hash := Int64.logxor !hash piece_key;
hash := Int64.logxor !hash piece_key;  (* Now it's gone! *)

(* Correct: XOR once to toggle *)
hash := Int64.logxor !hash piece_key
```

### 4. Wrong Castling Update

```ocaml
(* Wrong: Only remove old *)
hash := Int64.logxor !hash old_castling_key

(* Correct: Remove old AND add new *)
hash := Int64.logxor !hash old_castling_key
hash := Int64.logxor !hash new_castling_key
```

## Performance

Zobrist hashing is **extremely fast**:

- Compute: ~64 XOR operations (~10 ns)
- Update: ~4-6 XOR operations (~1 ns)

This is why it's perfect for transposition tables accessed millions of times per second.

## Alternative: Polyglot Format

The Polyglot opening book format uses a specific Zobrist hashing scheme. If you want your engine to read Polyglot books, implement their exact random key generation:

```ocaml
(* Polyglot uses specific keys from a standard *)
let polyglot_piece_keys = [| ... |]  (* Fixed values *)
```

See [Opening Books](opening-books.md) for details.

## Further Reading

- [Transposition Tables](transposition-tables.md) - Why you need Zobrist hashing
- [Chess Programming Wiki - Zobrist Hashing](https://www.chessprogramming.org/Zobrist_Hashing)
- [Polyglot Book Format](http://hgm.nubati.net/book_format.html)
- `lib/engine/zobrist.ml` - See implementation
