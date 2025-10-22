---
layout: default
title: Opening Books
parent: Chess Programming Guide
nav_order: 12
description: "Pre-computed opening theory for better play"
permalink: /docs/opening-books
---

# Opening Books

## What Is an Opening Book?

An opening book is a database of pre-analyzed opening positions and moves, allowing your engine to play established opening theory instead of spending time searching obvious early moves. Books are typically in Polyglot format—a binary file containing position hashes and move statistics.

**Elo Impact:** ~50-150 Elo depending on book quality. More importantly, it prevents your engine from playing weak/unusual openings and saves thinking time for the middlegame.

## Why They Matter

### Save Time

In the opening, many moves are well-known:

```
1. e4 e5 2. Nf3 Nc6 3. Bb5  ← Italian Game, played millions of times
```

Searching depth 6 might take 5 seconds per move, but the book lookup takes 0.001 seconds. Those saved seconds matter later in the game!

### Play Known Theory

Without a book, your engine might:

- Play dubious openings (1. h3?)
- Get into refuted lines
- Reach objectively worse positions

With a book:

- Plays mainline openings
- Follows grandmaster games
- Reaches balanced middlegames

### Variety

A good book contains multiple moves per position, adding variety:

```
Position: 1. e4 e5 2. Nf3
Book: Nc6 (50%), Nf6 (30%), d6 (15%), Bc5 (5%)
```

Each game takes a different path, making your engine less predictable.

## Polyglot Book Format

The standard format for chess opening books:

### File Structure

```
Binary file with 16-byte entries:
[8 bytes: position hash]
[2 bytes: move]
[2 bytes: weight]
[4 bytes: learn data]

Entries are sorted by hash for binary search.
```

### Position Hash

Uses **Polyglot Zobrist hashing**—a specific hashing scheme:

```ocaml
(* Polyglot uses specific random numbers *)
let polyglot_hash pos =
  let hash = ref 0L in

  (* XOR in pieces *)
  for sq = 0 to 63 do
    match piece_at pos sq with
    | Some piece ->
      let idx = polyglot_piece_index piece in
      hash := Int64.logxor !hash polyglot_random.(64 * idx + sq)
    | None -> ()
  done;

  (* XOR in castling rights *)
  let castling = polyglot_castling_index pos in
  hash := Int64.logxor !hash polyglot_castling_random.(castling);

  (* XOR in en passant *)
  (match en_passant_square pos with
   | Some sq ->
     let file = sq mod 8 in
     hash := Int64.logxor !hash polyglot_ep_random.(file)
   | None -> ());

  (* XOR in side to move (if black) *)
  if side_to_move pos = Black then
    hash := Int64.logxor !hash polyglot_side_random;

  !hash
```

**Important:** Polyglot hashing differs from your internal Zobrist hashing. You need to implement both!

### Move Encoding

Moves are encoded in 16 bits:

```
Bits 0-5:   From square (0-63)
Bits 6-11:  To square (0-63)
Bits 12-14: Promotion piece (0=none, 1=knight, 2=bishop, 3=rook, 4=queen)
Bit 15:     Unused
```

```ocaml
let encode_move move =
  let from = move.from in
  let to_ = move.to_ in
  let promo = match move.promotion with
    | None -> 0
    | Some Knight -> 1
    | Some Bishop -> 2
    | Some Rook -> 3
    | Some Queen -> 4
    | _ -> 0
  in
  from lor (to_ lsl 6) lor (promo lsl 12)

let decode_move encoded pos =
  let from = encoded land 0x3F in
  let to_ = (encoded lsr 6) land 0x3F in
  let promo_code = (encoded lsr 12) land 0x7 in
  let promotion = match promo_code with
    | 1 -> Some Knight
    | 2 -> Some Bishop
    | 3 -> Some Rook
    | 4 -> Some Queen
    | _ -> None
  in
  (* Build move from squares and promotion *)
  make_move pos from to_ promotion
```

### Weight

A 16-bit unsigned integer indicating how often the move appears in the book's source games. Higher weight = more popular.

## Reading a Polyglot Book

### Binary Search

Entries are sorted by hash, allowing binary search:

```ocaml
let probe_book book pos =
  let hash = polyglot_hash pos in
  let entries = ref [] in

  (* Binary search for first matching entry *)
  let rec find_first left right =
    if left >= right then left
    else
      let mid = (left + right) / 2 in
      seek_entry book mid;
      let entry_hash = read_int64 book in
      if entry_hash < hash then find_first (mid + 1) right
      else find_first left mid
  in

  let start_idx = find_first 0 (book_size / 16) in

  (* Read all entries with matching hash *)
  seek_entry book start_idx;
  while true do
    let entry_hash = read_int64 book in
    if entry_hash <> hash then break;

    let move_encoded = read_uint16 book in
    let weight = read_uint16 book in
    let _learn = read_uint32 book in

    let move = decode_move move_encoded pos in
    entries := (move, weight) :: !entries
  done;

  !entries
```

### Move Selection

Two common strategies:

**1. Best move (highest weight):**

```ocaml
let get_best_book_move book pos =
  let entries = probe_book book pos in
  match entries with
  | [] -> None
  | _ ->
    let best = List.fold_left (fun (mv1, w1) (mv2, w2) ->
      if w2 > w1 then (mv2, w2) else (mv1, w1)
    ) (List.hd entries) (List.tl entries) in
    Some (fst best)
```

**2. Weighted random:**

```ocaml
let get_random_book_move book pos =
  let entries = probe_book book pos in
  match entries with
  | [] -> None
  | _ ->
    let total_weight = List.fold_left (fun sum (_, w) -> sum + w) 0 entries in
    let rand = Random.int total_weight in

    let rec select_weighted entries acc =
      match entries with
      | [] -> None
      | (mv, w) :: rest ->
        let new_acc = acc + w in
        if rand < new_acc then Some mv
        else select_weighted rest new_acc
    in
    select_weighted entries 0
```

Weighted random adds variety—your engine won't always play the same opening!

## Creating Your Own Book

### From PGN Files

1. **Parse games:**

```ocaml
let process_pgn_file filename =
  let games = PGN_parser.parse filename in
  let book_entries = Hashtbl.create 1_000_000 in

  List.iter (fun game ->
    let pos = ref (Position.initial ()) in
    List.iter (fun move ->
      let hash = polyglot_hash !pos in
      let entry = (hash, move) in

      (* Increment weight for this position-move *)
      let weight =
        match Hashtbl.find_opt book_entries entry with
        | Some w -> w + 1
        | None -> 1
      in
      Hashtbl.replace book_entries entry weight;

      pos := Position.make_move !pos move
    ) game.moves
  ) games;

  book_entries
```

2. **Filter by occurrence:**

```ocaml
(* Only keep moves that appear >= 3 times *)
let filtered = Hashtbl.filter (fun _ weight -> weight >= 3) book_entries
```

3. **Write binary file:**

```ocaml
let write_book filename entries =
  let sorted =
    Hashtbl.to_seq entries
    |> List.of_seq
    |> List.sort (fun ((h1, _), _) ((h2, _), _) -> compare h1 h2)
  in

  let oc = open_out_bin filename in
  List.iter (fun ((hash, move), weight) ->
    write_int64 oc hash;
    write_uint16 oc (encode_move move);
    write_uint16 oc weight;
    write_uint32 oc 0  (* Learn data *)
  ) sorted;
  close_out oc
```

### Book Statistics

Track quality metrics:

```ocaml
type book_stats = {
  num_positions: int;
  num_moves: int;
  avg_moves_per_position: float;
  max_depth: int;  (* Deepest book line *)
}
```

A good book might have:

- 100,000-1,000,000 unique positions
- 500,000-5,000,000 position-move pairs
- Average 3-8 moves per position
- Depth 15-25 (in opening lines)

## Using Books During Search

### When to Use Book

```ocaml
let get_move game book =
  if game.fullmove_number <= 15 then  (* First ~15 moves *)
    match probe_book book game.position with
    | [] -> None  (* Not in book, search normally *)
    | entries -> Some (select_move entries)
  else
    None  (* Past opening, rely on search *)
```

### Book Exit Strategy

Gradually phase out of the book:

```ocaml
let use_book_probability move_number =
  if move_number <= 10 then 1.0       (* Always use *)
  else if move_number <= 15 then 0.5  (* 50% chance *)
  else 0.0                            (* Never use *)

let get_move game book =
  let prob = use_book_probability game.fullmove_number in
  if Random.float 1.0 < prob then
    match probe_book book game.position with
    | [] -> search_for_move game
    | entries -> select_move entries
  else
    search_for_move game
```

This adds randomness and prevents always following the longest book lines.

## Book File Locations

Standard search paths:

```ocaml
let find_book () =
  let candidates = [
    "./book.bin";                              (* Current directory *)
    Sys.getenv "XDG_DATA_HOME" ^ "/chessml/book.bin";
    Sys.getenv "HOME" ^ "/.local/share/chessml/book.bin";
    Sys.getenv "HOME" ^ "/.chessml/book.bin";
    "/usr/local/share/chessml/book.bin";
    "/usr/share/chessml/book.bin";
  ] in
  List.find_opt Sys.file_exists candidates
```

## Common Pitfalls

### 1. Wrong Zobrist Hashing

```ocaml
(* Wrong: Use your internal hash *)
let hash = Zobrist.compute pos in

(* Correct: Use Polyglot hash *)
let hash = polyglot_hash pos in
```

Polyglot has specific random numbers—you must use those exact values!

### 2. Forgetting to Validate Moves

```ocaml
(* Wrong: Assume book move is legal *)
let move = decode_move encoded pos in
make_move pos move

(* Correct: Verify legality *)
let move = decode_move encoded pos in
if is_legal pos move then
  make_move pos move
else
  search_for_move pos
```

Book files can be corrupted or incompatible with your move generation.

### 3. Using Book Too Long

```ocaml
(* Wrong: Use book for entire game *)
if has_book_move then use_book_move

(* Correct: Exit after opening *)
if move_number <= 15 && has_book_move then use_book_move
```

Books don't cover middlegames/endgames—search is better there.

### 4. No Variety

```ocaml
(* Wrong: Always play highest weight *)
let move = List.hd (sort_by_weight entries) in

(* Correct: Add randomness *)
let move = weighted_random_select entries in
```

Without variety, your engine plays the same opening every game.

## Performance

Book lookup is extremely fast:

- **Binary search:** O(log n) = ~20 comparisons for 1M entries
- **Read entries:** O(k) where k = moves for this position (typically 2-5)
- **Total time:** ~0.1ms

Compare to search: 1-10 seconds per move!

## Advanced: Learning

Some engines modify book weights during play:

```ocaml
(* After game ends *)
let update_book_weights book game result =
  List.iteri (fun ply move ->
    if ply < 15 then  (* Only opening moves *)
      let pos = game.positions.(ply) in
      let hash = polyglot_hash pos in

      (* Increase weight if won, decrease if lost *)
      let delta = match result with
        | Win -> +10
        | Draw -> +1
        | Loss -> -5
      in

      update_weight book hash move delta
  ) game.moves
```

This lets the book "learn" from experience, but requires careful tuning to avoid overfitting.

## Further Reading

- [Zobrist Hashing](zobrist-hashing.md) - Needed for book lookups
- [Polyglot Book Format](http://hgm.nubati.net/book_format.html) - Official specification
- [Chess Programming Wiki - Opening Book](https://www.chessprogramming.org/Opening_Book)
- `lib/engine/opening_book.ml` and `polyglot.ml` - See implementation
- `bin/create_book.ml` - Book generation from PGN files
