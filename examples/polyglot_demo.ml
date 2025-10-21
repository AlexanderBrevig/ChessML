(** Demonstrate Polyglot format encoding/decoding *)

open Chessml

let () =
  Printf.printf "=== Polyglot Format Demo ===\n\n";
  (* Example 1: Encode a move *)
  Printf.printf "1. Move Encoding:\n";
  let from_sq = Square.e2 in
  let to_sq = Square.e4 in
  let encoded = Polyglot.encode_move from_sq to_sq 0 in
  Printf.printf "   e2e4 encoded as: 0x%04x\n" encoded;
  Printf.printf
    "   Binary: from=%d to=%d\n\n"
    (encoded land 0x3F)
    ((encoded lsr 6) land 0x3F);
  (* Example 2: Create and write an entry *)
  Printf.printf "2. Creating Book Entry:\n";
  let pos = Position.default () in
  let key = Zobrist.compute pos in
  let entry = Polyglot.make_entry key Square.e2 Square.e4 5000 in
  Printf.printf "   Zobrist key: 0x%Lx\n" entry.Polyglot.key;
  Printf.printf "   Move: 0x%04x\n" entry.Polyglot.move;
  Printf.printf "   Weight: %d\n\n" entry.Polyglot.weight;
  (* Example 3: Write and read back *)
  Printf.printf "3. Write/Read Roundtrip:\n";
  let tmp_file = Filename.temp_file "polyglot_test" ".bin" in
  let oc = open_out_bin tmp_file in
  Polyglot.write_entry oc entry;
  close_out oc;
  let ic = open_in_bin tmp_file in
  (match Polyglot.read_entry ic with
   | Some read_entry ->
     Printf.printf "   ✓ Successfully read entry\n";
     Printf.printf "   Key matches: %b\n" (read_entry.Polyglot.key = entry.Polyglot.key);
     Printf.printf "   Move matches: %b\n" (read_entry.Polyglot.move = entry.Polyglot.move);
     Printf.printf
       "   Weight matches: %b\n\n"
       (read_entry.Polyglot.weight = entry.Polyglot.weight)
   | None -> Printf.printf "   ✗ Failed to read entry\n\n");
  close_in ic;
  Sys.remove tmp_file;
  (* Example 4: Decode move *)
  Printf.printf "4. Move Decoding:\n";
  let pos = Position.default () in
  (match Polyglot.decode_move pos encoded with
   | Some move ->
     Printf.printf
       "   Decoded move: %s -> %s\n"
       (Square.to_string (Move.from move))
       (Square.to_string (Move.to_square move));
     Printf.printf
       "   Move kind: %s\n\n"
       (match Move.kind move with
        | Move.PawnDoublePush -> "Pawn Double Push"
        | Move.Quiet -> "Quiet"
        | Move.Capture -> "Capture"
        | _ -> "Other")
   | None -> Printf.printf "   ✗ Failed to decode\n\n");
  (* Example 5: Opening book with multiple entries *)
  Printf.printf "5. Creating Mini Book:\n";
  let book_file = Filename.temp_file "mini_book" ".bin" in
  let oc = open_out_bin book_file in
  (* Add e4, d4, Nf3 for starting position *)
  let start_key = Zobrist.compute (Position.default ()) in
  let entries =
    [ Polyglot.make_entry start_key Square.e2 Square.e4 10000
    ; Polyglot.make_entry start_key Square.d2 Square.d4 8000
    ; Polyglot.make_entry start_key Square.g1 Square.f3 4000
    ]
  in
  (* Sort by key (required for binary search) *)
  let sorted =
    List.sort (fun e1 e2 -> Int64.compare e1.Polyglot.key e2.Polyglot.key) entries
  in
  List.iter (Polyglot.write_entry oc) sorted;
  close_out oc;
  Printf.printf "   Written %d entries\n" (List.length entries);
  Printf.printf
    "   File size: %d bytes (%d entries * 16 bytes)\n\n"
    (Unix.stat book_file).st_size
    (List.length entries);
  (* Read back and probe *)
  Printf.printf "6. Probing the Book:\n";
  (match Opening_book.open_book book_file with
   | Some book ->
     let moves = Opening_book.probe (Some book) (Position.default ()) in
     Printf.printf "   Found %d moves:\n" (List.length moves);
     List.iter
       (fun (move, weight) ->
          Printf.printf
            "     %s -> %s (weight: %d)\n"
            (Square.to_string (Move.from move))
            (Square.to_string (Move.to_square move))
            weight)
       moves;
     Opening_book.close_book book
   | None -> Printf.printf "   ✗ Failed to open book\n");
  Sys.remove book_file;
  Printf.printf "\n✅ Polyglot demo complete!\n"
;;
