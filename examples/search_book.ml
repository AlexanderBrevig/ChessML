open Chessml
open Chessml_core

let () =
  Printf.printf "Direct Book Search Test\n";
  Printf.printf "=======================\n\n";
  (* Target key we're looking for *)
  let target_key = 0xea296e5ba23f6188L in
  Printf.printf "Looking for key: 0x%016Lx\n\n" target_key;
  (* Read book entries *)
  let ch = open_in_bin "book.bin" in
  let read_u16 () =
    let b1 = input_byte ch in
    let b2 = input_byte ch in
    (b1 lsl 8) lor b2
  in
  let read_u32 () =
    let w1 = read_u16 () in
    let w2 = read_u16 () in
    Int64.logor (Int64.shift_left (Int64.of_int w1) 16) (Int64.of_int w2)
  in
  let read_u64 () =
    let d1 = read_u32 () in
    let d2 = read_u32 () in
    Int64.logor (Int64.shift_left d1 32) d2
  in
  let rec find_key () =
    try
      let key = read_u64 () in
      let move = read_u16 () in
      let weight = read_u16 () in
      let _ = read_u32 () in
      (* learn *)
      if key = target_key
      then (
        Printf.printf "✅ FOUND! Move: 0x%04x  Weight: %d\n" move weight;
        find_key ())
      else find_key ()
    with
    | End_of_file -> ()
  in
  find_key ();
  close_in ch;
  (* Now try with opening_book module *)
  Printf.printf "\n--- Using Opening_book module ---\n\n";
  let book_opt = Chessml.Engine.Opening_book.open_book "book.bin" in
  match book_opt with
  | None -> Printf.printf "❌ Failed to load book\n"
  | Some book ->
    let pos =
      Position.of_fen "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
    in
    let stats = Chessml.Engine.Opening_book.get_book_stats book_opt pos in
    Printf.printf "Found %d moves\n" (List.length stats);
    List.iter
      (fun (move, weight, pct) ->
         Printf.printf "  %s - weight: %d (%.1f%%)\n" (Move.to_uci move) weight pct)
      stats;
    Chessml.Engine.Opening_book.close_book book
;;
