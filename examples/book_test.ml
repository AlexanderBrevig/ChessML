(** Test opening book functionality *)

open Chessml
open Chessml_engine

let () =
  Printf.printf "Opening Book Test\n";
  Printf.printf "=================\n\n";
  (* Try to open a book file *)
  let book_path = "book.bin" in
  Printf.printf "Looking for book file: %s\n" book_path;
  match Opening_book.open_book book_path with
  | None ->
    Printf.printf "❌ No book file found.\n\n";
    Printf.printf "To use this test:\n";
    Printf.printf "1. Download a Polyglot book (e.g., from Stockfish):\n";
    Printf.printf
      "   wget https://github.com/official-stockfish/books/raw/master/Performance.bin\n";
    Printf.printf "2. Rename it to book.bin in the project root\n";
    Printf.printf "3. Run this test again\n"
  | Some book ->
    Printf.printf "✅ Book file loaded successfully\n\n";
    (* Test starting position *)
    let start_pos =
      Position.of_fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    in
    Printf.printf "Starting position:\n";
    let stats = Opening_book.get_book_stats (Some book) start_pos in
    if stats = []
    then Printf.printf "  No book moves found\n"
    else (
      Printf.printf "  Book moves available:\n";
      List.iter
        (fun (move, weight, pct) ->
           Printf.printf "    %s - weight: %d (%.1f%%)\n" (Move.to_uci move) weight pct)
        (List.take 10 stats);
      (* Show top 10 *)
      Printf.printf "\n  Best move: ";
      (match Opening_book.get_book_move ~random:false (Some book) start_pos with
       | Some move -> Printf.printf "%s\n" (Move.to_uci move)
       | None -> Printf.printf "none\n");
      Printf.printf "  Random move: ";
      match Opening_book.get_book_move ~random:true (Some book) start_pos with
      | Some move -> Printf.printf "%s\n" (Move.to_uci move)
      | None -> Printf.printf "none\n");
    (* Test various opening positions by actually making moves *)
    let start =
      Position.of_fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    in
    Printf.printf "\nAfter 1.e4:\n";
    let pos_e4 =
      Position.make_move start (Move.make Square.e2 Square.e4 Move.PawnDoublePush)
    in
    let key_e4 = Zobrist.compute pos_e4 in
    Printf.printf "  Zobrist key: 0x%016Lx\n" key_e4;
    let stats = Opening_book.get_book_stats (Some book) pos_e4 in
    if stats = []
    then Printf.printf "  No book moves found\n"
    else (
      Printf.printf "  Book moves available:\n";
      List.iter
        (fun (move, weight, pct) ->
           Printf.printf "    %s - weight: %d (%.1f%%)\n" (Move.to_uci move) weight pct)
        stats);
    Printf.printf "\nAfter 1.d4:\n";
    let pos_d4 =
      Position.make_move start (Move.make Square.d2 Square.d4 Move.PawnDoublePush)
    in
    let stats = Opening_book.get_book_stats (Some book) pos_d4 in
    if stats = []
    then Printf.printf "  No book moves found\n"
    else (
      Printf.printf "  Book moves available:\n";
      List.iter
        (fun (move, weight, pct) ->
           Printf.printf "    %s - weight: %d (%.1f%%)\n" (Move.to_uci move) weight pct)
        stats);
    Printf.printf "\nAfter 1.e4 c6 (Caro-Kann):\n";
    let pos_caro = Position.make_move pos_e4 (Move.make Square.c7 Square.c6 Move.Quiet) in
    let stats = Opening_book.get_book_stats (Some book) pos_caro in
    if stats = []
    then Printf.printf "  No book moves found\n"
    else (
      Printf.printf "  Book moves available:\n";
      List.iter
        (fun (move, weight, pct) ->
           Printf.printf "    %s - weight: %d (%.1f%%)\n" (Move.to_uci move) weight pct)
        stats);
    Printf.printf "\nAfter 1.d4 d5 (London setup):\n";
    let pos_london =
      Position.make_move pos_d4 (Move.make Square.d7 Square.d5 Move.PawnDoublePush)
    in
    let stats = Opening_book.get_book_stats (Some book) pos_london in
    if stats = []
    then Printf.printf "  No book moves found\n"
    else (
      Printf.printf "  Book moves available:\n";
      List.iter
        (fun (move, weight, pct) ->
           Printf.printf "    %s - weight: %d (%.1f%%)\n" (Move.to_uci move) weight pct)
        stats);
    Opening_book.close_book book;
    Printf.printf "\n✅ Test complete\n"
;;
