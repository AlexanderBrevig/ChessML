(** Create comprehensive opening book from PGN files *)

open Chessml

(** Configuration *)
let max_ply = 30 (* Maximum opening depth in half-moves *)
let min_game_count = 1 (* Minimum games to include a position *)
let openings_dir = "openings" (* Directory containing PGN files *)

(** Hash table to accumulate move statistics: (zobrist_key, move) -> count *)
module MoveKey = struct
  type t = int64 * Move.t

  let equal (k1, m1) (k2, m2) =
    Int64.equal k1 k2 && Move.from m1 = Move.from m2 && Move.to_square m1 = Move.to_square m2
  ;;

  let hash (k, m) = Hashtbl.hash (Int64.to_int k, Move.from m, Move.to_square m)
end

module MoveStats = Hashtbl.Make (MoveKey)

let move_counts = MoveStats.create 1000000

(** Get all PGN files from directory *)
let get_pgn_files dir =
  let files = Sys.readdir dir in
  Array.to_list files
  |> List.filter (fun f -> Filename.check_suffix f ".pgn")
  |> List.map (fun f -> Filename.concat dir f)
;;

(** Process a single game and add moves to statistics *)
let process_game_debug = ref true

let process_game game =
  let rec process_moves pos move_list ply_count =
    if ply_count >= max_ply
    then ply_count (* Return number of plies processed *)
    else (
      match move_list with
      | [] -> ply_count (* Return number of plies processed *)
      | mv :: rest ->
        let key = Zobrist.compute pos in
        let move_key = key, mv in
        let current_count =
          try MoveStats.find move_counts move_key with
          | Not_found -> 0
        in
        MoveStats.replace move_counts move_key (current_count + 1);
        let new_pos = Position.make_move pos mv in
        process_moves new_pos rest (ply_count + 1))
  in
  let moves = Pgn_parser.game_to_moves game in
  let san_move_count = List.length game.Pgn_parser.moves in
  let parsed_move_count = List.length moves in
  if !process_game_debug && san_move_count > 0 then (
    Printf.eprintf "Debug: SAN moves (%d): %s\n" san_move_count
      (String.concat ", " (List.filteri (fun i _ -> i < 5) game.Pgn_parser.moves));
    Printf.eprintf "Debug: Parsed moves (%d)\n" parsed_move_count;
    process_game_debug := false
  );
  let start_pos = Position.default () in
  let plies_processed = process_moves start_pos moves 0 in
  plies_processed
;;

(** Process all PGN files and build statistics *)
let process_all_files files =
  let total_games = ref 0 in
  let total_plies = ref 0 in
  let total_files = List.length files in
  Printf.printf "📂 Processing %d PGN files from %s/\n\n" total_files openings_dir;
  List.iteri
    (fun idx filename ->
       Printf.printf
         "[%d/%d] %s... "
         (idx + 1)
         total_files
         (Filename.basename filename);
       flush stdout;
       let games = Pgn_parser.parse_file filename in
       let game_count = List.length games in
       total_games := !total_games + game_count;
       let plies_in_file = ref 0 in
       List.iter (fun g -> plies_in_file := !plies_in_file + process_game g) games;
       total_plies := !total_plies + !plies_in_file;
       Printf.printf "✓ (%d games, avg %.1f plies)\n" game_count 
         (float_of_int !plies_in_file /. float_of_int game_count))
    files;
  Printf.printf "\n📊 Processed %d games total (avg %.1f plies per game)\n\n" 
    !total_games
    (float_of_int !total_plies /. float_of_int !total_games)
;;

(** Convert statistics to book entries with weights *)
let create_book_entries () =
  let entries = ref [] in
  MoveStats.iter
    (fun (zobrist, move) count ->
       if count >= min_game_count
       then (
         (* Weight is proportional to frequency, scaled for book format *)
         let weight = min 65535 (count * 100) in
         entries := (zobrist, move, weight) :: !entries))
    move_counts;
  !entries
;;

let () =
  Printf.printf "📚 Creating Opening Book from PGN Files\n";
  Printf.printf "%s\n\n" (String.make 60 '=');
  (* Get all PGN files *)
  let pgn_files = get_pgn_files openings_dir in
  if List.length pgn_files = 0
  then (
    Printf.eprintf "Error: No PGN files found in %s/\n" openings_dir;
    exit 1);
  (* Process all games *)
  process_all_files pgn_files;
  (* Create book entries *)
  Printf.printf "📝 Building book entries (min %d games)...\n" min_game_count;
  let entries = create_book_entries () in
  Printf.printf "   • %d unique position-move combinations\n" (List.length entries);
  let unique_positions =
    List.map (fun (k, _, _) -> k) entries |> List.sort_uniq Int64.compare |> List.length
  in
  Printf.printf "   • %d unique positions\n\n" unique_positions;
  (* Sort entries by zobrist key (required for binary search) *)
  Printf.printf "💾 Writing book.bin...\n";
  let sorted_entries =
    List.sort (fun (k1, _, _) (k2, _, _) -> Int64.compare k1 k2) entries
  in
  (* Write to file *)
  let oc = open_out_bin "book.bin" in
  List.iter
    (fun (key, move, weight) ->
       let entry = Polyglot.make_entry key (Move.from move) (Move.to_square move) weight in
       Polyglot.write_entry oc entry)
    sorted_entries;
  close_out oc;
  let file_size = List.length sorted_entries * 16 in
  Printf.printf "   • %d bytes\n" file_size;
  Printf.printf "   • %d moves\n\n" (List.length sorted_entries);
  Printf.printf "✅ Created book.bin successfully!\n\n";
  (* Self-test the generated book *)
  Printf.printf "%s\n" (String.make 60 '=');
  Printf.printf "🧪 Self-Testing Book\n\n";
  let book = Opening_book.open_book "book.bin" in
  let test_passed = ref true in
  (* Test starting position *)
  Printf.printf "Test: Starting position has book moves... ";
  flush stdout;
  let start_pos = Position.default () in
  (match Opening_book.get_book_move ~random:true book start_pos with
   | Some mv ->
     Printf.printf "✅\n";
     Printf.printf "   Sample move: %s\n" (Move.to_uci mv)
   | None ->
     Printf.printf "❌ No book moves found!\n";
     test_passed := false);
  Printf.printf "\n";
  if !test_passed
  then Printf.printf "✅ Book is working correctly!\n"
  else (
    Printf.printf "❌ Book test failed.\n";
    exit 1)
;;
