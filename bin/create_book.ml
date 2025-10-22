(** Create comprehensive opening book from PGN files *)

open Chessml

(** Configuration *)
let max_ply = 20 (* Maximum opening depth in half-moves *)

let min_game_count = 3 (* Minimum games to include a position *)
let openings_dir = "openings" (* Directory containing PGN files *)

(** Hash table to accumulate move statistics: (zobrist_key, move) -> count *)
module MoveKey = struct
  type t = int64 * Move.t

  let equal (k1, m1) (k2, m2) =
    Int64.equal k1 k2
    && Move.from m1 = Move.from m2
    && Move.to_square m1 = Move.to_square m2
  ;;

  let hash (k, m) = Hashtbl.hash (Int64.to_int k, Move.from m, Move.to_square m)
end

module MoveStats = Hashtbl.Make (MoveKey)

(** Write hash table to binary file *)
let write_stats_to_file filename stats =
  let oc = open_out_bin filename in
  MoveStats.iter
    (fun (zobrist, move) count ->
       output_binary_int oc (Int64.to_int (Int64.shift_right zobrist 32));
       output_binary_int oc (Int64.to_int zobrist);
       output_byte oc (Move.from move);
       output_byte oc (Move.to_square move);
       output_binary_int oc count)
    stats;
  close_out oc
;;

(** Read stats from binary file and merge into hash table *)
let merge_stats_from_file filename stats =
  let ic = open_in_bin filename in
  try
    while true do
      let high = input_binary_int ic in
      let low = input_binary_int ic in
      let zobrist =
        Int64.logor (Int64.shift_left (Int64.of_int high) 32) (Int64.of_int low)
      in
      let from_sq = input_byte ic in
      let to_sq = input_byte ic in
      let count = input_binary_int ic in
      (* Create a placeholder move - we only care about from/to for the key *)
      let move = Move.make from_sq to_sq Move.Quiet in
      let key = zobrist, move in
      let old =
        try MoveStats.find stats key with
        | Not_found -> 0
      in
      MoveStats.replace stats key (old + count)
    done
  with
  | End_of_file ->
    close_in ic;
    Sys.remove filename
;;

let move_counts = MoveStats.create 100000

(** Get all PGN files from directory *)
let get_pgn_files dir =
  let files = Sys.readdir dir in
  Array.to_list files
  |> List.filter (fun f -> Filename.check_suffix f ".pgn")
  |> List.map (fun f -> Filename.concat dir f)
;;

(** Process a single game and add moves to local statistics *)
let process_game_debug = ref true

let process_game local_stats game =
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
          try MoveStats.find local_stats move_key with
          | Not_found -> 0
        in
        MoveStats.replace local_stats move_key (current_count + 1);
        let new_pos = Position.make_move pos mv in
        process_moves new_pos rest (ply_count + 1))
  in
  let moves = Pgn_parser.game_to_moves game in
  let san_move_count = List.length game.Pgn_parser.moves in
  let parsed_move_count = List.length moves in
  if !process_game_debug && san_move_count > 0
  then (
    Printf.eprintf
      "Debug: SAN moves (%d): %s\n"
      san_move_count
      (String.concat ", " (List.filteri (fun i _ -> i < 5) game.Pgn_parser.moves));
    Printf.eprintf "Debug: Parsed moves (%d)\n" parsed_move_count;
    process_game_debug := false);
  let start_pos = Position.default () in
  let plies_processed = process_moves start_pos moves 0 in
  plies_processed
;;

(** Process all PGN files and build statistics *)
let process_all_files files =
  let total_files = List.length files in
  Printf.printf "📂 Processing %d PGN files from %s/\n\n" total_files openings_dir;
  (* Lock-free parallel processing using Domainslib.Task *)
  let open Domainslib in
  let num_domains =
    try int_of_string (Sys.getenv "CHESSML_PARALLEL") with
    | _ -> 4
  in
  let pool = Task.setup_pool ~num_domains () in
  let completed = Atomic.make 0 in
  let total_games = Atomic.make 0 in
  let total_plies = Atomic.make 0 in
  let chunk_num = ref 0 in
  (* Process files in chunks to manage memory *)
  let chunk_size = num_domains * 8 in
  let rec process_chunks remaining =
    match remaining with
    | [] -> ()
    | _ ->
      let chunk, rest =
        let rec take n acc lst =
          match lst, n with
          | [], _ | _, 0 -> List.rev acc, lst
          | x :: xs, n -> take (n - 1) (x :: acc) xs
        in
        take chunk_size [] remaining
      in
      chunk_num := !chunk_num + 1;
      let temp_files =
        Task.run pool (fun () ->
          List.map
            (fun filename ->
               Task.async pool (fun () ->
                 let games = Pgn_parser.parse_file filename in
                 let local_stats = MoveStats.create 500 in
                 let file_plies = ref 0 in
                 List.iter
                   (fun game ->
                      let plies = process_game local_stats game in
                      file_plies := !file_plies + plies)
                   games;
                 let count = Atomic.fetch_and_add completed 1 + 1 in
                 ignore (Atomic.fetch_and_add total_games (List.length games));
                 ignore (Atomic.fetch_and_add total_plies !file_plies);
                 Printf.printf
                   "[%d/%d] %s: %d games, %d plies\n"
                   count
                   total_files
                   (Filename.basename filename)
                   (List.length games)
                   !file_plies;
                 flush stdout;
                 (* Write to temp file and return filename *)
                 let temp_file =
                   Printf.sprintf "/tmp/chessml_book_%d_%d.tmp" !chunk_num count
                 in
                 write_stats_to_file temp_file local_stats;
                 MoveStats.clear local_stats;
                 temp_file))
            chunk
          |> List.map (Task.await pool))
      in
      (* Merge temp files into global stats *)
      List.iter (fun temp_file -> merge_stats_from_file temp_file move_counts) temp_files;
      Gc.minor ();
      process_chunks rest
  in
  process_chunks files;
  Task.teardown_pool pool;
  let final_games = Atomic.get total_games in
  let final_plies = Atomic.get total_plies in
  Printf.printf
    "\n📊 Processed %d games total (avg %.1f plies per game)\n\n"
    final_games
    (float_of_int final_plies /. float_of_int final_games)
;;

(** Convert statistics to book entries with weights *)
let create_book_entries () =
  (* First pass: find max count for normalization context *)
  let max_count = ref 0 in
  MoveStats.iter (fun _ count -> max_count := max !max_count count) move_counts;
  Printf.printf "   • Max game count for any move: %d\n" !max_count;
  let entries = ref [] in
  MoveStats.iter
    (fun (zobrist, move) count ->
       if count >= min_game_count
       then (
         (* Logarithmic scaling to better use the 16-bit range:
            - Maps counts from [min_game_count, max_count] to [1, 65535]
            - Uses log scale so differences are preserved even for high counts
            - Formula: weight = 1 + (65534 * log(count) / log(max_count))
            
            Example with max_count = 100,000:
            - count = 3       -> weight ≈ 5,263   (low frequency)
            - count = 100     -> weight ≈ 26,314  (moderate)
            - count = 1,000   -> weight ≈ 39,471  (high)
            - count = 10,000  -> weight ≈ 52,629  (very high)
            - count = 100,000 -> weight = 65,535  (maximum)
         *)
         let log_count = log (float_of_int count) in
         let log_max = log (float_of_int !max_count) in
         let normalized = log_count /. log_max in
         let weight = 1 + int_of_float (65534.0 *. normalized) in
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
       let entry =
         Polyglot.make_entry key (Move.from move) (Move.to_square move) weight
       in
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
