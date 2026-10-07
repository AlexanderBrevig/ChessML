(** Create comprehensive opening book from PGN files *)

open Chessml

(** Configuration *)
let max_ply = 20 (* Maximum opening depth in half-moves *)

let min_game_count = 3 (* Minimum games to include a position *)
let openings_dir = "openings" (* Directory containing PGN files *)

(** Move statistics keyed by (Polyglot key, Polyglot-encoded move) *)
module MoveKey = struct
  type t = int64 * int

  let equal (k1, m1) (k2, m2) = Int64.equal k1 k2 && m1 = m2
  let hash = Hashtbl.hash
end

module MoveStats = Hashtbl.Make (MoveKey)

(** Add the counts of [local] into [global] *)
let merge_stats global local =
  MoveStats.iter
    (fun key count ->
       let old = Option.value ~default:0 (MoveStats.find_opt global key) in
       MoveStats.replace global key (old + count))
    local
;;

let move_counts = MoveStats.create 100000

(** Get all PGN files from directory *)
let get_pgn_files dir =
  let files = Sys.readdir dir in
  Array.to_list files
  |> List.filter (fun f -> Filename.check_suffix f ".pgn")
  |> List.map (fun f -> Filename.concat dir f)
;;

(** Count the first [max_ply] moves of a game; returns the number of plies counted *)
let process_game local_stats game =
  let rec process_moves pos move_list ply_count =
    match move_list with
    | mv :: rest when ply_count < max_ply ->
      let move_key = Position.key pos, Polyglot.encode_move mv in
      let current_count =
        Option.value ~default:0 (MoveStats.find_opt local_stats move_key)
      in
      MoveStats.replace local_stats move_key (current_count + 1);
      process_moves (Position.make_move pos mv) rest (ply_count + 1)
    | _ -> ply_count
  in
  process_moves (Position.default ()) (Pgn_parser.game_to_moves game) 0
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
      let chunk_stats =
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
                 local_stats))
            chunk
          |> List.map (Task.await pool))
      in
      List.iter (merge_stats move_counts) chunk_stats;
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
            - count = 3       -> weight ≈ 6,254   (low frequency)
            - count = 100     -> weight ≈ 26,214  (moderate)
            - count = 1,000   -> weight ≈ 39,321  (high)
            - count = 10,000  -> weight ≈ 52,428  (very high)
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
    List.map (fun (k, _, _) -> k) entries
    |> List.sort_uniq Int64.unsigned_compare
    |> List.length
  in
  Printf.printf "   • %d unique positions\n\n" unique_positions;
  (* Polyglot books are sorted by key as an unsigned integer (binary search) *)
  Printf.printf "💾 Writing book.bin...\n";
  let sorted_entries =
    List.sort (fun (k1, _, _) (k2, _, _) -> Int64.unsigned_compare k1 k2) entries
  in
  (* Write to file *)
  let oc = open_out_bin "book.bin" in
  List.iter
    (fun (key, move, weight) ->
       Polyglot.write_entry oc { Polyglot.key; move; weight; learn = 0 })
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
