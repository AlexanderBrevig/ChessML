(** UCI - Universal Chess Interface protocol implementation
    
    Implements the UCI protocol for communication with chess GUIs (Arena, ChessBase, etc.).
    Handles position setup, search commands, time management, and info output.
    Supports standard UCI commands: position, go, stop, quit, isready, setoption.
    
    Reference: http://wbec-ridderkerk.nl/html/UCIProtocol.html
*)

open Chessml_core
open Chessml_engine

type search_params =
  { depth : int option
  ; movetime : int option
  ; wtime : int option
  ; btime : int option
  ; winc : int option
  ; binc : int option
  ; movestogo : int option
  }

(** Parse UCI setoption command *)
let parse_setoption (tokens : string list) : unit =
  let rec find_name_value acc = function
    | [] -> None, None
    | "name" :: rest ->
      let rec collect_name name_parts = function
        | [] -> Some (String.concat " " (List.rev name_parts)), None
        | "value" :: value_parts ->
          ( Some (String.concat " " (List.rev name_parts))
          , Some (String.concat " " value_parts) )
        | token :: rest -> collect_name (token :: name_parts) rest
      in
      collect_name [] rest
    | _ :: rest -> find_name_value acc rest
  in
  match find_name_value [] tokens with
  | Some name, Some value ->
    (match String.lowercase_ascii name with
     | "maxdepth" ->
       (try
          let depth = int_of_string value in
          Config.set_max_search_depth depth;
          Printf.eprintf "Set MaxDepth to %d\n" depth;
          flush stderr
        with
        | _ ->
          Printf.eprintf "Invalid MaxDepth value: %s\n" value;
          flush stderr)
     | "quiescencedepth" ->
       (try
          let depth = int_of_string value in
          Config.set_max_quiescence_depth depth;
          Printf.eprintf "Set QuiescenceDepth to %d\n" depth;
          flush stderr
        with
        | _ ->
          Printf.eprintf "Invalid QuiescenceDepth value: %s\n" value;
          flush stderr)
     | "usequiescence" ->
       (match String.lowercase_ascii value with
        | "true" ->
          Config.set_use_quiescence true;
          Printf.eprintf "Set UseQuiescence to true\n";
          flush stderr
        | "false" ->
          Config.set_use_quiescence false;
          Printf.eprintf "Set UseQuiescence to false\n";
          flush stderr
        | _ ->
          Printf.eprintf "Invalid UseQuiescence value: %s (use true/false)\n" value;
          flush stderr)
     | "usetranspositiontable" ->
       (match String.lowercase_ascii value with
        | "true" ->
          Config.set_use_transposition_table true;
          Printf.eprintf "Set UseTranspositionTable to true\n";
          flush stderr
        | "false" ->
          Config.set_use_transposition_table false;
          Printf.eprintf "Set UseTranspositionTable to false\n";
          flush stderr
        | _ ->
          Printf.eprintf
            "Invalid UseTranspositionTable value: %s (use true/false)\n"
            value;
          flush stderr)
     | "debugoutput" ->
       (match String.lowercase_ascii value with
        | "true" ->
          Config.set_debug_output true;
          Printf.eprintf "Set DebugOutput to true\n";
          flush stderr
        | "false" ->
          Config.set_debug_output false;
          Printf.eprintf "Set DebugOutput to false\n";
          flush stderr
        | _ ->
          Printf.eprintf "Invalid DebugOutput value: %s (use true/false)\n" value;
          flush stderr)
     | "hash" ->
       (try
          let size_mb = int_of_string value in
          let size_entries = size_mb * 1024 * 1024 / 24 in
          (* Rough estimate *)
          Config.set_transposition_table_size size_entries;
          Printf.eprintf "Set Hash to %d MB (%d entries)\n" size_mb size_entries;
          flush stderr
        with
        | _ ->
          Printf.eprintf "Invalid Hash value: %s\n" value;
          flush stderr)
     | _ ->
       Printf.eprintf "Unknown UCI option: %s\n" name;
       flush stderr)
  | Some name, None ->
    Printf.eprintf "UCI option %s missing value\n" name;
    flush stderr
  | None, _ ->
    Printf.eprintf "UCI setoption missing name\n";
    flush stderr
;;

(** Apply a list of UCI moves to a game *)
let apply_moves (game : Game.t) (moves : string list) : Game.t =
  List.fold_left
    (fun g move_str ->
       try
         let mv = Move.of_uci move_str in
         Game.make_move g mv
       with
       | _ ->
         Printf.eprintf "Warning: Invalid move %s\n" move_str;
         flush stderr;
         g)
    game
    moves
;;

(** Parse UCI position command *)
let parse_position (tokens : string list) (current_game : Game.t) : Game.t =
  match tokens with
  | "startpos" :: rest ->
    let game = Game.default () in
    (match rest with
     | "moves" :: moves -> apply_moves game moves
     | _ -> game)
  | "fen" :: fen_parts ->
    (* FEN string might be split across multiple tokens *)
    let rec collect_fen acc remaining =
      match remaining with
      | [] -> String.concat " " (List.rev acc), []
      | "moves" :: moves -> String.concat " " (List.rev acc), moves
      | token :: rest ->
        if List.length acc >= 6
        then
          (* FEN has 6 parts max *)
          String.concat " " (List.rev acc), remaining
        else collect_fen (token :: acc) rest
    in
    let fen_string, moves = collect_fen [] fen_parts in
    let game = Game.from_fen fen_string in
    if moves = [] then game else apply_moves game moves
  | _ ->
    Printf.eprintf "Warning: Invalid position command\n";
    flush stderr;
    current_game
;;

(** Parse go command parameters *)
let parse_go_params (tokens : string list) : search_params =
  let rec parse acc = function
    | [] -> acc
    | "depth" :: d :: rest -> parse { acc with depth = Some (int_of_string d) } rest
    | "movetime" :: t :: rest -> parse { acc with movetime = Some (int_of_string t) } rest
    | "wtime" :: t :: rest -> parse { acc with wtime = Some (int_of_string t) } rest
    | "btime" :: t :: rest -> parse { acc with btime = Some (int_of_string t) } rest
    | "winc" :: t :: rest -> parse { acc with winc = Some (int_of_string t) } rest
    | "binc" :: t :: rest -> parse { acc with binc = Some (int_of_string t) } rest
    | "movestogo" :: n :: rest ->
      parse { acc with movestogo = Some (int_of_string n) } rest
    | "infinite" :: rest ->
      parse { acc with depth = Some 20 } rest (* Cap at depth 20 for infinite *)
    | _ :: rest -> parse acc rest
  in
  parse
    { depth = None
    ; movetime = None
    ; wtime = None
    ; btime = None
    ; winc = None
    ; binc = None
    ; movestogo = None
    }
    tokens
;;

(** Calculate search depth based on time controls *)
let calculate_depth (params : search_params) (side_to_move : Types.color) : int =
  match params.depth with
  | Some d -> d
  | None ->
    (* Use time controls to determine depth *)
    let our_time =
      match side_to_move with
      | White -> params.wtime
      | Black -> params.btime
    in
    (match our_time with
     | Some time_ms when time_ms < 1000 -> 3 (* Very low time *)
     | Some time_ms when time_ms < 5000 -> 4 (* Low time *)
     | Some time_ms when time_ms < 30000 -> 5 (* Medium time *)
     | Some time_ms when time_ms < 60000 -> 6 (* Good time *)
     | Some _ -> 7 (* Lots of time *)
     | None -> 5 (* Default depth *))
;;

(** Main UCI protocol loop *)
let main_loop () =
  let game = ref (Game.default ()) in
  (* Try to load opening book from multiple locations *)
  let try_book_paths =
    [ "book.bin"
    ; (* Current directory *)
      "/home/ab/github.com/chessml/book.bin"
    ; (* Absolute path *)
      Filename.concat
        (Sys.getenv_opt "HOME" |> Option.value ~default:"/home/ab")
        "github.com/chessml/book.bin"
    ]
  in
  let rec try_open_book = function
    | [] -> None
    | path :: rest ->
      (match Opening_book.open_book path with
       | Some book -> Some (book, path)
       | None -> try_open_book rest)
  in
  let opening_book_result = try_open_book try_book_paths in
  let opening_book = Option.map fst opening_book_result in
  (* Log book status to stderr for debugging *)
  (match opening_book_result with
   | Some (_, path) ->
     Printf.eprintf "# Opening book loaded from %s\n" path;
     flush stderr
   | None ->
     Printf.eprintf "# No opening book found\n";
     flush stderr);
  Printf.printf "# ChessML UCI Engine\n";
  flush stdout;
  try
    while true do
      let line = read_line () in
      let line = String.trim line in
      if line = ""
      then ()
      else (
        let tokens = String.split_on_char ' ' line |> List.filter (fun s -> s <> "") in
        match tokens with
        | [] -> ()
        | "uci" :: _ ->
          Printf.printf "id name ChessML 0.1.0\n";
          Printf.printf "id author ChessML Team\n";
          Printf.printf "option name Hash type spin default 16 min 1 max 1024\n";
          Printf.printf
            "option name MaxDepth type spin default %d min 1 max 50\n"
            (Config.get_max_search_depth ());
          Printf.printf
            "option name QuiescenceDepth type spin default %d min 1 max 20\n"
            (Config.get_max_quiescence_depth ());
          Printf.printf
            "option name UseQuiescence type check default %b\n"
            (Config.get_use_quiescence ());
          Printf.printf
            "option name UseTranspositionTable type check default %b\n"
            (Config.get_use_transposition_table ());
          Printf.printf
            "option name DebugOutput type check default %b\n"
            (Config.get_debug_output ());
          Printf.printf
            "option name OwnBook type check default %b\n"
            (Option.is_some opening_book);
          Printf.printf "option name Threads type spin default 1 min 1 max 1\n";
          Printf.printf "uciok\n";
          flush stdout
        | "debug" :: _ ->
          (* Ignore debug commands for now *)
          ()
        | "isready" :: _ ->
          Printf.printf "readyok\n";
          flush stdout
        | "setoption" :: rest -> parse_setoption rest
        | "register" :: _ ->
          (* No registration needed *)
          ()
        | "ucinewgame" :: _ -> game := Game.default ()
        | "position" :: rest -> game := parse_position rest !game
        | "go" :: params ->
          let search_params = parse_go_params params in
          let pos = Game.position !game in
          let side = Position.side_to_move pos in
          let depth = calculate_depth search_params side in
          (* Track search time *)
          let start_time = Unix.gettimeofday () in
          (* First, try the opening book *)
          let book_move = Opening_book.get_book_move ~random:true opening_book pos in
          let result =
            match book_move with
            | Some mv ->
              (* Book move found - use it without searching *)
              Printf.eprintf "# Book move: %s\n" (Move.to_uci mv);
              flush stderr;
              { Search.best_move = Some mv
              ; score = 0
              ; (* Book moves don't have scores *)
                depth = 0
              ; nodes = Int64.zero
              }
            | None ->
              (* No book move, search normally *)
              Search.find_best_move ~verbose:false !game depth
          in
          let end_time = Unix.gettimeofday () in
          let search_time_ms = int_of_float ((end_time -. start_time) *. 1000.0) in
          (* Format score for UCI (handle mate scores) *)
          let score_str =
            if abs result.score >= 90000
            then (
              let mate_in = ((100000 - abs result.score) / 2) + 1 in
              if result.score > 0
              then Printf.sprintf "mate %d" mate_in
              else Printf.sprintf "mate -%d" mate_in)
            else Printf.sprintf "cp %d" result.score
          in
          (* Send UCI info about the search *)
          Printf.printf
            "info depth %d score %s nodes %Ld time %d"
            result.depth
            score_str
            result.nodes
            search_time_ms;
          (match result.best_move with
           | Some mv ->
             Printf.printf " pv %s\n" (Move.to_uci mv);
             Printf.printf "bestmove %s\n" (Move.to_uci mv);
             flush stdout
           | None ->
             (* No legal moves - game over *)
             Printf.printf "\nbestmove 0000\n";
             flush stdout)
        | "stop" :: _ ->
          (* For now, we don't support stopping mid-search *)
          ()
        | "ponderhit" :: _ ->
          (* Pondering not supported *)
          ()
        | "quit" :: _ -> exit 0
        | cmd :: _ ->
          Printf.eprintf "Unknown command: %s\n" cmd;
          flush stderr)
    done
  with
  | End_of_file -> exit 0
  | e ->
    Printf.eprintf "Error: %s\n" (Printexc.to_string e);
    flush stderr;
    exit 1
;;
