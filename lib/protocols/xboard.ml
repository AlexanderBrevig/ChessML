(** XBoard - XBoard/WinBoard protocol implementation (aka CECP)
    
    Implements the XBoard/WinBoard protocol for communication with chess GUIs
    (XBoard, WinBoard, etc.). Handles game setup, move commands, time controls,
    and engine features. Supports protocol version 2 with feature negotiation.
    
    Reference: https://www.gnu.org/software/xboard/engine-intf.html
*)

open Chessml_core
open Chessml_engine

let version = "0.1.0"
let name = "ChessML"

(** Calculate search time based on remaining time *)
let calculate_search_time_ms time_centiseconds =
  (* Convert centiseconds to milliseconds *)
  let time_ms = time_centiseconds * 10 in
  (* Use a simple time management strategy:
     - Use 1/30th of remaining time for normal moves
     - Minimum 10ms (for very low time), maximum 30 seconds
     - If we have less than 500ms total, use 1/3 of remaining time
  *)
  let target_time = if time_ms < 500 then time_ms / 3 else time_ms / 30 in
  max 10 (min 30000 target_time)
;;

(** Convert move to XBoard notation (different from UCI) *)
let move_to_xboard_notation (mv : Move.t) : string =
  match Move.kind mv with
  | Move.ShortCastle -> "O-O"
  | Move.LongCastle -> "O-O-O"
  | _ -> Move.to_uci mv (* Regular moves use UCI notation *)
;;

(** Find best move, checking book first *)
let find_move opening_book game search_time_ms max_depth log =
  let pos = Game.position game in
  (* First, try the opening book *)
  Printf.fprintf log "Checking opening book...\n";
  flush log;
  let book_move = Opening_book.get_book_move ~random:true opening_book pos in
  match book_move with
  | Some mv ->
    Printf.fprintf log "BOOK MOVE FOUND: %s\n" (move_to_xboard_notation mv);
    flush log;
    Some mv
  | None ->
    (* No book move, search normally *)
    Printf.fprintf log "No book move, searching...\n";
    flush log;
    let result =
      Search.find_best_move ~verbose:false ~max_time_ms:search_time_ms game max_depth
    in
    Printf.fprintf log "Search completed\n";
    flush log;
    result.Search.best_move
;;

(** Main XBoard protocol loop *)
let main_loop () =
  Printexc.record_backtrace true;
  (* Enable exception backtraces *)

  (* Ignore SIGPIPE to prevent crashes when writing to closed pipes *)
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let game = ref (Game.default ()) in
  let force_mode = ref false in
  (* In force mode, engine doesn't think *)
  let post = ref false in
  (* Post thinking output *)
  let my_time = ref 30000 in
  (* Time remaining in centiseconds (default 5 min) *)
  let opponent_time = ref 30000 in
  (* Opponent time remaining *)
  (* Try to load opening book from multiple locations *)
  let try_book_paths = Config.get_book_paths () in
  let rec try_open_book = function
    | [] -> None
    | path :: rest ->
      (match Opening_book.open_book path with
       | Some book -> Some (book, path)
       | None -> try_open_book rest)
  in
  let opening_book_result = try_open_book try_book_paths in
  let opening_book = Option.map fst opening_book_result in
  (* Open debug log file *)
  let log =
    open_out_gen
      [ Open_wronly; Open_creat; Open_append; Open_text ]
      0o666
      "/tmp/chessml_xboard.log"
  in
  Printf.fprintf log "\n=== ChessML XBoard Engine Started ===\n";
  Printf.fprintf log "Current directory: %s\n" (Sys.getcwd ());
  Printf.fprintf
    log
    "Opening book: %s\n"
    (match opening_book_result with
     | Some (_, path) -> Printf.sprintf "loaded from %s" path
     | None -> "not found");
  flush log;
  flush stdout;
  (* Search (or probe the book) and play the engine's move on the current game *)
  let engine_reply () =
    let legal_moves = Game.legal_moves !game in
    let resign () =
      Printf.fprintf log "SEND: resign\n";
      flush log;
      Printf.printf "resign\n";
      flush stdout
    in
    if legal_moves = []
    then resign ()
    else (
      let search_time_ms = calculate_search_time_ms !my_time in
      (* Limit search depth when very low on time *)
      let max_depth = if !my_time < 100 then 3 else Config.get_max_search_depth () in
      match find_move opening_book !game search_time_ms max_depth log with
      | Some mv when List.mem mv legal_moves ->
        let move_str = move_to_xboard_notation mv in
        Printf.fprintf log "SEND: move %s\n" move_str;
        flush log;
        Printf.printf "move %s\n" move_str;
        flush stdout;
        game := Game.make_move !game mv
      | Some mv ->
        Printf.fprintf
          log
          "ERROR: engine produced illegal move %s in %s\n"
          (Move.to_uci mv)
          (Game.to_fen !game);
        flush log;
        resign ()
      | None -> resign ())
  in
  (* Apply an opponent move, then reply unless in force mode *)
  let user_move move_str =
    match Game.find_move !game move_str with
    | None ->
      Printf.fprintf log "Illegal move %s in %s\n" move_str (Game.to_fen !game);
      flush log;
      Printf.printf "Illegal move: %s\n" move_str;
      flush stdout
    | Some mv ->
      game := Game.make_move !game mv;
      if not !force_mode then engine_reply ()
  in
  try
    while true do
      Printf.fprintf log "=== Waiting for next command ===\n";
      flush log;
      let line =
        try
          Printf.fprintf log "About to call read_line()...\n";
          flush log;
          let result = read_line () in
          Printf.fprintf log "read_line() returned successfully\n";
          flush log;
          result
        with
        | End_of_file ->
          Printf.fprintf log "STDIN closed (End_of_file), exiting gracefully\n";
          flush log;
          close_out log;
          exit 0
      in
      Printf.fprintf log "RECV: %s\n" line;
      flush log;
      let tokens = String.split_on_char ' ' line |> List.filter (fun s -> s <> "") in
      (* Debug: log received commands to stderr *)
      (* Disabled for production use
      if tokens <> [] then begin
        Printf.eprintf "Received: %s\n" line;
        flush stderr
      end;
      *)
      match tokens with
      | [] -> ()
      | "xboard" :: _ ->
        (* XBoard mode - just acknowledge *)
        ()
      | "protover" :: _ ->
        (* Protocol version 2 features *)
        Printf.printf "feature ping=1 setboard=1 colors=0 usermove=1 option=1 done=1\n";
        flush stdout
      | "new" :: _ ->
        (* Start new game *)
        game := Game.default ();
        force_mode := false;
        ()
      | "force" :: _ ->
        (* Enter force mode - don't think, just accept moves *)
        force_mode := true;
        ()
      | "go" :: _ ->
        force_mode := false;
        engine_reply ()
      | "usermove" :: move_str :: _ -> user_move move_str
      | (("O-O" | "0-0" | "O-O-O" | "0-0-0") as move_str) :: _ -> user_move move_str
      | move_str :: _
        when String.length move_str >= 4
             && String.length move_str <= 5
             && move_str.[0] >= 'a'
             && move_str.[0] <= 'h'
             && move_str.[1] >= '1'
             && move_str.[1] <= '8'
             && move_str.[2] >= 'a'
             && move_str.[2] <= 'h'
             && move_str.[3] >= '1'
             && move_str.[3] <= '8' ->
        (* Old-style move without "usermove" prefix *)
        user_move move_str
      | "setboard" :: fen_parts ->
        (* Set position from FEN *)
        let fen = String.concat " " fen_parts in
        Printf.fprintf log "RECV setboard command with FEN: %s\n" fen;
        flush log;
        (try
           Printf.fprintf
             log
             "Before Game.of_fen: current position = %s\n"
             (Position.to_fen (Game.position !game));
           flush log;
           let new_game = Game.of_fen fen in
           Printf.fprintf
             log
             "Game.of_fen created new game with position: %s\n"
             (Position.to_fen (Game.position new_game));
           flush log;
           game := new_game;
           Printf.fprintf
             log
             "After assignment: game position = %s\n"
             (Position.to_fen (Game.position !game));
           flush log
         with
         | ex ->
           Printf.fprintf log "ERROR loading FEN: %s - %s\n" fen (Printexc.to_string ex);
           flush log;
           Printf.printf "Error (bad FEN): %s\n" fen;
           flush stdout)
      | "ping" :: n :: _ ->
        (* Respond to ping *)
        Printf.fprintf log "SEND: pong %s\n" n;
        flush log;
        Printf.printf "pong %s\n" n;
        flush stdout
      | "post" :: _ -> post := true
      | "nopost" :: _ -> post := false
      | "hard" :: _ ->
        (* Turn on pondering - not implemented *)
        ()
      | "easy" :: _ ->
        (* Turn off pondering *)
        ()
      | "random" :: _ ->
        (* Enable random play - ignore *)
        ()
      | "computer" :: _ ->
        (* Opponent is a computer - ignore *)
        ()
      | "level" :: _ ->
        (* Time controls - ignore for now *)
        ()
      | "time" :: time_str :: _ ->
        (* Our time remaining in centiseconds *)
        (try
           my_time := int_of_string time_str;
           Printf.fprintf log "Set my time to %d centiseconds\n" !my_time;
           flush log
         with
         | _ ->
           Printf.fprintf log "Invalid time value: %s\n" time_str;
           flush log)
      | "otim" :: time_str :: _ ->
        (* Opponent time remaining in centiseconds *)
        (try
           opponent_time := int_of_string time_str;
           Printf.fprintf log "Set opponent time to %d centiseconds\n" !opponent_time;
           flush log
         with
         | _ ->
           Printf.fprintf log "Invalid opponent time value: %s\n" time_str;
           flush log)
      | "option" :: rest ->
        (* XBoard option command: option name=value *)
        (match rest with
         | name_value :: _ ->
           (match String.split_on_char '=' name_value with
            | [ name; value ] ->
              (match String.lowercase_ascii name with
               | "maxdepth" ->
                 (try
                    let depth = int_of_string value in
                    Config.set_max_search_depth depth;
                    Printf.fprintf log "Set MaxDepth to %d\n" depth;
                    flush log
                  with
                  | _ ->
                    Printf.fprintf log "Invalid MaxDepth value: %s\n" value;
                    flush log)
               | "quiescencedepth" ->
                 (try
                    let depth = int_of_string value in
                    Config.set_max_quiescence_depth depth;
                    Printf.fprintf log "Set QuiescenceDepth to %d\n" depth;
                    flush log
                  with
                  | _ ->
                    Printf.fprintf log "Invalid QuiescenceDepth value: %s\n" value;
                    flush log)
               | "usequiescence" ->
                 (match String.lowercase_ascii value with
                  | "true" | "1" ->
                    Config.set_use_quiescence true;
                    Printf.fprintf log "Set UseQuiescence to true\n";
                    flush log
                  | "false" | "0" ->
                    Config.set_use_quiescence false;
                    Printf.fprintf log "Set UseQuiescence to false\n";
                    flush log
                  | _ ->
                    Printf.fprintf
                      log
                      "Invalid UseQuiescence value: %s (use true/false or 1/0)\n"
                      value;
                    flush log)
               | "usetranspositiontable" ->
                 (match String.lowercase_ascii value with
                  | "true" | "1" ->
                    Config.set_use_transposition_table true;
                    Printf.fprintf log "Set UseTranspositionTable to true\n";
                    flush log
                  | "false" | "0" ->
                    Config.set_use_transposition_table false;
                    Printf.fprintf log "Set UseTranspositionTable to false\n";
                    flush log
                  | _ ->
                    Printf.fprintf
                      log
                      "Invalid UseTranspositionTable value: %s (use true/false or 1/0)\n"
                      value;
                    flush log)
               | "debugoutput" ->
                 (match String.lowercase_ascii value with
                  | "true" | "1" ->
                    Config.set_debug_output true;
                    Printf.fprintf log "Set DebugOutput to true\n";
                    flush log
                  | "false" | "0" ->
                    Config.set_debug_output false;
                    Printf.fprintf log "Set DebugOutput to false\n";
                    flush log
                  | _ ->
                    Printf.fprintf
                      log
                      "Invalid DebugOutput value: %s (use true/false or 1/0)\n"
                      value;
                    flush log)
               | _ ->
                 Printf.fprintf log "Unknown XBoard option: %s\n" name;
                 flush log)
            | _ ->
              Printf.fprintf
                log
                "Invalid XBoard option format: %s (use name=value)\n"
                name_value;
              flush log)
         | [] ->
           Printf.fprintf log "XBoard option command missing arguments\n";
           flush log)
      | "quit" :: _ -> exit 0
      | "?" :: _ ->
        (* Move now - not implemented yet *)
        ()
      | _ :: _ ->
        (* Unknown command - ignore it *)
        ()
    done;
    Printf.fprintf log "END";
    flush log
  with
  | End_of_file ->
    Printf.fprintf log "Caught End_of_file in main exception handler\n";
    flush log;
    exit 0
  | e ->
    Printf.fprintf log "Caught exception in main handler: %s\n" (Printexc.to_string e);
    Printf.fprintf log "Backtrace: %s\n" (Printexc.get_backtrace ());
    flush log;
    exit 1
;;
