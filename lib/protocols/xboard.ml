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
        (* Start thinking *)
        force_mode := false;
        let pos = Game.position !game in
        let legal_moves = Movegen.generate_moves pos in
        if legal_moves = []
        then Printf.printf "resign\n"
        else (
          let search_time_ms = calculate_search_time_ms !my_time in
          (* Limit search depth when very low on time *)
          let max_depth = if !my_time < 100 then 3 else Config.get_max_search_depth () in
          match find_move opening_book !game search_time_ms max_depth log with
          | Some mv ->
            let move_str = move_to_xboard_notation mv in
            Printf.fprintf log "SEND: move %s\n" move_str;
            flush log;
            Printf.printf "move %s\n" move_str;
            flush stdout;
            game := Game.make_move !game mv
          | None ->
            Printf.fprintf log "SEND: resign\n";
            flush log;
            Printf.printf "resign\n";
            flush stdout)
      | "usermove" :: move_str :: _ ->
        (* User made a move *)
        (try
           Printf.fprintf
             log
             "Before user move, position: %s\n"
             (Position.to_fen (Game.position !game));
           flush log;
           let mv_parsed = Move.of_uci move_str in
           (* Validate that the move is legal in the current position *)
           let pos = Game.position !game in
           let legal_moves = Movegen.generate_moves pos in
           let legal_move =
             List.find_opt
               (fun legal_mv ->
                  Move.from legal_mv = Move.from mv_parsed
                  && Move.to_square legal_mv = Move.to_square mv_parsed)
               legal_moves
           in
           match legal_move with
           | None ->
             Printf.fprintf log "ERROR: Opponent move %s is not legal!\n" move_str;
             Printf.fprintf log "Position: %s\n" (Position.to_fen pos);
             Printf.fprintf
               log
               "Legal moves: %s\n"
               (String.concat ", " (List.map Move.to_uci legal_moves));
             flush log;
             Printf.printf "Illegal move: %s\n" move_str;
             flush stdout
           | Some mv ->
             (* Use the legal move (with correct kind) instead of parsed move *)
             game := Game.make_move !game mv;
             Printf.fprintf
               log
               "After user move %s, position: %s\n"
               move_str
               (Position.to_fen (Game.position !game));
             flush log;
             (* If not in force mode, respond with our move *)
             if not !force_mode
             then (
               Printf.fprintf log "Processing move, about to search...\n";
               flush log;
               let pos = Game.position !game in
               let legal_moves = Movegen.generate_moves pos in
               Printf.fprintf log "Generated %d legal moves\n" (List.length legal_moves);
               flush log;
               if legal_moves = []
               then (
                 Printf.fprintf log "SEND: resign\n";
                 flush log;
                 Printf.printf "resign\n";
                 flush stdout)
               else (
                 let search_time_ms = calculate_search_time_ms !my_time in
                 let max_depth = if !my_time < 100 then 3 else Config.get_max_search_depth () in
                 Printf.fprintf
                   log
                   "Starting search at depth %d with %dms time limit...\n"
                   max_depth
                   search_time_ms;
                 flush log;
                 match find_move opening_book !game search_time_ms max_depth log with
                 | Some mv ->
                   (* Verify the move is actually legal before sending *)
                   if List.mem mv legal_moves
                   then (
                     let move_str = move_to_xboard_notation mv in
                     Printf.fprintf log "SEND: move %s\n" move_str;
                     flush log;
                     (* Log current position for debugging *)
                     let pos = Game.position !game in
                     Printf.fprintf
                       log
                       "Position after search: %s\n"
                       (Position.to_fen pos);
                     flush log;
                     Printf.printf "move %s\n" move_str;
                     flush stdout;
                     Printf.fprintf log "Making engine move on game...\n";
                     flush log;
                     game := Game.make_move !game mv;
                     Printf.fprintf log "Engine move applied successfully\n";
                     flush log)
                   else (
                     let pos = Game.position !game in
                     Printf.fprintf
                       log
                       "ERROR: Search returned illegal move %s!\n"
                       (Move.to_uci mv);
                     Printf.fprintf log "Position FEN: %s\n" (Position.to_fen pos);
                     Printf.fprintf
                       log
                       "Legal moves were: %s\n"
                       (String.concat ", " (List.map Move.to_uci legal_moves));
                     flush log;
                     Printf.printf "resign\n";
                     flush stdout)
                 | None ->
                   Printf.fprintf log "SEND: resign\n";
                   flush log;
                   Printf.printf "resign\n";
                   flush stdout);
               Printf.fprintf log "Finished processing usermove, returning to main loop\n";
               flush log)
             else (
               Printf.fprintf log "In force mode, not responding\n";
               flush log)
         with
         | ex ->
           Printf.fprintf log "ERROR in usermove handler: %s\n" (Printexc.to_string ex);
           flush log;
           Printf.eprintf
             "ERROR processing move %s: %s\n"
             move_str
             (Printexc.to_string ex);
           flush stderr;
           Printf.printf "Illegal move: %s\n" move_str;
           flush stdout)
      | "O-O" :: _ | "0-0" :: _ ->
        (* Kingside castling *)
        Printf.fprintf log "RECV: Kingside castling\n";
        flush log;
        (try
           let pos = Game.position !game in
           let side = Position.side_to_move pos in
           let castling_move =
             if side = White
             then Move.make (Square.of_uci "e1") (Square.of_uci "g1") Move.ShortCastle
             else Move.make (Square.of_uci "e8") (Square.of_uci "g8") Move.ShortCastle
           in
           game := Game.make_move !game castling_move;
           Printf.fprintf log "Applied kingside castling successfully\n";
           flush log;
           (* If not in force mode, respond with our move *)
           if not !force_mode
           then (
             let pos = Game.position !game in
             let legal_moves = Movegen.generate_moves pos in
             if legal_moves = []
             then (
               Printf.printf "resign\n";
               flush stdout)
             else (
               let search_time_ms = calculate_search_time_ms !my_time in
               let max_depth = if !my_time < 100 then 3 else Config.get_max_search_depth () in
               let result =
                 Search.find_best_move
                   ~verbose:false
                   ~max_time_ms:search_time_ms
                   !game
                   max_depth
               in
               match result.Search.best_move with
               | Some mv ->
                 let move_str = move_to_xboard_notation mv in
                 Printf.printf "move %s\n" move_str;
                 flush stdout;
                 game := Game.make_move !game mv
               | None ->
                 Printf.printf "resign\n";
                 flush stdout))
         with
         | ex ->
           Printf.fprintf log "ERROR in kingside castling: %s\n" (Printexc.to_string ex);
           flush log;
           Printf.printf "Illegal move: O-O\n";
           flush stdout)
      | "O-O-O" :: _ | "0-0-0" :: _ ->
        (* Queenside castling *)
        Printf.fprintf log "RECV: Queenside castling\n";
        flush log;
        (try
           let pos = Game.position !game in
           let side = Position.side_to_move pos in
           let castling_move =
             if side = White
             then Move.make (Square.of_uci "e1") (Square.of_uci "c1") Move.LongCastle
             else Move.make (Square.of_uci "e8") (Square.of_uci "c8") Move.LongCastle
           in
           game := Game.make_move !game castling_move;
           Printf.fprintf log "Applied queenside castling successfully\n";
           flush log;
           (* If not in force mode, respond with our move *)
           if not !force_mode
           then (
             let pos = Game.position !game in
             let legal_moves = Movegen.generate_moves pos in
             if legal_moves = []
             then (
               Printf.printf "resign\n";
               flush stdout)
             else (
               let search_time_ms = calculate_search_time_ms !my_time in
               let max_depth = if !my_time < 100 then 3 else Config.get_max_search_depth () in
               let result =
                 Search.find_best_move
                   ~verbose:false
                   ~max_time_ms:search_time_ms
                   !game
                   max_depth
               in
               match result.Search.best_move with
               | Some mv ->
                 let move_str = move_to_xboard_notation mv in
                 Printf.printf "move %s\n" move_str;
                 flush stdout;
                 game := Game.make_move !game mv
               | None ->
                 Printf.printf "resign\n";
                 flush stdout))
         with
         | ex ->
           Printf.fprintf log "ERROR in queenside castling: %s\n" (Printexc.to_string ex);
           flush log;
           Printf.printf "Illegal move: O-O-O\n";
           flush stdout)
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
        (* Old-style move without "usermove" prefix - validate UCI format *)
        (try
           let mv = Move.of_uci move_str in
           game := Game.make_move !game mv;
           (* If not in force mode, respond with our move *)
           if not !force_mode
           then (
             let pos = Game.position !game in
             let legal_moves = Movegen.generate_moves pos in
             if legal_moves = []
             then (
               Printf.fprintf log "SEND: resign\n";
               flush log;
               Printf.printf "resign\n";
               flush stdout)
             else (
               let search_time_ms = calculate_search_time_ms !my_time in
               let max_depth = if !my_time < 100 then 3 else Config.get_max_search_depth () in
               let result =
                 Search.find_best_move
                   ~verbose:false
                   ~max_time_ms:search_time_ms
                   !game
                   max_depth
               in
               match result.Search.best_move with
               | Some mv ->
                 let move_str = move_to_xboard_notation mv in
                 Printf.fprintf log "SEND: move %s\n" move_str;
                 flush log;
                 Printf.printf "move %s\n" move_str;
                 flush stdout;
                 game := Game.make_move !game mv
               | None ->
                 Printf.fprintf log "SEND: resign\n";
                 flush log;
                 Printf.printf "resign\n";
                 flush stdout))
         with
         | ex ->
           Printf.eprintf
             "ERROR processing move %s: %s\n"
             move_str
             (Printexc.to_string ex);
           flush stderr;
           Printf.printf "Illegal move: %s\n" move_str;
           flush stdout)
      | "setboard" :: fen_parts ->
        (* Set position from FEN *)
        let fen = String.concat " " fen_parts in
        Printf.fprintf log "RECV setboard command with FEN: %s\n" fen;
        flush log;
        (try
           Printf.fprintf
             log
             "Before Game.from_fen: current position = %s\n"
             (Position.to_fen (Game.position !game));
           flush log;
           let new_game = Game.from_fen fen in
           Printf.fprintf
             log
             "Game.from_fen created new game with position: %s\n"
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
