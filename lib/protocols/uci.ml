(** UCI - Universal Chess Interface protocol implementation

    Searches run on a separate thread so that "stop", "isready" and "quit" are
    handled while the engine thinks. Every line is handled independently: a
    malformed command is reported with "info string" and does not end the
    session.

    Reference: https://www.shredderchess.com/download/div/uci.zip
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
  ; infinite : bool
  }

type session =
  { mutable game : Game.t
  ; book : Opening_book.book option
  ; send : string -> unit
  ; mutable search : Thread.t option
  ; mutable stopped : bool Atomic.t (** stop flag of the current search *)
  }

let create_session ?(send = print_endline) ?book () =
  let lock = Mutex.create () in
  let send line =
    Mutex.protect lock (fun () ->
      send line;
      flush stdout)
  in
  { game = Game.default (); book; send; search = None; stopped = Atomic.make false }
;;

(** Apply UCI moves to a game. Each move is resolved against the legal moves so
    castling, en passant and double pushes are applied correctly. *)
let apply_moves (game : Game.t) (moves : string list) : Game.t =
  List.fold_left
    (fun g move_str ->
       match Game.find_move g move_str with
       | Some mv -> Game.make_move g mv
       | None -> failwith ("illegal move " ^ move_str))
    game
    moves
;;

(** Parse "position [startpos | fen <fen>] [moves ...]" (without the keyword) *)
let parse_position (tokens : string list) : Game.t =
  let rec split_at_moves acc = function
    | [] -> List.rev acc, []
    | "moves" :: moves -> List.rev acc, moves
    | t :: rest -> split_at_moves (t :: acc) rest
  in
  match split_at_moves [] tokens with
  | [ "startpos" ], moves -> apply_moves (Game.default ()) moves
  | "fen" :: (_ :: _ as fen), moves ->
    apply_moves (Game.of_fen (String.concat " " fen)) moves
  | _ -> failwith "position: expected 'startpos' or 'fen <fen>'"
;;

(** Parse "go" parameters *)
let parse_go_params (tokens : string list) : search_params =
  let int s =
    match int_of_string_opt s with
    | Some n -> n
    | None -> failwith ("go: expected a number, got " ^ s)
  in
  let rec parse acc = function
    | [] -> acc
    | "depth" :: v :: rest -> parse { acc with depth = Some (int v) } rest
    | "movetime" :: v :: rest -> parse { acc with movetime = Some (int v) } rest
    | "wtime" :: v :: rest -> parse { acc with wtime = Some (int v) } rest
    | "btime" :: v :: rest -> parse { acc with btime = Some (int v) } rest
    | "winc" :: v :: rest -> parse { acc with winc = Some (int v) } rest
    | "binc" :: v :: rest -> parse { acc with binc = Some (int v) } rest
    | "movestogo" :: v :: rest -> parse { acc with movestogo = Some (int v) } rest
    | "infinite" :: rest -> parse { acc with infinite = true } rest
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
    ; infinite = false
    }
    tokens
;;

(** Time limit for a search in milliseconds, [None] for no limit *)
let time_limit_ms (params : search_params) (side : Types.color) =
  let remaining, increment =
    match side with
    | White -> params.wtime, params.winc
    | Black -> params.btime, params.binc
  in
  if params.infinite
  then None
  else (
    match params.movetime, remaining with
    | Some ms, _ -> Some ms
    | None, Some remaining_ms ->
      Some
        (Protocol_common.time_budget_ms
           ~remaining_ms
           ~increment_ms:(Option.value ~default:0 increment)
           ~moves_to_go:params.movestogo)
    | None, None -> None)
;;

let info_line start (r : Search.search_result) =
  let ms = max 1 (int_of_float ((Unix.gettimeofday () -. start) *. 1000.0)) in
  Printf.sprintf
    "info depth %d score %s nodes %Ld nps %Ld time %d pv %s"
    r.depth
    (Score.to_uci r.score)
    r.nodes
    (Int64.div (Int64.mul r.nodes 1000L) (Int64.of_int ms))
    ms
    (String.concat " " (List.map Move.to_uci r.pv))
;;

(** Stop a running search and wait for it to report its move *)
let stop session =
  Atomic.set session.stopped true;
  Option.iter Thread.join session.search;
  session.search <- None
;;

let go session params =
  stop session;
  let stopped = Atomic.make false in
  session.stopped <- stopped;
  let game = session.game in
  let pos = Game.position game in
  match Protocol_common.book_move session.book pos with
  | Some mv when not params.infinite ->
    session.send (Printf.sprintf "info string book move %s" (Move.to_uci mv));
    session.send ("bestmove " ^ Move.to_uci mv)
  | _ ->
    let depth = Option.value params.depth ~default:(Config.get_max_search_depth ()) in
    let max_time_ms = time_limit_ms params (Position.side_to_move pos) in
    let start = Unix.gettimeofday () in
    let run () =
      let result =
        Search.find_best_move
          ~verbose:false
          ?max_time_ms
          ~stop:stopped
          ~on_iteration:(fun r -> session.send (info_line start r))
          game
          depth
      in
      (* An infinite search reports only once the GUI says stop *)
      while params.infinite && not (Atomic.get stopped) do
        Thread.delay 0.005
      done;
      session.send
        ("bestmove " ^ Option.fold ~none:"0000" ~some:Move.to_uci result.best_move)
    in
    session.search <- Some (Thread.create run ())
;;

let send_id_and_options session =
  session.send "id name ChessML";
  session.send "id author Alexander Brevig";
  List.iter
    (fun (o : Protocol_common.engine_option) ->
       session.send
         (match o.kind with
          | Spin { default; min; max } ->
            Printf.sprintf
              "option name %s type spin default %d min %d max %d"
              o.name
              default
              min
              max
          | Check default ->
            Printf.sprintf "option name %s type check default %b" o.name default))
    Protocol_common.options;
  session.send "uciok"
;;

(** "setoption name <name> value <value>" (without the keyword) *)
let set_option session tokens =
  let rec split name = function
    | "value" :: value -> String.concat " " (List.rev name), String.concat " " value
    | t :: rest -> split (t :: name) rest
    | [] -> String.concat " " (List.rev name), ""
  in
  match tokens with
  | "name" :: rest ->
    let name, value = split [] rest in
    (match Protocol_common.set_option name value with
     | Ok () -> ()
     | Error msg -> session.send ("info string " ^ msg))
  | _ -> session.send "info string setoption: expected 'name'"
;;

(** Handle one input line. Returns [`Quit] after "quit". *)
let handle_line session line =
  let tokens = String.split_on_char ' ' (String.trim line) |> List.filter (( <> ) "") in
  try
    match tokens with
    | [] -> `Continue
    | "uci" :: _ ->
      send_id_and_options session;
      `Continue
    | "isready" :: _ ->
      session.send "readyok";
      `Continue
    | "setoption" :: rest ->
      set_option session rest;
      `Continue
    | "ucinewgame" :: _ ->
      stop session;
      Search.new_game ();
      session.game <- Game.default ();
      `Continue
    | "position" :: rest ->
      stop session;
      session.game <- parse_position rest;
      `Continue
    | "go" :: rest ->
      go session (parse_go_params rest);
      `Continue
    | "stop" :: _ ->
      stop session;
      `Continue
    | "quit" :: _ ->
      stop session;
      `Quit
    | ("debug" | "register" | "ponderhit") :: _ -> `Continue
    | cmd :: _ ->
      session.send ("info string unknown command " ^ cmd);
      `Continue
  with
  | Failure msg | Invalid_argument msg ->
    session.send ("info string error: " ^ msg);
    `Continue
;;

(** Wait for a running search to finish (for tests) *)
let wait session =
  Option.iter Thread.join session.search;
  session.search <- None
;;

(** Main UCI loop: read commands from stdin until "quit" or end of input *)
let main_loop () =
  let book =
    match Protocol_common.load_book () with
    | Some (book, path) ->
      prerr_endline ("Opening book loaded from " ^ path);
      Some book
    | None -> None
  in
  let session = create_session ?book () in
  let rec loop () =
    match In_channel.input_line stdin with
    | None -> stop session
    | Some line ->
      (match handle_line session line with
       | `Continue -> loop ()
       | `Quit -> ())
  in
  loop ()
;;
