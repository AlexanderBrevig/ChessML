(** XBoard - XBoard/WinBoard protocol implementation (aka CECP)

    Supports protocol version 2: feature negotiation, force/go/playother, undo and
    remove, setboard, level/st/sd/time/otim time controls, thinking output (post)
    and result claims. The engine thinks synchronously; a malformed command gets
    an "Error" reply and does not end the session.

    Reference: https://www.gnu.org/software/xboard/engine-intf.html
*)

open Chessml_core
open Chessml_engine

type session =
  { mutable game : Game.t
  ; mutable undo_stack : Game.t list
  ; mutable engine_side : Types.color option (** [None] in force mode *)
  ; mutable game_over : bool
  ; mutable my_time_cs : int option
  ; mutable increment_ms : int
  ; mutable moves_per_control : int option
  ; mutable fixed_time_ms : int option
  ; mutable depth_limit : int option
  ; mutable post : bool
  ; book : Opening_book.book option
  ; send : string -> unit
  }

let create_session ?(send = print_endline) ?book () =
  { game = Game.default ()
  ; undo_stack = []
  ; engine_side = Some Black
  ; game_over = false
  ; my_time_cs = None
  ; increment_ms = 0
  ; moves_per_control = None
  ; fixed_time_ms = None
  ; depth_limit = None
  ; post = false
  ; book
  ; send =
      (fun line ->
        send line;
        flush stdout)
  }
;;

(** Claim the result if the game has ended; returns true if it has *)
let claim_result session =
  let game = session.game in
  let pos = Game.position game in
  let result =
    if Game.legal_moves game = []
    then
      if Movegen.in_check pos
      then
        Some
          (if Position.side_to_move pos = White
           then "0-1 {Black mates}"
           else "1-0 {White mates}")
      else Some "1/2-1/2 {Stalemate}"
    else if Position.halfmove pos >= 100
    then Some "1/2-1/2 {Draw by fifty move rule}"
    else if Game.is_threefold_repetition game
    then Some "1/2-1/2 {Draw by repetition}"
    else if Position.has_insufficient_material pos
    then Some "1/2-1/2 {Insufficient material}"
    else None
  in
  Option.iter
    (fun r ->
       session.game_over <- true;
       session.send r)
    result;
  Option.is_some result
;;

let play session mv =
  session.undo_stack <- session.game :: session.undo_stack;
  session.game <- Game.make_move session.game mv
;;

(** XBoard scores mates as 100000 + moves to mate *)
let xboard_score score =
  if Score.is_mate score
  then (
    let n = Score.mate_in_moves score in
    if n > 0 then 100000 + n else -100000 + n)
  else score
;;

let think_time_ms session =
  match session.fixed_time_ms, session.my_time_cs with
  | Some ms, _ -> ms
  | None, Some cs ->
    let moves_to_go =
      Option.map
        (fun mps ->
           let played = (Position.fullmove (Game.position session.game) - 1) mod mps in
           mps - played)
        session.moves_per_control
    in
    Protocol_common.time_budget_ms
      ~remaining_ms:(cs * 10)
      ~increment_ms:session.increment_ms
      ~moves_to_go
  | None, None -> 5000
;;

(** Search (or probe the book) and play the engine's move *)
let think session =
  if not (session.game_over || claim_result session)
  then (
    let pos = Game.position session.game in
    let start = Unix.gettimeofday () in
    let mv =
      match Protocol_common.book_move session.book pos with
      | Some mv -> Some mv
      | None ->
        let on_iteration (r : Search.search_result) =
          if session.post
          then
            session.send
              (Printf.sprintf
                 "%d %d %d %Ld %s"
                 r.depth
                 (xboard_score r.score)
                 (int_of_float ((Unix.gettimeofday () -. start) *. 100.0))
                 r.nodes
                 (String.concat " " (List.map Move.to_uci r.pv)))
        in
        (Search.find_best_move
           ~verbose:false
           ~max_time_ms:(think_time_ms session)
           ~on_iteration
           session.game
           (Option.value session.depth_limit ~default:(Config.get_max_search_depth ())))
          .best_move
    in
    Option.iter
      (fun mv ->
         session.send ("move " ^ Move.to_uci mv);
         play session mv;
         ignore (claim_result session))
      mv)
;;

(** Apply the opponent's move, then reply if the engine is to move *)
let user_move session move_str =
  match Game.find_move session.game move_str with
  | None -> session.send ("Illegal move: " ^ move_str)
  | Some mv ->
    play session mv;
    if not (claim_result session)
    then (
      let stm = Position.side_to_move (Game.position session.game) in
      if session.engine_side = Some stm then think session)
;;

let undo session n =
  for _ = 1 to n do
    match session.undo_stack with
    | previous :: rest ->
      session.game <- previous;
      session.undo_stack <- rest;
      session.game_over <- false
    | [] -> ()
  done
;;

(** Minutes ("5") or minutes:seconds ("0:30") to milliseconds *)
let parse_base_time s =
  match String.split_on_char ':' s with
  | [ m ] -> int_of_string m * 60_000
  | [ m; sec ] -> (int_of_string m * 60_000) + (int_of_string sec * 1000)
  | _ -> failwith ("bad time " ^ s)
;;

let send_features session =
  let option_feature (o : Protocol_common.engine_option) =
    match o.kind with
    | Spin { default; min; max } ->
      Printf.sprintf "feature option=\"%s -spin %d %d %d\"" o.name default min max
    | Check default ->
      Printf.sprintf "feature option=\"%s -check %d\"" o.name (if default then 1 else 0)
  in
  session.send "feature done=0";
  session.send
    "feature myname=\"ChessML\" ping=1 setboard=1 usermove=1 playother=1 colors=0 \
     sigint=0 sigterm=0 reuse=1 analyze=0";
  List.iter (fun o -> session.send (option_feature o)) Protocol_common.options;
  session.send "feature done=1"
;;

let is_coordinate_move s =
  let n = String.length s in
  (n = 4 || n = 5)
  && s.[0] >= 'a'
  && s.[0] <= 'h'
  && s.[1] >= '1'
  && s.[1] <= '8'
  && s.[2] >= 'a'
  && s.[2] <= 'h'
  && s.[3] >= '1'
  && s.[3] <= '8'
;;

(** Handle one input line. Returns [`Quit] after "quit". *)
let handle_line session line =
  let tokens = String.split_on_char ' ' (String.trim line) |> List.filter (( <> ) "") in
  let side_to_move () = Position.side_to_move (Game.position session.game) in
  try
    (match tokens with
     | [] | "xboard" :: _ -> ()
     | "protover" :: _ -> send_features session
     | "new" :: _ ->
       Search.new_game ();
       session.game <- Game.default ();
       session.undo_stack <- [];
       session.engine_side <- Some Black;
       session.game_over <- false;
       session.depth_limit <- None
     | "force" :: _ -> session.engine_side <- None
     | "go" :: _ ->
       session.engine_side <- Some (side_to_move ());
       think session
     | "playother" :: _ ->
       session.engine_side <- Some (Types.Color.opponent (side_to_move ()))
     | "usermove" :: mv :: _ -> user_move session mv
     | (("O-O" | "0-0" | "O-O-O" | "0-0-0") as mv) :: _ -> user_move session mv
     | mv :: _ when is_coordinate_move mv -> user_move session mv
     | "undo" :: _ -> undo session 1
     | "remove" :: _ -> undo session 2
     | "setboard" :: fen ->
       session.game <- Game.of_fen (String.concat " " fen);
       session.undo_stack <- [];
       session.game_over <- false
     | "level" :: mps :: base :: inc :: _ ->
       let mps = int_of_string mps in
       session.moves_per_control <- (if mps > 0 then Some mps else None);
       session.my_time_cs <- Some (parse_base_time base / 10);
       session.increment_ms <- int_of_float (float_of_string inc *. 1000.0);
       session.fixed_time_ms <- None
     | "st" :: secs :: _ ->
       session.fixed_time_ms <- Some (int_of_float (float_of_string secs *. 1000.0))
     | "sd" :: depth :: _ -> session.depth_limit <- Some (int_of_string depth)
     | "time" :: cs :: _ -> session.my_time_cs <- Some (int_of_string cs)
     | "post" :: _ -> session.post <- true
     | "nopost" :: _ -> session.post <- false
     | "ping" :: n :: _ -> session.send ("pong " ^ n)
     | "result" :: _ -> session.game_over <- true
     | "option" :: rest ->
       (match String.index_opt (String.concat " " rest) '=' with
        | Some i ->
          let s = String.concat " " rest in
          (match
             Protocol_common.set_option
               (String.sub s 0 i)
               (String.sub s (i + 1) (String.length s - i - 1))
           with
           | Ok () -> ()
           | Error msg -> session.send ("Error (" ^ msg ^ "): option"))
        | None -> session.send "Error (expected name=value): option")
     | ( "accepted"
       | "rejected"
       | "otim"
       | "hard"
       | "easy"
       | "random"
       | "computer"
       | "name"
       | "rating"
       | "ics"
       | "draw"
       | "?"
       | "."
       | "hint"
       | "bk" )
       :: _ -> ()
     | "quit" :: _ -> raise Exit
     | cmd :: _ -> session.send ("Error (unknown command): " ^ cmd));
    `Continue
  with
  | Exit -> `Quit
  | Failure msg | Invalid_argument msg ->
    session.send (Printf.sprintf "Error (%s): %s" msg (String.trim line));
    `Continue
;;

(** Main XBoard loop: read commands from stdin until "quit" or end of input *)
let main_loop () =
  Sys.set_signal Sys.sigint Sys.Signal_ignore;
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let book = Option.map fst (Protocol_common.load_book ()) in
  let session = create_session ?book () in
  let rec loop () =
    match In_channel.input_line stdin with
    | None -> ()
    | Some line ->
      (match handle_line session line with
       | `Continue -> loop ()
       | `Quit -> ())
  in
  loop ()
;;
