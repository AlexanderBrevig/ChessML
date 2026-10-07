(** Self-play harness for basic endgames: random legal positions for a material
    signature, engine plays both sides, report how often and how fast it mates *)

open Chessml

type outcome =
  | Mate of int (** plies until mate *)
  | Stalemate
  | Draw (** repetition, fifty moves or insufficient material *)
  | Timeout

(** Random legal position with White to have [white] pieces (besides the king)
    and Black only a king. [Random.State] makes the positions reproducible. *)
let random_position rng white_pieces =
  let rec attempt () =
    let squares = Array.make 64 '.' in
    let place c =
      let rec go () =
        (* pawns only on ranks 2-7 *)
        let sq =
          if c = 'P' then 8 + Random.State.int rng 48 else Random.State.int rng 64
        in
        if squares.(sq) = '.' then squares.(sq) <- c else go ()
      in
      go ()
    in
    place 'K';
    place 'k';
    List.iter place white_pieces;
    let rows =
      List.init 8 (fun i ->
        let rank = 7 - i in
        let buf = Buffer.create 8 in
        let empty = ref 0 in
        for file = 0 to 7 do
          match squares.((rank * 8) + file) with
          | '.' -> incr empty
          | c ->
            if !empty > 0 then Buffer.add_string buf (string_of_int !empty);
            empty := 0;
            Buffer.add_char buf c
        done;
        if !empty > 0 then Buffer.add_string buf (string_of_int !empty);
        Buffer.contents buf)
    in
    let side = if Random.State.bool rng then "w" else "b" in
    let fen = String.concat "/" rows ^ " " ^ side ^ " - - 0 1" in
    let pos = Position.of_fen fen in
    let wk = Position.white_king_sq pos
    and bk = Position.black_king_sq pos in
    let other = Types.Color.opponent (Position.side_to_move pos) in
    let other_king = if other = White then wk else bk in
    (* kings apart, side not to move not in check, no white piece en prise to the
       black king (that would be a draw, not a test of technique), and a game left *)
    let hangs =
      Int64.logand (Movegen.king_attacks bk) (Position.get_color_pieces pos White) <> 0L
    in
    if
      Square.distance wk bk <= 1
      || hangs
      || Movegen.is_square_attacked pos other_king (Position.side_to_move pos)
      || Movegen.generate_moves pos = []
    then attempt ()
    else fen
  in
  attempt ()
;;

let play ?(depth = 5) ?max_time_ms ?(max_plies = 120) fen =
  let rec go game plies =
    let pos = Game.position game in
    if Game.legal_moves game = []
    then if Movegen.in_check pos then Mate plies else Stalemate
    else if Game.is_draw game
    then Draw
    else if plies >= max_plies
    then Timeout
    else (
      match (Search.find_best_move ~verbose:false ?max_time_ms game depth).best_move with
      | Some mv -> go (Game.make_move game mv) (plies + 1)
      | None -> Timeout)
  in
  Search.new_game ();
  go (Game.of_fen fen) 0
;;

(** Play [n] random positions; returns (mates, stalemates, draws, timeouts,
    average plies to mate) *)
let run ?depth ?max_time_ms ?max_plies ~seed ~n white_pieces =
  let rng = Random.State.make [| seed |] in
  let results =
    List.init n (fun _ ->
      play ?depth ?max_time_ms ?max_plies (random_position rng white_pieces))
  in
  let count p = List.length (List.filter p results) in
  let mate_plies =
    List.filter_map
      (function
        | Mate p -> Some p
        | _ -> None)
      results
  in
  ( List.length mate_plies
  , count (( = ) Stalemate)
  , count (( = ) Draw)
  , count (( = ) Timeout)
  , if mate_plies = []
    then 0.0
    else
      float_of_int (List.fold_left ( + ) 0 mate_plies)
      /. float_of_int (List.length mate_plies) )
;;

(** Like [run] for K+P vs K, but only positions the KPK table says White wins *)
let run_kpk_wins ?depth ?max_plies ~seed ~n () =
  let rng = Random.State.make [| seed |] in
  let rec winning () =
    let fen = random_position rng [ 'P' ] in
    let pos = Position.of_fen fen in
    let pawn = Option.get (Bitboard.lsb (Position.get_pieces pos White Pawn)) in
    if
      Kpk.wins
        ~strong_king:(Position.white_king_sq pos)
        ~weak_king:(Position.black_king_sq pos)
        ~pawn
        ~strong_to_move:(Position.side_to_move pos = White)
    then fen
    else winning ()
  in
  List.init n (fun _ -> play ?depth ?max_plies (winning ()))
;;
