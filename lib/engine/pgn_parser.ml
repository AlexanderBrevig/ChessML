(** PGN Parser - Parse Portable Game Notation chess files

    A tokenizer over the whole text handles tag pairs, comments ({...} and ;
    to end of line, possibly spanning lines), recursive variations (skipped),
    numeric annotation glyphs, move numbers ("12." and "12...") and results.
    A game ends at its result token, or when tag pairs follow its moves.
*)

open Chessml_core

type game =
  { event : string option
  ; site : string option
  ; date : string option
  ; round : string option
  ; white : string option
  ; black : string option
  ; result : string option
  ; fen : string option
  ; moves : string list
  }

(** {1 SAN moves} *)

let piece_of_char = function
  | 'K' -> Some Types.King
  | 'Q' -> Some Types.Queen
  | 'R' -> Some Types.Rook
  | 'B' -> Some Types.Bishop
  | 'N' -> Some Types.Knight
  | _ -> None
;;

(** Resolve a SAN move ("e4", "Nbd7", "exd8=Q", "O-O+", "R1a3") to the legal move
    it denotes in [pos] *)
let parse_san_move pos san =
  (* Drop check, mate and annotation suffixes, and the promotion '=' *)
  let san =
    String.to_seq (String.trim san)
    |> Seq.filter (fun c -> not (List.mem c [ '+'; '#'; '!'; '?'; '=' ]))
    |> String.of_seq
  in
  let legal = Movegen.generate_moves pos in
  match san with
  | "O-O" | "0-0" -> List.find_opt (fun mv -> Move.kind mv = Move.ShortCastle) legal
  | "O-O-O" | "0-0-0" -> List.find_opt (fun mv -> Move.kind mv = Move.LongCastle) legal
  | _ when String.length san < 2 -> None
  | _ ->
    let n = String.length san in
    (* Trailing promotion piece, as in "e8Q" or "exd8N" *)
    let promotion, body =
      match piece_of_char san.[n - 1] with
      | Some kind when n >= 3 -> Some kind, String.sub san 0 (n - 1)
      | _ -> None, san
    in
    let n = String.length body in
    let kind, start =
      match piece_of_char body.[0] with
      | Some kind -> kind, 1
      | None -> Types.Pawn, 0
    in
    (match Square.of_uci (String.sub body (n - 2) 2) with
     | exception Invalid_argument _ -> None
     | to_sq ->
       (* Disambiguation: file and/or rank of the origin, ignoring 'x' *)
       let hints =
         String.to_seq (String.sub body start (max 0 (n - 2 - start)))
         |> Seq.filter (( <> ) 'x')
         |> List.of_seq
       in
       let matches_hint from c =
         if c >= 'a' && c <= 'h'
         then from mod 8 = Char.code c - Char.code 'a'
         else if c >= '1' && c <= '8'
         then from / 8 = Char.code c - Char.code '1'
         else false
       in
       let candidates =
         List.filter
           (fun mv ->
              Move.to_square mv = to_sq
              && Move.promotion mv = promotion
              && (match Position.piece_at pos (Move.from mv) with
                  | Some p -> p.kind = kind
                  | None -> false)
              && List.for_all (matches_hint (Move.from mv)) hints)
           legal
       in
       (match candidates with
        | [ mv ] -> Some mv
        | _ -> None))
;;

(** {1 Tokenizer} *)

type token =
  | Tag of string * string
  | San of string
  | Result of string

let is_result = function
  | "1-0" | "0-1" | "1/2-1/2" | "*" -> true
  | _ -> false
;;

(** Split PGN text into tags, SAN moves and results *)
let tokenize text =
  let n = String.length text in
  let tokens = ref [] in
  let add t = tokens := t :: !tokens in
  let rec skip_until i c =
    if i >= n || text.[i] = c then i + 1 else skip_until (i + 1) c
  in
  let rec skip_variation i depth =
    if i >= n
    then i
    else (
      match text.[i] with
      | '(' -> skip_variation (i + 1) (depth + 1)
      | ')' -> if depth = 1 then i + 1 else skip_variation (i + 1) (depth - 1)
      | '{' -> skip_variation (skip_until (i + 1) '}') depth
      | _ -> skip_variation (i + 1) depth)
  in
  let is_space c = c = ' ' || c = '\t' || c = '\n' || c = '\r' in
  let rec word_end i =
    if i >= n || is_space text.[i] || String.contains "{}()[];" text.[i]
    then i
    else word_end (i + 1)
  in
  let rec go i =
    if i < n
    then (
      match text.[i] with
      | c when is_space c -> go (i + 1)
      | '{' -> go (skip_until (i + 1) '}')
      | ';' | '%' -> go (skip_until (i + 1) '\n')
      | '(' -> go (skip_variation i 0)
      | '[' ->
        let close = skip_until (i + 1) ']' in
        let inner = String.sub text (i + 1) (max 0 (close - i - 2)) |> String.trim in
        (match String.index_opt inner ' ' with
         | Some sp ->
           let value = String.trim (String.sub inner sp (String.length inner - sp)) in
           let value =
             if String.length value >= 2 && value.[0] = '"'
             then String.sub value 1 (String.length value - 2)
             else value
           in
           add (Tag (String.sub inner 0 sp, value))
         | None -> ());
        go close
      | _ ->
        let j = word_end i in
        let word = String.sub text i (max 1 (j - i)) in
        if is_result word
        then add (Result word)
        else if word.[0] = '$'
        then ()
        else (
          (* Strip a leading move number: "12.", "12...", "12.e4" *)
          let k = ref 0 in
          while !k < String.length word && word.[!k] >= '0' && word.[!k] <= '9' do
            incr k
          done;
          if !k < String.length word && word.[!k] = '.'
          then (
            while !k < String.length word && word.[!k] = '.' do
              incr k
            done;
            if !k < String.length word
            then add (San (String.sub word !k (String.length word - !k))))
          else if !k = 0
          then add (San word));
        go (max j (i + 1)))
  in
  go 0;
  List.rev !tokens
;;

(** {1 Games} *)

let make_game tags moves result =
  let tag name =
    match List.assoc_opt name tags with
    | Some "" | None -> None
    | v -> v
  in
  { event = tag "Event"
  ; site = tag "Site"
  ; date = tag "Date"
  ; round = tag "Round"
  ; white = tag "White"
  ; black = tag "Black"
  ; result =
      (match result with
       | Some _ -> result
       | None -> tag "Result")
  ; fen = tag "FEN"
  ; moves = List.rev moves
  }
;;

(** Parse all games in PGN text *)
let parse_string text =
  let rec loop tags moves games = function
    | [] ->
      List.rev
        (if tags = [] && moves = [] then games else make_game tags moves None :: games)
    | Tag (k, v) :: rest when moves <> [] ->
      (* Tags after moves: the previous game had no result token *)
      loop [ k, v ] [] (make_game tags moves None :: games) rest
    | Tag (k, v) :: rest -> loop ((k, v) :: tags) moves games rest
    | San m :: rest -> loop tags (m :: moves) games rest
    | Result r :: rest -> loop [] [] (make_game tags moves (Some r) :: games) rest
  in
  loop [] [] [] (tokenize text)
;;

(** Parse all games in a PGN file *)
let parse_file filename =
  parse_string (In_channel.with_open_bin filename In_channel.input_all)
;;

(** Starting position of a game (its FEN tag, or the standard position) *)
let start_position game =
  match game.fen with
  | Some fen -> Position.of_fen fen
  | None -> Position.default ()
;;

(** The game's moves, up to the first one that cannot be resolved *)
let game_to_moves game =
  let rec go pos acc = function
    | [] -> List.rev acc
    | san :: rest ->
      (match parse_san_move pos san with
       | Some mv -> go (Position.make_move pos mv) (mv :: acc) rest
       | None -> List.rev acc)
  in
  go (start_position game) [] game.moves
;;
