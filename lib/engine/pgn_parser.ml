(** PGN Parser - Parse Portable Game Notation chess files *)

open Chessml_core

(** A parsed game with metadata and moves *)
type game =
  { event : string option
  ; site : string option
  ; date : string option
  ; round : string option
  ; white : string option
  ; black : string option
  ; result : string option
  ; moves : string list
  }

(** Parse a SAN move string to a Move.t given a position *)
let parse_san_move pos san_str =
  let san = String.trim san_str in
  let san_clean =
    san
    |> fun s ->
    Str.global_replace (Str.regexp "[+#?!]") "" s
    |> fun s -> Str.global_replace (Str.regexp "=") "" s
  in
  let legal_moves = Movegen.generate_moves pos in
  (* Handle castling *)
  if san_clean = "O-O" || san_clean = "0-0"
  then List.find_opt (fun mv -> Move.kind mv = Move.ShortCastle) legal_moves
  else if san_clean = "O-O-O" || san_clean = "0-0-0"
  then List.find_opt (fun mv -> Move.kind mv = Move.LongCastle) legal_moves
  else (
    (* Parse algebraic notation *)
    let len = String.length san_clean in
    if len < 2
    then None
    else (
      (* Extract destination square (always last 2 chars) *)
      let to_str = String.sub san_clean (len - 2) 2 in
      try
        let to_sq = Square.of_uci to_str in
        (* Determine piece type and disambiguation from first part *)
        let piece_char = String.get san_clean 0 in
        let is_pawn_move = piece_char >= 'a' && piece_char <= 'h' in
        let is_pawn_capture = is_pawn_move && len >= 3 && String.get san_clean 1 = 'x' in
        let piece_type, disambig_start =
          if is_pawn_move
          then
            (* Pawn move or capture *)
            if is_pawn_capture
            then
              (* Pawn capture like "exd5" - disambig is the file *)
              Some Types.Pawn, 0
            else
              (* Simple pawn move like "e4" *)
              Some Types.Pawn, -1
          else (
            (* Piece move *)
            let pt =
              match piece_char with
              | 'K' -> Some Types.King
              | 'Q' -> Some Types.Queen
              | 'R' -> Some Types.Rook
              | 'B' -> Some Types.Bishop
              | 'N' -> Some Types.Knight
              | _ -> None
            in
            pt, 1)
        in
        (* Extract disambiguation (anything between piece and destination) *)
        let disambig =
          if disambig_start >= 0 && disambig_start < len - 2
          then (
            let middle_len = len - 2 - disambig_start in
            if middle_len > 0
            then (
              let middle = String.sub san_clean disambig_start middle_len in
              (* Remove 'x' for captures *)
              let middle_clean = Str.global_replace (Str.regexp "x") "" middle in
              if String.length middle_clean > 0 then Some middle_clean else None)
            else None)
          else None
        in
        (* Find matching legal move *)
        List.find_opt
          (fun mv ->
             Move.to_square mv = to_sq
             && (match piece_type with
                 | None -> true
                 | Some pt ->
                   let from_sq = Move.from mv in
                   let piece = Position.piece_at pos from_sq in
                   (match piece with
                    | Some p -> Types.Piece.kind p = pt
                    | None -> false))
             &&
             match disambig with
             | None -> true
             | Some dis ->
               let from_sq = Move.from mv in
               let from_uci = Square.to_uci from_sq in
               String.contains from_uci (String.get dis 0))
          legal_moves
      with
      | _ -> None))
;;

(** Parse header line *)
let parse_header line =
  if String.starts_with ~prefix:"[" line && String.ends_with ~suffix:"]" line
  then (
    let content = String.sub line 1 (String.length line - 2) in
    match String.split_on_char ' ' content with
    | tag :: rest ->
      let value = String.concat " " rest |> String.trim in
      let value =
        if String.starts_with ~prefix:"\"" value && String.ends_with ~suffix:"\"" value
        then String.sub value 1 (String.length value - 2)
        else value
      in
      Some (tag, value)
    | [] -> None)
  else None
;;

(** Extract moves from a game text line *)
let extract_moves line =
  (* Remove comments in curly braces *)
  let line_no_comments = Str.global_replace (Str.regexp "{[^}]*}") "" line in
  let tokens = Str.split (Str.regexp "[ \t\n]+") (String.trim line_no_comments) in
  List.filter_map
    (fun token ->
       if
         token = ""
         || token = "*"
         || token = "1-0"
         || token = "0-1"
         || token = "1/2-1/2"
         || String.starts_with ~prefix:"[" token
         || String.starts_with ~prefix:"(" token
       then None
       else if String.contains token '.'
       then (
         (* Extract move from token like "1.e4" or "23...Nf6" *)
         match String.split_on_char '.' token with
         | _ :: move :: _ when move <> "" -> Some move
         | _ -> None)
       else Some token)
    tokens
;;

(** Parse a single PGN game from lines *)
let parse_game_lines lines =
  let headers = ref [] in
  let moves = ref [] in
  let in_moves = ref false in
  List.iter
    (fun line ->
       let line = String.trim line in
       if line = ""
       then ()
       else if String.starts_with ~prefix:"[" line
       then (
         match parse_header line with
         | Some (tag, value) -> headers := (tag, value) :: !headers
         | None -> ())
       else (
         in_moves := true;
         let game_moves = extract_moves line in
         moves := !moves @ game_moves))
    lines;
  let get_header tag =
    List.assoc_opt tag !headers
    |> Option.map String.trim
    |> function
    | Some "" -> None
    | x -> x
  in
  { event = get_header "Event"
  ; site = get_header "Site"
  ; date = get_header "Date"
  ; round = get_header "Round"
  ; white = get_header "White"
  ; black = get_header "Black"
  ; result = get_header "Result"
  ; moves = !moves
  }
;;

(** Parse multiple games from a PGN file *)
let parse_file filename =
  let ic = open_in filename in
  let games = ref [] in
  let current_game_lines = ref [] in
  let in_game = ref false in
  let finish_current_game () =
    if !current_game_lines <> []
    then (
      let game = parse_game_lines (List.rev !current_game_lines) in
      games := game :: !games;
      current_game_lines := [];
      in_game := false)
  in
  try
    while true do
      let line = input_line ic in
      let line_trimmed = String.trim line in
      if line_trimmed = ""
      then
        (* Skip empty lines *)
        ()
      else if String.starts_with ~prefix:"[" line_trimmed
      then (
        (* Header line *)
        (* If we were already in a game and see a new header, finish previous game *)
        if
          !in_game
          && !current_game_lines <> []
          && not
               (List.exists
                  (fun l -> String.starts_with ~prefix:"[" l)
                  !current_game_lines)
        then finish_current_game ();
        current_game_lines := line :: !current_game_lines;
        in_game := true)
      else (
        (* Move line or other content *)
        current_game_lines := line :: !current_game_lines;
        in_game := true;
        (* If this line contains a game termination marker, finish the game *)
        if
          String.contains line_trimmed '*'
          || (String.contains line_trimmed '1' && String.contains line_trimmed '-')
        then finish_current_game ())
    done;
    []
  with
  | End_of_file ->
    close_in ic;
    (* Process last game if any *)
    finish_current_game ();
    List.rev !games
  | ex ->
    close_in ic;
    Printf.eprintf "Error parsing PGN file %s: %s\n" filename (Printexc.to_string ex);
    List.rev !games
;;

(** Convert a game's SAN moves to actual Move.t list *)
let game_to_moves game =
  let rec process_moves pos san_list acc =
    match san_list with
    | [] -> List.rev acc
    | san :: rest ->
      (match parse_san_move pos san with
       | None ->
         (* Stop on first unparseable move *)
         List.rev acc
       | Some mv ->
         let new_pos = Position.make_move pos mv in
         process_moves new_pos rest (mv :: acc))
  in
  let start_pos = Position.default () in
  process_moves start_pos game.moves []
;;

(** Get statistics about a PGN file *)
let file_stats filename =
  let games = parse_file filename in
  let total_games = List.length games in
  let total_moves = List.fold_left (fun acc g -> acc + List.length g.moves) 0 games in
  Printf.printf "File: %s\n" (Filename.basename filename);
  Printf.printf "  Games: %d\n" total_games;
  Printf.printf "  Total moves: %d\n" total_moves;
  if total_games > 0
  then
    Printf.printf
      "  Avg moves/game: %.1f\n"
      (float_of_int total_moves /. float_of_int total_games);
  ()
;;
