(** Play from FEN - A utility to observe engine self-play from a given position
    
    Usage: play_from.exe "<FEN string>" [max_moves]
    
    Example: play_from.exe "2R4k/8/8/8/3P4/8/8/K7 b - - 0 1" 20
*)

open Chessml

let play_from_fen fen_string max_moves =
  Printf.printf "╔════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║  Playing from FEN position                                 ║\n";
  Printf.printf "╚════════════════════════════════════════════════════════════╝\n\n";
  Printf.printf "Starting FEN: %s\n\n" fen_string;
  let game = Game.of_fen fen_string in
  let rec play_moves game_state move_num =
    if move_num > max_moves
    then (
      Printf.printf "\n╔════════════════════════════════════════════════════════════╗\n";
      Printf.printf
        "║  Max moves (%d) reached                                    ║\n"
        max_moves;
      Printf.printf "╚════════════════════════════════════════════════════════════╝\n")
    else (
      let pos = Game.position game_state in
      let current_fen = Position.to_fen pos in
      let side = Position.side_to_move pos in
      let side_str = if side = Types.White then "White" else "Black" in
      Printf.printf "═══ Move %d: %s to move ═══\n" move_num side_str;
      Printf.printf "Position: %s\n" current_fen;
      Position.draw_board pos;
      Printf.printf "\n";
      (* Check for game termination *)
      let legal_moves = Game.legal_moves game_state in
      if List.length legal_moves = 0
      then (
        (* No legal moves - check if it's checkmate or stalemate *)
        let king_sq = ref None in
        for sq = 0 to 63 do
          match Position.piece_at pos sq with
          | Some p when p.Types.color = side && p.Types.kind = Types.King ->
            king_sq := Some sq
          | _ -> ()
        done;
        match !king_sq with
        | None ->
          Printf.printf
            "\n╔════════════════════════════════════════════════════════════╗\n";
          Printf.printf "║  ERROR: No king found!                                     ║\n";
          Printf.printf "╚════════════════════════════════════════════════════════════╝\n"
        | Some sq ->
          let opponent = Types.Color.opponent side in
          let attackers = Movegen.compute_attackers_to pos sq opponent in
          if Bitboard.is_empty attackers
          then (
            Printf.printf
              "\n╔════════════════════════════════════════════════════════════╗\n";
            Printf.printf
              "║  STALEMATE - No legal moves, king not in check            ║\n";
            Printf.printf
              "╚════════════════════════════════════════════════════════════╝\n")
          else (
            Printf.printf
              "\n╔════════════════════════════════════════════════════════════╗\n";
            Printf.printf
              "║  CHECKMATE - %s wins!                                 ║\n"
              (if side = Types.White then "Black" else "White");
            Printf.printf
              "╚════════════════════════════════════════════════════════════╝\n"))
      else if Game.is_draw game_state
      then (
        Printf.printf "\n╔════════════════════════════════════════════════════════════╗\n";
        Printf.printf "║  DRAW - Game is drawn                                      ║\n";
        Printf.printf "╚════════════════════════════════════════════════════════════╝\n")
      else (
        (* Search for best move *)
        let result = Search.find_best_move ~verbose:false game_state 10 in
        match result.best_move with
        | None ->
          Printf.printf
            "\n╔════════════════════════════════════════════════════════════╗\n";
          Printf.printf "║  ERROR: Engine found no move!                              ║\n";
          Printf.printf "╚════════════════════════════════════════════════════════════╝\n"
        | Some mv ->
          let move_uci = Move.to_uci mv in
          let from_sq = Move.from mv in
          let to_sq = Move.to_square mv in
          let is_promo = Move.is_promotion mv in
          let promo_str =
            if is_promo
            then (
              match Move.promotion mv with
              | Some Types.Queen -> "=Q"
              | Some Types.Rook -> "=R"
              | Some Types.Bishop -> "=B"
              | Some Types.Knight -> "=N"
              | _ -> "")
            else ""
          in
          Printf.printf "Move: %s%s (score: %+d cp)\n" move_uci promo_str result.score;
          Printf.printf
            "  From: %s, To: %s\n"
            (Square.to_uci from_sq)
            (Square.to_uci to_sq);
          (* Make the move *)
          let new_game = Game.make_move game_state mv in
          let new_fen = Position.to_fen (Game.position new_game) in
          Printf.printf "  Resulting FEN: %s\n\n" new_fen;
          (* Continue playing *)
          play_moves new_game (move_num + 1)))
  in
  play_moves game 1
;;

let () =
  let argc = Array.length Sys.argv in
  if argc < 2
  then (
    Printf.eprintf "Usage: %s \"<FEN string>\" [max_moves]\n" Sys.argv.(0);
    Printf.eprintf "\nExample:\n";
    Printf.eprintf "  %s \"2R4k/8/8/8/3P4/8/8/K7 b - - 0 1\" 20\n" Sys.argv.(0);
    exit 1);
  let fen = Sys.argv.(1) in
  let max_moves = if argc >= 3 then int_of_string Sys.argv.(2) else 30 in
  try play_from_fen fen max_moves with
  | Failure msg ->
    Printf.eprintf "Error: %s\n" msg;
    exit 1
  | Invalid_argument msg ->
    Printf.eprintf "Invalid argument: %s\n" msg;
    exit 1
;;
