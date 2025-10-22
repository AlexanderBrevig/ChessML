(* Demonstrate move ordering with MVV-LVA and human-like heuristics *)

open Chessml

let show_move_ordering fen =
  Printf.printf "\n=== Move Ordering Demo ===\n";
  Printf.printf "FEN: %s\n\n" fen;
  let game = Game.of_fen fen in
  let pos = Game.position game in
  let moves = Movegen.generate_moves pos in
  Printf.printf "Unordered moves (%d total):\n" (List.length moves);
  List.iteri
    (fun i mv ->
       if i < 10
       then (* Show first 10 *)
         Printf.printf "  %s\n" (Move.to_uci mv))
    moves;
  if List.length moves > 10
  then Printf.printf "  ... and %d more\n" (List.length moves - 10);
  (* This is internal to Search module, so we'll create our own scoring *)
  let score_move mv =
    let is_check = ref false in
    (try
       let new_pos = Position.make_move pos mv in
       let opponent = Types.Color.opponent (Position.side_to_move pos) in
       let king_sq =
         match Movegen.find_king new_pos opponent with
         | Some sq -> sq
         | None -> 0
       in
       let side = Position.side_to_move new_pos in
       let attackers = Movegen.compute_attackers_to new_pos king_sq side in
       is_check := not (Bitboard.is_empty attackers)
     with
     | _ -> ());
    let mvv_lva =
      if Move.is_capture mv
      then (
        let to_sq = Move.to_square mv in
        let from_sq = Move.from mv in
        let victim =
          match Position.piece_at pos to_sq with
          | Some p -> Eval.piece_value p
          | None -> 100 (* En passant *)
        in
        let attacker =
          match Position.piece_at pos from_sq with
          | Some p -> Eval.piece_kind_value p.Types.kind
          | None -> 0
        in
        (victim * 10) - attacker)
      else 0
    in
    if !is_check
    then 10000 + mvv_lva, "CHECK"
    else if Move.is_capture mv
    then 1000 + mvv_lva, "CAPTURE (MVV-LVA)"
    else 50, "QUIET"
  in
  let scored = List.map (fun mv -> mv, score_move mv) moves in
  let sorted = List.sort (fun (_, (s1, _)) (_, (s2, _)) -> compare s2 s1) scored in
  Printf.printf "\nOrdered moves (showing top 15):\n";
  List.iteri
    (fun i (mv, (score, category)) ->
       if i < 15
       then
         Printf.printf "  %2d. %-6s  [%5d] %s\n" (i + 1) (Move.to_uci mv) score category)
    sorted;
  if List.length sorted > 15
  then Printf.printf "  ... and %d more quiet moves\n" (List.length sorted - 15)
;;

let () =
  Printf.printf "=== Advanced Move Ordering Demonstration ===\n";
  Printf.printf "Priority: Checks > Captures (MVV-LVA) > Threats > Quiet\n";
  (* Position with mix of checks, captures, and quiet moves *)
  show_move_ordering
    "r1bqk2r/pppp1ppp/2n2n2/2b1p3/2B1P3/3P1N2/PPP2PPP/RNBQK2R w KQkq - 0 5";
  (* Position with many captures available *)
  show_move_ordering
    "r1bqkb1r/ppp2ppp/2np1n2/4p3/2B1P3/3P1N2/PPP2PPP/RNBQK2R w KQkq - 0 5";
  (* Position where checking is powerful *)
  show_move_ordering "6k1/5ppp/4Q3/8/8/8/5PPP/6K1 w - - 0 1"
;;
