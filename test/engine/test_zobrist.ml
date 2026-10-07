(** Unit tests for Zobrist hashing *)

open Chessml.Engine

let test_zobrist_same_position () =
  let pos1 = Position.default () in
  let pos2 = Position.default () in
  let hash1 = Position.key pos1 in
  let hash2 = Position.key pos2 in
  Alcotest.(check bool) "Same position same hash" true (hash1 = hash2)
;;

let test_zobrist_different_positions () =
  let pos1 = Position.default () in
  let pos2 =
    Position.of_fen "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
  in
  let hash1 = Position.key pos1 in
  let hash2 = Position.key pos2 in
  Alcotest.(check bool) "Different positions different hash" true (hash1 <> hash2)
;;

let test_zobrist_move_and_unmove () =
  let game = Game.default () in
  let moves = Game.legal_moves game in
  match moves with
  | mv :: _ ->
    let hash1 = Position.key (Game.position game) in
    let game2 = Game.make_move game mv in
    let hash2 = Position.key (Game.position game2) in
    Alcotest.(check bool) "Hash changes after move" true (hash1 <> hash2)
  | [] -> Alcotest.fail "Should have moves"
;;

(* Test vectors from the Polyglot book format specification *)
let test_polyglot_keys () =
  let check name moves expected =
    let game =
      List.fold_left
        (fun g s ->
           match Game.find_move g s with
           | Some mv -> Game.make_move g mv
           | None -> Alcotest.failf "%s: illegal move %s" name s)
        (Game.default ())
        moves
    in
    Alcotest.(check string)
      name
      (Printf.sprintf "%016Lx" expected)
      (Printf.sprintf "%016Lx" (Position.key (Game.position game)))
  in
  check "startpos" [] 0x463b96181691fc9cL;
  check "e4" [ "e2e4" ] 0x823c9b50fd114196L;
  check "e4 d5" [ "e2e4"; "d7d5" ] 0x0756b94461c50fb0L;
  check "e4 d5 e5" [ "e2e4"; "d7d5"; "e4e5" ] 0x662fafb965db29d4L;
  check "e4 d5 e5 f5" [ "e2e4"; "d7d5"; "e4e5"; "f7f5" ] 0x22a48b5a8e47ff78L;
  check "e4 d5 e5 f5 Ke2" [ "e2e4"; "d7d5"; "e4e5"; "f7f5"; "e1e2" ] 0x652a607ca3f242c1L;
  check
    "e4 d5 e5 f5 Ke2 Kf7"
    [ "e2e4"; "d7d5"; "e4e5"; "f7f5"; "e1e2"; "e8f7" ]
    0x00fdd303c946bdd9L;
  check "a4 b5 h4 b4 c4" [ "a2a4"; "b7b5"; "h2h4"; "b5b4"; "c2c4" ] 0x3c8123ea7b067637L;
  check
    "a4 b5 h4 b4 c4 bxc3 Ra3"
    [ "a2a4"; "b7b5"; "h2h4"; "b5b4"; "c2c4"; "b4c3"; "a1a3" ]
    0x5c3f9b829b279560L
;;

let () =
  let open Alcotest in
  run
    "Zobrist"
    [ ( "hashing"
      , [ test_case "Same position" `Quick test_zobrist_same_position
        ; test_case "Different positions" `Quick test_zobrist_different_positions
        ; test_case "Move changes hash" `Quick test_zobrist_move_and_unmove
        ; test_case "Polyglot test vectors" `Quick test_polyglot_keys
        ] )
    ]
;;
