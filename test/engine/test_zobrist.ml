(** Unit tests for Zobrist hashing *)

open Chessml.Engine

let test_zobrist_same_position () =
  let pos1 = Position.default () in
  let pos2 = Position.default () in
  let hash1 = Zobrist.compute pos1 in
  let hash2 = Zobrist.compute pos2 in
  Alcotest.(check bool) "Same position same hash" true (hash1 = hash2)
;;

let test_zobrist_different_positions () =
  let pos1 = Position.default () in
  let pos2 =
    Position.of_fen "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
  in
  let hash1 = Zobrist.compute pos1 in
  let hash2 = Zobrist.compute pos2 in
  Alcotest.(check bool) "Different positions different hash" true (hash1 <> hash2)
;;

let test_zobrist_move_and_unmove () =
  let game = Game.default () in
  let moves = Game.legal_moves game in
  match moves with
  | mv :: _ ->
    let hash1 = Zobrist.compute (Game.position game) in
    let game2 = Game.make_move game mv in
    let hash2 = Zobrist.compute (Game.position game2) in
    Alcotest.(check bool) "Hash changes after move" true (hash1 <> hash2)
  | [] -> Alcotest.fail "Should have moves"
;;

let () =
  let open Alcotest in
  run
    "Zobrist"
    [ ( "hashing"
      , [ test_case "Same position" `Quick test_zobrist_same_position
        ; test_case "Different positions" `Quick test_zobrist_different_positions
        ; test_case "Move changes hash" `Quick test_zobrist_move_and_unmove
        ] )
    ]
;;
