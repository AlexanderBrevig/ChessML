(** Perft: count leaf nodes of the legal move tree against published values *)

open Chessml

let rec perft pos depth =
  if depth = 0
  then 1
  else
    List.fold_left
      (fun acc mv -> acc + perft (Position.make_move pos mv) (depth - 1))
      0
      (Movegen.generate_moves pos)
;;

let perft_case name fen depth expected =
  Alcotest.test_case name `Quick (fun () ->
    Alcotest.(check int) name expected (perft (Position.of_fen fen) depth))
;;

let moves fen =
  Movegen.generate_moves (Position.of_fen fen)
  |> List.map Move.to_uci
  |> List.sort compare
;;

let test_en_passant_pins () =
  (* Capturing en passant would expose the king along the rank *)
  Alcotest.(check bool)
    "horizontal ep pin"
    false
    (List.mem "b5c6" (moves "8/8/8/KPp4r/8/8/8/7k w - c6 0 1"));
  (* ... or along the diagonal *)
  Alcotest.(check bool)
    "diagonal ep pin"
    false
    (List.mem "c5d6" (moves "8/5bk1/8/2Pp4/8/1K6/8/8 w - d6 0 1"));
  (* En passant may capture the pawn that gives check *)
  Alcotest.(check bool)
    "ep captures checking pawn"
    true
    (List.mem "e4d3" (moves "8/8/8/2k5/3Pp3/8/8/4K3 b - d3 0 1"))
;;

let test_castling_needs_rook () =
  let m = moves "r3k2r/8/8/8/8/8/8/4K3 w KQkq - 0 1" in
  Alcotest.(check bool) "no O-O without rook" false (List.mem "e1g1" m);
  Alcotest.(check bool) "no O-O-O without rook" false (List.mem "e1c1" m)
;;

let () =
  Alcotest.run
    "Perft"
    [ ( "perft"
      , [ perft_case "startpos d4" Position.fen_startpos 4 197281
        ; perft_case
            "kiwipete d3"
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
            3
            97862
        ; perft_case "position 3 d5" "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1" 5 674624
        ; perft_case
            "position 4 d3"
            "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1"
            3
            9467
        ; perft_case
            "position 5 d3"
            "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8"
            3
            62379
        ; perft_case
            "position 6 d3"
            "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10"
            3
            89890
        ] )
    ; ( "special"
      , [ Alcotest.test_case "En passant pins" `Quick test_en_passant_pins
        ; Alcotest.test_case "Castling needs rook" `Quick test_castling_needs_rook
        ] )
    ]
;;
