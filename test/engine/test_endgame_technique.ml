(** The engine must actually win basic endgames from random positions *)

let check_mates name pieces ~n ~depth ~max_plies =
  let mates, stalemates, draws, timeouts, avg =
    Endgame_harness.run ~seed:42 ~n ~depth ~max_plies pieces
  in
  Printf.printf "%s: %d/%d mates, average %.1f plies\n" name mates n avg;
  Alcotest.(check (list int))
    (name ^ ": stalemates, draws, timeouts")
    [ 0; 0; 0 ]
    [ stalemates; draws; timeouts ];
  Alcotest.(check int) (name ^ ": mates") n mates
;;

(* Textbook K+P vs K positions; expected results checked against Syzygy *)
let test_kpk_table () =
  List.iter
    (fun (fen, expected) ->
       let pos = Chessml.Position.of_fen fen in
       let score = Chessml.Eval.evaluate pos in
       let white_wins =
         if Chessml.Position.side_to_move pos = White then score > 0 else score < 0
       in
       Alcotest.(check bool) fen expected white_wins)
    [ "4k3/8/4K3/4P3/8/8/8/8 w - - 0 1", true (* king in front on the 6th wins *)
    ; "4k3/8/4K3/4P3/8/8/8/8 b - - 0 1", true
    ; "8/8/4k3/8/8/4K3/4P3/8 w - - 0 1", true (* White takes the opposition *)
    ; "8/8/4k3/8/8/4K3/4P3/8 b - - 0 1", false (* Black takes it: draw *)
    ; "8/8/4k3/8/8/8/4P3/4K3 w - - 0 1", false
    ; "k7/8/8/K7/P7/8/8/8 w - - 0 1", false (* rook pawn, defender in the corner *)
    ; "7k/8/8/8/8/8/P7/K7 w - - 0 1", true
    ]
;;

let test_kpk_conversion () =
  let results = Endgame_harness.run_kpk_wins ~seed:5 ~n:5 ~depth:4 ~max_plies:120 () in
  Alcotest.(check int)
    "won K+P vs K positions converted to mate"
    5
    (List.length
       (List.filter
          (function
            | Endgame_harness.Mate _ -> true
            | _ -> false)
          results))
;;

let () =
  Alcotest.run
    "Endgame technique"
    [ ( "lone king"
      , [ Alcotest.test_case "K+Q vs K" `Quick (fun () ->
            check_mates "KQK" [ 'Q' ] ~n:10 ~depth:4 ~max_plies:60)
        ; Alcotest.test_case "K+R vs K" `Quick (fun () ->
            check_mates "KRK" [ 'R' ] ~n:10 ~depth:4 ~max_plies:80)
        ] )
    ; ( "bishop and knight"
      , [ Alcotest.test_case "K+B+N vs K" `Quick (fun () ->
            (* the hardest basic mate: depth 7 and the fifty-move limit *)
            check_mates "KBNK" [ 'B'; 'N' ] ~n:4 ~depth:7 ~max_plies:100)
        ] )
    ; ( "king and pawn"
      , [ Alcotest.test_case "KPK table" `Quick test_kpk_table
        ; Alcotest.test_case "Converts won K+P vs K" `Quick test_kpk_conversion
        ] )
    ]
;;
