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

let () =
  Alcotest.run
    "Endgame technique"
    [ ( "lone king"
      , [ Alcotest.test_case "K+Q vs K" `Quick (fun () ->
            check_mates "KQK" [ 'Q' ] ~n:10 ~depth:4 ~max_plies:60)
        ; Alcotest.test_case "K+R vs K" `Quick (fun () ->
            check_mates "KRK" [ 'R' ] ~n:10 ~depth:4 ~max_plies:80)
        ] )
    ]
;;
