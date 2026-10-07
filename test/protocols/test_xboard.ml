(** XBoard protocol tests: drive a session line by line and inspect its output *)

open Chessml

let session () =
  let out = ref [] in
  let s = Xboard.create_session ~send:(fun l -> out := l :: !out) () in
  fun lines ->
    out := [];
    List.iter (fun l -> ignore (Xboard.handle_line s l)) lines;
    List.rev !out
;;

let moves out =
  List.filter_map
    (fun l ->
       if String.starts_with ~prefix:"move " l
       then Some (String.sub l 5 (String.length l - 5))
       else None)
    out
;;

let test_features () =
  let run = session () in
  let out = run [ "xboard"; "protover 2" ] in
  Alcotest.(check bool) "done=1" true (List.mem "feature done=1" out);
  Alcotest.(check bool)
    "sigint=0"
    true
    (List.exists (fun l -> Str.string_match (Str.regexp ".*sigint=0") l 0) out)
;;

let test_reply_and_force () =
  let run = session () in
  let out = run [ "new"; "sd 2"; "usermove e2e4" ] in
  Alcotest.(check int) "engine replies as black" 1 (List.length (moves out));
  let out = run [ "new"; "force"; "usermove e2e4"; "usermove e7e5" ] in
  Alcotest.(check int) "force mode is silent" 0 (List.length (moves out));
  let out = run [ "sd 2"; "go" ] in
  Alcotest.(check int) "go makes the engine move" 1 (List.length (moves out))
;;

let test_illegal_and_underpromotion () =
  let run = session () in
  Alcotest.(check (list string))
    "illegal"
    [ "Illegal move: e2e5" ]
    (run [ "new"; "force"; "usermove e2e5" ]);
  let out =
    run [ "setboard 8/4P3/8/8/8/8/k6p/7K w - - 0 1"; "force"; "usermove e7e8n"; "ping 1" ]
  in
  Alcotest.(check (list string)) "accepted" [ "pong 1" ] out
;;

let test_undo () =
  let run = session () in
  let out =
    run
      [ "new"
      ; "force"
      ; "usermove e2e4"
      ; "undo"
      ; "usermove d2d4"
      ; "remove"
      ; "usermove g1f3"
      ]
  in
  Alcotest.(check (list string)) "undo/remove keep the board in sync" [] out
;;

let test_results () =
  let run = session () in
  let out = run [ "setboard 6k1/5ppp/8/8/8/8/8/R5K1 w - - 0 1"; "sd 3"; "go" ] in
  Alcotest.(check (list string))
    "mates and claims"
    [ "move a1a8"; "1-0 {White mates}" ]
    out;
  let out = run [ "setboard k7/8/1K6/8/8/8/8/2Q5 w - - 0 1"; "force"; "usermove c1c7" ] in
  Alcotest.(check (list string))
    "stalemate is a draw, not resign"
    [ "1/2-1/2 {Stalemate}" ]
    out
;;

let test_errors () =
  let run = session () in
  let out = run [ "sd x"; "level 40 5"; "ping 7" ] in
  Alcotest.(check bool) "still alive" true (List.mem "pong 7" out)
;;

let () =
  Alcotest.run
    "XBoard"
    [ ( "xboard"
      , [ Alcotest.test_case "Features" `Quick test_features
        ; Alcotest.test_case "Reply and force" `Quick test_reply_and_force
        ; Alcotest.test_case
            "Illegal and underpromotion"
            `Quick
            test_illegal_and_underpromotion
        ; Alcotest.test_case "Undo and remove" `Quick test_undo
        ; Alcotest.test_case "Result claims" `Quick test_results
        ; Alcotest.test_case "Errors" `Quick test_errors
        ] )
    ]
;;
