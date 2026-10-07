(** UCI protocol tests: drive a session line by line and inspect its output *)

open Chessml

let session () =
  let out = ref [] in
  let s = Uci.create_session ~send:(fun l -> out := l :: !out) () in
  let run lines =
    out := [];
    List.iter (fun l -> ignore (Uci.handle_line s l)) lines;
    Uci.wait s;
    List.rev !out
  in
  run
;;

let bestmove lines =
  match List.find_opt (fun l -> String.starts_with ~prefix:"bestmove " l) lines with
  | Some l -> String.sub l 9 (String.length l - 9)
  | None -> Alcotest.fail ("no bestmove in: " ^ String.concat " | " lines)
;;

let test_handshake () =
  let run = session () in
  let out = run [ "uci"; "isready" ] in
  Alcotest.(check bool) "uciok" true (List.mem "uciok" out);
  Alcotest.(check bool) "readyok" true (List.mem "readyok" out);
  Alcotest.(check bool)
    "Hash option"
    true
    (List.exists (String.starts_with ~prefix:"option name Hash type spin") out)
;;

let test_position_moves () =
  (* Castling and en passant from the GUI must be applied with their side effects *)
  let game =
    Uci.parse_position
      [ "startpos"; "moves"; "e2e4"; "e7e5"; "g1f3"; "b8c6"; "f1c4"; "g8f6"; "e1g1" ]
  in
  Alcotest.(check string)
    "castled"
    "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQ1RK1 b kq - 5 4"
    (Game.to_fen game);
  let game =
    Uci.parse_position
      [ "fen"; "8/8/8/3pP3/8/8/8/k6K"; "w"; "-"; "d6"; "0"; "1"; "moves"; "e5d6" ]
  in
  Alcotest.(check string) "en passant" "8/8/3P4/8/8/8/8/k6K b - - 0 1" (Game.to_fen game)
;;

let test_go_depth () =
  let run = session () in
  let out = run [ "position startpos moves e2e4"; "go depth 3" ] in
  let mv = bestmove out in
  let game = Uci.parse_position [ "startpos"; "moves"; "e2e4" ] in
  Alcotest.(check bool) "legal bestmove" true (Game.find_move game mv <> None);
  Alcotest.(check bool)
    "info with pv"
    true
    (List.exists
       (fun l -> String.starts_with ~prefix:"info depth 3 " l && String.length l > 0)
       out)
;;

let test_mate_score () =
  let run = session () in
  let out = run [ "position fen 6k1/5ppp/8/8/8/8/8/R5K1 w - - 0 1"; "go depth 4" ] in
  Alcotest.(check string) "mates" "a1a8" (bestmove out);
  Alcotest.(check bool)
    "score mate 1"
    true
    (List.exists
       (fun l ->
          let words = String.split_on_char ' ' l in
          let rec has = function
            | "score" :: "mate" :: "1" :: _ -> true
            | _ :: rest -> has rest
            | [] -> false
          in
          has words)
       out)
;;

let test_movetime () =
  let run = session () in
  let start = Unix.gettimeofday () in
  let out = run [ "position startpos"; "go movetime 200" ] in
  let elapsed = Unix.gettimeofday () -. start in
  ignore (bestmove out);
  Alcotest.(check bool) (Printf.sprintf "within time (%.2fs)" elapsed) true (elapsed < 0.6)
;;

let test_infinite_stop () =
  let out = ref [] in
  let s = Uci.create_session ~send:(fun l -> out := l :: !out) () in
  ignore (Uci.handle_line s "position startpos");
  ignore (Uci.handle_line s "go infinite");
  Thread.delay 0.2;
  ignore (Uci.handle_line s "isready");
  Alcotest.(check bool) "readyok while searching" true (List.mem "readyok" !out);
  ignore (Uci.handle_line s "stop");
  ignore (bestmove (List.rev !out))
;;

let test_errors_do_not_quit () =
  let run = session () in
  let out =
    run
      [ "go depth abc"
      ; "position fen garbage"
      ; "position startpos moves e2e5"
      ; "isready"
      ]
  in
  Alcotest.(check bool) "still answers" true (List.mem "readyok" out);
  Alcotest.(check int)
    "three errors reported"
    3
    (List.length (List.filter (String.starts_with ~prefix:"info string error") out))
;;

let test_time_budget () =
  let params =
    Uci.parse_go_params [ "wtime"; "60000"; "btime"; "1000"; "winc"; "1000" ]
  in
  let white = Option.get (Uci.time_limit_ms params White) in
  let black = Option.get (Uci.time_limit_ms params Black) in
  Alcotest.(check bool) "white uses a fraction" true (white > 1000 && white < 10000);
  Alcotest.(check bool) "black never flags" true (black < 1000)
;;

let () =
  Alcotest.run
    "UCI"
    [ ( "uci"
      , [ Alcotest.test_case "Handshake" `Quick test_handshake
        ; Alcotest.test_case "Position with moves" `Quick test_position_moves
        ; Alcotest.test_case "go depth" `Quick test_go_depth
        ; Alcotest.test_case "Mate score" `Quick test_mate_score
        ; Alcotest.test_case "go movetime" `Quick test_movetime
        ; Alcotest.test_case "go infinite / stop" `Quick test_infinite_stop
        ; Alcotest.test_case "Errors do not quit" `Quick test_errors_do_not_quit
        ; Alcotest.test_case "Time budget" `Quick test_time_budget
        ] )
    ]
;;
