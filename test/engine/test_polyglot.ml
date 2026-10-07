(** Tests for the Polyglot move encoding and opening book lookup *)

open Chessml

let find game str =
  match Game.find_move game str with
  | Some mv -> mv
  | None -> Alcotest.failf "move %s should be legal" str
;;

let test_encoding () =
  let game = Game.default () in
  (* to square in bits 0-5, from square in bits 6-11 *)
  Alcotest.(check int)
    "e2e4"
    (28 lor (12 lsl 6))
    (Polyglot.encode_move (find game "e2e4"));
  let castle = Game.of_fen "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1" in
  Alcotest.(check int)
    "O-O is e1h1"
    (7 lor (4 lsl 6))
    (Polyglot.encode_move (find castle "O-O"));
  Alcotest.(check int)
    "O-O-O is e1a1"
    (0 lor (4 lsl 6))
    (Polyglot.encode_move (find castle "O-O-O"));
  let promo = Game.of_fen "8/4P3/8/8/8/8/k7/7K w - - 0 1" in
  Alcotest.(check int)
    "e7e8n"
    (60 lor (52 lsl 6) lor (1 lsl 12))
    (Polyglot.encode_move (find promo "e7e8n"))
;;

let test_decoding () =
  let castle = Game.of_fen "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1" in
  let pos = Game.position castle in
  (match Polyglot.decode_move pos (7 lor (4 lsl 6)) with
   | Some mv ->
     Alcotest.(check bool) "e1h1 decodes to O-O" true (Move.kind mv = Move.ShortCastle)
   | None -> Alcotest.fail "e1h1 should decode");
  Alcotest.(check bool)
    "illegal move decodes to None"
    true
    (Polyglot.decode_move pos (36 lor (4 lsl 6)) = None)
;;

let test_book_lookup () =
  (* Keys with bit 63 and bit 31 set exercise the unsigned binary search *)
  let start = Game.default () in
  let after_e4 = Game.make_move start (find start "e2e4") in
  let key g = Position.key (Game.position g) in
  let entries =
    [ Polyglot.make_entry (key start) (find start "e2e4") 100
    ; Polyglot.make_entry (key start) (find start "d2d4") 50
    ; Polyglot.make_entry (key after_e4) (find after_e4 "c7c5") 70
    ; { Polyglot.key = 0x8000000080000000L; move = 1; weight = 1; learn = 0 }
    ; { Polyglot.key = 0x0000000080000000L; move = 1; weight = 1; learn = 0 }
    ; { Polyglot.key = 0xFFFFFFFFFFFFFFFFL; move = 1; weight = 1; learn = 0 }
    ]
    |> List.sort (fun a b -> Int64.unsigned_compare a.Polyglot.key b.Polyglot.key)
  in
  let file = Filename.temp_file "chessml_book" ".bin" in
  let oc = open_out_bin file in
  List.iter (Polyglot.write_entry oc) entries;
  close_out oc;
  let book = Opening_book.open_book file in
  let moves g =
    Opening_book.probe book (Game.position g)
    |> List.map (fun (mv, w) -> Move.to_uci mv, w)
    |> List.sort compare
  in
  Alcotest.(check (list (pair string int)))
    "startpos"
    [ "d2d4", 50; "e2e4", 100 ]
    (moves start);
  Alcotest.(check (list (pair string int))) "after 1.e4" [ "c7c5", 70 ] (moves after_e4);
  Option.iter Opening_book.close_book book;
  Sys.remove file
;;

let () =
  Alcotest.run
    "Polyglot"
    [ ( "polyglot"
      , [ Alcotest.test_case "Move encoding" `Quick test_encoding
        ; Alcotest.test_case "Move decoding" `Quick test_decoding
        ; Alcotest.test_case "Book lookup" `Quick test_book_lookup
        ] )
    ]
;;
