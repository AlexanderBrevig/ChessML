(** Create comprehensive opening book using Position.make_move API *)

open Chessml_core
open Chessml_engine

(** Move specification for book *)
type book_move =
  { from_sq : int
  ; to_sq : int
  ; weight : int
  ; description : string [@warning "-69"]
  }

(** Opening book entries *)
let openings = ref []

(** Add moves from a position (computed from move sequence) *)
let add_position_moves pos moves =
  let key = Zobrist.compute pos in
  List.iter (fun move -> openings := (key, move) :: !openings) moves
;;

(** Make a move and return new position *)
let make from_sq to_sq kind = Move.make from_sq to_sq kind

(** Apply a sequence of UCI moves to get a position *)
let position_after_moves move_list =
  let start =
    Position.of_fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  in
  List.fold_left (fun pos mv -> Position.make_move pos mv) start move_list
;;

let () =
  Printf.printf "📚 Creating Comprehensive Opening Book (API-based)\n";
  Printf.printf "%s\n\n" (String.make 50 '=');
  let e2 = Square.e2
  and e4 = Square.e4 in
  let d2 = Square.d2
  and d4 = Square.d4 in
  let g1 = Square.g1
  and f3 = Square.f3 in
  let c2 = Square.c2
  and c4 = Square.c4 in
  let e7 = Square.e7
  and e5 = Square.e5
  and e6 = Square.e6 in
  let c7 = Square.c7
  and c5 = Square.c5
  and c6 = Square.c6 in
  let d7 = Square.d7
  and d5 = Square.d5
  and d6 = Square.d6 in
  let b1 = Square.b1
  and c3 = Square.c3 in
  let g8 = Square.g8
  and f6 = Square.f6 in
  let f1 = Square.f1
  and c4_sq = Square.c4
  and c5_sq = Square.c5 in
  let b8 = Square.b8 in
  let c1 = Square.c1
  and f4 = Square.f4 in
  let g7 = Square.g7
  and g6 = Square.g6 in
  let f7 = Square.f7
  and f5 = Square.f5 in
  (* ========================================================================
     STARTING POSITION
     ======================================================================== *)
  let start = Position.default () in
  add_position_moves
    start
    [ { from_sq = e2; to_sq = e4; weight = 10000; description = "e4 - King's Pawn" }
    ; { from_sq = d2; to_sq = d4; weight = 8000; description = "d4 - Queen's Pawn" }
    ; { from_sq = g1; to_sq = f3; weight = 4000; description = "Nf3 - Reti" }
    ; { from_sq = c2; to_sq = c4; weight = 3000; description = "c4 - English" }
    ];
  (* ========================================================================
     AFTER 1.e4 - King's Pawn Openings
     ======================================================================== *)
  let after_e4 = position_after_moves [ make e2 e4 Move.PawnDoublePush ] in
  add_position_moves
    after_e4
    [ { from_sq = e7; to_sq = e5; weight = 5000; description = "e5 - Open Game" }
    ; { from_sq = c7; to_sq = c5; weight = 4000; description = "c5 - Sicilian" }
    ; { from_sq = c7; to_sq = c6; weight = 3000; description = "c6 - Caro-Kann" }
    ; { from_sq = e7; to_sq = e6; weight = 2500; description = "e6 - French" }
    ; { from_sq = d7; to_sq = d6; weight = 2000; description = "d6 - Pirc" }
    ];
  (* ========================================================================
     1.e4 e5 - Open Game
     ======================================================================== *)
  let after_e4_e5 =
    position_after_moves
      [ make e2 e4 Move.PawnDoublePush; make e7 e5 Move.PawnDoublePush ]
  in
  add_position_moves
    after_e4_e5
    [ { from_sq = g1; to_sq = f3; weight = 5000; description = "Nf3 - King's Knight" }
    ; { from_sq = b1; to_sq = c3; weight = 3000; description = "Nc3 - Vienna Game" }
    ; { from_sq = f1
      ; to_sq = c4_sq
      ; weight = 2000
      ; description = "Bc4 - Bishop's Opening"
      }
    ];
  (* ========================================================================
     VIENNA GAME: 1.e4 e5 2.Nc3
     ======================================================================== *)
  let after_vienna =
    position_after_moves
      [ make e2 e4 Move.PawnDoublePush
      ; make e7 e5 Move.PawnDoublePush
      ; make b1 c3 Move.Quiet
      ]
  in
  add_position_moves
    after_vienna
    [ { from_sq = g8; to_sq = f6; weight = 4000; description = "Nf6 - Vienna Game" }
    ; { from_sq = b8; to_sq = c6; weight = 3000; description = "Nc6" }
    ; { from_sq = f1; to_sq = c5_sq; weight = 2000; description = "Bc5" }
    ];
  (* ========================================================================
     CARO-KANN DEFENSE: 1.e4 c6
     ======================================================================== *)
  let after_e4_c6 =
    position_after_moves [ make e2 e4 Move.PawnDoublePush; make c7 c6 Move.Quiet ]
  in
  add_position_moves
    after_e4_c6
    [ { from_sq = d2
      ; to_sq = d4
      ; weight = 5000
      ; description = "d4 - Classical Caro-Kann"
      }
    ; { from_sq = b1; to_sq = c3; weight = 2000; description = "Nc3 - Caro-Kann 2.Nc3" }
    ];
  (* Caro-Kann: 1.e4 c6 2.d4 *)
  let after_caro_d4 =
    position_after_moves
      [ make e2 e4 Move.PawnDoublePush
      ; make c7 c6 Move.Quiet
      ; make d2 d4 Move.PawnDoublePush
      ]
  in
  add_position_moves
    after_caro_d4
    [ { from_sq = d7; to_sq = d5; weight = 5000; description = "d5 - Main line" } ];
  (* Caro-Kann: 1.e4 c6 2.d4 d5 *)
  let after_caro_d5 =
    position_after_moves
      [ make e2 e4 Move.PawnDoublePush
      ; make c7 c6 Move.Quiet
      ; make d2 d4 Move.PawnDoublePush
      ; make d7 d5 Move.PawnDoublePush
      ]
  in
  add_position_moves
    after_caro_d5
    [ { from_sq = e4
      ; to_sq = d5
      ; weight = 3000
      ; description = "exd5 - Exchange variation"
      }
    ; { from_sq = b1; to_sq = c3; weight = 4000; description = "Nc3 - Classical" }
    ; { from_sq = e4; to_sq = e5; weight = 2000; description = "e5 - Advance variation" }
    ];
  (* ========================================================================
     PIRC DEFENSE: 1.e4 d6
     ======================================================================== *)
  let after_e4_d6 =
    position_after_moves [ make e2 e4 Move.PawnDoublePush; make d7 d6 Move.Quiet ]
  in
  add_position_moves
    after_e4_d6
    [ { from_sq = d2; to_sq = d4; weight = 5000; description = "d4 - Classical Pirc" }
    ; { from_sq = b1; to_sq = c3; weight = 2000; description = "Nc3" }
    ];
  (* Pirc: 1.e4 d6 2.d4 *)
  let after_pirc_d4 =
    position_after_moves
      [ make e2 e4 Move.PawnDoublePush
      ; make d7 d6 Move.Quiet
      ; make d2 d4 Move.PawnDoublePush
      ]
  in
  add_position_moves
    after_pirc_d4
    [ { from_sq = g8; to_sq = f6; weight = 5000; description = "Nf6 - Main line" }
    ; { from_sq = g7; to_sq = g6; weight = 3000; description = "g6 - Fianchetto" }
    ];
  (* Pirc: 1.e4 d6 2.d4 Nf6 *)
  let after_pirc_nf6 =
    position_after_moves
      [ make e2 e4 Move.PawnDoublePush
      ; make d7 d6 Move.Quiet
      ; make d2 d4 Move.PawnDoublePush
      ; make g8 f6 Move.Quiet
      ]
  in
  add_position_moves
    after_pirc_nf6
    [ { from_sq = b1; to_sq = c3; weight = 5000; description = "Nc3" }
    ; { from_sq = f1; to_sq = Square.d3; weight = 2000; description = "Bd3" }
    ];
  (* ========================================================================
     AFTER 1.d4 - Queen's Pawn Openings
     ======================================================================== *)
  let after_d4 = position_after_moves [ make d2 d4 Move.PawnDoublePush ] in
  add_position_moves
    after_d4
    [ { from_sq = d7; to_sq = d5; weight = 5000; description = "d5 - Closed Game" }
    ; { from_sq = g8; to_sq = f6; weight = 4000; description = "Nf6 - Indian Defenses" }
    ; { from_sq = e7; to_sq = e6; weight = 2000; description = "e6" }
    ; { from_sq = f7; to_sq = f5; weight = 1500; description = "f5 - Dutch Defense" }
    ];
  (* ========================================================================
     LONDON SYSTEM: 1.d4 d5 2.Bf4
     ======================================================================== *)
  let after_d4_d5 =
    position_after_moves
      [ make d2 d4 Move.PawnDoublePush; make d7 d5 Move.PawnDoublePush ]
  in
  add_position_moves
    after_d4_d5
    [ { from_sq = c1; to_sq = f4; weight = 4000; description = "Bf4 - London System" }
    ; { from_sq = c2; to_sq = c4; weight = 4000; description = "c4 - Queen's Gambit" }
    ; { from_sq = g1; to_sq = f3; weight = 3000; description = "Nf3" }
    ];
  (* London: 1.d4 d5 2.Bf4 *)
  let after_london_bf4 =
    position_after_moves
      [ make d2 d4 Move.PawnDoublePush
      ; make d7 d5 Move.PawnDoublePush
      ; make c1 f4 Move.Quiet
      ]
  in
  add_position_moves
    after_london_bf4
    [ { from_sq = g8; to_sq = f6; weight = 5000; description = "Nf6 - Main line" }
    ; { from_sq = c7; to_sq = c5; weight = 2000; description = "c5" }
    ; { from_sq = e7; to_sq = e6; weight = 2000; description = "e6" }
    ];
  (* London: 1.d4 d5 2.Bf4 Nf6 *)
  let after_london_nf6 =
    position_after_moves
      [ make d2 d4 Move.PawnDoublePush
      ; make d7 d5 Move.PawnDoublePush
      ; make c1 f4 Move.Quiet
      ; make g8 f6 Move.Quiet
      ]
  in
  add_position_moves
    after_london_nf6
    [ { from_sq = e2; to_sq = Square.e3; weight = 5000; description = "e3 - Main line" }
    ; { from_sq = g1; to_sq = f3; weight = 3000; description = "Nf3" }
    ];
  (* London: 1.d4 d5 2.Bf4 Nf6 3.e3 *)
  let after_london_e3 =
    position_after_moves
      [ make d2 d4 Move.PawnDoublePush
      ; make d7 d5 Move.PawnDoublePush
      ; make c1 f4 Move.Quiet
      ; make g8 f6 Move.Quiet
      ; make e2 Square.e3 Move.Quiet
      ]
  in
  add_position_moves
    after_london_e3
    [ { from_sq = e7; to_sq = e6; weight = 4000; description = "e6" }
    ; { from_sq = c7; to_sq = c5; weight = 3000; description = "c5" }
    ; { from_sq = b8; to_sq = Square.d7; weight = 2000; description = "Nbd7" }
    ];
  (* ========================================================================
     KING'S INDIAN: 1.d4 Nf6 2.c4
     ======================================================================== *)
  let after_d4_nf6 =
    position_after_moves [ make d2 d4 Move.PawnDoublePush; make g8 f6 Move.Quiet ]
  in
  add_position_moves
    after_d4_nf6
    [ { from_sq = c2; to_sq = c4; weight = 5000; description = "c4 - King's Indian" }
    ; { from_sq = g1; to_sq = f3; weight = 3000; description = "Nf3" }
    ];
  (* King's Indian: 1.d4 Nf6 2.c4 *)
  let after_ki_c4 =
    position_after_moves
      [ make d2 d4 Move.PawnDoublePush
      ; make g8 f6 Move.Quiet
      ; make c2 c4 Move.PawnDoublePush
      ]
  in
  add_position_moves
    after_ki_c4
    [ { from_sq = g7
      ; to_sq = g6
      ; weight = 4000
      ; description = "g6 - King's Indian Defense"
      }
    ; { from_sq = e7
      ; to_sq = e6
      ; weight = 3000
      ; description = "e6 - Nimzo/Queen's Indian"
      }
    ; { from_sq = c7; to_sq = c5; weight = 2000; description = "c5 - Symmetrical" }
    ];
  (* King's Indian: 1.d4 Nf6 2.c4 g6 *)
  let after_ki_g6 =
    position_after_moves
      [ make d2 d4 Move.PawnDoublePush
      ; make g8 f6 Move.Quiet
      ; make c2 c4 Move.PawnDoublePush
      ; make g7 g6 Move.Quiet
      ]
  in
  add_position_moves
    after_ki_g6
    [ { from_sq = b1; to_sq = c3; weight = 5000; description = "Nc3" }
    ; { from_sq = g1; to_sq = f3; weight = 3000; description = "Nf3" }
    ];
  (* ========================================================================
     AFTER 1.Nf3 - Reti Opening
     ======================================================================== *)
  let after_nf3 = position_after_moves [ make g1 f3 Move.Quiet ] in
  add_position_moves
    after_nf3
    [ { from_sq = d7; to_sq = d5; weight = 4000; description = "d5" }
    ; { from_sq = g8; to_sq = f6; weight = 3000; description = "Nf6" }
    ; { from_sq = c7; to_sq = c5; weight = 2000; description = "c5" }
    ];
  (* ========================================================================
     AFTER 1.c4 - English Opening
     ======================================================================== *)
  let after_c4 = position_after_moves [ make c2 c4 Move.PawnDoublePush ] in
  add_position_moves
    after_c4
    [ { from_sq = e7; to_sq = e5; weight = 3000; description = "e5 - Reversed Sicilian" }
    ; { from_sq = g8; to_sq = f6; weight = 3000; description = "Nf6" }
    ; { from_sq = c7; to_sq = c5; weight = 2000; description = "c5 - Symmetrical" }
    ];
  (* ======================================================================== *)
  Printf.printf "Building book with %d moves...\n\n" (List.length !openings);
  (* Sort entries by zobrist key (required for binary search) *)
  let sorted_entries = List.sort (fun (k1, _) (k2, _) -> Int64.compare k1 k2) !openings in
  (* Write to file *)
  let oc = open_out_bin "book.bin" in
  List.iter
    (fun (key, move) ->
       let entry = Polyglot.make_entry key move.from_sq move.to_sq move.weight in
       Polyglot.write_entry oc entry)
    sorted_entries;
  close_out oc;
  let file_size = List.length sorted_entries * 16 in
  Printf.printf "✅ Created book.bin\n";
  Printf.printf "   • %d bytes\n" file_size;
  Printf.printf "   • %d moves\n" (List.length sorted_entries);
  Printf.printf
    "   • %d positions\n"
    (List.length (List.sort_uniq compare (List.map fst sorted_entries)));
  Printf.printf "\n";
  Printf.printf "Coverage:\n";
  Printf.printf "  ✓ Starting position (e4, d4, Nf3, c4)\n";
  Printf.printf "  ✓ Vienna Game (1.e4 e5 2.Nc3)\n";
  Printf.printf "  ✓ Caro-Kann Defense (1.e4 c6 2.d4 d5)\n";
  Printf.printf "  ✓ Pirc Defense (1.e4 d6 2.d4 Nf6)\n";
  Printf.printf "  ✓ London System (1.d4 d5 2.Bf4 Nf6 3.e3)\n";
  Printf.printf "  ✓ King's Indian (1.d4 Nf6 2.c4 g6)\n";
  Printf.printf "  ✓ Reti Opening (1.Nf3)\n";
  Printf.printf "  ✓ English Opening (1.c4)\n";
  Printf.printf "\n";
  (* Self-test the generated book *)
  Printf.printf "%s\n" (String.make 50 '=');
  Printf.printf "🧪 Self-Testing Book\n\n";
  let book = Opening_book.open_book "book.bin" in
  let test_passed = ref true in
  (* Helper to test if a specific move exists for a position *)
  let test_move pos from_sq to_sq _description =
    let found = ref false in
    (* Try multiple times with random=true to see if we get the move *)
    for _i = 1 to 20 do
      match Opening_book.get_book_move ~random:true book pos with
      | Some mv when Move.from mv = from_sq && Move.to_square mv = to_sq -> found := true
      | _ -> ()
    done;
    !found
  in
  (* Test 1: Starting position *)
  Printf.printf "Test 1: Starting position\n";
  let start_pos =
    Position.of_fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  in
  let has_e4 = test_move start_pos Square.e2 Square.e4 "e4" in
  let has_d4 = test_move start_pos Square.d2 Square.d4 "d4" in
  let has_nf3 = test_move start_pos Square.g1 Square.f3 "Nf3" in
  let has_c4 = test_move start_pos Square.c2 Square.c4 "c4" in
  if has_e4 && has_d4 && has_nf3 && has_c4
  then Printf.printf "  ✅ Found e4, d4, Nf3, c4\n"
  else (
    Printf.printf
      "  ❌ Missing moves: e4=%b d4=%b Nf3=%b c4=%b\n"
      has_e4
      has_d4
      has_nf3
      has_c4;
    test_passed := false);
  (* Test 2: After 1.e4 *)
  Printf.printf "Test 2: After 1.e4\n";
  let pos_e4 = position_after_moves [ make Square.e2 Square.e4 Move.PawnDoublePush ] in
  let has_e5 = test_move pos_e4 Square.e7 Square.e5 "e5" in
  let has_c5 = test_move pos_e4 Square.c7 Square.c5 "c5" in
  let has_c6 = test_move pos_e4 Square.c7 Square.c6 "c6" in
  if has_e5 && has_c5 && has_c6
  then Printf.printf "  ✅ Found e5, c5, c6\n"
  else (
    Printf.printf "  ❌ Missing moves: e5=%b c5=%b c6=%b\n" has_e5 has_c5 has_c6;
    test_passed := false);
  (* Test 3: After 1.e4 e5 (Vienna Game) *)
  Printf.printf "Test 3: After 1.e4 e5\n";
  let pos_vienna =
    position_after_moves
      [ make Square.e2 Square.e4 Move.PawnDoublePush
      ; make Square.e7 Square.e5 Move.PawnDoublePush
      ]
  in
  let has_nc3 = test_move pos_vienna Square.b1 Square.c3 "Nc3" in
  let has_nf3 = test_move pos_vienna Square.g1 Square.f3 "Nf3" in
  if has_nc3 && has_nf3
  then Printf.printf "  ✅ Found Nc3, Nf3\n"
  else (
    Printf.printf "  ❌ Missing moves: Nc3=%b Nf3=%b\n" has_nc3 has_nf3;
    test_passed := false);
  (* Test 4: After 1.d4 *)
  Printf.printf "Test 4: After 1.d4\n";
  let pos_d4 = position_after_moves [ make Square.d2 Square.d4 Move.PawnDoublePush ] in
  let has_d5 = test_move pos_d4 Square.d7 Square.d5 "d5" in
  let has_nf6 = test_move pos_d4 Square.g8 Square.f6 "Nf6" in
  if has_d5 && has_nf6
  then Printf.printf "  ✅ Found d5, Nf6\n"
  else (
    Printf.printf "  ❌ Missing moves: d5=%b Nf6=%b\n" has_d5 has_nf6;
    test_passed := false);
  Printf.printf "\n";
  if !test_passed
  then Printf.printf "✅ All tests passed! Book is working correctly.\n"
  else (
    Printf.printf "❌ Some tests failed. Please check the book.\n";
    exit 1)
;;
