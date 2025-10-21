(** Create comprehensive opening book with major openings *)

open Chessml_engine

(** Move specification: from square, to square, weight, description *)
type book_move =
  { from_sq : int
  ; to_sq : int
  ; weight : int
  ; description : string [@warning "-69"]
  }

(** Helper to convert algebraic notation to square index *)
let sq notation =
  let file = Char.code notation.[0] - Char.code 'a' in
  let rank = Char.code notation.[1] - Char.code '1' in
  file + (rank * 8)
;;

(** Encode move in Polyglot format *)
let encode_move from_sq to_sq promotion = from_sq lor (to_sq lsl 6) lor (promotion lsl 12)

(** Create a 16-byte Polyglot book entry *)
let create_entry zobrist_key from_sq to_sq weight =
  let move = encode_move from_sq to_sq 0 in
  let buffer = Bytes.create 16 in
  (* Helper to write big-endian integers *)
  let set_be_int64 buf pos v =
    for i = 0 to 7 do
      Bytes.set
        buf
        (pos + i)
        (Char.chr (Int64.to_int (Int64.shift_right_logical v ((7 - i) * 8)) land 0xFF))
    done
  in
  let set_be_int16 buf pos v =
    Bytes.set buf pos (Char.chr ((v lsr 8) land 0xFF));
    Bytes.set buf (pos + 1) (Char.chr (v land 0xFF))
  in
  let set_be_int32 buf pos v =
    for i = 0 to 3 do
      Bytes.set
        buf
        (pos + i)
        (Char.chr (Int32.to_int (Int32.shift_right_logical v ((3 - i) * 8)) land 0xFF))
    done
  in
  (* Zobrist key - 8 bytes big-endian *)
  set_be_int64 buffer 0 zobrist_key;
  (* Move - 2 bytes big-endian *)
  set_be_int16 buffer 8 move;
  (* Weight - 2 bytes big-endian *)
  set_be_int16 buffer 10 weight;
  (* Learn - 4 bytes (unused, set to 0) *)
  set_be_int32 buffer 12 0l;
  Bytes.to_string buffer
;;

(** Opening book builder *)
let openings = ref []

let add_position fen moves =
  let pos = Position.of_fen fen in
  let key = Zobrist.compute pos in
  List.iter (fun move -> openings := (key, move) :: !openings) moves
;;

let () =
  Printf.printf "📚 Creating Comprehensive Opening Book\n";
  Printf.printf "%s\n\n" (String.make 50 '=');
  (* ========================================================================
     STARTING POSITION
     ======================================================================== *)
  add_position
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    [ { from_sq = sq "e2"
      ; to_sq = sq "e4"
      ; weight = 10000
      ; description = "e4 - King's Pawn"
      }
    ; { from_sq = sq "d2"
      ; to_sq = sq "d4"
      ; weight = 8000
      ; description = "d4 - Queen's Pawn"
      }
    ; { from_sq = sq "g1"; to_sq = sq "f3"; weight = 4000; description = "Nf3 - Reti" }
    ; { from_sq = sq "c2"; to_sq = sq "c4"; weight = 3000; description = "c4 - English" }
    ];
  (* ========================================================================
     AFTER 1.e4 - King's Pawn Openings
     ======================================================================== *)
  add_position
    "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
    [ { from_sq = sq "e7"
      ; to_sq = sq "e5"
      ; weight = 5000
      ; description = "e5 - Open Game"
      }
    ; { from_sq = sq "c7"; to_sq = sq "c5"; weight = 4000; description = "c5 - Sicilian" }
    ; { from_sq = sq "c7"
      ; to_sq = sq "c6"
      ; weight = 3000
      ; description = "c6 - Caro-Kann"
      }
    ; { from_sq = sq "e7"; to_sq = sq "e6"; weight = 2500; description = "e6 - French" }
    ; { from_sq = sq "d7"; to_sq = sq "d6"; weight = 2000; description = "d6 - Pirc" }
    ];
  (* ========================================================================
     1.e4 e5 - Open Game
     ======================================================================== *)
  add_position
    "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2"
    [ { from_sq = sq "g1"
      ; to_sq = sq "f3"
      ; weight = 5000
      ; description = "Nf3 - King's Knight"
      }
    ; { from_sq = sq "b1"
      ; to_sq = sq "c3"
      ; weight = 3000
      ; description = "Nc3 - Vienna Game"
      }
    ; { from_sq = sq "f1"
      ; to_sq = sq "c4"
      ; weight = 2000
      ; description = "Bc4 - Bishop's Opening"
      }
    ];
  (* ========================================================================
     VIENNA GAME: 1.e4 e5 2.Nc3
     ======================================================================== *)
  add_position
    "rnbqkbnr/pppp1ppp/8/4p3/4P3/2N5/PPPP1PPP/R1BQKBNR b KQkq - 1 2"
    [ { from_sq = sq "g8"
      ; to_sq = sq "f6"
      ; weight = 4000
      ; description = "Nf6 - Vienna Game"
      }
    ; { from_sq = sq "b8"; to_sq = sq "c6"; weight = 3000; description = "Nc6" }
    ; { from_sq = sq "f8"; to_sq = sq "c5"; weight = 2000; description = "Bc5" }
    ];
  (* ========================================================================
     CARO-KANN DEFENSE: 1.e4 c6
     ======================================================================== *)
  add_position
    "rnbqkbnr/pp1ppppp/2p5/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"
    [ { from_sq = sq "d2"
      ; to_sq = sq "d4"
      ; weight = 5000
      ; description = "d4 - Classical Caro-Kann"
      }
    ; { from_sq = sq "b1"
      ; to_sq = sq "c3"
      ; weight = 2000
      ; description = "Nc3 - Caro-Kann 2.Nc3"
      }
    ];
  (* Caro-Kann: 1.e4 c6 2.d4 *)
  add_position
    "rnbqkbnr/pp1ppppp/2p5/8/3PP3/8/PPP2PPP/RNBQKBNR b KQkq d3 0 2"
    [ { from_sq = sq "d7"
      ; to_sq = sq "d5"
      ; weight = 5000
      ; description = "d5 - Main line"
      }
    ];
  (* Caro-Kann: 1.e4 c6 2.d4 d5 *)
  add_position
    "rnbqkbnr/pp2pppp/2p5/3p4/3PP3/8/PPP2PPP/RNBQKBNR w KQkq d6 0 3"
    [ { from_sq = sq "e4"
      ; to_sq = sq "d5"
      ; weight = 3000
      ; description = "exd5 - Exchange variation"
      }
    ; { from_sq = sq "b1"
      ; to_sq = sq "c3"
      ; weight = 4000
      ; description = "Nc3 - Classical"
      }
    ; { from_sq = sq "e4"
      ; to_sq = sq "e5"
      ; weight = 2000
      ; description = "e5 - Advance variation"
      }
    ];
  (* ========================================================================
     PIRC DEFENSE: 1.e4 d6
     ======================================================================== *)
  add_position
    "rnbqkbnr/ppp1pppp/3p4/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"
    [ { from_sq = sq "d2"
      ; to_sq = sq "d4"
      ; weight = 5000
      ; description = "d4 - Classical Pirc"
      }
    ; { from_sq = sq "b1"; to_sq = sq "c3"; weight = 2000; description = "Nc3" }
    ];
  (* Pirc: 1.e4 d6 2.d4 *)
  add_position
    "rnbqkbnr/ppp1pppp/3p4/8/3PP3/8/PPP2PPP/RNBQKBNR b KQkq d3 0 2"
    [ { from_sq = sq "g8"
      ; to_sq = sq "f6"
      ; weight = 5000
      ; description = "Nf6 - Main line"
      }
    ; { from_sq = sq "g7"
      ; to_sq = sq "g6"
      ; weight = 3000
      ; description = "g6 - Fianchetto"
      }
    ];
  (* Pirc: 1.e4 d6 2.d4 Nf6 *)
  add_position
    "rnbqkb1r/ppp1pppp/3p1n2/8/3PP3/8/PPP2PPP/RNBQKBNR w KQkq - 1 3"
    [ { from_sq = sq "b1"; to_sq = sq "c3"; weight = 5000; description = "Nc3" }
    ; { from_sq = sq "f1"; to_sq = sq "d3"; weight = 2000; description = "Bd3" }
    ];
  (* ========================================================================
     AFTER 1.d4 - Queen's Pawn Openings
     ======================================================================== *)
  add_position
    "rnbqkbnr/pppppppp/8/8/3P4/8/PPP1PPPP/RNBQKBNR b KQkq d3 0 1"
    [ { from_sq = sq "d7"
      ; to_sq = sq "d5"
      ; weight = 5000
      ; description = "d5 - Closed Game"
      }
    ; { from_sq = sq "g8"
      ; to_sq = sq "f6"
      ; weight = 4000
      ; description = "Nf6 - Indian Defenses"
      }
    ; { from_sq = sq "e7"; to_sq = sq "e6"; weight = 2000; description = "e6" }
    ; { from_sq = sq "f7"
      ; to_sq = sq "f5"
      ; weight = 1500
      ; description = "f5 - Dutch Defense"
      }
    ];
  (* ========================================================================
     LONDON SYSTEM: 1.d4 d5 2.Bf4
     ======================================================================== *)
  add_position
    "rnbqkbnr/ppp1pppp/8/3p4/3P4/8/PPP1PPPP/RNBQKBNR w KQkq d6 0 2"
    [ { from_sq = sq "c1"
      ; to_sq = sq "f4"
      ; weight = 4000
      ; description = "Bf4 - London System"
      }
    ; { from_sq = sq "c2"
      ; to_sq = sq "c4"
      ; weight = 4000
      ; description = "c4 - Queen's Gambit"
      }
    ; { from_sq = sq "g1"; to_sq = sq "f3"; weight = 3000; description = "Nf3" }
    ];
  (* London: 1.d4 d5 2.Bf4 *)
  add_position
    "rnbqkbnr/ppp1pppp/8/3p4/3P1B2/8/PPP1PPPP/RN1QKBNR b KQkq - 1 2"
    [ { from_sq = sq "g8"
      ; to_sq = sq "f6"
      ; weight = 5000
      ; description = "Nf6 - Main line"
      }
    ; { from_sq = sq "c7"; to_sq = sq "c5"; weight = 2000; description = "c5" }
    ; { from_sq = sq "e7"; to_sq = sq "e6"; weight = 2000; description = "e6" }
    ];
  (* London: 1.d4 d5 2.Bf4 Nf6 *)
  add_position
    "rnbqkb1r/ppp1pppp/5n2/3p4/3P1B2/8/PPP1PPPP/RN1QKBNR w KQkq - 2 3"
    [ { from_sq = sq "e2"
      ; to_sq = sq "e3"
      ; weight = 5000
      ; description = "e3 - Main line"
      }
    ; { from_sq = sq "g1"; to_sq = sq "f3"; weight = 3000; description = "Nf3" }
    ];
  (* London: 1.d4 d5 2.Bf4 Nf6 3.e3 *)
  add_position
    "rnbqkb1r/ppp1pppp/5n2/3p4/3P1B2/4P3/PPP2PPP/RN1QKBNR b KQkq - 0 3"
    [ { from_sq = sq "e7"; to_sq = sq "e6"; weight = 4000; description = "e6" }
    ; { from_sq = sq "c7"; to_sq = sq "c5"; weight = 3000; description = "c5" }
    ; { from_sq = sq "b8"; to_sq = sq "d7"; weight = 2000; description = "Nbd7" }
    ];
  (* ========================================================================
     KING'S INDIAN: 1.d4 Nf6 2.c4
     ======================================================================== *)
  add_position
    "rnbqkb1r/pppppppp/5n2/8/3P4/8/PPP1PPPP/RNBQKBNR w KQkq - 1 2"
    [ { from_sq = sq "c2"
      ; to_sq = sq "c4"
      ; weight = 5000
      ; description = "c4 - King's Indian"
      }
    ; { from_sq = sq "g1"; to_sq = sq "f3"; weight = 3000; description = "Nf3" }
    ];
  (* King's Indian: 1.d4 Nf6 2.c4 *)
  add_position
    "rnbqkb1r/pppppppp/5n2/8/2PP4/8/PP2PPPP/RNBQKBNR b KQkq c3 0 2"
    [ { from_sq = sq "g7"
      ; to_sq = sq "g6"
      ; weight = 4000
      ; description = "g6 - King's Indian Defense"
      }
    ; { from_sq = sq "e7"
      ; to_sq = sq "e6"
      ; weight = 3000
      ; description = "e6 - Nimzo/Queen's Indian"
      }
    ; { from_sq = sq "c7"
      ; to_sq = sq "c5"
      ; weight = 2000
      ; description = "c5 - Symmetrical"
      }
    ];
  (* King's Indian: 1.d4 Nf6 2.c4 g6 *)
  add_position
    "rnbqkb1r/pppppp1p/5np1/8/2PP4/8/PP2PPPP/RNBQKBNR w KQkq - 0 3"
    [ { from_sq = sq "b1"; to_sq = sq "c3"; weight = 5000; description = "Nc3" }
    ; { from_sq = sq "g1"; to_sq = sq "f3"; weight = 3000; description = "Nf3" }
    ];
  (* ========================================================================
     AFTER 1.Nf3 - Reti Opening
     ======================================================================== *)
  add_position
    "rnbqkbnr/pppppppp/8/8/8/5N2/PPPPPPPP/RNBQKB1R b KQkq - 1 1"
    [ { from_sq = sq "d7"; to_sq = sq "d5"; weight = 4000; description = "d5" }
    ; { from_sq = sq "g8"; to_sq = sq "f6"; weight = 3000; description = "Nf6" }
    ; { from_sq = sq "c7"; to_sq = sq "c5"; weight = 2000; description = "c5" }
    ];
  (* ========================================================================
     AFTER 1.c4 - English Opening
     ======================================================================== *)
  add_position
    "rnbqkbnr/pppppppp/8/8/2P5/8/PP1PPPPP/RNBQKBNR b KQkq c3 0 1"
    [ { from_sq = sq "e7"
      ; to_sq = sq "e5"
      ; weight = 3000
      ; description = "e5 - Reversed Sicilian"
      }
    ; { from_sq = sq "g8"; to_sq = sq "f6"; weight = 3000; description = "Nf6" }
    ; { from_sq = sq "c7"
      ; to_sq = sq "c5"
      ; weight = 2000
      ; description = "c5 - Symmetrical"
      }
    ];
  (* ======================================================================== *)
  Printf.printf "Building book with %d moves...\n\n" (List.length !openings);
  (* Sort entries by zobrist key (required for binary search) *)
  let sorted_entries = List.sort (fun (k1, _) (k2, _) -> Int64.compare k1 k2) !openings in
  (* Create binary entries *)
  let entries =
    List.map
      (fun (key, move) -> create_entry key move.from_sq move.to_sq move.weight)
      sorted_entries
  in
  (* Write to file *)
  let output_file = "book.bin" in
  let oc = open_out_bin output_file in
  List.iter (fun entry -> output_string oc entry) entries;
  close_out oc;
  let file_size = List.length entries * 16 in
  let num_positions =
    List.length (List.sort_uniq compare (List.map fst sorted_entries))
  in
  Printf.printf "✅ Created %s\n" output_file;
  Printf.printf "   • %d bytes\n" file_size;
  Printf.printf "   • %d moves\n" (List.length entries);
  Printf.printf "   • %d positions\n" num_positions;
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
  Printf.printf "Test it: dune exec examples/book_test.exe\n"
;;
