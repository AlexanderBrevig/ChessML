(** Magic Bitboards for fast sliding piece attack generation
    
    This implements the "plain" magic bitboard approach which uses pre-computed
    magic numbers and lookup tables to generate rook and bishop attacks in O(1).
    
    References:
    - https://www.chessprogramming.org/Magic_Bitboards
    - Stockfish implementation
*)

open Chessml_core

(** Magic entry for a square *)
type magic_entry =
  { mask : Int64.t (* Relevant occupancy bits (excludes edges) *)
  ; magic : Int64.t (* Magic number *)
  ; shift : int (* Shift amount (64 - bits) *)
  ; offset : int (* Offset into attack table *)
  }

(** Attack tables - will be filled during initialization *)
let rook_attacks_table : Int64.t array = Array.make 102400 0L

let bishop_attacks_table : Int64.t array = Array.make 5248 0L

(** Magic entries for each square *)
let rook_magics : magic_entry array =
  Array.make 64 { mask = 0L; magic = 0L; shift = 0; offset = 0 }
;;

let bishop_magics : magic_entry array =
  Array.make 64 { mask = 0L; magic = 0L; shift = 0; offset = 0 }
;;

(** Compute relevant occupancy mask for rooks (excludes edges) *)
let rook_mask (sq : int) : Int64.t =
  let file = sq mod 8 in
  let rank = sq / 8 in
  let mask = ref 0L in
  (* North (exclude last rank) *)
  for r = rank + 1 to 6 do
    mask := Bitboard.set !mask (file + (r * 8))
  done;
  (* South (exclude first rank) *)
  for r = rank - 1 downto 1 do
    mask := Bitboard.set !mask (file + (r * 8))
  done;
  (* East (exclude last file) *)
  for f = file + 1 to 6 do
    mask := Bitboard.set !mask (f + (rank * 8))
  done;
  (* West (exclude first file) *)
  for f = file - 1 downto 1 do
    mask := Bitboard.set !mask (f + (rank * 8))
  done;
  !mask
;;

(** Compute relevant occupancy mask for bishops (excludes edges) *)
let bishop_mask (sq : int) : Int64.t =
  let file = sq mod 8 in
  let rank = sq / 8 in
  let mask = ref 0L in
  (* Northeast *)
  (try
     for i = 1 to 7 do
       let f = file + i in
       let r = rank + i in
       if f > 6 || r > 6 then raise Exit;
       mask := Bitboard.set !mask (f + (r * 8))
     done
   with
   | Exit -> ());
  (* Northwest *)
  (try
     for i = 1 to 7 do
       let f = file - i in
       let r = rank + i in
       if f < 1 || r > 6 then raise Exit;
       mask := Bitboard.set !mask (f + (r * 8))
     done
   with
   | Exit -> ());
  (* Southeast *)
  (try
     for i = 1 to 7 do
       let f = file + i in
       let r = rank - i in
       if f > 6 || r < 1 then raise Exit;
       mask := Bitboard.set !mask (f + (r * 8))
     done
   with
   | Exit -> ());
  (* Southwest *)
  (try
     for i = 1 to 7 do
       let f = file - i in
       let r = rank - i in
       if f < 1 || r < 1 then raise Exit;
       mask := Bitboard.set !mask (f + (r * 8))
     done
   with
   | Exit -> ());
  !mask
;;

(** Compute rook attacks from a square with given blockers (slow, for initialization) *)
let rook_attacks_slow (sq : int) (blockers : Int64.t) : Int64.t =
  let file = sq mod 8 in
  let rank = sq / 8 in
  let attacks = ref 0L in
  (* North *)
  (try
     for r = rank + 1 to 7 do
       let target = file + (r * 8) in
       attacks := Bitboard.set !attacks target;
       if Bitboard.contains blockers target then raise Exit
     done
   with
   | Exit -> ());
  (* South *)
  (try
     for r = rank - 1 downto 0 do
       let target = file + (r * 8) in
       attacks := Bitboard.set !attacks target;
       if Bitboard.contains blockers target then raise Exit
     done
   with
   | Exit -> ());
  (* East *)
  (try
     for f = file + 1 to 7 do
       let target = f + (rank * 8) in
       attacks := Bitboard.set !attacks target;
       if Bitboard.contains blockers target then raise Exit
     done
   with
   | Exit -> ());
  (* West *)
  (try
     for f = file - 1 downto 0 do
       let target = f + (rank * 8) in
       attacks := Bitboard.set !attacks target;
       if Bitboard.contains blockers target then raise Exit
     done
   with
   | Exit -> ());
  !attacks
;;

(** Compute bishop attacks from a square with given blockers (slow, for initialization) *)
let bishop_attacks_slow (sq : int) (blockers : Int64.t) : Int64.t =
  let file = sq mod 8 in
  let rank = sq / 8 in
  let attacks = ref 0L in
  (* Northeast *)
  (try
     for i = 1 to 7 do
       let f = file + i in
       let r = rank + i in
       if f > 7 || r > 7 then raise Exit;
       let target = f + (r * 8) in
       attacks := Bitboard.set !attacks target;
       if Bitboard.contains blockers target then raise Exit
     done
   with
   | Exit -> ());
  (* Northwest *)
  (try
     for i = 1 to 7 do
       let f = file - i in
       let r = rank + i in
       if f < 0 || r > 7 then raise Exit;
       let target = f + (r * 8) in
       attacks := Bitboard.set !attacks target;
       if Bitboard.contains blockers target then raise Exit
     done
   with
   | Exit -> ());
  (* Southeast *)
  (try
     for i = 1 to 7 do
       let f = file + i in
       let r = rank - i in
       if f > 7 || r < 0 then raise Exit;
       let target = f + (r * 8) in
       attacks := Bitboard.set !attacks target;
       if Bitboard.contains blockers target then raise Exit
     done
   with
   | Exit -> ());
  (* Southwest *)
  (try
     for i = 1 to 7 do
       let f = file - i in
       let r = rank - i in
       if f < 0 || r < 0 then raise Exit;
       let target = f + (r * 8) in
       attacks := Bitboard.set !attacks target;
       if Bitboard.contains blockers target then raise Exit
     done
   with
   | Exit -> ());
  !attacks
;;

(** Generate all possible blocker configurations for a mask *)
let generate_blocker_configs (mask : Int64.t) : Int64.t array =
  let bits = Bitboard.population mask in
  let num_configs = 1 lsl bits in
  let configs = Array.make num_configs 0L in
  (* Extract bit positions from mask *)
  let bit_positions = Array.make bits 0 in
  let idx = ref 0 in
  for sq = 0 to 63 do
    if Bitboard.contains mask sq
    then (
      bit_positions.(!idx) <- sq;
      incr idx)
  done;
  (* Generate all 2^bits configurations *)
  for i = 0 to num_configs - 1 do
    let config = ref 0L in
    for bit = 0 to bits - 1 do
      if i land (1 lsl bit) <> 0 then config := Bitboard.set !config bit_positions.(bit)
    done;
    configs.(i) <- !config
  done;
  configs
;;

(** Compute magic index *)
let magic_index (entry : magic_entry) (blockers : Int64.t) : int =
  let relevant = Int64.logand blockers entry.mask in
  let hash = Int64.mul relevant entry.magic in
  let index = Int64.to_int (Int64.shift_right_logical hash entry.shift) in
  entry.offset + index
;;

(** Pre-computed magic numbers for rooks (found via testing/research) *)
let rook_magic_numbers =
  [| 0x0080001020400080L
   ; 0x0040001000200040L
   ; 0x0080081000200080L
   ; 0x0080040800100080L
   ; 0x0080020400080080L
   ; 0x0080010200040080L
   ; 0x0080008001000200L
   ; 0x0080002040800100L
   ; 0x0000800020400080L
   ; 0x0000400020005000L
   ; 0x0000801000200080L
   ; 0x0000800800100080L
   ; 0x0000800400080080L
   ; 0x0000800200040080L
   ; 0x0000800100020080L
   ; 0x0000800040800100L
   ; 0x0000208000400080L
   ; 0x0000404000201000L
   ; 0x0000808010002000L
   ; 0x0000808008001000L
   ; 0x0000808004000800L
   ; 0x0000808002000400L
   ; 0x0000010100020004L
   ; 0x0000020000408104L
   ; 0x0000208080004000L
   ; 0x0000200040005000L
   ; 0x0000100080200080L
   ; 0x0000080080100080L
   ; 0x0000040080080080L
   ; 0x0000020080040080L
   ; 0x0000010080800200L
   ; 0x0000800080004100L
   ; 0x0000204000800080L
   ; 0x0000200040401000L
   ; 0x0000100080802000L
   ; 0x0000080080801000L
   ; 0x0000040080800800L
   ; 0x0000020080800400L
   ; 0x0000020001010004L
   ; 0x0000800040800100L
   ; 0x0000204000808000L
   ; 0x0000200040008080L
   ; 0x0000100020008080L
   ; 0x0000080010008080L
   ; 0x0000040008008080L
   ; 0x0000020004008080L
   ; 0x0000010002008080L
   ; 0x0000004081020004L
   ; 0x0000204000800080L
   ; 0x0000200040008080L
   ; 0x0000100020008080L
   ; 0x0000080010008080L
   ; 0x0000040008008080L
   ; 0x0000020004008080L
   ; 0x0000800100020080L
   ; 0x0000800041000080L
   ; 0x00FFFCDDFCED714AL
   ; 0x007FFCDDFCED714AL
   ; 0x003FFFCDFFD88096L
   ; 0x0000040810002101L
   ; 0x0001000204080011L
   ; 0x0001000204000801L
   ; 0x0001000082000401L
   ; 0x0001FFFAABFAD1A2L
  |]
;;

(** Pre-computed magic numbers for bishops *)
let bishop_magic_numbers =
  [| 0x0002020202020200L
   ; 0x0002020202020000L
   ; 0x0004010202000000L
   ; 0x0004040080000000L
   ; 0x0001104000000000L
   ; 0x0000821040000000L
   ; 0x0000410410400000L
   ; 0x0000104104104000L
   ; 0x0000040404040400L
   ; 0x0000020202020200L
   ; 0x0000040102020000L
   ; 0x0000040400800000L
   ; 0x0000011040000000L
   ; 0x0000008210400000L
   ; 0x0000004104104000L
   ; 0x0000002082082000L
   ; 0x0004000808080800L
   ; 0x0002000404040400L
   ; 0x0001000202020200L
   ; 0x0000800802004000L
   ; 0x0000800400A00000L
   ; 0x0000200100884000L
   ; 0x0000400082082000L
   ; 0x0000200041041000L
   ; 0x0002080010101000L
   ; 0x0001040008080800L
   ; 0x0000208004010400L
   ; 0x0000404004010200L
   ; 0x0000840000802000L
   ; 0x0000404002011000L
   ; 0x0000808001041000L
   ; 0x0000404000820800L
   ; 0x0001041000202000L
   ; 0x0000820800101000L
   ; 0x0000104400080800L
   ; 0x0000020080080080L
   ; 0x0000404040040100L
   ; 0x0000808100020100L
   ; 0x0001010100020800L
   ; 0x0000808080010400L
   ; 0x0000820820004000L
   ; 0x0000410410002000L
   ; 0x0000082088001000L
   ; 0x0000002011000800L
   ; 0x0000080100400400L
   ; 0x0001010101000200L
   ; 0x0002020202000400L
   ; 0x0001010101000200L
   ; 0x0000410410400000L
   ; 0x0000208208200000L
   ; 0x0000002084100000L
   ; 0x0000000020880000L
   ; 0x0000001002020000L
   ; 0x0000040408020000L
   ; 0x0004040404040000L
   ; 0x0002020202020000L
   ; 0x0000104104104000L
   ; 0x0000002082082000L
   ; 0x0000000020841000L
   ; 0x0000000000208800L
   ; 0x0000000010020200L
   ; 0x0000000404080200L
   ; 0x0000040404040400L
   ; 0x0002020202020200L
  |]
;;

(** Initialize magic bitboards *)
let init () =
  let rook_offset = ref 0 in
  let bishop_offset = ref 0 in
  (* Initialize rook magics *)
  for sq = 0 to 63 do
    let mask = rook_mask sq in
    let bits = Bitboard.population mask in
    let magic = rook_magic_numbers.(sq) in
    let shift = 64 - bits in
    rook_magics.(sq) <- { mask; magic; shift; offset = !rook_offset };
    (* Generate all blocker configurations and fill attack table *)
    let configs = generate_blocker_configs mask in
    Array.iter
      (fun blockers ->
         let attacks = rook_attacks_slow sq blockers in
         let idx = magic_index rook_magics.(sq) blockers in
         rook_attacks_table.(idx) <- attacks)
      configs;
    rook_offset := !rook_offset + (1 lsl bits)
  done;
  (* Initialize bishop magics *)
  for sq = 0 to 63 do
    let mask = bishop_mask sq in
    let bits = Bitboard.population mask in
    let magic = bishop_magic_numbers.(sq) in
    let shift = 64 - bits in
    bishop_magics.(sq) <- { mask; magic; shift; offset = !bishop_offset };
    (* Generate all blocker configurations and fill attack table *)
    let configs = generate_blocker_configs mask in
    Array.iter
      (fun blockers ->
         let attacks = bishop_attacks_slow sq blockers in
         let idx = magic_index bishop_magics.(sq) blockers in
         bishop_attacks_table.(idx) <- attacks)
      configs;
    bishop_offset := !bishop_offset + (1 lsl bits)
  done
;;

(** Fast rook attack lookup *)
let rook_attacks (sq : int) (blockers : Int64.t) : Int64.t =
  let idx = magic_index rook_magics.(sq) blockers in
  rook_attacks_table.(idx)
;;

(** Fast bishop attack lookup *)
let bishop_attacks (sq : int) (blockers : Int64.t) : Int64.t =
  let idx = magic_index bishop_magics.(sq) blockers in
  bishop_attacks_table.(idx)
;;

(** Queen attacks (combination of rook and bishop) *)
let queen_attacks (sq : int) (blockers : Int64.t) : Int64.t =
  Int64.logor (rook_attacks sq blockers) (bishop_attacks sq blockers)
;;
