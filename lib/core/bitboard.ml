(** Bitboard - Efficient board representation using 64-bit integers
    
    Represents the chess board as a bitboard where each bit corresponds to a square.
    Provides fast set operations (union, intersection, complement) and efficient
    iteration over occupied squares. Uses CPU intrinsics (CTZ/CLZ) for performance.
    
    Little-Endian Rank-File (LERF) mapping: a1=0, b1=1, ..., g8=62, h8=63
*)

open Types

type t = Int64.t

(* External C function for fast LSB finding using CPU intrinsics *)
external find_lsb_fast : Int64.t -> int = "caml_bitboard_ctz"

let empty = 0L
let full = Int64.minus_one

(* File bitboards *)
let file_a = 0x0101010101010101L
let file_b = 0x0202020202020202L
let file_c = 0x0404040404040404L
let file_d = 0x0808080808080808L
let file_e = 0x1010101010101010L
let file_f = 0x2020202020202020L
let file_g = 0x4040404040404040L
let file_h = 0x8080808080808080L
let not_file_a = 0xfefefefefefefefeL
let not_file_h = 0x7f7f7f7f7f7f7f7fL

(* Rank bitboards *)
let rank_1 = 0x00000000000000FFL
let rank_2 = 0x000000000000FF00L
let rank_3 = 0x0000000000FF0000L
let rank_4 = 0x00000000FF000000L
let rank_5 = 0x000000FF00000000L
let rank_6 = 0x0000FF0000000000L
let rank_7 = 0x00FF000000000000L
let rank_8 = 0xFF00000000000000L

(* Special bitboards *)
let a1_h8_diag = 0x8040201008040201L
let h1_a8_diag = 0x0102040810204080L
let light_squares = 0x55AA55AA55AA55AAL
let dark_squares = 0xAA55AA55AA55AA55L
let edges = 0xFF818181818181FFL
let corners = 0x8100000000000081L
let center = 0x0000001818000000L
let back_ranks = 0xFF000000000000FFL
let of_square sq = Int64.shift_left 1L sq [@@inline always]
let of_file file = Int64.shift_left file_a (File.to_int file) [@@inline always]
let of_rank rank = Int64.shift_left rank_1 (Rank.to_int rank * 8) [@@inline always]

let first_rank color =
  match color with
  | White -> rank_1
  | Black -> rank_8
;;

let second_rank color =
  match color with
  | White -> rank_2
  | Black -> rank_7
;;

let seventh_rank color =
  match color with
  | White -> rank_7
  | Black -> rank_2
;;

let eighth_rank color =
  match color with
  | White -> rank_8
  | Black -> rank_1
;;

let set bb sq = Int64.logor bb (of_square sq) [@@inline always]
let clear bb sq = Int64.logand bb (Int64.lognot (of_square sq)) [@@inline always]
let toggle bb sq = Int64.logxor bb (of_square sq) [@@inline always]
let contains bb sq = Int64.logand bb (of_square sq) <> 0L [@@inline always]
let is_empty bb = bb = 0L [@@inline always]
let is_not_empty bb = bb <> 0L [@@inline always]

let population bb =
  let rec count acc n =
    if n = 0L then acc else count (acc + 1) (Int64.logand n (Int64.sub n 1L))
  in
  count 0 bb
;;

let lsb bb =
  if bb = 0L
  then None
  else (
    let pos = find_lsb_fast bb in
    if pos = -1 then None else Some pos)
;;

let msb bb =
  if bb = 0L
  then None
  else (
    let rec find_msb i =
      if i < 0
      then None
      else if Int64.logand bb (Int64.shift_left 1L i) <> 0L
      then Some i
      else find_msb (i - 1)
    in
    find_msb 63)
;;

(* Pop LSB: returns (position option, new bitboard)
 * Note: This function still allocates for API compatibility.
 * For performance-critical code, use find_lsb_fast + inline bit clearing instead. *)
let pop_lsb bb =
  if bb = 0L
  then None, bb
  else (
    let sq = find_lsb_fast bb in
    Some sq, Int64.logand bb (Int64.sub bb 1L))
;;

(* Allocation-free iteration using direct bit manipulation *)
let iter f bb =
  let rec loop b =
    if b <> 0L
    then (
      let sq = find_lsb_fast b in
      if sq >= 0
      then (
        f sq;
        (* Clear the LSB: b & (b - 1) *)
        loop (Int64.logand b (Int64.sub b 1L))))
      [@@inline always]
  in
  loop bb
[@@inline always]
;;

(* Allocation-free fold using direct bit manipulation *)
let fold f bb acc =
  let rec loop b acc =
    if b = 0L
    then acc
    else (
      let sq = find_lsb_fast b in
      if sq >= 0
      then
        (* Clear the LSB and continue *)
        loop (Int64.logand b (Int64.sub b 1L)) (f sq acc)
      else acc)
      [@@inline always]
  in
  loop bb acc
[@@inline always]
;;

let to_list bb = List.rev (fold (fun sq acc -> sq :: acc) bb [])
let of_list squares = List.fold_left (fun bb sq -> set bb sq) empty squares
let north bb = Int64.shift_left bb 8 [@@inline always]
let south bb = Int64.shift_right_logical bb 8 [@@inline always]
let east bb = Int64.logand (Int64.shift_left bb 1) not_file_a [@@inline always]
let west bb = Int64.logand (Int64.shift_right_logical bb 1) not_file_h [@@inline always]
let north_east bb = north (east bb) [@@inline always]
let north_west bb = north (west bb) [@@inline always]
let south_east bb = south (east bb) [@@inline always]
let south_west bb = south (west bb) [@@inline always]

let to_string bb =
  let buf = Buffer.create 144 in
  for rank = 7 downto 0 do
    Buffer.add_string buf (string_of_int (rank + 1));
    Buffer.add_string buf "| ";
    for file = 0 to 7 do
      let sq = file + (rank * 8) in
      if contains bb sq then Buffer.add_string buf "X " else Buffer.add_string buf ". "
    done;
    Buffer.add_char buf '\n'
  done;
  Buffer.add_string buf "  ----------------\n";
  Buffer.add_string buf "   a b c d e f g h\n";
  Buffer.contents buf
;;
