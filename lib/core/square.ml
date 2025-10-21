(** Square - Chess board square representation and utilities
    
    Represents individual squares on the chess board using integers 0-63.
    Provides conversions between square indices, algebraic notation (e.g., "e4"),
    and file/rank coordinates. Supports arithmetic and color determination.
    
    Uses LERF mapping: a1=0, b1=1, ..., g8=62, h8=63
*)

open Types

(** A square on the chess board, represented using least-significant file mapping *)
type t = int

let make file rank = File.to_int file + (Rank.to_int rank * 8)
let file sq = File.of_int (sq mod 8)
let rank sq = Rank.of_int (sq / 8)
let to_int sq = sq

let of_int i =
  if i < 0 || i > 63 then invalid_arg "Square.of_int: value must be in [0, 63]" else i
;;

(* Named squares *)
let a1 = make FileA Rank1
let a2 = make FileA Rank2
let a3 = make FileA Rank3
let a4 = make FileA Rank4
let a5 = make FileA Rank5
let a6 = make FileA Rank6
let a7 = make FileA Rank7
let a8 = make FileA Rank8
let b1 = make FileB Rank1
let b2 = make FileB Rank2
let b3 = make FileB Rank3
let b4 = make FileB Rank4
let b5 = make FileB Rank5
let b6 = make FileB Rank6
let b7 = make FileB Rank7
let b8 = make FileB Rank8
let c1 = make FileC Rank1
let c2 = make FileC Rank2
let c3 = make FileC Rank3
let c4 = make FileC Rank4
let c5 = make FileC Rank5
let c6 = make FileC Rank6
let c7 = make FileC Rank7
let c8 = make FileC Rank8
let d1 = make FileD Rank1
let d2 = make FileD Rank2
let d3 = make FileD Rank3
let d4 = make FileD Rank4
let d5 = make FileD Rank5
let d6 = make FileD Rank6
let d7 = make FileD Rank7
let d8 = make FileD Rank8
let e1 = make FileE Rank1
let e2 = make FileE Rank2
let e3 = make FileE Rank3
let e4 = make FileE Rank4
let e5 = make FileE Rank5
let e6 = make FileE Rank6
let e7 = make FileE Rank7
let e8 = make FileE Rank8
let f1 = make FileF Rank1
let f2 = make FileF Rank2
let f3 = make FileF Rank3
let f4 = make FileF Rank4
let f5 = make FileF Rank5
let f6 = make FileF Rank6
let f7 = make FileF Rank7
let f8 = make FileF Rank8
let g1 = make FileG Rank1
let g2 = make FileG Rank2
let g3 = make FileG Rank3
let g4 = make FileG Rank4
let g5 = make FileG Rank5
let g6 = make FileG Rank6
let g7 = make FileG Rank7
let g8 = make FileG Rank8
let h1 = make FileH Rank1
let h2 = make FileH Rank2
let h3 = make FileH Rank3
let h4 = make FileH Rank4
let h5 = make FileH Rank5
let h6 = make FileH Rank6
let h7 = make FileH Rank7
let h8 = make FileH Rank8
let all = List.init 64 (fun i -> i)

let of_uci str =
  if String.length str <> 2
  then invalid_arg ("Square.of_uci: invalid UCI string: " ^ str)
  else (
    let file = File.of_char str.[0] in
    let rank = Rank.of_char str.[1] in
    make file rank)
;;

let to_uci sq =
  let f = File.to_char (file sq) in
  let r = Rank.to_char (rank sq) in
  String.make 1 f ^ String.make 1 r
;;

let to_string = to_uci

let distance sq1 sq2 =
  let f1 = File.to_int (file sq1) in
  let f2 = File.to_int (file sq2) in
  let r1 = Rank.to_int (rank sq1) in
  let r2 = Rank.to_int (rank sq2) in
  abs (f1 - f2) + abs (r1 - r2)
;;

let forward sq color n =
  let r = Rank.to_int (rank sq) in
  let new_rank =
    match color with
    | White -> r + n
    | Black -> r - n
  in
  if new_rank < 0 || new_rank > 7
  then None
  else Some (make (file sq) (Rank.of_int new_rank))
;;

let backward sq color n = forward sq (Color.opponent color) n

let rank_relative_to sq color =
  match color with
  | White -> sq
  | Black -> make (file sq) (Rank.of_int (7 - Rank.to_int (rank sq)))
;;
