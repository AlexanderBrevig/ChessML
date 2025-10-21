(** Square module interface *)

open Types

type t = int

val make : file -> rank -> t
val file : t -> file
val rank : t -> rank
val to_int : t -> int
val of_int : int -> t
val a1 : t
val a2 : t
val a3 : t
val a4 : t
val a5 : t
val a6 : t
val a7 : t
val a8 : t
val b1 : t
val b2 : t
val b3 : t
val b4 : t
val b5 : t
val b6 : t
val b7 : t
val b8 : t
val c1 : t
val c2 : t
val c3 : t
val c4 : t
val c5 : t
val c6 : t
val c7 : t
val c8 : t
val d1 : t
val d2 : t
val d3 : t
val d4 : t
val d5 : t
val d6 : t
val d7 : t
val d8 : t
val e1 : t
val e2 : t
val e3 : t
val e4 : t
val e5 : t
val e6 : t
val e7 : t
val e8 : t
val f1 : t
val f2 : t
val f3 : t
val f4 : t
val f5 : t
val f6 : t
val f7 : t
val f8 : t
val g1 : t
val g2 : t
val g3 : t
val g4 : t
val g5 : t
val g6 : t
val g7 : t
val g8 : t
val h1 : t
val h2 : t
val h3 : t
val h4 : t
val h5 : t
val h6 : t
val h7 : t
val h8 : t
val all : t list
val of_uci : string -> t
val to_uci : t -> string
val to_string : t -> string
val distance : t -> t -> int
val forward : t -> color -> int -> t option
val backward : t -> color -> int -> t option
val rank_relative_to : t -> color -> t
