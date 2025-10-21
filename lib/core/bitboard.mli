(** Bitboard module interface *)

open Types

type t = Int64.t

val empty : t
val full : t
val file_a : t
val file_b : t
val file_c : t
val file_d : t
val file_e : t
val file_f : t
val file_g : t
val file_h : t
val not_file_a : t
val not_file_h : t
val rank_1 : t
val rank_2 : t
val rank_3 : t
val rank_4 : t
val rank_5 : t
val rank_6 : t
val rank_7 : t
val rank_8 : t
val a1_h8_diag : t
val h1_a8_diag : t
val light_squares : t
val dark_squares : t
val edges : t
val corners : t
val center : t
val back_ranks : t
val of_square : Square.t -> t
val of_file : file -> t
val of_rank : rank -> t
val first_rank : color -> t
val second_rank : color -> t
val seventh_rank : color -> t
val eighth_rank : color -> t
val set : t -> Square.t -> t
val clear : t -> Square.t -> t
val toggle : t -> Square.t -> t
val contains : t -> Square.t -> bool
val is_empty : t -> bool
val is_not_empty : t -> bool
val population : t -> int
val lsb : t -> Square.t option
val msb : t -> Square.t option
val pop_lsb : t -> Square.t option * t
val iter : (Square.t -> unit) -> t -> unit
val fold : (Square.t -> 'a -> 'a) -> t -> 'a -> 'a
val to_list : t -> Square.t list
val of_list : Square.t list -> t
val north : t -> t
val south : t -> t
val east : t -> t
val west : t -> t
val north_east : t -> t
val north_west : t -> t
val south_east : t -> t
val south_west : t -> t
val to_string : t -> string
