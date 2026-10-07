(** Specialized evaluation for endgames recognized by their material *)

(** Score that marks a won endgame: above any normal evaluation, below mates *)
val known_win : int

(** Evaluation of a recognized endgame from the side to move's perspective, or
    [None] when the general evaluation applies *)
val evaluate : Position.t -> int option
