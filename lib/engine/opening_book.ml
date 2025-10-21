(** Opening Book - Polyglot opening book support
    
    Reads and queries opening books in Polyglot (.bin) format. Provides weighted
    random move selection from book entries for varied opening play. Supports
    binary search for fast lookups and filtering by minimum weight threshold.
    
    File format: Binary sorted by Zobrist key, 16 bytes per entry
    Reference: http://hgm.nubati.net/book_format.html
*)

open Chessml_core

(** Book file handle *)
type book =
  { filename : string
  ; channel : in_channel option ref
  ; num_entries : int
  }

(** Open a polyglot book file *)
let open_book (filename : string) : book option =
  try
    let ch = open_in_bin filename in
    seek_in ch 0;
    let file_size = in_channel_length ch in
    let num_entries = file_size / 16 in
    close_in ch;
    Some { filename; channel = ref None; num_entries }
  with
  | _ -> None
;;

(** Close book file *)
let close_book (book : book) : unit =
  match !(book.channel) with
  | Some ch ->
    close_in ch;
    book.channel := None
  | None -> ()
;;

(** Get channel, opening if needed *)
let get_channel (book : book) : in_channel =
  match !(book.channel) with
  | Some ch -> ch
  | None ->
    let ch = open_in_bin book.filename in
    book.channel := Some ch;
    ch
;;

(** Binary search for entries matching a key *)
let find_entries (book : book) (key : Int64.t) : Polyglot.entry list =
  try
    let ch = get_channel book in
    (* Binary search for first matching entry *)
    let rec binary_search low high =
      if low > high
      then low
      else (
        let mid = (low + high) / 2 in
        seek_in ch (mid * 16);
        match Polyglot.read_entry ch with
        | None -> low
        | Some entry ->
          let cmp = Int64.compare entry.key key in
          if cmp < 0
          then binary_search (mid + 1) high
          else if cmp > 0
          then binary_search low (mid - 1)
          else (
            (* Found match, search backwards for first *)
            let rec find_first pos =
              if pos = 0
              then 0
              else (
                seek_in ch ((pos - 1) * 16);
                match Polyglot.read_entry ch with
                | Some entry when entry.key = key -> find_first (pos - 1)
                | _ -> pos)
            in
            find_first mid))
    in
    let first_idx = binary_search 0 (book.num_entries - 1) in
    (* Collect all entries with matching key *)
    let rec collect_entries pos acc =
      if pos >= book.num_entries
      then List.rev acc
      else (
        seek_in ch (pos * 16);
        match Polyglot.read_entry ch with
        | Some entry when entry.key = key -> collect_entries (pos + 1) (entry :: acc)
        | _ -> List.rev acc)
    in
    collect_entries first_idx []
  with
  | _ -> []
;;

(** Probe book for position - returns list of (move, weight) *)
let probe (book : book option) (pos : Position.t) : (Move.t * int) list =
  match book with
  | None -> []
  | Some book ->
    (* Get zobrist key, compute it if it's zero *)
    let key =
      let k = Position.key pos in
      if k = 0L then Zobrist.compute pos else k
    in
    let entries = find_entries book key in
    (* Decode moves and filter valid ones *)
    List.filter_map
      (fun (entry : Polyglot.entry) ->
         match Polyglot.decode_move pos entry.Polyglot.move with
         | Some move -> Some (move, entry.Polyglot.weight)
         | None -> None)
      entries
;;

(** Select best move from book entries *)
let select_best (entries : (Move.t * int) list) : Move.t option =
  if entries = []
  then None
  else (
    (* Sort by weight descending *)
    let sorted = List.sort (fun (_, w1) (_, w2) -> compare w2 w1) entries in
    Some (fst (List.hd sorted)))
;;

(** Select random move weighted by popularity *)
let select_random (entries : (Move.t * int) list) : Move.t option =
  if entries = []
  then None
  else (
    let total_weight = List.fold_left (fun acc (_, w) -> acc + w) 0 entries in
    if total_weight = 0
    then (
      (* All weights are 0, pick uniformly *)
      let idx = Random.int (List.length entries) in
      Some (fst (List.nth entries idx)))
    else (
      (* Weighted random selection *)
      let rand = Random.int total_weight in
      let rec select sum = function
        | [] -> None
        | (move, weight) :: rest ->
          let new_sum = sum + weight in
          if rand < new_sum then Some move else select new_sum rest
      in
      select 0 entries))
;;

(** Get book move for position *)
let get_book_move ?(random = false) (book : book option) (pos : Position.t)
  : Move.t option
  =
  let entries = probe book pos in
  if random then select_random entries else select_best entries
;;

(** Get book statistics for position *)
let get_book_stats (book : book option) (pos : Position.t) : (Move.t * int * float) list =
  let entries = probe book pos in
  if entries = []
  then []
  else (
    let total_weight = List.fold_left (fun acc (_, w) -> acc + w) 0 entries in
    List.map
      (fun (move, weight) ->
         let percentage =
           if total_weight > 0
           then 100.0 *. float_of_int weight /. float_of_int total_weight
           else 0.0
         in
         move, weight, percentage)
      entries
    |> List.sort (fun (_, w1, _) (_, w2, _) -> compare w2 w1))
;;
