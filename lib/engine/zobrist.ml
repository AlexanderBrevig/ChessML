(** Zobrist - Incremental position hashing for transposition tables
    
    Implements Zobrist hashing to generate unique 64-bit keys for chess positions.
    Uses pre-computed random numbers for each piece/square combination, castling
    rights, en passant files, and side to move. Supports incremental updates for
    efficient hash computation during move making.
    
    Used by: Transposition tables, opening books, repetition detection
*)

open Chessml_core
open Types

type t = Int64.t

(** XoShiRo256** PRNG for generating random numbers *)
module XoShiRo = struct
  let seeds =
    [| 0b1001000111000101101010110011110011101011111111010101101001110001L
     ; 0b0000011010111010001001010011101110011101110110001001011111001101L
     ; 0b1000000000010101101101011110010110011100110000100111010111101001L
     ; 0b1111100011110100001001111111110001010100000100011101111001010011L
    |]
  ;;

  let create () = Array.copy seeds

  let next state =
    let open Int64 in
    let s = Array.copy state in
    let result =
      let tmp = mul s.(1) 5L in
      let rotated = logor (shift_left tmp 7) (shift_right_logical tmp (64 - 7)) in
      mul rotated 9L
    in
    let t = shift_left s.(1) 17 in
    s.(2) <- logxor s.(2) s.(0);
    s.(3) <- logxor s.(3) s.(1);
    s.(1) <- logxor s.(1) s.(2);
    s.(0) <- logxor s.(0) s.(3);
    s.(2) <- logxor s.(2) t;
    let s3 = s.(3) in
    s.(3) <- logor (shift_left s3 45) (shift_right_logical s3 (64 - 45));
    result, s
  ;;
end

(** Constants *)
let num_squares = 64

let num_pieces = 12 (* 6 piece kinds * 2 colors *)

let num_castling_rights =
  16 (* 4 bits: white short, white long, black short, black long *)
;;

(** Zobrist hash table *)
type zobrist_table =
  { piece_keys : int64 array array (* [square][piece] *)
  ; ep_keys : int64 array (* [square] *)
  ; castling_keys : int64 array (* [castling_index] *)
  ; color_key : int64 array (* [color] *)
  }

(** Initialize the Zobrist hash table with random keys *)
let init_zobrist_table () =
  let prng = ref (XoShiRo.create ()) in
  (* Helper to get next random number *)
  let next_random () =
    let value, new_state = XoShiRo.next !prng in
    prng := new_state;
    value
  in
  (* Initialize piece keys *)
  let piece_keys =
    Array.init num_squares (fun sq ->
      let keys = Array.make num_pieces 0L in
      for piece_idx = 0 to num_pieces - 1 do
        keys.(piece_idx) <- next_random ()
      done;
      (* Initialize EP keys for valid ranks (rank 3 and rank 6) *)
      let rank = sq / 8 in
      if rank = 2 || rank = 5
      then (* Rank3 = 2, Rank6 = 5 *)
        ignore (next_random ()) (* Just advance the PRNG *);
      keys)
  in
  (* Initialize EP keys (only for ranks 3 and 6) *)
  let ep_keys =
    Array.init num_squares (fun sq ->
      let rank = sq / 8 in
      if rank = 2 || rank = 5
      then (* Rank3 = 2, Rank6 = 5 *)
        next_random ()
      else 0L)
  in
  (* Initialize castling keys *)
  let castling_keys = Array.init num_castling_rights (fun _ -> next_random ()) in
  (* Initialize color keys (only Black has a key, White is 0) *)
  let color_key = Array.make 2 0L in
  color_key.(Color.to_int Black) <- next_random ();
  { piece_keys; ep_keys; castling_keys; color_key }
;;

(** Global Zobrist table, initialized once *)
let zobrist_table = init_zobrist_table ()

(** Get piece index for Zobrist hashing *)
let piece_index piece =
  let color_offset = Color.to_int piece.color * 6 in
  let kind_offset = PieceKind.to_int piece.kind in
  color_offset + kind_offset
;;

(** Get castling rights index *)
let castling_rights_index (rights : Position.castling_rights array) =
  let open Position in
  let white_idx =
    let w = rights.(Color.to_int White) in
    (if Option.is_some w.short then 1 else 0) lor if Option.is_some w.long then 2 else 0
  in
  let black_idx =
    let b = rights.(Color.to_int Black) in
    (if Option.is_some b.short then 1 else 0) lor if Option.is_some b.long then 2 else 0
  in
  (white_idx lsl 2) lor black_idx
;;

(** Compute Zobrist hash from raw components (for Position.of_fen) *)
let compute_from_raw
      (board : piece option array)
      (side_to_move : color)
      (castling_rights : Position.castling_rights array)
      (ep_square : Square.t option)
  =
  let open Int64 in
  let key = ref 0L in
  (* Hash all pieces *)
  Array.iteri
    (fun sq piece_opt ->
       match piece_opt with
       | Some piece ->
         let piece_idx = piece_index piece in
         key := logxor !key zobrist_table.piece_keys.(sq).(piece_idx)
       | None -> ())
    board;
  (* Hash en passant *)
  (match ep_square with
   | Some ep_sq -> key := logxor !key zobrist_table.ep_keys.(ep_sq)
   | None -> ());
  (* Hash castling *)
  let castling_idx = castling_rights_index castling_rights in
  key := logxor !key zobrist_table.castling_keys.(castling_idx);
  (* Hash side to move *)
  let color_idx = Color.to_int side_to_move in
  key := logxor !key zobrist_table.color_key.(color_idx);
  !key
;;

(** Compute Zobrist hash for a position *)
let compute pos =
  let open Int64 in
  let key = ref 0L in
  (* Hash all pieces on the board *)
  let board = Position.board pos in
  Array.iteri
    (fun sq piece_opt ->
       match piece_opt with
       | Some piece ->
         let piece_idx = piece_index piece in
         key := logxor !key zobrist_table.piece_keys.(sq).(piece_idx)
       | None -> ())
    board;
  (* Hash en passant square if it exists *)
  (match Position.ep_square pos with
   | Some ep_sq -> key := logxor !key zobrist_table.ep_keys.(ep_sq)
   | None -> ());
  (* Hash castling rights *)
  let castling_idx = castling_rights_index (Position.castling_rights pos) in
  key := logxor !key zobrist_table.castling_keys.(castling_idx);
  (* Hash side to move *)
  let color_idx = Color.to_int (Position.side_to_move pos) in
  key := logxor !key zobrist_table.color_key.(color_idx);
  !key
;;

(** Hash/unhash a piece at a square *)
let hash_piece key square piece =
  let piece_idx = piece_index piece in
  Int64.logxor key zobrist_table.piece_keys.(square).(piece_idx)
;;

(** Hash/unhash an en passant square *)
let hash_ep_square key square = Int64.logxor key zobrist_table.ep_keys.(square)

(** Hash/unhash castling rights *)
let hash_castling_rights key rights =
  let idx = castling_rights_index rights in
  Int64.logxor key zobrist_table.castling_keys.(idx)
;;

(** Hash/unhash side to move *)
let hash_side_to_move key color =
  let color_idx = Color.to_int color in
  Int64.logxor key zobrist_table.color_key.(color_idx)
;;
