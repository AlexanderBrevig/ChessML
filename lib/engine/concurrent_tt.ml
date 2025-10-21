(** Concurrent TT - Thread-safe transposition table for parallel search
    
    Lock-striped transposition table allowing concurrent read/write access from
    multiple threads with minimal contention. Uses separate mutexes for different
    table segments to maximize parallelism. Supports standard TT operations:
    store, lookup, and clear with atomic updates.
    
    Used by: Lazy SMP parallel search
*)

open Chessml_core

type entry_type =
  | Exact
  | LowerBound
  | UpperBound

type tt_entry =
  { key : Int64.t
  ; score : int
  ; depth : int
  ; best_move : Move.t option
  ; entry_type : entry_type
  }

type t =
  { table : tt_entry option Atomic.t array
  ; locks : Mutex.t array
  ; size : int
  ; num_locks : int
  }

(** Create a concurrent transposition table
    @param size Number of entries (power of 2 recommended)
    @param num_locks Number of locks for striping (typically 256-1024)
*)
let create ?(num_locks = 512) size =
  { table = Array.init size (fun _ -> Atomic.make None)
  ; locks = Array.init num_locks (fun _ -> Mutex.create ())
  ; size
  ; num_locks
  }
;;

(** Get index into hash table *)
let index tt key = Int64.to_int (Int64.unsigned_rem key (Int64.of_int tt.size))

(** Get lock index for a given key *)
let lock_index tt key = Int64.to_int (Int64.unsigned_rem key (Int64.of_int tt.num_locks))

(** Store an entry in the transposition table
    Uses lock striping to allow concurrent access
*)
let store tt key depth score best_move entry_type =
  let idx = index tt key in
  let lock_idx = lock_index tt key in
  let entry = { key; score; depth; best_move; entry_type } in
  (* Lock only the relevant stripe *)
  Mutex.lock tt.locks.(lock_idx);
  (* Replace-by-depth strategy: only replace if deeper or same depth *)
  let should_store =
    match Atomic.get tt.table.(idx) with
    | None -> true
    | Some existing ->
      (* Replace if we have deeper search or same position *)
      depth >= existing.depth || Int64.equal key existing.key
  in
  if should_store then Atomic.set tt.table.(idx) (Some entry);
  Mutex.unlock tt.locks.(lock_idx)
;;

(** Lookup an entry in the transposition table
    No locking needed for reads (atomic load)
*)
let lookup tt key =
  let idx = index tt key in
  match Atomic.get tt.table.(idx) with
  | Some entry when Int64.equal entry.key key -> Some entry
  | _ -> None
;;

(** Clear the transposition table *)
let clear tt =
  for i = 0 to tt.size - 1 do
    Atomic.set tt.table.(i) None
  done
;;

(** Get statistics about table usage *)
let stats tt =
  let used = ref 0 in
  for i = 0 to tt.size - 1 do
    match Atomic.get tt.table.(i) with
    | Some _ -> incr used
    | None -> ()
  done;
  let usage_pct = float_of_int !used /. float_of_int tt.size *. 100.0 in
  Printf.sprintf "TT: %d/%d entries (%.1f%% full)" !used tt.size usage_pct
;;
