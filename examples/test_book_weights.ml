(** Quick test of book weight distribution *)

open Chessml

let () =
  Printf.printf "=== Book Weight Distribution Test ===\n\n";
  
  match Opening_book.open_book "book.bin" with
  | None -> 
    Printf.printf "❌ Could not open book.bin\n";
    exit 1
  | Some book ->
    let pos = Position.default () in
    let stats = Opening_book.get_book_stats (Some book) pos in
    
    Printf.printf "Starting position moves (sorted by weight):\n\n";
    List.iteri (fun i (move, weight, pct) ->
      if i < 10 then
        Printf.printf "%2d. %-6s | weight: %5d | %.1f%%\n" 
          (i+1) (Move.to_uci move) weight pct
    ) stats;
    
    Printf.printf "\n\nSampling with ~random:true (weighted) 1000 times:\n\n";
    
    let move_counts = Hashtbl.create 20 in
    for _i = 1 to 1000 do
      match Opening_book.get_book_move ~random:true (Some book) pos with
      | Some mv ->
        let uci = Move.to_uci mv in
        let count = try Hashtbl.find move_counts uci with Not_found -> 0 in
        Hashtbl.replace move_counts uci (count + 1)
      | None -> ()
    done;
    
    let sorted_results = 
      Hashtbl.fold (fun mv count acc -> (mv, count) :: acc) move_counts []
      |> List.sort (fun (_, c1) (_, c2) -> compare c2 c1)
    in
    
    List.iteri (fun i (mv, count) ->
      if i < 10 then
        Printf.printf "%2d. %-6s: %3d times (%.1f%%)\n" 
          (i+1) mv count (100.0 *. float_of_int count /. 1000.0)
    ) sorted_results;
    
    Printf.printf "\n\nExpected behavior:\n";
    Printf.printf "- Higher weight moves (e4, d4, c4, Nf3) should appear most often\n";
    Printf.printf "- Lower weight moves (h3, a4, etc) should appear rarely\n";
    Printf.printf "- Distribution should roughly match book weights\n";
    
    Opening_book.close_book book
;;
