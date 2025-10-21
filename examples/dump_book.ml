let () =
  Printf.printf "Opening Book Contents\n";
  Printf.printf "=====================\n\n";
  let ch = open_in_bin "book.bin" in
  let read_u16 () =
    let b1 = input_byte ch in
    let b2 = input_byte ch in
    (b1 lsl 8) lor b2
  in
  let read_u32 () =
    let w1 = read_u16 () in
    let w2 = read_u16 () in
    Int64.logor (Int64.shift_left (Int64.of_int w1) 16) (Int64.of_int w2)
  in
  let read_u64 () =
    let d1 = read_u32 () in
    let d2 = read_u32 () in
    Int64.logor (Int64.shift_left d1 32) d2
  in
  let rec read_entries acc =
    try
      let key = read_u64 () in
      let move = read_u16 () in
      let weight = read_u16 () in
      let _ = read_u32 () in
      (* learn *)
      Printf.printf "Key: 0x%016Lx  Move: 0x%04x  Weight: %d\n" key move weight;
      read_entries (acc + 1)
    with
    | End_of_file -> acc
  in
  let count = read_entries 0 in
  close_in ch;
  Printf.printf "\nTotal: %d entries\n" count
;;
