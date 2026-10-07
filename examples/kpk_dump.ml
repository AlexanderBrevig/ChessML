(** Print every legal K+P vs K position (White pawn, either side to move) with the
    KPK table's verdict, W or D. Compare with Syzygy using scripts/verify_kpk.py. *)
open Chessml

let () =
  let fen_of wk bk p stm =
    let b = Array.make 64 '.' in
    b.(wk) <- 'K';
    b.(bk) <- 'k';
    b.(p) <- 'P';
    let rows =
      List.init 8 (fun i ->
        let r = 7 - i in
        let buf = Buffer.create 8
        and e = ref 0 in
        for f = 0 to 7 do
          match b.((r * 8) + f) with
          | '.' -> incr e
          | c ->
            if !e > 0 then Buffer.add_string buf (string_of_int !e);
            e := 0;
            Buffer.add_char buf c
        done;
        if !e > 0 then Buffer.add_string buf (string_of_int !e);
        Buffer.contents buf)
    in
    String.concat "/" rows ^ (if stm then " w" else " b") ^ " - - 0 1"
  in
  for p = 8 to 55 do
    for wk = 0 to 63 do
      for bk = 0 to 63 do
        List.iter
          (fun stm ->
             let pawn_attacks_bk = Bitboard.contains (Movegen.pawn_attacks p White) bk in
             if
               wk <> bk
               && wk <> p
               && bk <> p
               && Square.distance wk bk > 1
               && not (stm && pawn_attacks_bk)
             then
               Printf.printf
                 "%s %c\n"
                 (fen_of wk bk p stm)
                 (if Kpk.wins ~strong_king:wk ~weak_king:bk ~pawn:p ~strong_to_move:stm
                  then 'W'
                  else 'D'))
          [ true; false ]
      done
    done
  done
;;
