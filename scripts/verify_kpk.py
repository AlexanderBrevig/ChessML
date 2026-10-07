#!/usr/bin/env python3
"""Check ChessML's KPK table against the Syzygy tablebase.

    dune exec --profile=release examples/kpk_dump.exe > kpk.txt
    python3 scripts/verify_kpk.py kpk.txt <dir with KPvK.rtbw>

Needs python-chess and KPvK.rtbw (https://tablebase.sesse.net/syzygy/3-4-5/).
"""
import sys
import chess
import chess.syzygy

dump, tb_dir = sys.argv[1], sys.argv[2]
tablebase = chess.syzygy.open_tablebase(tb_dir)
checked = mismatches = 0
for line in open(dump):
    fen, ours = line.rsplit(" ", 1)
    board = chess.Board(fen)
    wdl = tablebase.probe_wdl(board)
    white_wins = wdl > 0 if board.turn == chess.WHITE else wdl < 0
    checked += 1
    if (ours.strip() == "W") != white_wins:
        mismatches += 1
        if mismatches <= 10:
            print("mismatch:", fen, "ours", ours.strip(), "syzygy wdl", wdl)
print(f"{checked} positions checked, {mismatches} mismatches")
sys.exit(1 if mismatches else 0)
