#!/usr/bin/env fish
# Script to estimate engine Elo by playing against Stockfish at various skill levels

# Requires: cutechess-cli, stockfish

set ENGINE_PATH (pwd)/_build/default/bin/chessml_xboard.exe
set BOOK_PATH (pwd)/book.bin
set GAMES_PER_LEVEL 2
set TIME_CONTROL 40/300 # 300 seconds (5 minutes) for 40 moves

echo "Building engine..."
eval (opam env)
dune build --profile=release

if not test -f $ENGINE_PATH
    echo "Error: Engine not found at $ENGINE_PATH"
    exit 1
end

echo "Opening book: "(test -f $BOOK_PATH && echo "✓ Found at $BOOK_PATH" || echo "✗ Not found")
echo ""

# Test against Stockfish at different skill levels
# Skill 0 ≈ 1300 Elo
# Skill 5 ≈ 1600 Elo  
# Skill 10 ≈ 1900 Elo

for skill in 0 # 2 # 5 10 14 16 # this is where we are :D
    echo ""
    echo "Playing $GAMES_PER_LEVEL games against Stockfish Skill Level $skill..."

    cutechess-cli \
        -engine name=ChessML cmd=$ENGINE_PATH proto=xboard \
        -engine name=Stockfish cmd=stockfish proto=uci option."Skill Level"=$skill \
        -each tc=$TIME_CONTROL \
        -rounds $GAMES_PER_LEVEL \
        -repeat \
        -pgnout "results_skill_$skill.pgn" \
        -recover 2>&1 | tee "results_skill_$skill.txt"

    echo "Results saved to results_skill_$skill.txt and results_skill_$skill.pgn"
end

echo ""
echo "Testing complete! Analyze results with:"
echo "  grep 'Score of ChessML' results_skill_*.txt"
echo ""
echo "Check opening book usage in /tmp/chessml_xboard.log"
