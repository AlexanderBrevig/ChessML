#!/usr/bin/env fish

# Test opening play with XBoard protocol
# This script tests if the engine follows proper opening principles

echo "Testing Opening Play with XBoard Protocol"
echo "=========================================="
echo ""

set ENGINE ./_build/default/bin/chessml_xboard.exe

# Test 1: Check if engine develops properly from starting position
echo "Test 1: Does the engine develop pieces properly?"
echo "Position: Starting position after 1.e4 e5"
echo ""

# Send commands to engine with time control
printf "xboard\nprotover 2\nnew\nst 3\nforce\ne2e4\ne7e5\ngo\n" | timeout 20 $ENGINE 2>&1 | grep -E "^move" | tail -1

echo "Expected: Knight development (g1f3 or b1c3)"
echo ""

# Test 2: Check if engine avoids early queen moves
echo "Test 2: After 1.e4 e5 2.Nf3, does Black develop properly?"
echo ""

printf "xboard\nprotover 2\nnew\nst 3\nforce\ne2e4\ne7e5\ng1f3\ngo\n" | timeout 20 $ENGINE 2>&1 | grep -E "^move" | tail -1

echo "Expected: Knight or bishop development (b8c6, g8f6, f8c5, f8d6, etc), NOT d8h4/d8f6"
echo ""

# Test 3: Italian Game opening
echo "Test 3: Italian Game - 1.e4 e5 2.Nf3 Nc6 3.Bc4 - does Black develop?"
echo ""

printf "xboard\nprotover 2\nnew\nst 3\nforce\ne2e4\ne7e5\ng1f3\nb8c6\nf1c4\ngo\n" | timeout 20 $ENGINE 2>&1 | grep -E "^move" | tail -1

echo "Expected: Bishop development (f8c5 or f8e7 or f8d6) or knight (g8f6)"
echo ""

# Test 4: Check castling preference
echo "Test 4: After development, does engine castle?"
echo "Position: 1.e4 e5 2.Nf3 Nc6 3.Bc4 Bc5 4.d3 d6 5.Nc3 Nf6"
echo ""

printf "xboard\nprotover 2\nnew\nst 3\nforce\ne2e4\ne7e5\ng1f3\nb8c6\nf1c4\nf8c5\nd2d3\nd7d6\nb1c3\ng8f6\ngo\n" | timeout 25 $ENGINE 2>&1 | grep -E "^move" | tail -1

echo "Expected: Castling (O-O) or preparation for castling"
echo ""

# Test 5: Avoiding early queen moves
echo "Test 5: After 1.e4 e5 2.d4, does Black avoid Qh4?"
echo ""

printf "xboard\nprotover 2\nnew\nst 3\nforce\ne2e4\ne7e5\nd2d4\ngo\n" | timeout 20 $ENGINE 2>&1 | grep -E "^move" | tail -1

echo "Expected: Solid moves like exd4, Nc6, or Nf6. NOT d8h4"
echo ""

echo "✅ Opening test complete!"
echo ""
echo "See OPENING_TEST_RESULTS.md for detailed analysis"
