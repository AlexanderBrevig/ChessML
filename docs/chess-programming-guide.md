---
layout: default
title: Chess Programming Guide
nav_order: 2
has_children: true
description: "Complete overview of chess programming concepts and techniques"
permalink: /docs/chess-programming-guide
---

# Chess Programming Concepts Guide

This guide explains the core concepts used in ChessML and modern chess engines. Each document focuses on **why** the technique matters and **how** to implement it, with clear examples and practical advice.

## Core Concepts (Foundational)

These are the building blocks every chess engine needs:

### [Bitboards](bitboards.md)

**Elo Impact:** ~100-200 Elo  
64-bit integers representing chess board state. Enables parallel operations on all squares simultaneously using bitwise operations. Essential for fast move generation.

### [Magic Bitboards](magic-bitboards.md)

**Elo Impact:** ~50-100 Elo  
Pre-computed lookup tables for sliding piece (rook, bishop, queen) attack generation. Uses "magic numbers" to hash occupancy patterns into table indices for constant-time lookups.

### [Zobrist Hashing](zobrist-hashing.md)

**Elo Impact:** Enables transposition tables (~200-300 Elo)  
Fast incremental position hashing using XOR operations. Required for transposition table implementation. Positions can be compared using 64-bit keys instead of full board comparison.

## Search Techniques (Critical for Strength)

These determine how deeply and efficiently your engine can search:

### [Alpha-Beta Pruning](alpha-beta-pruning.md)

**Elo Impact:** ~1000+ Elo vs basic minimax  
The foundation of modern chess search. Prunes branches that provably can't affect the final decision, typically reducing nodes searched by 50-90%. Works best with good [move ordering](move-ordering.md).

### [Transposition Tables](transposition-tables.md)

**Elo Impact:** ~200-300 Elo  
Cache position evaluations to avoid re-searching identical positions reached through different move orders. Requires [Zobrist hashing](zobrist-hashing.md) for fast position keys. Typically saves 30-40% of search effort at depth 6+.

### [Quiescence Search](quiescence-search.md)

**Elo Impact:** ~150-250 Elo  
Extends search beyond fixed depth to resolve tactical sequences (captures, checks). Prevents the "horizon effect" where the engine stops searching mid-tactics and misjudges positions. Uses [SEE](static-exchange-evaluation.md) to prune losing captures.

### [Null Move Pruning](null-move-pruning.md)

**Elo Impact:** ~100-150 Elo  
Aggressive pruning by giving the opponent a free move. If they still can't beat [beta](alpha-beta-pruning.md), the position is too good and can be pruned. Reduces search tree by 40-60% with minimal accuracy loss.

### [Late Move Reductions (LMR)](late-move-reductions.md)

**Elo Impact:** ~100-200 Elo  
Search later moves (likely weaker) at reduced depth. If one looks promising at shallow search, re-search at full depth. Relies heavily on good [move ordering](move-ordering.md). Effectively increases search depth by 1-2 plies.

## Move Ordering (Force Multiplier)

Good move ordering is critical for alpha-beta efficiency:

### [Move Ordering](move-ordering.md)

**Elo Impact:** ~100-200 Elo  
The art of trying promising moves first. Combines [hash moves](transposition-tables.md), [capture ordering](static-exchange-evaluation.md), killer moves, history heuristic, and countermoves. Good ordering can effectively double [alpha-beta](alpha-beta-pruning.md) search depth.

### [Static Exchange Evaluation (SEE)](static-exchange-evaluation.md)

**Elo Impact:** ~30-80 Elo  
Calculates material outcome of capture sequences without search. Essential for ordering captures in [move ordering](move-ordering.md) and pruning bad captures in [quiescence search](quiescence-search.md).

## Evaluation (Understanding Positions)

How the engine judges who's winning:

### [Evaluation Function](evaluation-function.md)

**Elo Impact:** ~200-400 Elo  
Assigns numerical scores to positions. Includes material counting, piece-square tables, pawn structure, king safety, and positional factors. The "eyes" of the engine.

## Opening Knowledge

Pre-computed opening theory:

### [Opening Books](opening-books.md)

**Elo Impact:** ~50-150 Elo  
Database of pre-analyzed opening positions. Saves time in known positions and ensures solid opening play. Typically uses Polyglot format for compatibility.

---

## Learning Path

### Beginner (Build a Basic Engine)

1. [Bitboards](bitboards.md) - Represent the board efficiently
2. [Alpha-Beta Pruning](alpha-beta-pruning.md) - Basic search
3. [Evaluation Function](evaluation-function.md) - Material + piece-square tables

**Result:** ~1500-1800 Elo engine

### Intermediate (Add Intelligence)

4. [Zobrist Hashing](zobrist-hashing.md) - Position keys
5. [Transposition Tables](transposition-tables.md) - Cache results
6. [Move Ordering](move-ordering.md) - Basic ordering (hash + MVV-LVA)
7. [Quiescence Search](quiescence-search.md) - Tactical stability

**Result:** ~2000-2200 Elo engine

### Advanced (Optimize Performance)

8. [Magic Bitboards](magic-bitboards.md) - Fast attack generation
9. [Null Move Pruning](null-move-pruning.md) - Aggressive pruning
10. [Late Move Reductions](late-move-reductions.md) - Deeper search
11. [Static Exchange Evaluation](static-exchange-evaluation.md) - Better capture evaluation
12. [Opening Books](opening-books.md) - Opening knowledge

**Result:** ~2400+ Elo engine

---

## Quick Reference

### Elo Gains Summary

| Technique            | Elo Gain   | Complexity | Priority     |
| -------------------- | ---------- | ---------- | ------------ |
| Alpha-Beta Pruning   | ~1000      | Medium     | **Critical** |
| Transposition Tables | ~200-300   | Medium     | **Critical** |
| Evaluation Function  | ~200-400   | Medium     | **Critical** |
| Quiescence Search    | ~150-250   | Medium     | **High**     |
| Move Ordering        | ~100-200   | Medium     | **High**     |
| Null Move Pruning    | ~100-150   | Low        | **High**     |
| Late Move Reductions | ~100-200   | High       | **High**     |
| Bitboards            | ~100-200   | High       | **Medium**   |
| Opening Books        | ~50-150    | Low        | **Medium**   |
| Magic Bitboards      | ~50-100    | High       | **Medium**   |
| SEE                  | ~30-80     | Medium     | **Medium**   |
| Zobrist Hashing      | Enables TT | Low        | **Critical** |

### Implementation Order

**Phase 1 - Basic Functionality:**

- Bitboards (board representation)
- Alpha-Beta (search)
- Basic Evaluation (material + PST)

**Phase 2 - Core Improvements:**

- Zobrist Hashing
- Transposition Tables
- Quiescence Search
- Basic Move Ordering

**Phase 3 - Optimizations:**

- Null Move Pruning
- Late Move Reductions
- Advanced Move Ordering (killers, history, countermoves)
- SEE

**Phase 4 - Polish:**

- Magic Bitboards
- Opening Books
- Evaluation Tuning

---

## Additional Resources

- [Chess Programming Wiki](https://www.chessprogramming.org/) - Comprehensive reference
- [Stockfish Source](https://github.com/official-stockfish/Stockfish) - World's strongest open-source engine
- [ChessML Source](https://github.com/alexanderbrevig/chessml) - This engine's implementation

## Contributing

Found an error or want to improve these docs? See [CONTRIBUTING.md](../CONTRIBUTING.md).

---

_These documents are written for newcomers to chess programming. The goal is understanding over completeness — enough detail to implement effectively, not exhaustively._
