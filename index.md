---
layout: home
title: Home
nav_order: 1
description: "ChessML - A comprehensive guide to chess programming concepts and techniques implemented in OCaml"
permalink: /
---

# ChessML Documentation

Welcome to the ChessML documentation! This comprehensive guide covers chess programming concepts, from fundamental board representation to advanced search techniques.

## About ChessML

ChessML is a chess engine written in OCaml that demonstrates modern chess programming techniques. Whether you're new to chess programming or looking to understand specific optimizations, these guides will help you build a strong chess engine.

## Learning Path

### 🎯 Beginner - Build a Basic Engine

Start with the fundamentals:
1. [Bitboards](docs/bitboards) - Represent the board efficiently
2. [Alpha-Beta Pruning](docs/alpha-beta-pruning) - Basic search algorithm
3. [Evaluation Function](docs/evaluation-function) - Material + piece-square tables

**Result:** ~1500-1800 Elo engine

### 🚀 Intermediate - Add Intelligence

Improve your engine's strength:
4. [Zobrist Hashing](docs/zobrist-hashing) - Position keys for caching
5. [Transposition Tables](docs/transposition-tables) - Cache search results
6. [Move Ordering](docs/move-ordering) - Try best moves first
7. [Quiescence Search](docs/quiescence-search) - Tactical stability

**Result:** ~2000-2200 Elo engine

### ⚡ Advanced - Optimize Performance

Make your engine competitive:
8. [Magic Bitboards](docs/magic-bitboards) - Fast attack generation
9. [Null Move Pruning](docs/null-move-pruning) - Aggressive search reduction
10. [Late Move Reductions](docs/late-move-reductions) - Search deeper efficiently
11. [Static Exchange Evaluation](docs/static-exchange-evaluation) - Better capture evaluation
12. [Opening Books](docs/opening-books) - Opening knowledge

**Result:** ~2400+ Elo engine

## Quick Links

- [Chess Programming Concepts Guide](docs/chess-programming-guide) - Complete overview
- [GitHub Repository](https://github.com/AlexanderBrevig/ChessML) - Source code
- [Contributing Guide](https://github.com/AlexanderBrevig/ChessML/blob/main/CONTRIBUTING.md) - How to contribute

## Documentation Structure

Our documentation is organized into clear categories:

- **Core Concepts** - Fundamental board representation and data structures
- **Search Techniques** - Algorithms for finding the best move
- **Move Ordering** - Optimizing search efficiency
- **Evaluation** - Judging chess positions

Each guide includes:
- Clear explanations of concepts
- Implementation examples in OCaml
- Elo impact estimates
- Common pitfalls and solutions

---

_Ready to build a chess engine? Start with the [Chess Programming Guide](docs/chess-programming-guide)!_

**Happy Chess Programming! ♟️**
