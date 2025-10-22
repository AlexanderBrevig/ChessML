---
layout: default
title: Developer Guide
nav_order: 3
description: "ChessML codebase architecture and development workflow"
permalink: /docs/developer-guide
---

# ChessML Developer Guide

> **Learning chess programming?** Start with the **[Chess Programming Concepts Guide](chess-programming-guide)** for detailed explanations of all techniques used in this engine.

This document covers the codebase architecture and development workflow for ChessML contributors.

---

## 🏗️ Codebase Architecture

### Core Modules (`lib/core/`)

- **Bitboard**: 64-bit bitwise board representation with C-accelerated operations
- **Square**: Board square representation (0-63 indexing)
- **Move**: Encoded move representation with type safety
- **Types**: Fundamental chess types (pieces, colors, etc.)

### Engine Modules (`lib/engine/`)

- **Position**: Game state management with Zobrist hashing
- **Movegen**: Legal move generation using magic bitboards
- **Search**: Alpha-beta search with iterative deepening
- **Eval**: Static evaluation with material and positional scoring
- **Game**: High-level game management and move validation
- **Polyglot**: Binary format for storing books

### Protocol Support (`lib/protocols/`)

- **UCI**: Universal Chess Interface protocol
- **XBoard**: XBoard/WinBoard protocol

---

## 🧪 Testing Strategy

### Unit Tests (`test/`)

- **Core Tests**: Bitboard, move generation, types
- **Engine Tests**: Position, evaluation, search
- **Protocol Tests**: UCI/XBoard compliance
- **Special Moves**: Castling, en passant, promotion
- **Regression Tests**: Bug fix validation

### Benchmarks (`examples/`)

- **Search Benchmarks**: Single-threaded performance
- **Parallel Benchmarks**: Multi-threaded scaling
- **Feature Tests**: Individual technique validation
- **Quality Tests**: Move quality analysis

---

## ️ Build System

### Dune Build Files

- **Modular structure**: Separate libraries for core/engine/protocols
- **Test integration**: Alcotest framework
- **Public binaries**: UCI and XBoard executables
- **Examples**: Benchmarks and demonstrations

### Key Commands

```bash
dune build                    # Build all targets
dune runtest                  # Run all tests
dune exec bin/chessml_uci.exe # Run UCI engine
just test-protocols           # Run protocol tests
```

---

## 💡 Key Implementation Notes

For detailed explanations of these concepts, see the [Chess Programming Concepts Guide](chess-programming-guide.md).

### Hybrid Board Representation

ChessML uses **both** a board array and bitboards for optimal performance:

**Board Array (`piece option array`)** - O(1) "What piece is on square X?"

- Used in: move making, castling logic, capture detection, PGN parsing
- Example: `piece_at pos Square.e4` → instant lookup

**12 Piece-Type Bitboards** - O(1) "Where are all pieces of type X?"

- Used in: material counting, move generation, attack detection
- Example: `get_pieces pos White Knight` → all white knight positions

**Why both?**

Without board array (bitboards only):

```ocaml
(* Finding what's on e4 requires 12 bitboard checks *)
let piece_at sq =
  if contains white_pawns sq then Some WhitePawn
  else if contains white_knights sq then Some WhiteKnight
  else (* ...10 more checks... *)
```

Without bitboards (array only):

```ocaml
(* Material counting requires 64 array lookups *)
let count_material () =
  for sq = 0 to 63 do
    match board.(sq) with
    | Some piece -> total := !total + value piece
    | None -> ()
  done
```

**The hybrid approach** gives O(1) performance for both access patterns at the cost of ~2KB memory and keeping them synchronized during move making. This is standard in modern engines (Stockfish, Leela, etc.).

### Other Core Concepts

- **Move Encoding**: 16-bit packed representation (see `lib/core/move.ml`)
- **Zobrist Hashing**: Incremental position hashing via XOR (see `lib/engine/zobrist.ml`)
- **Polyglot Books**: Binary format for opening books (see `lib/engine/polyglot.ml` and `examples/polyglot_demo.ml`)

---

## 🔧 Development Workflow

### Running Tests

```bash
dune runtest                  # Run all tests
just test                     # Alternative via justfile
```

### Code Formatting

```bash
dune fmt                      # Format all code
ocamlformat --inplace file.ml # Format specific file
```

### Building

```bash
dune build                    # Debug build
dune build --profile=release  # Optimized build
```

### Benchmarking

```bash
dune exec --profile=release examples/search_bench.exe
```

---

## 📈 Future Ideas

- Remove `Position.board` field (use bitboards only)
- NNUE evaluation
- Better time management
- Syzygy tablebase support
- Enhanced parallel search scaling

---

## 💡 Contributing

1. **Run tests** before submitting: `dune runtest`
2. **Format code**: Use `dune fmt` for consistency
3. **Add tests** for new features
4. **Update docs** when adding techniques
5. **Benchmark** performance-critical changes

See [CONTRIBUTING.md](../CONTRIBUTING.md) for more details.

---

## 📚 Documentation

**New to chess programming?** Start with the **[Chess Programming Concepts Guide](chess-programming-guide.md)**

This comprehensive guide explains all the techniques used in ChessML, with detailed explanations of why they matter and how to implement them.

### External Resources

**Chess Programming:**

- [Chess Programming Wiki](https://www.chessprogramming.org/)
- [Stockfish](https://github.com/official-stockfish/Stockfish) - World's strongest open-source engine
- [Ethereal](https://github.com/AndyGrant/Ethereal) - Clean modern engine

**OCaml:**

- [Real World OCaml](https://dev.realworldocaml.org/)
- [OCaml Manual](https://ocaml.org/manual/)
- [Dune Documentation](https://dune.readthedocs.io/)

---

**Happy Chess Programming! ♟️**
