# ChessML Engine - Developer Guide

A high-performance chess engine written in OCaml, implementing modern chess programming techniques.

## 🎯 Quick Overview

ChessML is a UCI/XBoard-compatible chess engine featuring:

- **Bitboard-based** move generation
- **Advanced search** with planned parallel support (in progress)
- **Sophisticated evaluation** with pawn structure analysis
- **Opening book** integration

---

## 🏗️ Architecture

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

## 🔍 Search Techniques

### Core Algorithm

- **Alpha-Beta Pruning**: Minimax with cutoff optimization
- **Iterative Deepening**: Gradual depth increase with early move ordering
- **Transposition Table**: Position caching to avoid redundant evaluation
- **Quiescence Search**: Tactical extension to avoid horizon effect

### Advanced Pruning

- **Null Move Pruning**: Forward pruning by passing the turn
- **Late Move Reductions (LMR)**: Reduced depth for late moves
- **Futility Pruning**: Skip nodes unlikely to raise alpha
- **Reverse Futility Pruning**: Early beta cutoffs in quiet positions
- **Late Move Pruning**: Skip late quiet moves at low depths

### Move Ordering

- **Hash Move First**: Best move from transposition table
- **MVV-LVA**: Most Valuable Victim - Least Valuable Attacker
- **SEE (Static Exchange Evaluation)**: Precise capture evaluation
- **Killer Moves**: Moves that caused cutoffs at same depth
- **History Heuristic**: Statistical move success tracking
- **Countermove Heuristic**: Refutation move pairs

---

## 🧮 Evaluation Features

### Material

- Standard piece values (P=100, N=320, B=330, R=500, Q=900)
- Piece-square tables for positional bonuses

### Pawn Structure

- **Passed Pawns**: Bonus for advancement and protection
- **Doubled Pawns**: Penalty for stacked pawns
- **Isolated Pawns**: Penalty for lack of support
- **Backward Pawns**: Penalty for vulnerable pawns
- **Pawn Chains**: Bonus for connected pawns
- **Central Control**: Bonus for center pawns

### Positional

- King safety evaluation
- Rook on open/semi-open files
- Bishop pair bonus
- Knight outposts
- Development bonuses

---

## ⚡ Performance Optimizations

### Bitboard Operations

- **Magic Bitboards**: Fast slider piece move generation
- **C Stubs**: Performance-critical operations in C
- **Precomputed Tables**: Attack tables and move masks

### Search Optimization

- **Aspiration Windows**: Narrow alpha-beta bounds
- **Principal Variation Search (PVS)**: Minimal window re-searches
- **Parallel Search**: Multi-threaded search with shared TT
  - Lazy SMP implementation
  - Root move splitting
  - Concurrent transposition table

### Memory Efficiency

- Move encoding in 16 bits
- Packed position representation
- Efficient hash table implementation

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

## 📊 Opening Book

- **Binary format** for fast loading
- **Position-based lookup** using Zobrist hashes
- **Multiple moves per position** with weights
- **Transposition-aware** book creation

---

## 🛠️ Build System

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

## 🎓 Key Concepts for Contributors

### 1. **Bitboard Representation**

Each piece type has a 64-bit integer where set bits represent occupied squares.

```ocaml
type t = int64  (* Each bit = one square *)
```

### 2. **Move Encoding**

Moves encoded in 16 bits: from (6) + to (6) + flags (4)

```ocaml
type t = int  (* Packed: FFFFTTTTTTFFFFFF *)
```

### 3. **Zobrist Hashing**

Incremental position hashing for transposition table:

```ocaml
hash = hash ^ zobrist_piece[piece][square]
```

### 4. **Alpha-Beta Search**

```ocaml
val alphabeta : Position.t -> depth:int -> alpha:int -> beta:int -> int
```

- Alpha: Best score for maximizing player
- Beta: Best score for minimizing player
- Prune when alpha >= beta

### 5. **Transposition Table**

Stores: position hash, depth, score, best move, entry type

```ocaml
type tt_entry = {
  key: int64;
  score: int;
  depth: int;
  best_move: Move.t option;
  entry_type: Exact | LowerBound | UpperBound;
}
```

### 6. **Polyglot Opening Book Format**

Binary format for chess opening books (16 bytes per entry)

```ocaml
(* Entry structure *)
type entry = {
  key: int64;      (* Zobrist hash *)
  move: int;       (* Encoded move *)
  weight: int;     (* Popularity/strength *)
  learn: int;      (* Learning data *)
}

(* Move encoding: from | (to << 6) | (promo << 12) *)
let encoded = Polyglot.encode_move from_sq to_sq 0

(* Reading from book *)
match Polyglot.read_entry channel with
| Some entry -> (* process entry *)
| None -> (* end of file *)

(* Writing to book *)
Polyglot.write_entry channel entry
```

See `examples/polyglot_demo.ml` for complete examples.

---

## 📈 Future Improvements Ideas

- Improve the book with deeper and more openings
- Try to get rid of the Position.board field and only use bitboards
- NNUE evaluation
- Better time management
- Endgame tablebases
- Syzygy tablebase support
- Enhanced parallel search scaling

---

## 💡 Contributing Guidelines

1. **Run tests** before submitting: `dune runtest`
2. **Follow OCaml style**: Use `ocamlformat` for consistency or `dune fmt`
3. **Add tests** for new features
4. **Update docs** when adding techniques
5. **Benchmark** performance-critical changes

---

## 📚 Learning Resources

### Chess Programming

- [Chess Programming Wiki](https://www.chessprogramming.org/)
- [Stockfish](https://github.com/official-stockfish/Stockfish) - Reference implementation
- [Ethereal](https://github.com/AndyGrant/Ethereal) - Clean modern engine

### OCaml

- [Real World OCaml](https://dev.realworldocaml.org/)
- [OCaml Manual](https://ocaml.org/manual/)
- [Dune Documentation](https://dune.readthedocs.io/)

---

**Happy Chess Programming! ♟️**
