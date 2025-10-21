# ChessML

> [!NOTE]
> **Personal Learning Project**  
> This is a personal hobby project created for learning chess programming and OCaml.
> It has no specific roadmap, goals, or direction—just exploration and experimentation.
> Code quality and features evolve organically as I learn. Contributions and suggestions
> are welcome, but expect things to change arbitrarily as I try new ideas!
> I will concider this project done when I can no longer beat it 😎

A bitboard chess engine written in OCaml with UCI & XBoard protocol support.

Play chess against ChessML using PyChess or any UCI- or XBoard-compatible GUI! ♟️

## 🚀 Quick Start

### Install Dependencies

```bash
# Ubuntu/Debian
sudo apt install opam pychess stockfish
# you also need to install cutechess, see below

# Initialize OCaml environment
opam init
opam switch create 5.4.0
opam switch 5.4.0
eval (opam env)
opam install dune alcotest ocaml-lsp-server ocamlformat
```

Cutechess install

```bash
sudo apt install git build-essential cmake qtbase5-dev qtbase5-dev-tools libqt5svg5-dev
git clone git@github.com:cutechess/cutechess.git
cd cutechess
mkdir build
cd build
cmake ..
make
cp cutechess cutechess-cli /usr/local/bin # copy binaries to some directory in your path or add $PWD to $PATH
```

### Build the Engine

```bash
# Development build (debug mode, faster compilation)
dune build

# Release build (optimized, ~30-40% faster performance)
dune build --profile=release

# Build the opening book
dune exec bin/create_book.exe
```

> **⚠️ Performance Note**: For benchmarks, testing, or actual gameplay, **always use `--profile=release`**!
> Release mode enables critical optimizations (inlining, flambda, bounds check elimination) that significantly improve search speed.
>
> - Debug mode: ~86K NPS
> - Release mode: ~120K NPS (+40% faster)

## ✨ Features

- **Fast Move Generation** ⚡: Efficient bitboard-based move generation
- **Alpha-Beta Search** 🧠: Minimax search with alpha-beta pruning
- **Transposition Tables** 💾: Position caching for faster search
- **Opening Book** 📚: Support for Polyglot opening books
- **UCI Protocol** 🖥️: Compatible with chess GUIs like Arena and ChessBase
- **XBoard Protocol** 🎮: Play via the xboard/winboard interface

## 🔧 Development

ChessML uses [Just](https://github.com/casey/just) as a command runner see `just list`.

### Quick Start

```bash
# Build the project
just

# Run all tests (includes quick search tests ~11s)
just test

# Run deep search validation (~2+ minutes)
just test-search

# Format code
just format

# Watch mode (rebuild on changes)
just watch
```

### Using Dune Directly

You can also use dune commands directly:

```bash
# Development build (debug mode)
dune build

# Release build (optimized - use for performance testing!)
dune build --profile=release

# Run all tests (quick by default ~11s)
dune runtest

# Run quick search tests only
dune exec test/engine/test_search.exe -- -q

# Run slow/deep search tests
dune exec test/engine/test_search.exe -- test slow_tests

# Test UCI engine manually
echo "uci
setoption name MaxDepth value 3
position startpos
go depth 2
quit" | ./_build/default/bin/chessml_uci.exe

# Test XBoard engine manually
printf "xboard\nprotover 2\nnew\nusermove e2e4\nquit\n" | ./_build/default/bin/chessml_xboard.exe

# Performance benchmarks (ALWAYS use --profile=release!)
dune exec --profile=release examples/bench_breakdown.exe
```

## ⚙️ Configuration

The engine supports runtime configuration via both UCI and XBoard protocols:

### UCI Configuration

```bash
# Set maximum search depth (1-50)
setoption name MaxDepth value 8

# Set quiescence search depth (1-20)
setoption name QuiescenceDepth value 6

# Enable/disable quiescence search
setoption name UseQuiescence value true

# Enable/disable transposition table
setoption name UseTranspositionTable value true

# Set hash table size in MB
setoption name Hash value 64
```

### XBoard Configuration

```bash
# Configure via option commands
option MaxDepth=8
option UseQuiescence=true
option DebugOutput=false
```

### 💪 Strength Levels

- **Depth 1-3**: Beginner 🐣 (very fast)
- **Depth 4-5**: Intermediate 🎯 (default, ~1-10s per move)
- **Depth 6-8**: Advanced 🔥 (slower, stronger)
- **Depth 9+**: Expert 🏆 (very slow, tournament strength)

## 🧪 Testing

ChessML uses a comprehensive test suite with both quick and thorough validation:

### Test Categories

- **Core Tests**: Data structures, move generation, position handling
- **Engine Tests**: Evaluation, game logic, zobrist hashing
- **Search Tests**:
  - **Quick** (~10s): Basic functionality, shallow search validation
  - **Slow** (~2+ min): Deep search validation, performance analysis
- **Integration Tests**: End-to-end functionality

### Running Tests

```bash
# Quick development feedback (~11s total)
dune runtest
# or
just test

# Deep search validation when needed
just test-search-slow

# Run specific test categories
just test-core          # Core data structures
just test-engine        # Engine functionality
just test-integration   # Integration tests
```

The test suite automatically runs quick tests by default to keep development cycles fast, with deep validation available when needed.

### 🎲 Play Against ChessML

1. Launch PyChess:

   ```bash
   pychess
   ```

2. Add ChessML engine:

   - Go to **Edit → Preferences → Engines**
   - Click **Add**
   - Command: `/path/to/ChessML/ChessML`
   - Name: `ChessML`
   - Click **OK**

3. Start a game:
   - **File → New Game**
   - Select **Human vs Engine**
   - Choose **ChessML**
   - Click **Start**

## 📖 Documentation

- [docs/README.md](./docs/README.md) - Intro to ChessML

## 🙏 Credits

- [Chess Programming Wiki](https://www.chessprogramming.org/)
- [Stockfish](https://github.com/official-stockfish/Stockfish) - Reference implementation
- [Real World OCaml](https://dev.realworldocaml.org/)
- [OCaml Manual](https://ocaml.org/manual/)

## 🤝 Contributing

Contributions and suggestions are welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

## 📄 License

ChessML is licensed under the [MIT License](LICENSE) - see the [LICENSE](https://github.com/alexanderbrevig/chessml/blob/main/LICENSE) file for details.

## 📝 TODO

- Improve parallel search performance
- Tune evaluation parameters
- Add endgame tablebase support
- Implement time management
- Add more opening book positions

### Running Engine Matches

```sh
# Run a match against Stockfish (skill level 0)
cutechess-cli \
   -engine name=ChessML cmd=./_build/default/bin/chessml_xboard.exe proto=xboard \
   -engine name=Stockfish cmd=stockfish proto=uci option."Skill Level"=0 \
   -each tc=40/300 \
   -rounds 2 \
   -repeat \
   -pgnout "games.pgn" \
   -recover
```
