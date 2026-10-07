# ChessML

> [!NOTE]
> **Personal Learning Project**  
> This is a personal hobby project created for learning chess programming and OCaml.
> It has no specific roadmap, goals, or direction—just exploration and experimentation.
> Code quality and features evolve organically as I learn. Contributions and suggestions
> are welcome, but expect things to change arbitrarily as I try new ideas!
>
> I will concider this project done when I can no longer beat it 😎

A bitboard chess engine written in OCaml with UCI & XBoard protocol support.

Play chess against ChessML using any UCI- or XBoard-compatible GUI! ♟️

Learn with me here: https://alexanderbrevig.github.io/ChessML/

## ✨ Features

- ⚡**Fast Move Generation**: Efficient bitboard-based move generation
- 🧠**Alpha-Beta Search**: Minimax search with alpha-beta pruning
- 💾**Transposition Tables**: Position caching for faster search
- 📚**Opening Book**: Support for Polyglot opening books generated from PGN
- 🏗️**Book Generator**: 1M+ position-move combinations from 9M+ games
- 🖥️**UCI Protocol**: Compatible with chess GUIs like Arena and ChessBase
- 🎮**XBoard Protocol**: Play via the xboard/winboard interface

## 🚀 Quick Start

### Install Dependencies

```bash
# Ubuntu/Debian
sudo apt install opam stockfish

# Initialize OCaml environment
opam init
opam switch create 5.3.0
opam switch 5.3.0
eval (opam env)
opam install dune alcotest ocaml-lsp-server ocamlformat
```

### Build the Engine

```bash
# Development build (debug mode, faster compilation)
dune build

# Release build (optimized, ~30-40% faster performance)
dune build --profile=release

```

> **⚠️ Performance Note**: For benchmarks, testing, or actual gameplay, **always use `--profile=release`**!
> Release mode enables critical optimizations (inlining, flambda, bounds check elimination) that significantly improve search speed.
>
> - Debug mode: ~86K NPS
> - Release mode: ~120K NPS (+40% faster)

### Opening Book Creation and Setup

> ![WARN]
> You will need 4.5 GiB RAM to run this.
> It takes ~15 minutes to complete the process and generates 18MB of data.

```bash
./scripts/download_openings.fish # about 6 minutes and 30 seconds
dune build --profile=release && CHESSML_PARALLEL=8 ./_build/default/bin/create_book.exe # about 8 minutes
```

The book uses the standard Polyglot format, so third-party Polyglot `.bin` books work too. Books built by versions before the switch to Polyglot keys must be regenerated.

You should the see something like

```
📊 Processed 9065131 games total (avg 7.9 plies per game)

📝 Building book entries (min 3 games)...
   • 1113046 unique position-move combinations
   • 682561 unique positions

💾 Writing book.bin...
   • 17808736 bytes
   • 1113046 moves
```

ChessML will automatically look for the opening book in the following locations (in order):

1. Current directory: `./book.bin`
2. XDG data directory: `$XDG_DATA_HOME/chessml/book.bin` (typically `~/.local/share/chessml/book.bin`)
3. User directory: `~/.chessml/book.bin`
4. System-wide: `/usr/local/share/chessml/book.bin`
5. System-wide: `/usr/share/chessml/book.bin`

Cutechess will not find the book, so please install it.
To install the opening book for your user:

```bash
# Create the directory
mkdir -p ~/.local/share/chessml

# Copy the book file
cp book.bin ~/.local/share/chessml/
```

## 🔧 Development

ChessML uses [Just](https://github.com/casey/just) as a command runner see `just list`.

### Quick Start

```bash
# Build the project
just

# Run all tests (a few seconds)
just test

# Run the search tests including the slow cases
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

# Run all tests
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
  - **Quick**: mates, tactics, search behavior (run by `dune runtest`)
  - **Slow**: deeper searches (`just test-search`)
- **Perft**: move generation against published node counts
- **Protocol Tests**: UCI and XBoard sessions driven line by line
- **Integration Tests**: End-to-end functionality

### Running Tests

```bash
# Quick development feedback (a few seconds)
dune runtest
# or
just test

# Deep search validation when needed
just test-search

# Run specific test categories
just test-core          # Core data structures
just test-engine        # Engine functionality
just test-integration   # Integration tests
```

The test suite automatically runs quick tests by default to keep development cycles fast, with deep validation available when needed.

### 🎲 Play Against ChessML

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

Launch cutechess, add the engine in settings (I recomment to use xboard version) and create a new game.

## 📖 Documentation

- [docs/README.md](./docs/README.md) - Intro to ChessML

## 🙏 Credits

- [Chess Programming Wiki](https://www.chessprogramming.org/)
- [Stockfish](https://github.com/official-stockfish/Stockfish) - Reference implementation
- [Real World OCaml](https://dev.realworldocaml.org/)
- [OCaml Manual](https://ocaml.org/manual/)
- [PGNMentor](https://www.pgnmentor.com) - Opening PGN database

## 🤝 Contributing

Contributions and suggestions are welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

## 📄 License

ChessML is licensed under the [MIT License](LICENSE) - see the [LICENSE](https://github.com/alexanderbrevig/chessml/blob/main/LICENSE) file for details.

## 📝 TODO

- Figure out if I can publish the book.bin generated from PGNMentor
- Tune evaluation parameters
- Add endgame tablebase support
- Lazy SMP parallel search
- Endgame knowledge (KQK, KRK, KBNK mating guidance, tapered evaluation)

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
