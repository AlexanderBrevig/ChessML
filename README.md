# ChessML

> [!NOTE]
> **Personal Learning Project**
> This is a hobby project for learning chess programming and OCaml, written for my own pleasure.
> It has no roadmap, goals, or direction—just exploration and experimentation.
> You are welcome to try it, read it, or suggest things, but expect it to change
> arbitrarily as I try new ideas!
>
> I will consider this project done when I can no longer beat it 😎

A chess engine written in OCaml with UCI and XBoard support, so you can play it in a chess GUI.

Learn with me here: https://alexanderbrevig.github.io/ChessML/

## What it is

- **Board and moves**: bitboards with magic bitboards for sliding pieces; move generation is checked with perft against the standard test positions.
- **Search**: iterative deepening principal variation search with a transposition table, quiescence search, null move pruning, late move reductions and the usual pruning tricks. Detects repetitions, the fifty-move rule and insufficient material.
- **Evaluation**: hand-written terms (material, piece-square tables, pawn structure, king safety, development, a few endgame patterns).
- **Opening book**: standard Polyglot `.bin` books, plus a tool that builds one from PGN files.
- **Protocols**: UCI and XBoard. I play it through [cutechess](https://github.com/cutechess/cutechess); other GUIs should work but are untested.

What it is not: competitive with modern engines. It is single-threaded, has no neural network evaluation, no endgame tablebases and no endgame mating technique (it can fail to convert a won K+Q vs K). The evaluation is untuned. It has only been built and run on Linux.

## Strength

Very roughly **1850–2000** on Stockfish's `UCI_Elo` scale. That comes from 200 games at 30s+0.3s per game against Stockfish 18 limited to 1320–2200 Elo, single thread, with the opening book. The error bars are large (±150) and Stockfish's limited mode is calibrated at longer time controls, so this is a ballpark, not a human rating.

To measure it yourself:

```sh
cutechess-cli -tournament gauntlet \
   -engine name=ChessML cmd=./_build/default/bin/chessml_uci.exe proto=uci \
   -engine name=SF1900 cmd=stockfish proto=uci option.UCI_LimitStrength=true option.UCI_Elo=1900 \
   -each tc=30+0.3 -games 2 -rounds 10 -concurrency 8 -pgnout games.pgn
```

## Building

You need OCaml 5.3 or newer and opam.

```bash
opam switch create 5.3.0
eval $(opam env)        # fish: eval (opam env)
opam install . --deps-only --with-test

dune build --profile=release
```

Use the release profile for playing and benchmarking. The engines are `./_build/default/bin/chessml_uci.exe` and `./_build/default/bin/chessml_xboard.exe`; point your GUI at either.

## Opening book

No book is included (I have not figured out whether I may publish one built from PGNMentor games), so you build your own. This needs about 4 GiB of RAM, 3.5 GiB of disk for the PGN downloads and around 10 minutes:

```bash
./scripts/download_openings.fish   # downloads the PGNMentor openings into openings/
dune build --profile=release && CHESSML_PARALLEL=12 ./_build/default/bin/create_book.exe
```

This reads the first 20 plies of about 4.2M games and writes a 24 MB `book.bin`. A move goes into the book if it was played in at least 3 games and in at least 5% of the games that reached the position; its weight is proportional to how often it was played. From the starting position it plays 1.e4 57%, 1.d4 28%, 1.c4 8% and 1.Nf3 6% of the time.

Any Polyglot book works, including third-party ones. ChessML looks for `book.bin` in:

1. the current directory
2. `$XDG_DATA_HOME/chessml/` (usually `~/.local/share/chessml/`)
3. `~/.chessml/`
4. `/usr/local/share/chessml/` and `/usr/share/chessml/`

GUIs usually start engines in another directory, so install the book for your user:

```bash
mkdir -p ~/.local/share/chessml && cp book.bin ~/.local/share/chessml/
```

## Options

Both protocols accept the same options (`setoption name Hash value 64` in UCI, `option Hash=64` in XBoard):

| Option | Default | Meaning |
| --- | --- | --- |
| `Hash` | 16 | Transposition table size in MB |
| `MaxDepth` | 20 | Maximum search depth; lower it to make the engine weaker |
| `QuiescenceDepth` | 8 | Maximum quiescence search depth |
| `UseQuiescence` | true | Search captures at the horizon |
| `UseTranspositionTable` | true | Use the transposition table |
| `OwnBook` | true | Play moves from the opening book |
| `DebugOutput` | false | Extra output on stderr |

Normally the engine thinks for a share of its remaining clock time, so `MaxDepth` only matters at slow time controls.

## Development

[Just](https://github.com/casey/just) wraps the common commands (`just list`):

```bash
just              # dune build
just test         # dune runtest, a few seconds
just test-search  # search tests including the slow ones
just format       # ocamlformat 0.27.0
dune exec --profile=release examples/search_bench.exe
```

Quick manual check:

```bash
printf "uci\nposition startpos moves e2e4\ngo depth 6\nquit\n" | ./_build/default/bin/chessml_uci.exe
```

The tests cover the core types, perft, evaluation symmetry, SEE, search (mates, tactics, repetition), the opening book and PGN parser, and UCI/XBoard sessions. [docs/README.md](./docs/README.md) describes the code layout, and [CONTRIBUTING.md](CONTRIBUTING.md) says how to contribute.

## Ideas for later

- Tune the evaluation
- Endgame knowledge (mating technique, tapered evaluation)
- Endgame tablebases
- Lazy SMP parallel search
- Find out whether a book built from PGNMentor games can be published

## Credits

- [Chess Programming Wiki](https://www.chessprogramming.org/)
- [Stockfish](https://github.com/official-stockfish/Stockfish)
- [Real World OCaml](https://dev.realworldocaml.org/)
- [OCaml Manual](https://ocaml.org/manual/)
- [PGNMentor](https://www.pgnmentor.com) for the opening PGN databases

## License

[MIT](LICENSE)
