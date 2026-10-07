# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

ChessML is a personal-learning bitboard chess engine in OCaml (>= 5.3) with UCI and XBoard front-ends. There is no roadmap; features are experimental and may be reworked freely. Commits follow Conventional Commits (`feat:`, `fix:`, `docs:` …). Evaluation tuning is the author's domain: fix clear bugs, but ask before changing heuristic weights or margins.

## Commands

`just` wraps dune (`just list` for all recipes). On this machine there is no opam switch; run commands inside
`nix-shell -p ocamlPackages.ocaml dune_3 ocamlPackages.findlib ocamlPackages.alcotest ocamlPackages.domainslib ocaml-ng.ocamlPackages_5_3.ocamlformat_0_27_0 --run "<cmd>"`
(ocamlformat 0.27.0 is marked broken in the default package set, hence the 5.3 one).

```bash
dune build                          # debug build (also: just)
dune build --profile=release        # ALWAYS use for benchmarks/perf measurements
dune runtest                        # all tests (a few seconds; test_search runs with -q = quick only)
just test-search                    # search tests incl. `Slow cases
dune exec test/engine/test_eval.exe                     # one test executable
dune exec test/engine/test_eval.exe -- test <suite>     # one alcotest suite
just test-engine                    # every test in test/engine (also test-core, test-protocols)
just format                         # dune build @fmt --auto-promote (ocamlformat 0.27.0, janestreet profile)
just example <name>                 # dune exec examples/<name>.exe
dune exec --profile=release examples/search_bench.exe
just setup                          # opam install . --deps-only --with-test --with-doc
```

Manual engine smoke tests:

```bash
printf "uci\nposition startpos moves e2e4\ngo depth 6\nquit\n" | ./_build/default/bin/chessml_uci.exe
printf "xboard\nprotover 2\nnew\nsd 4\nusermove e2e4\nquit\n" | ./_build/default/bin/chessml_xboard.exe
```

Each test file is its own executable declared in the directory's `dune` file — a new test file needs a `(test ...)` stanza there. CI (`.github/workflows/ci.yml`) checks formatting, builds and runs the tests.

## Architecture

Three dune sub-libraries, re-exported by the umbrella `lib/chessml.ml` (so callers write `Chessml.Position`, `Chessml.Search`, etc.):

- **`chessml.core`** (`lib/core`): `Types`, `Square`, `Bitboard`, `Move`. `bitboard_stubs.c` provides unboxed `[@@noalloc]` ctz/msb/popcount intrinsics.
- **`chessml.engine`** (`lib/engine`): everything else. The `(modules ...)` list in `lib/engine/dune` is explicit — **new engine modules must be added there**.
- **`chessml.protocols`** (`lib/protocols`): `Uci` and `Xboard` sessions (`handle_line` per input line, `main_loop` on stdin), sharing options, book access and time budgeting via `Protocol_common`.

Warnings are not suppressed (dev profile treats them as errors). Modules with an `.mli` must keep it in sync.

Key engine pieces and how they connect:

- **`Position`** is immutable (`make_move : t -> Move.t -> t`): mailbox board + piece/color/occupancy bitboards + Polyglot Zobrist `key`, all updated through one `toggle` primitive. `Position.compute_key` recomputes the key from scratch (tests assert they agree). **`Game`** adds the key history; `Game.find_move` turns "e2e4"/"e7e8q"/"O-O" into the matching legal move — always use it for external input, never `Move.of_uci` directly (that yields a `Quiet` move with no castling/ep semantics).
- **`Zobrist`** holds the official Polyglot Random64 keys (`Polyglot_random`); the position key *is* the opening-book key. En passant is only hashed when a capture is possible.
- **`Movegen`** generates pseudo-legal moves from bitboards and keeps those where the king is safe on the post-move occupancy. `attackers_to pos sq color occupied` (magics for sliders) is the single attack primitive used by movegen, check detection, SEE and eval. Correctness: `test/engine/test_perft.ml`.
- **`Eval`** returns centipawns from the side to move's perspective (0 for insufficient material) and orchestrates `Eval_pawn_structure` (cached in `Pawn_cache`, keyed by both pawn bitboards, score white-minus-black), `Eval_pieces`, `Eval_king_safety`, `Eval_endgame`, and `Piece_tables` (tables written a8-first; White reads `sq lxor 56`). Eval must be color-symmetric (`test_eval` checks mirrored FENs).
- **`Search`**: iterative-deepening PVS + quiescence. All tables (TT, killers by ply, history, countermoves) live in `Search.state` (`default_state` for the protocols; `new_game`, `set_hash_size_mb`). Repetition/50-move/insufficient-material draws are detected in the tree. `find_best_move` takes `?max_time_ms`, `?stop` (an `Atomic` flag another thread may set) and `?on_iteration`. Pruning margins, LMR table and move ordering live in `Search_common`.
- **`Score`**: mate scores encode distance (`mated_in ply`, `to_tt`/`of_tt` for TT storage, `to_uci`). Never hard-code mate values.
- **`Config`** is a global ref of search options set via `Protocol_common.set_option` (UCI `setoption`, XBoard `option`). Tests that change it should call `Config.reset_to_defaults`.
- **Opening book**: real Polyglot format (`Polyglot` encodes moves per spec, castling as king-takes-rook; keys sorted unsigned), read by `Opening_book`. Book paths: `Config.get_book_paths`. `book.bin` is not committed.
- **Building the book**: `fish scripts/download_openings.fish` fetches the pgnmentor collection into `openings/` (3.5 GB, gitignored); `CHESSML_PARALLEL=12 ./_build/default/bin/create_book.exe` (release build) counts the first 20 plies of every game via `Pgn_parser` (`domainslib` workers, merged in memory) and writes `book.bin`. A move is kept if it was played in at least 3 games and at least 5% of the games reaching that position; weights are linear in the game count, relative to the most played move of the same position (log weights made fringe openings like 1.b4 far too common, and lost 23-42 head to head).
- **UCI** runs the search on a `Thread` so `stop`/`isready` work mid-search; XBoard thinks synchronously.

## Other directories

- `examples/`: benchmarks, demos and book tools (each declared in `examples/dune`).
- `docs/` + `index.md`, `_config.yml`, `_sass`, `Gemfile`: Jekyll site of chess-programming articles deployed to GitHub Pages on push to `main` (`.github/workflows/deploy-docs.yml`).
- `scripts/*.fish`: helper scripts for downloading openings and running cutechess-cli Elo matches against Stockfish.

## Measuring strength

Engine changes should be checked with games, not only tests. `cutechess-cli` and `stockfish` are not installed globally here; use `nix-shell -p cutechess stockfish`. Compare a change head to head against the previous binary (100+ games at 10s+0.1s), or run a gauntlet against Stockfish with `option.UCI_LimitStrength=true option.UCI_Elo=<1600|1900|2200>` (see README "Strength"; current estimate ~1850-2000). Twenty games per level is about +-170 Elo of noise.
