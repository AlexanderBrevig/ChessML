# Examples

This directory contains example programs demonstrating ChessML's features and capabilities, organized by category.

## Core Examples - Educational and Demonstration

These examples demonstrate basic ChessML functionality:

- **`perft_example.ml`** - Standard perft (move generation) benchmark. Use this to test correctness and performance of move generation.
- **`search_example.ml`** - Basic search usage demonstration. Shows how to use the search API.
- **`move_ordering_demo.ml`** - Demonstrates move ordering techniques (MVV-LVA, killer moves, history heuristic).
- **`quiescence_test.ml`** - Shows quiescence search in action on tactical positions.
- **`history_demo.ml`** - Demonstrates the history heuristic and its impact on move ordering.

**Usage:**

```bash
dune exec examples/perft_example.exe
dune exec examples/search_example.exe
dune exec examples/move_ordering_demo.exe
```

## Parallel Search Benchmarks and Analysis

Tools for analyzing parallel search performance:

- **`parallel_bench.ml`** - Comprehensive parallel search benchmark suite.
- **`parallel_quality_test.ml`** - Tests search quality (accuracy) with parallel search.
- **`speedup_analysis.ml`** - Detailed speedup analysis across different positions and depths.
- **`quick_parallel_test.ml`** - Quick parallel vs single-threaded comparison.
- **`bench_breakdown.ml`** - Performance breakdown by search component.

**Usage:**

```bash
dune exec examples/speedup_analysis.exe
dune exec examples/quick_parallel_test.exe
```

## Advanced Search Techniques - Educational

These examples demonstrate advanced search optimizations:

- **`test_futility.ml`** - Futility pruning demonstration and effectiveness testing.
- **`test_lmr.ml`** - Late Move Reductions (LMR) demonstration.
- **`test_null_move.ml`** - Null move pruning demonstration.
- **`profile_test.ml`** - Profiling and performance analysis.

**Usage:**

```bash
dune exec examples/test_futility.exe
dune exec examples/test_lmr.exe
```

## Opening Book Utilities

Tools for working with Polyglot opening books:

- **`book_test.ml`** - Test opening book integration, show book moves for positions.
- **`create_expanded_book.ml`** - Create an expanded book from an existing one.
- **`dump_book.ml`** - Dump book contents for inspection.
- **`search_book.ml`** - Search for specific positions in a book.

**Usage:**

```bash
dune exec examples/book_test.exe
dune exec examples/dump_book.exe book.bin
dune exec examples/search_book.exe book.bin "e2e4"
```

## Running Examples

Build all examples:

```bash
dune build examples
```

Run a specific example:

```bash
dune exec examples/<name>.exe
```

Or use the Justfile:

```bash
just example <name>  # without .exe extension
```

## Notes

- Examples marked "Educational" are useful for learning how specific techniques work
- Benchmark examples help measure and optimize performance
- Book utilities are practical tools for managing opening books
- All examples use the ChessML library and demonstrate real-world usage patterns
