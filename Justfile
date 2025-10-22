# ChessML - Just recipes for building and testing

# Default recipe (runs when you type 'just')
default: build

# Build the project
build:
    dune build

# Clean build artifacts
clean:
    dune clean

# Run all tests (includes quick search tests ~11s)
test:
    dune runtest

# Run tests with verbose output
test-verbose:
    dune runtest --verbose

test-search:
    dune exec test/engine/test_search.exe

# Run specific test suites
test-core:
    dune exec test/core/test_types.exe
    dune exec test/core/test_square.exe
    dune exec test/core/test_bitboard.exe
    dune exec test/core/test_move.exe

test-engine:
    dune exec test/engine/test_position.exe
    dune exec test/engine/test_game.exe
    dune exec test/engine/test_zobrist.exe
    dune exec test/engine/test_eval.exe

test-protocols:
    dune exec test/protocols/test_uci.exe
    dune exec test/protocols/test_xboard.exe

test-integration:
    dune exec test/test_integration.exe

# Install the package
install:
    dune install

# Uninstall the package
uninstall:
    dune uninstall

# Build a specific example
example name:
    dune exec examples/{{name}}.exe

# Build documentation
doc:
    dune build @doc

# Watch mode - rebuild on file changes
watch:
    dune build --watch

# Format code
format:
    dune build @fmt --auto-promote

# Check formatting without changing files
format-check:
    dune build @fmt

# Run perft benchmarks
perft depth="5":
    dune exec examples/perft_example.exe {{depth}}

# Show test coverage (runs quick tests by default)
coverage:
    dune runtest --instrument-with bisect_ppx --force
    bisect-ppx-report html
    @echo "Coverage report generated in _coverage/"

# Clean everything including opam-installed packages
clean-all: clean
    rm -rf _opam

# Setup development environment
setup:
    opam install . --deps-only --with-test --with-doc

# List all recipes
list:
    @just --list

