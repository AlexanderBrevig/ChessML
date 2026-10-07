# ChessML - Just recipes for building and testing

# Default recipe (runs when you type 'just')
default: build

# Build the project
build:
    dune build

# Clean build artifacts
clean:
    dune clean

# Run all tests (a few seconds)
test:
    dune runtest

# Run tests with verbose output
test-verbose:
    dune runtest --verbose

# Run the search tests including the `Slow cases
test-search:
    dune exec test/engine/test_search.exe

# Run the tests of one directory
test-core:
    dune build @test/core/runtest --force

test-engine:
    dune build @test/engine/runtest --force

test-protocols:
    dune build @test/protocols/runtest --force

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

# Run the perft example (move generation counts)
perft:
    dune exec examples/perft_example.exe

# Show test coverage (needs bisect_ppx: opam install bisect_ppx)
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

