# Contributing to ChessML

Thanks for your interest in contributing to ChessML! This is a personal learning project, so contributions and suggestions are welcome as I explore chess programming concepts.

## 🎯 Project Philosophy

ChessML is a **learning-focused hobby project** with no fixed roadmap. Code quality and features evolve organically as I experiment with different ideas. Expect things to change arbitrarily as new concepts are explored!

## 🚀 Getting Started

### Prerequisites

- OCaml 5.3+
- Opam package manager
- Dune build system

### Setup Development Environment

```bash
# Clone the repository
git clone https://github.com/alexanderbrevig/chessml.git
cd chessml

# Install dependencies
opam install . --deps-only --with-test --with-doc

# Build the project
dune build

# Run tests
dune runtest
```

## 🧪 Testing

Before submitting changes, ensure tests pass:

```bash
# Quick tests (~11s)
just test

# Full test suite including deep search validation
just test-search

# Specific test categories
just test-core
just test-engine
just test-integration
```

## 📝 Code Style

ChessML follows standard OCaml conventions:

```bash
# Format code automatically
just format

# Check formatting
just format-check
```

## 🐛 Reporting Issues

When reporting bugs, please include:

- FEN position (if applicable)
- Expected vs actual behavior
- Steps to reproduce
- OCaml version and OS

## 💡 Suggesting Features

Feature suggestions are welcome! Keep in mind:

- This is an educational project - interesting chess programming concepts are prioritized over completeness
- Features may be implemented experimentally and changed/removed later
- No guarantees on timeline or acceptance

## 📬 Submitting Changes

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Make your changes
4. Run tests and formatting
5. Commit with clear messages using [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/)
6. Push to your fork
7. Open a Pull Request

## 🤝 Code of Conduct

Be respectful, constructive, and patient. This is a learning environment!

## 📚 Resources

Helpful resources for chess programming:

- [Chess Programming Wiki](https://www.chessprogramming.org/)
- [Stockfish Documentation](https://github.com/official-stockfish/Stockfish)
- [Real World OCaml](https://dev.realworldocaml.org/)

## ❓ Questions?

Feel free to open an issue for questions or discussion!
