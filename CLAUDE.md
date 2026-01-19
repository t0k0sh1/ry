# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**ry** is a simple arithmetic expression evaluator and scripting language written in Rust. It supports:
- Integer and floating-point arithmetic
- Boolean types and comparison operators
- Interactive REPL mode or `.ry` script file execution

## Build and Development Commands

```bash
# Build
cargo build                  # Debug build
cargo build --release        # Optimized build

# Run
cargo run                    # Start REPL
cargo run -- <file.ry>       # Execute script file

# Test
cargo test                   # All tests
cargo test --lib             # Unit tests only (in src/eval.rs)
cargo test --test '*'        # Integration tests only
cargo test <pattern>         # Run tests matching pattern

# Code Quality
cargo fmt --check            # Check formatting
cargo fmt                    # Auto-format
cargo clippy -- -D warnings  # Lint (warnings as errors)
```

Pre-commit hooks via lefthook run format, lint, and test checks automatically.

## Architecture

The interpreter follows a classic Lexer → Parser → Evaluator pipeline, all contained in `src/eval.rs`:

1. **Value** (`enum`): Runtime values - `Int(i64)`, `Float(f64)`, `Bool(bool)`
2. **Token** (`enum`): Lexical tokens for numbers, operators, parentheses, booleans
3. **Lexer**: Tokenizes input strings, handles multi-character operators (`**`, `//`, `==`, `!=`, `<=`, `>=`)
4. **Parser**: Recursive descent parser with operator precedence handling
5. **Evaluator**: Recursively evaluates parsed expressions

**Key files:**
- `src/main.rs`: CLI entry point
- `src/lib.rs`: Public API exports, REPL and file execution logic
- `src/eval.rs`: Core interpreter implementation (~1100 lines including tests)

## Operator Precedence (lowest to highest)

1. Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
2. Additive: `+`, `-`
3. Multiplicative: `*`, `/`, `%`, `//`
4. Power: `**` (right-associative)

## Type System Behavior

- Integer operations use checked arithmetic with overflow fallback to float
- Mixed Int/Float operations promote to Float
- Division (`/`) always returns Float
- Floor division (`//`) returns Int when both operands are Int, otherwise Float
- Comparison operators return Bool
