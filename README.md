[English](README.md) | [日本語](README.ja.md) | [繁體中文](README.zh.md)

<p align="center">
  <img src="docs/logo.png" alt="Ry" width="200">
</p>

<p align="center">
  A simple programming language based on LLVM JIT.<br>
  It reads source code, compiles it to native code with LLVM ORC JIT, and executes it immediately.
</p>

## Features

- **LLVM JIT Compilation** — Fast native execution powered by ORC LLJIT
- **Rich Type System** — `int`, `float`, `bool`, `str`, `Option<T>`, `Error`, tuples, `List<T>`, `Map<K,V>`, `Set<T>`, `enum`, function types, user-defined structs
- **Operators** — Arithmetic, comparison, logical, bitwise (`>>>` logical right shift), compound assignment, `in` / `not in`, string repetition (`"ab" * 3`), `as` type cast, with operator overloading support
- **F-String** — String interpolation with `f"Hello {name}"`
- **Design by Contract** — `require` (preconditions), `ensure` (postconditions), `invariant` (record invariants), `old()`, `result`
- **Directives** — `@deprecated` compile-time metadata annotations
- **Functions** — `function` definitions, recursion, overloading, lambdas (closures), higher-order functions, UFCS
- **Control Flow** — `if`/`else`, `when`, `while`, `for...in`, `break`/`continue`
- **File I/O** — File read/write, byte operations, standard input (`std.io`)
- **Filesystem** — Directory listing, recursive walk, glob, copy, move, remove, permissions (`std.filesystem`)
- **Packages** — Directory-based packages, auto-imported `std` library, `from ... import ...`
- **Concurrency** — `async`/`await` with work-stealing scheduler, `@parallel` for loops, native thread API (`std.thread`)
- **Type Safety** — Type inference, type annotations, immutable type bindings, `@const` directive

## Sample Code

```python
# Variables and types
x: int = 42
name: str = "hello"
pi = 3.14159

# Function definition
function factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)

print(factorial(5))    # 120

# Lambdas and closures
offset = 10
add_offset = (x: int) -> int => x + offset
print(add_offset(5))   # 15

# Structs
record Point:
    x: int
    y: int

function operator+(a: Point, b: Point) -> Point:
    return Point(a.x + b.x, a.y + b.y)

p = Point(1, 2) + Point(3, 4)
print(p.x)             # 4

# Collections
xs = [1, 2, 3]
m = {"a": 1, "b": 2}
s = {1, 2, 3}

for x in xs:
    print(x)

print(2 in s)          # true
print(m["a"])           # 1

# Stream-like operations (filter, map, sort)
result = [5, 3, 1, 4, 2].filter((x: int) => x > 1).map((x: int) => x * 10).sort()
print(result)          # [20, 30, 40, 50]

# Enums
enum Color:
    Red
    Green
    Blue

c = Color::Red
print(c)               # Red

# Package import
from math import sqrt, PI
print(sqrt(PI))
```

## Installation

### One-liner (macOS Apple Silicon)

```bash
curl -fsSL https://raw.githubusercontent.com/t0k0sh1/ry/main/install.sh | sh
```

To specify a particular version:

```bash
curl -fsSL https://raw.githubusercontent.com/t0k0sh1/ry/main/install.sh | sh -s v0.0.4
```

By default, it installs to `~/.local/bin`. You can change this with the `RY_INSTALL_DIR` environment variable.

The standard library is installed to `$RY_HOME/share/std/` (default: `~/.ry/share/std/`).

### Build from Source

Requirements:
- LLVM 21
- CMake 3.20+
- C++17 compatible compiler

```bash
cmake -B build -DLLVM_DIR=/usr/local/llvm/lib/cmake/llvm
cmake --build build
```

## Usage

```bash
ry <file.ry>              # Run a Ry script
echo '<code>' | ry -c      # Run code from stdin
ry test [options] [path]   # Run tests (*.test.ry)
ry init                    # Initialize a project in current directory
ry new <name>              # Create a new project
ry run [<script-name>]     # Run a project script
ry fmt [options] [path]    # Format source files
ry self-update             # Update ry itself
```

The `self-update` command verifies release artifacts using Ed25519 signature verification and SHA-256 checksums. Signature verification is required by default; if the signature file is unavailable, the update is aborted. Set `RY_SKIP_SIGNATURE=1` to allow proceeding when the signature file is missing (not recommended). Invalid signatures always abort the update regardless of this setting.

Stdin also supports here-documents:

```bash
ry -c <<'RY'
a = 1
b = 2
print(a + b)
RY
```

Run `ry <command> --help` for detailed options.

For internal execution analysis, Ry also supports a structured trace mode:

```bash
ry --trace app/main.ry
ry --trace-out=/tmp/ry-trace.jsonl app/main.ry
ry test --trace tests/spec
```

`--trace` emits JSON Lines to stderr by default. Use `--trace-out` to redirect the trace stream to a file while keeping program stdout unchanged.

## Development

```bash
cd build && ctest --output-on-failure
```

## Documentation

For detailed language specifications and tutorials, see [docs/](docs/README.md).
