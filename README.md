<p align="center">
  <img src="docs/logo.png" alt="Ry" width="200">
</p>

<p align="center">
  A simple programming language based on LLVM JIT.<br>
  It reads source code, compiles it to native code with LLVM ORC JIT, and executes it immediately.
</p>

## Features

- **LLVM JIT Compilation** — Fast native execution powered by ORC LLJIT
- **Rich Type System** — `int`, `float`, `bool`, `str`, `Option<T>`, `Error`, tuples, `List<T>`, `Map<K,V>`, `Set<T>`, `enum`, function types, user-defined records, union types (`int | str`)
- **Operators** — Arithmetic, comparison, logical, bitwise (`>>>` logical right shift), compound assignment, `in` / `not in`, string repetition (`"ab" * 3`), collection arithmetic (`List + List` concatenation, `Map + Map` merge, `Set + Set` union), `as` type cast, error propagation `?`, with operator overloading support
- **Pattern Matching** — `case` expressions with enum / `Option` / `Result` / literal / tuple / record destructuring, guard clauses (`x if x > 0`), exhaustiveness checking
- **F-String** — String interpolation with `f"Hello {name}"`
- **Design by Contract** — `require` (preconditions), `ensure` (postconditions), `invariant` (record invariants)
- **Directives** — `@deprecated`, `@const`, `@native`, `@parallel`, `@inline`, `@each`, `@property`, `@describe`, `@it` and other compile-time instructions
- **Functions** — `fn` definitions, recursion, overloading, lambdas (closures), higher-order functions, UFCS
- **Control Flow** — `if`/`else`, `case`, `while`, `for...in`, `break`/`continue`
- **File I/O** — File read/write, byte operations, standard input (`std.io`)
- **Filesystem** — Directory listing, recursive walk, glob, copy, move, remove, permissions (`std.filesystem`)
- **Packages** — Directory-based packages, auto-imported `std` library, `from ... import ...`
- **Concurrency** — `async`/`await` with work-stealing scheduler, `@parallel` for loops, native thread API (`std.thread`)
- **Memory Management** — ARC (Automatic Reference Counting) with a cycle collector (`std.gc`)
- **Testing Framework** — `@describe` / `@it` directives on named functions, matchers (`expect(x).to_eq(...)`), parameterized tests (`@each`), property-based tests (`@property`)
- **Type Safety** — Type inference, type annotations, immutable type bindings, `@const` directive

## Sample Code

```python
# Variables and types
x: int = 42
name: str = "hello"
pi = 3.14159

# Function definition
fn factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)

print(factorial(5))    # 120

# Lambdas and closures
offset = 10
add_offset = (x: int) -> int => x + offset
print(add_offset(5))   # 15

# Records
record Point:
    x: int
    y: int

fn operator+(a: Point, b: Point) -> Point:
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

# Chained / compound assignment on indexed fields
pts = [Point(1, 2), Point(3, 4)]
pts[0].x += 10          # list[i].field compound assignment
print(pts[0].x)         # 11

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
curl -fsSL https://raw.githubusercontent.com/t0k0sh1/ry/main/install.sh | sh -s v0.0.8
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

For detailed language specifications, see [docs/](docs/README.md).
