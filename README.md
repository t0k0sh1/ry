[English](README.md) | [日本語](README.ja.md) | [繁體中文](README.zh.md)

# ry

A simple programming language based on LLVM JIT. It reads source code, compiles it to native code with LLVM ORC JIT, and executes it immediately.

## Features

- **LLVM JIT Compilation** — Fast native execution powered by ORC LLJIT
- **Rich Type System** — `int`, `float`, `bool`, `str`, `Option<T>`, tuples, `List<T>`, `Map<K,V>`, `Set<T>`, `enum`, function types, user-defined structs
- **Operators** — Arithmetic, comparison, logical, bitwise (`>>>` logical right shift), compound assignment, `in` / `not in`, string repetition (`"ab" * 3`), with operator overloading support
- **Functions** — `fn` definitions, recursion, overloading, lambdas (closures), higher-order functions, UFCS
- **Control Flow** — `if`/`elif`/`else`, `while`, `for...in`, `break`/`continue`
- **Modules** — Function imports via `from ... import ...`
- **Type Safety** — Type inference, type annotations, immutable type bindings, let/var

## Sample Code

```python
# Variables and types
let x: int = 42
let name: str = "hello"
let pi = 3.14159

# Function definition
fn factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)

print(factorial(5))    # 120

# Lambdas and closures
let offset = 10
let add_offset = (x: int): int => x + offset
print(add_offset(5))   # 15

# Structs
type Point:
    x: int
    y: int

fn operator+(a: Point, b: Point) -> Point:
    return Point(a.x + b.x, a.y + b.y)

let p = Point(1, 2) + Point(3, 4)
print(p.x)             # 4

# Collections
let xs = [1, 2, 3]
let m = {"a": 1, "b": 2}
let s = {1, 2, 3}

for x in xs:
    print(x)

print(2 in s)          # true
print(m["a"])           # 1

# Stream-like operations (filter, map, sort)
let result = [5, 3, 1, 4, 2].filter((x: int) -> x > 1).map((x: int) -> x * 10).sort()
print(result)          # [20, 30, 40, 50]

# Enums
enum Color:
    Red
    Green
    Blue

let c = Color::Red
print(c)               # Red

# Module import
from math import add
print(add(1, 2))
```

## Installation

### One-liner (macOS Apple Silicon)

```bash
curl -fsSL https://raw.githubusercontent.com/t0k0sh1/ry/main/install.sh | sh
```

To specify a particular version:

```bash
curl -fsSL https://raw.githubusercontent.com/t0k0sh1/ry/main/install.sh | sh -s v0.0.1
```

By default, it installs to `~/.local/bin`. You can change this with the `RY_INSTALL_DIR` environment variable.

### Build from Source

Requirements:
- LLVM 21
- CMake 3.20+
- C++17 compatible compiler

```bash
cmake -B build -DLLVM_DIR=/usr/local/llvm/lib/cmake/llvm
cmake --build build
```

## Run

```bash
ry <file.ry>
```

## Test

```bash
cd build && ctest --output-on-failure
```

## Documentation

For detailed language specifications and tutorials, see [docs/](docs/README.md).
