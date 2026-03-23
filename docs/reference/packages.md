[English](packages.md) | [日本語](../ja/reference/packages.md) | [繁體中文](../zh/reference/packages.md)

# Package Reference

## Overview

Ry uses a package system to organize code. A **package** can be either a single `.ry` file or a directory containing multiple `.ry` files. Use the `from` statement to import packages.

The `std` package (standard library) is automatically imported into every program.

---

## Import Syntax

### Import All Definitions

```python
from math
```

Imports all functions and types from the package.

### Selective Import

```python
from math import add
```

Imports only the specified definition.

### Multiple Selective Import

```python
from math import add, sub
```

Imports multiple definitions separated by commas.

---

## Package Resolution

Packages are resolved using dot-separated notation:

| Import Statement | Resolution |
|---|---|
| `from math` | `math/` directory (package) or `math.ry` file |
| `from utils.math` | `utils/math/` directory or `utils/math.ry` file |
| `from std.str` | `std/str/` directory or `std/str.ry` file |

### Resolution Order

For each search path, the system checks:
1. **Directory** (`{path}/`) — if exists, all `.ry` files in the directory are loaded (package)
2. **File** (`{path}.ry`) — single file (backward compatible)

### Directory Packages

When a package resolves to a directory:
- All `.ry` files in the directory are automatically loaded
- Files starting with `_` are excluded
- No special entry file (like `__init__.py`) is needed
- All functions and types defined in the directory's files are exported

### Private Symbols

Definitions whose names start with `_` (underscore) are private to the package and cannot be imported:

- Wildcard imports (`from pkg`) automatically exclude `_`-prefixed symbols
- Named imports (`from pkg import _helper`) produce a compile error

```python
# mylib/internal.ry
fn _helper() -> int:     # private — not importable
    return 42
fn public_api() -> int:  # public — importable
    return _helper()
```

```
mypackage/
  math.ry      # fn add(), fn sub()
  string.ry    # fn concat()
```

```python
from mypackage          # imports add, sub, concat
from mypackage import add   # imports only add
```

---

## Standard Library (`std`)

The `std` package is automatically imported into every program. It provides:
- Built-in functions (`print`, `length`, `range`, etc.)
- String functions (`contains`, `find`, `replace`, etc.)
- Type conversion functions (`to_int`, `to_float`, `to_str`)
- Collection functions (`map`, `filter`, `sort`, etc.)

### Sub-packages

The following sub-packages require explicit import:

| Package | Description |
|---------|-------------|
| [`std.math`](math.md) | Mathematical constants and functions |
| [`std.io`](io.md) | File I/O, standard input, and byte conversions |

```python
from std.math import sqrt, PI, sin
```

You can also explicitly import specific definitions from `std`:

```python
from std.str import contains
```

### RY_HOME

The standard library is installed at `$RY_HOME/lib/std/`. The default value of `RY_HOME` is `~/.ry`.

```bash
export RY_HOME="$HOME/.ry"   # default
```

---

## Search Path Priority

1. The directory of the importing file
2. `$RY_HOME/lib` (standard library location)
3. Executable-relative `lib/` directories
4. Paths specified in the `RY_PATH` environment variable (colon-separated)

---

## RY_PATH Environment Variable

Specifying colon-separated directories in `RY_PATH` adds them to the package search path.

```bash
export RY_PATH="/usr/local/ry/lib:/home/user/ry-packages"
```

---

## Constraints

| Constraint | Details |
|------|------|
| Allowed location | Top level only (not inside functions or blocks) |
| Duplicate imports | Automatically skipped (no error) |
| Circular imports | Compile error |

```python
# Error example: Import inside a block
fn main():
    from math   # Error: imports only allowed at top level

# OK: Importing the same package multiple times does not cause an error
from math
from math   # Skipped
```

---

## Creating Package Files

### Single File Package

```python
# math.ry
fn add(a: int, b: int) -> int:
    return a + b

fn sub(a: int, b: int) -> int:
    return a - b
```

```python
# main.ry
from math import add, sub

print(add(1, 2))   # 3
print(sub(5, 3))   # 2
```

### Directory Package

```
mylib/
  math.ry
  string.ry
```

```python
# main.ry
from mylib import add, concat
```
