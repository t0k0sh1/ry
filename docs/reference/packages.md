[English](packages.md) | [日本語](../ja/reference/packages.md) | [繁體中文](../zh/reference/packages.md)

# Package Reference

## Overview

Ry uses a package system to organize code. A **package** can be either a single `.ry` file or a directory containing multiple `.ry` files. Use the `from` statement to import packages.

The `std` package (standard library) is automatically imported into every program.

---

## Import Syntax

### Import All Definitions

```ry
from math
```

Imports all functions and types from the package.

### Selective Import

```ry
from math import sqrt
```

Imports only the specified definition.

### Multiple Selective Import

```ry
from math import sqrt, PI
```

Imports multiple definitions separated by commas.

### Relative Import

```ry
from .helper import greet
```

Imports from a module relative to the current file's directory. The `.` prefix restricts resolution to the current directory only (standard library and other search paths are not searched).

### Relative Import from Subdirectory

```ry
from .utils import helper_fn
from .utils.calc import add
```

Imports from a subdirectory relative to the current file's directory.

### Relative Import All from Current Directory

```ry
from . import add, sub
```

Imports specific symbols from the current directory package (all `.ry` files in the directory, excluding `_`-prefixed and `.test.ry` files).

---

## Package Resolution

Packages are resolved using dot-separated notation:

| Import Statement | Resolution |
|---|---|
| `from math` | `math/` directory (package) or `math.ry` file |
| `from utils.math` | `utils/math/` directory or `utils/math.ry` file |
| `from str` | `str/` directory or `str.ry` file |

### Resolution Order

For each search path, the system checks:
1. **Directory** (`{path}/`) — if exists, all `.ry` files in the directory are loaded (package)
2. **File** (`{path}.ry`) — single file (backward compatible)

### Directory Packages

When a package resolves to a directory:
- All `.ry` files in the directory are automatically loaded
- Files starting with `_` are excluded
- Test files (`.test.ry`) are excluded
- No special entry file (like `__init__.py`) is needed
- All functions, types, and directive definitions defined in the directory's files are exported

### Private Symbols

Definitions whose names start with `_` (underscore) are private to the package and cannot be imported:

- Wildcard imports (`from pkg`) automatically exclude `_`-prefixed symbols
- Named imports (`from pkg import _helper`) produce a compile error

```ry
# mylib/internal.ry
fn _helper() -> int:     # private — not importable
    return 42
fn public_api() -> int:  # public — importable
    return _helper()
```

```
mypackage/
  calc.ry      # fn add(), fn sub()
  string.ry    # fn concat()
```

```ry
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
| [`math`](math.md) | Mathematical constants and functions |
| [`io`](io.md) | File I/O, standard input, and byte conversions |
| [`path`](path.md) | File path operations (join, basename, dirname, etc.) |

```ry
from math import sqrt, PI, sin
```

You can also explicitly import specific definitions from standard library packages:

```ry
from str import contains
```

### RY_HOME

The standard library is installed at `$RY_HOME/share/std/`. The default value of `RY_HOME` is `~/.ry`.

```bash
export RY_HOME="$HOME/.ry"   # default
```

### RY_ENV

The `RY_ENV` environment variable controls the runtime environment mode. You can also use the `--env=<value>` CLI flag.

| Value | Alias | `.env` loading | Lib search |
|-------|-------|---------------|------------|
| `prod` | `production` | Disabled | Project override for repo builds → `$RY_HOME/share` (fallback: `lib`) → `exe/../share` (fallback: `lib`) → `exe/share` (fallback: `lib`) |
| `dev` | `development` | `.env.dev` → `.env` | Same as `prod` |
| `test` | — | `.env.test` → `.env` | Same as `prod` |
| `staging` | — | `.env.staging` → `.env` | Same as `prod` |
| `internal` | — | `.env.internal` → `.env` | Project override for repo builds → `exe/../share` (fallback: `lib`) → `exe/share` (fallback: `lib`) (`$RY_HOME` skipped) |
| (unset) (default) | — | `.env` only | Same as `prod` |

Aliases are automatically resolved to their canonical form. For example, `RY_ENV=production` is normalized to `prod`.

In `prod` mode, `.env` files are not loaded for security — production secrets should be managed via infrastructure-level environment variables (CI/CD, secret managers, etc.).

For other modes, `.env.<env>` is loaded first (if it exists), then `.env`. Environment-specific values take precedence because existing variables are not overwritten.

```bash
# Short form (recommended)
RY_ENV=dev ./build/ry app.ry

# Long form (backward compatible)
RY_ENV=development ./build/ry app.ry

# CLI flag
./build/ry --env=dev test

# prod mode: .env is NOT loaded
RY_ENV=prod ./build/ry app.ry

# Additional isolation when developing Ry itself
RY_ENV=internal ./build/ry test
```

When a `ry` executable is built inside the Ry source tree, it can use a repo-local stdlib override from the project's `package.toml`. This keeps repo builds aligned with the checked-out `share/std` even if `~/.ry/share/std` is older. Installed `ry` binaries ignore that override and continue to use `$RY_HOME/share/std`.

---

## Search Path Priority

1. The directory of the importing file
2. Repo-local stdlib override from the current Ry checkout, when using a repo-built `ry`
3. `$RY_HOME/share` (standard library location, falls back to `$RY_HOME/lib` for legacy installs)
4. Executable-relative `share/` directories (falls back to `lib/` for legacy layouts)
5. Paths specified in the `RY_PATH` environment variable (colon-separated)

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
| Relative imports | `from .` and `from .pkg` resolve only against the current file's directory |
| Parent directory imports | `from ..` is not supported |
| Package names | Only letters, digits, and underscores are allowed (no hyphens) |

```ry
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

```ry
# calc.ry
fn add(a: int, b: int) -> int:
    return a + b

fn sub(a: int, b: int) -> int:
    return a - b
```

```ry
# main.ry
from calc import add, sub

print(add(1, 2))   # 3
print(sub(5, 3))   # 2
```

### Directory Package

```
mylib/
  calc.ry
  string.ry
```

```ry
# main.ry
from mylib import add, concat
```

---

## Native Function Naming Convention

Stdlib package functions that are implemented as C runtime functions follow the `__ry_<package>_<function_name>` convention.

> **Note**: This convention applies to stdlib package functions (e.g., `base64`, `filesystem`, `path`). Built-in functions (e.g., `print`, `length`) and math functions use varied implementations (inline LLVM IR, libc calls) and do not follow this naming pattern.

### Format

```text
__ry_<package>_<function_name>
```

### Rules

1. **Prefix**: `__ry_`
2. **Package**: The package name (e.g., `base64` from `from base64 import encode`)
3. **Function name**: The snake_case function name as declared in Ry
4. **Overloads**: When a function has multiple overloads with different arities, append the argument count as a suffix (e.g., `__ry_path_join2`, `__ry_path_join3`)
5. **Error getter**: Each package that returns `Result` types provides `__ry_<pkg>_get_last_error`

### Examples

| Ry declaration | C runtime function name |
|---------------|------------------------|
| `base64::encode(data: str) -> str` | `__ry_base64_encode` |
| `filesystem::list_dir(path: str) -> Result<List<str>, Error>` | `__ry_filesystem_list_dir` |
| `path::join(a: str, b: str) -> str` | `__ry_path_join2` |
| `path::join(a: str, b: str, c: str) -> str` | `__ry_path_join3` |
