# Module Reference

## Overview

Ry uses a module system to organize code. A **module** can be either a single `.ry` file or a directory containing multiple `.ry` files. Two import forms are supported:

| Form | Syntax | Behavior |
|---|---|---|
| Selective import | `from <module> import <name>` | Binds the named symbol into the current file's scope |
| Qualified import | `import <module>` | Binds the module itself; access members via `<module>.<name>` |

The standard library (`std`) is automatically imported into every program.

---

## Import Syntax

### Import All Definitions

```ry
from math
```

Imports all functions and types from the module.

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

### Symbol Alias

```ry
from math import sqrt as squareRoot
```

Each imported name may carry an optional `as <ident>` clause that binds the
symbol under a different local name. Aliases can be mixed with non-aliased
names in a single statement:

```ry
from math import sqrt as sr, PI, sin as s
```

Self-alias (`foo as foo`) is normalized to a plain import — the formatter
will round-trip it as `from math import foo`.

Aliases work end-to-end for `@const` values, functions, records, enums,
and type aliases. Aliases for mutable global values (non-`@const`
assignments) and `@directive` definitions remain unsupported — mutable
globals and directives cannot be re-bound via `as`. Aliases for generic
functions and generic enums are also rejected today; non-generic forms
work as expected.

### Braced Selective Import

```ry
from math import { PI, E }
```

The selective import list may be wrapped in braces. Single-line and
multi-line forms are both accepted, with an optional trailing comma —
useful for importing many symbols without losing readability:

```ry
from math import {
  PI,
  E,
}
```

Braced form composes with `as <ident>` aliases exactly like the
comma-separated form:

```ry
from math import { PI as p, E }
```

The empty form `from math import {}` is rejected as a parse error.
Symbol resolution, visibility, and alias limitations are identical to
the comma-separated form — the braces are purely syntactic. Editor
support (tree-sitter): single-line braced imports are recognized; brace-
internal newline suppression for the multi-line form is tracked in
[#1727](https://github.com/t0k0sh1/ry/issues/1727).

### Wildcard Import (unsupported)

Wildcard import `from math import *` is **not** supported. Every `*`
at an import-name position is rejected with an actionable diagnostic:

```text
error: selective import does not support wildcards ('from x import *');
use 'from x import a, b' or 'from x import {a, b}' instead
```

The diagnostic fires uniformly across all four wildcard positions —
`from math import *`, `from math import {*}`, `from math import a, *`,
and `from math import {a, *}`. Use the selective form
(`from math import sqrt, PI`) or the braced form
(`from math import { sqrt, PI }`) instead.

### Qualified Import

```ry
import math
x = math.sqrt(2.0)       # 1.4142135623730951
y = math.PI              # 3.141592653589793
```

`import <module>` binds the module name itself; members are then accessed
via `<module>.<name>` for both functions and constants. The dot here is
not UFCS — it is a qualified namespace lookup that resolves directly to
the module's exports.

Qualified import composes with selective import — both forms may target
the same module in the same file:

```ry
import math
from math import PI
print(math.sqrt(PI))     # 1.7724538509055159
print(PI)                # 3.141592653589793
```

Qualified import is especially useful when two modules export the same
name. Importing one selectively and the other qualified avoids the
collision:

```ry
from str import contains
import list

xs: List<int> = [1, 2, 3]
list.append(xs, 4)             # list module's append
contains("hello", "ell")       # str module's contains
```

**Constraints (v0.0.23)**:

| Constraint | Details |
|---|---|
| Module form | Single identifier only. `import a.b` is rejected — use `from a.b import ...` instead. |
| Alias | `import <module> as <local>` registers `<local>` as the effective module name (Python-style: alias **replaces** the original — bare `<module>` is no longer in scope). The alias must be camelCase. Two imports that resolve to the same effective name (e.g. `import math as m` then `import path as m`) are a parse error. |
| Module scope | Both standard library and user-defined modules are supported. Generic functions, `enum` declarations, and `type` aliases inside a qualified-imported user-defined module are rejected with a diagnostic suggesting `from <module> import ...` (Phase 2). |
| Duplicate | `import math` followed by `import math` in the same file is a parse error. |
| Shadowing | After `import math`, declaring `math: int = ...` (or any local named `math`) is a parse error. Rename the local or remove the matching `import` statement. |
| Unimported stdlib use | `<mod>.fn(...)` or `<mod>.field` where `<mod>` names a registered stdlib module (`math`, `json`, `path`, …) and `<mod>` itself is not in scope as an imported module is rejected at codegen with `module '<mod>' is not imported (add 'import <mod>' at the top of the file)`. When the module has been imported under an alias (`import <mod> as <alias>`), the bare `<mod>` reference is rejected with a targeted alias suggestion instead: `'<mod>' is not defined. Did you mean '<alias>' (aliased from '<mod>')?` — after `import math as m`, only `m.fn(...)` works; `math.fn(...)` triggers the alias suggestion. A local variable that happens to share a stdlib module name shadows the module and is exempt from this check. (#1746, #1747) |

`import` is only valid at top level — placing it inside a function or
block is rejected, matching the existing rule for `from` imports.

### Relative Import

```ry
from .helper import greet
```

Imports from a module relative to the current file's directory. The `.` prefix restricts resolution to the current directory only (standard library and other search paths are not searched).

### Relative Import from Subdirectory

```ry
from .utils import helperFn
from .utils.calc import add
```

Imports from a subdirectory relative to the current file's directory.

### Relative Import All from Current Directory

```ry
from . import add, sub
```

Imports specific symbols from the current directory module (all `.ry` files in the directory, excluding `.test.ry` files).

---

## Module Resolution

Modules are resolved using dot-separated notation:

| Import Statement | Resolution |
|---|---|
| `from math` | `math/` directory (module) or `math.ry` file |
| `from utils.math` | `utils/math/` directory or `utils/math.ry` file |
| `from str` | `str/` directory or `str.ry` file |

### Resolution Order

For each search path, the system checks:
1. **Directory** (`{path}/`) — if exists, all `.ry` files in the directory are loaded (directory module)
2. **File** (`{path}.ry`) — single file (backward compatible)

### Directory Modules

When a module resolves to a directory:
- All `.ry` files in the directory are automatically loaded
- Test files (`.test.ry`) are excluded
- No special entry file (like `__init__.py`) is needed
- All functions, types, and directive definitions defined in the directory's files are exported (subject to the visibility rules below)

### Visibility

Every definition (`fn`, `record`, `enum`, `type` alias, `let`, `@directive`) has one of two visibilities:

| Visibility | Marker | Visible from |
|---|---|---|
| **package-internal** (default) | none | the same package only |
| **public** | `@public` | any importer (any package) |

A **package** here means the visibility boundary — the directory tree rooted at the nearest ancestor directory containing a `package.toml` file. Two source files belong to the same package when they share the same package root. Files that have no `package.toml` ancestor (e.g. ad-hoc scripts, REPL `-c` input) share a single anonymous package. (This is distinct from the future external-library "package" reserved in the [glossary](glossary.md#package); see [Visibility scopes](glossary.md#visibility-scopes).)

The leading `_` underscore on an identifier carries **no** visibility meaning — it is just a naming convention. Visibility is controlled exclusively by `@public`.

```ry
# mylib/internal.ry
fn helper() -> int:           # package-internal — visible only from the
    return 42                 # same package as mylib/internal.ry
@public
fn publicApi() -> int:        # public — visible from any package
    return helper()
```

```text
mymodule/
  calc.ry      # @public fn add(), @public fn sub()
  string.ry    # @public fn concat()
```

```ry
from mymodule              # imports the @public symbols add, sub, concat
from mymodule import add   # imports only add (must be @public if cross-package)
```

Importing a non-`@public` symbol from another package is a compile error. Wildcard imports across packages silently filter out non-`@public` symbols. Inside the same package every definition is importable regardless of `@public`.

---

## Standard Library (`std`)

The standard library (`std`) is a collection of built-in modules automatically imported into every program. It provides:
- Built-in functions (`print`, `len`, `range`, etc.)
- String functions (`contains`, `find`, `replace`, etc.)
- Type conversion functions (`toInt`, `toFloat`, `toStr`)
- Collection functions (`map`, `filter`, `sort`, etc.)

The entire standard library forms a single package — `share/std/package.toml` is its package root. Stdlib modules can therefore share package-internal helpers across files; user code only sees the symbols marked `@public`. An import such as `from math import sqrt` resolves to a `@public` symbol exposed by the stdlib package.

### Sub-modules

The following sub-modules require explicit import:

| Module | Description |
|---------|-------------|
| [`math`](math.md) | Mathematical constants and functions |
| [`io`](io.md) | File I/O, standard input, and byte conversions |
| [`path`](path.md) | File path operations (join, basename, dirname, etc.) |

```ry
from math import sqrt, PI, sin
```

You can also explicitly import specific definitions from standard library modules:

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

When a `ry` executable is built inside the Ry source tree, it can use a repo-local stdlib override declared in the project manifest (`package.toml`). This keeps repo builds aligned with the checked-out `share/std` even if `~/.ry/share/std` is older. Installed `ry` binaries ignore that override and continue to use `$RY_HOME/share/std`.

---

## Search Path Priority

1. The directory of the importing file
2. Repo-local stdlib override from the current Ry checkout, when using a repo-built `ry`
3. `$RY_HOME/share` (standard library location, falls back to `$RY_HOME/lib` for legacy installs)
4. Executable-relative `share/` directories (falls back to `lib/` for legacy layouts)
5. Paths specified in the `RY_PATH` environment variable (colon-separated)

---

## RY_PATH Environment Variable

Specifying colon-separated directories in `RY_PATH` adds them to the module search path.

```bash
export RY_PATH="/usr/local/ry/lib:/home/user/ry-modules"
```

---

## Constraints

| Constraint | Details |
|------|------|
| Allowed location | Top level only (not inside functions or blocks) |
| Duplicate `from` imports | Automatically skipped (no error) |
| Duplicate qualified `import` | Compile error (`'import xxx' already in this file`) |
| Circular imports | Compile error |
| Relative imports | `from .` and `from .submodule` resolve only against the current file's directory |
| Parent directory imports | `from ..` is not supported |
| Module names | Only letters, digits, and underscores are allowed (no hyphens) |

```ry
# Error example: Import inside a block
fn main():
    from math   # Error: imports only allowed at top level

# OK: Repeating the same `from` import is silently skipped
from math
from math   # Skipped

# Error: the qualified form rejects duplicates at parse time
import math
import math   # Error: 'import math' already in this file
```

---

## Creating Module Files

### Single File Module

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

### Directory Module

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

Stdlib module functions that are implemented as C runtime functions follow the `__ry_<module>_<symbol>` convention.

> **Note**: This convention applies to stdlib module functions (e.g., `base64`, `filesystem`, `path`). Built-in functions (e.g., `print`, `len`) and math functions use varied implementations (inline LLVM IR, libc calls) and do not follow this naming pattern.

### Format

```text
__ry_<module>_<symbol>
```

### Rules

1. **Prefix**: `__ry_`
2. **Module**: The module name (e.g., `base64` from `from base64 import encode`)
3. **Symbol**: The C symbol name. Most modules now mirror the Ry function name verbatim (camelCase for v0.0.16+, e.g. `filesystem::listDir` → `__ry_filesystem_listDir`). Legacy modules (`base64`, `string`) still use snake_case C symbols regardless of the camelCase Ry name (e.g. `base64::encodeUrlSafe` → `__ry_base64_encode_url_safe`); this is a historical inconsistency tracked for cleanup.
4. **Overloads**: When a function has multiple overloads with different arities, append the argument count as a suffix (e.g., `__ry_path_join2`, `__ry_path_join3`)
5. **Error getter**: Each module that returns `Result` types provides `__ry_<module>_get_last_error`

### Examples

| Ry declaration | C runtime function name |
|---------------|------------------------|
| `base64::encode(data: str) -> str` | `__ry_base64_encode` |
| `base64::encodeUrlSafe(data: str) -> str` | `__ry_base64_encode_url_safe` (snake_case legacy) |
| `string::makeUninit(byteLen: int) -> str` | `__ry_string_make_uninit` (snake_case legacy) |
| `filesystem::listDir(path: str) -> Result<List<str>, Error>` | `__ry_filesystem_listDir` |
| `path::isAbsolute(p: str) -> bool` | `__ry_path_isAbsolute` |
| `gc::setThreshold(n: int) -> Unit` | `__ry_gc_setThreshold` |
| `path::join(a: str, b: str) -> str` | `__ry_path_join2` |
| `path::join(a: str, b: str, c: str) -> str` | `__ry_path_join3` |
