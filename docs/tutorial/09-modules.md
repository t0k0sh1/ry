[English](09-modules.md) | [日本語](../ja/tutorial/09-modules.md) | [繁體中文](../zh/tutorial/09-modules.md)

# Packages

[<- Prev: Advanced Features](08-advanced.md) | [Next: Design by Contract ->](10-contracts.md)

Ry uses a package system to organize code across files and directories. For the full specification, see [Package Reference](../reference/packages.md).

---

## from/import Syntax

Use the `from` syntax to import functions from another file.

```python
from math import add, sub   # Selective import
from math                    # Full import (all definitions)
```

This makes functions defined in `math.ry` available for use.

---

## Subdirectories (Dot-Separated Paths)

Use dot-separated paths to specify packages in subdirectories.

```python
from utils.math import add   # Import from utils/math.ry
```

Each dot corresponds to a directory separator.

---

## Directory Packages

A package can be either a single `.ry` file or a directory containing multiple `.ry` files. When a package resolves to a directory, all `.ry` files in it are automatically loaded.

```
mypackage/
  math.ry      # fn add(), fn sub()
  string.ry    # fn concat()
```

```python
from mypackage              # imports add, sub, concat
from mypackage import add   # imports only add
```

No special entry file (like `__init__.py`) is needed. Files starting with `_` are excluded.

---

## Standard Library (`std`)

The `std` package is automatically imported into every program. You don't need to write `from std` — it's always available.

```python
# These functions are available without any import
print("hello")
n = len("world")
xs = range(5)
```

You can also explicitly import specific definitions from `std` sub-packages:

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

Package files are searched in the following order:

1. **Directory of the importing file** — The directory containing the file with the import statement is searched first.
2. **`$RY_HOME/lib`** — Standard library location.
3. **Executable-relative `lib/`** — Directories relative to the `ry` executable.
4. **`RY_PATH` environment variable** — If not found, directories specified in `RY_PATH` are searched in order.

---

## RY_PATH Environment Variable

Multiple directories can be specified separated by colons.

```bash
export RY_PATH=/home/user/ry-libs:/usr/local/ry-libs
```

Once set, packages in the specified directories can be imported from anywhere.

---

## Limitations

- `from` statements can only be written at the **top level** of a file. They cannot be placed inside functions or blocks.
- Importing the same package multiple times is automatically skipped (no duplicate imports).
- **Circular imports** (A imports B and B imports A) result in an error.

```python
# Error example: a.ry and b.ry import each other
# a.ry: from b import foo
# b.ry: from a import bar  <- circular import error
```

---

[<- Prev: Advanced Features](08-advanced.md) | [Next: Design by Contract ->](10-contracts.md)
