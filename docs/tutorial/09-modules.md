[English](09-modules.md) | [日本語](../ja/tutorial/09-modules.md) | [繁體中文](../zh/tutorial/09-modules.md)

# Modules

[<- Prev: Advanced Features](08-advanced.md)

Modules allow you to split and manage your code across multiple files.

---

## from/import Syntax

Use the `from` syntax to import functions from another file.

```python
from math import add, sub
```

This makes `add` and `sub` defined in `math.ry` available for use.

---

## Selective Import vs Full Import

### Selective Import

Explicitly import only the functions you need.

```python
from math import add, sub
```

### Full Import

Specifying only the module name imports all functions from that module.

```python
from math
```

---

## Subdirectories (Dot-Separated Paths)

Use dot-separated paths to specify modules in subdirectories.

```python
from utils.math import add   # Import from utils/math.ry
```

Each dot corresponds to a directory separator.

---

## Search Path Priority

Module files are searched in the following order:

1. **Directory of the importing file** -- The directory containing the file with the import statement is searched first.
2. **`RY_PATH` environment variable** -- If not found, directories specified in `RY_PATH` are searched in order.

---

## RY_PATH Environment Variable

Multiple directories can be specified separated by colons.

```python
# Example of setting in the shell
export RY_PATH=/home/user/ry-libs:/usr/local/ry-libs
```

Once set, modules in the specified directories can be imported from anywhere.

---

## Limitations

- `from` statements can only be written at the **top level** of a file. They cannot be placed inside functions or blocks.
- Importing the same module multiple times is automatically skipped (no duplicate imports).
- **Circular imports** (A imports B and B imports A) result in an error.

```python
# Error example: a.ry and b.ry import each other
# a.ry: from b import foo
# b.ry: from a import bar  <- circular import error
```

---

[<- Prev: Advanced Features](08-advanced.md)
