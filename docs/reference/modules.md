[English](modules.md) | [日本語](../ja/reference/modules.md) | [繁體中文](../zh/reference/modules.md)

# Module Reference

## Overview

Ry manages modules on a per-file basis. Use the `from` statement to import modules.

---

## Import Syntax

### Import All Functions

```python
from math
```

Imports all functions from the module.

### Selective Import

```python
from math import add
```

Imports only the specified function.

### Multiple Selective Import

```python
from math import add, sub
```

Imports multiple functions separated by commas.

---

## Subdirectory Modules

Subdirectories are specified using dot-separated notation.

| Import Statement | Corresponding File Path |
|-------------|-------------------|
| `from math` | `math.ry` |
| `from utils.math` | `utils/math.ry` |
| `from a.b.c` | `a/b/c.ry` |

```python
from utils.math import add
from net.http import get
```

The file extension (`.ry`) is not included in the module name.

---

## Search Path Priority

1. The directory of the importing file
2. Paths specified in the `RY_PATH` environment variable (colon-separated)

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
| Duplicate imports | Automatically skipped (no error) |
| Circular imports | Compile error |

```python
# Error example: Import inside a block
fn main():
    from math   # Error: imports only allowed at top level

# OK: Importing the same module multiple times does not cause an error
from math
from math   # Skipped
```

---

## Creating Module Files

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
