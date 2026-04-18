[English](path.md) | [日本語](../ja/reference/path.md) | [繁體中文](../zh/reference/path.md)

# Path Function Reference

File path operations. All functions require explicit import from `path`.

```ry
from path import join, basename, dirname, extension, resolve, is_absolute
```

## Function List

| Function | Signature | Description |
|----------|-----------|-------------|
| `join` | `(str, str) -> Result<str, Error>` | Joins two path segments |
| `join` | `(str, str, str) -> Result<str, Error>` | Joins three path segments |
| `join` | `(str, str, str, str) -> Result<str, Error>` | Joins four path segments |
| `basename` | `(str) -> Result<str, Error>` | Extracts the filename component |
| `dirname` | `(str) -> Result<str, Error>` | Extracts the directory component |
| `extension` | `(str) -> Result<str, Error>` | Extracts the file extension (including dot) |
| `resolve` | `(str) -> Result<str, Error>` | Resolves a path to its absolute canonical form |
| `is_absolute` | `(str) -> bool` | Returns whether a path is absolute |

`join`, `basename`, `dirname`, `extension`, and `resolve` return `Err` when any path argument contains an embedded NUL byte. `is_absolute` is never an error — it only reads the first byte of the path.

## Examples

### Joining Paths

```ry
from path import join

case join("/tmp", "data", "file.txt"):
  Ok(p): print(p)   # /tmp/data/file.txt
  Err(e): print(e.message)

# Absolute second argument replaces the first
case join("/tmp", "/usr"):
  Ok(p): print(p)   # /usr
  Err(e): print(e.message)
```

### Extracting Path Components

```ry
from path import basename, dirname, extension

p = "/home/user/docs/report.pdf"

case basename(p):
  Ok(b): print(b)   # report.pdf
  Err(e): print(e.message)

case dirname(p):
  Ok(d): print(d)   # /home/user/docs
  Err(e): print(e.message)

case extension(p):
  Ok(ext): print(ext)   # .pdf
  Err(e): print(e.message)
```

### Extension Edge Cases

```ry
from path import extension

print(extension("archive.tar.gz")?)   # .gz
print(extension(".gitignore")?)       # (empty string — hidden file with no extension)
print(extension(".config.json")?)     # .json
print(extension("Makefile")?)         # (empty string)
```

### Checking Absolute Paths

```ry
from path import is_absolute

print(is_absolute("/usr/local"))  # true
print(is_absolute("src/main.ry")) # false
```

### Resolving Paths

```ry
from path import resolve

case resolve("/tmp"):
  Ok(p):
    print(p)  # /private/tmp (on macOS) or /tmp
  Err(e):
    print(e.message)

case resolve("/nonexistent"):
  Ok(p):
    print(p)
  Err(e):
    print(e.message)  # cannot resolve path '/nonexistent': No such file or directory
```

> **Note:** `resolve("")` returns `Err` with message `"cannot resolve path: empty path"`.
