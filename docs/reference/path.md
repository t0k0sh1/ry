[English](path.md) | [日本語](../ja/reference/path.md) | [繁體中文](../zh/reference/path.md)

# Path Function Reference

File path operations. All functions require explicit import from `path`.

```python
from path import join, basename, dirname, extension, resolve, is_absolute
```

## Function List

| Function | Signature | Description |
|----------|-----------|-------------|
| `join` | `(str, str) -> str` | Joins two path segments |
| `join` | `(str, str, str) -> str` | Joins three path segments |
| `join` | `(str, str, str, str) -> str` | Joins four path segments |
| `basename` | `(str) -> str` | Extracts the filename component |
| `dirname` | `(str) -> str` | Extracts the directory component |
| `extension` | `(str) -> str` | Extracts the file extension (including dot) |
| `resolve` | `(str) -> Result<str, Error>` | Resolves a path to its absolute canonical form |
| `is_absolute` | `(str) -> bool` | Returns whether a path is absolute |

## Examples

### Joining Paths

```python
from path import join

p = join("/tmp", "data", "file.txt")
print(p)  # /tmp/data/file.txt

# Absolute second argument replaces the first
print(join("/tmp", "/usr"))  # /usr
```

### Extracting Path Components

```python
from path import basename, dirname, extension

p = "/home/user/docs/report.pdf"

print(basename(p))    # report.pdf
print(dirname(p))     # /home/user/docs
print(extension(p))   # .pdf
```

### Extension Edge Cases

```python
from path import extension

print(extension("archive.tar.gz"))  # .gz
print(extension(".gitignore"))      # (empty string — hidden file with no extension)
print(extension(".config.json"))    # .json
print(extension("Makefile"))        # (empty string)
```

### Checking Absolute Paths

```python
from path import is_absolute

print(is_absolute("/usr/local"))  # true
print(is_absolute("src/main.ry")) # false
```

### Resolving Paths

```python
from path import resolve

when resolve("/tmp"):
  case Ok(p):
    print(p)  # /private/tmp (on macOS) or /tmp
  case Err(e):
    print(e.message)

when resolve("/nonexistent"):
  case Ok(p):
    print(p)
  case Err(e):
    print(e.message)  # cannot resolve path '/nonexistent': No such file or directory
```
