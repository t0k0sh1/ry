[English](filesystem.md) | [日本語](../ja/reference/filesystem.md) | [繁體中文](../zh/reference/filesystem.md)

# Filesystem Function Reference

File and directory manipulation. All functions require explicit import from `filesystem`.

The `filesystem` package handles operations on files and directories themselves (copy, move, remove, etc.), while the `io` package handles reading and writing file contents.

```python
from filesystem import list_dir, walk, glob_files, copy, move, remove, remove_all,
                       make_dir, make_dir_all, file_size, is_file, is_dir, is_symlink,
                       chmod, symlink, read_link
```

## Function List

| Function | Signature | Description |
|----------|-----------|-------------|
| `list_dir` | `(str) -> Result<List<str>, Error>` | Lists entries in a directory (non-recursive) |
| `walk` | `(str) -> Result<List<str>, Error>` | Recursively lists all files and directories |
| `glob_files` | `(str) -> Result<List<str>, Error>` | Finds files matching a glob pattern |
| `copy` | `(str, str) -> Result<Unit, Error>` | Copies a file |
| `move` | `(str, str) -> Result<Unit, Error>` | Moves or renames a file or directory |
| `remove` | `(str) -> Result<Unit, Error>` | Removes a file or empty directory |
| `remove_all` | `(str) -> Result<Unit, Error>` | Removes a file or directory tree recursively |
| `make_dir` | `(str) -> Result<Unit, Error>` | Creates a single directory |
| `make_dir_all` | `(str) -> Result<Unit, Error>` | Creates a directory and all missing parents |
| `file_size` | `(str) -> Result<int, Error>` | Returns file size in bytes |
| `is_file` | `(str) -> bool` | Checks if path is a regular file |
| `is_dir` | `(str) -> bool` | Checks if path is a directory |
| `is_symlink` | `(str) -> bool` | Checks if path is a symbolic link |
| `chmod` | `(str, int) -> Result<Unit, Error>` | Changes file permissions (POSIX mode) |
| `symlink` | `(str, str) -> Result<Unit, Error>` | Creates a symbolic link |
| `read_link` | `(str) -> Result<str, Error>` | Reads the target of a symbolic link |

## Examples

### Directory Operations

```python
from filesystem import make_dir, make_dir_all, list_dir, remove_all

# Create a single directory
match make_dir("/tmp/myapp"):
  case Ok(_):
    print("created")
  case Err(e):
    print("error: " + e.message)

# Create nested directories (like mkdir -p)
make_dir_all("/tmp/myapp/data/logs")

# List directory contents
match list_dir("/tmp/myapp"):
  case Ok(entries):
    for entry in entries:
      print(entry)
  case Err(e):
    print("error: " + e.message)

# Remove a directory tree (like rm -rf)
remove_all("/tmp/myapp")
```

### File Operations

```python
from filesystem import copy, move, remove, file_size
from io import write_text

write_text("/tmp/hello.txt", "Hello, World!")

# Copy a file
copy("/tmp/hello.txt", "/tmp/hello_copy.txt")

# Get file size
match file_size("/tmp/hello.txt"):
  case Ok(sz):
    print("size: " + to_string(sz))
  case Err(e):
    print("error: " + e.message)

# Move / rename a file
move("/tmp/hello_copy.txt", "/tmp/renamed.txt")

# Remove a file
remove("/tmp/renamed.txt")
```

### Recursive Traversal

```python
from filesystem import walk, glob_files

# Walk a directory tree (like find)
match walk("/var/log"):
  case Ok(files):
    for f in files:
      print(f)
  case Err(e):
    print("error: " + e.message)

# Glob pattern matching
match glob_files("/var/log/*.log"):
  case Ok(matches):
    for m in matches:
      print(m)
  case Err(e):
    print("error: " + e.message)
```

### Path Type Checks

```python
from filesystem import is_file, is_dir, is_symlink

if is_file("/etc/hosts"):
  print("regular file")

if is_dir("/tmp"):
  print("directory")

if is_symlink("/usr/local/bin/python"):
  print("symbolic link")
```

### Symbolic Links

```python
from filesystem import symlink, read_link, is_symlink

# Create a symlink
symlink("/usr/local/bin/ry", "/tmp/ry_link")

# Check and read symlink
if is_symlink("/tmp/ry_link"):
  match read_link("/tmp/ry_link"):
    case Ok(target):
      print("points to: " + target)
    case Err(e):
      print("error: " + e.message)
```

### Permissions

```python
from filesystem import chmod

# chmod 755 (rwxr-xr-x) — use decimal value: 0o755 = 493
chmod("/tmp/script.sh", 493)

# chmod 644 (rw-r--r--) — 0o644 = 420
chmod("/tmp/data.txt", 420)
```

## Notes

- `is_file`, `is_dir`, and `is_symlink` return `false` on error (e.g., path does not exist)
- `is_file` and `is_dir` follow symlinks; `is_symlink` uses `lstat` to detect links
- `list_dir` returns entry names only (not full paths)
- `walk` returns full paths for all entries (both files and directories)
- `glob_files` returns an empty list (not an error) when no files match the pattern
- `remove` fails on non-empty directories; use `remove_all` for recursive deletion
