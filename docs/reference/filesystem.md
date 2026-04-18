[English](filesystem.md) | [日本語](../ja/reference/filesystem.md) | [繁體中文](../zh/reference/filesystem.md)

# Filesystem Function Reference

File and directory manipulation. All functions require explicit import from `filesystem`.

The `filesystem` package handles operations on files and directories themselves (copy, move, remove, etc.), while the `io` package handles reading and writing file contents.

```ry
from filesystem import list_dir, walk, glob_files, copy, move, remove, remove_all
from filesystem import make_dir, make_dir_all, file_size, is_file, is_dir, is_symlink
from filesystem import chmod, symlink, read_link
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
| `is_file` | `(str) -> Result<bool, Error>` | Checks if path is a regular file. Returns `Err` if path contains an embedded NUL byte. |
| `is_dir` | `(str) -> Result<bool, Error>` | Checks if path is a directory. Returns `Err` if path contains an embedded NUL byte. |
| `is_symlink` | `(str) -> Result<bool, Error>` | Checks if path is a symbolic link. Returns `Err` if path contains an embedded NUL byte. |
| `chmod` | `(str, int) -> Result<Unit, Error>` | Changes file permissions (POSIX mode) |
| `symlink` | `(str, str) -> Result<Unit, Error>` | Creates a symbolic link |
| `read_link` | `(str) -> Result<str, Error>` | Reads the target of a symbolic link |

## Examples

### Directory Operations

```ry
from filesystem import make_dir, make_dir_all, list_dir, remove_all

# Create a single directory
case make_dir("/tmp/myapp"):
  Ok(_):
    print("created")
  Err(e):
    print("error: " + e.message)

# Create nested directories (like mkdir -p)
make_dir_all("/tmp/myapp/data/logs")

# List directory contents
case list_dir("/tmp/myapp"):
  Ok(entries):
    for entry in entries:
      print(entry)
  Err(e):
    print("error: " + e.message)

# Remove a directory tree (like rm -rf)
remove_all("/tmp/myapp")
```

### File Operations

```ry
from filesystem import copy, move, remove, file_size
from io import write_text

write_text("/tmp/hello.txt", "Hello, World!")

# Copy a file
copy("/tmp/hello.txt", "/tmp/hello_copy.txt")

# Get file size
case file_size("/tmp/hello.txt"):
  Ok(sz):
    print("size: " + to_str(sz))
  Err(e):
    print("error: " + e.message)

# Move / rename a file
move("/tmp/hello_copy.txt", "/tmp/renamed.txt")

# Remove a file
remove("/tmp/renamed.txt")
```

### Recursive Traversal

```ry
from filesystem import walk, glob_files

# Walk a directory tree (like find)
case walk("/var/log"):
  Ok(files):
    for f in files:
      print(f)
  Err(e):
    print("error: " + e.message)

# Glob pattern matching
case glob_files("/var/log/*.log"):
  Ok(matches):
    for m in matches:
      print(m)
  Err(e):
    print("error: " + e.message)
```

### Path Type Checks

```ry
from filesystem import is_file, is_dir, is_symlink

case is_file("/etc/hosts"):
  Ok(true): print("regular file")
  Ok(false): print("not a regular file")
  Err(e): print("error: " + e.message)

case is_dir("/tmp"):
  Ok(true): print("directory")
  _: ...

case is_symlink("/usr/local/bin/python"):
  Ok(true): print("symbolic link")
  _: ...
```

### Symbolic Links

```ry
from filesystem import symlink, read_link, is_symlink

# Create a symlink
symlink("/usr/local/bin/ry", "/tmp/ry_link")

# Check and read symlink
case is_symlink("/tmp/ry_link"):
  Ok(true):
    case read_link("/tmp/ry_link"):
      Ok(target):
        print("points to: " + target)
      Err(e):
        print("error: " + e.message)
  _: ...
```

### Permissions

```ry
from filesystem import chmod

# chmod 755 (rwxr-xr-x) — use decimal value: 0o755 = 493
chmod("/tmp/script.sh", 493)

# chmod 644 (rw-r--r--) — 0o644 = 420
chmod("/tmp/data.txt", 420)
```

## Notes

- `is_file`, `is_dir`, and `is_symlink` return `Ok(false)` when the path does not exist; `Err` when the path contains an embedded NUL byte
- `is_file` and `is_dir` follow symlinks; `is_symlink` uses `lstat` to detect links
- `list_dir` returns entry names only (not full paths)
- `walk` returns full paths for all entries (both files and directories)
- `glob_files` returns an empty list (not an error) when no files match the pattern
- `remove` fails on non-empty directories; use `remove_all` for recursive deletion
