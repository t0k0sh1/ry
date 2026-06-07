# Filesystem Function Reference

File and directory manipulation. All functions require explicit import from `filesystem`.

The `filesystem` module handles operations on files and directories themselves (copy, move, remove, etc.), while the `io` module handles reading and writing file contents.

```ry
from filesystem import listDir, walk, glob, copy, move, remove, removeAll
from filesystem import mkdir, mkdirAll, fileSize, isFile, isDir, isSymlink
from filesystem import chmod, symlink, readLink
```

## Function List

| Function | Signature | Description |
|----------|-----------|-------------|
| `listDir` | `(str) -> Result<List<str>, Error>` | Lists entries in a directory (non-recursive) |
| `walk` | `(str) -> Result<List<str>, Error>` | Recursively lists all files and directories |
| `glob` | `(str) -> Result<List<str>, Error>` | Finds files matching a glob pattern |
| `copy` | `(str, str) -> Result<Unit, Error>` | Copies a file |
| `move` | `(str, str) -> Result<Unit, Error>` | Moves or renames a file or directory |
| `remove` | `(str) -> Result<Unit, Error>` | Removes a file or empty directory |
| `removeAll` | `(str) -> Result<Unit, Error>` | Removes a file or directory tree recursively |
| `mkdir` | `(str) -> Result<Unit, Error>` | Creates a single directory |
| `mkdirAll` | `(str) -> Result<Unit, Error>` | Creates a directory and all missing parents |
| `fileSize` | `(str) -> Result<int, Error>` | Returns file size in bytes |
| `isFile` | `(str) -> Result<bool, Error>` | Checks if path is a regular file. Returns `Err` if path contains an embedded NUL byte. |
| `isDir` | `(str) -> Result<bool, Error>` | Checks if path is a directory. Returns `Err` if path contains an embedded NUL byte. |
| `isSymlink` | `(str) -> Result<bool, Error>` | Checks if path is a symbolic link. Returns `Err` if path contains an embedded NUL byte. |
| `chmod` | `(str, int) -> Result<Unit, Error>` | Changes file permissions (POSIX mode) |
| `symlink` | `(str, str) -> Result<Unit, Error>` | Creates a symbolic link |
| `readLink` | `(str) -> Result<str, Error>` | Reads the target of a symbolic link |

## Examples

### Directory Operations

```ry
from filesystem import mkdir, mkdirAll, listDir, removeAll

# Create a single directory
case mkdir("/tmp/myapp"):
  Ok(_):
    print("created")
  Err(e):
    print("error: " + e.message)

# Create nested directories (like mkdir -p)
mkdirAll("/tmp/myapp/data/logs")

# List directory contents
case listDir("/tmp/myapp"):
  Ok(entries):
    for entry in entries:
      print(entry)
  Err(e):
    print("error: " + e.message)

# Remove a directory tree (like rm -rf)
removeAll("/tmp/myapp")
```

### File Operations

```ry
from filesystem import copy, move, remove, fileSize
from io import writeText

writeText("/tmp/hello.txt", "Hello, World!")

# Copy a file
copy("/tmp/hello.txt", "/tmp/hello_copy.txt")

# Get file size
case fileSize("/tmp/hello.txt"):
  Ok(sz):
    print("size: " + str(sz))
  Err(e):
    print("error: " + e.message)

# Move / rename a file
move("/tmp/hello_copy.txt", "/tmp/renamed.txt")

# Remove a file
remove("/tmp/renamed.txt")
```

### Recursive Traversal

```ry
from filesystem import walk, glob

# Walk a directory tree (like find)
case walk("/var/log"):
  Ok(files):
    for f in files:
      print(f)
  Err(e):
    print("error: " + e.message)

# Glob pattern matching
case glob("/var/log/*.log"):
  Ok(matches):
    for m in matches:
      print(m)
  Err(e):
    print("error: " + e.message)
```

### Path Type Checks

```ry
from filesystem import isFile, isDir, isSymlink

case isFile("/etc/hosts"):
  Ok(true): print("regular file")
  Ok(false): print("not a regular file")
  Err(e): print("error: " + e.message)

case isDir("/tmp"):
  Ok(true): print("directory")
  _: ...

case isSymlink("/usr/local/bin/python"):
  Ok(true): print("symbolic link")
  _: ...
```

### Symbolic Links

```ry
from filesystem import symlink, readLink, isSymlink

# Create a symlink
symlink("/usr/local/bin/ry", "/tmp/ry_link")

# Check and read symlink
case isSymlink("/tmp/ry_link"):
  Ok(true):
    case readLink("/tmp/ry_link"):
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

- `isFile`, `isDir`, and `isSymlink` return `Ok(false)` when the path does not exist; `Err` when the path contains an embedded NUL byte
- `isFile` and `isDir` follow symlinks; `isSymlink` uses `lstat` to detect links
- `listDir` returns entry names only (not full paths)
- `walk` returns full paths for all entries (both files and directories)
- `glob` returns an empty list (not an error) when no files match the pattern
- `remove` fails on non-empty directories; use `removeAll` for recursive deletion
