# I/O Function Reference

Standard I/O and file operations. All functions require explicit import from `io`.

```ry
from ry.io import readText, writeText, exists
```

## Function List

### Standard Input

| Function | Signature | Description |
|----------|-----------|-------------|
| `readLine` | `() -> Result<Option<str>, Error>` | Reads one line from stdin; `Ok(Some(line))` on success (trailing newline removed), `Ok(None)` at EOF, `Err(e)` on I/O error |
| `readAll` | `() -> str` | Reads all of stdin until EOF |

### File I/O

| Function | Signature | Description |
|----------|-----------|-------------|
| `readText` | `(str) -> Result<str, Error>` | Reads entire file as a string |
| `writeText` | `(str, str) -> Result<Unit, Error>` | Writes a string to a file (overwrites) |
| `appendText` | `(str, str) -> Result<Unit, Error>` | Appends a string to the end of a file |
| `exists` | `(str) -> bool` | Checks if a file exists |
| `deleteFile` | `(str) -> Result<Unit, Error>` | Deletes a file |
| `readBytes` | `(str) -> Result<List<u8>, Error>` | Reads a file as a byte list |
| `writeBytes` | `(str, List<u8>) -> Result<Unit, Error>` | Writes a byte list to a file |

### File Handle API

| Function | Signature | Description |
|----------|-----------|-------------|
| `open` | `(str, str) -> Result<File, Error>` | Opens a file; mode must be `"r"`, `"w"`, `"a"`, `"rb"`, `"wb"`, or `"ab"` |
| `readAll` | `(File) -> Result<str, Error>` | Reads the entire file content into a string |
| `readLine` | `(File) -> Result<Option<str>, Error>` | Reads one line; returns `Ok(None)` at EOF |
| `writeText` | `(File, str) -> Result<Unit, Error>` | Writes a string to the file |
| `close` | `(File) -> Unit` | Closes the file handle (idempotent) |
| `lines` | `(File) -> Iterator<str>` | Returns a lazy line iterator usable with `for ... in` |

`File` is an opaque resource handle managed by ARC. The file is closed automatically when the handle goes out of scope; calling `close` explicitly allows earlier release.

> **Note**: `readAll` / `readLine` / `writeText` without a `File` first argument route to the path-based or stdin variants above. The compiler dispatches on the argument type at compile time.
>
> **Scope-based release**: A `File` can be bound with the `using` statement to have `close` called automatically on every exit path of a block (`return`, `?`, `break`, `continue`, or normal block end). See [`control-flow.md` § using](control-flow.md#using).
>
> **`lines()` iterator**: `for line in lines(f) { ... }` yields one line at a time without loading the file into memory — suitable for large logs. The iterator retains the underlying `File` for its lifetime and shares the file position with subsequent `readLine` / `lines` calls. After `close(f)`, iteration terminates at the next step (no error is raised, mirroring Python).

### Byte Conversions

| Function | Signature | Description |
|----------|-----------|-------------|
| `toBytes` | `(str) -> List<u8>` | Converts a string to UTF-8 bytes |
| `bytesToStr` | `(List<u8>) -> Result<str, Error>` | Converts a byte list to a string |

## Examples

### Reading and Writing Files

```ry
from ry.io import readText, writeText, appendText, exists, deleteFile

case writeText("hello.txt", "Hello, World!"):
    Ok(_):
        case readText("hello.txt"):
            Ok(content):
                print(content)   # Hello, World!
            Err(e):
                print(e.message)
    Err(e):
        print(e.message)

print(exists("hello.txt"))   # true

case deleteFile("hello.txt"):
    Ok(_):
        print(exists("hello.txt"))   # false
    Err(e):
        print(e.message)
```

### File Handle API

```ry
from ry.io import open, readAll, readLine, writeText, close, deleteFile, lines

# Write then read back via handle
case open("/tmp/hello.txt", "w"):
    Ok(fw):
        writeText(fw, "hello handle")
        close(fw)
    Err(e):
        print(e.message)

case open("/tmp/hello.txt", "r"):
    Ok(fr):
        case readAll(fr):
            Ok(s):
                print(s)   # hello handle
            Err(e):
                print(e.message)
        close(fr)
    Err(e):
        print(e.message)

# Read lines one by one
case open("/tmp/hello.txt", "r"):
    Ok(f):
        loop:
            case readLine(f):
                Ok(opt):
                    case opt:
                        Some(line): print(line)
                        None: break
                Err(e):
                    print(e.message)
                    break
        close(f)
    Err(e):
        print(e.message)

# Lazy line iteration with lines() — suitable for large files
case open("/tmp/hello.txt", "r"):
    Ok(f):
        using fh = f:
            for line in lines(fh):
                print(line)
    Err(e):
        print(e.message)

deleteFile("/tmp/hello.txt")
```

### Byte Operations

```ry
from ry.io import toBytes, bytesToStr, writeBytes, readBytes

bs = toBytes("ABC")
print(len(bs))    # 3

case writeBytes("data.bin", bs):
    Ok(_):
        case readBytes("data.bin"):
            Ok(rb):
                case bytesToStr(rb):
                    Ok(s):
                        print(s)          # ABC
                    Err(e):
                        print(e.message)
            Err(e):
                print(e.message)
    Err(e):
        print(e.message)
```

### Reading from Standard Input

```ry
from ry.io import readLine

case readLine():
    Ok(opt):
        case opt:
            Some(name):
                print(f"Hello, {name}!")
            None:
                print("(no input)")
    Err(e):
        print(e.message)
```

`readLine()` returns `Ok(Some(line))` for a successful read (trailing newline removed), `Ok(None)` at EOF (e.g. when stdin is closed), and `Err(e)` on I/O failure. The `input()` builtin returns the same `Result<Option<str>, Error>` shape with the same semantics and is available without an `import`.

## Error Handling

File operations return `Result<T, Error>` instead of terminating on failure. Use `case` with `Ok`/`Err` patterns to handle errors. The `Error.message` field carries a runtime-supplied detail string describing the failure:

```ry
case readText("missing.txt"):
    Ok(content):
        print(content)
    Err(e):
        print(e.message)   # cannot open file 'missing.txt' for reading

case open("missing.txt", "r"):
    Ok(f):
        close(f)
    Err(e):
        print(e.message)   # open: cannot open 'missing.txt' in mode 'r'
```

| Operation | Error Condition |
|-----------|----------------|
| `readText` / `readBytes` | File does not exist or cannot be opened |
| `writeText` / `writeBytes` / `appendText` | File cannot be opened for writing |
| `deleteFile` | File cannot be deleted |
| `readText` / `writeText` / `appendText` / `deleteFile` / `readBytes` / `writeBytes` | Path contains an embedded NUL byte |
| `open` | File does not exist (mode `"r"` / `"rb"`), cannot be created/opened (mode `"w"` / `"a"` / `"wb"` / `"ab"`), or mode is not `"r"` / `"w"` / `"a"` / `"rb"` / `"wb"` / `"ab"` |
| `readAll` (File) | Read error after opening |
| `readLine` (stdin) / `readLine` (File) | Read error (I/O failure, not EOF — EOF is `Ok(None)`) |
| `writeText` (File, str) | Write error |
| `open` / `readText` / `writeText` / `appendText` / `readBytes` / `writeBytes` | Path refers to a symbolic link (rejected by `O_NOFOLLOW`; security hardening) |
| `readText` / `readBytes` / `readAll` (File) | File exceeds the 256 MiB read limit |

## Notes

- `List<u8>` is used as the buffer type. Standard list operations (`len()`, `append()`, `slice()`, index access) all work with byte lists.
- `bytesToStr()` and `writeBytes()` require a `List<u8>` argument. Four ways to produce a compatible byte list: (1) explicit `u8` suffixes (`[97u8, 0u8, 98u8]`), (2) `toBytes("...")` to convert a string literal, (3) a type-annotated variable declaration (`bs: List<u8> = [97, 0, 98]`), or (4) reassignment to a `List<u8>` variable (`bs = [99, 100, 101]`). Plain integer list literals without annotation, explicit suffix, or a typed variable target use 64-bit element layout and are rejected at compile time.
- File paths are relative to the current working directory unless absolute paths are specified.
- `exists` returns `false` for paths containing an embedded NUL byte (such paths cannot refer to a real file under POSIX).
- `writeText` and `writeBytes` overwrite existing files. Use `appendText` to add content to existing files.
- `writeText`, `appendText`, and `readText` are binary-transparent: content may contain embedded NUL bytes and the full byte sequence is preserved (#1133).
- Symbolic links are rejected by all file-opening operations (`open`, `readText`, `writeText`, `appendText`, `readBytes`, `writeBytes`) via `O_NOFOLLOW` as a security hardening measure. Opening a symlinked path fails with the standard "cannot open file ..." error; the message does not currently distinguish symlinks from other open failures (e.g. missing file, permission denied). (#1849)
- `readText`, `readBytes`, and `readAll(File)` reject files larger than 256 MiB and return `Err`. The error message includes the actual size, e.g. `file 'big.bin' is too large (300000000 bytes, max 268435456)` for the path-based variants, or `readAll: file too large (300000000 bytes, max 268435456)` for the File-handle variant. `writeText`, `writeBytes`, and `appendText` have no equivalent upper bound. (#1849)
