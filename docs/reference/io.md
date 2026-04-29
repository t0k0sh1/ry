# I/O Function Reference

Standard I/O and file operations. All functions require explicit import from `io`.

```ry
from io import readText, writeText, exists
```

## Function List

### Standard Input

| Function | Signature | Description |
|----------|-----------|-------------|
| `readLine` | `() -> str` | Reads one line from stdin (trailing newline removed) |
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

### Byte Conversions

| Function | Signature | Description |
|----------|-----------|-------------|
| `toBytes` | `(str) -> List<u8>` | Converts a string to UTF-8 bytes |
| `bytesToStr` | `(List<u8>) -> Result<str, Error>` | Converts a byte list to a string |

## Examples

### Reading and Writing Files

```ry
from io import readText, writeText, appendText, exists, deleteFile

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

### Byte Operations

```ry
from io import toBytes, bytesToStr, writeBytes, readBytes

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
from io import readLine

name = readLine()
print(f"Hello, {name}!")
```

## Error Handling

File operations return `Result<T, Error>` instead of terminating on failure. Use `case` with `Ok`/`Err` patterns to handle errors:

```ry
case readText("missing.txt"):
    Ok(content):
        print(content)
    Err(e):
        print(e.message)   # cannot open file 'missing.txt' for reading
```

| Operation | Error Condition |
|-----------|----------------|
| `readText` / `readBytes` | File does not exist or cannot be opened |
| `writeText` / `writeBytes` / `appendText` | File cannot be opened for writing |
| `deleteFile` | File cannot be deleted |
| `readText` / `writeText` / `appendText` / `deleteFile` / `readBytes` / `writeBytes` | Path contains an embedded NUL byte |

## Notes

- `List<u8>` is used as the buffer type. Standard list operations (`len()`, `append()`, `slice()`, index access) all work with byte lists.
- `bytesToStr()` and `writeBytes()` require a `List<u8>` argument. Four ways to produce a compatible byte list: (1) explicit `u8` suffixes (`[97u8, 0u8, 98u8]`), (2) `toBytes("...")` to convert a string literal, (3) a type-annotated variable declaration (`bs: List<u8> = [97, 0, 98]`), or (4) reassignment to a `List<u8>` variable (`bs = [99, 100, 101]`). Plain integer list literals without annotation, explicit suffix, or a typed variable target use 64-bit element layout and are rejected at compile time.
- File paths are relative to the current working directory unless absolute paths are specified.
- `exists` returns `false` for paths containing an embedded NUL byte (such paths cannot refer to a real file under POSIX).
- `writeText` and `writeBytes` overwrite existing files. Use `appendText` to add content to existing files.
- `writeText`, `appendText`, and `readText` are binary-transparent: content may contain embedded NUL bytes and the full byte sequence is preserved (#1133).
