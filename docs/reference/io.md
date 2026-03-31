[English](io.md) | [日本語](../ja/reference/io.md) | [繁體中文](../zh/reference/io.md)

# I/O Function Reference

Standard I/O and file operations. All functions require explicit import from `io`.

```python
from io import read_text, write_text, exists
```

## Function List

### Standard Input

| Function | Signature | Description |
|----------|-----------|-------------|
| `read_line` | `() -> str` | Reads one line from stdin (trailing newline removed) |
| `read_all` | `() -> str` | Reads all of stdin until EOF |

### File I/O

| Function | Signature | Description |
|----------|-----------|-------------|
| `read_text` | `(str) -> Result<str, Error>` | Reads entire file as a string |
| `write_text` | `(str, str) -> Result<Unit, Error>` | Writes a string to a file (overwrites) |
| `append_text` | `(str, str) -> Result<Unit, Error>` | Appends a string to the end of a file |
| `exists` | `(str) -> bool` | Checks if a file exists |
| `delete_file` | `(str) -> Result<Unit, Error>` | Deletes a file |
| `read_bytes` | `(str) -> Result<List<u8>, Error>` | Reads a file as a byte list |
| `write_bytes` | `(str, List<u8>) -> Result<Unit, Error>` | Writes a byte list to a file |

### Byte Conversions

| Function | Signature | Description |
|----------|-----------|-------------|
| `to_bytes` | `(str) -> List<u8>` | Converts a string to UTF-8 bytes |
| `bytes_to_str` | `(List<u8>) -> Result<str, Error>` | Converts a byte list to a string |

## Examples

### Reading and Writing Files

```python
from io import read_text, write_text, append_text, exists, delete_file

match write_text("hello.txt", "Hello, World!"):
    case Ok(_):
        match read_text("hello.txt"):
            case Ok(content):
                print(content)   # Hello, World!
            case Err(e):
                print(e.message)
    case Err(e):
        print(e.message)

print(exists("hello.txt"))   # true

match delete_file("hello.txt"):
    case Ok(_):
        print(exists("hello.txt"))   # false
    case Err(e):
        print(e.message)
```

### Byte Operations

```python
from io import to_bytes, bytes_to_str, write_bytes, read_bytes

bs = to_bytes("ABC")
print(length(bs))    # 3

match write_bytes("data.bin", bs):
    case Ok(_):
        match read_bytes("data.bin"):
            case Ok(rb):
                match bytes_to_str(rb):
                    case Ok(s):
                        print(s)          # ABC
                    case Err(e):
                        print(e.message)
            case Err(e):
                print(e.message)
    case Err(e):
        print(e.message)
```

### Reading from Standard Input

```python
from io import read_line

name = read_line()
print(f"Hello, {name}!")
```

## Error Handling

File operations return `Result<T, Error>` instead of terminating on failure. Use `match` with `Ok`/`Err` patterns to handle errors:

```python
match read_text("missing.txt"):
    case Ok(content):
        print(content)
    case Err(e):
        print(e.message)   # cannot open file 'missing.txt' for reading
```

| Operation | Error Condition |
|-----------|----------------|
| `read_text` / `read_bytes` | File does not exist or cannot be opened |
| `write_text` / `write_bytes` / `append_text` | File cannot be opened for writing |
| `delete_file` | File cannot be deleted |
| `bytes_to_str` | Input contains NUL byte |

## Notes

- `List<u8>` is used as the buffer type. Standard list operations (`length()`, `append()`, `slice()`, index access) all work with byte lists.
- File paths are relative to the current working directory unless absolute paths are specified.
- `write_text` and `write_bytes` overwrite existing files. Use `append_text` to add content to existing files.
