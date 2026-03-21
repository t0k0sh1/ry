[English](io.md) | [日本語](../ja/reference/io.md) | [繁體中文](../zh/reference/io.md)

# I/O Function Reference

Standard I/O and file operations. All functions require explicit import from `std.io`.

```python
from std.io import read_text, write_text, file_exists
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
| `read_text` | `(str) -> str` | Reads entire file as a string |
| `write_text` | `(str, str) -> Unit` | Writes a string to a file (overwrites) |
| `append_text` | `(str, str) -> Unit` | Appends a string to the end of a file |
| `file_exists` | `(str) -> bool` | Checks if a file exists |
| `delete_file` | `(str) -> Unit` | Deletes a file |
| `read_bytes` | `(str) -> List<byte>` | Reads a file as a byte list |
| `write_bytes` | `(str, List<byte>) -> Unit` | Writes a byte list to a file |

### Byte Conversions

| Function | Signature | Description |
|----------|-----------|-------------|
| `str_to_bytes` | `(str) -> List<byte>` | Converts a string to UTF-8 bytes |
| `bytes_to_str` | `(List<byte>) -> str` | Converts a byte list to a string |

## Examples

### Reading and Writing Files

```python
from std.io import read_text, write_text, append_text, file_exists, delete_file

write_text("hello.txt", "Hello, World!")
let content = read_text("hello.txt")
print(content)   # Hello, World!

append_text("hello.txt", "\nGoodbye!")
print(read_text("hello.txt"))
# Hello, World!
# Goodbye!

print(file_exists("hello.txt"))   # true
delete_file("hello.txt")
print(file_exists("hello.txt"))   # false
```

### Byte Operations

```python
from std.io import str_to_bytes, bytes_to_str, write_bytes, read_bytes

let bs = str_to_bytes("ABC")
print(len(bs))    # 3

write_bytes("data.bin", bs)
let rb = read_bytes("data.bin")
let s = bytes_to_str(rb)
print(s)          # ABC
```

### Reading from Standard Input

```python
from std.io import read_line

let name = read_line()
print(f"Hello, {name}!")
```

## Error Handling

All file operations terminate with a runtime error if the operation fails:

| Operation | Error Condition |
|-----------|----------------|
| `read_text` / `read_bytes` | File does not exist or cannot be opened |
| `write_text` / `write_bytes` / `append_text` | File cannot be opened for writing |
| `delete_file` | File cannot be deleted |

Error messages are printed to stderr and the program exits with code 1.

## Notes

- `List<byte>` is used as the buffer type. Standard list operations (`len()`, `append()`, `slice()`, index access) all work with byte lists.
- File paths are relative to the current working directory unless absolute paths are specified.
- `write_text` and `write_bytes` overwrite existing files. Use `append_text` to add content to existing files.
