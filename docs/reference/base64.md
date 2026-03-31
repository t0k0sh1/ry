[English](base64.md) | [日本語](../ja/reference/base64.md) | [繁體中文](../zh/reference/base64.md)

# Base64 Function Reference

Base64 encoding and decoding. All functions require explicit import from `base64`.

```python
from base64 import encode, decode, encode_url_safe, decode_url_safe
```

## Function List

| Function | Signature | Description |
|----------|-----------|-------------|
| `encode` | `(str) -> str` | Encodes a string to standard base64 |
| `decode` | `(str) -> Result<str, Error>` | Decodes a standard base64 string |
| `encode_url_safe` | `(str) -> str` | Encodes a string to URL-safe base64 (no padding) |
| `decode_url_safe` | `(str) -> Result<str, Error>` | Decodes a URL-safe base64 string |

## Examples

### Basic Encoding and Decoding

```python
from base64 import encode, decode

encoded = encode("Hello, World!")
print(encoded)  # SGVsbG8sIFdvcmxkIQ==

when decode(encoded):
    case Ok(s):
        print(s)  # Hello, World!
    case Err(e):
        print(e.message)
```

### URL-safe Base64

URL-safe base64 uses `-` and `_` instead of `+` and `/`, and omits padding (`=`). Useful for URLs, filenames, and tokens.

```python
from base64 import encode_url_safe, decode_url_safe

encoded = encode_url_safe("data with special chars: ?&=")
# No + / or = in the output

when decode_url_safe(encoded):
    case Ok(s):
        print(s)
    case Err(e):
        print(e.message)
```

### Working with Byte Data

To encode/decode byte data, combine with `to_bytes` / `bytes_to_str` from `io`.

```python
from base64 import encode, decode
from io import to_bytes, bytes_to_str

bytes = to_bytes("binary data")
encoded = encode(bytes_to_str(bytes)?)
```

## Error Handling

`decode` and `decode_url_safe` return `Result<str, Error>`. Decoding fails if the input contains invalid base64 characters.

```python
when decode("!!!not-valid!!!"):
    case Ok(s):
        print(s)
    case Err(e):
        print(e.message)  # "invalid base64 character at position 0"
```

With the `?` operator:

```python
function process(input: str) -> Result<str, Error>:
    decoded = decode(input)?
    return Ok(decoded)
```
