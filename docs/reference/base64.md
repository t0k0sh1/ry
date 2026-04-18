[English](base64.md) | [日本語](../ja/reference/base64.md) | [繁體中文](../zh/reference/base64.md)

# Base64 Function Reference

Base64 encoding and decoding. All functions require explicit import from `base64`.

```ry
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

```ry
from base64 import encode, decode

encoded = encode("Hello, World!")
print(encoded)  # SGVsbG8sIFdvcmxkIQ==

case decode(encoded):
    Ok(s):
        print(s)  # Hello, World!
    Err(e):
        print(e.message)
```

### URL-safe Base64

URL-safe base64 uses `-` and `_` instead of `+` and `/`, and omits padding (`=`). Useful for URLs, filenames, and tokens.

```ry
from base64 import encode_url_safe, decode_url_safe

encoded = encode_url_safe("data with special chars: ?&=")
# No + / or = in the output

case decode_url_safe(encoded):
    Ok(s):
        print(s)
    Err(e):
        print(e.message)
```

### Binary Data and NUL Bytes

Input strings may contain embedded NUL bytes (`\0`); `encode` and `encode_url_safe` operate on the full byte length and do not truncate at NUL. `decode` and `decode_url_safe` return `Err` if the input contains a NUL byte, since NUL is not a valid base64 character.


## Error Handling

`decode` and `decode_url_safe` return `Result<str, Error>`. Decoding fails if the input contains invalid base64 characters.

```ry
case decode("!!!not-valid!!!"):
    Ok(s):
        print(s)
    Err(e):
        print(e.message)  # "invalid base64 character at position 0"
```

With the `?` operator:

```ry
function process(input: str) -> Result<str, Error>:
    decoded = decode(input)?
    return Ok(decoded)
```
