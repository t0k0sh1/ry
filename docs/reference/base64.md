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
| `encode_bytes` | `(List<u8>) -> str` | Encodes raw bytes to standard base64 |
| `encode_bytes_url_safe` | `(List<u8>) -> str` | Encodes raw bytes to URL-safe base64 (no padding) |
| `decode_bytes` | `(str) -> Result<List<u8>, Error>` | Decodes a standard base64 string to raw bytes |
| `decode_bytes_url_safe` | `(str) -> Result<List<u8>, Error>` | Decodes a URL-safe base64 string to raw bytes |

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

## Working with Byte Data

### Binary Data and NUL Bytes

Input strings may contain embedded NUL bytes (`\0`); `encode` and `encode_url_safe` operate on the full byte length and do not truncate at NUL. `decode` and `decode_url_safe` return `Err` if the input contains a NUL byte, since NUL is not a valid base64 character.

`encode_bytes` and `decode_bytes` operate directly on `List<u8>`, making them suitable for binary data such as images, audio, or cryptographic payloads that may contain arbitrary byte values including embedded NUL bytes.

```ry
from base64 import encode_bytes, decode_bytes
from io import read_bytes, write_bytes

# Encode raw binary file content to base64
case read_bytes("/path/to/image.jpg"):
    Ok(data):
        encoded = encode_bytes(data)
        print(encoded)
    Err(e):
        print(e.message)

# Decode base64 back to raw bytes
case decode_bytes("AP8A"):
    Ok(data):
        case write_bytes("/tmp/out.bin", data):
            Ok(_):
                print("written")
            Err(e):
                print(e.message)
    Err(e):
        print(e.message)
```

URL-safe variants are also available for byte data:

```ry
from base64 import encode_bytes_url_safe, decode_bytes_url_safe

token: List<u8> = [0xFBu8, 0xFFu8, 0x00u8, 0x01u8]
encoded = encode_bytes_url_safe(token)
# encoded contains only A-Z, a-z, 0-9, - and _ (no padding)

case decode_bytes_url_safe(encoded):
    Ok(original):
        print(original == token)  # true
    Err(e):
        print(e.message)
```

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
fn process(input: str) -> Result<str, Error>:
    decoded = decode(input)?
    return Ok(decoded)
```
