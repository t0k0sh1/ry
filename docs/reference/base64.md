# Base64 Function Reference

Base64 encoding and decoding. All functions require explicit import from `base64`.

```ry
from ry.base64 import encode, decode, encodeUrlSafe, decodeUrlSafe
```

## Function List

| Function | Signature | Description |
|----------|-----------|-------------|
| `encode` | `(str) -> str` | Encodes a string to standard base64 |
| `decode` | `(str) -> Result<str, Error>` | Decodes a standard base64 string |
| `encodeUrlSafe` | `(str) -> str` | Encodes a string to URL-safe base64 (no padding) |
| `decodeUrlSafe` | `(str) -> Result<str, Error>` | Decodes a URL-safe base64 string |
| `encodeBytes` | `(List<u8>) -> str` | Encodes raw bytes to standard base64 |
| `encodeBytesUrlSafe` | `(List<u8>) -> str` | Encodes raw bytes to URL-safe base64 (no padding) |
| `decodeBytes` | `(str) -> Result<List<u8>, Error>` | Decodes a standard base64 string to raw bytes |
| `decodeBytesUrlSafe` | `(str) -> Result<List<u8>, Error>` | Decodes a URL-safe base64 string to raw bytes |

## Examples

### Basic Encoding and Decoding

```ry
from ry.base64 import encode, decode

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
from ry.base64 import encodeUrlSafe, decodeUrlSafe

encoded = encodeUrlSafe("data with special chars: ?&=")
# No + / or = in the output

case decodeUrlSafe(encoded):
    Ok(s):
        print(s)
    Err(e):
        print(e.message)
```

## Working with Byte Data

### Binary Data and NUL Bytes

Input strings may contain embedded NUL bytes (`\0`); `encode` and `encodeUrlSafe` operate on the full byte length and do not truncate at NUL. `decode` and `decodeUrlSafe` return `Err` if the input contains a NUL byte, since NUL is not a valid base64 character.

`encodeBytes` and `decodeBytes` operate directly on `List<u8>`, making them suitable for binary data such as images, audio, or cryptographic payloads that may contain arbitrary byte values including embedded NUL bytes.

```ry
from ry.base64 import encodeBytes, decodeBytes
from ry.io import readBytes, writeBytes

# Encode raw binary file content to base64
case readBytes("/path/to/image.jpg"):
    Ok(data):
        encoded = encodeBytes(data)
        print(encoded)
    Err(e):
        print(e.message)

# Decode base64 back to raw bytes
case decodeBytes("AP8A"):
    Ok(data):
        case writeBytes("/tmp/out.bin", data):
            Ok(_):
                print("written")
            Err(e):
                print(e.message)
    Err(e):
        print(e.message)
```

URL-safe variants are also available for byte data:

```ry
from ry.base64 import encodeBytesUrlSafe, decodeBytesUrlSafe

token: List<u8> = [0xFBu8, 0xFFu8, 0x00u8, 0x01u8]
encoded = encodeBytesUrlSafe(token)
# encoded contains only A-Z, a-z, 0-9, - and _ (no padding)

case decodeBytesUrlSafe(encoded):
    Ok(original):
        print(original == token)  # true
    Err(e):
        print(e.message)
```

## Error Handling

All four decode functions (`decode`, `decodeUrlSafe`, `decodeBytes`, `decodeBytesUrlSafe`) return a `Result` and fail on the following malformed inputs:

| Failure | Example | Error message contains |
|---------|---------|------------------------|
| Invalid character (outside the alphabet, including embedded NUL) | `"!!!not-valid!!!"` | `"invalid base64 character at position N"` |
| Truncated final group (only 1 character remains after stripping padding) | `"T"` | `"invalid base64: truncated input"` |
| Excess padding (more than 2 trailing `=`) | `"===="`, `"TWFu==="` | `"invalid base64: excess padding"` |
| Length not a multiple of 4 when padding is present | `"TWFu="`, `"TWFu=="`, `"="`, `"=="` | `"invalid base64: input length must be a multiple of 4 when padding is present"` |

Inputs without any `=` padding are not subject to the multiple-of-4 length check; this preserves the canonical no-padding form produced by `encodeUrlSafe` / `encodeBytesUrlSafe`. The padding validation is shared by the standard and URL-safe variants — `decodeUrlSafe("====")` and `decodeUrlSafe("TWFu=")` therefore also return `Err`.

```ry
case decode("!!!not-valid!!!"):
    Ok(s):
        print(s)
    Err(e):
        print(e.message)  # "invalid base64 character at position 0"

case decode("===="):
    Ok(s):
        print(s)
    Err(e):
        print(e.message)  # "invalid base64: excess padding (4 padding characters)"
```

With the `?` operator:

```ry
fn process(input: str) -> Result<str, Error>:
    decoded = decode(input)?
    return Ok(decoded)
```
