[English](../../reference/base64.md) | [日本語](../../ja/reference/base64.md) | [繁體中文](base64.md)

# Base64 函数参考

Base64 编码与解码。所有函数需要从 `base64` 明确导入。

```python
from base64 import encode, decode, encode_url_safe, decode_url_safe
```

## 函数列表

| 函数 | 签名 | 说明 |
|------|------|------|
| `encode` | `(str) -> str` | 将字符串编码为标准 base64 |
| `decode` | `(str) -> Result<str, Error>` | 解码标准 base64 字符串 |
| `encode_url_safe` | `(str) -> str` | 将字符串编码为 URL-safe base64（无填充） |
| `decode_url_safe` | `(str) -> Result<str, Error>` | 解码 URL-safe base64 字符串 |

## 使用示例

### 基本编码与解码

```python
from base64 import encode, decode

encoded = encode("Hello, World!")
print(encoded)  # SGVsbG8sIFdvcmxkIQ==

match decode(encoded):
    case Ok(s):
        print(s)  # Hello, World!
    case Err(e):
        print(e.message)
```

### URL-safe Base64

URL-safe base64 使用 `-` 和 `_` 取代 `+` 和 `/`，并省略填充（`=`）。适用于 URL、文件名和令牌。

```python
from base64 import encode_url_safe, decode_url_safe

encoded = encode_url_safe("data with special chars: ?&=")
# No + / or = in the output

match decode_url_safe(encoded):
    case Ok(s):
        print(s)
    case Err(e):
        print(e.message)
```

### 处理字节数据

要编码/解码字节数据，请结合 `io` 中的 `str_to_bytes` / `bytes_to_str` 使用。

```python
from base64 import encode, decode
from io import str_to_bytes, bytes_to_str

bytes = str_to_bytes("binary data")
encoded = encode(bytes_to_str(bytes)?)
```

## 错误处理

`decode` 和 `decode_url_safe` 返回 `Result<str, Error>`。当输入包含无效的 base64 字符时，解码会失败。

```python
match decode("!!!not-valid!!!"):
    case Ok(s):
        print(s)
    case Err(e):
        print(e.message)  # "invalid base64 character at position 0"
```

使用 `?` 运算符:

```python
fn process(input: str) -> Result<str, Error>:
    decoded = decode(input)?
    return Ok(decoded)
```
