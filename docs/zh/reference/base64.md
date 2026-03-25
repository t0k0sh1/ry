[English](../../reference/base64.md) | [日本語](../../ja/reference/base64.md) | [繁體中文](base64.md)

# Base64 函式參考

Base64 編碼與解碼。所有函式需要從 `base64` 明確匯入。

```python
from base64 import encode, decode, encode_url_safe, decode_url_safe
```

## 函式列表

| 函式 | 簽名 | 說明 |
|------|------|------|
| `encode` | `(str) -> str` | 將字串編碼為標準 base64 |
| `decode` | `(str) -> Result<str, Error>` | 解碼標準 base64 字串 |
| `encode_url_safe` | `(str) -> str` | 將字串編碼為 URL-safe base64（無填充） |
| `decode_url_safe` | `(str) -> Result<str, Error>` | 解碼 URL-safe base64 字串 |

## 使用範例

### 基本編碼與解碼

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

URL-safe base64 使用 `-` 和 `_` 取代 `+` 和 `/`，並省略填充（`=`）。適用於 URL、檔案名稱和令牌。

```python
from base64 import encode_url_safe, decode_url_safe

encoded = encode_url_safe("data with special chars: ?&=")

match decode_url_safe(encoded):
    case Ok(s):
        print(s)
    case Err(e):
        print(e.message)
```

## 錯誤處理

`decode` 和 `decode_url_safe` 回傳 `Result<str, Error>`。當輸入包含無效的 base64 字元時，解碼會失敗。

```python
match decode("!!!not-valid!!!"):
    case Ok(s):
        print(s)
    case Err(e):
        print(e.message)  # "invalid base64 character at position 0"
```
