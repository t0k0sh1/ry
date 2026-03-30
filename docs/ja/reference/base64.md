[English](../../reference/base64.md) | [日本語](base64.md) | [繁體中文](../../zh/reference/base64.md)

# Base64 関数リファレンス

Base64 エンコード・デコード。すべての関数は `base64` からの明示的なインポートが必要です。

```python
from base64 import encode, decode, encode_url_safe, decode_url_safe
```

## 関数一覧

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `encode` | `(str) -> str` | 文字列を標準 base64 にエンコード |
| `decode` | `(str) -> Result<str, Error>` | 標準 base64 文字列をデコード |
| `encode_url_safe` | `(str) -> str` | 文字列を URL-safe base64 にエンコード（パディングなし） |
| `decode_url_safe` | `(str) -> Result<str, Error>` | URL-safe base64 文字列をデコード |

## 使用例

### 基本的なエンコード・デコード

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

URL-safe base64 は `+` と `/` の代わりに `-` と `_` を使用し、パディング（`=`）を省略します。URL、ファイル名、トークンに適しています。

```python
from base64 import encode_url_safe, decode_url_safe

encoded = encode_url_safe("data with special chars: ?&=")
# 出力に + / = は含まれない

when decode_url_safe(encoded):
    case Ok(s):
        print(s)
    case Err(e):
        print(e.message)
```

### バイトデータの操作

バイトデータのエンコード・デコードには `io` の `str_to_bytes` / `bytes_to_str` と組み合わせます。

```python
from base64 import encode, decode
from io import str_to_bytes, bytes_to_str

bytes = str_to_bytes("binary data")
encoded = encode(bytes_to_str(bytes)?)
```

## エラーハンドリング

`decode` と `decode_url_safe` は `Result<str, Error>` を返します。無効な base64 文字が含まれている場合、デコードは失敗します。

```python
when decode("!!!not-valid!!!"):
    case Ok(s):
        print(s)
    case Err(e):
        print(e.message)  # "invalid base64 character at position 0"
```

`?` 演算子との組み合わせ:

```python
fn process(input: str) -> Result<str, Error>:
    decoded = decode(input)?
    return Ok(decoded)
```
