[English](../../reference/io.md) | [日本語](io.md) | [繁體中文](../../zh/reference/io.md)

# I/O 関数リファレンス

標準入出力とファイル操作の関数一覧です。すべての関数は `io` からの明示的なインポートが必要です。

```python
from io import read_text, write_text, exists
```

## 関数一覧

### 標準入力

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `read_line` | `() -> str` | stdin から 1 行読み取り（末尾改行除去） |
| `read_all` | `() -> str` | stdin を EOF まで全読み |

### ファイル I/O

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `read_text` | `(str) -> Result<str, Error>` | ファイル全体を文字列として読み取り |
| `write_text` | `(str, str) -> Result<Unit, Error>` | ファイルに文字列を書き込み（上書き） |
| `append_text` | `(str, str) -> Result<Unit, Error>` | ファイル末尾に文字列を追記 |
| `exists` | `(str) -> bool` | ファイル存在チェック |
| `delete_file` | `(str) -> Result<Unit, Error>` | ファイル削除 |
| `read_bytes` | `(str) -> Result<List<u8>, Error>` | ファイルをバイト列として読み取り |
| `write_bytes` | `(str, List<u8>) -> Result<Unit, Error>` | バイト列をファイルに書き込み |

### バイト列変換

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `to_bytes` | `(str) -> List<u8>` | 文字列を UTF-8 バイト列に変換 |
| `bytes_to_str` | `(List<u8>) -> Result<str, Error>` | バイト列を文字列に変換 |

## 使用例

### ファイルの読み書き

```python
from io import read_text, write_text, append_text, exists, delete_file

case write_text("hello.txt", "Hello, World!"):
    Ok(_):
        case read_text("hello.txt"):
            Ok(content):
                print(content)   # Hello, World!
            Err(e):
                print(e.message)
    Err(e):
        print(e.message)

print(exists("hello.txt"))   # true

case delete_file("hello.txt"):
    Ok(_):
        print(exists("hello.txt"))   # false
    Err(e):
        print(e.message)
```

### バイト操作

```python
from io import to_bytes, bytes_to_str, write_bytes, read_bytes

bs = to_bytes("ABC")
print(length(bs))    # 3

case write_bytes("data.bin", bs):
    Ok(_):
        case read_bytes("data.bin"):
            Ok(rb):
                case bytes_to_str(rb):
                    Ok(s):
                        print(s)          # ABC
                    Err(e):
                        print(e.message)
            Err(e):
                print(e.message)
    Err(e):
        print(e.message)
```

### 標準入力からの読み取り

```python
from io import read_line

name = read_line()
print(f"Hello, {name}!")
```

## エラーハンドリング

ファイル操作は失敗時に終了するのではなく、`Result<T, Error>` を返します。`match` で `Ok`/`Err` パターンを使ってエラーを処理してください:

```python
case read_text("missing.txt"):
    Ok(content):
        print(content)
    Err(e):
        print(e.message)   # cannot open file 'missing.txt' for reading
```

| 操作 | エラー条件 |
|------|-----------|
| `read_text` / `read_bytes` | ファイルが存在しない、または開けない |
| `write_text` / `write_bytes` / `append_text` | ファイルを書き込み用に開けない |
| `delete_file` | ファイルを削除できない |
| `bytes_to_str` | 入力に NUL バイトが含まれている |

## 備考

- `List<u8>` をバッファ型として使用します。標準的なリスト操作（`length()`、`append()`、`slice()`、インデックスアクセス）がすべてバイトリストで使えます。
- ファイルパスは絶対パスを指定しない限り、カレントディレクトリからの相対パスとなります。
- `write_text` と `write_bytes` は既存ファイルを上書きします。既存ファイルに追記するには `append_text` を使用してください。
