[English](../../reference/io.md) | [日本語](io.md) | [繁體中文](../../zh/reference/io.md)

# I/O 関数リファレンス

標準入出力とファイル操作の関数一覧です。すべての関数は `std.io` からの明示的なインポートが必要です。

```python
from std.io import read_text, write_text, file_exists
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
| `read_text` | `(str) -> str` | ファイル全体を文字列として読み取り |
| `write_text` | `(str, str) -> Unit` | ファイルに文字列を書き込み（上書き） |
| `append_text` | `(str, str) -> Unit` | ファイル末尾に文字列を追記 |
| `file_exists` | `(str) -> bool` | ファイル存在チェック |
| `delete_file` | `(str) -> Unit` | ファイル削除 |
| `read_bytes` | `(str) -> List<byte>` | ファイルをバイト列として読み取り |
| `write_bytes` | `(str, List<byte>) -> Unit` | バイト列をファイルに書き込み |

### バイト列変換

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `str_to_bytes` | `(str) -> List<byte>` | 文字列を UTF-8 バイト列に変換 |
| `bytes_to_str` | `(List<byte>) -> str` | バイト列を文字列に変換 |

## 使用例

### ファイルの読み書き

```python
from std.io import read_text, write_text, append_text, file_exists, delete_file

write_text("hello.txt", "Hello, World!")
@const
content = read_text("hello.txt")
print(content)   # Hello, World!

append_text("hello.txt", "\nGoodbye!")
print(read_text("hello.txt"))
# Hello, World!
# Goodbye!

print(file_exists("hello.txt"))   # true
delete_file("hello.txt")
print(file_exists("hello.txt"))   # false
```

### バイト操作

```python
from std.io import str_to_bytes, bytes_to_str, write_bytes, read_bytes

@const
bs = str_to_bytes("ABC")
print(len(bs))    # 3

write_bytes("data.bin", bs)
@const
rb = read_bytes("data.bin")
@const
s = bytes_to_str(rb)
print(s)          # ABC
```

### 標準入力からの読み取り

```python
from std.io import read_line

@const
name = read_line()
print(f"Hello, {name}!")
```

## エラー処理

すべてのファイル操作は、操作が失敗した場合に実行時エラーで終了します：

| 操作 | エラー条件 |
|------|-----------|
| `read_text` / `read_bytes` | ファイルが存在しない、または開けない |
| `write_text` / `write_bytes` / `append_text` | ファイルを書き込み用に開けない |
| `delete_file` | ファイルを削除できない |

エラーメッセージは stderr に出力され、プログラムは終了コード 1 で終了します。

## 備考

- `List<byte>` をバッファ型として使用します。標準的なリスト操作（`len()`、`append()`、`slice()`、インデックスアクセス）がすべてバイトリストで使えます。
- ファイルパスは絶対パスを指定しない限り、カレントディレクトリからの相対パスとなります。
- `write_text` と `write_bytes` は既存ファイルを上書きします。既存ファイルに追記するには `append_text` を使用してください。
