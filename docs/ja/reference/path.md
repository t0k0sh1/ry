[English](../../reference/path.md) | [日本語](path.md) | [简体中文](../../zh/reference/path.md)

# パス関数リファレンス

ファイルパスの操作を行います。すべての関数は `path` からの明示的なインポートが必要です。

```python
from path import join, basename, dirname, extension, resolve, is_absolute
```

## 関数一覧

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `join` | `(str, str) -> str` | 2つのパスセグメントを結合 |
| `join` | `(str, str, str) -> str` | 3つのパスセグメントを結合 |
| `join` | `(str, str, str, str) -> str` | 4つのパスセグメントを結合 |
| `basename` | `(str) -> str` | ファイル名コンポーネントを抽出 |
| `dirname` | `(str) -> str` | ディレクトリコンポーネントを抽出 |
| `extension` | `(str) -> str` | ファイル拡張子を抽出（ドットを含む） |
| `resolve` | `(str) -> Result<str, Error>` | パスを絶対正規形に解決 |
| `is_absolute` | `(str) -> bool` | パスが絶対パスかどうかを返す |

## 使用例

### パスの結合

```python
from path import join

p = join("/tmp", "data", "file.txt")
print(p)  # /tmp/data/file.txt

# 第2引数が絶対パスの場合、第1引数を置き換える
print(join("/tmp", "/usr"))  # /usr
```

### パスコンポーネントの抽出

```python
from path import basename, dirname, extension

p = "/home/user/docs/report.pdf"

print(basename(p))    # report.pdf
print(dirname(p))     # /home/user/docs
print(extension(p))   # .pdf
```

### 拡張子のエッジケース

```python
from path import extension

print(extension("archive.tar.gz"))  # .gz
print(extension(".gitignore"))      # （空文字列 — 拡張子のない隠しファイル）
print(extension(".config.json"))    # .json
print(extension("Makefile"))        # （空文字列）
```

### 絶対パスの判定

```python
from path import is_absolute

print(is_absolute("/usr/local"))  # true
print(is_absolute("src/main.ry")) # false
```

### パスの解決

```python
from path import resolve

match resolve("/tmp"):
  case Ok(p):
    print(p)  # /private/tmp (on macOS) or /tmp
  case Err(e):
    print(e.message)

match resolve("/nonexistent"):
  case Ok(p):
    print(p)
  case Err(e):
    print(e.message)  # cannot resolve path '/nonexistent': No such file or directory
```
