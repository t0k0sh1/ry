[English](../../reference/filesystem.md) | [日本語](filesystem.md) | [简体中文](../../zh/reference/filesystem.md)

# ファイルシステム関数リファレンス

ファイルおよびディレクトリの操作を行います。すべての関数は `filesystem` からの明示的なインポートが必要です。

`filesystem` パッケージはファイルやディレクトリ自体に対する操作（コピー、移動、削除など）を扱います。一方、`io` パッケージはファイルの内容の読み書きを扱います。

```python
from filesystem import list_dir, walk, glob_files, copy, move, remove, remove_all
from filesystem import make_dir, make_dir_all, file_size, is_file, is_dir, is_symlink
from filesystem import chmod, symlink, read_link
```

## 関数一覧

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `list_dir` | `(str) -> Result<List<str>, Error>` | ディレクトリ内のエントリを一覧表示（非再帰） |
| `walk` | `(str) -> Result<List<str>, Error>` | すべてのファイルとディレクトリを再帰的に一覧表示 |
| `glob_files` | `(str) -> Result<List<str>, Error>` | glob パターンに一致するファイルを検索 |
| `copy` | `(str, str) -> Result<Unit, Error>` | ファイルをコピー |
| `move` | `(str, str) -> Result<Unit, Error>` | ファイルまたはディレクトリを移動・リネーム |
| `remove` | `(str) -> Result<Unit, Error>` | ファイルまたは空のディレクトリを削除 |
| `remove_all` | `(str) -> Result<Unit, Error>` | ファイルまたはディレクトリツリーを再帰的に削除 |
| `make_dir` | `(str) -> Result<Unit, Error>` | 単一のディレクトリを作成 |
| `make_dir_all` | `(str) -> Result<Unit, Error>` | ディレクトリと不足している親ディレクトリをすべて作成 |
| `file_size` | `(str) -> Result<int, Error>` | ファイルサイズをバイト単位で返す |
| `is_file` | `(str) -> bool` | パスが通常ファイルかどうかを確認 |
| `is_dir` | `(str) -> bool` | パスがディレクトリかどうかを確認 |
| `is_symlink` | `(str) -> bool` | パスがシンボリックリンクかどうかを確認 |
| `chmod` | `(str, int) -> Result<Unit, Error>` | ファイルのパーミッションを変更（POSIX モード） |
| `symlink` | `(str, str) -> Result<Unit, Error>` | シンボリックリンクを作成 |
| `read_link` | `(str) -> Result<str, Error>` | シンボリックリンクのターゲットを読み取る |

## 使用例

### ディレクトリ操作

```python
from filesystem import make_dir, make_dir_all, list_dir, remove_all

# 単一のディレクトリを作成
match make_dir("/tmp/myapp"):
  case Ok(_):
    print("created")
  case Err(e):
    print("error: " + e.message)

# ネストされたディレクトリを作成（mkdir -p と同等）
make_dir_all("/tmp/myapp/data/logs")

# ディレクトリの内容を一覧表示
match list_dir("/tmp/myapp"):
  case Ok(entries):
    for entry in entries:
      print(entry)
  case Err(e):
    print("error: " + e.message)

# ディレクトリツリーを削除（rm -rf と同等）
remove_all("/tmp/myapp")
```

### ファイル操作

```python
from filesystem import copy, move, remove, file_size
from io import write_text

write_text("/tmp/hello.txt", "Hello, World!")

# ファイルをコピー
copy("/tmp/hello.txt", "/tmp/hello_copy.txt")

# ファイルサイズを取得
match file_size("/tmp/hello.txt"):
  case Ok(sz):
    print("size: " + to_str(sz))
  case Err(e):
    print("error: " + e.message)

# ファイルを移動・リネーム
move("/tmp/hello_copy.txt", "/tmp/renamed.txt")

# ファイルを削除
remove("/tmp/renamed.txt")
```

### 再帰的な走査

```python
from filesystem import walk, glob_files

# ディレクトリツリーを走査（find と同等）
match walk("/var/log"):
  case Ok(files):
    for f in files:
      print(f)
  case Err(e):
    print("error: " + e.message)

# glob パターンマッチング
match glob_files("/var/log/*.log"):
  case Ok(matches):
    for m in matches:
      print(m)
  case Err(e):
    print("error: " + e.message)
```

### パスの種類チェック

```python
from filesystem import is_file, is_dir, is_symlink

if is_file("/etc/hosts"):
  print("regular file")

if is_dir("/tmp"):
  print("directory")

if is_symlink("/usr/local/bin/python"):
  print("symbolic link")
```

### シンボリックリンク

```python
from filesystem import symlink, read_link, is_symlink

# シンボリックリンクを作成
symlink("/usr/local/bin/ry", "/tmp/ry_link")

# シンボリックリンクを確認・読み取り
if is_symlink("/tmp/ry_link"):
  match read_link("/tmp/ry_link"):
    case Ok(target):
      print("points to: " + target)
    case Err(e):
      print("error: " + e.message)
```

### パーミッション

```python
from filesystem import chmod

# chmod 755 (rwxr-xr-x) — 10進数値を使用: 0o755 = 493
chmod("/tmp/script.sh", 493)

# chmod 644 (rw-r--r--) — 0o644 = 420
chmod("/tmp/data.txt", 420)
```

## 注意事項

- `is_file`、`is_dir`、`is_symlink` はエラー時（例: パスが存在しない）に `false` を返す
- `is_file` と `is_dir` はシンボリックリンクをたどる。`is_symlink` はリンクを検出するために `lstat` を使用する
- `list_dir` はエントリ名のみを返す（フルパスではない）
- `walk` はすべてのエントリ（ファイルとディレクトリの両方）のフルパスを返す
- `glob_files` はパターンに一致するファイルがない場合、空のリストを返す（エラーではない）
- `remove` は空でないディレクトリでは失敗する。再帰的な削除には `remove_all` を使用する
