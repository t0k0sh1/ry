[English](../../reference/packages.md) | [日本語](packages.md) | [繁體中文](../../zh/reference/packages.md)

# パッケージリファレンス

## 概要

Ry はパッケージシステムでコードを管理します。**パッケージ**は単一の `.ry` ファイル、またはディレクトリ（複数の `.ry` ファイルを含む）のいずれかです。`from` 文でパッケージをインポートします。

`std` パッケージ（標準ライブラリ）はすべてのプログラムに自動的にインポートされます。

---

## インポート構文

### 全定義インポート

```python
from math
```

パッケージ内のすべての関数・型をインポートします。

### 選択インポート

```python
from math import add
```

指定した定義のみをインポートします。

### 複数選択インポート

```python
from math import add, sub
```

カンマ区切りで複数の定義を選択インポートします。

---

## パッケージ解決

ドット区切りのパッケージ名は以下のように解決されます:

| インポート文 | 解決先 |
|---|---|
| `from math` | `math/` ディレクトリ（パッケージ）または `math.ry` ファイル |
| `from utils.math` | `utils/math/` ディレクトリまたは `utils/math.ry` ファイル |
| `from std.str` | `std/str/` ディレクトリまたは `std/str.ry` ファイル |

### 解決順序

各検索パスに対して:
1. **ディレクトリ** (`{path}/`) — 存在すればディレクトリ内の全 `.ry` ファイルを読み込む（パッケージ）
2. **ファイル** (`{path}.ry`) — 単一ファイル（後方互換）

### ディレクトリパッケージ

パッケージがディレクトリに解決された場合:
- ディレクトリ内のすべての `.ry` ファイルが自動的に読み込まれる
- `_` で始まるファイルは除外される
- 特別なエントリファイル（`__init__.py` のようなもの）は不要
- ディレクトリ内のファイルで定義されたすべての関数・型がエクスポートされる

### プライベートシンボル

名前が `_`（アンダースコア）で始まる定義はパッケージ内部のプライベートシンボルとして扱われ、インポートできません:

- ワイルドカードインポート（`from pkg`）では `_` プレフィックスのシンボルが自動的に除外される
- 名前指定インポート（`from pkg import _helper`）はコンパイルエラーになる

```python
# mylib/internal.ry
fn _helper() -> int:     # プライベート — インポート不可
    return 42
fn public_api() -> int:  # パブリック — インポート可能
    return _helper()
```

```
mypackage/
  math.ry      # fn add(), fn sub()
  string.ry    # fn concat()
```

```python
from mypackage          # add, sub, concat をインポート
from mypackage import add   # add のみインポート
```

---

## 標準ライブラリ (`std`)

`std` パッケージはすべてのプログラムに自動的にインポートされます。提供する機能:
- 組み込み関数（`print`, `len`, `range` など）
- 文字列関数（`contains`, `find`, `replace` など）
- 型変換関数（`to_int`, `to_float`, `to_str`）
- コレクション関数（`map`, `filter`, `sort` など）

### サブパッケージ

以下のサブパッケージは明示的なインポートが必要です:

| パッケージ | 説明 |
|-----------|------|
| [`std.math`](math.md) | 数学定数・関数 |

```python
from std.math import sqrt, PI, sin
```

特定の定義を `std` から明示的にインポートすることもできます:

```python
from std.str import contains
```

### RY_HOME

標準ライブラリは `$RY_HOME/lib/std/` にインストールされます。`RY_HOME` のデフォルト値は `~/.ry` です。

```bash
export RY_HOME="$HOME/.ry"   # デフォルト
```

---

## 検索パスの優先順位

1. インポート元ファイルのディレクトリ
2. `$RY_HOME/lib`（標準ライブラリの場所）
3. 実行ファイル相対の `lib/` ディレクトリ
4. `RY_PATH` 環境変数に含まれるパス（コロン区切り）

---

## RY_PATH 環境変数

`RY_PATH` にコロン区切りでディレクトリを指定すると、パッケージ検索パスに追加されます。

```bash
export RY_PATH="/usr/local/ry/lib:/home/user/ry-packages"
```

---

## 制約

| 制約 | 詳細 |
|------|------|
| 使用可能な位置 | トップレベルのみ（関数・ブロック内は不可） |
| 二重インポート | 自動でスキップ（エラーにならない） |
| 循環インポート | コンパイルエラー |

```python
# エラー例: ブロック内でのインポート
fn main():
    from math   # エラー: トップレベル以外ではインポート不可

# OK: 同じパッケージを複数回インポートしてもエラーにならない
from math
from math   # スキップされる
```

---

## パッケージファイルの作成

### 単一ファイルパッケージ

```python
# math.ry
fn add(a: int, b: int) -> int:
    return a + b

fn sub(a: int, b: int) -> int:
    return a - b
```

```python
# main.ry
from math import add, sub

print(add(1, 2))   # 3
print(sub(5, 3))   # 2
```

### ディレクトリパッケージ

```
mylib/
  math.ry
  string.ry
```

```python
# main.ry
from mylib import add, concat
```
