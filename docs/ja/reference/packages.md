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
from math import sqrt
```

指定した定義のみをインポートします。

### 複数選択インポート

```python
from math import sqrt, PI
```

カンマ区切りで複数の定義を選択インポートします。

---

## パッケージ解決

ドット区切りのパッケージ名は以下のように解決されます:

| インポート文 | 解決先 |
|---|---|
| `from math` | `math/` ディレクトリ（パッケージ）または `math.ry` ファイル |
| `from utils.math` | `utils/math/` ディレクトリまたは `utils/math.ry` ファイル |
| `from str` | `str/` ディレクトリまたは `str.ry` ファイル |

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
  calc.ry      # fn add(), fn sub()
  string.ry    # fn concat()
```

```python
from mypackage          # add, sub, concat をインポート
from mypackage import add   # add のみインポート
```

---

## 標準ライブラリ (`std`)

`std` パッケージはすべてのプログラムに自動的にインポートされます。提供する機能:
- 組み込み関数（`print`, `length`, `range` など）
- 文字列関数（`contains`, `find`, `replace` など）
- 型変換関数（`to_int`, `to_float`, `to_str`）
- コレクション関数（`map`, `filter`, `sort` など）

### サブパッケージ

以下のサブパッケージは明示的なインポートが必要です:

| パッケージ | 説明 |
|-----------|------|
| [`math`](math.md) | 数学定数・関数 |
| [`io`](io.md) | ファイル I/O・標準入力・バイト変換 |

```python
from math import sqrt, PI, sin
```

標準ライブラリのパッケージから特定の定義を明示的にインポートすることもできます:

```python
from str import contains
```

### RY_HOME

標準ライブラリは `$RY_HOME/lib/std/` にインストールされます。`RY_HOME` のデフォルト値は `~/.ry` です。

```bash
export RY_HOME="$HOME/.ry"   # デフォルト
```

### RY_ENV

`RY_ENV` 環境変数でランタイム環境モードを制御します。`--env=<value>` CLI フラグでも指定可能です。

| 値 | エイリアス | `.env` 読み込み | lib 探索 |
|---|----------|---------------|---------|
| `prod` | `production` | 無効 | リポジトリビルド用プロジェクトオーバーライド → `$RY_HOME/lib` → `exe/../lib` → `exe/lib` |
| `dev` | `development` | `.env.dev` → `.env` | `prod` と同じ |
| `test` | — | `.env.test` → `.env` | `prod` と同じ |
| `staging` | — | `.env.staging` → `.env` | `prod` と同じ |
| `internal` | — | `.env.internal` → `.env` | リポジトリビルド用プロジェクトオーバーライド → `exe/../lib` → `exe/lib`（`$RY_HOME` スキップ） |
| （未設定）（デフォルト） | — | `.env` のみ | `prod` と同じ |

エイリアスは自動的に正規形に解決されます。例えば `RY_ENV=production` は `prod` に正規化されます。

`prod` モードではセキュリティのため `.env` ファイルを読み込みません。本番環境の秘密情報はインフラレベルの環境変数管理（CI/CD、シークレットマネージャー等）で管理してください。

その他のモードでは `.env.<環境名>` を先に読み込み（存在する場合）、次に `.env` を読み込みます。既存の環境変数は上書きされないため、環境別の値が優先されます。

```bash
# 短縮形（推奨）
RY_ENV=dev ./build/ry app.ry

# フルネーム（後方互換）
RY_ENV=development ./build/ry app.ry

# CLI フラグ
./build/ry --env=dev test

# prod モード: .env は読み込まれない
RY_ENV=prod ./build/ry app.ry

# Ry 自体の開発時の追加 isolation
RY_ENV=internal ./build/ry test
```

Ry のソースツリー内でビルドされた `ry` 実行バイナリは、プロジェクトの `package.toml` からリポジトリローカルの stdlib オーバーライドを使用できます。これにより、`~/.ry/lib/std` が古い場合でも、チェックアウトされた `lib/std` とリポジトリビルドの整合性が保たれます。インストール済みの `ry` バイナリはこのオーバーライドを無視し、`$RY_HOME/lib/std` を引き続き使用します。

---

## 検索パスの優先順位

1. インポート元ファイルのディレクトリ
2. 現在の Ry チェックアウトからのリポジトリローカル stdlib オーバーライド（リポジトリビルドの `ry` 使用時）
3. `$RY_HOME/lib`（標準ライブラリの場所）
4. 実行ファイル相対の `lib/` ディレクトリ
5. `RY_PATH` 環境変数に含まれるパス（コロン区切り）

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
# calc.ry
fn add(a: int, b: int) -> int:
    return a + b

fn sub(a: int, b: int) -> int:
    return a - b
```

```python
# main.ry
from calc import add, sub

print(add(1, 2))   # 3
print(sub(5, 3))   # 2
```

### ディレクトリパッケージ

```
mylib/
  calc.ry
  string.ry
```

```python
# main.ry
from mylib import add, concat
```
