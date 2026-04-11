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

### 相対インポート

```python
from .helper import greet
```

現在のファイルのディレクトリからの相対パスでモジュールをインポートします。`.` プレフィックスにより、解決は現在のディレクトリのみに制限されます（標準ライブラリやその他の検索パスは検索されません）。

### サブディレクトリからの相対インポート

```python
from .utils import helper_fn
from .utils.calc import add
```

現在のファイルのディレクトリからの相対パスでサブディレクトリからインポートします。

### カレントディレクトリからの全相対インポート

```python
from . import add, sub
```

カレントディレクトリパッケージ（ディレクトリ内のすべての `.ry` ファイル、`_` プレフィックスと `.test.ry` ファイルを除く）から特定のシンボルをインポートします。

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
- テストファイル（`.test.ry`）は除外される
- 特別なエントリファイル（`__init__.py` のようなもの）は不要
- ディレクトリ内のファイルで定義されたすべての関数・型がエクスポートされる

### プライベートシンボル

名前が `_`（アンダースコア）で始まる定義はパッケージ内部のプライベートシンボルとして扱われ、インポートできません:

- ワイルドカードインポート（`from pkg`）では `_` プレフィックスのシンボルが自動的に除外される
- 名前指定インポート（`from pkg import _helper`）はコンパイルエラーになる

```python
# mylib/internal.ry
function _helper() -> int:     # プライベート — インポート不可
    return 42
function public_api() -> int:  # パブリック — インポート可能
    return _helper()
```

```
mypackage/
  calc.ry      # function add(), function sub()
  string.ry    # function concat()
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
| [`path`](path.md) | ファイルパス操作（join、basename、dirname 等） |

```python
from math import sqrt, PI, sin
```

標準ライブラリのパッケージから特定の定義を明示的にインポートすることもできます:

```python
from str import contains
```

### RY_HOME

標準ライブラリは `$RY_HOME/share/std/` にインストールされます。`RY_HOME` のデフォルト値は `~/.ry` です。

```bash
export RY_HOME="$HOME/.ry"   # デフォルト
```

### RY_ENV

`RY_ENV` 環境変数でランタイム環境モードを制御します。`--env=<value>` CLI フラグでも指定可能です。

| 値 | エイリアス | `.env` 読み込み | lib 探索 |
|---|----------|---------------|---------|
| `prod` | `production` | 無効 | リポジトリビルド用プロジェクトオーバーライド → `$RY_HOME/share`（フォールバック: `lib`）→ `exe/../share`（フォールバック: `lib`）→ `exe/share`（フォールバック: `lib`） |
| `dev` | `development` | `.env.dev` → `.env` | `prod` と同じ |
| `test` | — | `.env.test` → `.env` | `prod` と同じ |
| `staging` | — | `.env.staging` → `.env` | `prod` と同じ |
| `internal` | — | `.env.internal` → `.env` | リポジトリビルド用プロジェクトオーバーライド → `exe/../share`（フォールバック: `lib`）→ `exe/share`（フォールバック: `lib`）（`$RY_HOME` スキップ） |
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

Ry のソースツリー内でビルドされた `ry` 実行バイナリは、プロジェクトの `package.toml` からリポジトリローカルの stdlib オーバーライドを使用できます。これにより、`~/.ry/share/std` が古い場合でも、チェックアウトされた `share/std` とリポジトリビルドの整合性が保たれます。インストール済みの `ry` バイナリはこのオーバーライドを無視し、`$RY_HOME/share/std` を引き続き使用します。

---

## 検索パスの優先順位

1. インポート元ファイルのディレクトリ
2. 現在の Ry チェックアウトからのリポジトリローカル stdlib オーバーライド（リポジトリビルドの `ry` 使用時）
3. `$RY_HOME/share`（標準ライブラリの場所、レガシーインストールでは `$RY_HOME/lib` にフォールバック）
4. 実行ファイル相対の `share/` ディレクトリ（レガシーレイアウトでは `lib/` にフォールバック）
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
| 相対インポート | `from .` と `from .pkg` は現在のファイルのディレクトリのみに対して解決される |
| 親ディレクトリインポート | `from ..` は未対応 |
| パッケージ名 | アルファベット、数字、アンダースコアのみ使用可能（ハイフンは不可） |

```python
# エラー例: ブロック内でのインポート
function main():
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
function add(a: int, b: int) -> int:
    return a + b

function sub(a: int, b: int) -> int:
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

---

## Native 関数の命名規約

C ランタイム関数として実装される stdlib パッケージの関数は、`__ry_<package>_<function_name>` 規約に従います。

> **注意**: この規約は stdlib パッケージ関数（例: `base64`, `filesystem`, `path`）に適用されます。組み込み関数（例: `print`, `length`）や math 関数は実装がさまざま（インライン LLVM IR、libc 呼び出し等）で、この命名パターンには従いません。

### フォーマット

```text
__ry_<package>_<function_name>
```

### ルール

1. **プレフィックス**: `__ry_`
2. **パッケージ**: パッケージ名（例: `from base64 import encode` なら `base64`）
3. **関数名**: Ry で宣言されている snake_case の関数名
4. **オーバーロード**: 関数がアリティの異なる複数のオーバーロードを持つ場合、引数数をサフィックスとして付加する（例: `__ry_path_join2`, `__ry_path_join3`）
5. **エラーゲッター**: `Result` 型を返す各パッケージは `__ry_<pkg>_get_last_error` を提供する

### 例

| Ry 宣言 | C ランタイム関数名 |
|---------|------------------|
| `base64::encode(data: str) -> str` | `__ry_base64_encode` |
| `filesystem::list_dir(path: str) -> Result<List<str>, Error>` | `__ry_filesystem_list_dir` |
| `path::join(a: str, b: str) -> str` | `__ry_path_join2` |
| `path::join(a: str, b: str, c: str) -> str` | `__ry_path_join3` |
