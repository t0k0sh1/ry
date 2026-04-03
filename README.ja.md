[English](README.md) | [日本語](README.ja.md) | [繁體中文](README.zh.md)

<p align="center">
  <img src="docs/logo.png" alt="Ry" width="200">
</p>

<p align="center">
  LLVM JIT ベースのシンプルなプログラミング言語。<br>
  ソースコードを読み込み、LLVM ORC JIT でネイティブコードにコンパイルし、即時実行します。
</p>

## 特徴

- **LLVM JIT コンパイル** — ORC LLJIT による高速なネイティブ実行
- **豊富な型システム** — `int`, `float`, `bool`, `str`, `Option<T>`, `Error`, タプル, `List<T>`, `Map<K,V>`, `Set<T>`, `enum`, 関数型, ユーザー定義構造体
- **演算子** — 算術・比較・論理・ビット演算（`>>>` 論理右シフト）・複合代入・`in` / `not in`・文字列繰り返し（`"ab" * 3`）・`as` 型キャスト（演算子オーバーロード対応）
- **F-String** — `f"Hello {name}"` による文字列補間
- **契約による設計** — `require`（事前条件）・`ensure`（事後条件）・`invariant`（構造体不変条件）・`old()`・`result`
- **ディレクティブ** — `@deprecated` コンパイル時メタデータアノテーション
- **関数** — `function` 定義・再帰・オーバーロード・ラムダ（クロージャ）・高階関数・UFCS
- **制御構文** — `if`/`else`, `when`, `while`, `for...in`, `break`/`continue`
- **ファイル I/O** — ファイル読み書き・バイト操作・標準入力（`std.io`）
- **ファイルシステム** — ディレクトリ一覧・再帰走査・glob・コピー・移動・削除・パーミッション（`std.filesystem`）
- **パッケージ** — ディレクトリベースのパッケージ、自動インポートされる `std` ライブラリ、`from ... import ...`
- **並行処理** — `async`/`await` と work-stealing スケジューラ、`@parallel` for ループ、ネイティブスレッド API（`std.thread`）
- **型安全** — 型推論・型アノテーション・型変更再代入禁止・`@const` ディレクティブ

## サンプルコード

```python
# 変数と型
x: int = 42
name: str = "hello"
pi = 3.14159

# 関数定義
function factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)

print(factorial(5))    # 120

# ラムダとクロージャ
offset = 10
add_offset = (x: int) -> int => x + offset
print(add_offset(5))   # 15

# 構造体
record Point:
    x: int
    y: int

function operator+(a: Point, b: Point) -> Point:
    return Point(a.x + b.x, a.y + b.y)

p = Point(1, 2) + Point(3, 4)
print(p.x)             # 4

# コレクション
xs = [1, 2, 3]
m = {"a": 1, "b": 2}
s = {1, 2, 3}

for x in xs:
    print(x)

print(2 in s)          # true
print(m["a"])           # 1

# ストリーム操作 (filter, map, sort)
result = [5, 3, 1, 4, 2].filter((x: int) => x > 1).map((x: int) => x * 10).sort()
print(result)          # [20, 30, 40, 50]

# 列挙型
enum Color:
    Red
    Green
    Blue

c = Color::Red
print(c)               # Red

# パッケージインポート
from math import sqrt, PI
print(sqrt(PI))
```

## インストール

### ワンライナー（macOS Apple Silicon）

```bash
curl -fsSL https://raw.githubusercontent.com/t0k0sh1/ry/main/install.sh | sh
```

特定バージョンを指定する場合:

```bash
curl -fsSL https://raw.githubusercontent.com/t0k0sh1/ry/main/install.sh | sh -s v0.0.4
```

デフォルトでは `~/.local/bin` にインストールされます。`RY_INSTALL_DIR` 環境変数で変更可能です。

標準ライブラリは `$RY_HOME/lib/std/`（デフォルト: `~/.ry/lib/std/`）にインストールされます。

### ソースからビルド

必要環境:
- LLVM 21
- CMake 3.20+
- C++17 対応コンパイラ

```bash
cmake -B build -DLLVM_DIR=/usr/local/llvm/lib/cmake/llvm
cmake --build build
```

## 使い方

```bash
ry <file.ry>              # Ry スクリプトを実行
echo '<code>' | ry         # 標準入力からコードを実行
ry test [options] [path]   # テストを実行 (*.test.ry)
ry init                    # カレントディレクトリでプロジェクトを初期化
ry new <name>              # 新しいプロジェクトを作成
ry run [<script-name>]     # プロジェクトスクリプトを実行
ry fmt [options] [path]    # ソースファイルをフォーマット
ry self-update             # ry 自体を更新
```

`self-update` コマンドは Ed25519 署名検証と SHA-256 チェックサムによりリリースアーティファクトを検証します。署名検証はデフォルトで必須です。署名ファイルが取得できない場合、更新は中止されます。`RY_SKIP_SIGNATURE=1` を設定すると、署名ファイルがない場合でも続行を許可します（非推奨）。無効な署名の場合は、この設定に関係なく常に更新を中止します。

ヒアドキュメントにも対応しています:

```bash
ry <<'RY'
a = 1
b = 2
print(a + b)
RY
```

各コマンドの詳細は `ry <command> --help` で確認できます。

内部実行の解析のために、Ry は構造化トレースモードもサポートしています:

```bash
ry --trace app/main.ry
ry --trace-out=/tmp/ry-trace.jsonl app/main.ry
ry test --trace tests/spec
```

`--trace` はデフォルトで JSON Lines を stderr に出力します。`--trace-out` を使用するとトレースストリームをファイルにリダイレクトし、プログラムの stdout はそのまま保持できます。

## 開発

```bash
cd build && ctest --output-on-failure
```

## ドキュメント

詳しい言語仕様・チュートリアルは [docs/](docs/ja/README.md) を参照してください。
