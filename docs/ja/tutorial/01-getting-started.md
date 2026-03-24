[English](../../tutorial/01-getting-started.md) | [日本語](01-getting-started.md) | [繁體中文](../../zh/tutorial/01-getting-started.md)

# 01 - はじめに

次のチュートリアル → [02 - 変数と型](02-variables-and-types.md)

---

## 必要環境

Ry をビルドして実行するには以下が必要です。

- **LLVM 21**
- **CMake 3.20 以上**
- **C++17 対応コンパイラ**（GCC 7+ / Clang 5+ 等）

---

## ビルド手順

リポジトリのルートで以下のコマンドを実行します。

```bash
cmake -B build -DLLVM_DIR=/usr/local/llvm/lib/cmake/llvm
cmake --build build
```

ビルドが成功すると `build/ry` という実行ファイルが生成されます。

---

## プロジェクトの初期化

`ry new` コマンドで新しいプロジェクトを作成できます。

```bash
ry new my-project
cd my-project
```

これにより以下のファイルとディレクトリが生成されます。

- `ry.toml` — プロジェクト設定ファイル
- `src/main.ry` — エントリポイント（サンプルコード付き）

カレントディレクトリをプロジェクトとして初期化する場合は `ry init` を使います。

```bash
mkdir my-project
cd my-project
ry init
```

詳細は [プロジェクト管理](../reference/project.md) を参照してください。

---

## 最初のプログラム

以下の内容を `hello.ry` というファイルに保存してください。

```python
print("Hello, World!")
```

次のコマンドで実行します。

```bash
./build/ry hello.ry
```

出力:

```
Hello, World!
```

---

## コメントの書き方

`#` から行末までがコメントとして扱われます。

```python
# これはコメントです
print("Hello")  # 行末コメントも使えます
```

コメントはコードの動作に影響を与えません。

---

次のチュートリアル → [02 - 変数と型](02-variables-and-types.md)
