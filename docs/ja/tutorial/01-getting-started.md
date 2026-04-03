[English](../../tutorial/01-getting-started.md) | [日本語](01-getting-started.md) | [繁體中文](../../zh/tutorial/01-getting-started.md)

# 01 - はじめに

次のチュートリアル -> [02 - 変数と型](02-variables-and-types.md)

---

## インストール

### クイックインストール（macOS Apple Silicon）

```bash
curl -fsSL https://raw.githubusercontent.com/t0k0sh1/ry/main/install.sh | sh
```

`ry` バイナリが `~/.local/bin` に、標準ライブラリが `~/.ry/lib/std/` にインストールされます。

`~/.local/bin` が `PATH` に含まれていることを確認してください:

```bash
export PATH="$HOME/.local/bin:$PATH"
```

ソースからビルドする場合や他のプラットフォームについては、[README のインストールセクション](../../../README.md#installation)を参照してください。

---

## プロジェクトの初期化

`ry new` コマンドで新しいプロジェクトを作成できます:

```bash
ry new my-project
cd my-project
```

これにより以下のファイルとディレクトリが生成されます:

- `package.toml` -- プロジェクト設定ファイル
- `src/main.ry` -- エントリポイント（サンプルコード付き）

カレントディレクトリをプロジェクトとして初期化する場合は `ry init` を使います:

```bash
mkdir my-project
cd my-project
ry init
```

詳細は[プロジェクト管理](../reference/project.md)を参照してください。

---

## 最初のプログラム

以下の内容を `hello.ry` というファイルに保存してください:

```python
print("Hello, World!")
```

次のコマンドで実行します:

```bash
ry hello.ry
```

出力:

```
Hello, World!
```

パイプや Here-document を使って、`-c` フラグで標準入力からコードを実行することもできます:

```bash
echo 'print("Hello, World!")' | ry -c

ry -c <<'RY'
print("Hello, World!")
RY
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

次のチュートリアル -> [02 - 変数と型](02-variables-and-types.md)
