[English](../../tutorial/09-modules.md) | [日本語](09-modules.md) | [繁體中文](../../zh/tutorial/09-modules.md)

# パッケージ

[← 前: 高度な機能](08-advanced.md) | [次: 契約による設計 →](10-contracts.md)

Ry はパッケージシステムを使って、ファイルやディレクトリにまたがるコードを管理します。詳細な仕様は[パッケージリファレンス](../reference/packages.md)を参照してください。

---

## from/import 構文

別ファイルの関数をインポートするには `from` 構文を使います。

```python
from math import add, sub   # 選択インポート
from math                    # 全関数インポート
```

これで `math.ry` に定義された関数が使えるようになります。

---

## サブディレクトリ（ドット区切り）

ドット区切りでサブディレクトリ内のパッケージを指定できます。

```python
from utils.math import add   # utils/math.ry をインポート
```

ドット1つがディレクトリの区切りに対応します。

---

## ディレクトリパッケージ

パッケージは単一の `.ry` ファイルでも、複数の `.ry` ファイルを含むディレクトリでも構いません。パッケージがディレクトリに解決される場合、その中のすべての `.ry` ファイルが自動的にロードされます。

```
mypackage/
  math.ry      # fn add(), fn sub()
  string.ry    # fn concat()
```

```python
from mypackage              # add, sub, concat をインポート
from mypackage import add   # add のみインポート
```

特別なエントリファイル（`__init__.py` のようなもの）は不要です。`_` で始まるファイルは除外されます。

---

## 標準ライブラリ（`std`）

`std` パッケージはすべてのプログラムに自動的にインポートされます。`from std` を記述する必要はありません。

```python
# これらの関数はインポートなしで利用可能
print("hello")
@const
n = len("world")
@const
xs = range(5)
```

`std` のサブパッケージから特定の定義を明示的にインポートすることもできます。

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

パッケージファイルは以下の順序で検索されます。

1. **インポート元ファイルのディレクトリ** — インポートを記述したファイルと同じディレクトリを最初に探します。
2. **`$RY_HOME/lib`** — 標準ライブラリの場所。
3. **実行ファイル相対の `lib/`** — `ry` 実行ファイルからの相対ディレクトリ。
4. **`RY_PATH` 環境変数** — 見つからない場合は `RY_PATH` に指定されたディレクトリを順番に検索します。

---

## RY_PATH 環境変数

複数のディレクトリをコロン区切りで指定できます。

```bash
export RY_PATH=/home/user/ry-libs:/usr/local/ry-libs
```

設定後は、指定ディレクトリ内のパッケージをどこからでもインポートできます。

---

## 制限事項

- `from` 文はファイルの**トップレベル**にのみ記述できます。関数やブロックの内部には書けません。
- 同じパッケージを複数回インポートしても、自動的にスキップされます（二重インポートは発生しません）。
- **循環インポート**（A が B をインポートし、B が A をインポートする）はエラーになります。

```python
# エラー例: a.ry と b.ry が互いをインポートしている場合
# a.ry: from b import foo
# b.ry: from a import bar  ← 循環インポートエラー
```

---

[← 前: 高度な機能](08-advanced.md) | [次: 契約による設計 →](10-contracts.md)
