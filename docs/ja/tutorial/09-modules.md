[English](../../tutorial/09-modules.md) | [日本語](09-modules.md) | [繁體中文](../../zh/tutorial/09-modules.md)

# パッケージ

[<- 前: エラーハンドリング](08-error-handling.md) | [次: 並行処理 ->](10-concurrency.md)

Ry はパッケージシステムを使って、ファイルやディレクトリにまたがるコードを管理します。詳細な仕様は[パッケージリファレンス](../reference/packages.md)を参照してください。

---

## from/import 構文

別ファイルの関数をインポートするには `from` 構文を使います。

```python
from math import sqrt, PI   # 選択インポート
from math                    # 全インポート（すべての定義）
```

これで `math.ry` に定義された関数が使えるようになります。

---

## サブディレクトリ（ドット区切り）

ドット区切りでサブディレクトリ内のパッケージを指定できます。

```python
from utils.calc import add   # utils/calc.ry をインポート
```

ドット1つがディレクトリの区切りに対応します。

---

## ディレクトリパッケージ

パッケージは単一の `.ry` ファイルでも、複数の `.ry` ファイルを含むディレクトリでも構いません。パッケージがディレクトリに解決される場合、その中のすべての `.ry` ファイルが自動的にロードされます。

```
mypackage/
  calc.ry      # function add(), function sub()
  string.ry    # function concat()
```

```python
from mypackage              # add, sub, concat をインポート
from mypackage import add   # add のみインポート
```

特別なエントリファイル（`__init__.py` のようなもの）は不要です。`_` で始まるファイルは除外されます。

---

## 相対インポート

先頭の `.` を使って、現在のファイルのディレクトリからの相対パスでインポートします。テストファイルから兄弟モジュールをインポートする際に特に便利です。

```python
from .helper import greet       # 同じディレクトリの helper.ry からインポート
from .utils import add          # utils/ サブディレクトリからインポート
from .utils.calc import mul     # utils/calc/ ネストされたサブディレクトリからインポート
from . import add, sub          # 現在のディレクトリパッケージからシンボルをインポート
```

相対インポートは現在のファイルのディレクトリに対して**のみ**解決されます -- 標準ライブラリやその他の検索パスは検索されません。プロジェクトに標準ライブラリパッケージと同名のモジュールがある場合の名前衝突を防ぎます。

```python
# プロジェクトに src/math/stats.ry がある場合:
from .math import mean    # 常にローカルの math パッケージに解決される
from math import sqrt     # 標準ライブラリの math パッケージに解決される
```

> **注意:** 親ディレクトリインポート（`from ..`）はサポートされていません。

---

## 標準ライブラリ（`std`）

`std` パッケージはすべてのプログラムに自動的にインポートされます。`from std` を記述する必要はありません。

```python
# これらの関数はインポートなしで利用可能
print("hello")
n = length("world")
xs = range(5)
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

---

## 検索パスの優先順位

パッケージファイルは以下の順序で検索されます:

1. **インポート元ファイルのディレクトリ** -- インポートを記述したファイルと同じディレクトリを最初に探します。
2. **`$RY_HOME/lib`** -- 標準ライブラリの場所。
3. **実行ファイル相対の `lib/`** -- `ry` 実行ファイルからの相対ディレクトリ。
4. **`RY_PATH` 環境変数** -- 見つからない場合は `RY_PATH` に指定されたディレクトリを順番に検索します。

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
# b.ry: from a import bar  <- 循環インポートエラー
```

---

[<- 前: エラーハンドリング](08-error-handling.md) | [次: 並行処理 ->](10-concurrency.md)
