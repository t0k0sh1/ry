# モジュールリファレンス

## 概要

Ry はファイル単位でモジュールを管理します。`from` 文でモジュールをインポートします。

---

## インポート構文

### 全関数インポート

```python
from math
```

モジュール内のすべての関数をインポートします。

### 選択インポート

```python
from math import add
```

指定した関数のみをインポートします。

### 複数選択インポート

```python
from math import add, sub
```

カンマ区切りで複数の関数を選択インポートします。

---

## サブディレクトリのモジュール

ドット区切りでサブディレクトリを指定します。

| インポート文 | 対応するファイルパス |
|-------------|-------------------|
| `from math` | `math.ry` |
| `from utils.math` | `utils/math.ry` |
| `from a.b.c` | `a/b/c.ry` |

```python
from utils.math import add
from net.http import get
```

モジュール名には拡張子（`.ry`）は含めません。

---

## 検索パスの優先順位

1. インポート元ファイルのディレクトリ
2. `RY_PATH` 環境変数に含まれるパス（コロン区切り）

---

## RY_PATH 環境変数

`RY_PATH` にコロン区切りでディレクトリを指定すると、モジュール検索パスに追加されます。

```bash
export RY_PATH="/usr/local/ry/lib:/home/user/ry-modules"
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

# OK: 同じモジュールを複数回インポートしてもエラーにならない
from math
from math   # スキップされる
```

---

## モジュールファイルの作成

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
