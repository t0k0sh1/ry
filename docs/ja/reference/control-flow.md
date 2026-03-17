[English](../../reference/control-flow.md) | [日本語](control-flow.md) | [繁體中文](../../zh/reference/control-flow.md)

# 制御構文リファレンス

## if / elif / else

### 構文

```python
if 条件式:
    # then ブロック
elif 条件式:
    # elif ブロック（複数可）
else:
    # else ブロック（省略可）
```

### 条件式の型

| 型 | false になる値 | true になる値 |
|---|---|---|
| `bool` | `false` | `true` |
| `int` | `0` | 非 0 |

`float` や `str` は条件式に直接使用できない。

### 例

```python
let x = 10

if x > 5:
    print("big")
elif x == 5:
    print("five")
else:
    print("small")
```

### スコープルール

- `if` / `elif` / `else` の各ブロックはそれぞれ独立したブロックスコープを持つ。
- ブロック内で宣言した変数はブロック外からアクセスできない。

```python
if true:
    let y = 42
# y はここではアクセス不可
```

---

## while

### 構文

```python
while 条件式:
    # ループ本体
```

条件式が `true` の間、ループ本体を繰り返す。

### 例

```python
let i = 0
while i < 5:
    print(i)
    i += 1
```

### break / continue との組み合わせ

```python
let i = 0
while true:
    if i >= 3:
        break
    i += 1
```

---

## for

### 構文

```python
# リスト / セット走査
for x in iterable_expr:
    # x に各要素が代入される

# range（0 始まり）
for i in range(n):
    # i = 0, 1, ..., n-1

# range（開始・終了指定）
for i in range(start, end):
    # i = start, start+1, ..., end-1

# range（ステップ指定）
for i in range(start, end, step):
    # i = start, start+step, start+2*step, ...
```

### 例

```python
let xs = [10, 20, 30]
for x in xs:
    print(x)

let s = {1, 2, 3}
for x in s:
    print(x)

for i in range(5):
    print(i)     # 0 1 2 3 4

for i in range(2, 6):
    print(i)     # 2 3 4 5

for i in range(0, 10, 2):
    print(i)     # 0 2 4 6 8

for i in range(10, 0, -3):
    print(i)     # 10 7 4 1
```

---

## break

- 最も内側のループ（`while` または `for`）を即座に脱出する。
- ループの外で使用するとコンパイルエラー。

```python
for i in range(10):
    if i == 5:
        break    # i == 5 で脱出
    print(i)     # 0 1 2 3 4
```

### エラー例

```python
# ループ外での break はコンパイルエラー
break   # Error: break outside loop
```

---

## continue

- 最も内側のループの現在のイテレーションを終了し、次のイテレーションへスキップする。
- ループの外で使用するとコンパイルエラー。

```python
for i in range(5):
    if i == 2:
        continue   # i == 2 をスキップ
    print(i)       # 0 1 3 4
```

---

## match

### 構文

```python
match 式:
    case パターン:
        # 本体
    case パターン if ガード条件:
        # ガード付き本体
    case _:
        # ワイルドカード（何にでもマッチ）
```

### パターンの種類

| パターン | 例 | 説明 |
|----------|-----|------|
| ワイルドカード | `_` | 何にでもマッチ |
| リテラル | `0`, `"hello"`, `true` | 値の等値比較 |
| 変数束縛 | `n` | 何にでもマッチし、変数に束縛 |
| enum バリアント | `Color::Red` | enum タグの比較（単純な enum） |
| ADT enum バリアント | `Shape::Circle(r)` | 関連データを持つ enum バリアントにマッチし、束縛する |
| `Some(x)` | `Some(v)` | Option が値ありの場合、中身を束縛 |
| `None` | `None` | Option が値なしの場合 |
| OR パターン | `1 \| 2 \| 3` | いずれかにマッチ |

### guard 節

`case パターン if 条件式:` の形式でガード条件を指定できる。パターンがマッチし、かつガード条件が真の場合にのみアームが実行される。

### OR パターン

複数のパターンを `|` で結合し、いずれかにマッチさせることができます。変数束縛（`n`、`Some(x)`、`Ok(v)`、`Err(e)`）は OR パターン内では使用できません。

```python
match x:
    case 1 | 2 | 3:
        print("small")
    case _:
        print("other")

# enum の OR パターン
match color:
    case Color::Red | Color::Blue:
        print("warm or cool")
    case Color::Green:
        print("green")
```

### 網羅性チェック

- enum 型: すべてのバリアントをカバーするか `_` が必要。OR パターンの各選択肢は個別にカウントされる。
- Option 型: `Some` と `None` の両方をカバーするか `_` が必要。
- bool 型: `true` と `false` の両方をカバーするか `_` が必要。
- int / float / str リテラル: `_` が必須。
- ガード付きアームは網羅性にカウントされない。

### 例

```python
# enum マッチ
enum Color:
    Red
    Green
    Blue

match color:
    case Color::Red:
        print("red")
    case Color::Green:
        print("green")
    case Color::Blue:
        print("blue")

# Option マッチ
let x: Option<int> = Some(42)
match x:
    case Some(v):
        print(v)
    case None:
        print("nothing")

# リテラルマッチ
match x:
    case 0:
        print("zero")
    case 1:
        print("one")
    case _:
        print("other")

# guard 節
match x:
    case n if n > 0:
        print("positive")
    case n if n < 0:
        print("negative")
    case _:
        print("zero")
```

### ADT enum マッチ

enum バリアントが関連データを持つ場合、バインディングパターンを使って値を取り出します。

```python
enum Shape:
    Circle(float)
    Rectangle(float, float)
    Point

let s = Shape::Circle(3.14)
match s:
    case Shape::Circle(r):
        print(r)        # 3.14
    case Shape::Rectangle(w, h):
        print(w)
        print(h)
    case Shape::Point:
        print("point")
```

複数フィールドを持つバリアントは、宣言順に各フィールドを別々の名前に束縛します。

### スコープルール

- 各 `case` アームはブロックスコープを持つ。
- 変数束縛パターン (`n`) や `Some(x)` で束縛された変数はそのアーム内でのみ有効。

---

## スコープルール

### ブロックスコープ

- `if` / `elif` / `else` / `while` / `for` / `match` の各ブロックはブロックスコープを持つ。
- ブロック内で宣言した変数はブロックの終了と同時にスコープから外れる。

```python
for i in range(3):
    let tmp = i * 2
# tmp はここではアクセス不可

if true:
    let a = 1
# a はここではアクセス不可
```

### シャドーイング

- 内側のスコープで外側と同名の変数を宣言すると、内側のスコープ内では内側の変数が参照される。
- 内側スコープを抜けると外側の変数に戻る。

```python
let x = 10
if true:
    let x = 99   # 外側の x をシャドーイング
    print(x)     # 99
print(x)         # 10
```
