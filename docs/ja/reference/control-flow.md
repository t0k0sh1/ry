[English](../../reference/control-flow.md) | [日本語](control-flow.md) | [繁體中文](../../zh/reference/control-flow.md)

# 制御構文リファレンス

## if / else

### 文の構文

```python
if condition:
    # then ブロック
else:
    # else ブロック（省略可）
```

### 式の形

`if` は値を生成する式としても使えます。2 つの形式がサポートされています:

**単一式形式**（`=>`）:

```python
x = if condition => true_value else false_value
```

例:

```python
abs_val = if x > 0 => x else -x
label = if score >= 90 => "A" else "B"
```

単一式形式の `else` 分岐は値を直接取ります（`=>` は不要）。両方の分岐は同じ型を返す必要があり、`else` は必須です。

**ブロック形式**（`:`）:

```python
x = if condition:
    compute_something()
else:
    compute_other()
```

ブロック形式では、各ブロックは式文で終わる必要があります（tail-expression セマンティクス）。`else` 分岐は必須で、両方の分岐は同じ型を返す必要があります。

値を返す多分岐の条件式が必要な場合は、代わりに `case:` を使ってください（下記参照）。

### 条件式の型

| 型 | false になる値 | true になる値 |
|---|---|---|
| `bool` | `false` | `true` |
| `int` | `0` | 非 0 |
| `float` | `0.0` | 非 0 |

`bool`、整数、`float` のみが条件式に使用できます。`str`、
`List`、`Map`、`Set`、イテレータ、クロージャ、record、`Option`、`Result`
は条件式に直接使用できません。コレクションや文字列に対しては、長さチェックを
明示的に書いてください:

```python
xs = [1, 2, 3]
# ✗ エラー: この型の値はブール条件として使えない
# if xs:
#     print("non-empty")
# ✓ 明示的な長さチェック
if length(xs) > 0:
    print("non-empty")
# ✓ is_empty を使った等価なコード
if not is_empty(xs):
    print("non-empty")
```

`Option` と `Result` については、条件式として使うのではなく、`case` で
明示的にバリアントをパターンマッチしてください。これらのルールは
`while`、`case` アーム、単項 `not` 演算子にも等しく適用されます。

### 例

```python
x = 10

if x > 5:
    print("big")
else:
    print("small or equal")
```

### スコープルール

- `if` / `else` の各ブロックはそれぞれ独立したブロックスコープを持つ。
- ブロック内で宣言した変数はブロック外からアクセスできない。

```python
if true:
    y = 42
# y はここではアクセス不可
```

---

## while

### 構文

```python
while condition:
    # ループ本体
```

条件式が `true` の間、ループ本体を繰り返す。

### 例

```python
i = 0
while i < 5:
    print(i)
    i += 1
```

### break / continue との組み合わせ

```python
i = 0
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

### 文字列の反復

`str` に対する `for` ループは、各 **Unicode コードポイント** を 1 文字の `str` として生成します。マルチバイトの UTF-8 シーケンス（CJK 文字や絵文字を含む）は正しくデコードされ、マルチバイト文字の途中で分割されることはありません。

これは **コードポイント** 単位の反復であり、**grapheme クラスタ** 単位ではありません。複数のコードポイントにまたがるユーザーが認識する文字 -- 結合マーク列（例: 基底文字 + U+0301）や ZWJ 絵文字シーケンス（例: 家族や肌の色の合成）-- は、コードポイントごとに 1 回ずつ複数の反復として生成されます。grapheme クラスタを意識した反復が必要な場合は、`for c in s:` に頼らず、将来のセグメンテーションヘルパーで文字列を分解してください。

```python
for c in "hello":
    print(c)               # h, e, l, l, o

for c in "こんにちは":
    print(c)               # こ, ん, に, ち, は  (バイトごとではない)

for c in "a🙂b":
    print(c)               # a, 🙂, b
```

ループ変数は `str` 型なので、他の文字列関数に渡すことができます:

```python
for c in "abc":
    print(to_upper(c))     # A, B, C
```

空文字列を反復するとループ本体は 0 回実行されます。`enumerate` と `zip` も `str` 引数を受け付け、同じコードポイント単位を生成します:

```python
for i, c in enumerate("abc"):
    print(i, c)

for a, b in zip("abc", "xyz"):
    print(a + b)           # ax, by, cz
```

### マップのキー・値走査

```python
for k, v in map_expr:
    # k はキー、v は各エントリの値
```

### タプル分解代入

タプルのリストを走査する際、タプルの要素数に合わせて N 個の変数に分解できます。`_` で値を破棄できます。

```python
xs = [10, 20, 30]

for i, x in enumerate(xs):
    print(f"{i}: {x}")    # 0: 10, 1: 20, 2: 30

for a, b in zip([1, 2], [10, 20]):
    print(a + b)          # 11, 22

for _, x in enumerate(xs):
    print(x)              # インデックスを破棄

# N要素の分解代入（3個以上の変数）
triples = [(1, 2, 3), (4, 5, 6)]
for a, b, c in triples:
    print(a + b + c)      # 6, 15

for a, _, c in triples:
    print(a + c)          # 4, 10（中間の要素を破棄）
```

### 範囲演算子（`..`）

`..` 演算子は両端を含む整数の範囲を生成します。`1 .. 5` は `[1, 2, 3, 4, 5]` を生成します。

```python
for i in 1 .. 5:
    print(i)     # 1 2 3 4 5
```

### 例

```python
xs = [10, 20, 30]
for x in xs:
    print(x)

s = {1, 2, 3}
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

# マップの走査
m = {"a": 1, "b": 2}
for k, v in m:
    print(k)
    print(v)

# 範囲演算子
for i in 1 .. 3:
    print(i)     # 1 2 3
```

---

## async / await

`async function` は並行実行される関数を宣言します。`async function` を呼び出すと `Task<T>` が返ります。別の `async function` 内では `await` を使い、同期コンテキストからは `block_on()` を使って結果を待ちます。

```python
async function add(a: int, b: int) -> int:
    return a + b

# 同期コンテキストからは block_on() を使用
t: Task<int> = add(20, 22)
print(block_on(t))                  # 42
print(block_on(add(1, 2)))          # 3

# async function 内では await を使用
async function double_add(a: int, b: int) -> int:
    result = await add(a, b)
    return result * 2
```

### ルール

- `async function name(...) -> T:` の `T` は await 後の値型です。
- `async function` の呼び出し結果は常に `Task<T>` です。
- `await expr` は `Task<T>` にのみ使用でき、結果は `T` です。
- `await` は `async function` 内でのみ使用可能。同期コンテキストからは `block_on(task)` を使用します。
- `block_on(task)` は現在のスレッドをタスク完了までブロックし、結果を返します。
- `async function ... -> Unit` をサポートします。値を返さない task の待機には `block_on(task)` を使うのが基本です。
- task はランタイムの worker pool 上で実行され、task ごとに OS スレッドを作る実装ではありません。
- `async` ラムダと `async @native function` は v1 では未対応です。

---

## `@parallel for`

`@parallel` は `range(...)` または整数 `..` を使う counted `for` ループにだけ付与できます。ループ本体はランタイムのワーカープール上でチャンク単位に並列実行されます。

```python
@parallel
for i in range(8):
    print(i)
```

### 制約

- 対応するのは `range(...)` と整数 `..` のみです。
- 分解代入付きの反復は未対応です。
- 外側スコープのミュータブル変数への代入は拒否されます。
- `break` と `continue` は使えません。
- v1 ではループ本体内のインデックス代入とフィールド代入も拒否されます。

ランタイムの worker 数は `available_parallelism()` で取得できます。

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

## `...`（Ellipsis）

- 何もしない文（no-op）。空ブロックのプレースホルダーとして使用する。
- 関数ボディ、`if`/`else`、`while`、`for`、`case` アームなど任意のブロック内で使用可能。

```python
function not_yet():
    ...

if true:
    ...
else:
    ...
```

---

## case

`case` は、対象値なしの多分岐条件フロー（以前の `when`）と、パターン
マッチング（以前の `match`）を 1 つの構文に統合したものです。2 つの
形式がサポートされています:

- `case:` -- 対象なし、各アームは条件式（`when:` の置き換え）
- `case <expr>:` -- 対象あり、各アームはパターン（`match` の置き換え）

どちらの形式もブロック本体（`:`）と式本体（`=>`）の両方をサポートします。

> **注意**: `when` と `match` キーワードは統合された `case` 構文に置き換え
> られ、削除されました。`when` / `match` を使う従来の Ry コードは移行が
> 必要です。

### 対象なしの case

対象値のない多分岐条件フローには `case:` を使います。

#### 構文

```python
case:
    condition:
        # 本体
    condition:
        # 本体
    _:
        # フォールバック
```

#### 例

```python
x = 0

case:
    x > 0:
        print("positive")
    x < 0:
        print("negative")
    _:
        print("zero")
```

アームは上から順に評価され、最初に条件が真になったアームだけが実行され
ます。文の場合、ワイルドカードアーム `_:` は省略可能です。

式形式の `case:` については、下の「式の形」セクションを参照してください。

---

## 対象ありの case（パターンマッチング）

### 構文

```python
case expression:
    pattern:
        # 本体
    pattern if guard_condition:
        # ガード付き本体
    _:
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
| `Ok(x)` | `Ok(v)` | Result が Ok の場合、中身を束縛 |
| `Err(x)` | `Err(e)` | Result が Err の場合、エラー値を束縛 |
| OR パターン | `1 \| 2 \| 3` | いずれかにマッチ |

### guard 節

`pattern if condition:` の形式でガード条件を指定できる。パターンがマッチし、かつガード条件が真の場合にのみアームが実行される。

### OR パターン

複数のパターンを `|` で結合し、いずれかにマッチさせることができます。変数束縛（`n`、`Some(x)`、`Ok(v)`、`Err(e)`）は OR パターン内では使用できません。

```python
case x:
    1 | 2 | 3:
        print("small")
    _:
        print("other")

# enum の OR パターン
case color:
    Color::Red | Color::Blue:
        print("warm or cool")
    Color::Green:
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
# enum のパターンマッチ
enum Color:
    Red
    Green
    Blue

case color:
    Color::Red:
        print("red")
    Color::Green:
        print("green")
    Color::Blue:
        print("blue")

# Option のパターンマッチ
x: Option<int> = Some(42)
case x:
    Some(v):
        print(v)
    None:
        print("nothing")

# Result のパターンマッチ
function divide(a: int, b: int) -> Result<int, Error>:
    if b == 0:
        return Err(Error("division by zero"))
    return Ok(a // b)

case divide(10, 2):
    Ok(v):
        print(v)         # 5
    Err(e):
        print(e.message)

# リテラルのパターンマッチ
case x:
    0:
        print("zero")
    1:
        print("one")
    _:
        print("other")

# guard 節
case x:
    n if n > 0:
        print("positive")
    n if n < 0:
        print("negative")
    _:
        print("zero")
```

### ADT enum のパターンマッチ

enum バリアントが関連データを持つ場合、バインディングパターンを使って値を取り出します。

```python
enum Shape:
    Circle(float)
    Rectangle(float, float)
    Point

s = Shape::Circle(3.14)
case s:
    Shape::Circle(r):
        print(r)        # 3.14
    Shape::Rectangle(w, h):
        print(w)
        print(h)
    Shape::Point:
        print("point")
```

複数フィールドを持つバリアントは、宣言順に各フィールドを別々の名前に束縛します。

### 式の形

`case:` と `case <expr>:` のいずれも、各アームの `:` を `=>` に置き換えることで式として使えます。各アームは単一の式を提供し、その値が結果になります。

```python
# 対象なしの case 式
label = case:
    x > 100 => "huge"
    x > 10  => "big"
    x > 0   => "small"
    _       => "non-positive"
```

パターンマッチング式の形式:

#### 構文

```python
result = case expression:
    pattern => value_expression
    pattern if guard => value_expression
    _ => default_value
```

case 文でサポートされるすべてのパターンは case 式でもサポートされます: リテラル、変数束縛、enum、ADT enum、`Some`/`None`、`Ok`/`Err`、OR パターン、guard、ワイルドカード。

case 式は網羅的でなければなりません（case 文と同じルール）。

#### 例

```python
# Option
value = case opt:
    Some(v) => v
    None    => 0

# Enum
label = case direction:
    Direction::North => "N"
    Direction::South => "S"
    Direction::East  => "E"
    Direction::West  => "W"

# Guard
grade = case score:
    n if n >= 90 => "A"
    n if n >= 80 => "B"
    _            => "F"

# OR パターン
kind = case x:
    1 | 2 | 3 => "small"
    _          => "large"

# ADT enum
area = case shape:
    Shape::Circle(r)  => 3.14 * r * r
    Shape::Rect(w, h) => w * h
    Shape::Point      => 0.0
```

### スコープルール

- 各 `case` アームはブロックスコープを持つ。
- 変数束縛パターン (`n`)、`Some(x)`、`Ok(v)`、`Err(e)` で束縛された変数はそのアーム内でのみ有効。

---

## スコープルール

### ブロックスコープ

- `if` / `else` / `while` / `for` / `case` の各ブロックはブロックスコープを持つ。
- ブロック内で宣言した変数はブロックの終了と同時にスコープから外れる。

```python
for i in range(3):
    tmp = i * 2
# tmp はここではアクセス不可

if true:
    a = 1
# a はここではアクセス不可
```

### 内側スコープでの再代入

- 内側のスコープで変数に代入すると、外側の変数が変更される（Python スタイルのスコーピング）。
- シャドーイングは行われず、内側の代入は同じ変数を変更する。

```python
x = 10
if true:
    x = 99   # 外側の x を変更
    print(x)     # 99
print(x)         # 99
```
