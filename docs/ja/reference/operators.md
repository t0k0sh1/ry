[English](../../reference/operators.md) | [日本語](operators.md) | [繁體中文](../../zh/reference/operators.md)

# 演算子リファレンス

## 優先順位テーブル

優先順位は数字が小さいほど高い（先に評価される）。

| 優先順位 | 演算子 | 説明 | 結合性 |
|---|---|---|---|
| 0 | `?` `!!` | エラー伝播（後置） | 左 |
| 1 | `()` | グループ化 | — |
| 2 | `+x` `-x` `~x` | 単項正・負、ビット NOT | 右 |
| 3 | `**` | 累乗 | 右 |
| 3.5 | `as` | 型キャスト | 左 |
| 4 | `*` `/` `%` `//` | 乗算・除算・剰余・整数除算 | 左 |
| 5 | `+` `-` | 加算・減算 | 左 |
| 6 | `<<` `>>` `>>>` | ビットシフト | 左 |
| 7 | `&` | ビット AND | 左 |
| 8 | `^` | ビット XOR | 左 |
| 9 | `\|` | ビット OR | 左 |
| 10 | `==` `!=` `<` `<=` `>` `>=` `in` `not in` | 比較・所属 | 左 |
| 11 | `not` | 論理 NOT | 右 |
| 12 | `and` | 論理 AND | 左 |
| 13 | `or` | 論理 OR | 左 |
| 13.5 | `??` | null 合体 | 左 |

## 算術演算子

| 演算子 | 説明 | 例 |
|---|---|---|
| `+` | 加算 / 文字列結合 | `1 + 2` -> `3`、`"a" + "b"` -> `"ab"`、`"x" + 1` -> `"x1"` |
| `-` | 減算 | `5 - 3` -> `2` |
| `*` | 乗算 / 文字列繰り返し | `4 * 3` -> `12`、`"ab" * 3` -> `"ababab"` |
| `/` | 除算（常に float） | `7 / 2` -> `3.5` |
| `//` | 整数除算（-∞ 方向への切り捨て） | `7 // 2` -> `3`、`-7 // 2` -> `-4` |
| `%` | 剰余 | `7 % 3` -> `1` |
| `**` | 累乗（常に float） | `2 ** 10` -> `1024.0` |
| `-x` | 単項マイナス | `-5`, `-3.14` |
| `+x` | 単項プラス | `+5`（符号変更なし） |

```python
a = 10 // 3    # 3 (int)
b = 10 / 3     # 3.3333... (float)
c = 2 ** 8     # 256.0 (float)
s = "foo" + "bar"  # "foobar"
t = "val=" + 42    # "val=42"
u = 3.14 + "!"    # "3.14!"
```

`+` の一方のオペランドが `str` で他方が `int`、`float`、`bool` の場合、非 `str` オペランドは自動的にその文字列表現に変換されて結合されます。

## 比較演算子

すべて `bool` を返す。

| 演算子 | 説明 |
|---|---|
| `==` | 等しい |
| `!=` | 等しくない |
| `<` | より小さい |
| `<=` | 以下 |
| `>` | より大きい |
| `>=` | 以上 |

- 数値型（int / float）とbool に対して使用可能。
- `str` 同士は辞書順（バイト順）で比較。
- レコード型はフィールドごとの自動比較で `==` と `!=` をサポート（[構造体リファレンス](structs.md#比較--)参照）。
- `in` 演算子はセット、リスト、マップに対する所属チェックに使用（`x in s`）。
- `not in` 演算子は `in` の否定（`x not in s`）。
- マップの場合、`in` はキーの存在を確認します。

```python
x = 3 < 5       # true
y = "abc" < "abd"  # true（辞書順）
s = {1, 2, 3}
z = 2 in s      # true
w = 4 not in s  # true
xs = [1, 2, 3]
a = 2 in xs     # true（リスト線形探索）
m = {"a": 1}
b = "a" in m    # true（マップキー検索）
```

## 論理演算子

| 演算子 | 説明 | 型 |
|---|---|---|
| `and` | 論理 AND | `bool` x `bool` -> `bool` |
| `or` | 論理 OR | `bool` x `bool` -> `bool` |
| `not` | 論理 NOT | `bool` -> `bool` |

```python
a = true and false   # false
b = true or false    # true
c = not true         # false
```

## ビット演算子

`int` 型のみ使用可能。`float` や `bool` に適用するとコンパイルエラー。

| 演算子 | 説明 | 例 |
|---|---|---|
| `&` | ビット AND | `0b1100 & 0b1010` -> `0b1000` |
| `\|` | ビット OR | `0b1100 \| 0b1010` -> `0b1110` |
| `^` | ビット XOR | `0b1100 ^ 0b1010` -> `0b0110` |
| `~` | ビット NOT（単項） | `~0` -> `-1` |
| `<<` | 左シフト | `1 << 4` -> `16` |
| `>>` | 算術右シフト | `16 >> 2` -> `4` |
| `>>>` | 論理右シフト | `-1 >>> 1` -> `9223372036854775807` |

```python
flags = 0b0001 | 0b0010   # 3
masked = flags & 0b0011   # 3
shifted = 1 << 8          # 256
```

## エラー伝播演算子（`?` / `!!`）

後置 `?` 演算子は happy path で `Result` や `Option` 値をアンラップし、unhappy path では短絡します。`!!` 演算子は `?` のエイリアスで、同一のセマンティクスを持ちます。

| オペランド | happy path | unhappy path |
|---|---|---|
| `Result<T, E>` | `Ok` の内側の値 `v` に評価される | 外側の関数から `Err(e)` を返す |
| `Option<T>` | `Some` の内側の値 `v` に評価される | 外側の関数から `None` を返す |

関数内部で使う場合、オペランドの型は外側の関数の戻り値型に一致する必要があります:

- `Result` 値に対する `?` は、外側の関数が `Result` を返す必要があります。
- `Option` 値に対する `?` は、外側の関数が `Option` を返す必要があります。

```python
function safe_divide(a: int, b: int) -> Result<int, Error>:
    if b == 0:
        return Err(Error("division by zero"))
    return Ok(a // b)

function compute(a: int, b: int, c: int) -> Result<int, Error>:
    x = safe_divide(a, b)?    # b == 0 の場合は Err を早期リターン
    y = safe_divide(x, c)!!
    return Ok(y + 1)

function safe_get(xs: List<int>, i: int) -> Option<int>:
    if i < 0 or i >= xs.length():
        return none
    return Some(xs[i])

function first_plus_second(xs: List<int>) -> Option<int>:
    a = safe_get(xs, 0)?    # 範囲外なら None を早期リターン
    b = safe_get(xs, 1)?
    return Some(a + b)
```

### トップレベルでの使用

`?` と `!!` はスクリプトのトップレベルでも直接使えます。トップレベルでは、`Err(e)` と `None` は致命的エラーとして扱われ、エラーメッセージが stderr に書き出され、プロセスがステータス `1` で終了します。

```python
function mk() -> Result<int, Error>:
    return Err(Error("something broke"))

v = mk()?   # "error: something broke" を stderr に出力してステータス 1 で終了

x: int? = none
y = x?      # "error: unexpected None" を stderr に出力してステータス 1 で終了
```

トップレベルでは、`Result` の `Err` 型は `Error` でなければなりません（`message` フィールドを出力できるようにするため）。

---

## `case:` 条件式

```python
x = case:
    condition => true_value
    _ => false_value
```

上から順に条件を評価し、最初に真になったアームの式を返します。すべての結果式は同じ型でなければなりません。`_ =>` ワイルドカードアームは必須なので、式は常に値を生成します。

```python
x = case:
    3 > 2 => 10
    _ => 20     # 10

s = case:
    false => "yes"
    _ => "no"  # "no"

# ネストされた三項演算は複数のアームにフラット化される
score = 85
y = case:
    score >= 90 => 3
    score >= 80 => 2
    _ => 1         # 2
```

---

## 範囲演算子

`..` 演算子は両端を含む整数の範囲を生成します。

```python
xs = 1 .. 5    # [1, 2, 3, 4, 5]

for i in 1 .. 3:
    print(i)       # 1 2 3
```

結果は左オペランドから右オペランドまで（両端含む）のすべての整数を含む `List<int>` です。

---

## null 合体演算子（`??`）

```python
x = optional_val ?? default_val
```

`??` 演算子は、左辺に `Option<T>` または `Result<T, E>` を受け付けます:

| 左辺 | 結果 |
|---|---|
| `Some(v)` | `v` |
| `None` | `default_val` |
| `Ok(v)` | `v` |
| `Err(_)` | `default_val`（エラー値は破棄される） |

右辺のオペランドは `Option` の内部型（または `Result` の `Ok` 型）と同じ型でなければなりません。

```python
a: int? = Some(10)
b: int? = none

print(a ?? 0)    # 10
print(b ?? 0)    # 0

function parse_int(s: str) -> Result<int, Error>:
    # ...

i = parse_int("42") ?? -1      # 成功なら 42、Err なら -1
j = parse_int("nope") ?? -1    # -1 -- Err 値は破棄される
```

---

## 複合代入演算子

変数を更新するショートハンド。`x op= y` は `x = x op y` と等価。

| 演算子 | 等価な式 |
|---|---|
| `x += y` | `x = x + y` |
| `x -= y` | `x = x - y` |
| `x *= y` | `x = x * y` |
| `x /= y` | `x = x / y` |
| `x %= y` | `x = x % y` |
| `x //= y` | `x = x // y` |
| `x **= y` | `x = x ** y` |
| `x &= y` | `x = x & y` |
| `x \|= y` | `x = x \| y` |
| `x ^= y` | `x = x ^ y` |
| `x <<= y` | `x = x << y` |
| `x >>= y` | `x = x >> y` |

```python
x = 10
x += 5    # x = 15
x -= 3    # x = 12
x *= 2    # x = 24
x //= 3  # x = 8
x &= 6   # x = 0
```

複合代入は任意の lvalue に対して許可されています -- 通常の変数、リスト
やマップの要素、record のフィールド、任意のネストしたチェーンに使えます:

```python
xs = [1, 2, 3]
xs[0] += 10              # リスト要素

record Point:
  x: int
  y: int
p = Point(1, 2)
p.x *= 5                 # record フィールド

pts = [Point(1, 2), Point(3, 4)]
pts[0].x -= 1            # チェーン: リスト中の record フィールド
```

チェーン LHS 上の各インデックス式はちょうど 1 回だけ評価されます。存在
しないマップキーに対する複合代入（`m["absent"] += 1`）はランタイムエラー
になります。

## インクリメント・デクリメント演算子

変数を 1 増減させるポストフィックス演算子。ステートメントとしてのみ使用可能。内部的にはそれぞれ `x = x + 1`、`x = x - 1` にデシュガーされる。

| 演算子 | 等価な式 |
|---|---|
| `x++` | `x = x + 1` |
| `x--` | `x = x - 1` |

```python
count = 0
count++       # count = 1
count++       # count = 2
count--       # count = 1

f = 1.5
f++           # f = 2.5（int 1 が float に型昇格）
```

> **注意**: `++` / `--` はステートメントとしてのみ使用でき、式の中では使えません。
> `@const` 変数にはインクリメント・デクリメントを適用できません（不変性が強制されます）。

---

## 演算の型規則

| 演算 | 左辺型 | 右辺型 | 結果型 |
|---|---|---|---|
| `+ - *` | int | int | int |
| `+ - *` | float | int / float | float |
| `+ - *` | int | float | float |
| `/` | 任意の数値 | 任意の数値 | float |
| `//` | int | int | int |
| `//` | float または int（片方 float） | -- | float |
| `**` | 任意の数値 | 任意の数値 | float |
| `%` | int | int | int |
| `%` | float または int（片方 float） | -- | float |
| `+` | str | str | str |
| `+` | str | int / float / bool | str |
| `+` | int / float / bool | str | str |
| `== != < <= > >=` | 数値 / bool / str | 同型 | bool |
| `*` | str | int | str |
| `in` | 任意 | Set<T> / List<T> / Map<K, V> | bool |
| `not in` | 任意 | Set<T> / List<T> / Map<K, V> | bool |
| `& \| ^ ~ << >> >>>` | int | int | int |
| `and or not` | bool | bool | bool |

## 演算子オーバーロード

ユーザー定義型に対して演算子の動作を定義できます。

### 構文

```python
# 二項演算子（引数2個）
function operator+(a: MyType, b: MyType) -> MyType:
    ...

# 単項演算子（引数1個）
function operator-(a: MyType) -> MyType:
    ...
```

### オーバーロード可能な演算子一覧

| 種別 | 演算子 |
|---|---|
| 算術（二項） | `+` `-` `*` `/` `%` `**` `//` |
| 比較（二項） | `==` `!=` `<` `<=` `>` `>=` |
| ビット（二項） | `&` `\|` `^` `<<` `>>` `>>>` |
| 論理（二項） | `and` `or` |
| 所属 | `in` |
| 添字 | `[]`（読み取り）、`[]=`（書き込み） |
| 呼び出し | `()` |
| キャスト | `as` |
| 単項 | `-` `~` `not` |
| 複合代入 | `+=` `-=` `*=` `/=` `%=` `//=` `**=` `&=` `\|=` `^=` `<<=` `>>=` |

### 戻り値型の制約

比較演算子と論理演算子は `bool` を返す必要があります:

| 種別 | 演算子 | 必須戻り値型 |
|---|---|---|
| 比較 | `==` `!=` `<` `<=` `>` `>=` | `bool` |
| 論理 | `and` `or` `not` | `bool` |
| 所属 | `in` | `bool` |
| キャスト | `as` | 必須（ターゲット型） |

```python
# OK
function operator==(a: Vec2, b: Vec2) -> bool:
    return a.x == b.x and a.y == b.y

# エラー: comparison operator '==' must return 'bool', but returns 'int'
function operator==(a: Vec2, b: Vec2) -> int:
    return 42
```

算術演算子・ビット演算子には戻り値型の制約はありません。

### 二項 / 単項の区別

引数の個数で区別します。

```python
# 二項 -
function operator-(a: Vec2, b: Vec2) -> Vec2:
    return Vec2(a.x - b.x, a.y - b.y)

# 単項 -
function operator-(v: Vec2) -> Vec2:
    return Vec2(-v.x, -v.y)
```

### 複合代入演算子のオーバーロード

複合代入演算子（`+=`、`-=` 等）は個別にオーバーロードできます。大きなデータ構造のインプレース最適化を可能にします。

```python
record Matrix:
    data: List
    rows: int
    cols: int

function operator+=(a: Matrix, b: Matrix) -> Matrix:
    for i in range(len(a.data)):
        a.data[i] = a.data[i] + b.data[i]
    return a
```

#### 解決優先順位

`x += y` が評価される際:

1. `operator+=` がその型に定義されている場合 → 直接呼び出す
2. `operator+=` が未定義で `operator+` がある場合 → `x = x + y` にフォールバック
3. どちらも未定義の場合（組み込み型以外） → コンパイルエラー

```python
record Vec2:
    x: float
    y: float

function operator+=(a: Vec2, b: Vec2) -> Vec2:
    return Vec2(a.x + b.x, a.y + b.y)

v = Vec2(1.0, 2.0)
v += Vec2(3.0, 4.0)  # operator+= を直接呼び出す
# v.x == 4.0, v.y == 6.0
```

複合代入演算子は引数が正確に 2 つ必要で、戻り値型の制約はありません。

### 添字演算子のオーバーロード

`[]`（読み取り）と `[]=`（書き込み）演算子でユーザー定義型にカスタムの添字アクセスを定義できます。複数インデックスアクセス（例: `m[row, col]`）もサポートされています。

```python
record Grid:
    a: int
    b: int
    c: int
    d: int

# 読み取り: 2個以上のパラメータ（オブジェクト + インデックス）が必要
function operator[](g: Grid, row: int, col: int) -> int:
    if row == 0 and col == 0:
        return g.a
    if row == 0 and col == 1:
        return g.b
    if row == 1 and col == 0:
        return g.c
    return g.d

# 書き込み: 3個以上のパラメータ（オブジェクト + インデックス + 値）が必要
function operator[]=(g: Grid, row: int, col: int, value: int):
    ...

g = Grid(1, 2, 3, 4)
print(g[0, 1])    # 2
g[1, 0] = 99
```

ユーザー定義の添字演算子が最初に試行され、一致しない場合は組み込みの添字動作（リスト、マップ、配列）がフォールバックとして使用されます。

### 所属演算子のオーバーロード

`in` 演算子をオーバーロードしてカスタムの所属チェックを定義できます。`bool` を返す必要があります。

```python
record Range:
    lo: int
    hi: int

function operator in(value: int, r: Range) -> bool:
    return value >= r.lo and value < r.hi

r = Range(1, 10)
print(5 in r)       # true
print(15 not in r)  # true
```

ユーザー定義の `in` 演算子が最初に試行され、一致しない場合は組み込みの動作（セット、マップ、リスト）がフォールバックとして使用されます。`in` が定義されていれば `not in` は自動的にサポートされます。

### 呼び出し演算子のオーバーロード

`()` 演算子でレコードを呼び出し可能なオブジェクトとして動作させることができます。2個以上のパラメータ（オブジェクト + 引数）が必要です。

```python
record Adder:
    base: int

function operator()(a: Adder, x: int) -> int:
    return a.base + x

add5 = Adder(5)
print(add5(10))    # 15
```

レコード値を保持する変数を関数のように呼び出すと、コンパイラはまず `operator()` のオーバーロードを試みます。一致しない場合は、他の呼び出し解決戦略（関数、コンストラクタ、ラムダ）が優先されます。

### キャスト演算子のオーバーロード

`as` 演算子をオーバーロードしてカスタムの型変換を定義できます。パラメータは正確に 1 つ（ソース値）で、戻り値型（ターゲット型）の指定が必要です。ソース型と戻り値型でディスパッチされます。

```python
record Celsius:
    value: int

record Fahrenheit:
    value: int

function operator as(c: Celsius) -> Fahrenheit:
    return Fahrenheit(c.value * 9 // 5 + 32)

c = Celsius(100)
f = c as Fahrenheit   # Fahrenheit(212)
```

ターゲット型はコンパイラが解決できる任意の型で、ジェネリック型を含みます:

```python
record Temperature:
    value: int

function operator as(t: Temperature) -> int?:
    return Some(t.value)

t = Temperature(42)
result: int? = t as int?   # Some(42)
```

ユーザー定義の `as` 演算子が最初に試行され、一致しない場合は組み込みのキャスト（int、float、bool、str 等）がフォールバックとして使用されます。
