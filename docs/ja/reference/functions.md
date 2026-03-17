[English](../../reference/functions.md) | [日本語](functions.md) | [繁體中文](../../zh/reference/functions.md)

# 関数リファレンス

## 関数定義の構文

```python
fn 関数名(引数名: 型, ...) -> 戻り値型:
    # 本体
    return 値
```

- 引数の型は必須。
- 戻り値型は省略可能（省略時は `Unit`）。
- 本体はインデントされたブロック。
- 関数には `require`（事前条件）と `ensure`（事後条件）を定義できます。[契約による設計](contracts.md) を参照。

```python
fn add(a: int, b: int) -> int:
    return a + b

fn greet(name: str):
    print("Hello, " + name)   # 戻り値型は Unit
```

---

## 引数と戻り値の型

| 項目 | 説明 |
|---|---|
| 引数型 | 必須。すべての引数に型アノテーションが必要 |
| 戻り値型 | 省略可能。省略時は `Unit`（void相当） |
| `Unit` | 値を返さない関数の戻り値型 |

```python
fn no_return(x: int):      # 戻り値型 Unit（省略）
    print(x)

fn get_value() -> int:     # 戻り値型 int
    return 42
```

---

## 再帰

関数は自身を呼び出せる。

```python
fn factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)
```

---

## オーバーロード

引数の数や型が異なる同名の関数を複数定義できる。

### ルール

- 引数の数または型が異なれば同名の関数を定義可能。
- 呼び出し時は引数の型と数に基づいて適切な関数が選択される。
- 戻り値型だけが異なるオーバーロードはできない。

```python
fn area(side: int) -> int:
    return side * side

fn area(w: int, h: int) -> int:
    return w * h

let a = area(5)       # 25
let b = area(3, 4)    # 12
```

---

## Unit 型関数

戻り値のない関数は `Unit` を返す。戻り値型は省略可能。

```python
fn log(msg: str):
    print(msg)

fn log_typed(msg: str) -> Unit:
    print(msg)
```

---

## ラムダ関数

無名関数をその場で定義できる。

### 構文

```python
# 単一式（式の値が返る。戻り値型は推論）
fn(引数名: 型, ...): 式

# 複数行ブロック
fn(引数名: 型, ...):
    # 複数の文
    return 値

# 戻り値型の明示（省略可能）
fn(引数名: 型, ...) -> 戻り値型: 式
```

### 例

```python
let double = fn(x: int): x * 2
let result = double(5)   # 10

let add = fn(a: int, b: int): a + b
let sum = add(3, 4)      # 7

# 複数行ラムダ
let abs = fn(x: int):
    if x < 0:
        return -x
    return x
```

---

## クロージャ

ラムダ関数は定義された時点の外側スコープの変数を**値でキャプチャ**する。

```python
let base = 10
let add_base = fn(x: int): x + base   # base を値でキャプチャ

base = 99          # キャプチャ済みの値には影響しない
let r = add_base(5)   # 15（キャプチャ時の base = 10 を使用）
```

### キャプチャルール

| 項目 | 内容 |
|---|---|
| キャプチャ方式 | 値キャプチャ（コピー） |
| キャプチャタイミング | ラムダ定義時 |
| 外側変数の変更の影響 | ない（コピーのため） |

---

## 関数型

関数を値として扱うための型。

### 構文

```python
fn(引数型1, 引数型2, ...) -> 戻り値型
```

### 例

```python
let f: fn(int) -> int = fn(x: int): x * 2
let g: fn(int, int) -> int = fn(a: int, b: int): a + b

fn apply(func: fn(int) -> int, x: int) -> int:
    return func(x)

let result = apply(f, 5)   # 10
```

---

## 高階関数

関数を引数として受け取ったり、戻り値として返したりできる。

```python
fn map_list(xs: List<int>, f: fn(int) -> int) -> List<int>:
    let result: List<int> = []
    for x in xs:
        result += [f(x)]
    return result

let doubled = map_list([1, 2, 3], fn(x: int): x * 2)
# [2, 4, 6]
```

---

## UFCS（統一関数呼び出し構文）

`a.f(b)` の形式で `f(a, b)` を呼び出せる。メソッドチェーンに使いやすい。

### 構文

```python
# 通常呼び出し
f(a, b)

# UFCS 呼び出し（等価）
a.f(b)
```

### チェーン

```python
fn double(x: int) -> int:
    return x * 2

fn add_one(x: int) -> int:
    return x + 1

let result = 5.double().add_one()   # double(5) → 10, add_one(10) → 11
```

### フィールドアクセスとの混在

フィールドアクセス（`.field`）と UFCS（`.method()`）は同じドット記法で書けるが、引数の有無で区別される。

```python
let p = Point(3, 4)
let len = p.x.to_float()   # フィールドアクセス + UFCS
```

---

## 演算子オーバーロード

ユーザー定義型に対して演算子の振る舞いを定義できる。

### 構文

```python
# 二項演算子（引数2個）
fn operator<op>(a: 型, b: 型) -> 戻り値型:
    ...

# 単項演算子（引数1個）
fn operator<op>(a: 型) -> 戻り値型:
    ...
```

### オーバーロード可能な演算子

| 種別 | 演算子 |
|---|---|
| 算術（二項） | `+` `-` `*` `/` `%` `**` `//` |
| 比較（二項） | `==` `!=` `<` `<=` `>` `>=` |
| ビット（二項） | `&` `\|` `^` `<<` `>>` |
| 論理（二項） | `and` `or` |
| 単項 | `-` `~` `not` |

### 二項 / 単項の区別

引数の個数で区別する。

```python
type Vec2:
    x: float
    y: float

# 二項 +
fn operator+(a: Vec2, b: Vec2) -> Vec2:
    return Vec2(a.x + b.x, a.y + b.y)

# 単項 -
fn operator-(v: Vec2) -> Vec2:
    return Vec2(-v.x, -v.y)

# 比較
fn operator==(a: Vec2, b: Vec2) -> bool:
    return a.x == b.x and a.y == b.y

let v1 = Vec2(1.0, 2.0)
let v2 = Vec2(3.0, 4.0)
let v3 = v1 + v2    # Vec2(4.0, 6.0)
let v4 = -v1        # Vec2(-1.0, -2.0)
```
