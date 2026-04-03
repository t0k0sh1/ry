[English](../../tutorial/05-functions.md) | [日本語](05-functions.md) | [繁體中文](../../zh/tutorial/05-functions.md)

# 関数

[<- 前: 制御構文](04-control-flow.md) | [次: Record と列挙型 ->](06-records.md)

---

## 基本的な関数定義

関数は `function` キーワードで定義します。引数の型は `name: type` の形式で宣言します。型を省略した場合、デフォルトで `any` になります。戻り値の型は `->` の後に指定します。

```python
function add(a: int, b: int) -> int:
    return a + b
```

- 引数の型宣言を推奨します。省略した場合、型は `any` にデフォルト設定されます。
- 戻り値型は `->` の後に指定します。
- `return` 文で値を返します。

---

## 関数の呼び出し

定義した関数は名前と引数を指定して呼び出します。

```python
function multiply(x: int, y: int) -> int:
    return x * y

result = multiply(3, 4)
print(result)   # 12
```

---

## 再帰関数

関数は自分自身を呼び出せます（再帰）。

```python
function factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)

print(factorial(5))   # 120
print(factorial(0))   # 1
```

---

## 関数オーバーロード

引数の数や型が異なる同名の関数を複数定義できます。

```python
function add(a: int, b: int) -> int:
    return a + b

function add(a: float, b: float) -> float:
    return a + b

print(add(1, 2))       # 3
print(add(1.5, 2.5))   # 4
```

呼び出し時の引数の型に応じて、適切な関数が自動的に選択されます。

> **注意**: 引数の型が同一で戻り値の型だけが異なる関数を定義するとコンパイルエラーになります。

---

## 戻り値型の省略（Unit 型）

戻り値が不要な関数は `->` を省略できます。この場合、関数は Unit 型を返します。

```python
function greet():
    print(42)

greet()   # 42
```

引数なし・戻り値なしの最もシンプルな関数形式です。

---

## デフォルト引数

引数にデフォルト値を設定できます。呼び出し側がその引数を省略すると、デフォルト値が使われます。

```python
function greet(name: str, greeting: str = "Hello") -> str:
    return f"{greeting}, {name}"

print(greet("Alice"))             # Hello, Alice
print(greet("Bob", "Good morning"))  # Good morning, Bob
```

複数のデフォルトパラメータも使えます:

```python
function connect(host: str, port: int = 8080, timeout: int = 30) -> str:
    return f"{host}:{port} (timeout={timeout})"

print(connect("localhost"))              # localhost:8080 (timeout=30)
print(connect("localhost", 3000))        # localhost:3000 (timeout=30)
print(connect("localhost", 3000, 10))    # localhost:3000 (timeout=10)
```

> **なぜデフォルト引数なのか?** 一般的なケースではシンプルな呼び出しを保ちつつ、必要に応じてカスタマイズできます -- 複数のオーバーロードが不要になります。
> **注意**: デフォルト値のある引数は、デフォルト値のない引数の後に配置する必要があります。

---

## ラムダ関数

ラムダ関数は式として関数を記述できます。単一式のラムダは `(parameters) => expression` の形式を、ブロックラムダは `(parameters):` の後にインデントされたブロックを続けます。どちらの場合も戻り値の型は自動的に推論されます。

### 単一式ラムダ

```python
double = (x: int) => x * 2
print(double(5))  # 10

add = (a: int, b: int) => a + b
print(add(3, 4))  # 7
```

### 引数なしラムダ

```python
answer = () => 42
print(answer())  # 42
```

### 複数行ラムダ

`:` の後に改行とインデントを追加して複数の文を記述できます。

```python
abs = (x: int):
    if x < 0:
        return -x
    return x

print(abs(-5))  # 5
print(abs(3))   # 3
```

> **なぜラムダなのか?** 短い使い捨ての関数に最適です -- 特に `filter` や `map` のような高階関数の引数として使います（後述）。

---

## クロージャ

ラムダ関数は定義されたスコープの変数をキャプチャできます。関数とそのキャプチャされた環境の組み合わせを**クロージャ**と呼びます。

```python
offset = 10
add_offset = (x: int) => x + offset
print(add_offset(5))  # 15
```

クロージャは変数を**値によって**キャプチャします -- クロージャ作成後に元の変数を変更しても、クロージャのコピーには影響しません。

```python
base = 10
f = (x: int) => x + base
base = 999
print(f(1))  # 11（キャプチャされた値 10 を使用）
```

これは双方向に作用します -- クロージャ内の変更も外側の変数に影響しません:

```python
counter = 0
items = [1, 2, 3]
items.map((x: int):
    counter += x    # クロージャのローカルコピーのみ変更
    return x
)
print(counter)  # 0（外側の変数は変更されない）
```

> **なぜ値によるキャプチャなのか?** 安全性と予測可能性を保証します -- クロージャ内の変更を心配せずに、現在のスコープだけを見て変数の値を常に推論できます。
> **なぜクロージャなのか?** 関数をその場で特殊化できます。例えば、1つのテンプレートから加算関数のファミリーを作成できます。

---

## 高階関数

他の関数を引数として取る関数を定義できます。関数型は `function(parameter_types) -> return_type` と記述します。

```python
function apply(f: function(int) -> int, x: int) -> int:
    return f(x)

double = (x: int) => x * 2
print(apply(double, 3))                # 6
print(apply((n: int) => n + 1, 10))  # 11
```

### 値としての関数

名前付き関数も変数に束縛したり引数として渡したりできます -- ラムダと同じように動作します。

```python
function square(x: int) -> int:
    return x * x

# 名前付き関数を引数として渡す
print(apply(square, 4))  # 16

# 変数に束縛
sq = square
print(sq(5))  # 25
```

> **なぜ高階関数なのか?** **何を**するかと**どのように**するかを分離できます。同じ `apply` 関数がどんな変換にも使え、コードの再利用性が高まります。[コレクション](07-collections.md)の `filter`、`map`、`reduce` でこのパターンを既に見ています。

---

## UFCS（統一関数呼び出し構文）

UFCS を使うと、`f(a, b)` を `a.f(b)` と書けます。最初の引数がドットの前に移動し、メソッドチェーンスタイルが可能になります。

```python
function add(a: int, b: int) -> int:
    return a + b

x = 1
print(x.add(2))   # add(x, 2) -> 3
```

### チェーン呼び出し

UFCS は複数の呼び出しをチェーンする際に真価を発揮します -- 内側から外側へではなく、左から右へ読めるようになります:

```python
function double(n: int) -> int:
    return n * 2

# チェーン（自然に読める: "x を取り、2 を足して、2倍する"）
print(x.add(2).double())   # 6

# 同等のネストされた呼び出し（読みにくい）
print(double(add(x, 2)))   # 6
```

> **なぜ UFCS なのか?** 深くネストされた関数呼び出しを読みやすい左から右へのパイプラインに変えます。`xs.iter().filter(...).map(...).to_list()` のようなイテレータチェーンで既にこれを見ています。

---

## 演習

1. **デフォルト引数**: `format_price(amount: int, currency: str = "USD", decimals: int = 2) -> str` 関数を書いて、価格をフォーマットしてください。`format_price(42)` と `format_price(42, "EUR")` の両方が動作することを確認してください。

2. **高階関数**: `apply_twice(f: function(int) -> int, x: int) -> int` 関数を書いて、`f` を `x` に2回適用してください（つまり `f(f(x))`）。`(x: int) => x + 1` でテストし、`apply_twice((x: int) => x + 1, 5)` が `7` を返すことを確認してください。

3. **UFCS チェーン**: `inc(n: int) -> int`（1を加算）と `triple(n: int) -> int`（3倍にする）を定義してください。UFCS を使って `5.inc().triple()` と書き、結果が `18` であることを確認してください。

---

[<- 前: 制御構文](04-control-flow.md) | [次: Record と列挙型 ->](06-records.md)
