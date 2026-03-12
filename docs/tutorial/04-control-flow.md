# 制御構文

[← 前: 演算子](03-operators.md) | [次: 関数 →](05-functions.md)

---

## if / elif / else

条件に応じて処理を分岐させるには `if` を使います。

```python
let x = 10

if x > 0:
    print(x)
elif x == 0:
    print(0)
else:
    print(-1)
```

- `elif` と `else` は省略できます。
- 条件式には `bool` 以外も指定できます。`int` の場合、`0` が偽、非 `0` が真として扱われます。
- `if` はネストできます。

```python
let a = 5
let b = 3

if a > 0:
    if b > 0:
        print(a + b)   # 8
```

---

## while ループ

条件が真である間、ブロックを繰り返し実行します。

```python
let i = 3
while i > 0:
    print(i)
    i = i - 1
# 3
# 2
# 1
```

---

## for ループと range

リストまたは `range` を使ってイテレーションできます。

```python
for x in [1, 2, 3]:
    print(x)
# 1
# 2
# 3
```

`range(n)` は `0` から `n - 1` までの整数を生成します。

```python
for i in range(5):
    print(i)
# 0
# 1
# 2
# 3
# 4
```

`range(start, end)` は `start` から `end - 1` までの整数を生成します。

```python
for i in range(2, 5):
    print(i)
# 2
# 3
# 4
```

---

## break と continue

`break` はループを即座に抜けます。`continue` は現在の反復をスキップして次の反復へ進みます。

```python
for i in range(10):
    if i == 5:
        break
    if i % 2 == 0:
        continue
    print(i)
# 1
# 3
```

`while` でも同様に使用できます。

```python
let n = 0
while true:
    n = n + 1
    if n % 2 == 0:
        continue
    if n > 7:
        break
    print(n)
# 1
# 3
# 5
# 7
```

> **注意**: ネストしたループの中では、`break` / `continue` は最も内側のループにのみ作用します。ループ外で使用するとコンパイルエラーになります。

---

## ネストの例

`for` と `while` はネストできます。

```python
for i in range(1, 4):
    for j in range(1, 4):
        if j == 2:
            continue
        print(i * 10 + j)
# 11
# 13
# 21
# 23
# 31
# 33
```

---

## スコープのルール

制御構文のブロックはスコープを持ちます。

### ブロックスコープ

ブロック内で宣言した変数はブロック外から参照できません。

```python
if true:
    let inner = 42
# ここで inner を参照するとコンパイルエラー
```

### 外側の変数への参照・再代入

ブロック内から外側の変数を参照・再代入できます。

```python
let count = 0
for i in range(5):
    count = count + i
print(count)   # 10
```

### シャドーイング

外側と同名の変数をブロック内で宣言すると、ブロック内ではその新しい変数が使われます（シャドーイング）。外側の変数は変化しません。

```python
let x = 1
if true:
    let x = 99
    print(x)   # 99
print(x)       # 1
```

---

## match

`match` は値に応じた分岐を行う構文です。enum や Option を安全に処理できます。

```python
enum Color:
    Red
    Green
    Blue

let c = Color::Green
match c:
    case Color::Red:
        print("red")
    case Color::Green:
        print("green")
    case Color::Blue:
        print("blue")
# green
```

### Option のマッチ

`unwrap()` の代わりに `match` を使うことで、`None` の場合も安全に処理できます。

```python
let x: Option<int> = Some(42)
match x:
    case Some(v):
        print(v)
    case None:
        print("nothing")
# 42
```

### ワイルドカードとリテラル

`_` は何にでもマッチするワイルドカードパターンです。リテラル値（数値・文字列・真偽値）でもマッチできます。

```python
let n = 5
match n:
    case 0:
        print("zero")
    case 1:
        print("one")
    case _:
        print("other")
# other
```

### guard 節

`if` でガード条件を追加できます。

```python
match n:
    case x if x > 0:
        print("positive")
    case x if x < 0:
        print("negative")
    case _:
        print("zero")
```

> **注意**: `match` はすべてのパターンを網羅する必要があります。enum はすべてのバリアント、Option は `Some` と `None` の両方、リテラルは `_` が必要です。

---

[← 前: 演算子](03-operators.md) | [次: 関数 →](05-functions.md)
