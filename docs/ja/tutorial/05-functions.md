[English](../../tutorial/05-functions.md) | [日本語](05-functions.md) | [繁體中文](../../zh/tutorial/05-functions.md)

# 関数

[← 前: 制御構文](04-control-flow.md) | [次: Record →](06-records.md)

---

## 基本的な関数定義

関数は `fn` キーワードで定義します。引数の型宣言は必須で `name: type` の形式で指定します。戻り値の型は `->` の後に指定します。

```python
fn add(a: int, b: int) -> int:
    return a + b
```

- 引数の型宣言は必須です。
- 戻り値型は `->` の後に指定します。
- `return` 文で値を返します。

---

## 関数の呼び出し

定義した関数は名前と引数を指定して呼び出します。

```python
fn multiply(x: int, y: int) -> int:
    return x * y

result = multiply(3, 4)
print(result)   # 12
```

---

## 再帰関数

関数は自分自身を呼び出せます（再帰）。

```python
fn factorial(n: int) -> int:
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
fn add(a: int, b: int) -> int:
    return a + b

fn add(a: float, b: float) -> float:
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
fn greet():
    print(42)

greet()   # 42
```

引数なし・戻り値なしの最もシンプルな関数形式です。

---

[← 前: 制御構文](04-control-flow.md) | [次: Record →](06-records.md)
