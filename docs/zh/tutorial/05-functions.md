[English](../../tutorial/05-functions.md) | [日本語](../../ja/tutorial/05-functions.md) | [繁體中文](05-functions.md)

# 函式

[← 前一篇：控制流程](04-control-flow.md) | [下一篇：Record →](06-records.md)

---

## 基本函式定義

函式使用 `fn` 關鍵字定義。參數的型別宣告為必要項，以 `name: type` 的格式指定。回傳值型別在 `->` 之後指定。

```python
fn add(a: int, b: int) -> int:
    return a + b
```

- 參數的型別宣告是必要的。
- 回傳值型別在 `->` 之後指定。
- 使用 `return` 陳述式回傳值。

---

## 函式呼叫

透過名稱和參數來呼叫已定義的函式。

```python
fn multiply(x: int, y: int) -> int:
    return x * y

result = multiply(3, 4)
print(result)   # 12
```

---

## 遞迴函式

函式可以呼叫自身（遞迴）。

```python
fn factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)

print(factorial(5))   # 120
print(factorial(0))   # 1
```

---

## 函式多載

可以定義多個同名但參數數量或型別不同的函式。

```python
fn add(a: int, b: int) -> int:
    return a + b

fn add(a: float, b: float) -> float:
    return a + b

print(add(1, 2))       # 3
print(add(1.5, 2.5))   # 4
```

呼叫時會根據參數的型別自動選擇適當的函式。

> **注意**：若定義參數型別相同但僅回傳值型別不同的函式，會產生編譯錯誤。

---

## 省略回傳值型別（Unit 型別）

不需要回傳值的函式可以省略 `->`。此時函式回傳 Unit 型別。

```python
fn greet():
    print(42)

greet()   # 42
```

這是最簡單的無參數、無回傳值函式形式。

---

[← 前一篇：控制流程](04-control-flow.md) | [下一篇：Record →](06-records.md)
