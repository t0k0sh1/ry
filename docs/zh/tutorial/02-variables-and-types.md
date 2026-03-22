[English](../../tutorial/02-variables-and-types.md) | [日本語](../../ja/tutorial/02-variables-and-types.md) | [繁體中文](02-variables-and-types.md)

# 02 - 變數與型別

← [01 - 入門](01-getting-started.md) / 下一個 → [03 - 運算子](03-operators.md)

---

## 使用 let 宣告常數

使用 `let` 關鍵字宣告不可變的變數（常數）。型別會根據右側的值自動推論。宣告後無法變更其值。

```python
let x = 42        # 推論為 int 型別
let y = 3.14      # 推論為 float 型別
let flag = true   # 推論為 bool 型別
let name = "Ry"   # 推論為 str 型別
```

---

## 使用 var 宣告變數

使用 `var` 關鍵字宣告可變的變數。宣告後可以重新賦予相同型別的值。

```python
var count = 0
count = count + 1   # OK：重新賦予相同型別的值
```

---

## 型別標註

可以明確指定變數的型別。

```python
let x: int = 42
let rate: float = 0.5
let ok: bool = false
let msg: str = "hello"
```

當型別標註與實際值的型別不一致時，會產生編譯錯誤。

---

## 基本型別

| 型別 | 說明 | 字面值範例 |
|----|------|-----------|
| `int` | 64 位元整數 | `0`, `42`, `-10` |
| `byte` | 無號 8 位元整數（0-255） | `let b: byte = 42` |
| `float` | 64 位元浮點數 | `0.0`, `3.14`, `-1.5` |
| `bool` | 布林值 | `true`, `false` |
| `str` | 字串 | `"hello"`, `""` |

---

## 字串操作

字串支援多種操作。

```python
let a = "Hello"
let b = "World"

# 串接
let c = a + ", " + b   # "Hello, World"

# 比較（字典序）
print(a == b)   # false
print(a != b)   # true
print(a < b)    # true（"H" < "W"）

# 長度
print(len(a))   # 5

# 子字串檢查
let s = "Hello, World!"
print(contains(s, "World"))      # true
print(starts_with(s, "Hello"))   # true
print(ends_with(s, "!"))         # true
```

---

## 跳脫序列

字串中可使用以下跳脫序列。

| 序列 | 意義 |
|------------|------|
| `\n` | 換行 |
| `\r` | 回車 |
| `\t` | Tab |
| `\\` | 反斜線 |
| `\"` | 雙引號 |
| `\0` | 空字元 |

```python
print("Hello\nWorld")   # 分成兩行輸出
print("A\tB")           # 以 Tab 分隔
print("say \"hi\"")     # 包含雙引號的字串
```

---

## 重新賦值規則

使用 `var` 宣告的變數可以重新賦值，但有以下限制：

```python
var x = 10
x = 20        # OK：重新賦予相同型別的值
# x = "text" # 錯誤：禁止變更型別的重新賦值
```

`let` 無法重新賦值。

```python
let N = 5
# N = 6  # 錯誤：禁止對 let 變數重新賦值
```

也無法重新宣告同名的變數。

```python
let x = 1
# let x = 2  # 錯誤：禁止重新宣告同名變數
```

---

## 元組解構

使用 `let` 或 `var`，可以在單次宣告中將元組拆解為多個變數。

```python
let a, b = (10, 20)
print(a)   # 10
print(b)   # 20
```

### 萬用字元

使用 `_` 忽略特定位置的值。

```python
let x, _ = (1, 2)   # 只綁定 x；2 被捨棄
print(x)             # 1
```

### 可變變數解構

使用 `var` 宣告可變變數。

```python
var a, b = (10, 20)
a = 99
print(a)   # 99
```

### 規則

- 左側的變數數量必須與元組的元素數量相符。
- 每個變數遵循與一般宣告相同的 `let`/`var` 規則。
- 不支援巢狀元組解構。

---

← [01 - 入門](01-getting-started.md) / 下一個 → [03 - 運算子](03-operators.md)
