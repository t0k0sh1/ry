[English](../../reference/operators.md) | [日本語](../../ja/reference/operators.md) | [繁體中文](operators.md)

# 運算子參考

## 優先順序表

優先順序數字越小越高（越先被求值）。

| 優先順序 | 運算子 | 說明 | 結合性 |
|---|---|---|---|
| 0 | `?` | 錯誤傳播（後綴） | 左 |
| 1 | `()` | 分組 | — |
| 2 | `+x` `-x` `~x` | 一元正號、負號、位元 NOT | 右 |
| 3 | `**` | 次方 | 右結合 |
| 3.5 | `as` | 型別轉換 | 左 |
| 4 | `*` `/` `%` `//` | 乘法、除法、餘數、整數除法 | 左 |
| 5 | `+` `-` | 加法、減法 | 左 |
| 6 | `<<` `>>` `>>>` | 位元移位 | 左 |
| 7 | `&` | 位元 AND | 左 |
| 8 | `^` | 位元 XOR | 左 |
| 9 | `\|` | 位元 OR | 左 |
| 10 | `==` `!=` `<` `<=` `>` `>=` `in` `not in` | 比較、歸屬 | 左 |
| 11 | `not` | 邏輯 NOT | 右 |
| 12 | `and` | 邏輯 AND | 左 |
| 13 | `or` | 邏輯 OR | 左 |
| 13.5 | `??` | 空值合併 | 左 |
| 14 | `?:` | 三元條件 | 右 |

## 算術運算子

| 運算子 | 說明 | 範例 |
|---|---|---|
| `+` | 加法 / 字串串接 | `1 + 2` → `3`、`"a" + "b"` → `"ab"` |
| `-` | 減法 | `5 - 3` → `2` |
| `*` | 乘法 / 字串重複 | `4 * 3` → `12`、`"ab" * 3` → `"ababab"` |
| `/` | 除法（始終為 float） | `7 / 2` → `3.5` |
| `//` | 整數除法（截斷） | `7 // 2` → `3` |
| `%` | 餘數 | `7 % 3` → `1` |
| `**` | 次方（始終為 float） | `2 ** 10` → `1024.0` |
| `-x` | 一元負號 | `-5`, `-3.14` |
| `+x` | 一元正號 | `+5`（不改變正負號） |

```python
a = 10 // 3    # 3 (int)
b = 10 / 3     # 3.3333... (float)
c = 2 ** 8     # 256.0 (float)
s = "foo" + "bar"  # "foobar"
```

## 比較運算子

全部回傳 `bool`。

| 運算子 | 說明 |
|---|---|
| `==` | 等於 |
| `!=` | 不等於 |
| `<` | 小於 |
| `<=` | 小於等於 |
| `>` | 大於 |
| `>=` | 大於等於 |

- 可用於數值型別（int / float）和 bool。
- `str` 之間以字典序（位元組順序）比較。
- `in` 運算子用於集合、串列、映射的歸屬檢查（`x in s`）。
- `not in` 運算子為 `in` 的否定（`x not in s`）。
- 對於映射，`in` 檢查鍵是否存在。

```python
x = 3 < 5       # true
y = "abc" < "abd"  # true（字典序）
s = {1, 2, 3}
z = 2 in s      # true
w = 4 not in s  # true
xs = [1, 2, 3]
a = 2 in xs     # true（串列線性搜尋）
m = {"a": 1}
b = "a" in m    # true（映射鍵搜尋）
```

## 邏輯運算子

| 運算子 | 說明 | 型別 |
|---|---|---|
| `and` | 邏輯 AND | `bool` × `bool` → `bool` |
| `or` | 邏輯 OR | `bool` × `bool` → `bool` |
| `not` | 邏輯 NOT | `bool` → `bool` |

```python
a = true and false   # false
b = true or false    # true
c = not true         # false
```

## 位元運算子

僅可用於 `int` 型別。對 `float` 或 `bool` 使用會產生編譯錯誤。

| 運算子 | 說明 | 範例 |
|---|---|---|
| `&` | 位元 AND | `0b1100 & 0b1010` → `0b1000` |
| `\|` | 位元 OR | `0b1100 \| 0b1010` → `0b1110` |
| `^` | 位元 XOR | `0b1100 ^ 0b1010` → `0b0110` |
| `~` | 位元 NOT（一元） | `~0` → `-1` |
| `<<` | 左移 | `1 << 4` → `16` |
| `>>` | 算術右移 | `16 >> 2` → `4` |
| `>>>` | 邏輯右移 | `-1 >>> 1` → `9223372036854775807` |

```python
flags = 0b0001 | 0b0010   # 3
masked = flags & 0b0011   # 3
shifted = 1 << 8          # 256
```

## 錯誤傳播運算子（`?`）

後綴 `?` 運算子用於解包 `Result` 值。如果值為 `Ok(v)`，則求值為 `v`。如果值為 `Err(e)`，則外層函式立即回傳 `Err(e)`。

外層函式必須具有 `Result` 回傳型別。

```python
fn safe_divide(a: int, b: int) -> Result<int, Error>:
    if b == 0:
        return Err(Error("division by zero"))
    return Ok(a // b)

fn compute(a: int, b: int, c: int) -> Result<int, Error>:
    x = safe_divide(a, b)?    # 若 b == 0 則提前回傳 Err
    y = safe_divide(x, c)?    # 若 c == 0 則提前回傳 Err
    return Ok(y + 1)
```

這等同於以下 `match` 模式，但更加簡潔：

```python
fn compute(a: int, b: int, c: int) -> Result<int, Error>:
    match safe_divide(a, b):
        case Ok(x):
            match safe_divide(x, c):
                case Ok(y):
                    return Ok(y + 1)
                case Err(e):
                    return Err(e)
        case Err(e):
            return Err(e)
```

---

## 三元條件運算子

```python
x = condition ? true_value : false_value
```

對 `condition` 進行求值。若為真，回傳 `true_value`；否則回傳 `false_value`。兩個分支必須具有相同的型別。右結合，因此巢狀三元運算子從右向左結合。

```python
x = 3 > 2 ? 10 : 20     # 10
s = false ? "yes" : "no" # "no"

# 巢狀（右結合）
y = true ? (false ? 1 : 2) : 3   # 2
```

---

## 範圍運算子

`..` 運算子建立包含兩端的整數範圍。

```python
xs = 1 .. 5    # [1, 2, 3, 4, 5]

for i in 1 .. 3:
    print(i)       # 1 2 3
```

結果是包含從左運算元到右運算元（兩端皆含）的所有整數的 `List<int>`。

---

## 空值合併運算子（`??`）

```python
x = option_val ?? default_val
```

如果 `option_val` 為 `Some(v)`，則回傳 `v`。否則回傳 `default_val`。右運算元必須與 Option 的內部型別相同。

```python
a: int? = Some(10)
b: int? = none

print(a ?? 0)    # 10
print(b ?? 0)    # 0
```

---

## 複合賦值運算子

更新變數的簡寫形式。`x op= y` 等價於 `x = x op y`。

| 運算子 | 等價的運算式 |
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

## 遞增／遞減運算子

用於將變數增減 1 的後綴運算子。僅可作為陳述式使用。內部分別被轉換為 `x = x + 1` 和 `x = x - 1`。

| 運算子 | 等價的運算式 |
|---|---|
| `x++` | `x = x + 1` |
| `x--` | `x = x - 1` |

```python
count = 0
count++       # count = 1
count++       # count = 2
count--       # count = 1

f = 1.5
f++           # f = 2.5（int 1 會提升為 float）
```

> **注意**：`++` / `--` 只能作為陳述式使用，不能在運算式中使用。
> `@const` 變數不能使用遞增／遞減（不可變性會被強制執行）。

---

## 運算的型別規則

| 運算 | 左運算元型別 | 右運算元型別 | 結果型別 |
|---|---|---|---|
| `+ - *` | int | int | int |
| `+ - *` | float | int / float | float |
| `+ - *` | int | float | float |
| `/` | 任意數值 | 任意數值 | float |
| `//` | 任意數值 | 任意數值 | int |
| `**` | 任意數值 | 任意數值 | float |
| `%` | int | int | int |
| `%` | float 或 int（其中一方為 float） | — | float |
| `+` | str | str | str |
| `== != < <= > >=` | 數值 / bool / str | 同型別 | bool |
| `*` | str | int | str |
| `in` | 任意 | Set<T> / List<T> / Map<K, V> | bool |
| `not in` | 任意 | Set<T> / List<T> / Map<K, V> | bool |
| `& \| ^ ~ << >> >>>` | int | int | int |
| `and or not` | bool | bool | bool |

## 運算子多載

可以為使用者定義型別定義運算子的行為。

### 語法

```python
# 二元運算子（2 個引數）
fn operator+(a: MyType, b: MyType) -> MyType:
    ...

# 一元運算子（1 個引數）
fn operator-(a: MyType) -> MyType:
    ...
```

### 可多載的運算子一覽

| 種類 | 運算子 |
|---|---|
| 算術（二元） | `+` `-` `*` `/` `%` `**` `//` |
| 比較（二元） | `==` `!=` `<` `<=` `>` `>=` |
| 位元（二元） | `&` `\|` `^` `<<` `>>` `>>>` |
| 邏輯（二元） | `and` `or` |
| 一元 | `-` `~` `not` |

### 二元 / 一元的區別

依引數個數區分。

```python
# 二元 -
fn operator-(a: Vec2, b: Vec2) -> Vec2:
    return Vec2(a.x - b.x, a.y - b.y)

# 一元 -
fn operator-(v: Vec2) -> Vec2:
    return Vec2(-v.x, -v.y)
```
