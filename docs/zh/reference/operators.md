[English](../../reference/operators.md) | [日本語](../../ja/reference/operators.md) | [繁體中文](operators.md)

# 運算子參考

## 優先順序表

優先順序數字越小越高（越先被求值）。

| 優先順序 | 運算子 | 說明 | 結合性 |
|---|---|---|---|
| 0 | `!!` | 錯誤傳播（後綴） | 左 |
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
let a = 10 // 3    # 3 (int)
let b = 10 / 3     # 3.3333... (float)
let c = 2 ** 8     # 256.0 (float)
let s = "foo" + "bar"  # "foobar"
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
let x = 3 < 5       # true
let y = "abc" < "abd"  # true（字典序）
let s = {1, 2, 3}
let z = 2 in s      # true
let w = 4 not in s  # true
let xs = [1, 2, 3]
let a = 2 in xs     # true（串列線性搜尋）
let m = {"a": 1}
let b = "a" in m    # true（映射鍵搜尋）
```

## 邏輯運算子

| 運算子 | 說明 | 型別 |
|---|---|---|
| `and` | 邏輯 AND | `bool` × `bool` → `bool` |
| `or` | 邏輯 OR | `bool` × `bool` → `bool` |
| `not` | 邏輯 NOT | `bool` → `bool` |

```python
let a = true and false   # false
let b = true or false    # true
let c = not true         # false
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
let flags = 0b0001 | 0b0010   # 3
let masked = flags & 0b0011   # 3
let shifted = 1 << 8          # 256
```

## 三元條件運算子

```python
let x = condition ? true_value : false_value
```

對 `condition` 進行求值。若為真，回傳 `true_value`；否則回傳 `false_value`。兩個分支必須具有相同的型別。右結合，因此巢狀三元運算子從右向左結合。

```python
let x = 3 > 2 ? 10 : 20     # 10
let s = false ? "yes" : "no" # "no"

# 巢狀（右結合）
let y = true ? (false ? 1 : 2) : 3   # 2
```

---

## 範圍運算子

`..` 運算子建立包含兩端的整數範圍。

```python
let xs = 1 .. 5    # [1, 2, 3, 4, 5]

for i in 1 .. 3:
    print(i)       # 1 2 3
```

結果是包含從左運算元到右運算元（兩端皆含）的所有整數的 `List<int>`。

---

## 空值合併運算子（`??`）

```python
let x = option_val ?? default_val
```

如果 `option_val` 為 `Some(v)`，則回傳 `v`。否則回傳 `default_val`。右運算元必須與 Option 的內部型別相同。

```python
let a: int? = Some(10)
let b: int? = none

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
var x = 10
x += 5    # x = 15
x -= 3    # x = 12
x *= 2    # x = 24
x //= 3  # x = 8
x &= 6   # x = 0
```

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
