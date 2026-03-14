[English](../../reference/builtins-string.md) | [日本語](../../ja/reference/builtins-string.md) | [繁體中文](builtins-string.md)

# 字串操作函式參考

針對字串（`str`）的操作函式一覽。所有函式皆可使用 UFCS 記法。

> **注意：** 所有字串操作以位元組為單位。對於多位元組字元（如中文等）可能無法正確運作。

## 函式一覽

### 搜尋與判定

| 函式 | 簽名 | 說明 |
|------|-----------|------|
| `contains` | `(str, str) → bool` | 回傳是否包含子字串 |
| `starts_with` | `(str, str) → bool` | 回傳是否以前綴開頭 |
| `ends_with` | `(str, str) → bool` | 回傳是否以後綴結尾 |
| `find` | `(str, str) → int` | 回傳子字串的位置（未找到為 -1） |

### 擷取與轉換

| 函式 | 簽名 | 說明 |
|------|-----------|------|
| `substring` | `(str, int, int) → str` | 取得子字串 |
| `char_at` | `(str, int) → str` | 取得指定位置的字元 |
| `replace` | `(str, str, str) → str` | 全部取代子字串 |

### 大小寫

| 函式 | 簽名 | 說明 |
|------|-----------|------|
| `to_upper` | `str → str` | 轉換為 ASCII 大寫 |
| `to_lower` | `str → str` | 轉換為 ASCII 小寫 |

### 去除空白

| 函式 | 簽名 | 說明 |
|------|-----------|------|
| `trim` | `str → str` | 去除前後的空白 |
| `trim_start` | `str → str` | 去除開頭的空白 |
| `trim_end` | `str → str` | 去除結尾的空白 |

### 生成與加工

| 函式 | 簽名 | 說明 |
|------|-----------|------|
| `repeat` | `(str, int) → str` | 將字串重複 n 次 |
| `reverse` | `str → str` | 反轉字串 |

### 分割與連接

| 函式 | 簽名 | 說明 |
|------|-----------|------|
| `split` | `(str, str) → List<str>` | 以分隔符號分割 |
| `join` | `(List<str>, str) → str` | 以分隔符號連接 |

### 型別轉換

| 函式 | 簽名 | 說明 |
|------|-----------|------|
| `to_int` | `str → int` | 將字串轉換為整數 |
| `to_float` | `str → float` | 將字串轉換為浮點數 |
| `to_str` | `int/float/bool/str → str` | 將值轉換為字串 |

---

## contains

**簽名：** `contains(s: str, sub: str) -> bool`

回傳字串 `s` 中是否包含子字串 `sub`。

```python
print(contains("hello", "ell"))   # true
print("hello".contains("xyz"))    # false (UFCS)
```

---

## starts_with

**簽名：** `starts_with(s: str, prefix: str) -> bool`

回傳字串 `s` 是否以 `prefix` 開頭。

```python
print(starts_with("hello", "hel"))   # true
print("hello".starts_with("world"))  # false (UFCS)
```

---

## ends_with

**簽名：** `ends_with(s: str, suffix: str) -> bool`

回傳字串 `s` 是否以 `suffix` 結尾。

```python
print(ends_with("hello", "llo"))   # true
print("hello".ends_with("world"))  # false (UFCS)
```

---

## find

**簽名：** `find(s: str, sub: str) -> int`

回傳字串 `s` 中子字串 `sub` 首次出現的位置（位元組偏移量）。未找到時回傳 `-1`。

```python
print(find("hello world", "world"))   # 6
print(find("hello", "xyz"))           # -1
print("abcdef".find("cd"))            # 2 (UFCS)
```

---

## substring

**簽名：** `substring(s: str, start: int, end: int) -> str`

回傳字串 `s` 從 `start` 到 `end`（不含）的子字串。

```python
print(substring("hello world", 0, 5))   # hello
print(substring("hello world", 6, 11))  # world
print("abcdef".substring(1, 4))         # bcd (UFCS)
```

---

## char_at

**簽名：** `char_at(s: str, i: int) -> str`

回傳字串 `s` 第 `i` 個位元組作為單字元字串。

```python
print(char_at("hello", 0))   # h
print("abc".char_at(2))       # c (UFCS)
```

---

## replace

**簽名：** `replace(s: str, old: str, new: str) -> str`

回傳將字串 `s` 中所有 `old` 替換為 `new` 後的新字串。

```python
print(replace("hello world", "world", "ry"))   # hello ry
print(replace("aaa", "a", "bb"))                # bbbbbb
print("foo bar foo".replace("foo", "baz"))      # baz bar baz (UFCS)
```

---

## to_upper

**簽名：** `to_upper(s: str) -> str`

回傳將 ASCII 小寫字母（a-z）轉換為大寫後的新字串。

```python
print(to_upper("hello"))         # HELLO
print("Hello World".to_upper())  # HELLO WORLD (UFCS)
```

---

## to_lower

**簽名：** `to_lower(s: str) -> str`

回傳將 ASCII 大寫字母（A-Z）轉換為小寫後的新字串。

```python
print(to_lower("HELLO"))         # hello
print("Hello World".to_lower())  # hello world (UFCS)
```

---

## trim

**簽名：** `trim(s: str) -> str`

回傳去除字串前後空白字元（空格、定位字元、換行、回車）後的新字串。

```python
print(trim("  hello  "))   # hello
print("  hi  ".trim())     # hi (UFCS)
```

---

## trim_start

**簽名：** `trim_start(s: str) -> str`

回傳去除字串開頭空白字元後的新字串。

```python
print(trim_start("  hello  "))   # hello
print("  hi".trim_start())       # hi (UFCS)
```

---

## trim_end

**簽名：** `trim_end(s: str) -> str`

回傳去除字串結尾空白字元後的新字串。

```python
print(trim_end("  hello  "))   #   hello
print("hi  ".trim_end())       # hi (UFCS)
```

---

## repeat

**簽名：** `repeat(s: str, n: int) -> str`

回傳將字串 `s` 重複 `n` 次後的新字串。

```python
print(repeat("ab", 3))     # ababab
print("ha".repeat(3))      # hahaha (UFCS)
```

---

## reverse

**簽名：** `reverse(s: str) -> str`

回傳以位元組為單位反轉後的新字串。

```python
print(reverse("hello"))    # olleh
print("abc".reverse())     # cba (UFCS)
```

---

## split

**簽名：** `split(s: str, delim: str) -> List<str>`

以分隔符號 `delim` 分割字串 `s`，回傳 `List<str>`。

```python
let parts = split("a,b,c", ",")
print(parts[0])   # a
print(parts[1])   # b
print(parts[2])   # c

for word in "hello world".split(" "):
    print(word)
# hello
# world
```

---

## join

**簽名：** `join(xs: List<str>, sep: str) -> str`

以分隔符號 `sep` 連接字串串列的元素，回傳結合後的字串。

```python
let parts = ["a", "b", "c"]
print(join(parts, ","))        # a,b,c
print(parts.join("-"))         # a-b-c (UFCS)
```

---

## to_int

**簽名：** `to_int(s: str) -> int`

將字串轉換為整數。

```python
print(to_int("42"))       # 42
print(to_int("-7"))       # -7
print("123".to_int())     # 123 (UFCS)
```

---

## to_float

**簽名：** `to_float(s: str) -> float`

將字串轉換為浮點數。

```python
print(to_float("3.14"))   # 3.14
print("2.5".to_float())   # 2.5 (UFCS)
```

---

## to_str

**簽名：** `to_str(v: int | float | bool | str) -> str`

將值轉換為字串。

| 型別 | 轉換格式 |
|----|---------|
| `int` | `%ld` |
| `float` | `%g` |
| `bool` | `"true"` / `"false"` |
| `str` | 直接回傳 |

```python
print(to_str(42))         # 42
print(to_str(3.14))       # 3.14
print(to_str(true))       # true
print(99.to_str())        # 99 (UFCS)
```
