[English](../../reference/builtins-string.md) | [日本語](../../ja/reference/builtins-string.md) | [简体中文](builtins-string.md)

# 字符串操作函数参考

针对字符串（`str`）的操作函数一览。所有函数均可使用 UFCS 记法。

> **注意：** 所有字符串操作均支持 UTF-8。`length()`、`char_at()`、`substring()`、`find()` 和 `reverse()` 以 Unicode 码位为单位操作，而非字节。如需获取字节长度，请使用 `byte_len()`。

## 函数列表

### 搜索与判定

| 函数 | 签名 | 说明 |
|------|-----------|------|
| `contains` | `(str, str, bool = false) -> bool` | 返回是否包含子字符串 |
| `starts_with` | `(str, str, bool = false) -> bool` | 返回是否以前缀开头 |
| `ends_with` | `(str, str, bool = false) -> bool` | 返回是否以后缀结尾 |
| `find` | `(str, str) -> Option<int>` | 返回子字符串的字符位置（未找到为 `None`） |

### 提取与转换

| 函数 | 签名 | 说明 |
|------|-----------|------|
| `substring` | `(str, int, int) -> str` | 提取子字符串（字符索引） |
| `char_at` | `(str, int) -> str` | 获取指定位置的 UTF-8 字符 |
| `replace` | `(str, str, str) -> str` | 替换所有匹配的子字符串 |

### 大小写转换

| 函数 | 签名 | 说明 |
|------|-----------|------|
| `to_upper` | `str -> str` | 转换为 ASCII 大写 |
| `to_lower` | `str -> str` | 转换为 ASCII 小写 |

### 去除空白

| 函数 | 签名 | 说明 |
|------|-----------|------|
| `trim` | `str -> str` | 去除前后的空白 |
| `trim_start` | `str -> str` | 去除开头的空白 |
| `trim_end` | `str -> str` | 去除结尾的空白 |

### 生成与加工

| 函数 | 签名 | 说明 |
|------|-----------|------|
| `repeat` | `(str, int) -> str` | 将字符串重复 n 次 |
| `reverse` | `str -> str` | 反转字符串（UTF-8 感知） |
| `byte_len` | `str -> int` | 返回字符串的字节长度 |

### 分割与连接

| 函数 | 签名 | 说明 |
|------|-----------|------|
| `split` | `(str, str) -> List<str>` | 以分隔符分割 |
| `join` | `(List<str>, str) -> str` | 以分隔符连接 |

### 类型转换

| 函数 | 签名 | 说明 |
|------|-----------|------|
| `to_int` | `str -> Result<int, Error>` | 将字符串转换为整数 |
| `to_float` | `str -> Result<float, Error>` | 将字符串转换为浮点数 |
| `to_str` | `int/float/bool/str/enum/record -> str` | 将值转换为字符串 |

---

## contains

**签名：** `contains(string: str, substring: str, ignore_case: bool = false) -> bool`

返回字符串 `string` 中是否包含子字符串 `substring`。当 `ignore_case` 为 `true` 时，比较不区分大小写（仅 ASCII）。

```python
print(contains("hello", "ell"))              # true
print("hello".contains("xyz"))               # false (UFCS)
print(contains("Hello World", "hello", true))  # true（不区分大小写）
```

---

## starts_with

**签名：** `starts_with(string: str, prefix: str, ignore_case: bool = false) -> bool`

返回字符串 `string` 是否以 `prefix` 开头。当 `ignore_case` 为 `true` 时，比较不区分大小写（仅 ASCII）。

```python
print(starts_with("hello", "hel"))              # true
print("hello".starts_with("world"))              # false (UFCS)
print(starts_with("Hello", "hello", true))  # true（不区分大小写）
```

---

## ends_with

**签名：** `ends_with(string: str, suffix: str, ignore_case: bool = false) -> bool`

返回字符串 `string` 是否以 `suffix` 结尾。当 `ignore_case` 为 `true` 时，比较不区分大小写（仅 ASCII）。

```python
print(ends_with("hello", "llo"))              # true
print("hello".ends_with("world"))              # false (UFCS)
print(ends_with("Hello World", "WORLD", true))  # true（不区分大小写）
```

---

## find

**签名：** `find(string: str, substring: str) -> Option<int>`

返回字符串 `string` 中子字符串 `substring` 首次出现的字符位置。未找到时返回 `None`。

```python
print(find("hello world", "world"))   # Some(6)
print(find("hello", "xyz"))           # None
print("abcdef".find("cd"))            # Some(2) (UFCS)
```

---

## substring

**签名：** `substring(string: str, start: int, end: int) -> str`

返回字符串 `string` 从 `start` 到 `end`（不含）的子字符串。索引为字符位置（UTF-8 感知）。

超出范围的索引会被钳制到 `[0, length]`。钳制后若 `end < start`，返回空字符串。

```python
print(substring("hello world", 0, 5))   # hello
print(substring("hello world", 6, 11))  # world
print("abcdef".substring(1, 4))         # bcd (UFCS)
print(substring("hello", -1, 100))      # hello（钳制）
```

---

## char_at

**签名：** `char_at(string: str, i: int) -> str`

返回字符串 `string` 第 `i` 个位置的 UTF-8 字符作为字符串。索引超出范围时产生运行时错误。

负数索引从末尾开始计算（Python 风格）：`-1` 指最后一个字符，`-2` 指倒数第二个，以此类推。

```python
print(char_at("hello", 0))    # h
print(char_at("hello", -1))   # o（最后一个字符）
print("abc".char_at(2))       # c (UFCS)
```

---

## replace

**签名：** `replace(string: str, old: str, new: str) -> str`

返回将字符串 `string` 中所有 `old` 替换为 `new` 后的新字符串。

如果 `old` 是空字符串，则原样返回输入（作为新副本）。

```python
print(replace("hello world", "world", "ry"))   # hello ry
print(replace("aaa", "a", "bb"))                # bbbbbb
print("foo bar foo".replace("foo", "baz"))      # baz bar baz (UFCS)
print(replace("hello", "", "X"))                # hello（空模式为 no-op）
```

---

## to_upper

**签名：** `to_upper(string: str) -> str`

返回将 ASCII 小写字母（a-z）转换为大写后的新字符串。

```python
print(to_upper("hello"))         # HELLO
print("Hello World".to_upper())  # HELLO WORLD (UFCS)
```

---

## to_lower

**签名：** `to_lower(string: str) -> str`

返回将 ASCII 大写字母（A-Z）转换为小写后的新字符串。

```python
print(to_lower("HELLO"))         # hello
print("Hello World".to_lower())  # hello world (UFCS)
```

---

## trim

**签名：** `trim(string: str) -> str`

返回去除字符串前后空白字符（空格、制表符、换行、回车）后的新字符串。

```python
print(trim("  hello  "))   # hello
print("  hi  ".trim())     # hi (UFCS)
```

---

## trim_start

**签名：** `trim_start(string: str) -> str`

返回去除字符串开头空白字符后的新字符串。

```python
print(trim_start("  hello  "))   # hello
print("  hi".trim_start())       # hi (UFCS)
```

---

## trim_end

**签名：** `trim_end(string: str) -> str`

返回去除字符串结尾空白字符后的新字符串。

```python
print(trim_end("  hello  "))   #   hello
print("hi  ".trim_end())       # hi (UFCS)
```

---

## repeat

**签名：** `repeat(string: str, count: int) -> str`

返回将字符串 `string` 重复 `count` 次后的新字符串。

```python
print(repeat("ab", 3))     # ababab
print("ha".repeat(3))      # hahaha (UFCS)
```

---

## reverse

**签名：** `reverse(string: str) -> str`

返回字符顺序反转后的新字符串（UTF-8 感知）。

```python
print(reverse("hello"))    # olleh
print("abc".reverse())     # cba (UFCS)
```

---

## byte_len

**签名：** `byte_len(string: str) -> int`

返回字符串 `string` 的字节长度。与返回 UTF-8 字符数的 `length()` 不同，`byte_len()` 返回的是字节数。

```python
print(byte_len("hello"))   # 5
print(byte_len("あいう"))   # 9
print(length("あいう"))        # 3 (characters)
```

---

## split

**签名：** `split(string: str, delimiter: str) -> List<str>`

以分隔符 `delimiter` 分割字符串 `string`，返回 `List<str>`。

当分隔符为空字符串 `""` 时，字符串会被分割为单个字符（UTF-8 感知）。

```python
parts = split("a,b,c", ",")
print(parts[0])   # a
print(parts[1])   # b
print(parts[2])   # c

for word in "hello world".split(" "):
    print(word)
# hello
# world

# 分割为单个字符
chars = split("hello", "")
print(chars)   # [h, e, l, l, o]

# UTF-8 字符
chars = split("あいう", "")
print(chars)   # [あ, い, う]
```

> **提示：** 要逐字符迭代字符串，您可以直接使用 `for` 循环而无需调用 `split`：`for c in s:` 会作为单字符 `str` 产生每个 UTF-8 码位。请参阅 [control-flow.md](control-flow.md#字符串迭代)。

---

## join

**签名：** `join(values: List<str>, sep: str) -> str`

以分隔符 `sep` 连接字符串列表的元素，返回合并后的字符串。

```python
parts = ["a", "b", "c"]
print(join(parts, ","))        # a,b,c
print(parts.join("-"))         # a-b-c (UFCS)
print(",".join(parts))         # a,b,c (UFCS, Python 风格)
```

---

## to_int

**签名：** `to_int(string: str) -> Result<int, Error>`

将字符串转换为整数。允许前导空白。字符串为空、包含无效字符或溢出时返回 `Err`。

```python
case to_int("42"):
    Ok(v):
        print(v)              # 42
    Err(e):
        print(e.message)

case "123".to_int():                
    Ok(v):
        print(v)              # 123
    Err(e):
        print(e.message)

# 无效输入返回 Err
print(to_int("abc"))          # Err(Error("to_int: invalid character in 'abc'"))
print(to_int(""))             # Err(Error("to_int: empty string"))
```

---

## to_float

**签名：** `to_float(string: str) -> Result<float, Error>`

将字符串转换为浮点数。字符串为空、包含无效字符或超出 `float` 范围时返回 `Err`。

```python
case to_float("3.14"):
    Ok(v):
        print(v)              # 3.14
    Err(e):
        print(e.message)

case "2.5".to_float():              
    Ok(v):
        print(v)              # 2.5
    Err(e):
        print(e.message)

# 无效输入返回 Err
print(to_float("abc"))         # Err(Error("to_float: invalid character in 'abc'"))
print(to_float(""))            # Err(Error("to_float: empty string"))
print(to_float("1e400"))       # Err(Error("to_float: out of range in '1e400'"))
```

---

## to_str

**签名：** `to_str(v: int | float | bool | str | enum | record) -> str`

将值转换为字符串。

| 类型 | 转换格式 |
|----|---------|
| `int` | `%ld` |
| `float` | `%g`，整数值会附带 `.0`（例如 `"3.0"`、`"0.0"`） |
| `bool` | `"true"` / `"false"` |
| `str` | 直接返回 |
| enum | 变体名称（例如 `"Red"`） |
| record | `TypeName(field1: val1, field2: val2)` |
| `List` / `Map` / `Set` | 递归格式化，支持嵌套容器（例如 `Map<str, List<int>>`） |
| union | 格式化为活动变体；支持 `List`、`Map`、`Set` 和 function 变体 |
| function 值（closure / lambda） | `"<closure>"` |

整数值的 `float` 会附带 `.0` 格式化（例如 `to_str(3.0) == "3.0"`），以便与 `int` 视觉上区分。Record 类型自动生成 `to_str` 表示。如果提供了用户定义的 `function to_str(v: MyRecord) -> str`，则优先使用用户定义的版本。这也适用于 `print()` 和 f-string 插值。

```python
print(to_str(42))         # 42
print(to_str(3.14))       # 3.14
print(to_str(3.0))        # 3.0          (整数值 float 保留 .0)
print(to_str(true))       # true
print(99.to_str())        # 99 (UFCS)

enum Color:
    Red
    Green

print(to_str(Color::Red))   # Red

record Point:
    x: int
    y: int

p = Point(10, 20)
print(to_str(p))          # Point(x: 10, y: 20)
print(p)                  # Point(x: 10, y: 20)
print(f"pos={p}")         # pos=Point(x: 10, y: 20)
```
