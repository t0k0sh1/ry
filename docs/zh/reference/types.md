[English](../../reference/types.md) | [日本語](../../ja/reference/types.md) | [简体中文](types.md)

# 类型参考

## 类型一览

| 类型 | 内部表示 | 字面值示例 | 说明 |
|---|---|---|---|
| `int` | i64 | `42`, `-7`, `0xFF`, `0b1010`, `100_000` | 64 位有符号整数 |
| `u8` | i8 | （无专用字面值） | 无符号 8 位整数（0-255）。通过类型标注 `b: u8 = 42` 使用 |
| `float` | f64 | `3.14`, `0.5`, `3.14_159`, `1e10`, `1.5e-3`, `2.5E+2` | 64 位浮点数（支持科学记数法） |
| `bool` | i1 | `true`, `false` | 布尔值 |
| `str` | ptr | `"hello"`, `""`, `"a\nb"` | 字符串（堆上的不可变字节序列） |
| `Unit` | void | （无返回值） | 无返回值函数的返回类型。必须用 `-> Unit` 显式指定 |
| `Option<T>` | `{ i1, T }` | `Some(42)`, `None` | 可能存在值的类型 |
| `(T1, T2, ...)` | LLVM StructType (literal) | `(1, 3.14)` | 元组类型 |
| `List<T>` | ptr（堆） | `[1, 2, 3]` | 动态数组 |
| `Map<K, V>` | ptr（堆） | `{"a": 1}` | 哈希映射 |
| `Set<T>` | ptr（堆） | `{1, 2, 3}` | 不重复的集合 |
| `function(T1, T2) -> R` | ptr（函数指针） | `(x: int) => x * 2` | 函数类型 |
| 用户定义类型 | LLVM StructType (named) | `record Point: ...` | 以 `record` 关键字定义的结构体 |
| `enum` | i64 / 标签联合 | `Color::Red`, `Shape::Circle(3.14)` | 以 `enum` 关键字定义的枚举类型（支持关联数据） |
| `Error` | `{ ptr, i64 }` | `Error("msg")`, `Error("msg", 404)` | 内置错误类型 |
| `Type` | `{ i64, ptr }` | `type_of(42)` | 由 `type_of` 返回的编译时类型标识。请参阅 [Type](#type) |
| `any` | `{ i64, [8 x i8] }` | `x: any = 42` | 可持有任意基本值的标签联合 |
| `T1 \| T2` | `{ i64, [N x i8] }` | `int \| str` | union 类型（可持有多种类型之一） |
| Int 字面量 | i64 | `42`, `0 \| 1` | int 字面量类型（值约束） |
| String 字面量 | ptr | `"N" \| "S"` | str 字面量类型（值约束） |
| 范围 | i64 | `1..12`, `-10..10` | 范围类型（包含两端的整数范围约束） |
| `i8` | i8 | `x: i8 = 42`, `x = 42i8` | 8 位有符号整数（低级，无隐式转换） |
| `i16` | i16 | `x: i16 = 100`, `x = 100i16` | 16 位有符号整数（低级，无隐式转换） |
| `i32` | i32 | `x: i32 = 42`, `x = 42i32` | 32 位有符号整数（低级，无隐式转换） |
| `i64` | i64 | `x: i64 = 100`, `x = 100i64` | 64 位有符号整数（低级，无隐式转换） |
| `u8` | i8 | `x: u8 = 200`, `x = 200u8` | 8 位无符号整数（低级，无隐式转换） |
| `u16` | i16 | `x: u16 = 60000`, `x = 60000u16` | 16 位无符号整数（低级，无隐式转换） |
| `u32` | i32 | `x: u32 = 4294967295`, `x = 100u32` | 32 位无符号整数（低级，无隐式转换） |
| `u64` | i64 | `x: u64 = 18446744073709551615`, `x = 0xFFFFFFFFFFFFFFFFu64` | 最大 2^64 − 1 的 64 位无符号整数（低级，无隐式转换） |
| `f32` | float | `x: f32 = 3.14`, `x = 1e10f32` | 32 位浮点数（低级，无隐式转换） |
| `weak T` | ptr (header) | `weak s` | ARC 管理值的弱引用（不阻止释放） |
| `Regex` | ptr | `/[a-z]+/`, `/\d{3}/` | 正则表达式模式（通过正则字面量语法创建） |
| `Result<T, E>` | `{ i1, T/E }` | `Ok(42)`, `Err(Error("fail"))` | 表示成功（`Ok`）或失败（`Err`）的类型 |
| `Task<T>` | ptr | （由 async 函数返回） | 异步任务句柄（与 `await` 和 `block_on` 配合使用） |
| `Iterator<T>` | ptr | （由 `iter()` 创建） | 用于顺序元素访问的惰性迭代器 |
| `T[N]` | `[N x T]` | `buf: i32[8]` | 固定长度连续数组。低级类型 T 的 N 个元素（栈分配） |

## 类型标注语法

声明变量时可以显式指定类型。当类型可推断时可以省略。

```python
x: int = 42
b: u8 = 255
f: float = 3.14
s: str = "hello"
b: bool = true
opt: Option<int> = Some(10)
t: (int, float) = (1, 3.14)
xs: List<int> = [1, 2, 3]
m: Map<str, int> = {"a": 1}
s: Set<int> = {1, 2, 3}
fn_val: function(int) -> int = (x: int) => x * 2
rx: Regex = /[0-9]+/
u: int | str = 42
a: any = 42
```

## 可用类型名称一览

| 类型名称 | 备注 |
|---|---|
| `int` | 内置标量类型 |
| `u8` | 内置标量类型（无符号 0-255） |
| `float` | 内置标量类型 |
| `bool` | 内置标量类型 |
| `str` | 内置字符串类型 |
| `Unit` | 无返回值函数的返回类型 |
| `Option<T>` | 泛型类型（T 为任意类型） |
| `(T1, T2, ...)` | 元组类型（元素数量和类型组合任意） |
| `List<T>` | 泛型动态数组类型 |
| `Map<K, V>` | 泛型哈希映射类型 |
| `Set<T>` | 泛型集合类型 |
| `function(T1, ...) -> R` | 函数类型 |
| `Error` | 内置错误类型（`message: str`、`code: int`） |
| `any` | 可持有任意基本值（`int`, `float`, `bool`, `str`）或 `Unit` 的内置类型。支持隐式转换：具体值赋值给 `any` 时自动包装，`any` 值赋值给具体类型时自动解包（带运行时类型检查）。支持 `any(int)` → `float` 的自动提升。详见 [any 类型](#any-类型) |
| `T1 \| T2 \| ...` | union 类型（以 `\|` 分隔的多个类型之一） |
| `i8` | 低级 8 位有符号整数（无隐式转换） |
| `i16` | 低级 16 位有符号整数（无隐式转换） |
| `i32` | 低级 32 位有符号整数（无隐式转换） |
| `i64` | 低级 64 位有符号整数（无隐式转换） |
| `u8` | 低级 8 位无符号整数（无隐式转换） |
| `u16` | 低级 16 位无符号整数（无隐式转换） |
| `u32` | 低级 32 位无符号整数（无隐式转换） |
| `u64` | 低级 64 位无符号整数（无隐式转换） |
| `f32` | 低级 32 位浮点数（无隐式转换） |
| `T[N]` | 低级类型 `T` 的 `N` 个元素的固定长度数组。栈分配，连续内存。支持索引读写和 `length()` |
| 用户定义类型名称 | 以 `record` 或 `enum` 关键字声明的类型 |

## 类型别名

`type` 关键字为现有类型创建新的名称。别名与原始类型完全互换。

```python
type Meters = float
type StringList = List<str>

d: Meters = 3.14
names: StringList = ["Alice", "Bob"]
```

> **命名约定**：类型别名名称必须使用 PascalCase（如 `Meters`、`StringList`）。编译器会强制执行此约定。

类型别名也可以用于函数类型、字面量类型和范围类型：

```python
type Callback = function(int, int) -> int

add: Callback = function(a: int, b: int) => a + b
print(add(3, 4))    # 7
```

```python
type Month = 1..12
type Direction = "N" | "S" | "E" | "W"
type Digit = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

m: Month = 6
d: Direction = "N"
n: Digit = 5
```

类型别名也可以以 union 类型（包括基本和用户定义类型）作为目标，且别名的行为与内联 union 完全相同：

```python
type Simple = int | str | bool

x: Simple = 42
y: Simple = "hello"
z: Simple = true

function describe(v: Simple) -> str:
  return to_str(v)
```

union 组件本身是别名的嵌套别名会被透明地展平，且重复成员会被去重。以下三种形式是等价的：

```python
type A = int | str
type B = A | bool          # 与 `int | str | bool` 相同
type C = B | int           # 与 `int | str | bool` 相同（int 被去重）

x: B = 42
y: B = "hello"
z: B = true
```

---

## 数值字面量

### 整数字面量

接受十进制、十六进制（`0x`/`0X`）和二进制（`0b`/`0B`）形式。下划线允许在数字之间作为视觉分隔符（`1_000_000`、`0xFFFF_FFFF`）。

接受的大小由目标类型决定：

| 目标 | 范围 |
|---|---|
| 裸 `int` / `i64` | `-9_223_372_036_854_775_808 .. 9_223_372_036_854_775_807`（i64） |
| `i8` / `i16` / `i32` | 对应的有符号范围 |
| `u8` / `u16` / `u32` | `0 .. 2^N - 1` |
| `u64` | `0 .. 18_446_744_073_709_551_615`（2^64 − 1） |

大型无符号字面量需要后缀（`18446744073709551615u64`）或接收变量上的类型注解（`x: u64 = 18446744073709551615`）。负字面量以一元负号作用于非负数大小，因此 `-1i8` 被接受，而 `-1u8` 被拒绝。

```python
max_u64: u64 = 18446744073709551615     # 2^64 - 1
mask:    u64 = 0xFFFF_FFFF_FFFF_FFFF    # 通过 hex 的相同值
word:    u32 = 4294967295               # 2^32 - 1
```

### 浮点字面量

```text
FloatLiteral := DecDigits '.' DecDigits Exponent? FloatSuffix?
             |  DecDigits Exponent FloatSuffix?
Exponent     := ('e' | 'E') ('+' | '-')? DecDigits
FloatSuffix  := 'f32' | 'f64'
```

任何期望浮点的地方都支持科学记数法：

```python
avogadro  = 6.022e23
planck    = 6.626e-34
light_spd = 2.998E8
big       = 1e10f32
```

溢出指数会产生 `+Inf`/`-Inf`（不是编译错误）。请注意，运行时 `to_float()` 转换器更严格：它在溢出时返回 `Err(Error)` 而不是产生 `+Inf`。

---

## 字面量类型

字面量类型将变量的值限制为特定的常量值。对于常量值，在编译时进行约束检查；对于动态值，在运行时进行约束检查。

### int 字面量类型

```python
x: 42 = 42           # 单一字面量类型
y: 0 | 1 = 0         # int 字面量的 union
z: 0 | 1 = 0
z = 1                     # OK
# z = 2                   # 编译错误（常量）或运行时错误（动态值）
```

### str 字面量类型

```python
dir: "N" | "S" | "E" | "W" = "N"
# @const bad: "N" | "S" = "X"    # 编译错误
```

### 约束检查

- **编译时**：当赋值为常量（`ConstantInt` 或字符串字面量）时，在编译时检查，违反时产生编译错误。
- **运行时**：当值为动态（如函数返回值）时，在运行时检查，违反时程序以错误退出。

---

## 范围类型

范围类型将整数变量的值限制在连续的范围内（包含两端）。

```python
month: 1..12 = 6       # OK
# @const bad: 1..12 = 0       # 编译错误：超出范围
# @const bad: 1..12 = 13      # 编译错误：超出范围

t: -10..10 = -5        # 支持负数范围
```

### 使用可变变量重新赋值（运行时检查）

```python
x: 1..12 = 6
x = 12                      # OK
# x = dynamic_value()       # 运行时检查：超出范围则错误退出
```

### 在函数参数中使用

```python
function set_month(m: 1..12) -> int:
    return m

set_month(6)                # OK
# set_month(13)             # 编译错误（常量参数）
```

---

## `none` 关键字与 Option 类型简写

`none` 关键字表示 Option 类型的值不存在，等同于 `None`。

`T?` 语法是 `Option<T>` 的简写。

```python
x: int? = 42       # 等同于 Option<int>
y: int? = none      # 等同于 None

function find(xs: List<int>, val: int) -> int?:
    for x in xs:
        if x == val:
            return Some(x)
    return none
```

---

## 弱引用（`weak T`）

`weak` 引用是对 ARC 管理值的非持有引用。与强引用不同，弱引用不会递增强引用计数。当最后一个强引用被释放时，被引用的对象会被释放——所有存活的弱引用会自动变为 `None`。

弱引用是用户层面打破引用循环的机制。

### 创建弱引用

在类型标注和表达式位置都使用 `weak` 关键字：

```python
s = "hello"
w: weak str = weak s
```

类型 `weak T` 是一个新的类型构造器，其中 `T` 必须是 ARC 管理的类型（目前为 `str`、`List<T>`、`Map<K, V>`、`Set<T>`）。

### 访问弱引用（升级）

访问弱变量会自动执行**升级**——对强引用计数进行原子检查和递增。结果始终为 `Option<T>`：

- 如果被引用的对象仍然存活（strong count > 0），则为 `Some(value)`
- 如果被引用的对象已被释放（strong count == 0），则为 `None`

```python
s = "alive"
w: weak str = weak s
case w:
  Some(v):
    print(v)           # "alive"
  None:
    print("deallocated")
```

合并运算符（`??`）也可以与弱引用配合使用：

```python
w: weak str = weak s
val = w ?? "default"
```

### 重新赋值

弱引用可以重新赋值。旧的弱引用被释放，新的被保留：

```python
a = "first"
b = "second"
w: weak str = weak a
w = weak b
```

### 线程安全

升级操作在内部使用比较并交换（CAS）循环，因此跨线程使用是安全的。这是必要的，因为强引用可能被并发释放。

### 作用域清理

弱引用在超出作用域时自动释放。如果强引用计数和弱引用计数都达到零，则 ARC 头部会被释放。

---

## F-String（字符串插值）

使用 `f"..."` 语法进行字符串插值。`{}` 内的表达式会被求值并转换为字符串。

```python
name = "world"
print(f"Hello {name}")     # Hello world

a = 1
b = 2
print(f"{a} + {b} = {a + b}")   # 1 + 2 = 3
```

### 插值中支持的类型

`{}` 内可使用求值结果为 `int`、`float`、`bool`、`str`、record 类型、元组或集合类型（`List`、`Map`、`Set`）的任意表达式。

```python
xs = [1, 2, 3]
print(f"items: {xs}")     # items: [1, 2, 3]

t = (1, "hello")
print(f"tuple: {t}")      # tuple: (1, hello)
```

### 转义序列

| 序列 | 输出 |
|---|---|
| `{{` | `{`（字面大括号） |
| `}}` | `}`（字面大括号） |
| `\n` `\r` `\t` `\\` `\"` | 与普通字符串相同 |

```python
print(f"{{braces}}")   # {braces}
```

## 类型转换（`as`）

使用 `as` 关键字进行显式的类型转换。

```python
x = 42 as float     # 42.0
y = 3.14 as int      # 3
z = 1 as bool        # true
s = 42 as str         # "42"
b = 255 as u8         # u8 值 255
```

### 支持的转换

| 来源 | 目标 | 行为 |
|---|---|---|
| `int` | `float` | `SIToFP` |
| `float` | `int` | 截断（`FPToSI`） |
| `int` | `bool` | `0` -> `false`、非零 -> `true` |
| `bool` | `int` | `false` -> `0`、`true` -> `1` |
| `int` / `float` / `bool` | `str` | 字符串表示 |
| `int` | `u8` | 截断（低 8 位） |
| `u8` | `int` | 零扩展 |

| `int` | `i8` / `i16` / `i32` / `i64` | 截断（或 i64 时为恒等） |
| `i8` / `i16` / `i32` / `i64` | `int` | 符号扩展（`SExt`） |
| `int` | `u8` / `u16` / `u32` / `u64` | 截断（或 u64 时为恒等） |
| `u8` / `u16` / `u32` / `u64` | `int` | 零扩展（`ZExt`） |
| 有符号 | 有符号（更宽） | 符号扩展（`SExt`） |
| 有符号 | 有符号（更窄） | 截断 |
| 无符号 | 无符号/有符号（更宽） | 零扩展（`ZExt`） |
| 无符号 | 无符号/有符号（更窄） | 截断 |
| 有符号 / 无符号整数 | `float` | `SIToFP` / `UIToFP` 然后 `f64` |
| `float` | 有符号 / 无符号整数 | `FPToSI` / `FPToUI` |
| `float` | `f32` | `FPTrunc` |
| `f32` | `float` | `FPExt` |
| 有符号整数 | `f32` | `SIToFP` |
| 无符号整数 | `f32` | `UIToFP` |
| `f32` | 有符号 / 无符号整数 | `FPToSI` / `FPToUI` |

`as` 转换的目标类型支持完整的类型语法，包括泛型类型：

```python
x = value as Option<int>
y = data as Map<str, int>
```

任何 `as` 转换（包括泛型）必须是内置转换或具有匹配的用户定义 `operator as`，否则为编译错误。字符串转数值请使用 `to_int()` / `to_float()`。

## 带关联数据的 enum（ADT）

在变体名称后面加上括号并指定类型，enum 变体就可以携带关联数据。不带括号的变体仍然是单纯的标签。

```python
enum Shape:
    Circle(float)
    Rectangle(float, float)
    Point
```

### 命名字段

变体可以选择性地使用命名字段以提高文档清晰度。命名字段使变体定义具有自描述性，但不改变运行时行为——构造和模式匹配仍然是位置性的。

```python
enum Shape:
    Circle(radius: float)
    Rectangle(width: float, height: float)
    Point
```

规则：
- 字段名必须为 `snake_case`。
- 在单个变体内，所有字段必须全部命名或全部未命名（不可混用）。
- 同一变体内的重复字段名是编译错误。

### 构造函数

使用 `EnumName::Variant(value)` 语法构建带有数据的变体。参数始终是位置性的，即使字段有名称也是如此。

```python
c = Shape::Circle(3.14)
r = Shape::Rectangle(4.0, 5.0)
p = Shape::Point
```

### 带绑定的模式匹配

使用 `case EnumName::Variant(binding):` 形式取出关联数据。绑定使用用户选择的变量名，而非字段名。

```python
case c:
    Shape::Circle(r):
        print(r)            # 3.14
    Shape::Rectangle(w, h):
        print(w)
        print(h)
    Shape::Point:
        print("point")
```

### 内部表示

ADT enum 以标签联合的形式存储：`{ i64 tag, [N x i8] data }`，`N` 的大小足以容纳最大变体的载荷。

---

## 泛型 enum

enum 可以使用角括号语法 `<T>` 带有类型参数，使相同的 enum 结构可以持有不同类型的载荷。

```python
enum MyOption<T>:
    MySome(T)
    MyNone
```

### 使用方式

提供具体的类型参数来实例化。当编译器无法推断类型时，需要显式指定类型参数。

```python
a = MyOption<int>::MySome(42)
b = MyOption<int>::MyNone

case a:
    MyOption::MySome(v):
        print(v)      # 42
    MyOption::MyNone:
        print("none")
```

---

## Error 类型

用于错误处理的内置类型。`Error` 具有两个字段：`message`（str）和 `code`（int）。

```python
e = Error("something went wrong")       # code 默认为 0
e2 = Error("not found", 404)            # 显式指定 code

print(e.message)   # something went wrong
print(e2.code)     # 404
print(e2)          # Error: not found (code: 404)
```

### 使用 Result 进行错误处理

可能失败的函数返回 `Result<V, E>`：

```python
function divide(a: int, b: int) -> Result<int, Error>:
    if b == 0:
        return Err(Error("division by zero"))
    return Ok(a // b)

case divide(10, 2):
    Ok(v):
        print(v)            # 5
    Err(e):
        print(e.message)
```

当返回值没有意义时，使用 `Result<Unit, Error>`：

```python
function save(path: str, data: str) -> Result<Unit, Error>:
    return Ok(0 as u8)   # Unit 占位符

case save("/tmp/test.txt", "hello"):
    Ok(_):
        print("saved")
    Err(e):
        print(e.message)
```

### Result 类型

`Result<V, E>` 是一个内置的参数化类型，有两个构造函数：

- `Ok(value)` — 成功变体
- `Err(error)` — 错误变体

与 `case` 配合使用进行穷举的错误处理。`Ok` 和 `Err` 两种情况都必须覆盖（或使用 `_` 通配符）。

**相等性：**
`Result<T, E>` 支持 `==` 和 `!=`。当两个 result 的变体匹配（`Ok`/`Ok` 或 `Err`/`Err`）且内部值相等时，它们相等。

```python
function make_ok(v: int) -> Result<int, Error>: return Ok(v)
make_ok(42) == make_ok(42)   # true
make_ok(1)  == make_ok(2)    # false
make_ok(1)  != Err(Error("e"))  # true
```

**测试匹配器：**
- `expect(x).to_be_ok()` — 断言结果为 `Ok`
- `expect(x).to_be_err()` — 断言结果为 `Err`

### 内部表示

`Error` 以 `{ ptr message, i64 code }` 表示。
`Result<V, E>` 以 `{ i1 isOk, V okValue, E errValue }` 表示。

## Type

`Type` 是内置 [`type_of`](builtins.md#type_of) 函数返回的值。它表示类型的编译时标识，并允许在运行时进行反射比较。

```ry
print(to_str(type_of(42)))          # int
print(to_str(type_of([1, 2, 3])))   # List

print(type_of(42) == type_of(100))  # true
print(type_of(42) == type_of(3.14)) # false
```

关键属性：

- 每个不同的类型定义（基本类型、集合、record、enum、`Option`、`Result`、`function`、`Type` 本身等）在编译时都会获得唯一的标识。
- `Type` 值上的 `==` / `!=` 比较的是标识，而不是显示名称。两个不同的 record（或同名的 record 与 enum）始终可区分。
- `print` 和 `to_str` 显示人类可读的类型名称（例如 `"int"`、`"List"`、`"Point"`、`"i32"`）。
- 低级数值类型（`i8`、`i16`、…、`f32`）与 `int` / `float` 区分。
- 集合泛型折叠为它们的基础名称：`type_of([1, 2])` 返回 `"List"`，而不是 `"List<int>"`。
- `Type` 是反射性的：`type_of(type_of(x))` 返回表示 `Type` 本身的 `Type` 值。

### 内部表示

`Type` 表示为 `{ i64 id, ptr name }`。`id` 字段用于相等性，`name` 字段用于显示。两个字段都在编译时由 `type_of` 填充。

## union 类型

可以使用 `|` 声明可能持有多种类型的变量。

```python
x: int | str = 42
x = "hello"     # 可重新赋值（union 中的任一类型）
print(x)        # hello
```

### 在函数参数与返回值中的使用

```python
function show(x: int | str) -> int:
    print(x)
    return 0

function get_val(flag: bool) -> int | str:
    if flag:
        return 42
    return "hello"
```

### 内部表示

union 类型以 `{ i64 tag, [N x i8] data }` 表示。`tag` 表示各组成类型的索引（按字母顺序排序后），`data` 是最大组成类型大小的字节数组。

### 相等性

union 类型目前支持基本变体（`int`、`float`、`str`、`bool`）的 `==` 和 `!=`。当两个 union 值持有相同变体（相同 tag）且内部值相等时，它们相等。

```python
x: int | str = 42
y: int | str = 42
x == y   # true

z: int | str = "42"
x == z   # false（不同 tag：int vs str）
```

### 约束

- 赋值不属于 union 的类型会产生编译错误
- `int | str` 和 `str | int` 是相同的类型（会被规范化）
- 使用 `print()` 输出 union 值时，会根据运行时的 tag 以适当的类型显示
- `==` 和 `!=` 支持基本变体（`int`、`float`、`str`、`bool`）；不支持 closure 变体

## any 类型

`any` 类型是一种内置的动态类型，可以持有任意基本值。它采用类似 Python 的灵活类型方式——当不需要静态类型保证时，`any` 让您无需使用泛型或 union 类型即可处理多种类型。

### 支持的类型

`any` 可以持有以下类型：

| 类型 | 标签 | 说明 |
|------|------|------|
| `int` | 0 | 64 位有符号整数 |
| `float` | 1 | 64 位浮点数 |
| `bool` | 2 | 布尔值 |
| `str` | 3 | 字符串 |
| `Unit` | 4 | Unit 值（用于无返回值的函数） |

`any` **无法**持有集合类型（`List`、`Map`、`Set`）、资源类型（`TcpListener`、`TcpStream` 等）、函数指针或用户定义类型（`record`、`enum`）。

### 内部表示

`any` 以标签联合实现：

```
{ i64 tag, [8 x i8] data }   // 共 16 字节
```

`tag` 字段标识存储的类型，`data` 字段持有值（最多 8 字节）。

### 包装与解包

具体类型的值在赋值给 `any` 时自动**包装**，`any` 的值在赋值给具体类型时自动**解包**。

```python
# 包装：具体类型 → any
x: any = 42          # int 被包装成 any
x = "hello"          # 可以重新赋值为不同类型

# 解包：any → 具体类型
function get_value() -> any:
    return 42
n: int = get_value()  # any(int) 被解包为 int

# 解包时的 int → float 自动提升
f: float = get_value()  # any(int) 被解包并提升为 float
```

如果运行时类型与目标类型不符（例如将 `any(str)` 解包至 `int` 变量），会产生**运行时错误**。

### 重新赋值

`any` 变量可以重新赋值为任何可持有类型的值：

```python
x: any = 42
x = 3.14       # OK：现在持有 float
x = "hello"    # OK：现在持有 str
x = true       # OK：现在持有 bool
```

### 算术运算

当两个操作数都是 `any` 时，运算会在运行时根据实际类型进行分派：

| 运算 | 类型 | 结果 |
|------|------|------|
| `+` | int + int | int |
| `+` | float + float | float |
| `+` | int + float | float |
| `+` | str + str | str（拼接） |
| `-` | 数值 | int 或 float |
| `*` | 数值 | int 或 float |
| `*` | str * int / int * str | str（重复） |
| `/` | 数值 | float（总是） |
| `//` | int // int | int |
| `//` | 含 float | float |
| `%` | 数值 | int 或 float |
| `**` | 数值 | float（总是） |
| 一元 `-` | int | int |
| 一元 `-` | float | float |

当一个操作数是 `any`、另一个是具体类型时，具体值会在运算前自动包装。

```python
x: any = 10
y: any = x + 20    # 20 被自动包装；结果是 any(int) = 30
```

不兼容的类型组合（例如 `str - int`）会导致**运行时错误**。

### 比较运算

| 运算 | 行为 |
|------|------|
| `==`、`!=` | 相同类型之间有效；int/float 混合比较可行 |
| `<`、`<=`、`>`、`>=` | 数值（int/float 混合可）和字符串（字典序） |

```python
x: any = 3
y: any = 3.0
print(x == y)    # true（int/float 比较）
```

比较时类型不符（例如 `int < str`）会导致**运行时错误**。

### 字符串转换

`any` 值支持 `print()` 和 f-string 插值：

```python
x: any = 42
print(x)              # 42
print(f"value: {x}")  # value: 42
```

转换规则：`int` → 十进制字符串、`float` → `%g` 格式、`bool` → `"true"`/`"false"`、`str` → 原样、`Unit` → `"Unit"`。

### 将 any 传递给有类型的函数

`any` 值可以传递给具有具体参数类型的函数。值会通过运行时类型检查自动解包：

```python
function add_one(x: int) -> int:
    return x + 1

v: any = 42
result = add_one(v)   # any(int) 被解包为 int；结果是 43
```

---

## 类型规则（运算时的类型转换）

| 运算 | 左操作数 | 右操作数 | 结果类型 | 备注 |
|---|---|---|---|---|
| `+` `-` `*` | int | int | int | |
| `+` `-` `*` | u8 | u8 | u8 | 低级类型：原生宽度的无符号运算，无隐式提升 |
| `+` `-` `*` | float 或 int | float 或 int（其中一方为 float） | float | 隐式 float 提升 |
| `/` | 任意数值 | 任意数值 | float | 始终为 float |
| `//` | 任意数值 | 任意数值 | int 或 float | 向下取整除法（向 -∞）；int 操作数结果为 int，含 float 则结果为 float |
| `**` | 任意数值 | 任意数值 | float | 使用 libm `pow` |
| `%` | int | int | int | |
| `%` | float 或 int | float 或 int（其中一方为 float） | float | |
| `+` | str | str | str | 字符串拼接 |
| `==` `!=` `<` `<=` `>` `>=` | str | str | bool | 字典序比较 |
| `==` `!=` `<` `<=` `>` `>=` | 数值或 bool | 数值或 bool | bool | |
| `in` | 任意 | Set<T> | bool | 元素是否包含在集合中 |
| `&` `\|` `^` `~` `<<` `>>` | int | int | int | 对 float 会产生错误 |
| `+` `-` `*` | i32 | i32 | i32 | 低级类型：无隐式转换，需要相同类型 |
| `/` `//` | i32 | i32 | i32 | 有符号整数除法（`SDiv`） |
| `/` `//` | u32 | u32 | u32 | 无符号整数除法（`UDiv`） |
| `%` | i32 | i32 | i32 | 有符号取余（`SRem`） |
| `%` | u32 | u32 | u32 | 无符号取余（`URem`） |
| `+` `-` `*` `/` | f32 | f32 | f32 | |
| `==` `!=` | i32/u32 | i32/u32 | bool | 符号无关的相等比较 |
| `<` `<=` `>` `>=` | i32 | i32 | bool | 有符号比较（`ICMP_SLT` 等） |
| `<` `<=` `>` `>=` | u32 | u32 | bool | 无符号比较（`ICMP_ULT` 等） |
| `>>` | i32 | i32 | i32 | 算术右移（保留符号位） |
| `>>` | u32 | u32 | u32 | 逻辑右移（零填充） |
| `**` | 低级 | 任意 | 错误 | 低级类型不支持幂运算符 |
| 混合 | 低级 | 不同 | 错误 | 混合低级和高级类型是编译错误 |

### 转义序列（str 字面值内）

| 序列 | 含义 |
|---|---|
| `\n` | 换行 |
| `\r` | 回车 |
| `\t` | 制表符 |
| `\\` | 反斜杠 |
| `\"` | 双引号 |
| `\0` | 空字符 |

## 类型安全约束

- **隐式拓宽转换** — 函数调用中支持安全的拓宽转换：`u8` → `int`、`u8` → `float`、`int` → `float`。对于二元运算符，混合 `int` 和 `float` 会触发 float 提升。`u8` 是低级类型，使用原生宽度的无符号运算；在二元运算符中混合 `u8` 与 `int` 是编译错误。窄化转换（例如 `float` → `int`）不允许隐式进行。从 `int` 字面量到 `u8` 的窄化转换仅在带有类型标注 `b: u8 = 42` 时允许。
- **变量类型在声明时固定** — 一旦以 `int` 声明的变量，就无法重新赋值为 `float`。
- **位运算仅限 `int`** — 对 `float` 或 `bool` 使用位运算会产生编译错误。
- **非 `bool` 类型也可用于条件式** — `if` 的条件式可使用 `int`（0 = false、非 0 = true）等 `bool` 以外的类型。
- **数值字面量分隔符** — 下划线可作为数值字面量中的视觉分隔符：`100_000`、`0xFF_FF`、`0b1010_0101`、`3.14_159`。下划线必须出现在数字之间（不允许前导、尾随或连续的下划线）。
- **数值字面量后缀** — 低级类型可通过字面量后缀指定：`42i32`、`255u8`、`3.14f32`、`.5f32`、`0xFFu8`、`0b1010u8`。带有 float 后缀的整数字面量（`42f32`）会产生浮点值。带有整数后缀的浮点字面量（`3.14i32`）是编译错误。超出范围的值（例如 `256u8`、`129i8`）也是编译错误。
- **低级数值类型（`i8`、`i16`、`i32`、`i64`、`u8`、`u16`、`u32`、`u64`、`f32`）无隐式转换** — 混合低级类型之间或低级与高级类型（`int`、`float`）之间会产生编译错误。请使用显式 `as` 转换。低级整数的 `/` 运算符执行整数除法（类似 Rust），而非浮点除法。有符号类型使用 `SDiv`/`SRem`，无符号类型使用 `UDiv`/`URem`。
- **有符号与无符号** — 有符号类型（`i8`、`i16`、`i32`、`i64`）使用有符号比较（`ICMP_SLT` 等）和算术右移（`AShr`）。无符号类型（`u8`、`u16`、`u32`、`u64`）使用无符号比较（`ICMP_ULT` 等）和逻辑右移（`LShr`）。`>>>` 运算符无论符号性如何，始终执行逻辑移位。
- **`int` 算术溢出是运行时错误** — 高级 `int` 类型的算术运算（`+`、`-`、`*`、一元 `-`）在溢出时产生运行时错误，类似 Swift 的默认行为。这防止了补码回绕导致的静默数据损坏。溢出的常量表达式在编译时被捕获。
- **低级整数溢出会回绕** — 低级整数类型的算术运算在溢出时使用 Ry 定义的补码回绕（有符号）或模运算（无符号）。例如，`2147483647i32 + 1i32` 会回绕为 `-2147483648`。如需显式溢出控制，请使用 `checked_add/sub/mul`（返回 `Result<T, Error>`）、`saturating_add/sub/mul`（钳制到类型边界）或 `wrapping_add/sub/mul`（自文档化的回绕行为）。参见[函数参考](functions.md#checkedsaturating-arithmetic)。
