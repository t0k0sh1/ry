[English](../../reference/functions.md) | [日本語](../../ja/reference/functions.md) | [简体中文](functions.md)

# 函数参考

## 函数定义语法

```python
fn function_name(param_name: type, ...) -> return_type:
    # body
    return value
```

- 参数类型可省略。省略时视为 `any` 类型。
- 返回类型可省略。省略时会从**函数体推断**（命名函数和 lambda 均如此）。若无 `return` 语句则推断为 `Unit`。若要显式允许任意返回类型，请指定 `-> any`。
- 函数体为缩进的代码块。
- 具有显式返回类型（`Unit` 和 `any` 除外）的函数，必须在所有控制流路径中包含 `return` 语句。若缺少则会产生编译错误。
- 函数可以定义 `require`（前置条件）和 `ensure`（后置条件）子句。参阅 [契约式设计](contracts.md)。

> **命名约定**：函数名称和参数名称必须使用 snake_case（如 `add`、`get_value`、`map_list`）。编译器会强制执行此约定。

```python
fn add(a: int, b: int) -> int:
    return a + b

fn greet(name: str) -> Unit:
    print("Hello, " + name)   # 返回类型为 Unit（显式）
```

---

## 参数与返回值的类型

| 项目 | 说明 |
|---|---|
| 参数类型 | 可省略。省略 `: type` 时默认为 `any` |
| 返回类型 | 可省略。省略时从函数体推断（若无 `return` 语句则为 `Unit`） |
| `Unit` | 不返回值的函数的返回类型 |

> **注意**：函数参数是**不可变的**。不能在函数体内对参数重新赋值。这确保了入口时的参数值始终可用于后置条件检查（参阅 [契约式设计](contracts.md)）。

```python
fn no_return(x: int) -> Unit:  # 返回类型 Unit（显式）
    print(x)

fn get_value() -> int:     # 返回类型 int
    return 42

fn identity(x) -> any:    # 参数类型 any（省略）
    return x
```

### 类型省略与 `any`

当参数的类型标注被省略时，该参数被视为 `any` ——一种在运行时接受任意基本值的动态类型。这类似于 Python 的无类型参数。

```python
# 所有参数默认为 any
fn add(a, b):
    return a + b

add(1, 2)              # 3（int + int）
add("hello", " world") # "hello world"（str + str）
add(1, 2.0)            # 3.0（int + float）
```

也可以在类型标注中显式使用 `any`：

```python
fn identity(x: any) -> any:
    return x
```

### 返回类型推断

当返回类型省略时，会从函数体中的 `return` 语句推断：

```python
fn double(x: int):     # 返回类型推断为 int
    return x * 2

fn greet(name: str):   # 返回类型推断为 Unit（无 return）
    print("Hello, " + name)
```

若要显式允许任意返回类型，请使用 `-> any`：

```python
fn flexible(x: any) -> any:
    return x    # 可以返回 int、float、str 等
```

---

## 递归

函数可以调用自身。

```python
fn factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)
```

### 尾调用优化

编译器会自动检测自递归尾调用——即函数的最后一个操作是调用自身——并应用 LLVM 的 `musttail` 优化。这保证了尾递归函数使用常量栈空间，防止深度递归时的栈溢出。

```python
fn sum_to(n: int, acc: int) -> int:
    if n <= 0:
        return acc
    return sum_to(n - 1, acc + n)    # 尾调用 → 被优化

sum_to(1000000, 0)    # 不会栈溢出
```

**尾调用优化的条件：**

- 函数在 `return` 语句中直接调用自身（`return f(args)`）
- 调用结果不经过任何进一步计算直接返回（`return n * f(n-1)` 不是尾调用）
- 函数没有 `ensure`（后置条件）子句

互递归（A 调用 B，B 调用 A）目前不会被优化。

---

## 重载

可以定义参数数量或类型不同的同名函数。

### 规则

- 参数的数量或类型不同即可定义同名函数。
- 调用时会根据参数的类型和数量选择适当的函数。
- 仅返回类型不同的重载是不允许的。

```python
fn area(side: int) -> int:
    return side * side

fn area(w: int, h: int) -> int:
    return w * h

a = area(5)       # 25
b = area(3, 4)    # 12
```

### 解析优先级

当多个重载匹配一个调用时，编译器使用以下优先级（从高到低）选择最具体的重载：

1. **精确类型匹配** — 参数类型与形参类型完全匹配
2. **隐式拓宽** — 安全的拓宽转换（`u8` → `int`、`u8` → `float`、`int` → `float`）
3. **union 类型匹配** — 参数类型是 union 形参类型的成员
4. **`any` 类型匹配** — 形参类型是 `any`（接受任何值）

精确匹配数量最多的重载胜出。如果两个或更多重载具有相同的具体程度，编译器会报告歧义错误。

低级数值类型（`i8`、`i16`、`i32`、`i64`、`u8`–`u64`、`f32`）**不**参与隐式拓宽——需要显式 `as` 转换。

```python
fn process(x: int) -> str:
    return "int"

fn process(x) -> str:          # x: any
    return "any"

process(42)       # "int" — 精确匹配（int）优于 any
process("hello")  # "any" — str 没有精确匹配，回退到 any
```

```python
fn double(x: float) -> float:
    return x * 2.0

double(5)         # OK — int 隐式拓宽为 float，返回 10.0
```

---

## 默认参数

参数可以有默认值，允许调用者省略尾部的参数。

### 语法

```python
fn connect(host: str, port: int = 8080, timeout: int = 30):
    # ...

connect("localhost")                    # port=8080, timeout=30
connect("localhost", 3000)              # port=3000, timeout=30
connect("localhost", 3000, 10000)       # port=3000, timeout=10000
```

### 规则

- 默认参数必须放在所有非默认参数之后。
- 有默认值的参数**必须**有显式的类型标注（例如：`x: int = 10`；`x = 10` 是编译错误）。
- 默认值必须是编译时常量表达式（字面量和 `@const` 变量）。
- 如果默认参数导致模糊的重载（参数数量范围重叠且类型匹配），编译器会报告错误。

```python
# 错误：模糊的重载
fn calc(x: int, y: int = 0) -> int:
    return x + y
fn calc(x: int) -> int:      # 与上面的 calc(int) 冲突
    return x * 2
```

### 限制

- **泛型函数**和 **lambda 表达式**不支持默认参数。

---

## Unit 类型函数

不返回值的函数会返回 `Unit`。返回类型可以省略（推断为 `Unit`）或用 `-> Unit` 显式指定。

```python
fn log(msg: str) -> Unit:
    print(msg)
```

---

## Task 与异步函数

`Task<T>` 是用于并发工作的内置句柄类型。`async fn` 返回 `Task<T>`，`await` 在另一个 `async fn` 内部提取 `T`，`block_on(task)` 从同步上下文中阻塞直到任务完成。

```python
async fn add(a: int, b: int) -> int:
    return a + b

# 从同步上下文中，使用 block_on()
t: Task<int> = add(20, 22)
print(block_on(t))                  # 42
block_on(add(1, 2))                 # 等待并丢弃结果

# 在 async fn 内部，使用 await
async fn double_add(a: int, b: int) -> int:
    return (await add(a, b)) * 2
```

### 规则

- `async fn name(...) -> T:` 使用等待结果类型 `T` 声明。
- 调用 `async fn` 会立即返回 `Task<T>`。
- `await expr` 要求 `expr` 为 `Task<T>` 并产生 `T`。
- `await` 只能在 `async fn` 内部使用。从同步上下文中使用 `block_on(task)`。
- `block_on(task)` 阻塞当前线程直到任务完成并返回结果。
- 支持 `async fn ... -> Unit`；当不产生值时，`block_on(task)` 是等待的主要方式。
- 任务在运行时工作线程池上运行；不是每个任务一个操作系统线程。
- v1 不支持 `async` lambda 和 `async @native fn`。

---

## Lambda 函数

可以内联定义匿名函数。

### 语法

```python
# 单一表达式（返回类型从表达式推断）
fn(param_name: type, ...) => expression

# 参数类型可省略（默认为 any）
fn(param_name, ...) => expression

# 多行代码块
fn(param_name: type, ...):
    # 多个语句
    return value

# 带显式返回类型（可选）
fn(param_name: type, ...) -> return_type => expression
```

### 示例

```python
double = fn(x: int) => x * 2
result = double(5)   # 10

add = fn(a: int, b: int) => a + b
sum = add(3, 4)      # 7

# 多行 lambda
abs = fn(x: int):
    if x < 0:
        return -x
    return x
```

---

## 闭包

Lambda 函数会以**值捕获**定义时外层作用域的变量。

```python
base = 10
add_base = fn(x: int) => x + base   # 以值捕获 base

base = 99          # 不影响已捕获的值
r = add_base(5)   # 15（使用捕获时的 base = 10）
```

### 捕获规则

| 项目 | 内容 |
|---|---|
| 捕获方式 | 值捕获（复制） |
| 捕获时机 | Lambda 定义时 |
| 外层变量修改的影响 | 无（因为是复制） |

---

## 函数类型

用于将函数作为值处理的类型。

### 语法

```python
fn(param_type1, param_type2, ...) -> return_type
```

### 示例

```python
f: fn(int) -> int = fn(x: int) => x * 2
g: fn(int, int) -> int = fn(a: int, b: int) => a + b

fn apply(func: fn(int) -> int, x: int) -> int:
    return func(x)

result = apply(f, 5)   # 10
```

---

## 高阶函数

可以接收函数作为参数，或将函数作为返回值返回。

```python
fn map_list(xs: List<int>, f: fn(int) -> int) -> List<int>:
    result: List<int> = []
    for x in xs:
        result += [f(x)]
    return result

doubled = map_list([1, 2, 3], fn(x: int) => x * 2)
# [2, 4, 6]
```

---

## 泛型函数

函数可以有类型参数，实现类型安全的复用而无需代码重复。

### 语法

```python
fn name<T, U>(param1: T, param2: U) -> T:
    # 使用 T、U 作为类型的函数体
```

### 示例

```python
fn identity<T>(x: T) -> T:
    return x

# 显式类型参数
result = identity[int](42)      # 42
result = identity[str]("hello") # "hello"

# 类型推断（从实际参数推导类型参数）
result = identity(42)           # T = int, result = 42
result = identity("hello")     # T = str, result = "hello"
```

### 多个类型参数

```python
fn pick_first<T, U>(a: T, b: U) -> T:
    return a

result = pick_first(1, "x")       # T = int, U = str, result = 1
result = pick_first("hello", 42)  # T = str, U = int, result = "hello"
```

### 类型约束（边界）

类型参数可以使用 `: RecordName` 语法以 record 类型进行约束。具体类型必须是绑定类型本身或其子类型。

```python
record Animal:
    name: str
    legs: int

record Dog < Animal:
    breed: str

fn get_name<T: Animal>(a: T) -> str:
    return a.name

get_name(Dog("Rex", 4, "Lab"))  # OK — Dog 是 Animal 的子类型
get_name(Animal("Cat", 4))      # OK — 精确类型匹配
```

有约束和无约束的类型参数可以混合使用：

```python
fn pair_name<T: Animal, U>(a: T, x: U) -> str:
    return a.name
```

### 工作原理

泛型函数使用**单态化**：为每个唯一的类型参数组合生成函数的特化版本。相同的实例化会被缓存并在多次调用间复用。当存在类型约束时，会在实例化时进行验证。

---

## UFCS（统一函数调用语法）

可以使用 `a.f(b)` 的形式调用 `f(a, b)`。方便用于方法链。

### 语法

```python
# 普通调用
f(a, b)

# UFCS 调用（等价）
a.f(b)
```

### 链接

```python
fn double(x: int) -> int:
    return x * 2

fn add_one(x: int) -> int:
    return x + 1

result = 5.double().add_one()   # double(5) -> 10, add_one(10) -> 11
```

### 与字段访问混用

字段访问（`.field`）和 UFCS（`.method()`）使用相同的点号记法，但通过是否有参数来区分。

```python
p = Point(3, 4)
length = p.x.to_float()   # 字段访问 + UFCS
```

---

## 运算符重载

可以为用户定义类型定义运算符的行为。

### 语法

```python
# 二元运算符（2 个参数）
fn operator<op>(a: type, b: type) -> return_type:
    ...

# 一元运算符（1 个参数）
fn operator<op>(a: type) -> return_type:
    ...
```

### 可重载的运算符

| 类别 | 运算符 |
|---|---|
| 算术（二元） | `+` `-` `*` `/` `%` `**` `//` |
| 比较（二元） | `==` `!=` `<` `<=` `>` `>=` |
| 位运算（二元） | `&` `\|` `^` `<<` `>>` |
| 逻辑（二元） | `and` `or` |
| 成员测试 | `in` |
| 下标 | `[]`（读取）、`[]=`（写入） |
| 调用 | `()` |
| 转换 | `as` |
| 一元 | `-` `~` `not` |

### 返回类型约束

比较、逻辑和成员测试运算符必须返回 `bool`：

| 类别 | 运算符 | 必需返回类型 |
|---|---|---|
| 比较 | `==` `!=` `<` `<=` `>` `>=` | `bool` |
| 逻辑 | `and` `or` `not` | `bool` |
| 成员测试 | `in` | `bool` |
| 转换 | `as` | 必需（目标类型） |

```python
# OK
fn operator==(a: Vec2, b: Vec2) -> bool:
    return a.x == b.x and a.y == b.y

# Error: comparison operator '==' must return 'bool', but returns 'int'
fn operator==(a: Vec2, b: Vec2) -> int:
    return 42
```

算术运算符和位运算符没有返回类型约束。

### 二元 / 一元的区别

依参数个数区分。

```python
record Vec2:
    x: float
    y: float

# 二元 +
fn operator+(a: Vec2, b: Vec2) -> Vec2:
    return Vec2(a.x + b.x, a.y + b.y)

# 一元 -
fn operator-(v: Vec2) -> Vec2:
    return Vec2(-v.x, -v.y)

# 比较
fn operator==(a: Vec2, b: Vec2) -> bool:
    return a.x == b.x and a.y == b.y

v1 = Vec2(1.0, 2.0)
v2 = Vec2(3.0, 4.0)
v3 = v1 + v2    # Vec2(4.0, 6.0)
v4 = -v1        # Vec2(-1.0, -2.0)
```

---

## 检查/饱和算术

用于低级整数类型（`i8`、`i16`、`i32`、`i64`、`u8`、`u16`、`u32`、`u64`）的显式溢出控制内置函数。两个参数必须是相同类型。

| 函数 | 返回值 | 行为 |
|----------|---------|----------|
| `checked_add(a, b)` | `Result<T, Error>` | 溢出时返回 `Err` |
| `checked_sub(a, b)` | `Result<T, Error>` | 下溢时返回 `Err` |
| `checked_mul(a, b)` | `Result<T, Error>` | 溢出时返回 `Err` |
| `saturating_add(a, b)` | `T` | 溢出时钳制到类型最小/最大值 |
| `saturating_sub(a, b)` | `T` | 下溢时钳制到类型最小/最大值 |
| `saturating_mul(a, b)` | `T` | 溢出时钳制到类型最小/最大值 |
| `wrapping_add(a, b)` | `T` | 显式回绕（与 `+` 相同） |
| `wrapping_sub(a, b)` | `T` | 显式回绕（与 `-` 相同） |
| `wrapping_mul(a, b)` | `T` | 显式回绕（与 `*` 相同） |

```python
# Checked：返回 Result，使用 match 或 ? 处理
r = checked_add(2147483647i32, 1i32)
match r:
  case Ok(v):
    print(v)
  case Err(e):
    print("overflow!")   # 输出 "overflow!"

# Saturating：钳制到边界
v = saturating_add(2147483647i32, 100i32)
print(v as int)   # 2147483647

# Wrapping：自文档化的回绕行为
v = wrapping_add(2147483647i32, 1i32)
print(v as int)   # -2147483648
```

> **注意**：这些函数不支持浮点类型（`f32`）或高级 `int` 类型。低级整数的默认 `+`、`-`、`*` 运算符使用回绕行为（有符号为补码，无符号为模运算）。
