[English](../../reference/directives.md) | [日本語](../../ja/reference/directives.md) | [繁體中文](directives.md)

# 指令

指令是可以附加到声明上的编译时元数据注解。使用 `@name` 语法，类似于 Java 注解。

## 语法

```
@name
@name(key=value, ...)
```

指令放置在目标声明之前。可以堆叠多个指令。

## 支持的目标

指令可以应用到以下声明:

- `function` - 函数定义（包括用 `@it` / `@describe` 装饰的具名测试函数）
- `record` - 记录定义
- 变量声明（使用 `@const` 或普通赋值）
- `record` 定义内的字段
- `for` - 仅限计数循环，用于 `@parallel`
- `it` / `describe` 调用（旧 lambda 形式）- 用于 `@each` 和 `@property` 的测试用例和测试组

## 内建指令

### `@deprecated`

将声明标记为已弃用。当已弃用的实体被使用（调用、引用或访问）时，会发出编译时警告。

**应用于函数:**

```
@deprecated
function old_function() -> int:
    return 42

print(old_function())   # warning: 'old_function' is deprecated
```

**应用于类型:**

```
@deprecated
record OldPoint:
    x: int
    y: int

@const
p = OldPoint(1, 2)  # warning: 'OldPoint' is deprecated
```

**应用于变量:**

```
@deprecated
@const
old_value = 99

print(old_value)         # warning: 'old_value' is deprecated
```

**应用于字段:**

```
record Config:
    @deprecated
    old_setting: int
    new_setting: int

@const
c = Config(1, 2)
print(c.old_setting)     # warning: 'Config.old_setting' is deprecated
print(c.new_setting)     # no warning
```

### `@const`

将变量标记为不可变。使用 `@const` 声明的变量在初始化后无法重新赋值。未使用 `@const` 时，变量默认为可变。

```
@const
x = 42
# x = 10   # Error: cannot reassign @const variable
```

**搭配类型注解:**

```
@const
name: str = "hello"
```

**元组解构:**

```
@const
a, b = (1, 2)
```

**顶层 `@const` 和函数。** 顶层 `@const` 声明对同一源文件中之后定义的任何顶层函数都可见，并且不可变性会对每次引用强制执行 — 包括通过顶层 `@const` 记录进行的字段变更。详细信息请参阅 [functions.md](functions.md) 中的「函数体内的顶层变量与 `@const`」章节。

### `@native`

声明由运行时提供实现的函数。该函数不能有函数体。

可选的字符串参数指定共享库模块名称。当调用 `@native("libname")` 函数时，JIT 会动态加载相应的共享库（macOS 上为 `libry_<libname>.dylib`，Linux 上为 `libry_<libname>.so`）并从中解析运行时符号：

```ry
@native              # 内建（静态链接到进程中）
@native("base64")    # 从 libry_base64.dylib/.so 动态加载
```

**基本语法:**

```
@native
function contains(string: str, substring: str) -> bool

print(contains("hello world", "world"))  # true
```

**运算符重载:**

```
@native
function operator+(a: str, b: str) -> str

print("hello" + " world")  # hello world
```

**与 UFCS 搭配使用:**

```
@native
function to_upper(string: str) -> str

print("hello".to_upper())  # HELLO
```

**参数数量验证:**

当 `@native` 声明包含类型签名时，编译器会在调用处验证参数数量。支持重载函数（例如：1、2、3 个参数的 `range`），只要任一重载匹配即通过验证。

```
@native
function range(n: int) -> List<int>
@native
function range(start: int, end: int) -> List<int>

print(length(range(5)))       # OK: matches 1-arg overload
print(length(range(1, 10)))   # OK: matches 2-arg overload
print(length(range()))        # Error: expects 1 or 2 argument(s), but got 0
```

**标准库声明 (`core/`):**

`core/` 目录包含所有内建函数的 `@native` 声明，按类别组织：

| 文件 | 内容 |
|---|---|
| `core/builtins.ry` | `print`, `length`, `range`, `enumerate`, `zip`, `exit`, `args`, `available_parallelism`, `sleep` |
| `core/str.ry` | `contains`, `starts_with`, `ends_with`, `find`, `substring`, `char_at`, `replace`, `to_upper`, `to_lower`, `trim`, `trim_start`, `trim_end`, `repeat`, `reverse`, `split`, `join` |
| `core/convert.ry` | `to_int`, `to_float`, `to_str` |
| `core/list.ry` | `append`, `pop`, `insert`, `remove_at`, `slice`, `distinct`, `flatten`, `sort`, `first`, `last`, `is_empty` |
| `core/map.ry` | `keys`, `values`, `items`, `has_key`, `get`, `merge` |
| `core/set.ry` | `add`, `remove`, `union`, `intersection`, `difference`, `symmetric_difference`, `is_subset`, `is_superset` |
| `core/higher_order.ry` | `filter`, `map`, `reduce`, `fold`, `any`, `all`, `sum`, `min`, `max` |

当 `ry` 可执行文件附近存在 `core/` 目录时，这些文件会作为前置自动加载。前置机制使得内建函数调用时的参数数量验证生效。

**约束:**
- `@native` 函数不能有函数体（签名后不能加 `:`）。
- 提供函数体会导致解析错误: `@native function must not have a body`。
- 对于裸 `@native`，声明的函数必须对应到现有的内建函数，否则调用时会在编译时失败。对于 `@native("libname")`，函数会基于声明的签名编译，如果无法从加载的库中解析符号，将在 JIT 链接时失败。

**库规范:**
- `@native("libname")` 指定原生函数位于名为 `libry_<libname>.dylib`（macOS）或 `libry_<libname>.so`（Linux）的共享库中。在 JIT 启动时，会按以下搜索路径顺序加载所需的共享库：
  1. `exe/../lib/` — 已安装布局
  2. `exe/lib/` — 开发/构建布局
  3. `$RY_HOME/lib/` — 用户安装环境
- `@native`（静态）和 `@native("libname")`（动态）声明都会注册以进行参数数量验证和调用解析。区别仅在于运行时符号如何提供给 JIT。
- 运行时函数名称遵循 `__ry_<libname>_<fn_name>` 约定（例如 `@native("base64") fn encode(...)` → `__ry_base64_encode`）。这适用于标准库包和用户定义的原生库。

### `@parallel`

将计数 `for` 循环标记为并行执行。

```
@parallel
for i in range(8):
    work(i)
```

**支持的目标:**

- 仅限 `for` 语句

**约束:**

- 一个 `for` 语句上只允许使用一个 `@parallel` 指令。
- 可迭代对象必须是 `range(...)` 或整数 `..` 范围。
- 不支持解构迭代。
- 禁止对外部可变变量赋值。
- v1 中禁止在循环体内使用 `break`、`continue`、索引赋值和字段赋值。

### `@each`

启用参数化测试，以不同参数多次运行测试。

**语法（具名函数，推荐）:**

```ry
@each([(arg1, arg2, ...), ...])
@it("should handle {0} and {1}")
function test_handle(param1: type, param2: type):
    # test body
```

**语法（旧 `it` lambda）:**

```ry
@each([(arg1, arg2, ...), ...])
it("should handle {0} and {1}", (param1: type, param2: type):
    # test body
)
```

参数可以是任何求值为元组列表的表达式，包括函数调用：

```ry
@each(make_inputs())
@it("should handle {0}")
function test_handle(x: int):
    # test body
```

**支持的目标:** 用 `@it` 装饰的函数，或旧的 `it` 调用。

**约束:**
- 参数必须求值为元组列表
- 元组的元素数量必须与函数参数数量匹配
- 描述字符串中的 `{0}`, `{1}`, ... 会被替换为参数的字符串表示

### `@property`

启用基于属性的测试，为测试生成随机输入。

**语法（具名函数，推荐）:**

```ry
@property(count=100)
@it("should verify property name")
function test_property(a: int, b: int):
    # test body with random values
```

**语法（旧 `it` lambda）:**

```ry
@property(count=100)
it("should verify property name", (a: int, b: int):
    # test body with random values
)
```

**支持的目标:** 用 `@it` 装饰的函数，或旧的 `it` 调用。

**参数:**

| 参数 | 类型 | 默认值 | 说明 |
|-----------|------|---------|-------------|
| `count` | int | 100 | 随机试验次数 |

**支持的参数类型:**

| 类型 | 范围 |
|------|-------|
| `int` | -1000 到 1000 |
| `float` | -1000.0 到 1000.0 |
| `bool` | true 或 false |
| `str` | 随机 ASCII、0-20 字符 |

失败时会显示反例（导致失败的参数值）。

### `@it`

通过装饰具名函数来声明测试用例。函数体成为测试体，由 `ry test` 执行。完整规范请参阅 [测试参考](testing.md)。

**语法:**

```ry
@it("description")
function test_name():
    # 断言
```

**基本示例:**

```ry
@it("should add 1 + 2 = 3")
function test_add():
    expect(1 + 2).to_eq(3)
```

**与 `@each` 或 `@property` 组合:**

```ry
@each([(1, 2, 3), (0, 0, 0), (-1, 1, 0)])
@it("should add {0} + {1} = {2}")
function test_add_each(a: int, b: int, expected: int):
    expect(a + b).to_eq(expected)

@property(count=100)
@it("should verify addition is commutative")
function test_commutative(a: int, b: int):
    expect(a + b).to_eq(b + a)
```

**支持的目标:** 仅 `function` 声明。函数不能有返回类型注解。

**约束:**
- 仅在用 `ry test` 执行的 `*.test.ry` 文件中有效
- 与 `@each` 组合时，函数的参数列表必须与元组元素数量匹配
- 与 `@property` 组合时，每个参数类型必须是支持的生成器类型之一（`int`、`float`、`bool`、`str`）

### `@describe`

通过装饰具名函数将一组相关测试分组。在主体中声明的内部 `@it` 函数属于该组，主体中直接声明的变量充当被每个内部 `@it` 捕获的共享设置。与旧 lambda 形式不同，`@describe` 组**可以嵌套**；输出按嵌套深度成比例缩进。

**语法:**

```ry
@describe("group name")
function group_name():
    @it("nested test")
    function test_nested():
        # 断言
```

**基本示例:**

```ry
@describe("arithmetic")
function arithmetic_tests():
    @it("should subtract")
    function test_sub():
        expect(10 - 3).to_eq(7)

    @it("should multiply")
    function test_mul():
        expect(4 * 5).to_eq(20)
```

**共享设置:**

外层 `@describe` 主体中声明的变量会被每个内部 `@it` 函数自动捕获。

```ry
@describe("shared setup")
function shared_setup_tests():
    base = 100
    offset = 5

    @it("should use base")
    function test_base():
        expect(base).to_eq(100)

    @it("should use base and offset")
    function test_combined():
        expect(base + offset).to_eq(105)
```

**嵌套组:**

```ry
@describe("outer")
function outer():
    @describe("inner")
    function inner():
        @it("should pass deeply nested test")
        function test_deep():
            expect(1 + 1).to_eq(2)
```

**支持的目标:** 仅 `function` 声明。函数不能有参数或返回类型注解。

### `@inline`

为 LLVM 优化器提供内联提示。默认情况下，标记函数进行积极内联。

**基本用法（始终内联）：**

```
@inline
function add(a: int, b: int) -> int:
    return a + b
```

**带 mode 参数：**

```
@inline(mode="always")
function hot_path(x: int) -> int:
    return x * 2 + 1

@inline(mode="hint")
function medium_path(x: int) -> int:
    return x + 1

@inline(mode="never")
function cold_error_handler(msg: str):
    print("ERROR: " + msg)
```

**模式：**

| 模式 | LLVM 属性 | 说明 |
|------|----------|------|
| `always`（默认） | `AlwaysInline` | 始终内联此函数 |
| `hint` | `InlineHint` | 向优化器建议内联 |
| `never` | `NoInline` | 禁止内联此函数 |

**约束：**
- `@inline` 不能与 `@native` 一起使用（native 函数没有可内联的函数体）。
- 未知的 mode 值会导致编译错误。

### 参数（未来扩展）

指令支持可选的参数语法，为未来扩展做准备:

```
@deprecated(reason="use new_api instead")
function old_api() -> int:
    return 0
```

目前，参数会被解析但不会被 `@deprecated` 指令使用。

## 注意事项

- 已弃用的实体仍然正常运作，仅会发出警告。
- 警告在使用点发出，不在定义点发出。
- 定义已弃用的实体但不使用它，不会产生警告。
- 未知的指令名称会导致解析错误。
- 在不支持的目标（如 `if`、`while`）上使用指令会导致解析错误。`@parallel` 是唯一支持在 `for` 上使用的指令。
