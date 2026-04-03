[English](README.md) | [日本語](README.ja.md) | [繁體中文](README.zh.md)

<p align="center">
  <img src="docs/logo.png" alt="Ry" width="200">
</p>

<p align="center">
  基于 LLVM JIT 的简洁编程语言。<br>
  读取源代码后，通过 LLVM ORC JIT 编译为原生代码并即时执行。
</p>

## 特性

- **LLVM JIT 编译** — 使用 ORC LLJIT 实现高速原生执行
- **丰富的类型系统** — `int`、`float`、`bool`、`str`、`Option<T>`、`Error`、元组、`List<T>`、`Map<K,V>`、`Set<T>`、`enum`、函数类型、用户自定义结构体
- **运算符** — 算术、比较、逻辑、位运算（`>>>` 逻辑右移）、复合赋值、`in` / `not in`、字符串重复（`"ab" * 3`）、`as` 类型转换，支持运算符重载
- **F-String** — 使用 `f"Hello {name}"` 进行字符串插值
- **契约式设计** — `require`（前置条件）、`ensure`（后置条件）、`invariant`（结构体不变量）、`old()`、`result`
- **指令** — `@deprecated` 编译时元数据注解
- **函数** — `function` 定义、递归、重载、Lambda（闭包）、高阶函数、UFCS
- **控制流** — `if`/`else`、`when`、`while`、`for...in`、`break`/`continue`
- **文件 I/O** — 文件读写、字节操作、标准输入（`std.io`）
- **文件系统** — 目录列表、递归遍历、glob、复制、移动、删除、权限管理（`std.filesystem`）
- **包管理** — 基于目录的包、自动导入的 `std` 标准库、`from ... import ...`
- **并发** — `async`/`await` 与 work-stealing 调度器、`@parallel` for 循环、原生线程 API（`std.thread`）
- **类型安全** — 类型推断、类型注解、不可变类型绑定、`@const` 指令

## 示例代码

```python
# 变量与类型
x: int = 42
name: str = "hello"
pi = 3.14159

# 函数定义
function factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)

print(factorial(5))    # 120

# Lambda 与闭包
offset = 10
add_offset = (x: int) -> int => x + offset
print(add_offset(5))   # 15

# 结构体
record Point:
    x: int
    y: int

function operator+(a: Point, b: Point) -> Point:
    return Point(a.x + b.x, a.y + b.y)

p = Point(1, 2) + Point(3, 4)
print(p.x)             # 4

# 集合类型
xs = [1, 2, 3]
m = {"a": 1, "b": 2}
s = {1, 2, 3}

for x in xs:
    print(x)

print(2 in s)          # true
print(m["a"])           # 1

# 流式操作 (filter, map, sort)
result = [5, 3, 1, 4, 2].filter((x: int) => x > 1).map((x: int) => x * 10).sort()
print(result)          # [20, 30, 40, 50]

# 枚举类型
enum Color:
    Red
    Green
    Blue

c = Color::Red
print(c)               # Red

# 包导入
from math import sqrt, PI
print(sqrt(PI))
```

## 安装

### 一行命令（macOS Apple Silicon）

```bash
curl -fsSL https://raw.githubusercontent.com/t0k0sh1/ry/main/install.sh | sh
```

指定特定版本：

```bash
curl -fsSL https://raw.githubusercontent.com/t0k0sh1/ry/main/install.sh | sh -s v0.0.4
```

默认安装至 `~/.local/bin`。可通过 `RY_INSTALL_DIR` 环境变量更改安装位置。

标准库安装至 `$RY_HOME/lib/std/`（默认：`~/.ry/lib/std/`）。

### 从源代码构建

环境要求：
- LLVM 21
- CMake 3.20+
- 支持 C++17 的编译器

```bash
cmake -B build -DLLVM_DIR=/usr/local/llvm/lib/cmake/llvm
cmake --build build
```

## 使用方法

```bash
ry <file.ry>              # 运行 Ry 脚本
echo '<code>' | ry -c      # 从标准输入运行代码
ry test [options] [path]   # 运行测试 (*.test.ry)
ry init                    # 在当前目录初始化项目
ry new <name>              # 创建新项目
ry run [<script-name>]     # 运行项目脚本
ry fmt [options] [path]    # 格式化源文件
ry self-update             # 更新 ry 本身
```

`self-update` 命令使用 Ed25519 签名验证和 SHA-256 校验和来验证发布工件。签名验证默认为必需。如果签名文件不可用，更新将中止。设置 `RY_SKIP_SIGNATURE=1` 可在签名文件缺失时允许继续（不推荐）。无效签名始终会中止更新，无论此设置如何。

标准输入也支持 here-document：

```bash
ry -c <<'RY'
a = 1
b = 2
print(a + b)
RY
```

运行 `ry <command> --help` 查看详细选项。

为了进行内部执行分析，Ry 还支持结构化跟踪模式：

```bash
ry --trace app/main.ry
ry --trace-out=/tmp/ry-trace.jsonl app/main.ry
ry test --trace tests/spec
```

`--trace` 默认将 JSON Lines 输出到 stderr。使用 `--trace-out` 可将跟踪流重定向到文件，同时保持程序的 stdout 不变。

## 开发

```bash
cd build && ctest --output-on-failure
```

## 文档

详细的语言规格与教程请参阅 [docs/](docs/zh/README.md)。
