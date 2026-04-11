[English](../../reference/packages.md) | [日本語](../../ja/reference/packages.md) | [简体中文](packages.md)

# 包参考

## 概述

Ry 使用包系统来组织代码。**包**可以是单个 `.ry` 文件，或是包含多个 `.ry` 文件的目录。使用 `from` 语句导入包。

`std` 包（标准库）会自动导入到每个程序中。

---

## 导入语法

### 导入全部定义

```python
from math
```

导入包内的所有函数与类型。

### 选择性导入

```python
from math import sqrt
```

仅导入指定的定义。

### 多重选择性导入

```python
from math import sqrt, PI
```

以逗号分隔选择性导入多个定义。

### 相对导入

```python
from .helper import greet
```

从当前文件目录的相对位置导入模块。`.` 前缀将解析限制为仅当前目录（不搜索标准库和其他搜索路径）。

### 从子目录相对导入

```python
from .utils import helper_fn
from .utils.calc import add
```

从当前文件目录的子目录导入。

### 从当前目录相对导入全部

```python
from . import add, sub
```

从当前目录包中导入指定符号（目录中的所有 `.ry` 文件，排除 `_` 前缀和 `.test.ry` 文件）。

---

## 包解析

以点号分隔的包名称按以下方式解析：

| 导入语句 | 解析结果 |
|---|---|
| `from math` | `math/` 目录（包）或 `math.ry` 文件 |
| `from utils.math` | `utils/math/` 目录或 `utils/math.ry` 文件 |
| `from str` | `str/` 目录或 `str.ry` 文件 |

### 解析顺序

对于每个搜索路径：
1. **目录** (`{path}/`) — 若存在，加载目录内的所有 `.ry` 文件（包）
2. **文件** (`{path}.ry`) — 单个文件（向后兼容）

### 目录包

当包解析为目录时：
- 目录内的所有 `.ry` 文件会自动加载
- 以 `_` 开头的文件会被排除
- 测试文件（`.test.ry`）会被排除
- 不需要特殊的入口文件（如 `__init__.py`）
- 目录内文件中定义的所有函数与类型都会被导出

### 私有符号

名称以 `_`（下划线）开头的定义为包内部的私有符号，无法被导入：

- 通配导入（`from pkg`）会自动排除 `_` 前缀的符号
- 具名导入（`from pkg import _helper`）会产生编译错误

```python
# mylib/internal.ry
function _helper() -> int:     # 私有 — 无法导入
    return 42
function public_api() -> int:  # 公开 — 可导入
    return _helper()
```

```
mypackage/
  calc.ry      # function add(), function sub()
  string.ry    # function concat()
```

```python
from mypackage          # 导入 add, sub, concat
from mypackage import add   # 仅导入 add
```

---

## 标准库（`std`）

`std` 包会自动导入到每个程序中。提供的功能：
- 内建函数（`print`、`length`、`range` 等）
- 字符串函数（`contains`、`find`、`replace` 等）
- 类型转换函数（`to_int`、`to_float`、`to_str`）
- 集合函数（`map`、`filter`、`sort` 等）

### 子包

以下子包需要明确导入：

| 包 | 说明 |
|------|------|
| [`math`](math.md) | 数学常量与函数 |
| [`io`](io.md) | 文件 I/O、标准输入、字节转换 |
| [`path`](path.md) | 文件路径操作（join、basename、dirname 等） |

```python
from math import sqrt, PI, sin
```

也可以直接从标准库的包中明确导入特定定义：

```python
from str import contains
```

### RY_HOME

标准库安装于 `$RY_HOME/share/std/`。`RY_HOME` 的默认值为 `~/.ry`。

```bash
export RY_HOME="$HOME/.ry"   # default
```

### RY_ENV

`RY_ENV` 环境变量控制运行时环境模式。也可以使用 `--env=<value>` CLI 标志。

| 值 | 别名 | `.env` 加载 | lib 搜索 |
|---|------|-----------|---------|
| `prod` | `production` | 禁用 | 仓库构建的项目覆盖 → `$RY_HOME/share`（回退至 `lib`）→ `exe/../share`（回退至 `lib`）→ `exe/share`（回退至 `lib`） |
| `dev` | `development` | `.env.dev` → `.env` | 与 `prod` 相同 |
| `test` | — | `.env.test` → `.env` | 与 `prod` 相同 |
| `staging` | — | `.env.staging` → `.env` | 与 `prod` 相同 |
| `internal` | — | `.env.internal` → `.env` | 仓库构建的项目覆盖 → `exe/../share`（回退至 `lib`）→ `exe/share`（回退至 `lib`）（跳过 `$RY_HOME`） |
| （未设置）（默认） | — | 仅 `.env` | 与 `prod` 相同 |

别名会自动解析为规范形式。例如 `RY_ENV=production` 会被规范化为 `prod`。

在 `prod` 模式下，出于安全考虑不会加载 `.env` 文件——生产环境的机密信息应通过基础设施级别的环境变量管理（CI/CD、密钥管理器等）。

其他模式下，先加载 `.env.<env>`（若存在），再加载 `.env`。由于不会覆盖已存在的环境变量，环境专属的值会优先生效。

```bash
# 简写形式（推荐）
RY_ENV=dev ./build/ry app.ry

# 完整名称（向后兼容）
RY_ENV=development ./build/ry app.ry

# CLI 标志
./build/ry --env=dev test

# prod 模式：不加载 .env
RY_ENV=prod ./build/ry app.ry

# 开发 Ry 自身时的额外隔离
RY_ENV=internal ./build/ry test
```

当在 Ry 源码树中构建 `ry` 可执行文件时，它可以使用项目 `package.toml` 中的仓库本地 stdlib 覆盖。这使得仓库构建与签出的 `share/std` 保持一致，即使 `~/.ry/share/std` 版本较旧。已安装的 `ry` 二进制文件会忽略该覆盖，继续使用 `$RY_HOME/share/std`。

---

## 搜索路径优先级

1. 导入来源文件所在的目录
2. 使用仓库构建的 `ry` 时，来自当前 Ry 签出的仓库本地 stdlib 覆盖
3. `$RY_HOME/share`（标准库位置，对于旧版安装会回退至 `$RY_HOME/lib`）
4. 可执行文件相对的 `share/` 目录（对于旧版布局会回退至 `lib/`）
5. `RY_PATH` 环境变量中包含的路径（以冒号分隔）

---

## RY_PATH 环境变量

在 `RY_PATH` 中以冒号分隔指定目录，即可添加到包搜索路径。

```bash
export RY_PATH="/usr/local/ry/lib:/home/user/ry-packages"
```

---

## 约束

| 约束 | 详细 |
|------|------|
| 可使用的位置 | 仅限顶层（函数或块内不可） |
| 重复导入 | 自动跳过（不会产生错误） |
| 循环导入 | 编译错误 |
| 相对导入 | `from .` 和 `from .pkg` 仅在当前文件目录中解析 |
| 父目录导入 | 不支持 `from ..` |
| 包名称 | 仅允许字母、数字和下划线（不允许连字符） |

```python
# 错误示例：在块内导入
function main():
    from math   # Error: imports only allowed at top level

# OK：多次导入相同包不会产生错误
from math
from math   # Skipped
```

---

## 创建包文件

### 单文件包

```python
# calc.ry
function add(a: int, b: int) -> int:
    return a + b

function sub(a: int, b: int) -> int:
    return a - b
```

```python
# main.ry
from calc import add, sub

print(add(1, 2))   # 3
print(sub(5, 3))   # 2
```

### 目录包

```
mylib/
  calc.ry
  string.ry
```

```python
# main.ry
from mylib import add, concat
```

---

## 原生函数命名约定

实作为 C 执行时函数的标准库包函数遵循 `__ry_<package>_<function_name>` 约定。

> **注意**：此约定适用于标准库包函数（例如 `base64`、`filesystem`、`path`）。内建函数（例如 `print`、`length`）和 math 函数使用不同的实作（内联 LLVM IR、libc 呼叫），不遵循此命名模式。

### 格式

```text
__ry_<package>_<function_name>
```

### 规则

1. **前缀**：`__ry_`
2. **包**：包名称（例如 `from base64 import encode` 中的 `base64`）
3. **函数名称**：在 Ry 中宣告的 snake_case 函数名称
4. **重载**：当函数有多个不同 arity 的重载时，将参数数量附加为后缀（例如 `__ry_path_join2`、`__ry_path_join3`）
5. **错误获取器**：每个返回 `Result` 类型的包都提供 `__ry_<pkg>_get_last_error`

### 范例

| Ry 宣告 | C 执行时函数名称 |
|---------------|------------------------|
| `base64::encode(data: str) -> str` | `__ry_base64_encode` |
| `filesystem::list_dir(path: str) -> Result<List<str>, Error>` | `__ry_filesystem_list_dir` |
| `path::join(a: str, b: str) -> str` | `__ry_path_join2` |
| `path::join(a: str, b: str, c: str) -> str` | `__ry_path_join3` |
