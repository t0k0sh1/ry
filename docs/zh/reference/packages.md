[English](../../reference/packages.md) | [日本語](../../ja/reference/packages.md) | [繁體中文](packages.md)

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
- 不需要特殊的入口文件（如 `__init__.py`）
- 目录内文件中定义的所有函数与类型都会被导出

### 私有符号

名称以 `_`（下划线）开头的定义为包内部的私有符号，无法被导入：

- 通配导入（`from pkg`）会自动排除 `_` 前缀的符号
- 具名导入（`from pkg import _helper`）会产生编译错误

```python
# mylib/internal.ry
fn _helper() -> int:     # 私有 — 无法导入
    return 42
fn public_api() -> int:  # 公开 — 可导入
    return _helper()
```

```
mypackage/
  calc.ry      # fn add(), fn sub()
  string.ry    # fn concat()
```

```python
from mypackage          # 导入 add, sub, concat
from mypackage import add   # 仅导入 add
```

---

## 标准库 (`std`)

`std` 包会自动导入到每个程序中。提供的功能：
- 内建函数（`print`, `length`, `range` 等）
- 字符串函数（`contains`, `find`, `replace` 等）
- 类型转换函数（`to_int`, `to_float`, `to_str`）
- 集合函数（`map`, `filter`, `sort` 等）

### 子包

以下子包需要明确导入：

| 包 | 说明 |
|------|------|
| [`math`](math.md) | 数学常量与函数 |
| [`io`](io.md) | 文件 I/O、标准输入、字节转换 |

```python
from math import sqrt, PI, sin
```

也可以直接从标准库的包中明确导入特定定义：

```python
from str import contains
```

### RY_HOME

标准库安装于 `$RY_HOME/lib/std/`。`RY_HOME` 的默认值为 `~/.ry`。

```bash
export RY_HOME="$HOME/.ry"   # default
```

### RY_ENV

`RY_ENV` 环境变量控制运行时环境模式。也可以使用 `--env=<value>` CLI 标志。

| 值 | 别名 | `.env` 加载 | lib 搜索 |
|---|------|-----------|---------|
| `prod` | `production` | 禁用 | 仓库构建的项目覆盖 → `$RY_HOME/lib` → `exe/../lib` → `exe/lib` |
| `dev` | `development` | `.env.dev` → `.env` | 与 `prod` 相同 |
| `test` | — | `.env.test` → `.env` | 与 `prod` 相同 |
| `staging` | — | `.env.staging` → `.env` | 与 `prod` 相同 |
| `internal` | — | `.env.internal` → `.env` | 仓库构建的项目覆盖 → `exe/../lib` → `exe/lib`（跳过 `$RY_HOME`） |
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

当在 Ry 源码树中构建 `ry` 可执行文件时，它可以使用项目 `package.toml` 中的仓库本地 stdlib 覆盖。这使得仓库构建与签出的 `lib/std` 保持一致，即使 `~/.ry/lib/std` 版本较旧。已安装的 `ry` 二进制文件会忽略该覆盖，继续使用 `$RY_HOME/lib/std`。

---

## 搜索路径优先级

1. 导入来源文件所在的目录
2. 使用仓库构建的 `ry` 时，来自当前 Ry 签出的仓库本地 stdlib 覆盖
3. `$RY_HOME/lib`（标准库位置）
4. 可执行文件相对的 `lib/` 目录
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

```python
# 错误示例：在块内导入
fn main():
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
fn add(a: int, b: int) -> int:
    return a + b

fn sub(a: int, b: int) -> int:
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
