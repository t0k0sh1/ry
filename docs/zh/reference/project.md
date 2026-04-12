[English](../../reference/project.md) | [日本語](../../ja/reference/project.md) | [简体中文](project.md)

# 项目管理

## CLI 概述

```bash
ry <file.ry> [args...]              # 运行 Ry 脚本
echo '<code>' | ry -c               # 从标准输入运行代码
ry test [options] [<file> | <dir>]  # 运行测试
ry init                             # 初始化项目
ry new <project-name>               # 创建新项目
ry run [<script-name>]              # 运行项目脚本
ry fmt [options] [<file> | <dir>]   # 格式化源文件
ry self-update [options]            # 更新 ry 本身
```

### 全局选项

| 选项 | 说明 |
|---|---|
| `-c` | 从标准输入读取并执行代码 |
| `-h`, `--help` | 显示帮助 |
| `-v`, `--version` | 显示版本 |
| `--env=<env>` | 设置环境（`production`\|`development`\|`internal`）。覆盖 `RY_ENV` 环境变量。 |

### 入口点执行

当没有给出文件参数时，`ry` 在当前目录（或父目录）中查找 `package.toml`，并运行 `entry` 字段指定的文件：

```bash
ry                        # 运行入口文件（例如 src/main.ry）
ry -- arg1 arg2           # 运行入口文件并传入参数
```

如果未找到 `package.toml` 或未设置 `entry` 字段，`ry` 会打印帮助信息并退出。

### 裸文件名

如果第一个参数是**单一路径组件**且其名称以 `.ry` 结尾（例如 `main.ry`），并且当前工作目录中不存在该名称的文件，`ry` 会按以下顺序在最近的 `package.toml` 项目中搜索：**首先**是项目根目录（包含 `package.toml` 的目录），**然后**是 `[paths]` 下列出的每个目录（按键名字母顺序排列；以 `_` 开头的键被保留并在此搜索中忽略，`_dev_stdlib` 单独处理）。**第一个**存在的常规文件优先（例如，`package.toml` 旁的 `foo.ry` 优先于 `src/foo.ry`，当两者同时存在时）。如果都没有匹配，`ry` 会报告该文件不存在并列出尝试过的路径。不带 `.ry` 后缀的标记不会以这种方式解析（因此误输入的子命令仍会显示 “unknown command”）。

如果参数是**包含多个组件的路径**（例如 `src/foo.ry` 或 `./foo.ry`），并且该路径不存在，`ry` 会报告**文件不存在**，而不是将其视为未知子命令。

当 `<file.ry>` 是基本文件名且不存在于当前目录中时，相同的规则同样适用于 `ry test <file.ry>`。

### 标准输入执行

使用 `-c` 标志从标准输入读取并执行代码：

```bash
echo 'print("hello")' | ry -c
```

---

## `ry init` - 项目初始化

将当前目录初始化为 Ry 项目。

```bash
ry init
```

### 生成的文件与目录

```
my-project/
  package.toml          # 项目配置文件
  src/
    main.ry        # 入口点（示例代码）
```

### 行为

1. 若 `package.toml` 已存在则错误退出
2. 创建 `src/` 目录（若不存在）
3. 生成 `package.toml`（`name` 为当前目录名称）
4. 生成 `src/main.ry`（若已存在则跳过）

---

## `ry new` - 创建新项目

创建新目录并将其初始化为 Ry 项目。

```bash
ry new my-project
```

### 生成的文件与目录

```
my-project/
  package.toml          # 项目配置文件
  src/
    main.ry        # 入口点（示例代码）
```

### 行为

1. 若未指定项目名称则错误退出
2. 若同名目录已存在则错误退出
3. 创建 `<project-name>/` 目录
4. 在其中创建 `src/` 目录
5. 生成 `package.toml`（`name` 为指定的项目名称）
6. 生成 `src/main.ry`

---

## `ry run` - 运行项目脚本

执行 `package.toml` 的 `[scripts]` 部分中定义的脚本。

```bash
ry run              # 列出所有可用脚本
ry run build        # 运行 "build" 脚本
ry run test         # 运行 "test" 脚本
```

### 行为

1. 从当前目录向上搜索 `package.toml`
2. 不带参数时，列出所有可用脚本及其命令
3. 带脚本名称时，通过 `/bin/sh -c` 执行对应的 shell 命令
4. 所执行命令的退出码会被传播
5. 如果脚本名称未找到，显示错误并列出可用脚本

### 注意事项

- 不需要 LLVM 初始化（快速启动）
- 命令在当前工作目录中执行
- 由于命令通过 shell 运行，因此支持 shell 功能（管道、重定向等）

---

## `ry fmt` - 代码格式化工具

以一致的 2 空格缩进和规范风格格式化 `.ry` 源代码文件。

```bash
ry fmt                     # 格式化项目中所有 .ry 文件（需要 package.toml）
ry fmt src/main.ry         # 格式化单个文件
ry fmt src/                # 递归格式化目录中所有 .ry 文件
ry fmt --check             # 检查文件是否已格式化（未格式化则 exit 1）
ry fmt --check src/        # 检查指定目录
```

### 格式化规则

- 每个代码块层级使用 2 空格缩进
- 二元运算符前后加空格（`a + b`，而非 `a+b`）
- 逗号后加空格（`f(a, b)`，而非 `f(a,b)`）
- 顶层定义之间（函数、记录、枚举）加空行
- 注释会被保留

### 行为

1. 读取源代码文件，解析为 AST，并以规范格式重新输出
2. 将格式化结果写回文件（就地修改）
3. 使用 `--check` 时仅报告未格式化的文件，若存在则以代码 1 退出（适用于 CI）
4. 递归格式化时跳过 `.git/`、`build/`、`node_modules/` 目录

### 注意事项

- 不需要 LLVM 初始化（快速启动）
- 复合赋值运算符（`+=`、`-=` 等）因解析器会进行脱糖，格式化后会变成展开形式（`x = x + expr`）
- 十六进制（`0xFF`）和二进制（`0b1010`）数字字面量会被转换为十进制表示

---

## `ry test` - 运行测试

发现并运行测试文件（`*.test.ry`）。完整的测试语法文档请参阅[测试](testing.md)。

```bash
ry test                        # 自动发现并运行所有 *.test.ry 文件
ry test tests/spec             # 运行目录下所有测试
ry test test_file.ry           # 运行指定的测试文件
ry test -p                     # 并行运行测试
ry test -w                     # 监视模式：文件变更时重新运行
ry test --coverage             # 收集行覆盖率信息
```

### 选项

| 选项 | 说明 |
|---|---|
| `-p`, `--parallel` | 并行运行测试 |
| `-w`, `--watch` | 监视变更并重新运行 |
| `--coverage`, `--cov` | 收集覆盖率信息 |
| `-h`, `--help` | 显示帮助 |

### 行为

1. 无参数时，搜索 `package.toml` 以找到项目根目录，并递归发现 `*.test.ry` 文件（跳过 `.git`、`build`、`node_modules`）
2. 所有测试通过时退出码为 0，有失败时为 1
3. `--coverage` 与 `--parallel` 一起使用时会回退到顺序执行

---

## `ry self-update` - 自我更新

将 ry 本身更新至最新版本。从 GitHub Releases 下载二进制文件并替换当前的可执行文件。

```bash
ry self-update              # 更新至最新稳定版
ry self-update --nightly    # 更新至最新 nightly 预发布版
ry self-update v0.0.1       # 更新至指定版本
```

### 行为

1. 显示当前版本
2. 根据参数解析更新目标版本
   - 无参数：GitHub 的最新稳定发行版（`/releases/latest`）
   - `--nightly`：最新的预发布版
   - 指定版本：指定标签的发行版
3. 若与当前版本相同，则以 `"Already up to date."` 退出
4. 下载二进制文件并替换当前的可执行文件

### 安全性

发行版归档文件通过两个步骤进行验证：

1. **真实性**：使用内嵌的 Ed25519 公钥验证 `checksums.txt.sig` 文件。
   - 如果签名文件**不存在**，除非设置了 `RY_SKIP_SIGNATURE=1`，否则更新将被中止。
   - 如果签名文件**存在但无效**，无论是否设置 `RY_SKIP_SIGNATURE`，更新都将被中止。
2. **完整性**：将归档文件的 SHA-256 哈希值与 `checksums.txt` 进行对比。

若签名文件缺失且仍希望继续更新（不推荐），可以设置 `RY_SKIP_SIGNATURE=1`。但如果签名文件存在且签名无效，则无论该变量是否设置，更新都会被中止。

### 注意事项

- 执行需要 `curl` 和 `tar` 命令
- 若因权限不足导致二进制文件替换失败，会显示建议使用 `sudo` 的消息（不会自动执行 sudo）
- 下载会先在临时目录中进行；但若跨文件系统的 `cp` 回退操作被中断，目标二进制文件可能处于不完整状态

---

## `package.toml` 配置文件

以 TOML 格式描述项目的元数据与路径设置。

```toml
[project]
name = "my-project"
version = "0.1.0"
entry = "src/main.ry"

[paths]
src = "src"

[scripts]
build = "cmake --preset default && cmake --build build"
test = "./build/ry_tests"
clean = "rm -rf build"
```

### `[project]` 部分

| 键 | 说明 |
|------|------|
| `name` | 项目名称（初始化时为目录名称） |
| `version` | 版本字符串 |
| `entry` | 作为入口点的源代码文件 |

### `[paths]` 部分

| 键 | 说明 |
|------|------|
| `src` | 源代码目录 |
| (其他键) | 额外的项目相对目录。值不能是绝对路径，也不能包含 `..`。这些目录与 `src` 一起，用于解析 `ry <file>` 和 `ry test <file>` 的**裸文件名**（参见上文的**裸文件名**）。 |
| `_dev_stdlib` | 可选；标准库位置的开发覆盖（参见工具文档）。不用于裸文件名解析。 |

### `[scripts]` 部分

定义可通过 `ry run <name>` 执行的具名脚本。每个键是脚本名称，值是 shell 命令字符串。

| 键 | 说明 |
|------|------|
| `<name>` | 要执行的 shell 命令（通过 `ry run <name>` 运行） |

### TOML 子集规范

`package.toml` 支持以下 TOML 子集。

- 部分头：`[section]`
- 键值对：`key = "value"`（仅字符串值）
- 注释：从 `#` 到行尾
- 空行会被忽略
