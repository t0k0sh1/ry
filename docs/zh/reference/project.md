[English](../../reference/project.md) | [日本語](../../ja/reference/project.md) | [简体中文](project.md)

# 项目管理

## CLI 概述

```bash
ry <file.ry> [args...]              # 运行 Ry 脚本
echo '<code>' | ry                  # 从标准输入运行代码
ry test [options] [<file> | <dir>]  # 运行测试
ry init                             # 初始化项目
ry new <project-name>               # 创建新项目
ry fmt [options] [<file> | <dir>]   # 格式化源文件
ry self-update [options]            # 更新 ry 本身
```

### 全局选项

| 选项 | 说明 |
|---|---|
| `-h`, `--help` | 显示帮助 |
| `-v`, `--version` | 显示版本 |
| `--env=<env>` | 设置环境（`production`\|`development`\|`internal`）。覆盖 `RY_ENV` 环境变量。 |

### 标准输入执行

当没有给出文件参数且标准输入不是终端时，`ry` 从标准输入读取源代码并执行：

```bash
echo 'print("hello")' | ry
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

### TOML 子集规范

`package.toml` 支持以下 TOML 子集。

- 部分头：`[section]`
- 键值对：`key = "value"`（仅字符串值）
- 注释：从 `#` 到行尾
- 空行会被忽略
