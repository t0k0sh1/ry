[English](../../tutorial/01-getting-started.md) | [日本語](../../ja/tutorial/01-getting-started.md) | [简体中文](01-getting-started.md)

# 01 - 入门

下一篇教程 -> [02 - 变量与类型](02-variables-and-types.md)

---

## 安装

### 快速安装（macOS Apple Silicon）

```bash
curl -fsSL https://raw.githubusercontent.com/t0k0sh1/ry/main/install.sh | sh
```

`ry` 二进制文件将安装到 `~/.local/bin`，标准库将安装到 `~/.ry/lib/std/`。

请确保 `~/.local/bin` 已添加到 `PATH`：

```bash
export PATH="$HOME/.local/bin:$PATH"
```

如需从源代码构建或在其他平台安装，请参阅 [README 的安装部分](../../../README.md#installation)。

---

## 项目初始化

使用 `ry new` 命令创建新项目：

```bash
ry new my-project
cd my-project
```

这将生成以下文件和目录：

- `package.toml` -- 项目配置文件
- `src/main.ry` -- 入口点（附带示例代码）

若要将当前目录初始化为项目，请使用 `ry init`：

```bash
mkdir my-project
cd my-project
ry init
```

详情请参阅[项目管理](../reference/project.md)。

---

## 第一个程序

将以下内容保存为 `hello.ry` 文件：

```python
print("Hello, World!")
```

使用以下命令运行：

```bash
ry hello.ry
```

输出：

```
Hello, World!
```

也可以通过管道或 Here-document 从标准输入执行代码：

```bash
echo 'print("Hello, World!")' | ry

ry <<'RY'
print("Hello, World!")
RY
```

---

## 注释的写法

从 `#` 到行尾的内容会被视为注释。

```python
# 这是一段注释
print("Hello")  # 也可以在行尾添加注释
```

注释不会影响代码的执行。

---

下一篇教程 -> [02 - 变量与类型](02-variables-and-types.md)
