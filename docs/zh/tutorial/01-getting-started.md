[English](../../tutorial/01-getting-started.md) | [日本語](../../ja/tutorial/01-getting-started.md) | [简体中文](01-getting-started.md)

# 01 - 入门

下一篇教程 -> [02 - 变量与类型](02-variables-and-types.md)

---

## 前提条件

要构建并运行 Ry，需要以下环境：

- **LLVM 21**
- **CMake 3.20 以上**
- **支持 C++17 的编译器**（GCC 7+ / Clang 5+ 等）

---

## 构建步骤

在仓库根目录下执行以下命令：

```bash
cmake -B build -DLLVM_DIR=/usr/local/llvm/lib/cmake/llvm
cmake --build build
```

构建成功后，会生成 `build/ry` 可执行文件。

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
./build/ry hello.ry
```

输出：

```
Hello, World!
```

也可以通过管道或 Here-document 从标准输入执行代码：

```bash
echo 'print("Hello, World!")' | ./build/ry

./build/ry <<'RY'
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
