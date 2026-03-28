[English](01-getting-started.md) | [日本語](../ja/tutorial/01-getting-started.md) | [繁體中文](../zh/tutorial/01-getting-started.md)

# 01 - Getting Started

Next tutorial -> [02 - Variables and Types](02-variables-and-types.md)

---

## Prerequisites

To build and run Ry, you need the following:

- **LLVM 21**
- **CMake 3.20 or later**
- **A C++17 compatible compiler** (GCC 7+ / Clang 5+, etc.)

---

## Build Instructions

Run the following commands from the repository root:

```bash
cmake -B build -DLLVM_DIR=/usr/local/llvm/lib/cmake/llvm
cmake --build build
```

On a successful build, an executable named `build/ry` will be generated.

---

## Project Initialization

You can create a new project with the `ry new` command:

```bash
ry new my-project
cd my-project
```

This generates the following files and directories:

- `package.toml` -- Project configuration file
- `src/main.ry` -- Entry point (with sample code)

To initialize the current directory as a project instead, use `ry init`:

```bash
mkdir my-project
cd my-project
ry init
```

See [Project Management](../reference/project.md) for details.

---

## Your First Program

Save the following content to a file named `hello.ry`:

```python
print("Hello, World!")
```

Run it with the following command:

```bash
./build/ry hello.ry
```

Output:

```
Hello, World!
```

You can also run code from stdin using a pipe or here-document:

```bash
echo 'print("Hello, World!")' | ./build/ry

./build/ry <<'RY'
print("Hello, World!")
RY
```

---

## Writing Comments

Everything from `#` to the end of the line is treated as a comment.

```python
# This is a comment
print("Hello")  # End-of-line comments are also supported
```

Comments do not affect the behavior of the code.

---

Next tutorial -> [02 - Variables and Types](02-variables-and-types.md)
