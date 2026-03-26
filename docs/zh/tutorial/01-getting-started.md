[English](../../tutorial/01-getting-started.md) | [日本語](../../ja/tutorial/01-getting-started.md) | [繁體中文](01-getting-started.md)

# 01 - 入門

下一個教學 → [02 - 變數與型別](02-variables-and-types.md)

---

## 必要環境

要建置並執行 Ry，您需要以下環境：

- **LLVM 21**
- **CMake 3.20 以上**
- **支援 C++17 的編譯器**（GCC 7+ / Clang 5+ 等）

---

## 建置步驟

在儲存庫根目錄下執行以下指令：

```bash
cmake -B build -DLLVM_DIR=/usr/local/llvm/lib/cmake/llvm
cmake --build build
```

建置成功後，會產生 `build/ry` 執行檔。

---

## 專案初始化

使用 `ry new` 指令建立新專案。

```bash
ry new my-project
cd my-project
```

這將產生以下檔案和目錄：

- `ry.toml` — 專案設定檔
- `src/main.ry` — 進入點（附帶範例程式碼）

若要將當前目錄初始化為專案，請使用 `ry init`：

```bash
mkdir my-project
cd my-project
ry init
```

詳情請參閱[專案管理](../reference/project.md)。

---

## 第一個程式

將以下內容儲存為 `hello.ry` 檔案。

```python
print("Hello, World!")
```

使用以下指令執行：

```bash
./build/ry hello.ry
```

輸出：

```
Hello, World!
```

也可以透過管道或 Here-document 從標準輸入執行程式碼：

```bash
echo 'print("Hello, World!")' | ./build/ry

./build/ry <<'RY'
print("Hello, World!")
RY
```

---

## 註解的寫法

從 `#` 到行尾的內容會被視為註解。

```python
# 這是一段註解
print("Hello")  # 也可以在行尾加上註解
```

註解不會影響程式碼的執行。

---

下一個教學 → [02 - 變數與型別](02-variables-and-types.md)
