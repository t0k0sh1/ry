[English](../../tutorial/09-modules.md) | [日本語](../../ja/tutorial/09-modules.md) | [简体中文](09-modules.md)

# 包

[<- 上一篇：错误处理](08-error-handling.md) | [下一篇：并发 ->](10-concurrency.md)

Ry 使用包系统来管理跨文件和目录的代码。详细规格请参阅[包参考手册](../reference/packages.md)。

---

## from/import 语法

使用 `from` 语法导入其他文件的函数。

```python
from math import sqrt, PI   # 选择性导入
from math                    # 全部导入（所有定义）
```

这样就可以使用 `math.ry` 中定义的函数。

---

## 子目录（点分隔路径）

使用点分隔路径指定子目录中的包。

```python
from utils.calc import add   # 从 utils/calc.ry 导入
```

每个点对应一层目录分隔。

---

## 目录包

包可以是单一的 `.ry` 文件，也可以是包含多个 `.ry` 文件的目录。当包解析为目录时，其中所有的 `.ry` 文件会自动加载。

```
mypackage/
  calc.ry      # function add(), function sub()
  string.ry    # function concat()
```

```python
from mypackage              # 导入 add、sub、concat
from mypackage import add   # 仅导入 add
```

不需要特殊的入口文件（如 `__init__.py`）。以 `_` 开头的文件会被排除。

---

## 相对导入

使用前导 `.` 相对于当前文件所在目录进行导入。这对需要从同级模块导入的测试文件特别有用。

```python
from .helper import greet       # 从同目录的 helper.ry 导入
from .utils import add          # 从 utils/ 子目录导入
from .utils.calc import mul     # 从 utils/calc/ 嵌套子目录导入
from . import add, sub          # 从当前目录包导入符号
```

相对导入**仅**相对于当前文件所在目录解析 —— 不会搜索标准库和其他搜索路径。这可以防止当你的项目中存在与标准库包同名的模块时发生名称冲突。

```python
# 如果你的项目有 src/math/stats.ry：
from .math import mean    # 始终解析为你的本地 math 包
from math import sqrt     # 解析为标准库的 math 包
```

> **注意：** 不支持父目录导入（`from ..`）。

---

## 标准库（`std`）

`std` 包会自动导入到所有程序中。不需要编写 `from std` —— 它始终可用。

```python
# 这些函数无需导入即可使用
print("hello")
n = length("world")
xs = range(5)
```

也可以从标准库的包中显式导入特定定义：

```python
from str import contains
```

### RY_HOME

標準庫安裝在 `$RY_HOME/share/std/`。`RY_HOME` 的默認值為 `~/.ry`。

```bash
export RY_HOME="$HOME/.ry"   # 默认
```

---

## 搜索路径优先级

包文件按以下顺序搜索：

1. **导入源文件的目录** —— 首先搜索包含导入语句的文件所在的目录。
2. **`$RY_HOME/share`** —— 標準庫的位置。對於舊版安裝，會回退至 **`$RY_HOME/lib`**。
3. **可執行文件相對的 `share/`** —— 相對於 `ry` 可執行文件的目錄。對於舊版佈局，會回退至可執行文件相對的 **`lib/`**。
4. **`RY_PATH` 环境变量** —— 如果未找到，按顺序搜索 `RY_PATH` 中指定的目录。

---

## RY_PATH 环境变量

可以使用冒号分隔指定多个目录。

```bash
export RY_PATH=/home/user/ry-libs:/usr/local/ry-libs
```

设置后，可以从任何地方导入指定目录中的包。

---

## 限制

- `from` 语句只能写在文件的**顶层**，不能写在函数或块内部。
- 多次导入同一包时，会自动跳过（不会发生重复导入）。
- **循环导入**（A 导入 B，B 导入 A）会产生错误。

```python
# 错误示例：a.ry 和 b.ry 互相导入
# a.ry: from b import foo
# b.ry: from a import bar  <- 循环导入错误
```

---

[<- 上一篇：错误处理](08-error-handling.md) | [下一篇：并发 ->](10-concurrency.md)
