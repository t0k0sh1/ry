[English](../../reference/path.md) | [日本語](../../ja/reference/path.md) | [简体中文](path.md)

# 路径函数参考

文件路径操作。所有函数需要从 `path` 显式导入。

```python
from path import join, basename, dirname, extension, resolve, is_absolute
```

## 函数一览

| 函数 | 签名 | 说明 |
|----------|-----------|-------------|
| `join` | `(str, str) -> str` | 连接两个路径段 |
| `join` | `(str, str, str) -> str` | 连接三个路径段 |
| `join` | `(str, str, str, str) -> str` | 连接四个路径段 |
| `basename` | `(str) -> str` | 提取文件名部分 |
| `dirname` | `(str) -> str` | 提取目录部分 |
| `extension` | `(str) -> str` | 提取文件扩展名（包含点号） |
| `resolve` | `(str) -> Result<str, Error>` | 将路径解析为绝对规范路径 |
| `is_absolute` | `(str) -> bool` | 返回路径是否为绝对路径 |

## 示例

### 连接路径

```python
from path import join

p = join("/tmp", "data", "file.txt")
print(p)  # /tmp/data/file.txt

# 绝对路径的第二参数会替换第一参数
print(join("/tmp", "/usr"))  # /usr
```

### 提取路径组成部分

```python
from path import basename, dirname, extension

p = "/home/user/docs/report.pdf"

print(basename(p))    # report.pdf
print(dirname(p))     # /home/user/docs
print(extension(p))   # .pdf
```

### 扩展名的边界情况

```python
from path import extension

print(extension("archive.tar.gz"))  # .gz
print(extension(".gitignore"))      # (empty string — hidden file with no extension)
print(extension(".config.json"))    # .json
print(extension("Makefile"))        # (empty string)
```

### 检查绝对路径

```python
from path import is_absolute

print(is_absolute("/usr/local"))  # true
print(is_absolute("src/main.ry")) # false
```

### 解析路径

```python
from path import resolve

match resolve("/tmp"):
  case Ok(p):
    print(p)  # /private/tmp (on macOS) or /tmp
  case Err(e):
    print(e.message)

match resolve("/nonexistent"):
  case Ok(p):
    print(p)
  case Err(e):
    print(e.message)  # cannot resolve path '/nonexistent': No such file or directory
```
