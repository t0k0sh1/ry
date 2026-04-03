[English](../../reference/filesystem.md) | [日本語](../../ja/reference/filesystem.md) | [简体中文](filesystem.md)

# 文件系统函数参考

文件和目录操作。所有函数需要从 `filesystem` 显式导入。

`filesystem` 包处理文件和目录本身的操作（复制、移动、删除等），而 `io` 包处理文件内容的读写。

```python
from filesystem import list_dir, walk, glob_files, copy, move, remove, remove_all
from filesystem import make_dir, make_dir_all, file_size, is_file, is_dir, is_symlink
from filesystem import chmod, symlink, read_link
```

## 函数一览

| 函数 | 签名 | 说明 |
|----------|-----------|-------------|
| `list_dir` | `(str) -> Result<List<str>, Error>` | 列出目录中的条目（非递归） |
| `walk` | `(str) -> Result<List<str>, Error>` | 递归列出所有文件和目录 |
| `glob_files` | `(str) -> Result<List<str>, Error>` | 查找匹配 glob 模式的文件 |
| `copy` | `(str, str) -> Result<Unit, Error>` | 复制文件 |
| `move` | `(str, str) -> Result<Unit, Error>` | 移动或重命名文件或目录 |
| `remove` | `(str) -> Result<Unit, Error>` | 删除文件或空目录 |
| `remove_all` | `(str) -> Result<Unit, Error>` | 递归删除文件或目录树 |
| `make_dir` | `(str) -> Result<Unit, Error>` | 创建单个目录 |
| `make_dir_all` | `(str) -> Result<Unit, Error>` | 创建目录及所有缺失的父目录 |
| `file_size` | `(str) -> Result<int, Error>` | 返回文件大小（字节） |
| `is_file` | `(str) -> bool` | 检查路径是否为普通文件 |
| `is_dir` | `(str) -> bool` | 检查路径是否为目录 |
| `is_symlink` | `(str) -> bool` | 检查路径是否为符号链接 |
| `chmod` | `(str, int) -> Result<Unit, Error>` | 更改文件权限（POSIX 模式） |
| `symlink` | `(str, str) -> Result<Unit, Error>` | 创建符号链接 |
| `read_link` | `(str) -> Result<str, Error>` | 读取符号链接的目标 |

## 示例

### 目录操作

```python
from filesystem import make_dir, make_dir_all, list_dir, remove_all

# 创建单个目录
match make_dir("/tmp/myapp"):
  case Ok(_):
    print("created")
  case Err(e):
    print("error: " + e.message)

# 创建嵌套目录（类似 mkdir -p）
make_dir_all("/tmp/myapp/data/logs")

# 列出目录内容
match list_dir("/tmp/myapp"):
  case Ok(entries):
    for entry in entries:
      print(entry)
  case Err(e):
    print("error: " + e.message)

# 删除目录树（类似 rm -rf）
remove_all("/tmp/myapp")
```

### 文件操作

```python
from filesystem import copy, move, remove, file_size
from io import write_text

write_text("/tmp/hello.txt", "Hello, World!")

# 复制文件
copy("/tmp/hello.txt", "/tmp/hello_copy.txt")

# 获取文件大小
match file_size("/tmp/hello.txt"):
  case Ok(sz):
    print("size: " + to_str(sz))
  case Err(e):
    print("error: " + e.message)

# 移动/重命名文件
move("/tmp/hello_copy.txt", "/tmp/renamed.txt")

# 删除文件
remove("/tmp/renamed.txt")
```

### 递归遍历

```python
from filesystem import walk, glob_files

# 递归遍历目录树（类似 find）
match walk("/var/log"):
  case Ok(files):
    for f in files:
      print(f)
  case Err(e):
    print("error: " + e.message)

# Glob 模式匹配
match glob_files("/var/log/*.log"):
  case Ok(matches):
    for m in matches:
      print(m)
  case Err(e):
    print("error: " + e.message)
```

### 路径类型检查

```python
from filesystem import is_file, is_dir, is_symlink

if is_file("/etc/hosts"):
  print("regular file")

if is_dir("/tmp"):
  print("directory")

if is_symlink("/usr/local/bin/python"):
  print("symbolic link")
```

### 符号链接

```python
from filesystem import symlink, read_link, is_symlink

# 创建符号链接
symlink("/usr/local/bin/ry", "/tmp/ry_link")

# 检查并读取符号链接
if is_symlink("/tmp/ry_link"):
  match read_link("/tmp/ry_link"):
    case Ok(target):
      print("points to: " + target)
    case Err(e):
      print("error: " + e.message)
```

### 权限

```python
from filesystem import chmod

# chmod 755 (rwxr-xr-x) —— 使用十进制值：0o755 = 493
chmod("/tmp/script.sh", 493)

# chmod 644 (rw-r--r--) —— 0o644 = 420
chmod("/tmp/data.txt", 420)
```

## 注意事项

- `is_file`、`is_dir` 和 `is_symlink` 在出错时（例如路径不存在）返回 `false`
- `is_file` 和 `is_dir` 会跟随符号链接；`is_symlink` 使用 `lstat` 检测链接
- `list_dir` 仅返回条目名称（不包含完整路径）
- `walk` 返回所有条目（文件和目录）的完整路径
- `glob_files` 在没有文件匹配模式时返回空列表（而非错误）
- `remove` 对非空目录会失败；使用 `remove_all` 进行递归删除
