[English](../../reference/io.md) | [日本語](../../ja/reference/io.md) | [繁體中文](io.md)

# I/O 函数参考手册

标准输入输出与文件操作。所有函数均需从 `io` 明确导入。

```python
from io import read_text, write_text, exists
```

## 函数列表

### 标准输入

| 函数 | 签名 | 说明 |
|------|------|------|
| `read_line` | `() -> str` | 从 stdin 读取一行（移除末尾换行） |
| `read_all` | `() -> str` | 读取 stdin 直到 EOF |

### 文件 I/O

| 函数 | 签名 | 说明 |
|------|------|------|
| `read_text` | `(str) -> Result<str, Error>` | 将整个文件作为字符串读取 |
| `write_text` | `(str, str) -> Result<Unit, Error>` | 将字符串写入文件（覆盖） |
| `append_text` | `(str, str) -> Result<Unit, Error>` | 在文件末尾追加字符串 |
| `exists` | `(str) -> bool` | 检查文件是否存在 |
| `delete_file` | `(str) -> Result<Unit, Error>` | 删除文件 |
| `read_bytes` | `(str) -> Result<List<u8>, Error>` | 将文件作为字节列表读取 |
| `write_bytes` | `(str, List<u8>) -> Result<Unit, Error>` | 将字节列表写入文件 |

### 字节转换

| 函数 | 签名 | 说明 |
|------|------|------|
| `to_bytes` | `(str) -> List<u8>` | 将字符串转换为 UTF-8 字节 |
| `bytes_to_str` | `(List<u8>) -> Result<str, Error>` | 将字节列表转换为字符串 |

## 使用示例

### 读写文件

```python
from io import read_text, write_text, append_text, exists, delete_file

case write_text("hello.txt", "Hello, World!"):
    Ok(_):
        case read_text("hello.txt"):
            Ok(content):
                print(content)   # Hello, World!
            Err(e):
                print(e.message)
    Err(e):
        print(e.message)

print(exists("hello.txt"))   # true

case delete_file("hello.txt"):
    Ok(_):
        print(exists("hello.txt"))   # false
    Err(e):
        print(e.message)
```

### 字节操作

```python
from io import to_bytes, bytes_to_str, write_bytes, read_bytes

bs = to_bytes("ABC")
print(length(bs))    # 3

case write_bytes("data.bin", bs):
    Ok(_):
        case read_bytes("data.bin"):
            Ok(rb):
                case bytes_to_str(rb):
                    Ok(s):
                        print(s)          # ABC
                    Err(e):
                        print(e.message)
            Err(e):
                print(e.message)
    Err(e):
        print(e.message)
```

### 从标准输入读取

```python
from io import read_line

name = read_line()
print(f"Hello, {name}!")
```

## 错误处理

文件操作返回 `Result<T, Error>` 而不是在失败时终止程序。使用 `match` 配合 `Ok`/`Err` 模式来处理错误:

```python
case read_text("missing.txt"):
    Ok(content):
        print(content)
    Err(e):
        print(e.message)   # cannot open file 'missing.txt' for reading
```

| 操作 | 错误条件 |
|------|---------|
| `read_text` / `read_bytes` | 文件不存在或无法打开 |
| `write_text` / `write_bytes` / `append_text` | 无法打开文件进行写入 |
| `delete_file` | 无法删除文件 |
| `bytes_to_str` | 输入包含 NUL 字节 |

## 备注

- 使用 `List<u8>` 作为缓冲区类型。标准列表操作（`length()`、`append()`、`slice()`、索引访问）均可用于字节列表。
- 文件路径若未指定绝对路径，则为相对于当前工作目录的相对路径。
- `write_text` 与 `write_bytes` 会覆盖现有文件。若要追加内容，请使用 `append_text`。
