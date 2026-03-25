[English](../../reference/io.md) | [日本語](../../ja/reference/io.md) | [繁體中文](io.md)

# I/O 函式參考手冊

標準輸入輸出與檔案操作的函式一覽。所有函式皆需從 `io` 明確匯入。

```python
from io import read_text, write_text, file_exists
```

## 函式一覽

### 標準輸入

| 函式 | 簽名 | 說明 |
|------|------|------|
| `read_line` | `() -> str` | 從 stdin 讀取一行（移除末尾換行） |
| `read_all` | `() -> str` | 讀取 stdin 直到 EOF |

### 檔案 I/O

| 函式 | 簽名 | 說明 |
|------|------|------|
| `read_text` | `(str) -> str` | 將整個檔案作為字串讀取 |
| `write_text` | `(str, str) -> Unit` | 將字串寫入檔案（覆蓋） |
| `append_text` | `(str, str) -> Unit` | 在檔案末尾追加字串 |
| `file_exists` | `(str) -> bool` | 檢查檔案是否存在 |
| `delete_file` | `(str) -> Unit` | 刪除檔案 |
| `read_bytes` | `(str) -> List<byte>` | 將檔案作為位元組串列讀取 |
| `write_bytes` | `(str, List<byte>) -> Unit` | 將位元組串列寫入檔案 |

### 位元組轉換

| 函式 | 簽名 | 說明 |
|------|------|------|
| `str_to_bytes` | `(str) -> List<byte>` | 將字串轉換為 UTF-8 位元組 |
| `bytes_to_str` | `(List<byte>) -> str` | 將位元組串列轉換為字串 |

## 使用範例

### 讀寫檔案

```python
from io import read_text, write_text, append_text, file_exists, delete_file

write_text("hello.txt", "Hello, World!")
content = read_text("hello.txt")
print(content)   # Hello, World!

append_text("hello.txt", "\nGoodbye!")
print(read_text("hello.txt"))
# Hello, World!
# Goodbye!

print(file_exists("hello.txt"))   # true
delete_file("hello.txt")
print(file_exists("hello.txt"))   # false
```

### 位元組操作

```python
from io import str_to_bytes, bytes_to_str, write_bytes, read_bytes

bs = str_to_bytes("ABC")
print(length(bs))    # 3

write_bytes("data.bin", bs)
rb = read_bytes("data.bin")
s = bytes_to_str(rb)
print(s)          # ABC
```

### 從標準輸入讀取

```python
from io import read_line

name = read_line()
print(f"Hello, {name}!")
```

## 錯誤處理

所有檔案操作在失敗時會以執行時錯誤終止：

| 操作 | 錯誤條件 |
|------|---------|
| `read_text` / `read_bytes` | 檔案不存在或無法開啟 |
| `write_text` / `write_bytes` / `append_text` | 無法開啟檔案進行寫入 |
| `delete_file` | 無法刪除檔案 |

錯誤訊息會輸出到 stderr，程式以結束代碼 1 終止。

## 備註

- 使用 `List<byte>` 作為緩衝區型別。標準串列操作（`length()`、`append()`、`slice()`、索引存取）均可用於位元組串列。
- 檔案路徑若未指定絕對路徑，則為相對於當前工作目錄的相對路徑。
- `write_text` 與 `write_bytes` 會覆蓋既有檔案。若要追加內容，請使用 `append_text`。
