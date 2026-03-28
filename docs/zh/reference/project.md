[English](../../reference/project.md) | [日本語](../../ja/reference/project.md) | [繁體中文](project.md)

# 專案管理

## `ry init` - 專案初始化

將當前目錄初始化為 Ry 專案。

```bash
ry init
```

### 生成的檔案與目錄

```
my-project/
  package.toml          # 專案設定檔
  src/
    main.ry        # 進入點（範例程式碼）
```

### 行為

1. 若 `package.toml` 已存在則錯誤結束
2. 建立 `src/` 目錄（若不存在）
3. 生成 `package.toml`（`name` 為當前目錄名稱）
4. 生成 `src/main.ry`（若已存在則跳過）

---

## `ry new` - 建立新專案

建立新目錄並將其初始化為 Ry 專案。

```bash
ry new my-project
```

### 生成的檔案與目錄

```
my-project/
  package.toml          # 專案設定檔
  src/
    main.ry        # 進入點（範例程式碼）
```

### 行為

1. 若未指定專案名稱則錯誤結束
2. 若同名目錄已存在則錯誤結束
3. 建立 `<project-name>/` 目錄
4. 在其中建立 `src/` 目錄
5. 生成 `package.toml`（`name` 為指定的專案名稱）
6. 生成 `src/main.ry`

---

## `ry fmt` - 程式碼格式化工具

以一致的 2 空格縮排和標準風格格式化 `.ry` 原始碼檔案。

```bash
ry fmt                     # 格式化專案中所有 .ry 檔案（需要 package.toml）
ry fmt src/main.ry         # 格式化單一檔案
ry fmt src/                # 遞迴格式化目錄中所有 .ry 檔案
ry fmt --check             # 檢查檔案是否已格式化（未格式化則 exit 1）
ry fmt --check src/        # 檢查指定目錄
```

### 格式化規則

- 每個區塊層級使用 2 空格縮排
- 二元運算子前後加空格（`a + b`，而非 `a+b`）
- 逗號後加空格（`f(a, b)`，而非 `f(a,b)`）
- 頂層定義之間（函式、記錄、列舉）加空行
- 註解會被保留

### 行為

1. 讀取原始碼檔案，解析為 AST，並以標準格式重新輸出
2. 將格式化結果寫回檔案（就地修改）
3. 使用 `--check` 時僅報告未格式化的檔案，若存在則以代碼 1 結束（適用於 CI）
4. 遞迴格式化時跳過 `.git/`、`build/`、`node_modules/` 目錄

### 注意事項

- 不需要 LLVM 初始化（快速啟動）
- 複合賦值運算子（`+=`、`-=` 等）因解析器會進行去糖化，格式化後會變成展開形式（`x = x + expr`）
- 十六進位（`0xFF`）和二進位（`0b1010`）數字字面量會被轉換為十進位表示

---

## `ry self-update` - 自我更新

將 ry 本身更新至最新版本。從 GitHub Releases 下載二進位檔並取代目前的執行檔。

```bash
ry self-update              # 更新至最新穩定版
ry self-update --nightly    # 更新至最新 nightly 預先發行版
ry self-update v0.0.1       # 更新至指定版本
```

### 行為

1. 顯示目前版本
2. 根據引數解析更新目標版本
   - 無引數：GitHub 的最新穩定發行版（`/releases/latest`）
   - `--nightly`：最新的預先發行版
   - 指定版本：指定標籤的發行版
3. 若與目前版本相同，則以 `"Already up to date."` 結束
4. 下載二進位檔並取代目前的執行檔

### 注意事項

- 執行需要 `curl` 和 `tar` 指令
- 若因權限不足導致二進位檔取代失敗，會顯示建議使用 `sudo` 的訊息（不會自動執行 sudo）
- 下載會先在臨時目錄中進行；但若跨檔案系統的 `cp` 備援操作被中斷，目標二進位檔可能處於不完整狀態

---

## `package.toml` 設定檔

以 TOML 格式記述專案的中繼資料與路徑設定。

```toml
[project]
name = "my-project"
version = "0.1.0"
entry = "src/main.ry"

[paths]
src = "src"
```

### `[project]` 區段

| 鍵 | 說明 |
|------|------|
| `name` | 專案名稱（初始化時為目錄名稱） |
| `version` | 版本字串 |
| `entry` | 作為進入點的原始碼檔案 |

### `[paths]` 區段

| 鍵 | 說明 |
|------|------|
| `src` | 原始碼目錄 |

### TOML 子集規格

`package.toml` 支援以下 TOML 子集。

- 區段標頭：`[section]`
- 鍵值對：`key = "value"`（僅字串值）
- 註解：從 `#` 到行尾
- 空行會被忽略
