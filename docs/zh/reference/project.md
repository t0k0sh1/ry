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
  ry.toml          # 專案設定檔
  src/
    main.ry        # 進入點（範例程式碼）
```

### 行為

1. 若 `ry.toml` 已存在則錯誤結束
2. 建立 `src/` 目錄（若不存在）
3. 生成 `ry.toml`（`name` 為當前目錄名稱）
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
  ry.toml          # 專案設定檔
  src/
    main.ry        # 進入點（範例程式碼）
```

### 行為

1. 若未指定專案名稱則錯誤結束
2. 若同名目錄已存在則錯誤結束
3. 建立 `<project-name>/` 目錄
4. 在其中建立 `src/` 目錄
5. 生成 `ry.toml`（`name` 為指定的專案名稱）
6. 生成 `src/main.ry`

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

## `ry.toml` 設定檔

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

`ry.toml` 支援以下 TOML 子集。

- 區段標頭：`[section]`
- 鍵值對：`key = "value"`（僅字串值）
- 註解：從 `#` 到行尾
- 空行會被忽略
