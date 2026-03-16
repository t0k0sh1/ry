[English](../README.md) | [日本語](../ja/README.md) | [繁體中文](README.md)

# Ry 語言文件

Ry 是一個基於 LLVM JIT 的簡潔程式語言。採用 Python 風格的縮排區塊語法，結合靜態型別與型別推論，設計簡單易用。

---

## 教學

初次使用 Ry 的使用者，請依序閱讀以下內容。

| # | 標題 | 內容 |
|---|------|------|
| 01 | [入門](tutorial/01-getting-started.md) | 環境建置、Hello World、註解 |
| 02 | [變數與型別](tutorial/02-variables-and-types.md) | 變數宣告、常數、型別標註、基本型別 |
| 03 | [運算子](tutorial/03-operators.md) | 算術、比較、邏輯、位元、複合賦值運算子 |
| 04 | [控制流程](tutorial/04-control-flow.md) | if/elif/else、while、for/range、break/continue |
| 05 | [函式定義](tutorial/05-functions.md) | fn 定義、遞迴、多載、Lambda、UFCS |
| 06 | [結構體與列舉型別](tutorial/06-structs.md) | type 定義、欄位存取、方法、enum |
| 07 | [集合型別](tutorial/07-collections.md) | 元組、串列、映射、集合 |
| 08 | [進階功能](tutorial/08-advanced.md) | 閉包、運算子多載、Option 型別 |
| 09 | [模組](tutorial/09-modules.md) | 使用 from/import 進行模組分割 |

環境建置與建置方法請參閱 [01 - 入門](tutorial/01-getting-started.md)。

---

## 參考手冊

語言規格的詳細內容請參閱參考手冊。

| 頁面 | 內容 |
|------|------|
| [型別一覽與型別規則](reference/types.md) | 所有型別的說明、型別提升規則、型別轉換 |
| [運算子一覽與優先順序](reference/operators.md) | 所有運算子與優先順序表 |
| [控制流程](reference/control-flow.md) | if、while、for 的完整文法 |
| [函式、Lambda、UFCS、運算子多載](reference/functions.md) | 函式定義的所有形式 |
| [結構體與列舉型別](reference/structs.md) | type 定義、enum 定義的完整文法 |
| [元組、串列、映射、集合](reference/collections.md) | 集合型別的操作方法 |
| [內建函式](reference/builtins.md) | print、len、Some、unwrap 等 |
| [字串操作函式](reference/builtins-string.md) | contains、find、replace、split、join 等 |
| [模組系統](reference/modules.md) | from/import 的文法與搜尋規則 |
| [測試功能](reference/testing.md) | 使用 describe/it/expect 進行測試 |
| [專案管理](reference/project.md) | ry init 與 ry.toml 的規格 |
| [契約式設計](reference/contracts.md) | require、ensure、invariant、old、result |
| [指令](reference/directives.md) | @deprecated 與編譯時元資料 |
| [錯誤一覽](reference/errors.md) | 編譯錯誤與執行時錯誤的說明 |
