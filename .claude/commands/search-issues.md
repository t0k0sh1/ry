---
allowed-tools: mcp__plugin_github_github__list_issues, mcp__plugin_github_github__search_issues, mcp__plugin_github_github__issue_read
description: GitHub issue を検索する
---

## Repository

- owner: `t0k0sh1`
- repo: `ry`

## 禁止事項

- **Python スクリプトや Bash スクリプトを作成・実行してはならない**。MCP ツールを直接呼び出すだけで完結させること。
- `search_issues` の query で `state:open` や `state:closed` を使ってはならない（0件になる）。代わりに `is:open` / `is:closed` を使うこと。

## 引数の解釈

ユーザーの引数: `$ARGUMENTS`

引数に応じて以下のように動作を切り替える:

### 引数なし、または「次のissue」「探して」等

次に取り組む issue の候補を探す。以下の手順で実行:

1. `search_issues` を呼ぶ:
   - query: `"is:open -label:wip -label:fixed"`
   - owner: `"t0k0sh1"`, repo: `"ry"`
   - sort: `"created"`, order: `"desc"`
   - perPage: `20`
2. 結果から候補を選定し、以下の優先順位で提示:
   - `bug` ラベル付き（バグ修正優先）
   - 効果の高い改善（`enhancement` ラベル）
   - その他
3. 各候補について `#番号: タイトル [ラベル]` の形式で一覧表示

### issue 番号が指定された場合（例: `307`, `#307`）

`issue_read` を呼ぶ:
- method: `"get"`
- owner: `"t0k0sh1"`, repo: `"ry"`
- issue_number: 指定された番号（数値）

### ラベル名が指定された場合（例: `bug`, `enhancement`, `v0.0.5`）

`list_issues` を呼ぶ:
- owner: `"t0k0sh1"`, repo: `"ry"`
- state: `"OPEN"`
- labels: `["指定されたラベル"]`
- orderBy: `"CREATED_AT"`, direction: `"DESC"`
- perPage: `20`

### キーワード検索（例: `HTTP`, `lambda`）

`search_issues` を呼ぶ:
- query: `"is:open キーワード"`
- owner: `"t0k0sh1"`, repo: `"ry"`
- sort: `"created"`, order: `"desc"`
- perPage: `20`

## MCP ツールの正しい呼び出しパターン（リファレンス）

### list_issues (GraphQL) — シンプルな一覧取得

| パラメータ | 型 | 例 | 備考 |
|---|---|---|---|
| owner | string | `"t0k0sh1"` | 必須 |
| repo | string | `"ry"` | 必須 |
| state | enum | `"OPEN"`, `"CLOSED"` | 大文字。省略時は両方 |
| labels | string[] | `["bug", "v0.0.5"]` | 配列で指定 |
| orderBy | enum | `"CREATED_AT"`, `"UPDATED_AT"`, `"COMMENTS"` | direction も必須 |
| direction | enum | `"ASC"`, `"DESC"` | orderBy も必須 |
| perPage | number | `20` | 最大100 |

### search_issues (REST) — 高度な検索

| パラメータ | 型 | 例 | 備考 |
|---|---|---|---|
| query | string | `"is:open label:bug"` | **`is:issue` は自動付与** |
| owner | string | `"t0k0sh1"` | オプション |
| repo | string | `"ry"` | オプション |
| sort | enum | `"created"`, `"updated"`, `"comments"` | |
| order | enum | `"asc"`, `"desc"` | |
| perPage | number | `20` | 最大100 |

**query 構文の注意:**
- `is:open` / `is:closed` を使う（`state:open` は**無効**、0件になる）
- `label:bug` でラベルフィルタ
- `-label:wip` でラベル除外
- `is:issue` は自動付与されるので書かない

### issue_read — 特定 issue の詳細

| パラメータ | 型 | 例 | 備考 |
|---|---|---|---|
| method | enum | `"get"` | 必須。他: `"get_comments"`, `"get_sub_issues"`, `"get_labels"` |
| owner | string | `"t0k0sh1"` | 必須 |
| repo | string | `"ry"` | 必須 |
| issue_number | number | `307` | 必須、数値 |
