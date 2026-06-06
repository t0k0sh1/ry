---
name: git-search-issues
description: Search GitHub issues by number, label, keyword, or find next issue candidates to work on. Also fires on Japanese triggers 次の issue 探して, issue 検索, ラベルで絞り込み, 次やる issue 探す.
allowed-tools: mcp__plugin_github_github__list_issues, mcp__plugin_github_github__search_issues, mcp__plugin_github_github__issue_read
metadata:
  short-description: Search GitHub issues
---

# Git Search Issues

Search and retrieve GitHub issues.

## Repository

- owner: `t0k0sh1`
- repo: `ry`

## Rules

- **Do not create or run Python or Bash scripts**. Use MCP tools directly to complete the task.
- **Do not use `state:open` or `state:closed`** in `search_issues` queries (returns 0 results). Use `is:open` / `is:closed` instead.

## Input

User input: `$ARGUMENTS`

Behavior depends on the input:

### No arguments, or "find next issue", "search", etc.

Find candidate issues to work on next:

1. Call `search_issues`:
   - query: `"is:open -label:wip -label:fixed"`
   - owner: `"t0k0sh1"`, repo: `"ry"`
   - sort: `"created"`, order: `"desc"`
   - perPage: `20`
2. Prioritize results:
   - `bug` label first (bug fixes take priority)
   - High-impact improvements (`enhancement` label)
   - Others
3. Display each candidate as `#number: title [labels]`

### Issue number specified (e.g. `307`, `#307`)

Call `issue_read`:
- method: `"get"`
- owner: `"t0k0sh1"`, repo: `"ry"`
- issue_number: the specified number (as integer)

### Label name specified (e.g. `bug`, `enhancement`, `v0.0.5`)

Call `list_issues`:
- owner: `"t0k0sh1"`, repo: `"ry"`
- state: `"OPEN"`
- labels: `["specified label"]`
- orderBy: `"CREATED_AT"`, direction: `"DESC"`
- perPage: `20`

### Keyword search (e.g. `HTTP`, `lambda`)

Call `search_issues`:
- query: `"is:open <keyword>"` (substitute the user's keyword into the query)
- owner: `"t0k0sh1"`, repo: `"ry"`
- sort: `"created"`, order: `"desc"`
- perPage: `20`

## MCP Tool Reference

### list_issues (GraphQL) — Simple listing

| Parameter | Type | Example | Notes |
|---|---|---|---|
| owner | string | `"t0k0sh1"` | Required |
| repo | string | `"ry"` | Required |
| state | enum | `"OPEN"`, `"CLOSED"` | Uppercase. Omit for both |
| labels | string[] | `["bug", "v0.0.5"]` | Array |
| orderBy | enum | `"CREATED_AT"`, `"UPDATED_AT"`, `"COMMENTS"` | direction also required |
| direction | enum | `"ASC"`, `"DESC"` | orderBy also required |
| perPage | number | `20` | Max 100 |

### search_issues (REST) — Advanced search

| Parameter | Type | Example | Notes |
|---|---|---|---|
| query | string | `"is:open label:bug"` | **`is:issue` is auto-added** |
| owner | string | `"t0k0sh1"` | Optional |
| repo | string | `"ry"` | Optional |
| sort | enum | `"created"`, `"updated"`, `"comments"` | |
| order | enum | `"asc"`, `"desc"` | |
| perPage | number | `20` | Max 100 |

**Query syntax notes:**
- Use `is:open` / `is:closed` (`state:open` is **invalid**, returns 0 results)
- `label:bug` for label filter
- `-label:wip` to exclude a label
- `is:issue` is auto-added, do not include it

### issue_read — Single issue details

| Parameter | Type | Example | Notes |
|---|---|---|---|
| method | enum | `"get"` | Required. Others: `"get_comments"`, `"get_sub_issues"`, `"get_labels"` |
| owner | string | `"t0k0sh1"` | Required |
| repo | string | `"ry"` | Required |
| issue_number | number | `307` | Required, integer |
