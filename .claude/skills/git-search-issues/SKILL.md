---
name: git-search-issues
description: Search GitHub issues by number, label, keyword, or find next issue candidates to work on.
allowed-tools: mcp__plugin_github_github__list_issues, mcp__plugin_github_github__search_issues, mcp__plugin_github_github__issue_read, Bash(scripts/claim-issue.sh:*)
metadata:
  short-description: Search GitHub issues
---

# Git Search Issues

## Repository

- owner: `t0k0sh1`
- repo: `ry`

## Rules

- **Do not create or run Python or Bash scripts**. Use MCP tools directly.
- **Do not use `state:open` or `state:closed`** in `search_issues` queries (returns 0 results). Use `is:open` / `is:closed` instead.

## Input

User input: `$ARGUMENTS`

### No arguments, or "find next issue", "search", etc.

1. Call `search_issues`:
   - query: `"is:open -label:wip -label:fixed"`, owner: `"t0k0sh1"`, repo: `"ry"`
   - sort: `"created"`, order: `"desc"`, perPage: `20`
2. Prioritize: `bug` first → `enhancement` → others.
3. Display each as `#number: title [labels]`.

After the user selects and asks to start work, run `scripts/claim-issue.sh '#<n>'` before continuing. Claim success is not a stopping point; continue in the same turn.

### Issue number specified (e.g. `307`, `#307`)

Call `issue_read`: method `"get"`, owner `"t0k0sh1"`, repo `"ry"`, issue_number: integer.

### Label name specified (e.g. `bug`, `enhancement`, `v0.0.5`)

Call `list_issues`: owner `"t0k0sh1"`, repo `"ry"`, state `"OPEN"`, labels `["specified label"]`, orderBy `"CREATED_AT"`, direction `"DESC"`, perPage `20`.

### Keyword search (e.g. `HTTP`, `lambda`)

Call `search_issues`: query `"is:open <keyword>"`, owner `"t0k0sh1"`, repo `"ry"`, sort `"created"`, order `"desc"`, perPage `20`.

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

**Query syntax notes:** `is:open` / `is:closed` (`state:open` is **invalid**); `label:bug`; `-label:wip`; `is:issue` auto-added.

### issue_read — Single issue details

| Parameter | Type | Example | Notes |
|---|---|---|---|
| method | enum | `"get"` | Required. Others: `"get_comments"`, `"get_sub_issues"`, `"get_labels"` |
| owner | string | `"t0k0sh1"` | Required |
| repo | string | `"ry"` | Required |
| issue_number | number | `307` | Required, integer |
