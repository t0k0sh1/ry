---
name: git-create-issue
description: Preview, validate, and create a GitHub issue only after explicit user approval.
allowed-tools: Bash(gh issue create:*), Bash(gh issue view:*), Bash(gh issue list:*), Bash(gh search issues:*), Bash(gh api:*), Bash(gh pr view:*)
metadata:
  short-description: Create an approved issue
---

# Git Create Issue

## Permission Gate

Before `gh issue create`, present one proposal:

| Field | Required content |
|---|---|
| Reason | Why tracking is needed |
| Summary | Title and expected outcome |
| Granularity | Why this is one issue |
| Confidence | High / medium / low with uncertainty |
| Labels | Proposed labels |
| Milestone | Proposed milestone or unset |

- Wait for explicit approval.
- Do not present issue creation as a menu option.
- A previously approved side-finding proposal satisfies this gate unless content changed.
- `/preparing-for-release` is outside this gate.

## Validate

1. Apply `/scope-decomposition`.
2. Search open and closed issues for duplicates.
3. If a likely duplicate exists, report it and stop.
4. Preserve existing labels when editing related issues.

## Create

- Create only the approved content.
- Omit milestone when unset.
- Capture the resulting issue number and URL.

## Report

- Issue number, title, URL, labels, and milestone.
- Relationship to the originating work, when applicable.
