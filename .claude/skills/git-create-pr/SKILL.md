---
name: git-create-pr
description: Open a PR to main from the current feature branch. Delegates to /git-push first if there are uncommitted or unpushed changes. PRs are opened (not draft). STOPs on main.
allowed-tools: Bash(git status:*), Bash(gh pr create:*), Bash(git diff:*), Bash(git branch:*), Bash(git log:*), Bash(git rev-parse:*), Read
metadata:
  short-description: Open a PR
---

# Git Create PR

## Context

- Current branch: !`git branch --show-current`
- Working tree: !`git status --short`
- Unpushed commits: !`git log @{u}..HEAD --oneline 2>/dev/null || echo "(no upstream)"`

## Branch safety

- **STOP if on `main`.** Tell the user to create a feature branch via `/git-branch-naming` first.

## Steps

### 1. Sync if needed

- Invoke `/git-push` first when **any** of the following is true:
  - Working tree has uncommitted changes (status non-empty)
  - There are local commits not yet on the remote
  - The branch has no upstream
- Otherwise skip to Step 2.

### 2. Open PR

- Run `gh pr create --base main` (open, not draft).
- Title: Conventional Commits format.
- Body: 1-3 line summary + `Closes #<issue>` for the linked issue.
