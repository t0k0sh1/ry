---
name: git-push
description: Commit, rebase onto main, and push the current branch. Use when you have local commits or working-tree changes to publish. STOPs on main.
allowed-tools: Bash(git add:*), Bash(git status:*), Bash(git push:*), Bash(git commit:*), Bash(git fetch:*), Bash(git rebase:*), Bash(git diff:*), Bash(git branch:*), Bash(git log:*), Read, Edit
metadata:
  short-description: Commit and push
---

# Git Push

## Context

- Current git status: !`git status`
- Current git diff: !`git diff HEAD`
- Current branch: !`git branch --show-current`

## Branch safety

- **STOP if on `main`.** Tell the user to create a feature branch via `/git-branch-naming` first.

## Steps

### 1. Commit

- Stage and create a single commit using **Conventional Commits** (`feat:` / `fix:` / `refactor:` / `chore:` / etc.).

### 2. Rebase onto `origin/main`

- `git fetch origin`
- `git rebase origin/main`
- **Do not re-run `git fetch` between rebase and push** — it weakens the `--force-with-lease` guard in Step 3.
- On conflict:
  - `git diff --name-only --diff-filter=U` to list conflicting files
  - `Read` + `Edit` to resolve
  - `git add <file>` per resolved file → `git rebase --continue`
  - If you cannot resolve: STOP and report to the user (do **not** auto-`git rebase --abort`)

### 3. Push

- Upstream already set: `git push --force-with-lease`
- First push (no upstream): `git push -u --force-with-lease origin <branch>`
- Force push is required because rebase rewrites SHAs. `--force-with-lease` (no argument) rejects the push if `origin/<branch>` advanced since the last `git fetch`.
