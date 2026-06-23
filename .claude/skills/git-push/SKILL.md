---
name: git-push
description: User-invoked slash command that ensures a feature branch, commits, rebases onto main, and pushes.
allowed-tools: Bash(git add:*), Bash(git status:*), Bash(git push:*), Bash(git commit:*), Bash(git fetch:*), Bash(git rebase:*), Bash(git diff:*), Bash(git branch:*), Bash(git log:*), Bash(git checkout -b:*), Bash(git rev-parse:*), Read, Edit
metadata:
  short-description: Branch, commit and push
---

# Git Push

## Invocation Gate

- Run only on direct `/git-push` invocation.

> Keep Steps 0-3 action-compatible with `/git-create-pr` Steps 1-4.

## Context

- Current git status: !`git status`
- Current git diff: !`git diff HEAD`
- Current branch: !`git branch --show-current`

## Steps

### 0. Branch ensure

- Run `git rev-parse --abbrev-ref HEAD`. If not `main`, skip to Step 1.
- If on `main`, create a feature branch:
  1. Infer `type` from user intent and changes:

     | Type | When to use |
     |------|-------------|
     | `feat` | New feature |
     | `fix` | Bug fix |
     | `docs` | Documentation only |
     | `refactor` | No behavior change |
     | `test` | Test changes |
     | `chore` | Build, CI, dependencies, tooling |

  2. Generate `<type>/<short-kebab-description>`, e.g. `feat/add-crypto-stdlib`.
  3. Before `git checkout -b`, lowercase the branch name, strip non-letters, and confirm it does not contain `main`; regenerate if needed.
  4. Run `git checkout -b <type>/<short-description>` and report it.

### 1. Commit

- Stage and create one Conventional Commit (`feat:`, `fix:`, `refactor:`, `chore:`, etc.).

### 2. Rebase onto `origin/main`

- `git fetch origin`
- `git rebase origin/main`
- Do not re-run `git fetch` between rebase and push; it weakens `--force-with-lease`.
- On conflict:
  - `git diff --name-only --diff-filter=U` to list conflicting files
  - `Read` + `Edit` to resolve
  - `git add <file>` per resolved file → `git rebase --continue`
  - If unresolvable: stop and report; do not auto-`git rebase --abort`

### 3. Push

- Upstream already set: `git push --force-with-lease`
- First push (no upstream): `git push -u --force-with-lease origin <branch>`
- Rebase rewrites SHAs; `--force-with-lease` rejects if `origin/<branch>` advanced since the last fetch.
