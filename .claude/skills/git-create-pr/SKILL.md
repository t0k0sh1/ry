---
name: git-create-pr
description: User-invoked slash command that ensures a feature branch, commits pending work, rebases onto main, pushes, and opens a PR.
allowed-tools: Bash(git status:*), Bash(gh pr create:*), Bash(git diff:*), Bash(git branch:*), Bash(git log:*), Bash(git rev-parse:*), Bash(git add:*), Bash(git push:*), Bash(git commit:*), Bash(git fetch:*), Bash(git rebase:*), Bash(git checkout -b:*), Read, Edit
metadata:
  short-description: Branch, push, and open a PR
---

# Git Create PR

## Invocation Gate

- Run only on direct `/git-create-pr` invocation.

## Context

- Current branch: !`git branch --show-current`
- Working tree: !`git status --short`
- Unpushed commits: !`git log @{u}..HEAD --oneline 2>/dev/null || echo "(no upstream)"`

## Steps

Stop if on `main` with a clean working tree and nothing ahead of `origin/main`.

### 1. Branch ensure

- Run `git rev-parse --abbrev-ref HEAD`. If not `main`, skip to Step 2.
- If on `main`, create a feature branch:
  - Infer Conventional Commit type (`feat`, `fix`, `docs`, `refactor`, `test`, `chore`).
  - Generate `<type>/<short-kebab-description>`.
  - Lowercase it, strip non-letters for the safety check, and ensure it does not contain `main`.
  - Run `git checkout -b <branch>`.

### 2. Commit

- Skip when working tree is clean.
- Stage and create one Conventional Commit (`feat:`, `fix:`, `refactor:`, `chore:`, etc.).

### 3. Rebase onto `origin/main`

- Skip when clean, upstream is set, and no commits are ahead of `@{u}`.
- `git fetch origin`
- `git rebase origin/main`
- Do not re-run `git fetch` before push.
- On conflict:
  - list conflicts with `git diff --name-only --diff-filter=U`
  - resolve files, `git add <file>`, then `git rebase --continue`
  - if unresolvable, stop and report; do not auto-`git rebase --abort`

### 4. Push

- Skip when clean, upstream is set, and no commits are ahead of `@{u}`.
- Upstream already set: `git push --force-with-lease`
- First push: `git push -u --force-with-lease origin <branch>`

### 5. Open PR

- Run `gh pr create --base main` (open, not draft).
- Title: Conventional Commits format.
- Body: 1-3 line summary + `Closes #<issue>` for the linked issue.
