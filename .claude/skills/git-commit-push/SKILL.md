---
name: git-commit-push
description: Commit, merge main, and push with branch safety check.
allowed-tools: Bash(git add:*), Bash(git status:*), Bash(git push:*), Bash(git commit:*), Bash(git fetch:*), Bash(git merge:*), Bash(git diff:*), Bash(git branch:*), Bash(git log:*), Read, Edit
metadata:
  short-description: Commit and push
---

# Git Commit Push

Commit, merge `main`, and push.

## Context

- Current git status: !`git status`
- Current git diff (staged and unstaged changes): !`git diff HEAD`
- Current branch: !`git branch --show-current`

## Branch Safety Check

Before committing, check the current branch name. If the current branch is `main`:
- **STOP** — do not commit
- Tell the user: "Cannot commit on `<branch>`. Create a feature branch first (use /git-branch-naming)."

## Steps

### 1. Commit

Create a single commit with an appropriate message based on the changes. Use conventional commit format.

### 2. Merge main

1. Run `git fetch origin` to get the latest remote state
2. Run `git merge origin/main` to merge upstream changes
3. **On conflict**:
   - Run `git diff --name-only --diff-filter=U` to list conflicting files
   - `Read` each conflicting file and examine the conflict markers
   - Use `Edit` to resolve the conflicts
   - Stage each resolved file with `git add`
   - Once all conflicts are resolved, run `git merge --continue`
   - If any conflict cannot be resolved, report to the user and stop (do NOT auto-abort the merge)

### 3. Push

Push the branch to origin.
