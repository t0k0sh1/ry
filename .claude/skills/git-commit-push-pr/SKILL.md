---
name: git-commit-push-pr
description: Commit, push, and open a PR to main. Creates feature branch if on main. PRs are opened (not draft).
allowed-tools: Bash(git checkout -b:*), Bash(git add:*), Bash(git status:*), Bash(git push:*), Bash(git commit:*), Bash(gh pr create:*), Bash(git fetch:*), Bash(git merge:*), Bash(git diff:*), Bash(git branch:*), Bash(git log:*), Read, Edit
metadata:
  short-description: Commit, push, and open a PR
---

# Git Commit Push PR

Create a branch (if needed), commit, merge `main`, push, and open a PR to `main`.

## Context

- Current git status: !`git status`
- Current git diff (staged and unstaged changes): !`git diff HEAD`
- Current branch: !`git branch --show-current`

## Steps

### 1. Create branch (if needed)

If the current branch is `main`, you **MUST** create a new feature branch before committing.
- Use the `<type>/<short-description>` naming convention
- Never commit directly to `main` branches

### 2. Commit

Create a single commit with an appropriate message. Use conventional commit format.

### 3. Merge main

1. Run `git fetch origin` to get the latest remote state
2. Run `git merge origin/main` to merge upstream changes
3. **On conflict**:
   - Run `git diff --name-only --diff-filter=U` to list conflicting files
   - `Read` each conflicting file and examine the conflict markers
   - Use `Edit` to resolve the conflicts
   - Stage each resolved file with `git add`
   - Once all conflicts are resolved, run `git merge --continue`
   - If any conflict cannot be resolved, report to the user and stop (do NOT auto-abort the merge)

### 4. Push

Push the branch to origin with `-u` flag to set upstream tracking.

### 5. Create PR

Create a pull request using `gh pr create --base main` (open, not draft).

- Write a clear title and body summarizing the changes
