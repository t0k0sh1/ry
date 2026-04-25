---
name: git-push
description: Merge base branch and push with smart base branch detection.
allowed-tools: Bash(git push:*), Bash(git fetch:*), Bash(git merge:*), Bash(gh pr view:*), Bash(git add:*), Bash(git diff:*), Bash(git branch:*), Bash(git log:*), Read, Edit
metadata:
  short-description: Push git commits
---

# Git Push

Merge the base branch and then push.

## Context

- Current branch: !`git branch --show-current`
- Recent commits: !`git log --oneline -10`
- Unpushed commits: !`git log --oneline @{u}..HEAD 2>/dev/null || echo "No upstream set"`

## Steps

### 1. Merge base branch

1. Detect the base branch:
   - Run `gh pr view --json baseRefName -q .baseRefName` to get the base branch of the PR
   - If no PR exists, default to `main`
2. Run `git fetch origin` to get the latest remote state
3. Run `git merge origin/<base branch>` to merge the base branch changes
4. **On conflict**:
   - Run `git diff --name-only --diff-filter=U` to list conflicting files
   - `Read` each conflicting file and examine the conflict markers
   - Use `Edit` to resolve the conflicts
   - Stage each resolved file with `git add`
   - Once all conflicts are resolved, run `git merge --continue`
   - If any conflict cannot be resolved, report to the user and stop (do NOT auto-abort the merge)

### 2. Push

Push the unpushed commits to origin.
