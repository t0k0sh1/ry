---
name: git-commit-push
description: Commit, merge base branch, and push with branch safety check and smart base branch detection for vx.x.x workflow.
metadata:
  short-description: Commit and push
---

# Git Commit Push

Commit, merge the base branch, and push.

## Context

- Current git status: !`git status`
- Current git diff (staged and unstaged changes): !`git diff HEAD`
- Current branch: !`git branch --show-current`

## Branch Safety Check

Before committing, check the current branch name. If the current branch is `main` or matches the pattern `v*.*.*` (e.g. `v0.0.8`):
- **STOP** — do not commit
- Tell the user: "Cannot commit on `<branch>`. Create a feature branch first (use /git-branch-naming)."

## Steps

### 1. Commit

Create a single commit with an appropriate message based on the changes. Use conventional commit format.

### 2. Merge base branch

1. Detect the base branch:
   - Run `gh pr view --json baseRefName -q .baseRefName` to get the base branch of the PR
   - If no PR exists, detect the parent branch:
     - Check if a `v*.*.*` branch exists locally that is an ancestor of the current branch
     - If found, use that as the base branch
     - Otherwise default to `main`
2. Run `git fetch origin` to get the latest remote state
3. Run `git merge origin/<base branch>` to merge the base branch changes
4. **On conflict**:
   - Run `git diff --name-only --diff-filter=U` to list conflicting files
   - `Read` each conflicting file and examine the conflict markers
   - Use `Edit` to resolve the conflicts
   - Stage each resolved file with `git add`
   - Once all conflicts are resolved, run `git merge --continue`
   - If any conflict cannot be resolved, report to the user and stop (do NOT auto-abort the merge)

### 3. Push

Push the branch to origin.
