---
name: git-commit-push-pr
description: Commit, push, and open a PR. Creates feature branch if on main. PRs are opened (not draft).
metadata:
  short-description: Commit, push, and open a PR
---

# Git Commit Push PR

Create a branch (if needed), commit, merge the base branch, push, and open a PR.

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

### 3. Merge base branch

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

### 4. Push

Push the branch to origin with `-u` flag to set upstream tracking.

### 5. Create PR

Create a pull request using `gh pr create` (open, not draft).

- Use the base branch detected in Step 3 as the `--base` argument
- Write a clear title and body summarizing the changes
