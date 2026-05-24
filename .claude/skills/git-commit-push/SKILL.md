---
name: git-commit-push
description: Commit, rebase onto main, and push with branch safety check.
allowed-tools: Bash(git add:*), Bash(git status:*), Bash(git push:*), Bash(git commit:*), Bash(git fetch:*), Bash(git rebase:*), Bash(git diff:*), Bash(git branch:*), Bash(git log:*), Read, Edit
metadata:
  short-description: Commit and push
---

# Git Commit Push

Commit, rebase onto `main`, and push.

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

### 2. Rebase onto main

1. Run `git fetch origin` to get the latest remote state. **Do not re-run `git fetch` after this point until the push completes** — refreshing the remote-tracking ref between rebase and push weakens the `--force-with-lease` guarantee in Step 3.
2. Run `git rebase origin/main` to replay the current branch onto upstream `main`.
3. **On conflict**:
   - Run `git diff --name-only --diff-filter=U` to list conflicting files
   - `Read` each conflicting file and examine the conflict markers
   - Use `Edit` to resolve the conflicts
   - Stage each resolved file with `git add`
   - Once all conflicts are resolved, run `git rebase --continue`
   - If any conflict cannot be resolved, report to the user and stop (do NOT auto-`git rebase --abort`). The user decides whether to abort with `git rebase --abort`.

### 3. Push

Run `git push --force-with-lease` to push the rebased branch. The rebase rewrites commit SHAs, so a force push is required on the second and later invocations.

- `--force-with-lease` (no argument) blocks the push if `origin/<branch>` has advanced since the most recent `git fetch`, preventing accidental overwrite of someone else's push.
- On the **first** push (when no remote ref exists yet), `--force-with-lease` is a safe no-op.
