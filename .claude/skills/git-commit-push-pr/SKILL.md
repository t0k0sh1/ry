---
name: git-commit-push-pr
description: Commit, rebase onto main, push, and open a PR to main. Creates feature branch if on main. PRs are opened (not draft).
allowed-tools: Bash(git checkout -b:*), Bash(git add:*), Bash(git status:*), Bash(git push:*), Bash(git commit:*), Bash(gh pr create:*), Bash(git fetch:*), Bash(git rebase:*), Bash(git diff:*), Bash(git branch:*), Bash(git log:*), Read, Edit
metadata:
  short-description: Commit, push, and open a PR
---

# Git Commit Push PR

Create a branch (if needed), commit, rebase onto `main`, push, and open a PR to `main`.

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

### 3. Rebase onto main

1. Run `git fetch origin` to get the latest remote state. **Do not re-run `git fetch` after this point until the push completes** — refreshing the remote-tracking ref between rebase and push weakens the `--force-with-lease` guarantee in Step 4.
2. Run `git rebase origin/main` to replay the current branch onto upstream `main`.
3. **On conflict**:
   - Run `git diff --name-only --diff-filter=U` to list conflicting files
   - `Read` each conflicting file and examine the conflict markers
   - Use `Edit` to resolve the conflicts
   - Stage each resolved file with `git add`
   - Once all conflicts are resolved, run `git rebase --continue`
   - If any conflict cannot be resolved, report to the user and stop (do NOT auto-`git rebase --abort`). The user decides whether to abort with `git rebase --abort`.

### 4. Push

Run `git push -u --force-with-lease origin <branch>` to push the rebased branch and set upstream tracking in one step.

- The rebase rewrites commit SHAs, so a force push is required on the second and later invocations.
- `--force-with-lease` (no argument) blocks the push if `origin/<branch>` has advanced since the most recent `git fetch`, preventing accidental overwrite of someone else's push.
- On the **first** push (when no remote ref exists yet), `--force-with-lease` is a safe no-op and `-u` records the upstream.

### 5. Create PR

Create a pull request using `gh pr create --base main` (open, not draft).

- Write a clear title and body summarizing the changes
