---
name: git-resolve-conflicts
description: Resolve PR merge conflicts locally by rebasing onto main, fixing conflicts, and pushing.
allowed-tools: Bash(git:*), Bash(gh pr view:*), Read, Edit
metadata:
  short-description: Resolve PR conflicts locally and push
---

# Git Resolve Conflicts

Resolve merge conflicts for a PR by rebasing onto `main` locally, fixing conflicts, and pushing with `--force-with-lease`.

## Context

- Current branch: !`git branch --show-current`
- PR info: !`gh pr view --json number,title,url,mergeable 2>/dev/null || echo "No PR found"`
- Current git status: !`git status`

## Inputs

User input: $ARGUMENTS

## Steps

### Step 1: Identify PR

- If the user specified a PR number (e.g. `#123` or `123`), run `gh pr view <number> --json number,title,url,headRefName,mergeable`.
- Otherwise use the PR associated with the current branch (from Context above).
- If no PR is found, stop:
  > No PR found. Run this command on a branch with an associated PR, or specify a PR number.

### Step 2: Verify current branch matches the PR head

1. Get `headRefName` from the PR.
2. Compare with the current branch (`git branch --show-current`).
3. If they differ:
   - Auto-detected PR: report mismatch and stop — do NOT rebase the wrong branch.
   - Explicit PR number given: run `git switch <headRefName>` (or `git switch --track origin/<headRefName>` if not local). Only proceed after switch succeeds.
   - If switch is not possible (dirty worktree, divergence, etc.): stop — do NOT rebase the wrong branch.

### Step 3: Fetch and rebase onto main

**Mid-rebase guard**: Check `git status` for `rebase in progress`, or for `.git/rebase-merge` / `.git/rebase-apply`. If in progress, skip to Step 4 to resume — do not run `git rebase` again.

Otherwise:

1. `git fetch origin` — **do not re-run `git fetch` after this point** until the push completes; refreshing the remote-tracking ref between rebase and push weakens `--force-with-lease`.
2. `git rebase origin/main`
3. If no conflicts, skip to Step 5.

### Step 4: Resolve conflicts

1. `git diff --name-only --diff-filter=U` — list conflicting files.
2. `Read` each file; examine conflict markers (`<<<<<<<`, `=======`, `>>>>>>>`).
3. `Edit` to resolve, preserving the intent of both branches.
4. `git add` each resolved file.
5. `git rebase --continue`.
6. If any conflict cannot be resolved: report to the user and stop (do NOT auto-`git rebase --abort`).

### Step 5: Push

`git push --force-with-lease` — force push is required because rebase rewrites SHAs. `--force-with-lease` blocks the push if `origin/<branch>` advanced since the last `git fetch`.

### Step 6: Report

- List of conflicting files (or note if none).
- Summary of how each conflict was resolved.
- Push result.
