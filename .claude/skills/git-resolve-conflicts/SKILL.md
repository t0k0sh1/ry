---
name: git-resolve-conflicts
description: Resolve PR merge conflicts locally by rebasing onto main, fixing conflicts, and pushing. Also fires on Japanese triggers コンフリクト解消, 衝突解決, マージ競合を直す, リベースで解決.
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

- If the user specified a PR number (e.g. `#123` or `123`), run `gh pr view <number> --json number,title,url,headRefName,mergeable` to get PR info
- Otherwise, use the PR associated with the current branch (from the Context above)
- If no PR is found, display the following and stop:
  > No PR found. Run this command on a branch with an associated PR, or specify a PR number.

### Step 2: Verify current branch matches the PR head

Before touching the working tree, confirm that the currently checked-out branch is the PR's head branch. Otherwise rebasing onto `main` here would pollute an unrelated local branch.

1. Get `headRefName` from the PR (fetch it via `gh pr view` if not already available).
2. Compare `headRefName` with the current branch (`git branch --show-current`).
3. If they differ:
   - If no explicit PR number was provided (i.e. the PR was auto-detected from the current branch), report the mismatch and stop — do NOT rebase the wrong branch onto `main`.
   - If the user invoked the skill with an explicit PR number and the current branch is unrelated, run `git switch <headRefName>` (pull from remote with `git switch --track origin/<headRefName>` if it does not exist locally yet). Only proceed after the switch succeeds.
   - If the switch is not possible (dirty worktree, local branch divergence, etc.), report the mismatch and stop — do NOT rebase the wrong branch onto `main`.

### Step 3: Fetch and rebase onto main

**Mid-rebase guard**: Before starting a new rebase, check whether a rebase is already in progress (e.g. from a previous skill invocation that did not finish):

- Inspect `git status` for the phrase `rebase in progress`, or check for the directory `.git/rebase-merge` or `.git/rebase-apply`.
- If a rebase is already in progress, **skip the new `git rebase` invocation** and proceed directly to Step 4 to resume conflict resolution. Running `git rebase origin/main` again here would corrupt the in-progress state.

Otherwise, start a fresh rebase:

1. Run `git fetch origin` to get the latest remote state. **Do not re-run `git fetch` after this point until the push completes** — refreshing the remote-tracking ref between rebase and push weakens the `--force-with-lease` guarantee in Step 5.
2. Run `git rebase origin/main` to replay the current branch onto upstream `main`.
3. If the rebase completes without conflicts, skip to Step 5.

### Step 4: Resolve conflicts

1. Run `git diff --name-only --diff-filter=U` to list conflicting files
2. `Read` each conflicting file and examine the conflict markers (`<<<<<<<`, `=======`, `>>>>>>>`)
3. Understand the intent of both the current branch and the base branch changes, then use `Edit` to resolve the conflicts
4. Stage each resolved file with `git add`
5. Once all conflicts are resolved, run `git rebase --continue` to complete the rebase
6. If any conflict cannot be resolved, report the details to the user and stop (do NOT auto-`git rebase --abort`). The user decides whether to abort with `git rebase --abort`.

### Step 5: Push

Run `git push --force-with-lease` to push the rebased branch.

- The rebase rewrites commit SHAs, so a force push is required.
- `--force-with-lease` (no argument) blocks the push if `origin/<branch>` has advanced since the most recent `git fetch`, preventing accidental overwrite of someone else's push.

### Step 6: Report

Display a summary including:

- List of conflicting files (or note that there were none)
- Summary of how each conflict was resolved
- Push result
