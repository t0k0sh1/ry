---
name: git-resolve-conflicts
description: Resolve PR merge conflicts locally by merging the base branch, fixing conflicts, and pushing.
allowed-tools: Bash(git:*), Bash(gh pr view:*), Read, Edit
metadata:
  short-description: Resolve PR conflicts locally and push
---

# Git Resolve Conflicts

Resolve merge conflicts for a PR by merging the base branch locally, fixing conflicts, and pushing.

## Context

- Current branch: !`git branch --show-current`
- PR info: !`gh pr view --json number,title,url,baseRefName,mergeable 2>/dev/null || echo "No PR found"`
- Current git status: !`git status`

## Inputs

User input: $ARGUMENTS

## Steps

### Step 1: Identify PR

- If the user specified a PR number (e.g. `#123` or `123`), run `gh pr view <number> --json number,title,url,baseRefName,mergeable` to get PR info
- Otherwise, use the PR associated with the current branch (from the Context above)
- If no PR is found, display the following and stop:
  > No PR found. Run this command on a branch with an associated PR, or specify a PR number.

### Step 2: Verify current branch matches the PR head

Before touching the working tree, confirm that the currently checked-out branch is the PR's head branch. Otherwise merging the base branch here would pollute an unrelated local branch.

1. Get `headRefName` and `baseRefName` from the PR (you already have `baseRefName` from Step 1 — also fetch `headRefName` in the same `gh pr view` call).
2. Compare `headRefName` with the current branch (`git branch --show-current`).
3. If they differ:
   - If the user invoked the skill with an explicit PR number and the current branch is unrelated, run `git switch <headRefName>` (pull from remote with `git switch --track origin/<headRefName>` if it does not exist locally yet). Only proceed after the switch succeeds.
   - If the switch is not possible (dirty worktree, local branch divergence, etc.), report the mismatch and stop — do NOT merge the base into the wrong branch.

### Step 3: Fetch and merge base branch

1. Get the base branch name from the PR's `baseRefName`
2. Run `git fetch origin` to get the latest remote state
3. Run `git merge origin/<base branch>` to merge the base branch into the current branch
4. If the merge completes without conflicts, skip to Step 5

### Step 4: Resolve conflicts

1. Run `git diff --name-only --diff-filter=U` to list conflicting files
2. `Read` each conflicting file and examine the conflict markers (`<<<<<<<`, `=======`, `>>>>>>>`)
3. Understand the intent of both the current branch and the base branch changes, then use `Edit` to resolve the conflicts
4. Stage each resolved file with `git add`
5. Once all conflicts are resolved, run `git merge --continue` to complete the merge
6. If any conflict cannot be resolved, report the details to the user and stop (do NOT auto-abort the merge)

### Step 5: Push

Push the branch to origin.

### Step 6: Report

Display a summary including:

- Base branch that was merged
- List of conflicting files (or note that there were none)
- Summary of how each conflict was resolved
- Push result
