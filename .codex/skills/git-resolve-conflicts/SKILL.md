---
name: git-resolve-conflicts
description: Resolve PR merge conflicts locally by merging the base branch, fixing conflicts, and pushing.
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

### Step 2: Fetch and merge base branch

1. Get the base branch name from the PR's `baseRefName`
2. Run `git fetch origin` to get the latest remote state
3. Run `git merge origin/<base branch>` to merge the base branch into the current branch
4. If the merge completes without conflicts, skip to Step 4

### Step 3: Resolve conflicts

1. Run `git diff --name-only --diff-filter=U` to list conflicting files
2. `Read` each conflicting file and examine the conflict markers (`<<<<<<<`, `=======`, `>>>>>>>`)
3. Understand the intent of both the current branch and the base branch changes, then use `Edit` to resolve the conflicts
4. Stage each resolved file with `git add`
5. Once all conflicts are resolved, run `git merge --continue` to complete the merge
6. If any conflict cannot be resolved, report the details to the user and stop (do NOT auto-abort the merge)

### Step 4: Push

Push the branch to origin.

### Step 5: Report

Display a summary including:

- Base branch that was merged
- List of conflicting files (or note that there were none)
- Summary of how each conflict was resolved
- Push result
