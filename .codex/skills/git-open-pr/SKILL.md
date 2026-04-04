---
name: git-open-pr
description: Mark a draft pull request as ready for review.
metadata:
  short-description: Mark a draft PR as ready for review
---

# Git Open PR

Mark a draft pull request as ready for review.

## Context

- Current branch: !`git branch --show-current`
- PR info: !`gh pr view --json number,title,url,isDraft 2>/dev/null || echo "No PR found"`

## Inputs

User input: $ARGUMENTS

## Steps

### Step 1: Identify PR

- If the user specified a PR number (e.g. `#123` or `123`), use that number
- Otherwise, use the PR associated with the current branch (from the Context above)
- If no PR is found, display the following and stop:
  > No PR found. Run this command on a branch with an associated PR, or specify a PR number.

### Step 2: Check PR state

- If the user specified a different PR number, fetch its details with `gh pr view <number> --json number,title,url,isDraft,state`
- If the PR state is not `OPEN`, display the following and stop:
  > PR #<number> is not open (state: <state>).
- If the PR is not a draft, display the following and stop:
  > PR #<number> is already open (not a draft).

### Step 3: Open PR

Run `gh pr ready <number>` to mark the PR as ready for review.

### Step 4: Report

Display a confirmation message including the PR number, title, and URL.
