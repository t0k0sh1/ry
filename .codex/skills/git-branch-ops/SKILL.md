---
name: git-branch-ops
description: Switch branches, pull the latest changes, or sync the current branch with its target branch in this repository. Use when the user asks to switch branches or bring the current branch up to date.
metadata:
  short-description: Switch or sync branches safely
---

# Git Branch Operations

Handle branch switching and branch synchronization for this repository.

## Supported Tasks

- Switch to a specified branch and pull the latest changes
- Sync the current branch with a target branch by merging `origin/<target>` into the current branch

Default target branch is `main` when the user does not specify one.

## Switching Branches

1. Read the requested branch name from the user's prompt. If none is provided, default to `main`.
2. Check available branches with `git branch -a`.
3. If the branch does not exist locally or remotely, stop and report that it was not found.
4. Run `git switch <branch>`.
5. Run `git pull`.
6. Summarize the branch switched to and whether pull succeeded.

## Syncing the Current Branch

Direction matters: merge `origin/<target>` into the current branch. Do not merge the current branch into the target branch.

1. Determine the target branch from the user's prompt. If none is provided, use `main`.
2. Read the current branch with `git branch --show-current`.
3. If the current branch already equals the target branch, stop and report that sync is unnecessary.
4. Run `git fetch origin`.
5. Run `git merge origin/<target>`.
6. If conflicts occur, stop, report the conflict, and leave the merge state intact for user-directed resolution. Do not abort the merge automatically.
7. On success, summarize the merge result.

## Repository Rules

- Do not create commits, push, or open PRs as part of this skill
- If the repository state is dirty and the requested operation is risky, report it clearly before continuing
