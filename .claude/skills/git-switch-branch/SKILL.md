---
name: git-switch-branch
description: Switch to a branch and pull latest changes.
allowed-tools: Bash(git switch:*), Bash(git branch:*), Bash(git pull:*)
metadata:
  short-description: Switch to a branch and pull latest changes
---

# Git Switch Branch

Switch to the branch specified by the user and pull the latest changes.

## Context

- Current branch: !`git branch --show-current`
- Available branches: !`git branch -a`

## Inputs

User input: $ARGUMENTS

## Steps

1. Extract the branch name from the user's input. If no branch is specified, default to `main`.
2. Check the available branches listed above to confirm the branch exists (local or remote).
3. If the branch does not exist, notify the user that the branch was not found and stop.
4. If the branch exists, run `git switch <branch>` to switch to it, then run `git pull` to pull the latest changes.
5. Report the result.
