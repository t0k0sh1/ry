---
name: git-switch-branch
description: Switch to a branch and pull latest changes.
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
2. Run `git fetch origin` to refresh remote branch information.
3. Check the available branches to confirm the branch exists (local or remote).
4. If the branch does not exist locally or remotely, notify the user and stop.
5. If the branch exists locally, run `git switch <branch>`.
6. If the branch exists only on remote, run `git switch --track origin/<branch>`.
7. Run `git pull` to pull the latest changes.
8. Report the result.
