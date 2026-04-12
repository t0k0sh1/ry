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
5. If the branch exists locally, run `git switch <branch>`. Otherwise (exists only on remote), run `git switch --track origin/<branch>`, which also sets upstream.
6. **Check upstream before pulling**. Run `git rev-parse --abbrev-ref --symbolic-full-name @{u} 2>/dev/null` to verify an upstream exists. If the current branch has no upstream (a local branch never pushed, or upstream was cleared):
   - If `origin/<branch>` exists, either run `git pull origin <branch>` (explicit remote + branch, no upstream assumption) or set upstream first with `git branch --set-upstream-to=origin/<branch>` and then `git pull`.
   - If no remote counterpart exists, skip pulling and report that the branch is local-only.
7. When upstream is configured, run `git pull` to pull the latest changes.
8. Report the result, including whether the branch was local-only or synced with a remote.
