---
name: git-commit
description: Create a git commit with branch safety check. Refuses to commit on main or release branches.
metadata:
  short-description: Create a git commit
---

# Git Commit

Create a single git commit with an appropriate message based on the current changes.

## Context

- Current git status: !`git status`
- Current git diff (staged and unstaged changes): !`git diff HEAD`
- Current branch: !`git branch --show-current`
- Recent commits: !`git log --oneline -10`

## Branch Safety Check

Before committing, check the current branch name. If the current branch is `main` or matches the pattern `v*.*.*` (e.g. `v0.0.8`):
- **STOP** — do not commit
- Tell the user: "Cannot commit on `<branch>`. Create a feature branch first (use /git-branch-naming)."

## Steps

1. Verify the current branch passes the safety check above
2. Stage and create a single commit with an appropriate message based on the changes
3. Use conventional commit format (e.g. `feat:`, `fix:`, `docs:`, `refactor:`, `test:`, `chore:`)

Do not perform any actions outside the scope of this skill.
