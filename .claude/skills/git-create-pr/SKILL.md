---
name: git-create-pr
description: Open a PR to main from the current feature branch. Delegates to /git-push first if there are uncommitted or unpushed changes, or when on main (in which case /git-push auto-creates the feature branch per its Step 0). PRs are opened (not draft).
allowed-tools: Bash(git status:*), Bash(gh pr create:*), Bash(git diff:*), Bash(git branch:*), Bash(git log:*), Bash(git rev-parse:*), Read
metadata:
  short-description: Open a PR
---

# Git Create PR

## Context

- Current branch: !`git branch --show-current`
- Working tree: !`git status --short`
- Unpushed commits: !`git log @{u}..HEAD --oneline 2>/dev/null || echo "(no upstream)"`

## Steps

### 1. Sync if needed

- Invoke `/git-push` first when **any** of the following is true:
  - Working tree has uncommitted changes (status non-empty)
  - There are local commits not yet on the remote
  - The branch has no upstream
  - Currently on `main` (`/git-push` Step 0 auto-creates a feature branch from main; AGENTS.md forbids PRs originating from main)
- After `/git-push` finishes, the current branch will be a feature branch with everything pushed; continue to Step 2.
- If none of the above apply (already on a clean, up-to-date feature branch), skip to Step 2.
- **Edge case**: if on `main` with a clean working tree and no unpushed work, STOP and tell the user there is nothing to PR.

### 2. Open PR

- Run `gh pr create --base main` (open, not draft).
- Title: Conventional Commits format.
- Body: 1-3 line summary + `Closes #<issue>` for the linked issue.
