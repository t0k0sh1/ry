---
name: git-create-pr
description: User-invoked slash command that opens a PR to main from a clean, pushed feature branch. Never invoke autonomously, from another skill, or merely because implementation is complete.
allowed-tools: Bash(git status:*), Bash(gh pr create:*), Bash(git diff:*), Bash(git branch:*), Bash(git log:*), Bash(git rev-parse:*), Read
metadata:
  short-description: Open a PR
---

# Git Create PR

## Invocation Gate

- Run only when the user directly invokes `/git-create-pr`.
- Never invoke this skill autonomously or from another skill.
- Never propose this skill, present it as an option, include it in a plan, or list it as a next step.

## Context

- Current branch: !`git branch --show-current`
- Working tree: !`git status --short`
- Unpushed commits: !`git log @{u}..HEAD --oneline 2>/dev/null || echo "(no upstream)"`

## Steps

### 1. Verify prerequisites

- Stop and report the unmet prerequisite when any of the following is true:
  - Working tree has uncommitted changes (status non-empty)
  - There are local commits not yet on the remote
  - The branch has no upstream
  - Currently on `main`
- Do not invoke or suggest `/git-push`.
- Continue only on a clean, up-to-date feature branch.

### 2. Open PR

- Run `gh pr create --base main` (open, not draft).
- Title: Conventional Commits format.
- Body: 1-3 line summary + `Closes #<issue>` for the linked issue.
