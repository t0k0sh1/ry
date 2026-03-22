---
name: git-pr-review-fixes
description: Fetch pull request review comments, triage them, and apply safe fixes in this repository. Use when the user asks to address PR review comments.
metadata:
  short-description: Triage and fix PR review comments
---

# Fix PR Reviews

Fetch PR review feedback with `gh`, classify it, and apply fixes where appropriate.

## Inputs

- A PR number like `123` or `#123`, or no number to use the PR associated with the current branch

## Steps

1. Determine the target PR.
If the user specifies a PR number, use it.
Otherwise, use the PR associated with the current branch from `gh pr view`.
If no PR is found, stop and tell the user to specify a PR number or run the skill on a branch with an open PR.

2. Fetch review data.
Run `gh repo view --json owner,name --jq '.owner.login + "/" + .name'` to resolve the repository.
Fetch inline comments with `gh api --paginate repos/{owner}/{repo}/pulls/{number}/comments`.
Fetch review summaries with `gh api --paginate repos/{owner}/{repo}/pulls/{number}/reviews`.
If there are no actionable review comments, stop and report that none were found.

3. Triage each comment into one of these categories.
- `Auto-fix`: clear, objective fixes such as typos, missing checks, narrow bug fixes, or explicit suggestion blocks
- `Needs confirmation`: design changes, refactors, tradeoff decisions, or unclear intent
- `Skip`: resolved comments, praise, or comments without a concrete change request

4. Present a compact summary table before editing.
Include category, reviewer, file, line, and a short summary.

5. Apply fixes.
Apply `Auto-fix` items directly.
Ask the user before applying `Needs confirmation` items.
Ignore `Skip` items.

6. Report the result.
Summarize how many items were auto-fixed, how many required confirmation, how many were skipped, and which files changed.

## Repository Rules

- Keep fixes within the active PR scope
- Do not commit, push, or merge as part of this skill
- Prefer minimal edits that directly address the review comment
