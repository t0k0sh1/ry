---
name: git-fix-pr-reviews
description: Fetch PR review comments, triage them, and apply fixes. Does not commit or push.
metadata:
  short-description: Triage and fix PR review comments
---

# Git Fix PR Reviews

Fetch PR review feedback, classify it, and apply fixes.

## Context

- Current branch: !`git branch --show-current`
- PR info: !`gh pr view --json number,title,url,reviewDecision,isDraft 2>/dev/null || echo "No PR found"`

## Inputs

User input: $ARGUMENTS

## Steps

### Step 1: Identify PR

- If the user specified a PR number (e.g. `#123` or `123`), use that number
- Otherwise, use the PR associated with the current branch (from the Context above)
- If no PR is found, display the following and stop:
  > No PR found. Run this command on a branch with an associated PR, or specify a PR number.

### Step 2: Fetch review comments

Get repository info with `gh repo view --json owner,name --jq '.owner.login + "/" + .name'` and call the following two APIs **in parallel**:

1. `gh api --paginate repos/{owner}/{repo}/pulls/{number}/comments` — inline comments (each has an `id` field needed for replies)
2. `gh api --paginate repos/{owner}/{repo}/pulls/{number}/reviews` — review summaries (general comments)

If there are no review comments at all, display the following and stop:
> No review comments found.

### Step 3: Triage classification

Classify each comment into one of the following 3 categories:

1. **Auto-fix** — Comments with a `suggestion` block or objective bug reports (typos, type errors, missing null checks, etc.). Must not be resolved.
2. **Needs confirmation** — Design changes, refactoring proposals, optimization trade-offs, or anything difficult to auto-judge.
3. **Skip** — Resolved comments, LGTM/praise, or questions without specific fix suggestions.

### Step 4: Summary table

Display the classification results for all comments in a table:

| # | Category | Reviewer | File | Line | Summary |
|---|----------|----------|------|------|---------|
| 1 | Auto-fix | ... | ... | ... | ... |
| 2 | Needs confirmation | ... | ... | ... | ... |
| 3 | Skip | ... | ... | ... | ... |

### Step 5: Apply fixes

- **Auto-fix**: Read the target file with `Read` and apply the fix immediately with `Edit`
- **Needs confirmation**: Ask the user which ones to apply, and only fix those that are approved
- **Skip (inline comment)**: Reply using `gh api repos/{owner}/{repo}/pulls/{number}/comments/{comment_id}/replies -f body='<reason>'` with a brief reason (e.g. "Intentional design", "Already fixed", "Nitpick — not applicable"). Use the comment `id` from Step 2.
- **Skip (review summary)**: No reply needed — review summaries are general text (LGTM, etc.) and don't support inline replies.

### Step 6: Report

After all fixes are applied, display a summary including:

- Number of auto-fixed items
- Number of user-approved fixes
- Number of skipped items
- List of modified files

**Important**: Do NOT commit or push. The user will do so explicitly.

### Do not resolve threads manually

**Do not resolve review threads manually.** CodeRabbit and human reviewers verify each reply and resolve threads themselves once they are satisfied. Manually resolving a thread short-circuits that verification loop and can hide follow-up feedback or missed fixes.
