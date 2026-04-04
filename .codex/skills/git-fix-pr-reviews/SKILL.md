---
name: git-fix-pr-reviews
description: Fetch PR review comments, triage them, apply fixes, and automatically commit and push the changes.
metadata:
  short-description: Triage and fix PR review comments
---

# Git Fix PR Reviews

Fetch PR review feedback, classify it, apply fixes, and push.

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

1. `gh api repos/{owner}/{repo}/pulls/{number}/comments` — inline comments (attached to specific files/lines)
2. `gh api repos/{owner}/{repo}/pulls/{number}/reviews` — review summaries (general comments)

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
- **Skip**: Do nothing

### Step 6: Report

After all fixes are applied, display a summary including:

- Number of auto-fixed items
- Number of user-approved fixes
- Number of skipped items
- List of modified files

### Step 7: Commit and push (only when all reviews are resolved)

**Gate**: Check the PR for unresolved review threads using the GraphQL API:

```bash
gh api graphql --paginate -f query='
query($owner:String!,$repo:String!,$pr:Int!,$endCursor:String) {
  repository(owner:$owner,name:$repo) {
    pullRequest(number:$pr) {
      reviewThreads(first:100,after:$endCursor) {
        nodes { isResolved }
        pageInfo { hasNextPage endCursor }
      }
    }
  }
}' -f owner='{owner}' -f repo='{repo}' -F pr='{number}' \
  --jq '[.data.repository.pullRequest.reviewThreads.nodes[] | select(.isResolved | not)] | length' \
  | awk '{s+=$1} END {print s+0}'
```

- If the count is **greater than 0**, display the count and stop:
  > <N> unresolved review thread(s) remain. Commit and push skipped — resolve all threads first.
- If **0**, proceed with commit and push.

Before committing, verify the current branch is not `main` or `v*.*.*`. If it is, stop and report the issue.

1. Stage all modified files with `git add`
2. Check if there are any staged changes (e.g. `git diff --cached --quiet`). If no changes, display "No changes to commit; skipping commit and push" and stop
3. Create a single commit with message: `fix: address PR review feedback`
4. Push to origin
