---
name: git-fix-pr-reviews
description: Fetch PR review comments, triage them, and apply fixes. Does not commit or push.
allowed-tools: Bash(gh:*), Bash(git branch:*), Read, Edit
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

### Step 2: Fetch review comments and thread mapping

Get repository info with `gh repo view --json owner,name --jq '.owner.login + "/" + .name'` and call the following three APIs **in parallel**:

1. `gh api --paginate repos/{owner}/{repo}/pulls/{number}/comments` — inline comments (each has an `id` field needed for replies)
2. `gh api --paginate repos/{owner}/{repo}/pulls/{number}/reviews` — review summaries (general comments)
3. Thread-to-comment mapping via GraphQL — fetch all review threads with their comment node IDs:
   ```
   gh api graphql -f query='{ repository(owner: "{owner}", name: "{repo}") { pullRequest(number: {number}) { reviewThreads(first: 100) { nodes { id isResolved comments(first: 100) { nodes { databaseId } } } } } }'
   ```
   Build a lookup map from comment `databaseId` (matches REST API `id`) → thread `id`. This mapping is used in Step 6 to resolve only handled threads.

If there are no review comments at all, display the following and stop:
> No review comments found.

### Step 3: Triage classification

Classify each comment into one of the following 3 categories:

1. **Auto-fix** — Comments with a `suggestion` block or objective bug reports (typos, type errors, missing null checks, etc.). Must not already be resolved.
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
- **Skip (inline comment)**: Reply with a brief reason (e.g. "Intentional design", "Already fixed", "Pre-existing issue — out of scope for this PR") using `gh api repos/{owner}/{repo}/pulls/{number}/comments/{comment_id}/replies -f body='<reason>'`. Use the comment `id` from Step 2.
- **Skip (review summary)**: No reply needed — review summaries are general text (LGTM, etc.) and don't support inline replies.

### Step 6: Resolve handled threads

After all fixes are applied and skip replies are posted, resolve **only the threads that were handled** in Step 5. Do NOT resolve threads that were not triaged or handled.

1. During Step 5, collect the **comment IDs** of every comment you handled (auto-fixed, user-approved, or replied to with a skip reason).
2. Using the comment-to-thread mapping built in Step 2, look up the corresponding **thread ID** for each handled comment ID.
3. Resolve each thread (skip threads that are already resolved):
   ```
   gh api graphql -f query='mutation { resolveReviewThread(input: {threadId: "{thread_id}"}) { thread { isResolved } } }'
   ```

This ensures only threads with a reply or fix are resolved. Unhandled threads (e.g., new comments added after triage, or threads from other review rounds) remain unresolved.

### Step 7: Report

After all fixes are applied, display a summary including:

- Number of auto-fixed items
- Number of user-approved fixes
- Number of skipped items
- Number of threads resolved
- List of modified files

**Important**: Do NOT commit or push. The user will do so explicitly.
