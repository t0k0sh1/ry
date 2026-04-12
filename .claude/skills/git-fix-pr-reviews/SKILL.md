---
name: git-fix-pr-reviews
description: Fetch PR review comments, triage them, apply fixes, and reply to every handled item so reviewers (e.g. CodeRabbit) can learn from the outcome. Does not commit or push.
allowed-tools: Bash(gh:*), Bash(git branch:*), Read, Edit
metadata:
  short-description: Triage, fix, and reply to PR review comments
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
2. `gh api --paginate repos/{owner}/{repo}/pulls/{number}/reviews` — review summaries (general comments). Read each review `body` and extract any **nitpick / suggestion items embedded inside the summary** — these are NOT inline comments and do not have their own `id`, so they cannot receive inline replies. Treat them as separate triage items referenced by file/line in the summary body. Common reviewer-specific markers to recognize (non-exhaustive): CodeRabbit uses a `<summary>🧹 Nitpick comments</summary>` `<details>` block; other reviewers may use `## Nitpicks`, `### Suggestions`, or similar section headers. If the review body has no recognizable nitpick section, treat the whole body as a general summary comment and skip this extraction.
3. Thread-to-comment mapping via GraphQL — fetch all review threads with their comment node IDs:

   ```shell
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

### Step 5: Apply fixes and reply to every handled item

**Every handled item must receive a reply**, not just skipped ones. Reviewers like CodeRabbit use reply content to learn and improve future review precision — silently resolving threads wastes that signal. The reply should state **what was done and why** in 1-5 lines.

For each category:

- **Auto-fix**: Read the target file with `Read` and apply the fix with `Edit`. After the fix, reply to the inline comment describing the concrete change (which file/line, what the fix does, and the commit/approach if it's not obvious). If the auto-fix decision was non-trivial or the reviewer's suggestion was partially adjusted, say so.
- **Needs confirmation — approved**: Apply the fix after user approval, then reply the same way as Auto-fix. If the user's instruction changed the approach from what the reviewer suggested, note that in the reply so the reviewer understands the deviation.
- **Needs confirmation — rejected / deferred**: Do not fix. Reply with a clear reason (e.g. "Intentional design — see ...", "Out of scope for this PR, filed as issue #NNN", "Will address in a follow-up").
- **Skip (inline comment)**: Reply with a brief reason (e.g. "Already fixed in commit abc1234", "Pre-existing — out of scope", "LGTM, no change needed").
- **Skip (review summary — general text like LGTM)**: No reply needed.
- **Review-summary nitpicks (embedded in the review body)**: These have no individual comment `id` and cannot receive inline replies. Instead, post **one consolidated issue comment** on the PR via `gh pr comment <number> --body '...'` that addresses each nitpick (one section per item, referencing the original file/line from the summary). At the top of the consolidated comment, mention the reviewer's handle so they pick it up for learning. The handle should be taken from the review author (e.g. the `user.login` field of the review JSON; common examples are `@coderabbitai` for CodeRabbit, `@copilot-pull-request-reviewer` for Copilot). If the reviewer cannot be determined, omit the mention rather than hardcoding a specific handle.

**Reply APIs**:

- Inline-comment reply:

  ```shell
  gh api repos/{owner}/{repo}/pulls/{number}/comments/{comment_id}/replies -f body='<markdown body>'
  ```

  Use the REST `id` from Step 2 (not the GraphQL node id).

- Consolidated PR issue comment (for review-summary nitpicks only):

  ```shell
  gh pr comment {number} --body '<markdown body>'
  ```

**Reply content guidance**:

- Lead with the outcome (`Fixed in abc1234.` / `Skipping — reason: ...`), then the rationale.
- Cite the commit SHA for fixes so the reviewer can verify.
- For partial fixes, be explicit about what was and wasn't addressed.
- Do not argue with the reviewer — if you disagree, briefly state why and move on.
- Avoid emojis and filler (`Thanks for the review!` is fine at most once per batch).

### Step 6: Resolve handled threads

After all fixes are applied and every handled item has received a reply (Step 5), resolve **only the threads that were handled**. Do NOT resolve threads that were not triaged or handled.

1. During Step 5, collect the **comment IDs** of every handled inline/thread comment (auto-fixed, user-approved, rejected with reason, or skipped with reason).
2. Using the comment-to-thread mapping built in Step 2, look up the corresponding **thread ID** for each handled comment ID.
3. If a handled item has no mapped thread ID (e.g., a review-summary nitpick that was addressed via a consolidated PR comment in Step 5), skip it — there is no thread to resolve.
4. Resolve each mapped thread (skip threads that are already resolved):

   ```shell
   gh api graphql -f query='mutation { resolveReviewThread(input: {threadId: "{thread_id}"}) { thread { isResolved } } }'
   ```

This ensures only threads with a reply or fix are resolved. Unhandled threads (e.g., new comments added after triage, or threads from other review rounds) remain unresolved.

### Step 7: Report

After all fixes and replies are applied, display a summary including:

- Number of auto-fixed items
- Number of user-approved fixes
- Number of user-rejected / deferred items
- Number of skipped items
- Number of inline replies posted
- Number of consolidated PR comments posted (for review-summary nitpicks)
- Number of threads resolved
- List of modified files

**Important**: Do NOT commit or push. The user will do so explicitly.

### Rationale — why every handled item gets a reply

Reviewers (especially AI reviewers like CodeRabbit) use reply content as training signal for future reviews on this repository. Silently resolving a thread — even for a correct auto-fix — tells the reviewer nothing about *what* changed or *why* a suggestion was or wasn't taken. A 2-3 line reply stating the fix (or the reason for not fixing) costs almost nothing and directly improves the next review.

Rules of thumb:

- Every triaged item (not just skipped ones) → reply.
- Cite commit SHAs and file/line references when possible.
- For rejected suggestions, state the reason once, clearly, without arguing.
- For review-summary nitpicks that have no inline thread, a single consolidated PR comment is the correct channel.
