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

### Step 2: Fetch review comments

Get repository owner and name as separate variables:

```shell
OWNER=$(gh repo view --json owner --jq '.owner.login')
REPO=$(gh repo view --json name --jq '.name')
```

Use `OWNER` wherever `<owner>` or `{owner}` appears in subsequent steps, and `REPO` wherever `<repo>` or `{repo}` appears.

Then call the following two APIs **in parallel**:

1. `gh api --paginate repos/{owner}/{repo}/pulls/{number}/comments` — inline comments (each has an `id` field needed for replies)
2. `gh api --paginate repos/{owner}/{repo}/pulls/{number}/reviews` — review summaries (general comments). Read each review `body` and extract any **nitpick / suggestion items embedded inside the summary** — these are NOT inline comments and do not have their own `id`, so they cannot receive inline replies. Treat them as separate triage items referenced by file/line in the summary body. Common reviewer-specific markers to recognize (non-exhaustive): CodeRabbit uses a `<summary>🧹 Nitpick comments</summary>` `<details>` block; other reviewers may use `## Nitpicks`, `### Suggestions`, or similar section headers. If the review body has no recognizable nitpick section, treat the whole body as a general summary comment and skip this extraction.

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

**Every handled item must receive a reply**, not just skipped ones. Reviewers like CodeRabbit use reply content as a training signal and to decide whether to auto-resolve the thread — a clear reply stating what was done and why directly improves review precision. The reply should state **what was done and why** in 1-5 lines.

For each category:

- **Auto-fix**: Read the target file with `Read` and apply the fix with `Edit`. After the fix, reply to the inline comment describing the concrete change (which file/line, what the fix does, and the commit/approach if it's not obvious). If the auto-fix decision was non-trivial or the reviewer's suggestion was partially adjusted, say so.
- **Needs confirmation — approved**: Apply the fix after user approval, then reply the same way as Auto-fix. If the user's instruction changed the approach from what the reviewer suggested, note that in the reply so the reviewer understands the deviation.
- **Needs confirmation — rejected / deferred**: Do not fix. Reply with a clear reason (e.g. "Intentional design — see ...", "Out of scope for this PR, filed as issue #NNN", "Will address in a follow-up").
- **Skip (inline comment)**: Reply with a brief reason (e.g. "Already fixed in commit abc1234", "Pre-existing — out of scope", "LGTM, no change needed").
- **Skip (review summary — general text like LGTM)**: No reply needed.
- **Review-summary nitpicks (embedded in the review body)**: These have no individual comment `id` and cannot receive inline replies. Instead, post **one consolidated issue comment per reviewer** on the PR via `gh pr comment <number> --body '...'`, addressing that reviewer's nitpicks (one section per item, referencing the original file/line from the summary). At the top of each consolidated comment, mention that reviewer's handle from the review author (`user.login`; e.g. `@coderabbitai`, `@copilot-pull-request-reviewer`). If multiple reviewers have nitpicks, post a separate consolidated comment for each. If the reviewer cannot be determined, omit the mention rather than hardcoding a specific handle.

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

### Step 6: Report

After all fixes and replies are applied, display a summary including:

- Number of auto-fixed items
- Number of user-approved fixes
- Number of user-rejected / deferred items
- Number of skipped items
- Number of inline replies posted
- Number of consolidated PR comments posted (for review-summary nitpicks)
- List of modified files

**Important**: Do NOT commit or push. The user will do so explicitly.

### Rationale — why every handled item gets a reply

Reviewers (especially AI reviewers like CodeRabbit) use reply content as training signal for future reviews on this repository. A 2-3 line reply stating the fix (or the reason for not fixing) costs almost nothing and directly improves the next review. CodeRabbit reads each reply to decide whether the thread should be resolved — supplying a clear reply is what enables that automation.

Rules of thumb:

- Every triaged item (not just skipped ones) → reply.
- Cite commit SHAs and file/line references when possible.
- For rejected suggestions, state the reason once, clearly, without arguing.
- For review-summary nitpicks that have no inline thread, a single consolidated PR comment is the correct channel.

### Do not resolve threads manually

**Do not resolve review threads manually.** CodeRabbit and human reviewers verify each reply and resolve threads themselves once they are satisfied. Manually resolving a thread short-circuits that verification loop and can hide follow-up feedback or missed fixes.
