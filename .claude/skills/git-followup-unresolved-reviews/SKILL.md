---
name: git-followup-unresolved-reviews
description: Find unresolved PR review threads where the reviewer was the last to speak and post short follow-up replies (e.g. "tracked in #NNN", "acknowledged"). Does not resolve threads and does not ping threads we were last to speak on. Use when the user asks to "ping unresolved reviews", "follow up on open review threads", "nudge CodeRabbit to re-verify", or "re-ping review comments".
allowed-tools: Bash(gh:*), Bash(git branch:*)
metadata:
  short-description: Re-ping unresolved PR review threads with follow-up replies
---

# Git Follow-up Unresolved Reviews

Find unresolved PR review threads where the last comment is not from the authenticated viewer, classify them, and post short follow-up replies to close the loop without resolving the threads.

## Context

- Current branch: !`git branch --show-current`
- PR info: !`gh pr view --json number,title,url,state 2>/dev/null || echo "No PR found"`

## Inputs

User input: $ARGUMENTS

## Steps

### Step 1: Identify PR

- If the user specified a PR number (e.g. `#123` or `123`), use that number.
- Otherwise, use the PR associated with the current branch (from the Context above).
- If no PR is found, display the following and stop:
  > No PR found. Run this command on a branch with an associated PR, or specify a PR number.
- If the PR is not open (`state != OPEN`), display a warning and stop:
  > PR is not open (state: `<state>`). Follow-up is only meaningful on open PRs.

### Step 2: Fetch viewer login and repository metadata (in parallel)

Run all three commands **in parallel**:

1. Get the GitHub login of the currently authenticated user:

   ```shell
   gh api user --jq .login
   ```

   Store this as `viewer`.

2. Get repository owner and name as separate variables:

   ```shell
   OWNER=$(gh repo view --json owner --jq '.owner.login')
   REPO=$(gh repo view --json name --jq '.name')
   ```

   Use `OWNER` wherever `<owner>` or `{owner}` appears in subsequent steps, and `REPO` wherever `<repo>` or `{repo}` appears.

### Step 3: Fetch unresolved review threads

Query review threads via GraphQL (note: limited to 100 threads; PRs with more than 100 threads may need manual verification):

```shell
gh api graphql -f query='{
  repository(owner: "<owner>", name: "<repo>") {
    pullRequest(number: <PR>) {
      reviewThreads(first: 100) {
        nodes {
          isResolved
          comments(last: 50) {
            nodes {
              databaseId
              author { login }
              body
              path
              originalLine
            }
          }
        }
      }
    }
  }
}'
```

`comments(last: 50)` fetches the full thread history in a single pass — needed in Step 5 to scan all comments for issue references. If the API call fails or returns no data, report the error and stop.

### Step 4: Filter target threads

From the results in Step 3, keep only threads where **both** conditions hold:

1. `isResolved == false`
2. The last comment's `author.login` is **not** equal to `viewer`

Threads where `viewer` was the last to speak are intentionally skipped — posting on top of our own comment would interfere with CodeRabbit's auto-resolve check.

If no threads match, display:
> No unresolved review threads where the reviewer was the last to speak. Nothing to do.

Then stop.

### Step 5: Classify each thread

For each filtered thread, scan the `comments` array to classify:

**Category A — Tracking issue already filed**:
- Condition: a comment where `author.login == viewer` contains a GitHub issue/PR reference (`#\d+`) AND that comment also contains language like "tracked in", "filed as", "filed in", "see #NNN", "created #NNN", or similar.
- Draft reply: `@{last-commenter-login} Thanks — this is tracked in #NNN. No further action needed on this thread; closing the loop.`
  - Replace `#NNN` with the actual issue reference found in `viewer`'s comment.
  - If multiple such references exist in viewer's comments, use the most recent one.

**Category B — Reviewer said no action needed**:
- Condition: the **last** comment (from the reviewer) matches one or more of these signals (case-insensitive, word-boundary match where noted): `no action`, `not needed`, `can skip`, `not blocking`, `won't fix`, `wont fix`, `\bnit\b` (matches "nit", "nit,", "nit —", etc.), `optional`, `minor`, `up to you`, `feel free to ignore`, `not required`, `just a suggestion`.
- Draft reply: `@{last-commenter-login} Acknowledged — no further action on our side. Closing the loop.`

**Category C — Other**:
- Anything that doesn't clearly fit A or B.
- Do **not** draft an auto-reply. Surface in the triage table and let the user decide.

### Step 6: Confirmation table

Display the following table with all filtered threads. For each thread, use `path` and `originalLine` from the last comment node to populate the File column:

| # | Category | Reviewer | File | Last comment (excerpt) | Draft reply |
|---|----------|----------|------|------------------------|-------------|
| 1 | A: Tracking issue | @alice | `src/foo.cpp:42` | "Tracked in #999..." | `@alice Thanks — this is tracked in #999...` |
| 2 | B: No action needed | @bob | `include/bar.hpp:10` | "Minor nit, optional..." | `@bob Acknowledged — no further action...` |
| 3 | C: Other | @carol | `src/baz.cpp:5` | "What about the edge case..." | _(manual)_ |

Then display:
```text
For categories A and B, replies will be posted automatically after your approval.
Category C threads are listed for your information — no auto-reply will be posted.

Approve all auto-replies? [Y/n / comma-separated numbers to approve selectively, e.g. "1,3"]
```

Wait for user input:
- `y`, `yes`, or empty → approve all A and B items.
- `n` or `no` → skip all; report the triage table and stop.
- Comma-separated numbers (e.g. `1,3`) → approve only those items; skip the rest silently.

### Step 7: Post replies

For each approved thread (categories A and B), post replies **in parallel** where possible:

1. Identify the `databaseId` of the **last comment** in the thread.
   - `databaseId` from the GraphQL response is the same value as the REST `id` field. Use it directly in the REST endpoint below.
2. Post a reply using the REST API:

   ```shell
   gh api repos/{owner}/{repo}/pulls/{PR}/comments/{databaseId}/replies \
     -f body='{draft reply body}'
   ```

   Use the draft reply body from Step 5 (with the `@{login}` mention at the top).

3. After posting, print a confirmation line:
   > ✓ Replied to thread on `{path}:{originalLine}` (reviewer: @`{login}`)

**Do not resolve threads.** Resolving is the reviewer's (or CodeRabbit's) call.

If a `gh api` call fails, report the error and continue to the next thread.

### Step 8: Report

After all replies are posted, display a summary:

```text
Summary
───────
Category A (tracking issue): N threads, N replies posted
Category B (no action needed): N threads, N replies posted
Category C (other / manual): N threads listed, 0 auto-replies
Skipped (viewer was last to speak): N threads

Replies were posted. Threads have NOT been resolved — CodeRabbit and reviewers will verify and resolve at their discretion.
```

---

### Do not resolve threads manually

**Never call `gh api` or any other tool to mark review threads as resolved.**

Per project policy (AGENTS.md `PR レビューコメントの Resolve`): CodeRabbit reads each reply and resolves the thread itself once satisfied. Human reviewers also resolve at their discretion. Manually resolving short-circuits that verification loop and can hide missed fixes or follow-up questions.

This skill only posts replies — the resolve decision always stays with the reviewer.

### Rationale — why @mention the last commenter

- **CodeRabbit**: The `@coderabbitai` mention triggers CodeRabbit's reply-review hook, which re-evaluates the thread and auto-resolves it if the response is satisfactory. Without the mention, CodeRabbit may not re-check.
- **Human reviewers**: The `@login` mention sends a GitHub notification so the reviewer sees the follow-up without having to poll.

Always derive the login from the API response (`author.login`) — never hardcode `@coderabbitai` or any other handle.
