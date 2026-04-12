---
name: harvest-review-knowledge
description: Extract project knowledge from CodeRabbit review threads on a PR and append to KNOWLEDGE.md. Use when the user wants to harvest, extract, or sync review feedback into KNOWLEDGE.md, or after a PR is merged and review learnings should be captured. Also trigger when the user mentions "harvest knowledge", "review knowledge", or "CodeRabbit learnings".
allowed-tools: Bash(gh:*), Bash(git branch:*), Bash(git log:*), Read, Edit
metadata:
  short-description: Harvest CodeRabbit feedback into KNOWLEDGE.md
---

# Harvest Review Knowledge

Extract actionable project knowledge from CodeRabbit review comment threads on a PR and append approved entries to `KNOWLEDGE.md`.

CodeRabbit's review threads often contain project-specific insights that emerge from the dialogue between the reviewer and the developer — especially when CodeRabbit withdraws a comment or records a new learning. This skill captures those insights before they get buried in closed PRs.

## Context

- Current branch: !`git branch --show-current`
- PR info: !`gh pr view --json number,title,url 2>/dev/null || echo "No PR found"`
- Repository: !`gh repo view --json owner,name --jq '.owner.login + "/" + .name'`

## Inputs

User input: $ARGUMENTS

## Steps

### Step 1: Identify PR

- If the user specified a PR number (e.g. `#909` or `909`), use that number
- Otherwise, use the PR associated with the current branch (from the Context above)
- If no PR is found, display the following and stop:
  > No PR found. Specify a PR number or run this on a branch with an associated PR.

### Step 2: Fetch CodeRabbit comments

Get repository info from the Context above and call the following three APIs **in parallel**:

1. `gh api --paginate repos/{owner}/{repo}/pulls/{number}/comments` — inline review comments
2. `gh api --paginate repos/{owner}/{repo}/issues/{number}/comments` — issue-level comments (includes CodeRabbit summary comments)
3. `gh pr view {number} --json title,mergedAt,closedAt,createdAt` — PR metadata for dating entries

Filter all comment results to `user.login == "coderabbitai[bot]"`.

If no CodeRabbit comments exist, display the following and stop:
> No CodeRabbit comments found on PR #{number}.

### Step 3: Reconstruct threads and extract knowledge signals

**For inline review comments** (`pulls/{number}/comments`):

1. Group comments by thread: comments with `in_reply_to_id == null` are thread roots; comments with `in_reply_to_id != null` are replies
2. For each CodeRabbit root comment, reconstruct the conversation chain:
   - Developer reply(s) — non-bot comments replying to the root
   - CodeRabbit follow-up(s) — bot comments replying to the developer
3. Mark each thread with knowledge signals by scanning CodeRabbit's follow-up responses:

| Signal | How to detect | Knowledge value |
|--------|---------------|-----------------|
| `learnings_added` | Body contains `✏️ Learnings added` | **High** — CodeRabbit recorded a new project-specific rule |
| `withdrawn` | Body contains `<!-- <review_comment_addressed> -->` AND text includes "withdrawn", "understood", "dropping", "acknowledged", "you're right" | **High** — a nuanced project convention was explained |
| `learnings_used` | Body contains `🧠 Learnings used` | **Low** — existing knowledge applied, not new |
| `severity` | Root comment contains `🔴 Critical`, `🟠 Major`, or `🟡 Minor` | Context for prioritization |

**For issue-level comments** (`issues/{number}/comments`):

Scan for CodeRabbit summary comments that contain `✏️ Learnings added` sections. These are standalone knowledge signals not tied to inline threads.

### Step 4: Extract knowledge candidates

Filter threads to those with high knowledge value, then extract candidates in priority order:

**Tier 1 — Withdrawn/acknowledged with new learning** (highest value):
Threads where `learnings_added == true` AND `withdrawn == true`. These capture project conventions that aren't obvious from the code — CodeRabbit flagged something, the developer explained why it's intentional, and CodeRabbit learned a new rule. Extract:
- The project rule or convention from the developer's explanation
- The "why" behind the rule
- The CodeRabbit learning text (from the `✏️ Learnings added` `<details>` block — the `Learning:` field)

**Tier 2 — Accepted Major/Critical fixes with generalizable pattern**:
Threads where severity is Major or Critical AND the fix was applied (not withdrawn). Only include if the pattern is **generalizable** — skip PR-specific bug fixes that don't teach a reusable rule. Extract:
- The class of mistake (not the specific instance)
- The rule to prevent recurrence

**Tier 3 — New learning without withdrawal**:
Threads where `learnings_added == true` but not withdrawn. The developer's response taught CodeRabbit something without disagreeing. Extract the learning content.

**Skip entirely**:
- Threads with only `🧠 Learnings used` (existing knowledge, not new)
- Threads with no developer reply
- Style/formatting-only threads
- PR-specific issues that don't generalize (e.g., "this variable should be named X")

For each candidate, draft a `KNOWLEDGE.md` entry following the writing rules in the file header:

```markdown
### Concise title stating the rule

**Source**: #{number} PR review (CodeRabbit, YYYY-MM-DD)
**Tags**: tag1, tag2, tag3

**Context**: What happened — the initial review comment and the developer's explanation, condensed to 2-3 sentences.

**Rule**: The actionable lesson — what to do (or not do) in future work.
```

Use the PR's `mergedAt` (preferred), `closedAt`, or `createdAt` for the date, formatted as `YYYY-MM-DD`.

### Step 5: Assign target sections and deduplicate

Read `KNOWLEDGE.md` with `Read`.

**Section assignment** — map each candidate to the best-matching section based on the review comment's `path` field:

| File path pattern | Section |
|-------------------|---------|
| `tests/`, `*test*` | Testing |
| `src/codegen*`, `include/ry/codegen*` | Codegen |
| `src/parser*`, `src/lexer*`, `include/ry/parser*` | Parser / Lexer |
| `src/runtime*`, `include/ry/runtime*` | Runtime / Memory |
| `CMakeLists.txt`, `.github/`, `cmake/` | Build / CI |
| `docs/`, `README*`, `CHANGELOG*` | Documentation |
| `share/std/`, `src/runtime_*` (stdlib packages) | Stdlib |
| `.claude/skills/`, `.codex/skills/` | Commands / Environment gotchas |
| Cross-cutting or meta-pattern | Review feedback patterns |

If the path is ambiguous, use the content of the entry to determine the best section.

**Deduplication** — check for existing entries that cover the same knowledge:

1. Search for the PR number in `**Source**:` lines — if an entry for this PR already exists with the same rule, mark as duplicate
2. Search by tags — if an existing entry covers the same rule from a different PR, mark as duplicate
3. Search by title keywords — catch near-duplicates with different phrasing

Remove duplicates from the candidate list and report them.

### Step 6: Present candidates for approval

Display each remaining candidate in a numbered list:

```
## Knowledge candidates from PR #NNN

### Candidate 1 (Tier 1 — Withdrawn + Learning) → Section: Documentation
[Full drafted entry]

Source thread: CodeRabbit flagged X → Developer explained Y → CodeRabbit withdrew and learned Z

---

### Candidate 2 (Tier 3 — New Learning) → Section: Codegen
[Full drafted entry]

Source thread: ...
```

Then ask:
> Review the candidates above. Reply with:
> - `all` to approve all
> - `1,3` to approve specific candidates by number
> - `none` to skip all
> - Or editing instructions for specific candidates (e.g. "2: change the rule to ...")

**Wait for user response before proceeding.**

### Step 7: Append approved entries and report

For each approved entry:

1. Read `KNOWLEDGE.md` with `Read`
2. Find the target section header (e.g., `## Documentation`)
3. Find the end of that section (the next `---` separator or next `## ` header)
4. Insert the new entry before the section separator using `Edit`, with a blank line before the `---`

After all entries are appended, display a summary:

```
## Summary

- **PR**: #NNN — PR title
- **CodeRabbit threads analyzed**: X
- **Knowledge candidates extracted**: Y (Tier 1: a, Tier 2: b, Tier 3: c)
- **Duplicates skipped**: Z
- **Entries approved and appended**: W
  - Section: Title (line NNN)
  - ...
```

**Important**: Do NOT commit or push. The user will do so explicitly.

## Why this skill exists

PR review threads are a rich source of project knowledge that's easy to lose. CodeRabbit's `✏️ Learnings added` sections are especially valuable — they represent moments where a developer corrected a misconception or explained a non-obvious convention. Without this skill, these insights stay buried in closed PR threads. By extracting them into `KNOWLEDGE.md`, future contributors (human and AI) benefit from every past review interaction.
