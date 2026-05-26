---
name: triage-side-finding
description: Triage hub for side findings — short-circuits via Q1 (hard-to-reproduce CI detection) / Q2 (explicit user direction) / Q3 (bug-forensics-analyst) / Q4 (three-way 即時修正 / 別 issue 起票 / ユーザー確認). Use during implementation, self-verification, or PR review when an out-of-scope finding surfaces. Origin diagnosis (regression vs pre-existing) is delegated to `bug-forensics-analyst` and reached only via Q3. Also fires on Japanese triggers 副次的な発見, side finding, scope, ついでに直したい, 別 issue 起票, 即時修正, OPEN PR 依存, fold-only, orphan issue 防止, triage, 判定フロー.
allowed-tools: Bash(gh issue:*), Bash(gh search:*), Bash(gh pr:*), Bash(gh api:*), Agent
---

# Triage Side Finding

Neutral triage hub for side findings detected during implementation, self-verification, or PR review. Routes to one of three outcomes — (a) 即時修正 / (b) 別 issue 起票 / (c) ユーザー確認.

> **Source-of-truth note**: previously in `AGENTS.md` §"責務の分離 > スコープ外の問題を発見した場合の対応ルール"; relocated by #1384. The "OPEN PR dependency" gate (Q4(b) Step 1 + fold-only rule) was added by #1694. The Q1-Q4 short-circuit redesign and `bug-forensics-analyst` integration came in #1752.

## Design intent

The skill must **not** bias side findings toward "file a separate issue." The previous `scope-out-issue` skill did, producing two failure modes (#1752): (a) hard-to-reproduce CI findings (ASan / TSan / UBSan / libFuzzer crashes) went stale before triage finished; (b) when the user said "fix it now," rule discipline overrode the call.

Q1 / Q2 short-circuit to immediate fix without invoking any agent or advisor, preserving the reproduction window. Q3 / Q4 run only when Q1 / Q2 did not settle the case.

## Decision Flow

Evaluate in order. **Once a stage settles, do not run later stages** (no `bug-forensics-analyst` or advisor invocation).

### Q1: Reproducing now, but hard to reproduce later?

Covers CI-only sanitizer hits (ASan / TSan / UBSan), libFuzzer crashes, concurrency races, and probabilistic memory corruption — observable right now but flaky on local reproduction.

**Falsifiable criteria** (any one ⇒ Q1 = Yes):

- CI-detected sanitizer / libFuzzer crash that **does not reliably reproduce after 3 local attempts** in `build-asan/` / `build-tsan/` / `build-fuzz/`
- TSan race that vanishes on re-run (intrinsic probabilistic behavior)
- libFuzzer crashing input with no saved corpus
- CI-only with no re-run guarantee (short job retention, log-only artifacts)

**Q1 = No** (proceed to Q2): bug reproducible locally at will, production crash with steps in hand, or pre-existing bug surfaced by a known reproducible test case.

→ **Q1 = Yes**: **fix immediately. Do not invoke `bug-forensics-analyst` or advisor.** Protects the reproduction window — not "while-we're-at-it cleanup" (no conflict with `/plan-rubric`'s no-incidental-fix rule). Persist crash inputs / stack traces (`tests/fuzz/regressions/` etc.) first, then land the fix in the current PR.
→ **Q1 = No**: proceed to Q2.

### Q2: Has the user explicitly directed the disposition?

The user has said "fix it in this branch," "make it a separate issue," "handle it here," etc.

**Before following the direction, report the following in one message** (informed-consent gate):

- **What** — fix target (file / function / change)
- **Where** — impact surface (modules / tests / dependencies)
- **Estimated diff size** — lines / files touched
- **Dependency risk** — does it materially change current PR scope or depend on an OPEN PR?

If the user confirms with that knowledge, follow it. Obvious quality-gate violations (leaving a sanitizer error in place, splitting a TDD cycle, etc.) are **out of Q2 scope** — AGENTS.md's quality-gate rules win (see `AGENTS.md` §"副次的発見の判断優先順位").

→ **Q2 = Yes**: **follow the user's direction. Do not invoke `bug-forensics-analyst` or advisor** (no rule-side override of an explicit "fix now").
→ **Q2 = No**: proceed to Q3.

### Q3: Origin analysis via `bug-forensics-analyst`

Run only when Q1 / Q2 did not settle the case.

```
Agent tool: subagent_type='bug-forensics-analyst'
```

The agent emits (full spec in `.claude/agents/bug-forensics-analyst.md`):

- Origin verdict: regression (current PR introduced it) / pre-existing (PR only exposed it) / pre-existing exposed (PR triggers it conditionally)
- Impact surface (code paths touched)
- Test coverage gaps
- Fix-direction recommendation (does **not** write fix code)

> **Caller responsibility**: keep the agent's input context (PR diff / blame range / failing test output) visible in conversation history; same session ⇒ no explicit transfer needed.

### Q4: Three-way verdict

Using Q3's analysis and `/plan-rubric`'s PR-size discipline (1 issue ≒ 1 PR), pick one:

**(a) 即時修正** — fix diff is related to the current PR's scope and doesn't grow it materially; judged a regression; simple fix with low side-effect risk. → Land in the feature branch / current PR.

**(b) 別 issue 起票** — unrelated concern (e.g. parser bug surfaced alongside a codegen improvement); materially expands the current PR's scope; pre-existing and loosely related. → Proceed to "Issue Creation Steps" below.

**(c) ユーザー確認** — design choice is open (multiple fix approaches); regression vs pre-existing boundary is blurry; mid-sized so both (a) and (b) look reasonable. → Present **What / Where / Context / estimated size / recommended option (with reason)** and wait.

## Issue Creation Steps

Run only when Q4 = (b).

> **Important**: new issue creation (`gh issue create`) **requires explicit user permission** (AGENTS.md §責務の分離 "ユーザーが明示的に指示すること"). Step 2 previews the issue and waits for permission before Step 3 onward; do not run `gh issue create` without it.

### Step 1: Check OPEN-PR dependency; fold or escalate

This is a **fold-only** rule. "File now, work after the dependency PR merges" is not allowed — an orphan issue is indistinguishable from "open and actionable" in the backlog, weakens tracker signal, and hides the dependency.

1. **Identify the OPEN PR**:

```bash
gh pr list --state open --json number,title,headRefName
gh pr view <number> --json files,state,headRefName --jq '{state, branch: .headRefName, files: [.files[].path]}'
```

2. **Decide dependency**:
   - Code path only on the OPEN PR's feature branch (not on `main`) → **has dependency** (fold or escalate)
   - Code path on `main` / workable independently → **no dependency** (proceed to Step 2)

3. **Fold (propose scope expansion)**: ask the PR author to absorb the new finding via PR comment and obtain agreement — do not unilaterally push commits. If you are the author, add a commit to your own feature branch.

4. **Escalate (when fold isn't viable)**: present **What** / **Where** / **estimated size** to the user when any of: extra work materially changes PR scope (design rethink); added diff is comparable to or larger than the PR's existing diff; PR is review-complete / merge-ready and re-review cost is high.

The user then chooses: file an independent issue, expand the PR's scope, branch off, etc.

**Motivating example (#1692)**: #1692 was filed as a v2 extension of unmerged PR #1693 (`verifyCalledWith`), depending on `__ry_mock_store_arg` / kind tag enum / `mockArgEqual` / `__ry_mock_count_matching_calls` from there — an orphan until #1693 merged. With this gate, the same case routes to fold (extend #1693's scope) or escalate.

### Step 2: User-permission gate (preview the proposed issue)

Run only when Step 1 resolves to "no dependency" (fold / escalate cases finish in Step 1).

Present the following six items and wait for explicit creation permission ("起票して" / "OK" etc.). **Do not run `gh issue create` until permission is given.**

| Item | Content |
|---|---|
| **Reason for filing** | Why this finding doesn't belong in the current PR (separation of concerns / scope size / pre-existing / Q4(b) rationale) |
| **Summary** | 1-3 lines of gist (not full body) |
| **Granularity** | Fits in 1 PR? Will Step 4 split it? |
| **Confidence** | High (reproduction available) / Medium (hypothesis + verification path) / Low (hypothesis only), with reasoning |
| **Label suggestion** | Auto-pick (e.g. `bug` / `enhancement` / `documentation` / `refactor`). **At least one required; never empty.** |
| **Milestone candidate** | Run `gh api repos/t0k0sh1/ry/milestones?state=open` and propose the current dev-version milestone as a **candidate**. **Do not auto-inherit** — let the user choose adopt / override / unset. |

Example presentation:

```text
別 issue 起票の許可をお願いします:

- 起票理由: パーサ修正 (現 PR) と直交する codegen 側のエラーメッセージ改善で、関心事が異なる
- 概要: <module>.<fn> でエラー時の line/column が出ない (仮説あり、修正方針も見えている)
- 粒度: 1 PR で収まる規模 (推定 50〜100 行)
- 解決確度: High (ローカルで再現済み、修正パスも特定済み)
- ラベル案: bug, enhancement
- マイルストーン候補: v0.0.25 (現開発バージョン) / 未設定も可

起票してよろしいですか?
```

Once permission is granted (including the adopted milestone), feed the answer into Step 3 onward. If denied, exit without filing.

### Step 3: Check for duplicates

```bash
gh search issues --repo t0k0sh1/ry "<keywords>" --state open
```

If a duplicate exists, add the new context as a comment and, if needed, align milestones with `gh issue edit <number> --milestone "<title>"` (milestone change also after user confirmation). Skip Step 4 and proceed to Step 6.

### Step 4: Decide whether to split

If the finding has multiple independent concerns or the estimate clearly exceeds 1 PR, file separate issues. Target **1 issue ≒ 1 PR**.

Example: parser bug + codegen improvement → 2 issues; correctness fix and performance work on the same runtime function → 2 issues.

When splitting, the **classification of split rationale (feature boundary / dependency / size), symmetry check (typed↔any / wrap↔unwrap / read↔write / base↔derived), and 3rd-level-derivative guard** are in `/scope-decomposition` REQ-1〜3.

If splitting, present the split proposal (titles + granularity) to the user and obtain permission again (equivalent to Step 2).

### Step 5: Create the issue

Using the permission and milestone choice from Step 2, file via `/git-create-issue`. The command body and body-template live there.

**To avoid duplicate permission prompts**: `/git-create-issue` Step 1 is its own permission gate, but Step 2 above already obtained the equivalent approval, so `/git-create-issue` **skips its Step 1 and starts at Step 2 (duplicate check)** (same skip condition recorded there). The six approved items (reason / summary / granularity / confidence / label / adopted milestone) feed `gh issue create` as-is.

### Step 6: Report

Report the issue number, title, and configured milestone to the user. List all if multiple were filed. For fold / escalate, report that outcome and the relevant OPEN PR number.
