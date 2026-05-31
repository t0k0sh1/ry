---
name: git-create-issue
description: Create a new GitHub issue, but ONLY after explicit user permission. Presents a 6-item preview (reason / summary / granularity / confidence / labels / milestone candidate) and waits for the user to approve before running `gh issue create`. Invoked from `/triage-side-finding` Step 2 when Q4(b) "別 issue 起票" is chosen, or directly when the user requests a new issue. Counterpart of `/git-claim-issue` (which only adds the `wip` label to an existing issue).
allowed-tools: Bash(gh issue create:*), Bash(gh issue view:*), Bash(gh issue list:*), Bash(gh search issues:*), Bash(gh api:*), Bash(gh pr view:*)
metadata:
  short-description: Create a new issue with explicit user permission gate
---

# Git Create Issue

Create a new GitHub issue **only after the user explicitly approves the proposed content**. Counterpart of `/git-claim-issue`, which adds the `wip` label to an existing issue.

## When to use

- After `/triage-side-finding` Step 1 (依存 PR 確認) clears "依存なし" and Q4 = (b) 別 issue 起票 was chosen
- When the user explicitly asks to file a new issue ("issue 立てて" / "起票して" / "create an issue for X")
- **NOT** for adding `wip` to an existing issue — use `/git-claim-issue` for that

## Repository

- owner: `t0k0sh1`
- repo: `ry`

## Critical safety rule

**`gh issue create` requires explicit user permission** (AGENTS.md §責務の分離「ユーザーが明示的に指示すること」). The presentation step (Step 1) is mandatory; Claude Code must not skip it and must wait for an explicit approval ("起票して" / "OK" / "go ahead" 等) before invoking `gh issue create`.

The previous failure mode (#1851 self-verification): Claude Code presented "別 issue 起票" as the leading option without consent, kept embedding it in subsequent suggestions, and only stopped when the user explicitly rejected it. This skill exists to make that path impossible: no `gh issue create` until the user says yes.

**Exception**: `/preparing-for-release` skill files Release prep / Release / Cleanup issues automatically; that flow is out of scope here because the user's `/preparing-for-release <X.Y.Z>` invocation itself counts as the permission.

## Steps

### Step 1: Present the proposal and wait for explicit permission

> **When invoked from `/triage-side-finding`**: Step 2 of that skill already presents the same 6-item proposal and obtains user approval. To avoid double-prompting, **skip this Step 1 and jump straight to Step 2 (duplicate check)**, carrying forward the approved reason / summary / granularity / confidence / labels / milestone from triage-side-finding Step 2. Re-prompting is required only if those parameters changed during Step 4 splitting (in which case triage-side-finding Step 4 itself re-asks).
>
> Run Step 1 only for **direct invocations** (the user typed `/git-create-issue` themselves, or another flow without a prior permission step).

Show the user the following 6 items as a single preview. Do **not** run `gh issue create` yet.

| Item | What to include |
|---|---|
| **起票理由 (Reason)** | Why this finding cannot be folded into the current PR (separation of concerns / scope size / pre-existing / Q4(b) rationale) |
| **概要 (Summary)** | 1〜3 line gist — not the full body, just the essence |
| **粒度 (Granularity)** | Whether it fits 1 issue ≒ 1 PR. **Apply `/scope-decomposition` REQ-5 thresholds** (estimated diff > 1000 lines / ≥ 2 REQ-1 symmetry gaps / REQ-2 (c) size-only rationale). If any threshold is hit, escalate to REQ-5 single-preview split before running this Step 1 preview |
| **解決確度 (Confidence)** | One of: **High** (再現手順あり) / **Medium** (仮説 + 検証手順あり) / **Low** (仮説のみ) — with a one-line justification |
| **ラベル案 (Labels)** | Auto-suggested label list (e.g. `bug` / `enhancement` / `documentation` / `refactor`). **At least one label is required — never propose an empty list.** Inspect existing repo labels via `gh label list --json name` if unsure |
| **マイルストーン候補 (Milestone)** | Fetched via the command below. Present the current development milestone as a *candidate*; the user decides whether to adopt it, pick a different one, or leave it unset. **Do not auto-inherit from the current PR.** |

To fetch the current open milestone:

```bash
gh api repos/t0k0sh1/ry/milestones?state=open --jq '.[] | {title, number, due_on}'
```

Pick the **active development milestone** — typically the nearest upcoming version — as the candidate to present, and **ask the user to confirm** rather than auto-deciding. The project's convention does not always match semver ordering: when multiple open milestones look plausible (e.g. both `v0.0.25` and `v0.1.0` are open), surface both to the user and let them say which is the current development cycle. When in doubt, present "未設定" as an equally valid choice.

#### Oversize escalation (`/scope-decomposition` REQ-5)

When the **粒度 (Granularity)** evaluation above flags any of:

- 推定 diff (added + removed の raw line count) が **1000 行を超える**
- REQ-1 4 軸 sweep で **2 つ以上の独立 gap** が surface
- REQ-2 (c) size-only rationale のみ ((a) feature boundary / (b) dependency に該当しない)

**escalate to `/scope-decomposition` REQ-5** first: 単一の **n 個分割 single preview** を提示してユーザーの承認 / 修正 / decline を待つ。分割が承認された場合は n 個の issue それぞれについて本 Step 1 (6 項目 preview) を順次実行する (各 issue の粒度は REQ-5 で reduced 済み)。分割が decline された場合は単一の oversized issue として本 Step 1 preview を続行し、起票理由に「REQ-2 (c) acknowledged: size split declined by user」と明記する。

> ユーザーが「〜する issue を作って」と明示依頼した場合でも、本 escalation は適用する — 分割提案は「ユーザー意図の拒否」ではなく「粒度提案」であり、同じユーザー許可 gate の枠内で行われる。

#### Presentation template

```text
別 issue 起票の許可をお願いします:

- 起票理由: <why this can't fold into the current PR>
- 概要: <1〜3 line gist>
- 粒度: <fits 1 PR? / needs splitting?>
- 解決確度: <High|Medium|Low> (<one-line justification>)
- ラベル案: <label1>, <label2>
- マイルストーン候補: <vX.Y.Z> (現開発バージョン) / 別指定 / 未設定 のいずれかを指定してください

起票してよろしいですか?
```

Wait for the user's reply. Acceptable approvals: 「起票して」 / 「OK」 / 「お願いします」 / "go ahead" / "yes" 等. If the user adjusts items (e.g. different milestone, additional label, refined title), incorporate the change and re-present a short confirmation before running Step 2. If the user declines or asks to defer, stop and do not file the issue.

### Step 2: Check for duplicates

```bash
gh search issues --repo t0k0sh1/ry "<keywords>" --state open
```

If a likely duplicate exists, do **not** create a new issue. Instead, surface the duplicate to the user (number + title), ask whether to add a comment to the existing one, and stop the create flow. Optionally use `gh issue edit <n> --milestone "<title>"` to align the existing issue's milestone *after a separate user confirmation*.

### Step 3: Run `gh issue create`

Only reach this step once Step 1 has explicit approval and Step 2 confirmed no duplicate.

```bash
gh issue create \
  --title "<明確で記述的なタイトル>" \
  --milestone "<milestone-title-from-Step-1>" \
  --label "<label1>" --label "<label2>" \
  --body "$(cat <<'EOF'
## Context

<!-- どのファイル / 関数 / コードパスが関与したか、どの PR / issue 作業中に発見したか -->

## Reproduction

<!-- 最小再現スニペットまたは手順 -->

## Expected vs Actual

**Expected:** <!-- 期待動作 -->
**Actual:** <!-- 実際動作 -->

## Discovery timing

<!-- いずれか: 実装中 / セルフ検証中 / PR レビュー対応中 -->
EOF
)"
```

- Omit `--milestone` if the user chose "未設定".
- Always pass at least one `--label`. `gh issue create --label foo` is safe (empty initial state); the destructive case applies only to `gh issue edit --label`.
- For non-bug items, you may drop the **Expected vs Actual** section. The other sections remain mandatory.

### Step 4: Report

Capture the new issue number from the URL printed by `gh issue create`. Report to the user:

- Issue number and title
- Applied labels
- Applied milestone (or "未設定")
- The full URL

If the user originally invoked this from `/triage-side-finding` Q4(b), also reaffirm that the side finding is now tracked and the original PR can proceed without it.

## Related skills

- `/triage-side-finding` — decides whether a side finding warrants a new issue (Q4(b)). Step 2 there delegates here.
- `/git-claim-issue` — adds `wip` to an *existing* issue. Use it as Task 1 of the implementation plan once the new issue is filed and the user wants to start work on it.
- `/scope-decomposition` — apply when Step 1 reveals the issue might need splitting (REQ-1〜3, REQ-5). REQ-5 triggers oversize → single-preview n-piece split before this Step 1 6-item preview.
- `/preparing-for-release` — files release-tracking issues automatically (exception to the permission gate, as noted above).
