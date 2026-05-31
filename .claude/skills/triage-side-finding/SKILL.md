---
name: triage-side-finding
description: Triage hub for side findings — short-circuits via Q1 (hard-to-reproduce CI detection) / Q2 (explicit user direction) / Q3 (bug-forensics-analyst) / Q4 (Claude Code 自律判断、フェーズ別 — Phase A 起票/分割/Plan モード時は即時修正/起票許可の 2 分岐、Phase B 実装中/レビュー対応中は同 PR 吸収 default + クラッシュ系無条件 + 非クラッシュは 1000 行閾値で Q2 再ルート). Use during implementation, self-verification, or PR review when an out-of-scope finding surfaces. Origin diagnosis (regression vs pre-existing) is delegated to `bug-forensics-analyst` and reached only via Q3. Also fires on Japanese triggers 副次的な発見, side finding, scope, ついでに直したい, 別 issue 起票, 即時修正, OPEN PR 依存, fold-only, orphan issue 防止, triage, 判定フロー, 起票許可, 自律判断, フェーズ別, 実装中スコープ外, 1000 行閾値, クラッシュ系.
allowed-tools: Bash(gh issue:*), Bash(gh search:*), Bash(gh pr:*), Bash(gh api:*), Agent
---

# Triage Side Finding

Neutral triage hub for side findings detected during implementation, self-verification, or PR review. Q4 では Claude Code が自律的に 2 分岐 — (a) 即時修正 / (b) 起票許可を求める — のいずれかを判定する (旧 3 択 [即時修正 / 別 issue 起票 / ユーザー確認] は廃止)。

> **Source-of-truth note**: previously in `AGENTS.md` §"責務の分離 > スコープ外の問題を発見した場合の対応ルール"; relocated by #1384. The "OPEN PR dependency" gate (Q4(b) Step 1 + fold-only rule) was added by #1694. The Q1-Q4 short-circuit redesign and `bug-forensics-analyst` integration came in #1752. Q4 の自律判断 2 分岐化 (旧 3 択廃止) は #1981 で導入 — AGENTS.md §"起票判断における選択肢提示の禁止" の MUST ルールに準拠。

## Design intent

The skill must **not** bias side findings toward "file a separate issue." The previous `scope-out-issue` skill did, producing two failure modes (#1752): (a) hard-to-reproduce CI findings (ASan / TSan / UBSan / libFuzzer crashes) went stale before triage finished; (b) when the user said "fix it now," rule discipline overrode the call.

加えて #1981 で、Q4 の判定を「ユーザーへの選択肢提示」に丸投げする旧 3 択 (即時修正 / 別 issue 起票 / ユーザー確認) も廃止した。起票要否は Claude Code が自律判断すべき責務であり、ユーザー選択肢に「別 issue に起票する」を含めることは AGENTS.md §"起票判断における選択肢提示の禁止" の MUST ルールで禁止されている。「禁止対象 = 旧 `scope-out-issue` 型の常時起票バイアス + 選択肢としてのユーザー提示」「許容 = Claude Code 自律判断後の `/git-create-issue` Step 1 経由 1 択 preview による許可要求 (選択肢ではない)」と区別する。

Q1 / Q2 short-circuit to immediate fix without invoking any agent or advisor, preserving the reproduction window. Q3 / Q4 run only when Q1 / Q2 did not settle the case.

## Decision Flow

Evaluate in order. **Once a stage settles, do not run later stages** (no `bug-forensics-analyst` or advisor invocation).

### Q1: Reproducing now, but hard to reproduce later?

Covers CI-only sanitizer hits (ASan / TSan / UBSan), libFuzzer crashes, concurrency races, and probabilistic memory corruption — observable right now but with non-deterministic local reproducibility because the timing / environment trigger conditions are not yet identified.

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

### Q4: Claude Code 自律判断 (フェーズ別)

**MUST (#1981)**: ここで 3 択以上の選択肢 (旧 [即時修正 / 別 issue 起票 / ユーザー確認]) をユーザーに提示してはならない。起票要否は Claude Code が自律判断する。詳細は AGENTS.md §"起票判断における選択肢提示の禁止" を参照。

#### Phase determination (Q4 判定の前提)

トリアージの発生フェーズによって Q4 の挙動を切り替える:

| Phase | 発生条件 | Q4 default | 自律的に Q4(b) 起票許可を起動するか |
|---|---|---|---|
| **A: 起票・分割・Plan モード中** | issue 起票時 / 既存 issue の分割判断時 / Plan モードでの scope 検討中 | (a)/(b) 自律判断 | あり (orthogonal な発見は (b) へ) |
| **B: 実装中・レビュー対応中** | コード編集中 / セルフ検証中 / PR レビュー対応中 | 同 PR 吸収を default | **なし** (Claude Code は自律的に (b) を選ばない、Q2 再ルートに倒す) |

**Why Phase B suppresses Q4(b)**: 着手中の issue は実装計画に沿って進めるため、separate issue 起票で実装計画を中断したくない (AGENTS.md §"副次的発見への対応" のフェーズ別ルール、ユーザー方針)。「実装中の発見は基本同 PR で対処、大規模ならユーザー判断」が default。

#### Phase A: 起票・分割・Plan モードでの判定

Q3 の analysis (origin verdict / impact surface / coverage gap / fix-direction) を input とし、`/plan-rubric` の PR-size discipline (1 issue ≒ 1 PR) を踏まえ、以下の判断基準で **Claude Code が自律的に 2 分岐のいずれかを選ぶ**:

**判断基準** — 4 軸で評価する:
1. **PR スコープ関連性**: 現 PR の scope (issue 本体の目的) と直接関係するか
2. **拡大度**: 修正を含めると PR の diff サイズ・review コストが材料的に膨張するか
3. **Origin verdict** (Q3 出力): regression (現 PR が混入) / pre-existing exposed (現 PR が露出させた) / pure pre-existing (現 PR と独立)
4. **サイドエフェクトリスク**: 修正が現 PR の他の変更に副作用を持つ可能性

##### 分岐 (a) 即時修正 (Phase A)

**条件 (全て満たす場合)**: PR スコープと関連 + 非拡大 (diff 増加が現 PR と同オーダー以下) + regression または pre-existing exposed + 低リスク (狭い影響範囲、確立されたパターン)。

→ feature branch / 現 PR にコミットして対処。`/plan-rubric` の no-incidental-fix rule は「現 PR と無関係な cleanup」を禁止する規則であり、本分岐は「Q1-Q4 トリアージを経て同 PR で対処すべきと自律判断したもの」のため衝突しない。

##### 分岐 (b) 起票許可を求める (Phase A)

**条件**: (a) の条件を 1 つ以上満たさない場合は、自律的に「起票許可を求める」分岐に倒し、**Issue Creation Steps** に進む。

**曖昧ケースの扱い** (旧 Q4(c) 該当): 設計選択肢が複数ある / regression vs pre-existing の境界が曖昧 / mid-sized で (a)/(b) 両方が成立しうる、といった不確実性は **preview 6 項目の Confidence: Medium/Low と理由付き** で表現する (Step 2 の preview gate でユーザーが判断材料を得られる)。ユーザーに 3 択を提示して判断を委ねてはならない。

**decline 時のフォールバック (Q2 への再ルート)**: ユーザーが Step 2 の preview を decline した場合は、**Q2 (informed-consent gate) への再ルート**として扱う:

1. 「現 PR で吸収可能か」を Claude Code が再評価し、`What / Where / 推定サイズ / 推奨アクション (理由付き)` を **1 つの推奨案として提示** してユーザー指示を仰ぐ (複数選択肢の列挙はしない)
2. ユーザーが「現 PR で対処」を選べば Q2 = Yes として実装、「対処しない」を選べば「発見を記録、現 PR では未対処」を 1 行報告して終了する
3. 再度メニュー (3 択以上) を提示してはならない (MUST ルール遵守)

> **用語注意**: ここでの「現 PR で吸収」は Step 1 escalate 節の `fold` (OPEN PR 依存時に PR author に scope 拡張を依頼) とは別概念。混同を避けるため本節では `fold` 流用ではなく「Q2 への再ルート」と表現する。

#### Phase B: 実装中・レビュー対応中の判定

Phase B では separate issue 起票を Claude Code が自律的に選ぶことはしない。代わりに以下の閾値で「同 PR 吸収」と「Q2 再ルート (ユーザー判断)」を判定する。

##### クラッシュ系の定義

以下のいずれかに該当する発見:
- ASan / UBSan / TSan / libFuzzer が検出
- `abort()` / SEGV / use-after-free (UAF) / memory leak / memory corruption の経路

> **安全側の境界**: assertion failure / data-corrupting race / infinite loop は本カテゴリ**外** (= 非クラッシュ系として扱う)。これらは sanitizer 経路ではないため別判定にしないと「small but critical」が漏れる可能性があるが、user 方針 (1K 行で判断委譲) との整合を優先する。判定が曖昧な場合は非クラッシュ系として扱い、サイズが小さければ Q4(a) 即時修正、大きければ Q2 再ルートに進む。

##### 規模見積もりの方法

estimated diff = **追加行 + 削除行の合計**を、検出時点で見積もる。raw line count (空行・コメント含む) を使う (検出時点で正確な分析コストを払わないため)。

##### 判定フロー (Phase B)

| 条件 | 判定 |
|---|---|
| クラッシュ系 (規模問わず) | **Q4(a) 即時修正** — 同 PR 内で対処 |
| 非クラッシュ系 かつ ≤ 1000 行 | **Q4(a) 即時修正** — 影響範囲が限定的 |
| 非クラッシュ系 かつ > 1000 行 | **Q2 (informed-consent gate) への再ルート** — ユーザー判断 |

##### Q2 再ルートの提示形式 (Phase B, 非クラッシュ > 1000 行)

ユーザーに以下を 1 メッセージで提示し、対応方針の指示を仰ぐ:

- **What** — 発見した問題 (file / function / 動作)
- **Where** — 影響範囲 (modules / tests / dependencies)
- **推定 diff サイズ** — added + removed 見積値 (1000 行超の根拠)
- **対応方法の選択肢** — 複数の対応アプローチがある場合は提示する。**「別 issue 起票」は選択肢に含めない** (AGENTS.md §"起票判断における選択肢提示の禁止" の MUST ルール、および Phase B の方針)。典型例: 「現 PR scope を拡張して対応」「修正範囲を最小化して対処」「発見を記録、現 PR では未対処」等
- **推奨アクション** — 1 つに絞って提示 (理由付き)

ユーザーが decline / 別案を提示した場合は再評価して 1 つ提示し直す。再度 3 択以上のメニューを提示してはならない (MUST ルール遵守)。

##### 実装中の閾値跨ぎ (≤ 1000 → > 1000)

検出時に「≤ 1000 行」と見積もって Q4(a) で開始した修正が、実装途中で diff が 1000 行を超えそうだと判明した場合は、その時点で実装を一旦停止し、**Q2 再ルート**にエスカレートする (上記提示形式に従う)。現時点の diff / 残作業見積 / 推奨アクションを提示してユーザー判断を仰ぐ。

> **Phase B での (b) 起票許可分岐は不採用**: Phase B で「同 PR で対処すべきでないほど大規模/直交な発見」が浮上した場合も、Claude Code は自律的に Q4(b) を選択せず、Q2 再ルートに倒してユーザー判断に委ねる。ユーザーが明示的に「別 issue にすべき」と判断した場合は Q2 = Yes 経由で `/git-create-issue` を起動する (ユーザー起点なので #1851 の MUST ルールには抵触しない)。

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

4. **Escalate (when fold isn't viable)**: present **What** / **Where** / **estimated size** / **推奨アクション (理由付き、1 つに絞る)** to the user when any of: extra work materially changes PR scope (design rethink); added diff is comparable to or larger than the PR's existing diff; PR is review-complete / merge-ready and re-review cost is high.

Claude Code は PR スコープ判定 (Q4 判断基準と同じ 4 軸) に基づき、最も妥当な対処方針を 1 つ推奨してユーザーの許可を待つ。**3 択以上の選択肢列挙はしない** (AGENTS.md §"起票判断における選択肢提示の禁止" の MUST ルール)。Claude Code 内部での候補としては典型的に「独立 issue 起票」が選ばれるが、状況により「現 PR scope 拡張」「別ブランチで対応」等もあり得る — ただしこれらは内部判断のための列挙であり、ユーザー提示時は 1 つに絞る。ユーザーが推奨を decline した場合のみ、別の選択肢を再評価して 1 つだけ提示し直す。

**Motivating example (#1692)**: #1692 was filed as a v2 extension of unmerged PR #1693 (`verifyCalledWith`), depending on `__ry_mock_store_arg` / kind tag enum / `mockArgEqual` / `__ry_mock_count_matching_calls` from there — an orphan until #1693 merged. With this gate, the same case routes to fold (extend #1693's scope) or escalate.

### Step 2: User-permission gate (preview the proposed issue)

Run only when Step 1 resolves to "no dependency" (fold / escalate cases finish in Step 1).

Present the following six items and wait for explicit creation permission ("起票して" / "OK" etc.). **Do not run `gh issue create` until permission is given.**

| Item | Content |
|---|---|
| **Reason for filing** | Why this finding doesn't belong in the current PR (separation of concerns / scope size / pre-existing / Q4(b) rationale) |
| **Summary** | 1-3 lines of gist (not full body) |
| **Granularity** | Fits in 1 PR? **Apply `/scope-decomposition` REQ-5 thresholds** (estimated diff > 1000 lines / ≥ 2 REQ-1 symmetry gaps / REQ-2 (c) size-only rationale). If any threshold is hit, escalate to REQ-5 single-preview split in Step 4 before continuing this Step 2 preview. |
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

When splitting, apply `/scope-decomposition`:
- **REQ-1** (4-axis symmetry: typed↔any / wrap↔unwrap / read↔write / base↔derived)
- **REQ-2** (3 split-rationale categories: feature boundary / dependency / size)
- **REQ-3** (3rd-level-derivative chain guard)
- **REQ-5** (oversize threshold + **single-preview n-piece split format**: present 1 split plan with titles / 1〜3 line summaries / REQ-2 justification / dependency order; no multi-pattern enumeration)

Present the REQ-5 single-preview split proposal to the user and obtain permission. If approved, file each piece via `/git-create-issue` (Step 1 skipped per its skip condition — Step 2 of this skill already approved per-issue parameters in the split preview).

### Step 5: Create the issue

Using the permission and milestone choice from Step 2, file via `/git-create-issue`. The command body and body-template live there.

**To avoid duplicate permission prompts**: `/git-create-issue` Step 1 is its own permission gate, but Step 2 above already obtained the equivalent approval, so `/git-create-issue` **skips its Step 1 and starts at Step 2 (duplicate check)** (same skip condition recorded there). The six approved items (reason / summary / granularity / confidence / label / adopted milestone) feed `gh issue create` as-is.

### Step 6: Report

Report the issue number, title, and configured milestone to the user. List all if multiple were filed. For fold / escalate, report that outcome and the relevant OPEN PR number.
