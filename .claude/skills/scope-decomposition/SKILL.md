---
name: scope-decomposition
description: Pre-verification rubric for splitting issues and filing derivatives. REQ-1 (symmetry, 4 axes) / REQ-2 (split rationale, 3 categories) / REQ-3 (derivation-chain guard) / REQ-5 (oversize → n-piece split single preview) apply at issue creation; REQ-4 (re-scan + target-shrinking split forbidden) applies in Plan mode.
allowed-tools: Read, Grep, Glob
---

# Scope Decomposition Rubric

A rubric to run before splitting an issue or filing a derivative. It guards against derivative-of-derivative chains (e.g. #1697 → #1797 → #1802) by enforcing two check points: at issue creation and in Plan mode.

> **Read-only skill.** No edits to issues, plans, or code. The caller writes the rubric's output into the issue body or Plan body.

---

## Why this skill exists

A derivative issue can spawn further derivatives, scattering one feature across multiple PRs and making "done" ambiguous. Concrete case from this repo (identified in #1804):

1. **#1697** (any holds collections) — original `any` extension. Mid-implementation, record support was deemed too large because SSA struct values cannot fit into `any.data[8]` → split off as **#1797**.
2. **#1797** (any holds records) — during Plan, the symmetry check between typed and `any` paths surfaced that cross-type unwrap (`let p: Parent = anyHoldingChild`) needs a struct field projection utility that codegen lacks → split off as **#1802**.
3. **#1802** (cross-type unwrap from any) — third-level derivative.

The third-level split (#1802) was discoverable when #1797 was filed but went unnoticed because subtype projection — an independent codegen concern — was not separated from the heap-boxing problem. This skill exists to catch that class of gap at filing time. The same case re-scanned through the rubric would have produced:

| Step | Applied REQ | What surfaces |
|---|---|---|
| Split #1697 → #1797 | REQ-2 | (a) feature boundary — heap-boxing is independent of SSA struct storage (✓ sound) |
| File #1797 | REQ-1 (base↔derived) | `any`-path subtype coercion needs a struct field projection utility that typed path has; declare this gap up front |
| File #1797 | (gap from REQ-1) | Add an "out of scope" section to the issue body, pre-announcing the future derivative |
| Split #1797 → #1802 | REQ-3 | #1697 → #1797 → #1802 is a 3rd-level chain → trigger the "consolidate ancestors" question |
| Split #1797 → #1802 | REQ-2 | (a) feature boundary holds — subtype projection is its own codegen problem |

Net effect: REQ-1 at #1797 filing would have turned #1802 from an "in-Plan discovery" into a pre-announced split.

---

## When to invoke

| Timing | Trigger | REQs to apply |
|---|---|---|
| **Issue creation** | After `/triage-side-finding` Q4(b) autonomously judges 起票許可を求める (new issue creation needed), before running `gh issue create`; or when deciding to split an existing issue; or when the user requests issue creation ("〜する issue を作って") | REQ-1, REQ-2, REQ-3, REQ-5 |
| **Plan mode** | After `EnterPlanMode`, when re-scanning the target issue's scope | REQ-4 |

---

## Issue creation checks (REQ-1, REQ-2, REQ-3)

Apply to both sides of any split: the new issue *and* the residual original.

### REQ-1: Symmetry check (4 axes)

Sweep the following four axes once:

| Axis | Question |
|---|---|
| **typed path ↔ `any` path** | Does what works on the typed path also work through `any`? |
| **wrap ↔ unwrap** | Will implementing only one direction leave an asymmetry? |
| **read ↔ write** | Will read-only or write-only coverage produce non-orthogonal behavior? |
| **base ↔ derived** | For subtype-bearing types (record `<` parent / enum variant / union), is base-only or derived-only coverage incomplete? |

**If a gap surfaces**: declare it up front in the issue body under an "Out of scope" section. Do not defer this to Plan-mode discovery.

### REQ-2: Split rationale classification

State the split rationale and assign one of:

| Class | Example | Verdict |
|---|---|---|
| (a) **Feature boundary** | SSA struct storage and subtype projection are independent codegen concerns | ✓ Sound |
| (b) **Dependency** | A needs B's codegen utility, B is unimplemented | ✓ Sound |
| (c) **Size** | "Too much to do in one PR" | ⚠ Warning |

When (c) is the answer, re-examine whether (a) or (b) actually backs it. Reclassify if a feature boundary or dependency is found; otherwise do not split (keep the larger PR or rethink the design). The rule is not a binary gate — it forces (c) to resolve depth-first into (a) or (b).

### REQ-3: Derivation chain guard (3rd level and beyond)

When filing a derivative-of-a-derivative, record the chain in the body:

```markdown
**Derived from**: #1697 → #1797 (this issue derives from #1797)
```

Before filing a **3rd-level or deeper** derivative, pause and consider whether the *ancestor chain should be consolidated* — closing prior derivatives and refiling a single issue with redrawn boundaries. A chain that keeps splitting often signals that the first split was not at a feature boundary. If consolidation is rejected, document which of REQ-2 (a) / (b) justifies the further split in the issue body.

---

## Issue creation oversize check (REQ-5)

### REQ-5: Oversize detection at issue creation/split time

新規 issue 作成時 (`/git-create-issue` Step 1 の preview 評価時) または既存 issue を分割する判断時に、**1 issue が大きすぎる場合の n 個分割を single preview で提案する**。ユーザーから「〜する issue を作って」と依頼された場合でも、適切な粒度に分割する根拠があれば split proposal を提示する (起票内容の提示であって 3 択以上の選択肢提示ではないため #1851 の MUST ルールに抵触しない)。

> **適用フェーズ**: Phase A (起票・分割の判断フェーズ) — Plan モード中の target-shrinking split は REQ-4 で禁止、本 REQ は **Plan モード開始前** (起票判断・分割判断のフェーズ) で動く。

#### 大きすぎ判定の閾値 (いずれか 1 つでも該当 ⇒ split 検討)

1. **規模**: 推定 diff (added + removed の raw line count) が **1000 行を超える**
2. **対称性 gap**: REQ-1 の 4 軸 sweep で **2 つ以上の独立な gap** が surface
3. **rationale の弱さ**: REQ-2 (a) feature boundary / (b) dependency のいずれにも該当せず、(c) size のみが split 根拠

これらは「split を検討するトリガー」であり、binary gate ではない。閾値該当でも feature boundary が引けず単一の連続作業になる場合は (REQ-2 (c) 解消失敗) split しない判断もありうる — その場合は理由を issue body / preview の reasoning に記録する。

#### Single preview の形式

split を提案する場合、**single preview (= 1 つの分割案を提示)** で行う。複数の分割パターンを並列で提示してはならない (起票判断時のユーザー判断負荷を最小化するため)。

提示形式:

```text
issue 起票前に分割を提案します:

- **トリガー**: <該当した閾値: 規模 / 対称性 gap / rationale 弱さ>
- **分割案** (n 個):
  - Issue A: <タイトル> — <1〜3 行 summary>
  - Issue B: <タイトル> — <1〜3 行 summary>
  - (...)
- **分割理由 (REQ-2 分類)**: <(a) feature boundary / (b) dependency のいずれかで justify>
- **REQ-3 連鎖警戒**: <derived-from がある場合は ancestor chain を明記>
- **依存順序**: <Issue A → Issue B の dependency があれば明示>

この分割で起票してよろしいですか?
```

ユーザーが decline / 修正を希望した場合は、(1) 別の分割パターンを **1 つだけ** 提示し直す、または (2)「分割せず 1 つの issue として起票」に倒す、のいずれか。複数の分割パターンを並列で再提示してはならない。

#### Trigger 経路

- `/git-create-issue` Step 1 の preview 評価時、「粒度 (Granularity)」項目で「分割が必要」と判断した場合 → 本 REQ に escalate
- 既存 issue を分割しようとする判断時 → 本 REQ で閾値 check
- `/triage-side-finding` Issue Creation Steps の Step 4 (Decide whether to split) で multi-concern 検出時 → 本 REQ で formalize

## Plan-mode checks (REQ-4)

### REQ-4: Re-scan procedure

In Plan mode, re-scan the target issue's scope before committing to an implementation.

> **MUST: 着手対象 issue の scope 縮小を伴う分割提案は禁止 (Plan モード中)**
>
> Plan モード中に**対象 issue 自体を分割して scope を縮小する**提案は禁止 (実装計画が狂うため、AGENTS.md §"Plan モードのルール" のユーザー方針)。「対象 issue が大きすぎるので 2 つに分けよう」「この部分は別 issue にして scope を絞ろう」等の **target-shrinking split** は Plan モードでは行わない。
>
> 判別: 「対象 issue の作業内容を狭めるか?」(YES → **禁止**) / 「対象 issue とは直交する独立な発見か?」(YES → orthogonal な Q4(b) 別 issue 起票として**許容**)。
>
> 大きすぎる issue を分割すべきという判断自体が必要な場合は、Plan モード**開始前** (起票・分割フェーズ = Phase A の前段) で `/scope-decomposition` REQ-5 を適用する。

**Re-scan の手順**:

1. **Rerun REQ-1's symmetry sweep.** Code may have shifted since filing, or the original sweep may have been shallow.
2. **If a gap surfaces:**
   - Add it to the Plan's "Out of scope" section before implementation begins (= 対象 issue の scope に含めないことを明示)。
   - **対象 issue を分割して gap を別 issue 化することは禁止** (上記 MUST ルール: target-shrinking split は Plan モードで不可)。
   - **orthogonal な発見**であれば `/triage-side-finding` Q4(b) 経由で別 issue 起票を提案できる (target issue の scope を変更しないため許容)。
   - 同 PR 内で対処できる小規模なものは `/triage-side-finding` Q4(a) 経由で fold in (即時修正、対象 issue の scope 内として実装計画に組み込む)。
3. **Re-check REQ-3.** If the target issue is itself a 3rd-level derivative, offer the user the option of consolidating ancestors before starting (consolidation は target を「広げ直す」操作であり target-shrinking split ではないため許容)。

---

## Related skills

- **`AGENTS.md` §"issue 起点の開発"** — entry point to this skill via the "issue 分割時のスコープ検証" bullet
- **`/plan-rubric`** Axis 2 (scope) — applies REQ-1〜3 / REQ-4 (re-sweep + target-shrinking split forbidden) after `/triage-side-finding` Q4(b)
- **`/triage-side-finding`** Q4(b) Issue Creation Steps Step 4 — links here for REQ-1〜3 + REQ-5 single-preview split format when judging a split
- **`/git-create-issue`** Step 1 粒度 (Granularity) item — escalates to REQ-5 when oversize thresholds are hit
- **`/git-claim-issue`** — `wip` labeling, orthogonal to this skill
- **`/knowledge-md-management`** — where to record any new knowledge surfaced by applying this rubric (path-scoped → `.claude/rules/`, cross-cutting → `.claude/skills/`)

**Notes**: The four REQ-1 axes enumerate ry-specific recurring cases; adding a new axis is a skill revision, not an ad-hoc per-issue decision. REQ-3's "consolidate ancestors" path has limited operational history — expect user judgment to be involved.
