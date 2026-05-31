---
name: scope-decomposition
description: Pre-verification rubric for splitting issues and filing derivatives. REQ-1 (symmetry, 4 axes) / REQ-2 (split rationale, 3 categories) / REQ-3 (derivation-chain guard) apply at issue creation; REQ-4 (re-scan) applies in Plan mode.
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
| **Issue creation** | After `/triage-side-finding` Q4(b) autonomously judges 起票許可を求める (new issue creation needed), before running `gh issue create`; or when deciding to split an existing issue | REQ-1, REQ-2, REQ-3 |
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

## Plan-mode checks (REQ-4)

### REQ-4: Re-scan procedure

In Plan mode, re-scan the target issue's scope before committing to an implementation.

1. **Rerun REQ-1's symmetry sweep.** Code may have shifted since filing, or the original sweep may have been shallow.
2. **If a gap surfaces:**
   - Add it to the Plan's "Out of scope" section before implementation begins.
   - If filing a separate issue, justify under REQ-2 (a) / (b) in the Plan body.
   - If the gap is small enough to handle in the same PR, fold it in via `/triage-side-finding` Q4(a) (immediate fix).
3. **Re-check REQ-3.** If the target issue is itself a 3rd-level derivative, offer the user the option of consolidating ancestors before starting.

---

## Related skills

- **`AGENTS.md` §"issue 起点の開発"** — entry point to this skill via the "issue 分割時のスコープ検証" bullet
- **`/plan-rubric`** Axis 2 (scope) — applies REQ-1〜3 after `/triage-side-finding` Q4(b)
- **`/triage-side-finding`** Q4(b) Step 4 — links here when judging a split
- **`/git-claim-issue`** — `wip` labeling, orthogonal to this skill
- **`/knowledge-md-management`** — where to record any new knowledge surfaced by applying this rubric (path-scoped → `.claude/rules/`, cross-cutting → `.claude/skills/`)

**Notes**: The four REQ-1 axes enumerate ry-specific recurring cases; adding a new axis is a skill revision, not an ad-hoc per-issue decision. REQ-3's "consolidate ancestors" path has limited operational history — expect user judgment to be involved.
