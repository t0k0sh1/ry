---
name: knowledge-md-management
description: Operate KNOWLEDGE.md, the staging buffer for unclassified lessons — where to write, entry format, grep convention, external-reference policy, and promotion to rules / skills. Use when capturing a lesson, looking knowledge up via grep, or splitting a stabilized entry out of KNOWLEDGE.md. Also fires on Japanese triggers ナレッジ追加, 教訓を残す, 経験を記録, 昇格, 切り出し, 肥大化整理, KNOWLEDGE.md に書く, KNOWLEDGE.md を参照.
allowed-tools: Bash
---

# Knowledge MD Management

`KNOWLEDGE.md` (repo root) is the staging buffer for lessons with no matching entry in `.claude/rules/` or `.claude/skills/` yet. Once an entry stabilizes, promote it to a rule or skill.

## When to use

- A new lesson, non-obvious fact, abandoned design, or command-recovery note to record
- You need to grep across the whole knowledge base (KNOWLEDGE.md isn't covered by path-scoped auto-load)
- A stabilized entry is ready to be split out into `.claude/rules/` or `.claude/skills/`

### Concrete write triggers

ナレッジを書くタイミングは典型的に以下の 4 つに集約される。これらに該当した時点で、§1 のルーティング (既存 entry を更新 vs KNOWLEDGE.md へ追記) を判断する。

| トリガー | 何を書くか |
|---|---|
| **PR レビュー対応後** | 他 PR にも再発しうるレビュー指摘 (path-scope に収まれば対応 rule、横断的なら `.claude/skills/pr-review-recurring-patterns/SKILL.md`)。単発の local 指摘は不要 |
| **実装中** | 非自明な事実 (型システムの落とし穴、ライブラリの裏仕様、再現条件付きバグ等)。「次に誰かが同じ問題に当たったとき即座に解けるか」を基準に判断 |
| **Plan 中** | 採用しなかった設計判断 (なぜ別案を選ばなかったか)。将来同じ alternative を再検討する時の判断材料 |
| **コマンドミスのリカバリ時** | `commands-environment-gotchas/SKILL.md` の `Wrong → Correct → Why` triple。プレーン typo は除外、second invocation で初めて気付いた非自明なものが対象 |

## 1. Where to write (REQ-1)

1. **A matching entry exists** in a rule / skill → append there. Do not touch KNOWLEDGE.md.
2. **No matching entry anywhere** → append to KNOWLEDGE.md as a temporary buffer.
3. **Entry has stabilized or coalesced into a clear theme** → follow §4 to promote.

Use §2's grep to check matching. When uncertain, default to KNOWLEDGE.md — promotion is easier than retraction.

## 2. Entry format & how to read (REQ-1, REQ-2)

### Format

Every entry follows the same shape as rule / skill entries (for grep consistency):

```markdown
### <short, specific heading>

**Source**: <PR / issue / commit>
**Tags**: <space-separated keywords>
**Rule**: <body — what to do or avoid, 1-3 paragraphs>
```

The `**Tags**:` line is mandatory — without it the entry is invisible to the grep convention.

### How to read

Grep all sources by tag:

```bash
grep -rnE '\*\*Tags\*\*:.*<keyword>' .claude/rules/ .claude/skills/ KNOWLEDGE.md
```

KNOWLEDGE.md has no path-scoped auto-load (no frontmatter `paths:` glob), so this grep must be run **explicitly** — it isn't triggered by file edits.

## 3. External-reference policy (REQ-3)

Do **not** reference individual KNOWLEDGE.md entries from AGENTS.md, `.claude/rules/<*>.md`, `.claude/skills/<*>/SKILL.md`, or `.claude/agents/<*>.md`. Any KNOWLEDGE.md edit (add / remove / reorder / promote) trivially dangles such references. Precedent: #1550 cleaned up six dangling lines under `.claude/rules/`.

### Forbidden reference patterns

Any reference combining `KNOWLEDGE` or `KNOWLEDGE.md` with one of:

| Pattern | Shape | Why it breaks |
|---|---|---|
| Line number | Capital `L` + integer | Line numbers shift on add / remove |
| Approximate line | Word `line` + number | Same |
| Positional | Word `entry` + direction (above / below / here) | Reorder flips the meaning |
| Heading name | Quoting a `### ...` heading from KNOWLEDGE.md | Breaks on entry move / rename |

### Allowed references

| Pattern | Example | Why OK |
|---|---|---|
| Meta-reference to the skill | ``KNOWLEDGE.md operations live at `/knowledge-md-management` `` | Points at the skill, not an entry |
| Mention as a whole | The "knowledge base" bullet in AGENTS.md | Treats KNOWLEDGE.md as a collection |

Generic mentions ("knowledge accumulates in KNOWLEDGE.md") are fine; pointing at a specific entry is not.

### Self-check

Before publishing new knowledge-base docs, verify:

```bash
grep -nE 'KNOWLEDGE(\.md)?\s*(L[0-9]+|line\s+|entry\s+(above|below|here))' \
  AGENTS.md .claude/rules/*.md .claude/skills/*/SKILL.md .claude/agents/*.md
```

Every hit is a violation to fix.

## 4. Promotion to rules / skills (REQ-4)

### When to promote

- **Per-entry** (primary): a single entry has stabilized and its permanent home — a rule or a skill — is clear → promote immediately
- **Bulk** (secondary, periodic): KNOWLEDGE.md exceeds **10 entries** or **400 lines** → review all entries for promotion candidates

Thresholds are defaults; adjust as needed.

### Choosing destination: rule vs skill

| Condition | Destination |
|---|---|
| Scopable by a frontmatter `paths:` glob (tied to specific paths / implementations) | `.claude/rules/<name>.md` |
| Procedure / intent / cross-cutting policy (path-independent or multi-path) | `.claude/skills/<name>/SKILL.md` |
| Meta-index of recurring PR-review themes | `.claude/skills/pr-review-recurring-patterns/SKILL.md` |
| Command / env-var / shell-syntax recovery | `.claude/skills/commands-environment-gotchas/SKILL.md` |

### Cleanup on promotion

- **Fully delete** the original entry from KNOWLEDGE.md. No pointer stub (e.g. "moved to ~.md").
- Stubs themselves become stale: if the destination entry is later renamed or removed, the stub is hard to find — same failure mode as §3.
- Record the promotion only in the PR description / commit message.

### Typical promotion PR shape

1. Remove the entry from KNOWLEDGE.md
2. Add it to the destination file (match neighboring entries' style)
3. Note "KNOWLEDGE.md → `<destination>` promotion" in the PR description
4. Run `/pre-commit-checklist` for self-verification
