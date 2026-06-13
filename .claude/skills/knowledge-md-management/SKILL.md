---
name: knowledge-md-management
description: Route, record, search, and promote project lessons across KNOWLEDGE.md, rules, and skills.
allowed-tools: Bash
---

# Knowledge Management

## Routing

| Finding | Destination |
|---|---|
| Matches an existing entry | Update that entry |
| Path-specific implementation constraint | `.claude/rules/<name>.md` |
| Procedure or cross-cutting policy | `.claude/skills/<name>/SKILL.md` |
| Command or environment recovery | Matching task skill or path-scoped rule; otherwise `KNOWLEDGE.md` |
| Recurring cross-path review feedback | Matching skill, or `KNOWLEDGE.md` when uncategorized |
| Sanitizer / fuzzer runtime incident | `KNOWLEDGE.md` section `## サニタイザー既知問題` |
| No clear destination | `KNOWLEDGE.md` |

## Search

```bash
rg '\*\*Tags\*\*:.*<keyword>' .claude/rules .claude/skills KNOWLEDGE.md
```

Search before adding an entry.

## Entry Format

```markdown
### Short specific heading

**Source**: <issue / PR / commit>
**Tags**: <space-separated keywords>
**Rule**: <required action or durable finding>
```

- Keep entries concise.
- Include only context required to apply the rule.
- Under `## サニタイザー既知問題`, use `### <Sanitizer>` and `#### <Entry>`.

## References

- Do not reference individual `KNOWLEDGE.md` entries from other instruction files.
- References to `KNOWLEDGE.md` as a whole or to this skill are allowed.

## Promotion

Promote an entry when its permanent destination is clear.

1. Add it to the matching rule or skill.
2. Remove the original `KNOWLEDGE.md` entry.
3. Do not leave a pointer stub.
4. Run `/pre-commit-checklist`.

Review promotion candidates when `KNOWLEDGE.md` exceeds 400 lines or 10 entries.
