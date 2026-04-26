---
name: pr-review-recurring-patterns
description: Meta-index of recurring PR reviewer comments and the .claude/rules/ entries they point to. Use when triaging a review comment that matches a known pattern — jump to the linked rule instead of re-diagnosing from scratch.
allowed-tools: Bash
---

# PR Review Recurring Patterns

Meta-entries linking recurring reviewer comments to the specific rules in `.claude/rules/`. Useful for `grep` when triaging a new review comment — if a reviewer's comment matches one of these patterns, jump to the linked rule.

> **Format exception**: entries in this section are pointers into other sections, not standalone lessons, so they intentionally use a lighter structure (`Tags` + `Seen in` + `Points to`) instead of the full `Source` / `Tags` / `Context` / `Rule` layout. Each entry still has a `**Tags**:` line so the tag-search convention works uniformly.

---

### "Add a regression test for the new rejection path" (recurring)

**Tags**: meta-index, testing, rejection-path
**Seen in**: #841 (CodeRabbit)
**Points to**: [Testing → Every new rejection branch needs a test](../../rules/tests-rejection-tdd.md)

---

### "Handle the collapsed case from the new canonicalizer" (recurring)

**Tags**: meta-index, codegen, canonicalization
**Seen in**: #844 (CodeRabbit, critical)
**Points to**: [Codegen → Canonicalization that may collapse shape must be handled at all call sites](../../rules/codegen-type-and-metadata.md)

---

### "New primitive not wired into type inference / generics" (recurring)

**Tags**: meta-index, codegen, primitive-type, type-reflection
**Seen in**: #825 (CodeRabbit, 4 comments)
**Points to**: [Codegen → New primitive types must be wired into every type-reflection site](../../rules/codegen-type-and-metadata.md)
