---
paths:
  - "CHANGELOG.md"
---

# Changelog

### Do not edit `CHANGELOG.md` directly — add a `changelog.d/` fragment instead

**Source**: PR #1556
**Tags**: changelog, release, fragments, preparing-for-release, workflow

**Rule**: Do not edit `CHANGELOG.md` directly. User-visible changes (`feat:` / `fix:` / breaking changes) must be added as fragment files at `changelog.d/<issue>-<slug>.md`.

**Why**:

- `CHANGELOG.md` is generated at release time by the `/preparing-for-release` skill, which aggregates `changelog.d/*.md` fragments into the `[Unreleased]` section (`.claude/skills/preparing-for-release/SKILL.md`).
- Direct edits cause conflicts with the release aggregation step, or result in entries being duplicated alongside the fragment-derived text.
- Per-PR fragments keep the code change and the changelog wording review inside the same PR diff.

**How to apply**:

1. Create a new file at `changelog.d/<issue>-<slug>.md` (e.g. `changelog.d/1542-release-codeql-gate.md`).
2. Start the content with a Keep a Changelog heading: `### Added`, `### Changed`, `### Fixed`, or `### Removed`.
3. End each entry with the PR / issue reference, e.g. `(#1542)`.

For skip conditions and verification, see `/pre-commit-checklist`.

**Exception** — `CHANGELOG.md` may be edited directly only when performing the following tasks via the `/preparing-for-release` skill:

- Aggregating `changelog.d/*.md` fragments into the `[Unreleased]` section.
- Promoting `[Unreleased]` to a new `[X.Y.Z] - YYYY-MM-DD` section.
- Updating the comparison links at the bottom (`[Unreleased]` / `[X.Y.Z]` URL references).

Any other workflow (regular feature/fix PRs, etc.) must not edit `CHANGELOG.md`.
