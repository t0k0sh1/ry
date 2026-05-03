---
paths:
  - "CHANGELOG.md"
---

# Do not edit CHANGELOG.md directly

Direct edits to `CHANGELOG.md` are forbidden. User-visible changes (`feat:` / `fix:` / breaking changes) must be added as fragment files under `changelog.d/<issue>-<slug>.md`.

## Why

- `CHANGELOG.md` is generated at release time by the `/preparing-for-release` skill, which aggregates `changelog.d/*.md` fragments into the `[Unreleased]` section (`.claude/skills/preparing-for-release/SKILL.md`).
- Direct edits cause conflicts with the release aggregation step, or result in entries being duplicated alongside the fragment-derived text.
- Per-PR fragments keep the code change and the changelog wording review inside the same PR diff.

## What to do instead

1. Create a new file at `changelog.d/<issue-number>-<slug>.md` (e.g. `changelog.d/1542-release-codeql-gate.md`).
2. Start the content with a Keep a Changelog heading: `### Added`, `### Changed`, `### Fixed`, or `### Removed`.
3. End each entry with the PR / issue reference, e.g. `(#1542)`.

For the full format, skip conditions, and how this fits into the pre-commit checklist, see `.claude/skills/pre-commit-checklist/SKILL.md` §4.

## Exception (the only case where editing CHANGELOG.md directly is allowed)

`CHANGELOG.md` may be edited directly only when performing the following tasks via the `/preparing-for-release` skill:

- Aggregating `changelog.d/*.md` fragments into the `[Unreleased]` section.
- Promoting `[Unreleased]` to a new `[X.Y.Z] - YYYY-MM-DD` section.
- Updating the comparison links at the bottom (`[Unreleased]` / `[X.Y.Z]` URL references).

Any other workflow (regular feature/fix PRs, etc.) must not edit `CHANGELOG.md`.
