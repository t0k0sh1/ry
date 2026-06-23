## Goal

Push tag `v<X.Y.Z>` to trigger `release.yml`.

## Prerequisites

- Release prep issue #<P> is merged.
- Local `main` is up to date with `origin/main`.

## Tasks

1. Verify `CHANGELOG.md`: top dated section is `[<X.Y.Z>] - YYYY-MM-DD`; `[Unreleased]` above it is empty.
2. Verify no other open issues remain in milestone `v<X.Y.Z>` except this issue. If any remain, ask whether to defer to `v<NEXT>`, close, or abort.
3. Create and push the tag:
   ```bash
   git tag v<X.Y.Z>
   git push origin v<X.Y.Z>
   ```
4. Report the release workflow URL and stop. Leave this issue open with `wip`.

## Note

`release.yml` waits for a successful CodeQL `event=push` run for the tag SHA. Use `workflow_dispatch skip_codeql_gate=true` only for CodeQL outage.

## Out of scope

Editing `CHANGELOG.md`; closing release issue or milestone.
