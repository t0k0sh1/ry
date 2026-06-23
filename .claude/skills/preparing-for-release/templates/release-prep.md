## Goal

Aggregate `changelog.d/` into `CHANGELOG.md` and finalize `[<X.Y.Z>] - YYYY-MM-DD`.

## Tasks

1. Run `scripts/assemble-changelog.sh`.
2. Verify `share/std/manifest.json` matches `find share/std -name '*.ry'`; fix drift in the same PR.
3. Rename `[Unreleased]` to `[<X.Y.Z>] - YYYY-MM-DD` using today's UTC date.
4. Insert a fresh empty `[Unreleased]` above it.
5. Update comparison links:
   - `[Unreleased]`: `v<X.Y.Z>...HEAD`
   - `[<X.Y.Z>]`: `v<PREV>...v<X.Y.Z>`
6. Check `.github/workflows/release.yml` uses the latest `ry-ci-glibc-old:llvm-<MAJOR>-rev<N>` tag; if bumped, add a changelog fragment before assembling.

## Verification

- `CHANGELOG.md` has empty `[Unreleased]`, finalized `[<X.Y.Z>]`, and updated links.
- `changelog.d/` assembled fragments are gone.
- Applicable checks pass.

## Out of scope

Pushing tag `v<X.Y.Z>`.
