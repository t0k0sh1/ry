## Goal

Verify release artifacts and close the milestone.

## Prerequisites

- Tag `v<X.Y.Z>` has been pushed.
- `release.yml` has finished.
- Release issue #<R> is still open with `wip`.

## Tasks

1. Verify the `release.yml` run for `v<X.Y.Z>` completed successfully.
2. Verify GitHub Release `v<X.Y.Z>` exists, is not draft, and is not prerelease.
3. Remove `wip` from release issue #<R>, then close it.
4. Confirm milestone `v<X.Y.Z>` has no open issues except `<this-issue>`, then close the milestone.
5. Remove `wip` from `<this-issue>`, then close it.

## Out of scope

Creating the next milestone or bumping docs/version files.
