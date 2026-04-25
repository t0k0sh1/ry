### Changed

- `release.yml` now deletes the matching `vX.Y.Z-nightly` prerelease (and its tag) after a stable `vX.Y.Z` release is published, preventing `ry self-update` from pinning users to a stale nightly that predates the stable release. (#1365)
