### Changed

- Release workflow now triggers on tag push (`v*.*.*`) instead of `workflow_dispatch` only. Pushing a semver tag from `main` builds, tests, and publishes a GitHub Release in one shot. (#1369)

### Removed

- `VERSION` file removed. CI derives the version from `${GITHUB_REF_NAME#v}`; local builds default to `0.0.0`. (#1369)
