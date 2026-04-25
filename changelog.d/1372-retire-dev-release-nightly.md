### Removed

- `ry self-update --nightly` flag and the implicit nightly default (when the running version had a prerelease suffix, `self-update` with no arguments previously targeted the latest prerelease). `self-update` now always targets the latest stable release unless an explicit version tag is given. The nightly build workflow (`dev-release.yml`) has been retired as part of this change. (#1372)
