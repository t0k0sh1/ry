### Fixed

- Installed `ry` binary no longer crashes with `dyld: Library not loaded` when using native packages (#659)
- Native shared libraries are now included in release and nightly distribution tarballs (#659)
- `self-update` now installs native shared libraries alongside the binary and stdlib (#659)
