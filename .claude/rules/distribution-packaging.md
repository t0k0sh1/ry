---
paths:
  - "scripts/bundle-dist.sh"
  - "scripts/verify-bundle.sh"
  - ".github/workflows/release.yml"
---

# Distribution Packaging

- Release binaries must bundle shared libLLVM and rewrite rpaths in `scripts/bundle-dist.sh`; `release.yml` copies `build/ry` directly.
- macOS install names need explicit `install_name_tool -change` rewrites and re-signing after mutation.
- Linux shipped binaries should use `BUILD_WITH_INSTALL_RPATH ON` with `INSTALL_RPATH`; do not rely on patchelf for the main rpath.
- Keep both unpacked and installed layouts covered: `@loader_path/lib` / `$ORIGIN/lib` and `@loader_path/../../.ry/lib` / `$ORIGIN/../../.ry/lib`.
- Do not bundle openssl.
- Do not add rpath machinery for corrosion cdylib libLLVM lookup; `ry` loads libLLVM globally on Linux, and macOS is handled by bundle rewrite.
- Shell helpers that mutate parent-scope flags must not be invoked through a pipe; pass haystack data as an argument or here-string.
- Bundled cdylibs that do not match `libry_*` must be listed explicitly everywhere native libs are selected. Current explicit libs: `libemit.*`, `liblower.*`.
- Keep the explicit-lib list in sync across `scripts/bundle-dist.sh`, `install.sh`, `src/cli/self_update.cpp`, and `scripts/verify-bundle.sh`.
- If a stale `libry_*` cdylib links libLLVM, treat it as an orphan emission cdylib and exclude/fail by that discriminator.
