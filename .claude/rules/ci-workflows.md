---
paths:
  - ".github/workflows/**/*.yml"
  - ".github/actions/**/*.yml"
  - ".github/codeql/**/*.yml"
---

# CI Workflows

- Normal Linux CI uses `ry-ci`; release Linux artifacts use `ry-ci-glibc-old` for the older glibc ABI.
- Release workflows must pin a literal immutable `ry-ci-glibc-old:llvm-<MAJOR>-rev<N>` tag; do not use mutable tags or dynamic lookup there.
- Shared CI tools belong in `docker/ci.Dockerfile`; release-required tools also belong in `docker/ci-glibc-old.Dockerfile`.
- Linux cache paths are inside the container, e.g. `/root/.cache/ccache`.
- Keep CodeQL on `push: branches: [main]`; C/C++ CodeQL should build target `ry`.
- CodeQL release gates must filter push runs, poll for existence and completion, and have `actions: read`.
- `actions/download-artifact@v4` `pattern:` is a glob; prefer separators such as `__` that cannot appear in matrix values.
- Warn-only jobs need `continue-on-error: true` on GitHub service steps such as upload/cache/code-scanning save.
- Keep Rust lint workflow scope aligned with `.claude/skills/pre-commit-checklist/run-rust-lint.sh`; keep `rust-toolchain.toml` aligned with `RUST_VERSION` in `docker/ci.Dockerfile`.
- Quote YAML step names containing space-`#`.
- Linux `test` / `asan` jobs omit `-p`; worker=1 keeps failures localizable and avoids JIT teardown leak accumulation.
