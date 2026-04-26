---
paths:
  - ".github/workflows/**/*.yml"
  - ".github/actions/**/*.yml"
---

# CI Workflows

### UBSan must disable `vptr` and `function` checks on this project

**Source**: #630 (2026-04-11, implementation)
**Tags**: ubsan, sanitizer, cmake, llvm, cxx-flags

**Context**: When enabling UBSan (`-fsanitize=undefined`) on ry, two
sub-checks fail in ways that have nothing to do with actual
undefined behavior:

1. `vptr` — ry compiles with `-fno-rtti` (to match LLVM's build
   flags, see `CMakeLists.txt:25`). The `vptr` check requires RTTI
   to resolve virtual-call types, so enabling it under `-fno-rtti`
   produces no coverage and in some toolchains hard-errors.
2. `function` — LLVM exposes many C-style function pointers that
   ry casts through `void *` (JIT symbol resolution, runtime
   dispatch, etc.). UBSan's `function` check flags every one of
   these as a type mismatch, drowning out real signal.

**Rule**: When adding or extending UBSan flags in `CMakeLists.txt`,
always pair `-fsanitize=undefined` with
`-fno-sanitize=vptr,function`. Do not try to fix the false positives
by changing the LLVM interop code — the checks themselves are the
wrong tool for this codebase.

**Also**: UBSan and ASan are compatible and are enabled together via
the `asan` CMake preset (`ENABLE_ASAN=ON` + `ENABLE_UBSAN=ON`). TSan
is **not** compatible with either and lives in its own `tsan` preset
(`build-tsan/`). `CMakeLists.txt` enforces this with a
`FATAL_ERROR` if `ENABLE_TSAN` is combined with the others.

### LLVM 21 via apt.llvm.org requires zlib1g-dev and libzstd-dev in addition to LLVM packages

**Source**: #1165 Docker dev environment setup (2026-04-18)
**Tags**: build, llvm, cmake, docker, dependencies

**Rule**: When using LLVM 21 packages from `apt.llvm.org` on Ubuntu, the CMake package
`find_package(LLVM REQUIRED ...)` will fail at configure time unless `zlib1g-dev` and
`libzstd-dev` are installed — even if you never explicitly use zlib or zstd yourself.

**Why**: `LLVMExports.cmake` (generated when LLVM was compiled) lists `ZLIB::ZLIB` and
`zstd::libzstd_shared` in the `INTERFACE_LINK_LIBRARIES` of the `LLVMSupport` target.
CMake validates all link-interface targets at import time. If either target is missing,
you get:

```text
CMake Error at .../LLVMExports.cmake:73 (set_target_properties):
  The link interface of target "LLVMSupport" contains: ZLIB::ZLIB
  but the target was not found.
```

**How to apply**: Any Dockerfile or CI step that installs LLVM 21 via `apt.llvm.org`
must also `apt-get install -y zlib1g-dev libzstd-dev` BEFORE the `find_package(LLVM)`
CMake configure step. Add them to the same `apt-get install` layer as `cmake`, not after.

### ubuntu:24.04 archive signing key expires — bypass for dev Dockerfiles

**Source**: #1165 Docker dev environment setup (2026-04-18)
**Tags**: docker, ubuntu, gpg, apt, dev-environment

**Rule**: On Apple Silicon (arm64) the ubuntu:24.04 Docker image uses `ports.ubuntu.com`
which is signed with a key that can expire on systems whose clock is ahead of the key's
validity period. When `apt-get update` fails with "At least one invalid signature was
encountered", installing `ubuntu-keyring` will not help (it is already the newest version
in-image). The correct fix for a **dev-only** Dockerfile is to replace `Signed-By:` with
`Trusted: yes` in `/etc/apt/sources.list.d/ubuntu.sources` before the first
`apt-get update`:

```dockerfile
RUN sed -i 's|^Signed-By:.*|Trusted: yes|' /etc/apt/sources.list.d/ubuntu.sources \
  && apt-get update \
  && apt-get install -y ...
```

**Why**: ubuntu 24.04 uses the deb822 sources format at
`/etc/apt/sources.list.d/ubuntu.sources`. The `Signed-By:` directive points to the GPG
keyring. Replacing it with `Trusted: yes` bypasses signature verification entirely. Do
NOT use this workaround for production images.

**How to apply**: Add the `sed` step as the first line of the `RUN` block that installs
build dependencies. Keep it in the same `RUN` as the `apt-get install` so that the
`Trusted: yes` directive is baked in and future `apt-get update` calls within the build
also work.

### `github.ref_name` points to `main` in scheduled workflows, not the checked-out branch

**Source**: #970 (2026-04-15, implementation)
**Tags**: ci, github-actions, schedule, ccache, gotcha

**Rule**: In a GitHub Actions scheduled workflow (`on: schedule:`), `github.ref_name` is
always the **default branch** (`main`), regardless of which branch the job checks out via
`actions/checkout` with an explicit `ref:`. Expressions like
`github.ref_name == 'main' || startsWith(github.ref_name, 'v')` therefore always evaluate
to `true` or `false` based on the workflow file's source branch, not the checked-out branch.

**Why**: When designing a scheduled workflow that dynamically checks out a branch
via `actions/checkout` with an explicit `ref:`, it is tempting to copy a
`ccache save:` expression like
`${{ github.ref_name == 'main' || startsWith(github.ref_name, 'v') }}` from a
push-triggered workflow. On a scheduled run this always evaluates to `true` (if the
workflow file lives on `main`) regardless of the resolved branch choice, but if the
workflow file ever ends up on a non-matching branch the cache would silently never be
saved. Relying on this expression in scheduled workflows is fragile. (Originally
encountered while designing the now-removed `ci-scheduled.yml`; the gotcha generalises
to any `on: schedule:` workflow.)

**How to apply**: In scheduled workflows that dynamically check out a branch, replace the
conditional `save:` expression with `save: true` (always save) or compute the condition from
the resolved `ref` output rather than `github.ref_name`.

### CodeQL must run on default branch to keep Code Scanning dashboard fresh

**Source**: #1367 (2026-04-25, planning)
**Tags**: ci, github-actions, codeql, dashboard, schedule, security

**Rule**: GitHub's Code Scanning Security alerts dashboard reflects the **latest analysis
on the default branch**, not on PR branches. A CodeQL workflow whose only triggers are
`pull_request:` and `workflow_dispatch:` will analyze every PR but never refresh the
dashboard after merge — alerts fixed (or newly introduced) on `main` will not appear
until someone manually dispatches the workflow.

**Why**: When migrating off the daily `schedule:` cron in `codeql.yml` (#1367), the
naive replacement is `pull_request: + workflow_dispatch:`. That covers PR gating but
silently drops dashboard freshness on `main`. The merge-commit run that GitHub used to
get from the cron is gone.

**How to apply**: Always pair `pull_request:` with `push: branches: [<default>]` when
removing a `schedule:` from a CodeQL workflow. Keep `workflow_dispatch:` as a manual
escape hatch but never rely on it for dashboard freshness. Verify after merge that
the next push to the default branch produces a CodeQL run in the Actions tab.

### scan-build: mirror tarball uses Debian-patched path for FindClang

**Source**: #1247 (2026-04-20)
**Tags**: ci, scan-build, llvm, mirror, static-analysis

**Rule**: The Debian-patched `scan-build` Perl script bundled in the LLVM
mirror tarball (via `clang-tools-{MAJOR}`) has a hard-coded fallback path
in `FindClang()` (line 1508) that points at `/usr/lib/llvm-{MAJOR}/bin/clang`.
The mirror tarball extracts to `/usr/local/llvm`, so that Debian path does
not exist on the runner. Always pass `--use-analyzer=/usr/local/llvm/bin/clang`
explicitly to `scan-build` — `--use-cc` controls only the build-time compiler
and is ignored by the analyzer-clang lookup.

**Why**: Before the mirror tarball was introduced, `setup-llvm` fell back
to `apt.llvm.org`, which populates `/usr/lib/llvm-{MAJOR}/`, so the
hard-coded Debian path resolved accidentally and `scan-build` worked
without `--use-analyzer`. Once the mirror took over, the path no longer
exists and `FindClang()` exhausts all three lookup candidates
(`$RealBin/bin/clang`, `/usr/lib/llvm-{MAJOR}/bin/clang`, Xcode toolchain)
and leaves `$Clang` undefined, producing
`Use of uninitialized value $Clang in concatenation` and
`scan-build: error: Cannot find an executable 'clang' relative to scan-build`.

**How to apply**: In every CI `scan-build` invocation (currently
`.github/workflows/ci.yml`) and every local-execution example in `AGENTS.md`,
include `--use-analyzer=/usr/local/llvm/bin/clang` alongside `--use-cc` /
`--use-c++`. macOS Homebrew `scan-build` does not have the Debian patch, so the
flag is redundant there — but keeping it uniform prevents drift between local
and CI invocations.

**How to verify**: `grep -n 'scan-build' .github/workflows/*.yml AGENTS.md`
and confirm `--use-analyzer` accompanies every invocation. In CI logs, the
`Use of uninitialized value $Clang` warning must be absent.
