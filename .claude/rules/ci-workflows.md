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

### Linux CI runs in pre-baked GHCR container images, no apt usage

**Source**: #1505 (2026-05-02)
**Tags**: ci, github-actions, container, ghcr, no-apt

**Rule**: Every Linux job in `.github/workflows/*.yml` (CI, CodeQL,
release) must use
`container: ghcr.io/${{ github.repository_owner }}/ry-ci:llvm-21`
(or `ry-ci-glibc-old:llvm-21` for release Linux jobs). Never add
`apt-get install` or `sudo apt-get` to a Linux job step. The container
pre-installs the entire toolchain: clang/clang++ 21
(`/usr/local/llvm`), cmake (`/opt/cmake`), ninja (`/opt/ninja`),
ccache (`/usr/local/bin`), OpenSSL (`/opt/openssl`), cppcheck
(`/opt/cppcheck`), and a vendored gtest tarball at
`$RY_VENDORED_GTEST_TARBALL`. `CC` / `CXX` / `PATH` /
`LD_LIBRARY_PATH` / `OPENSSL_ROOT_DIR` / `LLVM_DIR` are pre-set as
ENV.

**Why**: An Ubuntu apt mirror outage on 2026-05-02 (#1505) caused
`apt-get install` from `archive.ubuntu.com` and `apt.llvm.org` to fail
intermittently across all CI jobs. Pre-baking the toolchain into
GHCR-hosted images isolates CI from upstream Ubuntu/Debian repository
availability. Image rebuild is on demand via `build-ci-image.yml`
(`workflow_dispatch` + `push` on Dockerfile changes).

**How to apply**:
- Linux CI / CodeQL / release jobs: set
  `container: ghcr.io/.../ry-ci:llvm-21`
- For release Linux only: use `ry-ci-glibc-old:llvm-21`
  (`gcc:14-bookworm` base, glibc 2.36) so binaries remain runnable on
  older Linux distros (Ubuntu 22.04 with glibc 2.35, RHEL 9 with
  glibc 2.34, etc.).
- macOS jobs continue using Homebrew on the host runner — only Linux
  is containerised.
- If a new tool is needed in CI, add it to `docker/ci.Dockerfile`
  (and `docker/ci-glibc-old.Dockerfile` if release also needs it),
  trigger a manual image rebuild via `workflow_dispatch`, and wait
  for the new image before merging the workflow change. See
  `.claude/skills/ci-image-workflow/SKILL.md`.

**How to verify**: `grep -rnE 'apt(-get)?\b' .github/workflows/ docker/`
must return zero hits. CI logs should show every Linux job running
inside `ghcr.io/.../ry-ci:llvm-21` (visible at the start of each step
as `Set up job` / `Initialize containers`).

### ccache cache path inside container is `/root/.cache/ccache`, not the runner's home

**Source**: #1505 (2026-05-02)
**Tags**: ci, ccache, container, github-actions, cache

**Context**: `actions/cache@v4` reads / writes paths inside the
container's filesystem, not the host runner's. The container runs as
root by default, so ccache writes to `/root/.cache/ccache`, not
`/home/runner/.cache/ccache` (which is the path you would use on a
bare runner). `hendrikmuhs/ccache-action@v1` cannot be used in the
container because it internally invokes `sudo apt-get install ccache`,
violating the no-apt rule above.

**Rule**: When adding ccache to a container-based job, use
`actions/cache@v4` directly with `path: /root/.cache/ccache`. The
`ccache` binary is already at `/usr/local/bin/ccache` (installed
during image build), so no install step is needed. ccache picks up
`~/.cache/ccache` which expands to `/root/.cache/ccache` for the
root user automatically; setting `CCACHE_DIR` is unnecessary.

**How to apply**:

```yaml
- name: Restore ccache
  uses: actions/cache@v4
  with:
    path: /root/.cache/ccache
    key: ci-${{ matrix.job }}-${{ github.ref }}-${{ github.sha }}
    restore-keys: |
      ci-${{ matrix.job }}-${{ github.ref }}-
      ci-${{ matrix.job }}-
```

macOS jobs (in `release.yml`) keep using `hendrikmuhs/ccache-action@v1`
because they run on host runners (no container) and Homebrew is the
canonical install method on macOS.

### Release Linux binaries must use `ry-ci-glibc-old` for older-glibc compatibility

**Source**: #1505 (2026-05-02)
**Tags**: ci, release, glibc, container, abi, linux

**Context**: Regular CI uses `ry-ci` (Debian trixie via gcc:14-trixie,
glibc 2.40). Binaries built against trixie's glibc 2.40 fail at
startup on older Linux distros (Ubuntu 22.04 with glibc 2.35, RHEL 9
with glibc 2.34, etc.) with errors like
`/lib/x86_64-linux-gnu/libc.so.6: version 'GLIBC_2.40' not found`.

**Rule**: Release Linux build jobs (`release.yml`) must use
`ry-ci-glibc-old` (Debian bookworm via `gcc:14-bookworm`, glibc 2.36)
as the container, **not** `ry-ci`. The `ry-ci-glibc-old` image is
built from `docker/ci-glibc-old.Dockerfile` and rebuilt by the same
`build-ci-image.yml` `workflow_dispatch` (matrix includes both image
names).

**How to apply**: in `release.yml`, the Linux container is wired as

```yaml
container: ${{ matrix.llvm_install == 'container' && format('ghcr.io/{0}/ry-ci-glibc-old:llvm-21', github.repository_owner) || null }}
```

Other workflows (`ci.yml`, `codeql.yml`) use `ry-ci:llvm-21` because
glibc compat does not matter for CI artifacts that never leave the
runner. The dev `docker/Dockerfile` also inherits from `ry-ci`, not
`ry-ci-glibc-old`, since dev work does not produce distributable
binaries.

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

### scan-build: pass `--use-analyzer=/usr/local/llvm/bin/clang` explicitly

**Source**: #1247 (2026-04-20), updated for #1505 container migration (2026-05-02)
**Tags**: ci, scan-build, llvm, container, static-analysis

**Rule**: Always pass `--use-analyzer=/usr/local/llvm/bin/clang`
explicitly to every `scan-build` invocation in
`.github/workflows/ci.yml` and every example in
`.claude/skills/static-analysis-tools/SKILL.md`. The `--use-cc` flag
controls only the build-time compiler and is ignored by the
analyzer-clang lookup.

**Why**: `scan-build`'s `FindClang()` Perl function tries
`$RealBin/bin/clang`, `/usr/lib/llvm-{MAJOR}/bin/clang`, and the Xcode
toolchain in order. None of those resolve in the
`ghcr.io/.../ry-ci:llvm-21` container — LLVM is source-built and
installed at `/usr/local/llvm/`, scan-build itself is not in the
parent of an adjacent `bin/clang` (so `$RealBin/bin/clang`
mis-expands), and `/usr/lib/llvm-{MAJOR}` is a Debian-specific path
that does not exist on the container's debian-trixie base because
clang was never apt-installed. Without `--use-analyzer`, scan-build
leaves `$Clang` undefined and emits
`Use of uninitialized value $Clang in concatenation` followed by
`scan-build: error: Cannot find an executable 'clang' relative to scan-build`.

The same flag was already required before #1505: prior to the
container migration, the (now-removed) LLVM mirror tarball relied on
the Debian-patched scan-build script with a hard-coded
`/usr/lib/llvm-{MAJOR}` fallback that pointed nowhere because the
mirror extracted to `/usr/local/llvm/` instead. The container removed
both the Debian patch and the apt-installed `/usr/lib/llvm-*` tree;
the remediation (`--use-analyzer`) is the same in both worlds, so
keeping the flag uniform across history avoids drift.

**How to apply**: In every CI `scan-build` invocation
(`.github/workflows/ci.yml`) and every local-execution example
(`.claude/skills/static-analysis-tools/SKILL.md`), include
`--use-analyzer=/usr/local/llvm/bin/clang` alongside `--use-cc` /
`--use-c++`. macOS Homebrew `scan-build` resolves clang via
`$RealBin/bin/clang` correctly when scan-build is at
`/opt/homebrew/opt/llvm@21/bin/scan-build`, so the flag is redundant
there — but keeping it uniform prevents drift between local-macOS and
CI invocations.

**How to verify**: `grep -n 'scan-build' .github/workflows/*.yml .claude/skills/static-analysis-tools/SKILL.md`
and confirm `--use-analyzer` accompanies every invocation. In CI logs,
the `Use of uninitialized value $Clang` warning must be absent.
