---
name: static-analysis-tools
description: Clang-Tidy / Cppcheck / scan-build (Clang Static Analyzer) configuration, local-run commands, CI jobs, and suppression rules. Use when handling clang-tidy / cppcheck warnings, scan-build, NOLINT, lint failures, or static-analyzer false positives. Also fires on Japanese triggers clang-tidy 実行, cppcheck 警告, 静的解析, lint 失敗.
allowed-tools: Bash
---

# Static Analysis Tools

Configuration and invocation of Clang-Tidy, Cppcheck, and Clang Static Analyzer (scan-build) in ry.

> Relocated from `AGENTS.md` by #1384.

## Clang-Tidy

Config: project-root `.clang-tidy`. CI's `clang-tidy` job runs over every `src/*.cpp`.

```text
Enabled:  bugprone-*, performance-*, cert-*, selective modernize-*
Excluded: bugprone-easily-swappable-parameters, cert-err58-cpp, etc. (see .clang-tidy)
```

- `HeaderFilterRegex` is restricted to project headers (`include/ry/`); LLVM / GoogleTest are SYSTEM-included and auto-excluded.
- `compile_commands.json` is emitted by `CMAKE_EXPORT_COMPILE_COMMANDS=ON` under `build/`.
- CI build scope by event (#1741):
  - **pull request**: `cmake --build build --target ry --parallel` — fast build (`src/app/main.cpp` + `ry_lib` ≈ ~76 TU); `ry_tests` / native plugin / fuzz TUs skipped.
  - **push to main**: `cmake --build build --parallel` — full build (all targets).
  - **Note**: `--target ry` narrows **only the build step**. clang-tidy analysis still covers all 90 `src/*.cpp` files (14 of them outside `ry_lib`) in parallel on both events.
- Per-TU parallel analysis: `xargs -0 -n 1 -P "$(nproc)"` (#1741). `-n 1` is required — without it xargs hands every `.cpp` to a single clang-tidy invocation and `-P` parallelises nothing.
- Run locally via Docker (#1865 — sidesteps macOS PCH-compat and Homebrew LLVM PATH issues):
  ```bash
  ./docker/run.sh static-analysis clang-tidy
  ```
- Keep new code at zero Clang-Tidy warnings.

### Platform-specific false positives (libc++ vs libstdc++)

**Source**: #1405 (2026-04-27)
**Tags**: clang-tidy, bugprone-exception-escape, libc++, libstdc++, noexcept, platform-specific

**Context**: libc++ (macOS Homebrew LLVM) is more conservative than libstdc++ (Linux apt LLVM) when inferring `noexcept` for checks like `bugprone-exception-escape` — it won't infer std container move-assignment, `resize()`, or lambda `operator()` as `noexcept`. Code green on Linux CI can error on macOS local.

**Suppression policy**:

1. **Operations that really are noexcept**: declare `noexcept` explicitly — e.g. a destructor of only container move-assignments, written `~Foo() noexcept;` (update declaration and definition together). Spec-conformant and preferred over NOLINT. If the body also contains operations libc++ always sees as throwing (e.g. `resize()`), the warning persists, so add `// NOLINTNEXTLINE` too.
2. **Process boundaries, watchers, thread entries**: suppress with `// NOLINTNEXTLINE(bugprone-exception-escape): <reason>` where `std::terminate` is acceptable. State the specific context (`process boundary`, `watcher lambda`, `thread entry`) in the reason.

**Do not suppress** ordinary throwing functions — declaring them `noexcept` calls `std::terminate` on exception. Outside process boundaries, keep an exception design callers can catch.

**Local verification**: `./docker/run.sh static-analysis clang-tidy` reproduces Linux + libstdc++ (#1865) without juggling Apple clang ↔ Homebrew LLVM ↔ Linux LLVM or hitting PCH-compat issues.

**Examples**:
- `src/codegen.cpp` `CodeGen::FnScope::~FnScope() noexcept` — `noexcept` + `NOLINTNEXTLINE` (trailing `resize()` inferred throwing under libc++).
- `src/app/main.cpp` `main()` and the watcher lambda — `NOLINTNEXTLINE` (process boundary).

## Cppcheck

Suppressions: project-root `.cppcheck-suppressions`. CI's `lint` job runs over `src/` and `include/`.

```text
Enabled:  warning, performance, portability
Excluded: see .cppcheck-suppressions
```

- No `compile_commands.json` (no build required; runs quickly).
- Inline `// cppcheck-suppress <id>` comments honoured (`--inline-suppr`).
- Run locally via Docker (#1865):
  ```bash
  ./docker/run.sh static-analysis cppcheck
  ```
- Keep new code at zero Cppcheck warnings.

## Clang Static Analyzer (scan-build)

Path-sensitive symbolic-execution analysis catching null deref, use-after-free, memory leaks, uninitialised reads, and dead stores that Clang-Tidy / Cppcheck typically miss.

CI scope by event (#1738):
- **pull request**: `--target ry --parallel` — fast scan (`src/app/main.cpp` + `ry_lib` ≈ ~76 TU); test / native-plugin TUs excluded for fast PR feedback.
- **push to main**: `--parallel` (no `--target`) — full scan (all targets), wider coverage including tests, native plugins, and fuzz.

Both events run with `continue-on-error: true` (warn-only).

- `scan-build` ships with the LLVM 21 source build in the CI container (`ghcr.io/<owner>/ry-ci:llvm-21`) at `/usr/local/llvm/bin/scan-build`.
- No `compile_commands.json` (scan-build wraps the build itself).
- Run locally via Docker (#1865 — avoids macOS off-PATH / Homebrew-dependent scan-build). Fast scan (`ry` target only, PR-equivalent):
  ```bash
  ./docker/run.sh static-analysis scan-build
  ```
  The HTML report is bind-mounted to `build-scan-docker/scan-build-report/<timestamp>/index.html` on the host (container path `/workspace/build-scan/scan-build-report/`); it persists after container exit — open in a browser.
- Run all three tools at once: `./docker/run.sh static-analysis all`.
- `scan-build` and `all` build under `build-scan-docker/` (host) ↔ `build-scan/` (container) with an analyzer wrapper, leaving `build-docker/` untouched. No `rm -rf` needed before a follow-up `./docker/run.sh default ...`; delete `build-scan-docker/` to discard reports.
- Suppress false positives inline with `#ifndef __clang_analyzer__` (same granularity as clang-tidy `// NOLINT`).
- Warn-only (`continue-on-error: true`); address new null-dereference / use-after-free / division-by-zero findings in the same PR whenever possible.
