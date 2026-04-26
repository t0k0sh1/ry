---
name: libfuzzer-harness
description: libFuzzer harness requirements, known limitations, and crash-handling rules for the ry project. Use when adding a new fuzz target, troubleshooting a fuzzer build failure, diagnosing a crash, or reviewing an existing harness.
allowed-tools: Bash
---

# libFuzzer Harness

Reference for libFuzzer toolchain requirements and harness conventions in the ry project.

---

### libFuzzer requires Clang, whole-archive ry_lib, and split -fsanitize=fuzzer flags

**Source**: #896 (2026-04-18)
**Tags**: libfuzzer, fuzzer, sanitizer, cmake, clang, llvm

**Rule**: libFuzzer has three non-obvious toolchain requirements that must all be satisfied:

1. **Clang-only**: `-fsanitize=fuzzer` is not supported by GCC or Apple Clang (Apple's fuzzer runtime is a stub). The `fuzz` CMake preset enforces this with a `FATAL_ERROR` check on `CMAKE_CXX_COMPILER_ID`. On macOS use `/opt/homebrew/opt/llvm@21/bin/clang++`; on Linux CI `/usr/local/llvm/bin/clang++`.

2. **Whole-archive link of ry_lib**: Fuzz harnesses link `ry_lib` with `-Wl,-force_load` (macOS) / `-Wl,--whole-archive` (Linux) — the same pattern as `ry` and `ry_tests`. Without this, JIT and runtime symbols (`__ry_set_last_error`, `checked_malloc`, etc.) needed by directly-compiled runtime sources (e.g. `runtime_json.cpp`) will be undefined.

3. **Split `-fsanitize=fuzzer-no-link` vs `-fsanitize=fuzzer`**: `ENABLE_FUZZER` adds `-fsanitize=fuzzer-no-link` globally (coverage-only; no `main` injection) and the `add_ry_fuzz_target` CMake helper adds `-fsanitize=fuzzer` at link time **only on fuzz executables**. If `-fsanitize=fuzzer` were applied globally it would inject a competing `main` into `ry` and `ry_tests`, causing link errors.

**macOS extra**: Homebrew LLVM Clang needs `SDKROOT=$(xcrun --show-sdk-path)` to find system C headers; without it the PCH compilation for `ry_lib` fails with libc++ `<cstdio>` not found. The `fuzz` preset does **not** hardcode this path (not portable); callers must set it as an env var.

**How to apply**: When adding a new fuzz harness target, use `add_ry_fuzz_target(name sources...)` in CMakeLists.txt (inside `if(ENABLE_FUZZER)`). Never apply `-fsanitize=fuzzer` globally. When building locally on macOS, always prepend `SDKROOT=$(xcrun --show-sdk-path) CC=<llvm-clang> CXX=<llvm-clang++>`.

---

### Regex parser calls exit(1) on malformed patterns — not fuzzable until refactored

**Source**: #896 (2026-04-18)
**Tags**: libfuzzer, regex, exit, gotcha

**Rule**: `RegexParser::parse()` in `src/runtime_regex_parser.cpp:13-21` calls `exit(1)` on unrecognised patterns. This terminates the libFuzzer process immediately, causing the fuzzer to report a crash and stop. The fuzz harness for the regex engine was therefore **excluded from #896** and tracked as follow-up issue #1176.

**How to apply**: Before adding a `fuzz_regex` harness, refactor `RegexParser::parse()` to throw `std::runtime_error` (or return `std::optional<CompiledRegex>`) instead of `exit(1)`. Also check `src/runtime_utf8.cpp`'s NUL-terminated variants (`__ry_utf8_char_at`, `__ry_utf8_char_index`) which also `exit(1)` on OOB — do not call these from fuzz harnesses; use the `_checked`/`_n` bounded variants instead.

---

### Fuzz harnesses must catch `std::exception`, not subtype specifics

**Source**: #1275 (2026-04-21)
**Tags**: libfuzzer, fuzzer, harness, exceptions, catch, parser

**Rule**: In `LLVMFuzzerTestOneInput`, wrap the target call in
`catch (const std::exception &)` — never catch only `std::runtime_error` or
a named derived type. `std::logic_error` (including `std::out_of_range`
and `std::invalid_argument`) is a **disjoint** subtree from
`std::runtime_error`, so a `runtime_error`-only catch silently lets future
parser/lexer throws escape to `libc++abi`, which the fuzzer reports as a
deadly signal and terminates the run.

**Context**: `tests/fuzz/fuzz_parser.cpp` originally caught
`ry::DiagnosticError` and `std::runtime_error` (the first is redundant —
`DiagnosticError` derives from `std::runtime_error` per
`include/ry/diagnostic.hpp:22`). This missed `std::out_of_range` thrown by
`parseFloatLiteral` / `parseIntLiteral` in `include/ry/parser.hpp:196,210`
for malformed literals like `0B1f32`, causing a libFuzzer crash despite
the CLI (`src/main.cpp:297-310`) handling the same input cleanly via its
top-level `catch (const std::exception &)`.

**How to apply**: When writing or reviewing a fuzz harness, match the
established top-level pattern used across `src/main.cpp:308`,
`src/cli.cpp:51,96`, `src/runtime_json.cpp:766`, `src/formatter.cpp:702,798`
— a single `catch (const std::exception &)` backstop. Preserve the
`// NOLINT(bugprone-empty-catch)` suppression so clang-tidy stays clean
under the `/static-analysis-tools` skill (Clang-Tidy section). Document the expected exception types in the
catch-block comment instead of splitting into multiple specific catches.
