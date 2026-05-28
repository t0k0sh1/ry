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

2. **Whole-archive link of ry_lib**: Fuzz harnesses link `ry_lib` with `-Wl,-force_load` (macOS) / `-Wl,--whole-archive` (Linux) — the same pattern as `ry` and `ry_tests`. Without this, JIT and runtime symbols (`__ry_set_last_error`, `checked_malloc`, etc.) needed by directly-compiled runtime sources (e.g. `runtime/native/json.cpp`) will be undefined.

3. **Split `-fsanitize=fuzzer-no-link` vs `-fsanitize=fuzzer`**: `ENABLE_FUZZER` adds `-fsanitize=fuzzer-no-link` globally (coverage-only; no `main` injection) and the `add_ry_fuzz_target` CMake helper adds `-fsanitize=fuzzer` at link time **only on fuzz executables**. If `-fsanitize=fuzzer` were applied globally it would inject a competing `main` into `ry` and `ry_tests`, causing link errors.

**macOS note**: macOS-native libFuzzer builds historically required `SDKROOT=$(xcrun --show-sdk-path)` plus explicit `CC` / `CXX` exports, and `fuzz_json` hung under ASan on Darwin. Local fuzzer runs are now expected to go through `./docker/run.sh fuzz <binary> ...`, which uses the Linux toolchain in `ry-ci:llvm-21` and bypasses these issues entirely (issue #1865).

**How to apply**: When adding a new fuzz harness target, use `add_ry_fuzz_target(name sources...)` in CMakeLists.txt (inside `if(ENABLE_FUZZER)`). Never apply `-fsanitize=fuzzer` globally. Run locally via `./docker/run.sh fuzz <binary> -max_total_time=<sec> -artifact_prefix=tests/fuzz/regressions/<name>/ tests/fuzz/corpus/<name>` — the container handles `SDKROOT` / toolchain selection automatically.

---

### Fuzz harness sources must include every src/runtime/**/*.cpp whose symbols the directly-compiled sources call

**Source**: #1854 (2026-05-23, link failure during pre-commit fuzz run)
**Tags**: libfuzzer, fuzzer, harness, cmake, native-lib, cross-runtime-symbols, shared-lib

**Rule**: When `add_ry_fuzz_target(fuzz_X tests/fuzz/fuzz_X.cpp src/runtime/native/X.cpp ...)` lists a runtime source that calls symbols from **another** runtime file living in a separate `libry_<other>.dylib` (`add_ry_native_lib(<other>, src/runtime/native/<other>.cpp)`), the other runtime source must also appear in the fuzz target's source list. The shared libraries are NOT linked into fuzz binaries — they only `dlopen` at runtime in the main `ry` binary — so any symbol they normally provide must be supplied via direct source-level inclusion.

**Why**: Native libs (`libry_io.so`, `libry_json.so`, etc.) are SHARED libraries loaded by the JIT via `dlopen` based on `@native("mod")` declarations in `share/std/<mod>/<mod>.ry`. Fuzz harnesses don't run the JIT loader path — they call C functions directly from the harness TU. Their CMake target only whole-archive-links `ry_lib` plus the explicit source files passed to `add_ry_fuzz_target`. If runtime/native/json.cpp calls `__ry_io_file_read_all` (defined in runtime/native/io.cpp, which lives in `libry_io.so`), the fuzz_json link fails with `undefined reference to __ry_io_file_read_all` because `libry_io.so` is not part of the fuzz_json link line. The `__ry_set_last_error` / `__ry_get_last_error` exceptions live in `ry_lib` (whole-archive-linked) and resolve normally.

**How to apply**:
- When adding a new cross-runtime call (e.g. `runtime/native/json.cpp` calling `__ry_io_*` symbols from `runtime/native/io.cpp` in #1854), update both the runtime header (`include/ry/runtime/native/<other>.hpp` extern "C" block) **and** add `src/runtime/native/<other>.cpp` to the matching `add_ry_fuzz_target` line in `CMakeLists.txt`. Mirror the comment convention next to the target (`# fuzz_json ... runtime/native/io.cpp is also embedded because __ry_json_load_file references __ry_io_file_read_all`).
- Symptom of forgetting: `undefined reference to __ry_<other>_<symbol>` only at the fuzz target link step. Production / spec tests / sanitizer runs pass because they go through `dlopen` of the shared lib.
- Audit: `grep -E '__ry_(io|json|net|http)_' src/runtime/**/*.cpp` to find cross-module symbol calls, then verify each is reflected in CMakeLists.txt's `add_ry_fuzz_target` invocations.

---

### Regex parser calls exit(1) on malformed patterns — not fuzzable until refactored

**Source**: #896 (2026-04-18)
**Tags**: libfuzzer, regex, exit, gotcha

**Rule**: `RegexParser::parse()` in `src/runtime/core/regex_parser.cpp:13-21` calls `exit(1)` on unrecognised patterns. This terminates the libFuzzer process immediately, causing the fuzzer to report a crash and stop. The fuzz harness for the regex engine was therefore **excluded from #896** and tracked as follow-up issue #1176.

**How to apply**: Before adding a `fuzz_regex` harness, refactor `RegexParser::parse()` to throw `std::runtime_error` (or return `std::optional<CompiledRegex>`) instead of `exit(1)`. Also check `src/runtime/core/utf8.cpp`'s NUL-terminated variants (`__ry_utf8_char_at`, `__ry_utf8_char_index`) which also `exit(1)` on OOB — do not call these from fuzz harnesses; use the `_checked`/`_n` bounded variants instead.

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
the CLI (`src/app/main.cpp:297-310`) handling the same input cleanly via its
top-level `catch (const std::exception &)`.

**How to apply**: When writing or reviewing a fuzz harness, match the
established top-level pattern used across `src/app/main.cpp:308`,
`src/cli/cli.cpp:51,96`, `src/runtime/native/json.cpp:766`, `src/formatter.cpp:702,798`
— a single `catch (const std::exception &)` backstop. Preserve the
`// NOLINT(bugprone-empty-catch)` suppression so clang-tidy stays clean
under the `/static-analysis-tools` skill (Clang-Tidy section). Document the expected exception types in the
catch-block comment instead of splitting into multiple specific catches.
