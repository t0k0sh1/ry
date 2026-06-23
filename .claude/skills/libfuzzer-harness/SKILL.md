---
name: libfuzzer-harness
description: libFuzzer harness requirements, known limitations, and crash-handling rules for the ry project. Use when adding a new fuzz target, troubleshooting a fuzzer build failure, diagnosing a crash, or reviewing an existing harness.
allowed-tools: Bash
---

# libFuzzer Harness

For Linux Rust-cdylib/fuzz build verification, use the current `ghcr.io/t0k0sh1/ry-ci:llvm-21` image directly; the local development image may lag its baked Rust toolchain.

---

### libFuzzer requires Clang, whole-archive ry_lib, and split -fsanitize=fuzzer flags

**Source**: #896 (2026-04-18)
**Tags**: libfuzzer, fuzzer, sanitizer, cmake, clang, llvm

**Rule**: Three non-obvious toolchain requirements must all be satisfied:

1. **Clang-only**: `-fsanitize=fuzzer` is unsupported by GCC or Apple Clang. The `fuzz` CMake preset enforces this with a `FATAL_ERROR` check on `CMAKE_CXX_COMPILER_ID`. On macOS use `/opt/homebrew/opt/llvm@21/bin/clang++`; on Linux CI `/usr/local/llvm/bin/clang++`.

2. **Whole-archive link of ry_lib**: Fuzz harnesses link `ry_lib` with `-Wl,-force_load` (macOS) / `-Wl,--whole-archive` (Linux) — the same pattern as `ry` and `ry_tests`. Without this, JIT and runtime symbols (`__ry_set_last_error`, `checked_malloc`, etc.) will be undefined.

3. **Split `-fsanitize=fuzzer-no-link` vs `-fsanitize=fuzzer`**: `ENABLE_FUZZER` adds `-fsanitize=fuzzer-no-link` globally (coverage-only) and `add_ry_fuzz_target` adds `-fsanitize=fuzzer` at link time only on fuzz executables. Applying `-fsanitize=fuzzer` globally would inject a competing `main` into `ry` and `ry_tests`.

**macOS note**: Local fuzzer runs go through `./docker/run.sh fuzz <binary> ...`, which uses the Linux toolchain in `ry-ci:llvm-21` and bypasses macOS ASan/SDKROOT issues (issue #1865).

**How to apply**: Use `add_ry_fuzz_target(name sources...)` in `CMakeLists.txt` (inside `if(ENABLE_FUZZER)`). Run locally via `./docker/run.sh fuzz <binary> -max_total_time=<sec> -artifact_prefix=tests/fuzz/regressions/<name>/ tests/fuzz/corpus/<name>`.

---

### Fuzz harness sources must include every src/runtime/**/*.cpp whose symbols the directly-compiled sources call

**Source**: #1854 (2026-05-23, link failure during pre-commit fuzz run)
**Tags**: libfuzzer, fuzzer, harness, cmake, native-lib, cross-runtime-symbols, shared-lib

**Rule**: When a fuzz target's source calls symbols from another runtime file living in a separate `libry_<other>.dylib`, that other source must also appear in the fuzz target's source list. Shared libraries are NOT linked into fuzz binaries — they only `dlopen` at runtime — so symbols must be supplied via direct source inclusion.

**Why**: Native libs (`libry_io.so`, `libry_json.so`, etc.) are loaded by the JIT via `dlopen`. Fuzz harnesses call C functions directly and only whole-archive-link `ry_lib` plus explicit sources. If `runtime/native/json.cpp` calls `__ry_io_file_read_all` (in `runtime/native/io.cpp` / `libry_io.so`), the fuzz_json link fails with `undefined reference` because `libry_io.so` is not on the fuzz link line.

**How to apply**:
- When adding a cross-runtime call, update both the runtime header (`include/ry/runtime/native/<other>.hpp` extern "C" block) **and** add `src/runtime/native/<other>.cpp` to the matching `add_ry_fuzz_target` line. Mirror the comment convention (e.g. `# runtime/native/io.cpp is also embedded because __ry_json_load_file references __ry_io_file_read_all`).
- Symptom: `undefined reference to __ry_<other>_<symbol>` only at fuzz link; production / spec tests / sanitizer runs pass (they use `dlopen`).
- Audit: `grep -E '__ry_(io|json|net|http)_' src/runtime/**/*.cpp` to find cross-module symbol calls, then verify each is in `CMakeLists.txt`'s `add_ry_fuzz_target` invocations.

---

### Regex parser calls exit(1) on malformed patterns — not fuzzable until refactored

**Source**: #896 (2026-04-18)
**Tags**: libfuzzer, regex, exit, gotcha

**Rule**: `RegexParser::parse()` in `src/runtime/core/regex_parser.cpp` calls `exit(1)` on unrecognised patterns, terminating the libFuzzer process immediately. The regex fuzz harness was excluded from #896 and tracked as #1176.

**How to apply**: Before adding a `fuzz_regex` harness, refactor `RegexParser::parse()` to throw `std::runtime_error` instead of `exit(1)`. Also check `src/runtime/core/utf8.cpp`'s NUL-terminated variants (`__ry_utf8_char_at`, `__ry_utf8_char_index`) which also `exit(1)` on OOB — use the `_checked`/`_n` bounded variants instead.

---

### Fuzz harnesses must catch `std::exception`, not subtype specifics

**Source**: #1275 (2026-04-21)
**Tags**: libfuzzer, fuzzer, harness, exceptions, catch, parser

**Rule**: In `LLVMFuzzerTestOneInput`, wrap the target call in `catch (const std::exception &)` — never catch only `std::runtime_error` or a named derived type. `std::logic_error` (including `std::out_of_range` and `std::invalid_argument`) is a **disjoint** subtree from `std::runtime_error`, so a `runtime_error`-only catch lets future parser/lexer throws escape to `libc++abi`, which the fuzzer reports as a deadly signal.

**How to apply**: Match the top-level pattern used across `src/app/main.cpp`, `src/cli/cli.cpp`, `src/runtime/native/json.cpp`, `src/formatter.cpp` — a single `catch (const std::exception &)` backstop. Preserve the `// NOLINT(bugprone-empty-catch)` suppression; suppression policy is in `.claude/rules/build-warning-flags.md`. Document expected exception types in the catch-block comment instead of splitting into multiple catches.
