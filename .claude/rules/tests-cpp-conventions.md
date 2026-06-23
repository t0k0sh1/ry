---
paths:
  - "tests/test_*.cpp"
  - "tests/test_*.hpp"
---

# Tests — C++ Conventions

This file covers only hazards that are not visible from reading the code.

### Renaming stdlib `@native` functions: also sweep embedded Ry source in C++ tests

**Tags**: testing, codegen-test, stdlib, rename, refactor, blind-spot

Files such as `tests/test_codegen_directive.cpp` embed source inline as `runSource("@native(\"path\")\nfn is_absolute(p: str) -> bool\n...")`. Sweeping only `.ry` files misses these. Because exported symbols still match, the build succeeds; failure appears only at `./build/ry_tests` run time. When renaming a `@native` function, use the 4-step procedure in `/horizontal-sweep`.

### Stdlib-declared directives need `withStdlibDirectiveDecls()` in C++ tests

**Tags**: testing, codegen-test, directives, stdlib, module-loader, harness

`@inline` / `@parallel` / `@const` / `@deprecated` / `@each` / `@property` / `@it` / `@describe` are declared in stdlib `.ry` files (since #1390). C++ test harnesses (`runSource`, etc.) skip `ModuleLoader` entirely, so sources using these directives fail with `unknown directive '@inline'` — but only at **runtime**, not at build time. When moving a directive declaration to a stdlib `.ry` file, extending `withStdlibDirectiveDecls()` + applying it to existing tests + removing the registry entry must all land in **one commit** (a registry entry coexisting with a `.ry` declaration trips the build's collision check).

### Stdlib `@native` fns reachable from `runTestSource` need APPENDED inline decls (not prepended)

**Tags**: testing, codegen-test, stdlib, module-loader, harness, line-number, fail

`runTestSource` / `runTestSourceNoTestingImports` skip `ModuleLoader`, so `fail(…)` fails with `undefined function: fail` (since #718). **Prepending declarations is forbidden** — tests that assert line numbers silently shift and produce false passes. Append declarations after the source, as `withTestingFnDecls` does.

### Test loader-pipeline changes for AST variants with codegen no-op via `resolveImportsOnly` introspection

**Tags**: testing, module-loader, codegen-no-op, multi-pr-chain, blind-spot

In a multi-PR chain where an intermediate PR adds a loader/export but leaves codegen as a deliberate no-op, execution-based tests cannot distinguish "variant exists but is no-op" from "variant was filtered out". `EXPECT_THROW` fires at `module_loader.cpp:73-82` and bypasses variant-specific paths deeper in `extractDefinitions`, so it does not cover the no-op stage. Write introspection tests using `resolveImportsOnly()` + `std::holds_alternative<TheVariant>` to directly inspect the program.

### Use `-> Unit` in @it/@describe rejection tests to isolate the directive check

**Tags**: testing, directives, codegen-test

In rejection tests for `@it` / `@describe` return-type enforcement, using `-> int` / `-> bool` / `-> str` can cause "does not return a value on all code paths" to fire **before** the directive check, masking whether enforcement works. Using `-> Unit` matches the natural return type of `expect(...)`, ensuring the only path that throws is the directive enforcement.

### C++ `\xNN` hex escape consumes ALL following hex digits — never use `\xNNX` when X is a hex char

**Tags**: c++, test, nul-safe, string-literal

C++ string literal `\x` consumes all following hex characters as one escape. `"\x00a"` is not `NUL + 'a'`; it is `0x0a = '\n'` (one byte). In NUL-key identity tests, `"k\x00a"` and `"k\x00b"` both become the same key (`k\n`), causing tests to pass for the wrong reason. Use char-array initializers (`{'k', '\0', 'a'}`) or adjacent string literals (`"k\x00" "a"`).

### Thread-local HTTP error buffer is shared across tests in the same process

**Tags**: nul-safety, testing, http, thread-local

`http_last_error_buf` (`runtime_http_error.cpp`) is `thread_local` and persists for the thread's lifetime. If test A writes an error message and test B then fails without overwriting the buffer, test B's `e.message` will contain test A's stale message. Tests that assert error message content may produce different results depending on test execution order.

### Subprocess tests: pass the full path as argv[0] to `execl` (bare `"ry"` breaks stdlib resolution on Linux)

**Tags**: testing, subprocess, execl, fork, stdlib-resolution, linux, macos, blind-spot

`find_share_dir` in `src/project/paths.cpp` derives `exe_dir` from `fs::path(exe_path).parent_path()`. Passing bare `"ry"` as argv[0] makes `parent_path()` return `""`, which succeeds on macOS (returns CWD) but **fails on Linux glibc**, breaking exe-adjacent share lookup. When `RY_ENV=internal` is also set, `~/.ry/share` lookup is skipped too, producing `module not found: io`. This failure is Linux-only and does not reproduce on macOS — a non-symmetric trap. In `execl(RY_BINARY_PATH, ...)` calls, pass **the full path `RY_BINARY_PATH`** for argv[0] as well.
