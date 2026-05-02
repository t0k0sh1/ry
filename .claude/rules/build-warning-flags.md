---
paths:
  - "CMakeLists.txt"
  - "cmake/**/*.cmake"
  - ".clang-tidy"
  - ".cppcheck-suppressions"
  - ".github/workflows/*.yml"
---

# Build Warning Flags

### Compiler warning flags require SYSTEM includes for third-party headers

**Source**: #895 (implementation)
**Tags**: build, cmake, warnings, llvm, googletest, system-include

**Context**: When `-Wall -Wextra -Wpedantic -Wconversion -Wshadow` are
enabled on internal targets, LLVM and GoogleTest headers produce hundreds
of warnings (implicit conversions, unused parameters, non-standard
extensions, etc.) that are not actionable.

**Rule**: LLVM include directories must be added via
`target_include_directories(... SYSTEM PUBLIC ${LLVM_INCLUDE_DIRS})`,
not via the global `include_directories(${LLVM_INCLUDE_DIRS})`. The
global form does not support SYSTEM and causes third-party warnings to
leak into the build output. GoogleTest uses the `SYSTEM` keyword in
`FetchContent_Declare` (requires cmake 3.25+). The project's own
`include/` directory must NOT be marked SYSTEM — warnings on our own
headers are intentional.

Warning flags are stored in the `RY_WARNING_FLAGS` CMake variable and
applied per-target via `target_compile_options(... PRIVATE ...)` so they
do not leak into FetchContent targets. The `add_ry_native_lib()` helper
applies them automatically to all native shared libraries.

### Clang-Tidy check selection and __ry_ naming convention

**Source**: #893 (implementation)
**Tags**: build, ci, clang-tidy, static-analysis, naming

**Context**: The `.clang-tidy` config disables several checks that
produce false positives in this project. Key decisions:

- `bugprone-reserved-identifier` / `cert-dcl37-c` / `cert-dcl51-cpp`:
  All runtime functions use `__ry_*` prefix (double underscore = reserved
  in C++). This naming is intentional for ABI and FFI reasons — do not
  rename them, disable the check instead.
- `cert-err58-cpp`: `RY_REGISTER_STDLIB_PACKAGE` macro creates static
  initializers. This is the standard pattern for self-registering
  stdlib modules. (The macro name itself is preserved verbatim — it is
  the existing C++ symbol; only the prose around it uses the new
  v0.0.17 "module" terminology.)
- `cert-dcl50-cpp`: C-style variadic functions are used intentionally
  in runtime error formatting helpers.
- `performance-enum-size`: Internal enum base types are often
  intentional (ABI stability, JIT layout constraints).
- `performance-no-int-to-ptr`: Incompatible with LLVM IR builder
  patterns.
- `bugprone-multi-level-implicit-pointer-conversion`: C-style
  `free(ptr)` where `ptr` is `T**` is idiomatic in the runtime's
  manual memory management. This check fires on clang-tidy 21+ only.
- `cert-err33-c`: `snprintf`/`vsnprintf` return values are not
  meaningful in formatting-only contexts (error message buffers,
  string formatting). `std::atexit` failure is also not actionable.

**Rule**: Do not re-enable these checks without understanding why they
were disabled. If adding a new disabled check, document the reason in
the `.clang-tidy` comment header.

### Clang-Tidy: NOLINT patterns for intentional code

**Source**: #935 (implementation)
**Tags**: build, ci, clang-tidy, static-analysis

Some clang-tidy warnings are intentional patterns that should be
suppressed with `// NOLINT(...)` rather than refactored:

- **`performance-unnecessary-value-param`** on sink parameters
  (`std::string name` → `std::move(name)`): This is correct C++ sink
  idiom. Changing to `const std::string &` would break move semantics.
  Affected: `TypeNode::make*` factory methods in `ast.hpp`,
  `SourceManager::addSource` in `source_manager.hpp`.
- **`bugprone-empty-catch`** on shutdown/cleanup paths:
  `worker.join()` in `runtime_parallel.cpp` and stdlib-load try/catch
  in `jit_runner.cpp` intentionally swallow exceptions because join
  failure on thread shutdown is non-recoverable, and stdlib absence is
  expected. Add a brief justification comment alongside NOLINT.

**Rule**: When using NOLINT, always include the specific check name and
a one-line comment explaining why the suppression is justified.

### Cppcheck: suppression strategy and known false positives

**Source**: #894 (implementation)
**Tags**: build, ci, cppcheck, static-analysis

Cppcheck is run in the `lint` CI job without `compile_commands.json` (build-free,
fast). This means project macros defined in headers are not visible to Cppcheck,
causing `unknownMacro` false positives.

Known suppressions in `.cppcheck-suppressions`:

- **`unknownMacro`** (global): `RY_REGISTER_STDLIB_PACKAGE` and `DEFINE_LAST_ERROR`
  are project macros not resolved without `compile_commands.json`. Suppressed globally
  because running without the compilation database is intentional (avoids requiring a
  full build in the `lint` job).
- **`syntaxError:src/test_runtime.cpp`**: `__has_feature(...)` is a Clang compiler
  builtin predicate, not a function-like macro. Cppcheck cannot evaluate the
  `__has_feature(address_sanitizer)` idiom and emits a spurious `syntaxError`.

**Rule**: When adding a new suppression to `.cppcheck-suppressions`, always include
a comment explaining whether it is a false positive (and why) or a known acceptable
deviation. Use per-file suppressions (`id:src/file.cpp`) rather than global ones
where possible.

**Gotcha**: Cppcheck 2.13 (Ubuntu 24.04 package) does NOT support `#` comment lines
in `--suppressions-list` files. Comment syntax was added in 2.14. Keep
`.cppcheck-suppressions` comment-free to remain compatible with 2.13.

### Clang-Tidy: `performance-inefficient-string-concatenation` — error メッセージで `+` chain を使わない

**Source**: #1404 (2026-04-27, motivated by PR #1403)
**Tags**: build, clang-tidy, static-analysis, performance, string-concatenation

**Context**: PR #1403 が CI の `clang-tidy` ジョブで失敗した。原因は `src/directive_meta.cpp` の throw メッセージで `std::string operator+` のチェーンを使っていたこと。`performance-inefficient-string-concatenation` は `.clang-tidy` で `performance-*` ファミリー経由で有効化されており、`a + b + c + d` のような連鎖が各ステップで一時 `std::string` を heap-allocate することを警告する。

**Rule**: `throw std::runtime_error(...)`、`codegenError(...)`、その他のエラーメッセージ構築で 3 つ以上の文字列を `+` で連結してはならない。代わりに宣言-代入 + `+=` のチェーンを使う:

```cpp
// 禁止 (clang-tidy が performance-inefficient-string-concatenation で失敗)
throw std::runtime_error(
    "unknown named argument '" + *a.name +
    "' for directive '@" + directiveName + "'");

// 正しい
std::string msg = "unknown named argument '";
msg += *a.name;
msg += "' for directive '@";
msg += directiveName;
msg += "'";
throw std::runtime_error(msg);
```

**Why**: `a + b + c + d` は `(((a + b) + c) + d)` と評価され、各 `operator+` で新規 `std::string` を heap-allocate する。`+=` は in-place で再割り当てを最小化する。エラーメッセージのような頻度が低い箇所でも clang-tidy は警告するため、ローカル検出が必須。

**How to apply**:
- エラーメッセージは `std::string msg = "プレフィックス";` で初期化、残りを `msg += var; msg += "テキスト";` で構築
- 2 つ連結 (`"prefix " + var`) は警告対象外のことが多いが、3 つ以上は常に `+=` を使う
- canonical 例: `src/codegen_expr_literal.cpp:454-458` (set literal 型不一致エラー)、PR #1403 で修正された `src/directive_meta.cpp:127-158`

**Check name**: `performance-inefficient-string-concatenation` (`.clang-tidy` で `performance-*` ファミリー経由で有効)

### Zero-warnings policy and `-Werror` status

**Source**: #1498 (migrated from AGENTS.md, 2026-05-02)
**Tags**: build, cmake, warnings, zero-warnings, werror

**Rule**: New code must maintain zero compiler warnings under the
`-Wall -Wextra -Wpedantic -Wconversion -Wshadow` flag set. Warnings
in LLVM / GoogleTest headers are suppressed via `SYSTEM` includes and
do not count.

`-Werror` has not been introduced yet (tracked in a separate issue).
The flag set applies to internal targets (`ry_lib`, `ry`, `ry_tests`,
native libs); see the sibling entry "Compiler warning flags require
SYSTEM includes" for the `RY_WARNING_FLAGS` mechanism.
