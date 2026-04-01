# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Changed

- **Breaking**: Pattern matching syntax renamed from `when value:` to `match value:`; conditional `when:` (without subject) is unchanged (#482)
- **Breaking**: Anonymous function lambda form `function(...) => ...` is no longer supported; use parenthesized lambda syntax `(x: int) => x + 1` instead (#483)
- **Breaking**: Single-expression lambda syntax changed from `(params): expr` to `(params) => expr`; block lambdas `(params):\n  body` are unchanged (#498)
- **Breaking**: Self-update now requires Ed25519 signature verification by default; set `RY_SKIP_SIGNATURE=1` to opt out (#469)
- HTTP and JSON parsing hot paths now use pointer-based parsing to avoid unnecessary `substr` copies and temporary string allocations (#467)

### Added

- Match expression syntax with `=>` for single-expression arms, enabling `res = match x: case Some(v) => v case None => 0` — all pattern types (literal, variable, enum, Option, Result, OR, guard) are supported (#499)
- The `as` cast operator now accepts the full type syntax including generic types (e.g., `x as Option<int>`, `x as Map<str, int>`) (#490)
- Regex literal syntax (`/pattern/`) that produces a `Regex` type, enabling type-based overload resolution and UFCS-compatible function calls (e.g., `"hello".is_match(/[a-z]+/)`, `"a1b2".split(/[0-9]/)`) (#458)
- New text-first regex functions: `is_match`, `search`, `replace`, `split`, `find_all` — overloaded to accept `Regex` type patterns alongside existing string functions
- `print()` now accepts multiple arguments with space-separated output (e.g., `print(1, "hello", true)` → `1 hello true`), and calling `print()` with no arguments now prints only a newline
- `body_bytes()` function for `HttpRequest` and `HttpClientResponse` that returns `List<u8>`, enabling binary-safe HTTP body access without NUL-byte truncation (#284)

### Fixed

- Return-path analysis now recognizes exhaustive `match` statements on custom enums and `bool`, removing false "does not return a value on all code paths" errors when all variants are covered (#513)
- Indexing into `List`, `Map`, or `Set` fields of a record (e.g., `record.field[idx]`) no longer fails with "cannot determine list element type" (#511)
- `join()` now works correctly with UFCS string receiver (e.g., `",".join(parts)`) (#508)
- Closures returned from functions can now be called, and function-type parameters can be captured in closures — enabling higher-order patterns like `make_adder`, `compose`, and currying (#510)
- SEGFAULT when multiple functions use `match` on ADT enum parameters — `resolveType()` now correctly returns the ADT struct type instead of `i64` for enums with variant data (#507)
- `append!` / `appended` and other collection operations now work correctly on `List`, `Map`, and `Set` values returned from user-defined functions (#509)
- `operator as` codegen now supports generic target types (e.g., `int?`, `Result<int, Error>`), not just struct types (#501)
- Self-update tar validation now uses a whitelist approach, rejecting all archive entries that are not regular files or directories (device nodes, FIFOs, sockets, etc.) (#471)
- `print()` output inside `@parallel for` loops is no longer interleaved across threads — each `print()` call now produces atomic output via thread-local buffering (#473)
- Mocked functions now still enforce the original function's `require` and `ensure` contracts, preventing tests from bypassing contract checks (#441)
- Hardened codegen type promotion (`promoteToInt`/`promoteToFloat`) to reject struct and pointer types, preventing invalid LLVM IR from arithmetic, comparison, bitwise, and unary operators on non-numeric types (#394)
- Added null/allocation-failure guards in runtime functions (`runtime_io`, `runtime_net`, `runtime_path`, `runtime_regex`, `runtime_sort`) to prevent undefined behavior from null pointer dereference and integer overflow (#394)
- Cycle collector now generates visit functions for record (struct) types, enabling GC traversal of ARC pointer fields embedded in record types within ADT enum payloads (#432)
- Explicit resource free/close functions (`lock_free`, `close`, `json_free`, etc.) now decrement the ARC reference count instead of immediately freeing — aliased resources no longer cause use-after-free (#427)
- Closure destructors now recursively release captured resources and nested closures, preventing memory/resource leaks when closures are freed (#429)
- Variable reassignment now uses the full destructor resolver (covering resources and closures) instead of only resolving collection destructors
- Parser error message for unexpected tokens in statement position now says `unexpected token 'X'` instead of listing all valid keywords — also removes `expect` from keyword listing since it is a function, not a keyword (#404)
- Eliminated DNS rebinding TOCTOU gap in HTTP client SSRF protection — DNS is now resolved once and the same result is used for both the private-host check and the connection, preventing attackers from bypassing SSRF guards via DNS rebinding; also added IPv4-mapped IPv6 address detection (#470)
- HTTP response headers containing CR or LF characters are now silently skipped to prevent response splitting attacks (#472)

### Changed

- **Breaking:** Renamed the function declaration keyword from `fn` to `function`; legacy `fn` / `async fn` now produce migration errors with guidance
- Added concise Option A lambda syntax: `(x: int) -> int => x + 1` and `(x: int) => x + 1`
- **Breaking:** `args()` renamed to `arguments()` for command-line argument access (#111)
- **Breaking:** `recv()` renamed to `receive()`, `set_recv_timeout()` renamed to `set_receive_timeout()` for network operations (#111)
- **Breaking:** HTTP server functions simplified for UFCS: `http_method` → `method`, `http_path` → `path`, `http_header` → `header`, `http_body` → `body`, `http_query` → `query`, `http_query_all` → `query_all`, `http_cookie` → `cookie`, `http_cookies` → `cookies`, `http_form_field` → `form_field`, `http_form_file` → `form_file`, `http_form_fields` → `form_fields`, `http_response` → `response`, `http_listen` → `listen` (#208)
- **Breaking:** HTTP client accessor functions simplified: `http_client_status` → `status`, `http_client_body` → `body`, `http_client_header` → `header` (#208)
- **Breaking:** JSON functions simplified for UFCS: `json_type` → `kind`, `json_get` → `get`, `json_at` → `at`, `json_str` → `to_str`, `json_int` → `to_int`, `json_float` → `to_float`, `json_bool` → `to_bool`, `json_len` → `length`, `json_keys` → `keys` (#208)
- **Breaking:** IO functions simplified: `file_exists` → `exists`, `str_to_bytes` → `to_bytes` (#208)
- Expanded abbreviated parameter names in stdlib declarations: path (`a,b,c,d,p`), list (`n,f`), thread (`a`) (#111)
- Synced stdlib declaration files (`.ry`) with implementations: added missing `remove`, `take`, `tap` to `list.ry`, `remove` to `map.ry`, and corrected IO function return types to `Result` in `io.ry` (#454)
- **Breaking:** Stdin execution now requires explicit `-c` flag (`echo 'code' | ry -c`). Bare `ry` without arguments runs the `entry` file from `package.toml` instead of reading stdin (#443)

### Added

- Structured `--trace` / `--trace-out=PATH` CLI mode for machine-readable internal execution tracing as JSON Lines, covering parse/import/codegen/jit/runtime milestones plus function and branch events
- Restructured tutorials from 11 to 12 files: dismantled overcrowded `08-advanced.md` (14 topics) into focused chapters, added new `08-error-handling`, `10-concurrency`, and `12-building-a-project` tutorials, expanded `05-functions` with closures/default args/UFCS, `06-records` with ADT/operator overloading, `07-collections` with lazy iterators, and `02-variables-and-types` with f-strings/type casting. Each tutorial now includes "Why" explanations, exercises, and common mistakes (#444)
- Bare `ry` command runs the entry point file specified in `package.toml`, with `ry -- arg1 arg2` to pass arguments (#443)
- `--outline` option for `ry test`: prints the `describe`/`it` structure of test files without executing test bodies, useful for reviewing test organization at a glance (#442)
- Cycle collector for ARC: CPython-style trial deletion algorithm detects and reclaims circular reference chains that ARC alone cannot free. Includes `gc` stdlib package with `collect()`, `enable()`, `disable()`, `set_threshold()` API. Static analysis identifies potentially cyclic types at compile time — non-cyclic types have zero GC overhead (#417)
- ARC for closures: closures with captured variables are now ARC-managed — automatically freed when no longer referenced, with proper retain/release of captured ARC-typed variables (collections, resources, other closures) (#415)
- Copy-on-Write (CoW) semantics for collection types (List, Map, Set): shared collections are automatically deep-copied before mutation, preserving value semantics while avoiding unnecessary copies when the collection has a single owner (#414)
- ARC integration with resource types: `TcpStream`, `TcpListener`, `TlsStream`, `Lock`, `RWLock`, `Semaphore`, `Barrier`, `Thread`, `AtomicInt`, `AtomicBool`, `HttpRequest`, `HttpResponse`, `HttpClientResponse`, `JsonValue` are now automatically cleaned up when no longer referenced — deterministic RAII-style resource management via ARC destructors (#418)
- `weak` reference type for ARC: non-owning references that do not prevent deallocation, with atomic CAS-based upgrade to `Option<T>`, automatic scope cleanup, and pattern matching support (#416)
- `ignore_case` parameter for `contains()`, `starts_with()`, `ends_with()` — optional boolean (default `false`) enables ASCII case-insensitive matching
- ARC (Automatic Reference Counting) for collection types (List, Map, Set) and strings: automatic memory management via retain/release with scope-based cleanup, destructor generation for internal buffers, and immortal sentinel for global string constants (#413)
- ARC infrastructure: header layout (`{ strong_count, weak_count }`), `arc_alloc`/`arc_retain`/`arc_release` codegen primitives with Swift-style atomic switching support (#412)
- Relative imports: `from .helper import greet`, `from .utils import add`, `from . import add, sub` for importing relative to the current file's directory
- Auto-convert non-str operands to str in `+` concatenation: `"abc" + 2` produces `"abc2"`, `1 + "abc"` produces `"1abc"` (#393)
- Leading-dot float literals (e.g. `.5`, `.01`, `.5f64`) are now supported as shorthand for `0.5`, `0.01`, etc.
- Numeric underscore separators for improved readability: `100_000`, `0xFF_FF`, `0b1010_0101`, `3.14_159`
- Ed25519 signature verification for self-update artifacts to prevent supply-chain attacks (#124)
- `thread` package: native OS thread API with Thread, Lock, RWLock, Semaphore, Barrier, AtomicInt, AtomicBool (#363)
- `ry run` command to execute scripts defined in `package.toml` `[scripts]` section (#384)
- `path` standard library package with file path operations: `join`, `basename`, `dirname`, `extension`, `resolve`, `is_absolute` (#185)
- `filesystem` standard library package with file/directory manipulation: `list_dir`, `walk`, `glob_files`, `copy`, `move`, `remove`, `remove_all`, `make_dir`, `make_dir_all`, `file_size`, `is_file`, `is_dir`, `is_symlink`, `chmod`, `symlink`, `read_link` (#184)
- Runtime bounds checking for `char_at()` on strings — out-of-bounds access now raises a descriptive runtime error instead of silently returning an empty string (#395)
- Python-style negative index wrap-around for lists, arrays, and `char_at()` — e.g. `xs[-1]` accesses the last element (#395)
- Boundary clamping for `substring()` — out-of-range indices are clamped to `[0, length]` (#395)
- Descriptive runtime error messages for out-of-bounds access, including the actual index and collection length (#395)

### Changed

- Control-flow syntax now keeps `if`/`else`, removes `elif`, replaces `match` with `when value:`, and replaces ternary `?:` with `when:` expressions
- `char_at()` now uses a single-pass UTF-8 traversal for bounds checking and character extraction, eliminating a redundant full-string scan (#407)
- `ry new` / `ry init` now normalize hyphens to underscores in package names (e.g. `ry new my-app` creates `name = "my_app"` in package.toml)
- `.test.ry` files are excluded from directory package loading
- Fixed-length array type syntax changed from `[T; N]` to `T[N]` (e.g. `buf: i32[4] = [1, 2, 3, 4]`)

### Fixed

- Fixed float output examples in operator tutorial to match actual `%g` formatting (e.g. `1024` not `1024.0`)
- Added curl one-liner installer to Getting Started tutorial
- Fixed "struct" terminology to "record" across tutorial and reference docs
- Improved error message when hyphens are used in import paths (e.g. `from my-pkg import foo` now suggests using underscores)
- Binary operations between `str` and non-`str` types (e.g. `"abc" - 2`, `"abc" / 2`) now raise compile-time type errors instead of producing garbage output or LLVM IR verification errors (#396)
- `ry version` now works as an alias for `ry --version` instead of trying to execute the VERSION file on case-insensitive filesystems (#381)
- Dev Release nightly build fails due to missing dependencies (`openssl@3`, `ninja`, `googletest`) and removed schedule trigger (#380)

## [0.0.5] - 2026-03-28

### Added

- Low-level numeric types: i8, i16, i32, i64, u8, u16, u32, u64, f32 (#288)
- Numeric literal suffixes e.g. `42i32`, `3.14f32` (#289)
- Unsigned negation check — reject unary `-` on unsigned types (#312)
- `any` type with runtime dispatch, implicit conversion, and wrap/unwrap (#216, #219, #220, #221, #222, #223, #224, #225, #226, #227, #228)
- Return type inference for named functions when annotation is omitted
- `Result<V, E>` type for null-safe error handling (#104)
- `?` operator for Result error propagation (#176)
- `ensure` variable binding and remove `result`/`old` keywords (#105)
- Generic functions with type parameters (#210)
- Nested type parameter parsing (`>>`) (#263)
- Record auto-generated `operator==` and `operator!=` (#305)
- Record auto-generated `to_str` (#306)
- Record subtyping with `<` syntax for field inheritance and subtype coercion (#307)
- Record invariant inheritance: parent `invariant:` clauses are checked on child construction (#355)
- Auto-slice Error subtypes in `Err()` for Result return type coercion (#354)
- Subtype coercion for field assignment (#359)
- Subtype coercion for `?` error propagation operator (#360)
- Generic type constraints with record bounds (`<T: RecordName>`) (#297)
- `@inline` directive for function inlining hints (#299)
- Explicit value assignment for simple enum variants (#309)
- Named fields in ADT enum variants (#308)
- Subscript operator overloading `operator[]` / `operator[]=` with multi-index support (#202)
- Membership operator overloading `operator in` for user-defined types (#202)
- Call operator overloading `operator()` for callable records (#202)
- Cast operator overloading `operator as` for user-defined type conversions (#202)
- Tail call optimization (TCO) for self-recursive functions via LLVM `musttail` (#214)
- Compound assignment operator overloading with in-place optimization (#204)
- Enforce bool return type for comparison and logical operator overloads (#203)
- N-element tuple destructuring in for loops (#302)
- Implicit widening conversion in overload resolution (#212)
- `json` standard library package — parse/stringify with opaque JsonValue type (#179)
- `base64` standard library package (#183)
- TCP socket timeouts and TLS/SSL support (#76, #77)
- HTTP client functionality: `http_get`, `http_post`, `http_request` (#129)
- HTTP cookie parsing: `http_cookie`, `http_cookies` (#128)
- HTTP query parameter parsing: `http_query`, `http_query_all` (#127)
- HTTP chunked transfer encoding (#164)
- HTTP multipart/form-data parsing for server (#82)
- HTTP `max_requests` parameter for `http_listen` shutdown control (#165)
- Comprehensive HTTP status code reason phrases per RFC 9110 (#119, #125)
- `.env` file auto-loading and `env()` built-in function (#158)
- `RY_ENV` environment variable and `--env` CLI flag (#159)
- `.env` / `RY_ENV` integration with short aliases and environment-specific files (#171)
- `sleep(duration_ms: int)` built-in function (#146)
- `ry fmt` command for code formatting (#151)
- `ry new <project-name>` command (#149)
- `ry test --coverage` for line coverage measurement (#166)
- `ry test --watch` for auto-rerunning tests on file change (#163)
- Parallel test execution with `-p` / `--parallel` flag (#147)
- `--help` / `-h` option support for commands and subcommands (#337)
- HTTP keep-alive support for `http_listen` server (#79)
- Stdin execution via pipe and here-document (#250)
- `fail()` helper in test framework (#177)
- HTTP automatic redirect following for client requests (#148)
- Self-update artifact checksum verification (#116)
- Linux x86_64 (amd64) release build in CI (#154)
- Linux ARM64 (aarch64) release build in CI (#155)
- `block_on(task)` built-in function for synchronous Task waiting (#206)

### Changed

- Default return type changed from `Unit` to `any` when omitted (#218)
- Allow omitting parameter type annotations (defaults to `any`) (#217)
- Lambda expression syntax changed from `:` to `=>` (#301)
- Flatten stdlib imports — `from std.x` to `from x` (#178)
- Rename `ry.toml` to `package.toml` (#335)
- Restrict `await` to `async fn` context only — use `block_on()` in synchronous code (#206)

### Fixed

- Set literal now deduplicates elements at construction time — `{1, 2, 3, 2, 1}` correctly has length 3 (#376)
- Repo-built `ry` now prefers the checked-out stdlib over stale `~/.ry/lib/std`, restoring `base64`, `json`, and `net` timeout imports during language development (#367, #370)
- Floor division (`//`) now uses correct floor semantics instead of truncation (#239)
- Zero-division guards for integer `//` and `%` operators (#242)
- NaN comparison aligned with compiler's ordered semantics (#240)
- Require return on all code paths for non-Unit/any functions (#209)
- HTTP body NUL byte truncation (#281)
- Filter hop-by-hop headers in HTTP client requests (#280)
- `repeat()` type check and n<=0 guard (#272)
- ConstantInt metadata corruption from LLVM sharing (#311)
- Wrap value in `any` on reassignment to any-typed variable (#232)
- Reject non-str pointer types in `any` to prevent mistagging (#233)
- Overload ranking prefers concrete types over `any` (#252)
- OR pattern binding check — reject bindings but allow wildcards (#139)
- HTTP client response resource type tracking (#140)
- Directive move-only semantics to prevent silent expr loss (#102)
- Memory leak in `@property` test random strings (#100)
- UTF-8 `utf8_char_len_safe()` buffer overread (#99)
- TCP partial write handling (#114)
- TCP `recv` buffer freed on error (#115)
- TCP error handling unified to return Result instead of `exit(1)` (#120, #123)
- Truncated HTTP request body rejection (#117)
- `ry fmt` crash, `join()` arg mismatch, and multiple formatter bugs (#162)
- `ry fmt` duplicate blank line before section comments (#167)
- `ry fmt` round-trip verification to prevent code destruction (#168)
- `!` suffix restricted to function names only (#156)
- Nested stdlib modules copied recursively during self-update (#112)
- Self-update mandatory checksum verification and hardlink rejection (#126)
- Test timeout applied per `it`-block instead of per file (#333)

### Removed

- Concurrency primitives: channels, spawn, select, task_group, cancel (#304)
- `byte` type in favor of `u8` (#294)
- `join(task)` built-in — replaced by `block_on(task)` (#206)

## [0.0.4] - 2026-03-22

### Added

- Improved builtins — UTF-8, Option returns, mutating variants (#44)
- 9 new test matchers and extended existing ones (#46)
- `take` and `tap` list builtins (#47)
- Increment/decrement operators (`x++`, `x--`) (#48)
- Regex phase 2 — range quantifiers and non-greedy matching (#49)
- Lazy iterator abstraction (#50)
- Word boundary `\b`/`\B` and case-insensitive `(?i)` flag (#51)
- Concurrency primitives: spawn/await, channels, select, `@parallel for` (#54)
- `@each` / `@property` test directives (#57)
- `std.math` package (#58)
- `@native let` constants and `_`-prefix private symbols (#59)
- `std.io` module with file I/O, stdin, and byte operations (#60)
- TCP socket API for HTTP server foundation (#61)
- HTTP server API (#62)
- Directory path argument support in `ry test` (#64)
- Stable TimSort via C++ runtime replacing QuickSort (#52)

### Changed

- Replace `let`/`var` with Python-style assignment and `@const` directive (#75)

### Fixed

- Socket timeouts to prevent test hangs (#95)

## [0.0.3] - 2026-03-20

### Added

- `>>>`, string `*`, and `not in` operators (#10)
- `filter`, `map`, `sort` stream-like operations for lists (#11)
- Design by Contract support (#15)
- Directive support with `@deprecated` (#16)
- f-string, `as` cast, and `Result<T, E>` (#18)
- Compound assignment operators, `in`/`not in` for list/map, and `range()` step (#19)
- r-string (raw string) support (#20)
- Ternary operator, match OR pattern, list operations (#21)
- Lambda (`fn`), tuple destructuring, enum ADT, generic enum, collection ops (#22)
- `record` keyword, type alias, operators, naming enforcement, and collection ops (#23)
- `args()` and `exit(code)` built-in functions (#24)
- `@native` directive for built-in function declarations (#25)
- Collection functions: `remove`, `distinct`, `flatten`, `merge` (#26)
- Literal types and range types (#28)
- Function type aliases (#29)
- Generalized trailing block syntax, demoted `describe`/`it` to functions (#30)
- For-loop tuple destructuring and `@native` stdlib prelude (#31)
- `ry test` auto-discovery and removed `test_dir` (#32)
- Built-in Error type and `!!` operator replacing `Result<T, E>` (#33)
- Rust-style rich error messages (#35)
- Directory-based package system with std library (#36)
- NFA-based regex engine (Phase 1) (#37)
- `...` (ellipsis) no-op statement (#38)
- Mock/verify support in test framework (#39)
- Ry self-tests (#41)

### Changed

- Require type annotation for `none` and remove `unwrap()` (#34)

### Fixed

- Short-circuit eval, FnScope contract protection, lexer safety (#40)
- Three compiler bugs found during self-test development (#41)
- Self-update repo name and missing releases handling (#3)

## [0.0.2] - 2026-03-14

### Added

- `ry self-update` command (#1)

## [0.0.1] - 2026-03-14

Initial release.

[Unreleased]: https://github.com/t0k0sh1/ry/compare/v0.0.5...HEAD
[0.0.5]: https://github.com/t0k0sh1/ry/compare/v0.0.4...v0.0.5
[0.0.4]: https://github.com/t0k0sh1/ry/compare/v0.0.3...v0.0.4
[0.0.3]: https://github.com/t0k0sh1/ry/compare/v0.0.2...v0.0.3
[0.0.2]: https://github.com/t0k0sh1/ry/compare/v0.0.1...v0.0.2
[0.0.1]: https://github.com/t0k0sh1/ry/releases/tag/v0.0.1
