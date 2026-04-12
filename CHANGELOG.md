# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

## [0.0.9] - 2026-04-12

### Changed

- Documented that `share/std/math/math.ry` intentionally uses bare `@native` (no argument) because math functions have no separate shared library (#907)
- `include/ry/codegen.hpp` uniform closure comment now documents all three struct fields: `{thunk_ptr, env_ptr, env_dtor_ptr}` (#907)
- `include/ry/ry_layout.hpp` `ARC_HEADER_SIZE` derived from `sizeof(int64_t) * 2` instead of hardcoded `16` (#907)
- `AGENTS.md` wip-label timing unified to post-merge rule; constant registration guidance now cites header path (#907)
- `.cursorrules` stdlib CMake step now distinguishes `ry_lib` (codegen) from `add_ry_native_lib` (runtime) targets (#907)
- `.claude/skills/git-commit/SKILL.md` `allowed-tools` now includes `git diff` and `git log` (#907)
- `.claude/skills/git-resolve-conflicts/SKILL.md` (+ `.codex` mirror) verifies `headRefName` before merging base branch (#907)
- `.claude/skills/git-branch-naming/SKILL.md` removes `style` branch type; branch creation is now conditional (#907)
- `.claude/skills/git-fix-pr-reviews/SKILL.md` generalizes nitpick marker detection and parameterizes reviewer handle (#907)
- `.codex/skills/git-search-issues/SKILL.md` adds deterministic precedence rule for ambiguous inputs (#907)
- `.codex/skills/git-triage-issue/SKILL.md` scopes autonomous action to Cases 1-2 only (#907)
- `.claude/skills/git-merge-pr/SKILL.md` (+ `.codex` mirror) uses dynamic default branch detection instead of hardcoded `main` (#907)
- `.codex/skills/git-switch-branch/SKILL.md` handles local branches without upstream (#907)
- `.github/workflows/release.yml` native-lib glob uses `nullglob` + array check for diagnosable errors (#907)

### Fixed

- `install.sh` now fails with a clear error when the release archive does not contain a standard library at `share/std` or `lib/std`, instead of silently installing a broken `ry` that crashes at runtime (PR #901 review)
- `arc_alloc` now guards `ARC_HEADER_SIZE + data_size` against integer overflow via `__builtin_add_overflow`, preventing an undersized heap allocation followed by out-of-bounds writes if `data_size` is near `SIZE_MAX` (PR #901 review)
- ADT example in control-flow reference used `Shape::Rect` instead of `Shape::Rectangle` (#907)
- Concurrency tutorial incorrectly implied `send`/`receive`/`close` are `net` module exports; clarified they are language builtins (#907)
- `to_str` signature in builtins-string reference now matches the supported-types table (`any` instead of a restricted union) (#907)
- `docs/zh/reference/operators.md` described `else =>` as required but examples used `_ =>`; unified to `_ =>` (#907)
- `docs/tutorial/11-testing.md` `test_should_handle_error` example had uninitialized `result` variable (#907)
- `docs/reference/functions.md` mutual recursion description now correctly states forward-declaration applies to nested functions too (#907)
- Typo "overloads case a call" corrected to "overloads match a call" in `docs/reference/functions.md` (#907)
- Traditional Chinese leftovers (`字串`/`巢状`/`缩排`) normalized to Simplified Chinese in `docs/zh/tutorial/11-testing.md` (#907)
- "match statements/expressions" terminology in `docs/zh/reference/control-flow.md` updated to `case` (#907)
- Code fences for `@each`/`@property` in testing reference now include `ry` language identifier (markdownlint MD040) (#907)
- Heading "Handling Results with match" corrected to "with case" in `docs/tutorial/08-error-handling.md` (#907)

## [0.0.8] - 2026-04-12

### Added

- Systematic combinatorial test coverage in `tests/spec/combinatorial/` (#628): 113 tests across 9 files covering type×operation matrix (equality, fn argument/return, collection element, match, nested types, syntax combinations, print/display, stdlib boundary inputs)
- `@it("description")` directive on named functions: test cases can now be defined as ordinary named functions decorated with `@it` (#634)
- `@describe("group")` directive on named functions: test groups can now be defined as ordinary named functions decorated with `@describe` (#635)
- `@each` and `@property` directives compose with `@it` on named functions for parameterized and property-based tests (#634)
- Shared setup in `@describe`: variables declared in a describe function body are automatically captured by inner `@it` functions (#635)
- Nested `@describe` output indentation: test output is now indented proportionally to nesting depth (#635)
- `NativeFnSignature` registry that captures full type information (parameter names/types, return type, package) from `@native` function declarations (#646)
- Documented the `__ry_<pkg>_<name>` native function naming convention (#646)
- `@native("libname")` directive syntax for specifying shared library module names (#647)
- Dynamic library loading for `@native("libname")` declarations — the JIT now loads shared libraries at startup (#649)
- Stdlib runtime packages are built as shared libraries (`.dylib`/`.so`) in addition to the existing static linking (#649)
- Nested named functions now obey lexical scoping: they are visible only within their enclosing function and do not collide with same-named functions in sibling scopes (#660)
- Nested named functions can now capture variables from enclosing scopes, behaving as closures just like lambdas (#661)
- `==` and `!=` operators now work for `List<T>`, `Set<T>`, `Map<K,V>`, `Result<T,E>`, and union types (#725)
  - List: element-wise comparison (supports `int`, `float`, `str`, `bool` elements)
  - Set: unordered equality — `{1,2,3} == {3,2,1}` is `true`
  - Map: key/value equality — maps with the same key-value pairs are equal regardless of insertion order
  - Result: compares `is_ok` flag and the inner `Ok` or `Err` value
  - Union (`A|B`): compares tag (variant kind) first, then the inner value for matching tags
- `ry` and `ry test` can resolve a bare `*.ry` filename (e.g. `ry main.ry`) when the file is not in the current directory: the project root is tried first, then each `[paths]` directory in key order; the first match wins (#741).
- `?` operator now accepts `Option<T>` operands in addition to `Result<T, E>`. When used on a `Some(v)` it evaluates to `v`; when used on a `None` the enclosing function returns `None` early. The enclosing function must declare an `Option` return type. `!!` is an alias with identical semantics. (#795)
- `??` operator now accepts `Result<T, E>` on the left-hand side in addition to `Option<T>`. For `Ok(v)` it evaluates to `v`; for `Err(_)` it evaluates to the right-hand default (the error value is discarded). (#796)
- `?` / `!!` can now be used directly at the top level of a script. When the operand is `Err(e)` or `None`, the error message is written to stderr and the process exits with status `1`. `__ry_main__`'s existing return-type contract is unchanged. (#745)
- `for c in s:` now iterates a string character by character, yielding each UTF-8 code point as a single-character `str`. `enumerate(s)` and `zip(s, t)` also accept `str` arguments with the same semantics. (#746, #827)
- `type_of(expr)` built-in function that returns a `Type` value representing the compile-time type identity of its argument. Supports `==` / `!=` for identity-based comparison and is printable via `print` / `to_str`. Covers primitives, low-level numeric types, collections (`List`, `Map`, `Set`), records, enums, `Option`, `Result`, functions/closures, `None`, and `Type` itself (reflective) (#793)
- `Type` primitive type representing the compile-time identity of a Ry type. Each distinct type definition receives a unique identity, so different records (or a record and an enum sharing a name) are always distinguishable by `==` (#793)
- `case` statement and expression unify `when` (conditional branching) and `match` (pattern matching) into a single construct (#799). Two forms are supported: `case:` for multi-branch conditionals without a subject (replaces `when:`) and `case <expr>:` for pattern matching with a subject (replaces `match`). Both forms support a block body (`:`) and a single-expression body (`=>`). Use `_` as the wildcard/default arm instead of `else`.
- `if` expression syntax for two-branch conditional values (#798). Supports both a single-expression form (`if cond => true_value else false_value`) and a block form (`if cond: body else: body`) with tail-expression semantics. For multi-branch expressions, use `case:` instead.
- Scientific notation float literals (`1e10`, `1.5e-3`, `2.5E+2`, `1_000e3`). Overflowing exponents (`1e400`) produce `+Inf` to match the runtime `to_float` converter (#819)
- `math.round(x, digits)`, `math.floor(x, digits)`, and `math.ceil(x, digits)`
  overloads for rounding a `float` to a given number of decimal places,
  returning a `float`. Negative `digits` rounds to powers of ten
  (`round(1234.5, -2) == 1200.0`). The two-argument forms reuse C99
  half-away-from-zero semantics so the result matches the one-argument
  `round()` applied to the scaled value — note this differs from Python's
  banker's rounding (`round(2.675, 2) == 2.68`, not `2.67`). `NaN` and `±Inf`
  pass through unchanged. (#842)
- `math.log(x, base)` overload for computing a logarithm with an arbitrary
  base, defined as `log(x) / log(base)`. Domain errors on either argument
  propagate as `NaN` or `-Inf`. (#842)
- `math.pow(x, y)` overload for `(int, int) -> int` using fast-exponentiation
  (O(log y)). A negative exponent raises a runtime error
  (`pow() integer exponent must be non-negative`). Overflow wraps silently,
  matching Ry's existing integer arithmetic model. (#842)

### Changed

- Captured variables in closures are now effectively final — reassignment inside the closure body produces a compile error (#213)
- `print()` now delegates to `to_str()` for all type formatting, ensuring consistent output between `print()`, `to_str()`, and f-string interpolation (#616)
- All C runtime memory allocations now use OOM-safe wrappers (`checked_malloc`, `checked_strdup`, etc.) that abort with a clear message instead of silently returning NULL (#631)
- Integer overflow checks added to array-size calculations in hash table rehash, UTF-8 reverse, and JSON parser (#631)
- CI now enforces a lint check that blocks raw `malloc`/`realloc`/`strdup` in new code (#631)
- `describe()` and `it()` lambda call syntax is deprecated; use `@describe("name")` and `@it("name")` directives on named functions instead (#635)
- Stdlib source files moved from `lib/std/` to `share/std/` following Unix FHS conventions (#645)
- Refactored math, io, json package dispatch to use table-driven native call dispatch (#650)
- Stdlib native dispatch migrated to table-driven architecture for net, http, and thread packages (#651)
- Stdlib `.ry` declarations updated from `@native` to `@native("libname")` for dynamic library resolution (#651)
- Stdlib runtime implementations separated from the static compiler library into shared libraries (#651)
- Directive invocation syntax is now generalized: all directives use a unified argument model supporting positional arguments, named arguments, and mixed forms (e.g. `@it("description")`, `@describe("group")`, `@property(count=100)`)
- Built-in directive signatures are now defined in a registry (`DirectiveSignature`) with allowed argument shapes and target kinds, enabling consistent validation and future user-defined directives (#663)
- Migrated all test descriptions (`it()` / `@it()`) to "should-style" wording for natural "it should ..." readability in test output and `--outline` mode (#664)
- Added test description style guideline to `docs/reference/testing.md` (#664)
- Stdlib package dispatch now uses self-registering pattern instead of X-macros; adding a new stdlib package with custom codegen no longer requires modifying core compiler headers (#674)
- Resource type tracking is now dynamic via `ResourceKindRegistry` instead of a hardcoded enum; new opaque resource types can be added without modifying `codegen.hpp` (#674)
- Error messages for `?` and `??` operator misuse now mention both `Option` and `Result` in the offending context.
- String elements inside collections (`List`, `Set`, `Map`, `Array`, `Tuple`, record) are now wrapped in double quotes when displayed via `print()` or `to_str()`, following Rust's debug display convention. Empty strings are now visible: `[""]` instead of `[]` (#756)
- `to_float(str)` now returns `Result<float, Error>` instead of `float`, matching the shape of `to_int(str)`. Invalid input previously returned `0.0` silently; it now returns `Err(Error(...))`. Empty strings, non-numeric content, and out-of-range values are reported as errors. **Breaking change**: existing code must unwrap the `Result` (e.g., via `case` or `?`). (#806)
- Assigning to a top-level mutable `let` from inside a function now writes through to the top-level binding instead of silently shadowing it with a new local. Code that relied on the old shadowing behavior must rename the inner variable explicitly (#817)
- `remove_at(values: List<int>, index: int)` in `share/std/list.ry` is now declared to return `int` instead of `Unit`, matching both the runtime implementation and the existing `collections.test.ry` expectations (#889)

### Removed

- Legacy `native_fn_arg_counts_` dispatch guard replaced by `native_fn_sigs_` (#651)
- Removed dedicated codegen dispatch files for base64, filesystem, and gc packages (now handled by generic native dispatch) (#651)
- **Breaking**: The `when` and `match` keywords have been removed (#800). Legacy code using these keywords must migrate to `case`. Migration table:
  | Before | After |
  |---|---|
  | `when:` | `case:` |
  | `match value:` with `case pattern:` arms | `case value:` with bare `pattern:` arms |
  | `else:` / `else =>` inside `when` arms | `_:` / `_ =>` |

### Fixed

- `to_str()` on ADT enums with associated data now correctly formats all field types (previously only supported int, float, str, bool) (#616)
- `@parallel for` no longer corrupts captured `List` / `Map` / `Set` / `str`
  values. Worker-local ARC retain/release on captured collections now uses
  atomic operations, captured allocas are re-marked as ARC-managed inside the
  thunk, and every ARC-managed capture is retained at worker entry so the
  copy-on-write `strong_count > 1` invariant holds — preventing workers from
  mutating the shared buffer in place (which previously caused heap corruption
  under contention). (#630)
- `emitCowCheck` now uses an Acquire atomic load for `strong_count` in an
  atomic context, pairing with the `atomicrmw` retain/release and closing a
  TOCTOU race window that TSan flagged when multiple workers CoW-copied the
  same captured collection. (#630)
- `runtime_gc.cpp::collect_locked()` now reads and writes `strong_count` via
  `__atomic_load_n(ACQUIRE)` / `__atomic_store_n(RELEASE)` so garbage
  collection no longer races with concurrent ARC retain/release performed by
  `@parallel for` workers. (#630)
- `ExpectStmt` was not scanned during free-variable analysis, preventing closure capture of variables referenced in `expect(x).to_eq(...)` assertions inside nested `@it` functions (#635)
- Installed `ry` binary no longer crashes with `dyld: Library not loaded` when using native packages (#659)
- Native shared libraries are now included in release and nightly distribution tarballs (#659)
- `self-update` now installs native shared libraries alongside the binary and stdlib (#659)
- Broadened SSRF private address filter to block carrier-grade NAT (`100.64.0.0/10`), benchmarking (`198.18.0.0/15`), multicast (`224.0.0.0/4`), reserved (`240.0.0.0/4`), IPv6 unspecified (`::`), and IPv6 multicast (`ff00::/8`) (#667)
- Added error handling for `fcntl` failure when restoring blocking mode after non-blocking connect (#667)
- Passing a capturing closure as a `function(...)` argument no longer crashes (#688)
- Directive arguments now support compound expressions such as function calls and binary operators (`@each(make_inputs())`, `@foo(x + 1)`) (#694)
- Unknown or invalid directive arguments on `record`, record fields, variable assignments, and `for` loops now produce a compile-time error, consistent with how function directives are validated (#696)
- Option equality (`==` / `!=`) now correctly compares inner values when both operands are `Some`, instead of comparing only the `has_value` flag (#726)
- Element type metadata is now preserved when accessing elements of `List<Map<K,V>>`, `List<Set<T>>`, and `List<closure>` by index or in a `for` loop (#727)
  - `xs[0]["key"]` on `List<Map<str, int>>` now works correctly
  - `for m in xs: m["key"]` on `List<Map<str, int>>` now works correctly
  - `xs[0]` on `List<Set<int>>` supports the `in` operator
  - Closures stored in a list (`fns[0](arg)`) are now callable after retrieval
- `print()`, `to_str()`, and f-string interpolation now work with closure values — they produce `"<closure>"` instead of a compile-time error (#728)
- Parser no longer crashes on out-of-range integer literals such as `9223372036854775808` (INT64_MAX + 1); a clear compile error is reported instead (#729)
- Missing explicit paths to `*.ry` files (e.g. `ry src/missing.ry`) now report **no such file** instead of unknown command (#741).
- `package.toml` `[paths]` entries (other than `src`) round-trip through `serialize`/`load` (#741).
- Fixed SEGFAULT when calling a two-level nested function return with type annotation (#752)
- f-string interpolation inside closures now correctly captures outer variables (#753)
- Integer division by zero (`1 / 0`) now raises a runtime error instead of returning `inf` (#754)
- Lambda expressions returning pointer types (f-string, record `str` field, string concatenation, cast to `float`) no longer cause IR verify errors (#755)
- Return type inference now correctly handles local variables instead of falling back to `int` (#770)
- "return type mismatch" errors now show expected and actual types
- `Any`-typed string values inside collections are now displayed with double quotes, consistent with statically-typed strings (#771)
- Double quotes and backslashes inside strings are now escaped when displayed in collections (#772)
- Sprint buffer depth overflow now aborts with a clear error message instead of silently corrupting output (#773)
- Closure capture analysis now handles `CastExpr`, `WhenCondExpr`, `MatchExpr`, `RangeExpr`, `ErrorPropagateExpr`, `AwaitExpr`, `WeakExpr`, and `SetExpr`, preventing "undefined variable" errors when these expression types reference captured variables (#776)
- Match/when pattern bindings are now correctly excluded from closure capture analysis, preventing incorrect capture of outer variables with the same name (#779)
- Low-level integer types (`i32`, `u8`, etc.) now raise a runtime error on division/modulo by zero instead of causing undefined behavior (#783)
- Expression-bodied lambdas returning collection literals (List, Map, Set) now produce correct values (#788)
- Expression-bodied lambdas now correctly retain ARC references and clean up scope before returning, preventing potential use-after-free when returning captured ARC-managed values (#789)
- Propagate collection return type metadata for block-bodied lambdas with inferred return types, so `result.length()` / indexing work on the value returned by `f = (x: int):\n  return [x, x * 2]` style lambdas (#790).
- `1num = 1` now correctly produces a syntax error instead of silently succeeding (#794)
- `replace(s, "", repl)` no longer hangs with an infinite loop; an empty pattern now returns a fresh copy of the input unchanged (#802)
- `NaN != NaN` now returns `true` as required by IEEE 754; float `!=` comparisons use `fcmp une` (unordered not-equal) instead of `fcmp one` (#803)
- `is_empty()` now accepts `str` arguments in addition to lists, maps, and sets (#831)
- `Result<JsonValue, Error>` returned by `json.get` / `json.at` no longer
  sneaks past JSON type checks via metadata alone. `isJsonValue()` now
  also requires the underlying LLVM value to be a pointer, so passing a
  `Result` to `kind` / `stringify` / `get` / `at` produces the existing
  "requires a JsonValue argument" diagnostic instead of an LLVM IR verify
  error. `to_str(result)` and `print(result)` still work and format as
  `Ok(...)` / `Err(...)` via the generic `valueToString` path (#805).
- Using `List` / `str` / `Map` / `Set` (or any other ptr-backed value) as
  a boolean condition in `if` / `while` / `case` or under the unary `not`
  operator now produces a clear compile-time error suggesting
  `length(x) > 0` or `not is_empty(x)`, replacing the previous
  `icmp ne ptr, i0 0` IR verify failure (#818).
- `exit(0)` followed by more statements no longer triggers
  `Terminator found in the middle of a basic block`. `emitExit()` now
  switches to a fresh dead basic block so trailing IR lands on a valid
  (unreachable) block and LLVM DCE removes it during optimization (#821).
- `u64` maximum value (`18446744073709551615`) now parses successfully when written with a `u64` suffix or under a `u64` / unsigned type annotation. Hex and binary forms (`0xFFFFFFFFFFFFFFFFu64`, `0b11...1u64`) are accepted too; range checking for `int` / `i64` / `u8`-`u32` happens in codegen against the target type (#807)
- `print()` / `to_str()` on `Map<K, List|Map|Set<...>>` now shows the actual nested container contents instead of empty strings for the values (#811)
- `print()` / `to_str()` on union types with `List`, `Map`, or `Set` variants now works instead of failing at compile time with "cannot convert ... variant of union to string" (#836)
- Whole-number `float` values now print with a trailing `.0` (e.g. `3.0`, `0.0`) instead of being indistinguishable from `int`, matching Python behavior (#808)
- `print()` / `to_str()` on a `Map` whose value type is a function now outputs `<closure>` instead of garbage bytes (#810)
- `wrapInUnion` now disambiguates same-LLVM-type variants (e.g. `List<int> | Map<str, int>`) by the value's collection metadata instead of always picking the first pointer-typed variant, fixing runtime miscategorization for collection/function unions
- Chained assignment targets are now accepted by the parser and codegen,
  including `list[i].field = v`, `record.a.b = v`, `list[i][j] = v`, and
  compound forms such as `list[i] += v` and `record.field[i] *= v` (#812).
  Previously these raised "expected '=' after index expression" or
  "expected '=' after field name". Compound assignment to a missing map key
  (`m["absent"] += 1`) now produces a clear runtime error instead of
  silently inserting a default value.
- Compiler now rejects defining a `record` and an `enum` with the same name in the same compilation unit. This also covers generic enum templates: `record Foo` and `enum Foo<T>` can no longer coexist, and duplicate generic enum declarations are rejected. Previously both declarations were accepted, leading to inconsistent type lookup. (#815)
- Top-level `let` bindings and `@const` declarations are now visible from any top-level function defined after them in the same source file. This includes reads and field access for all types — primitives, strings, lists, maps, sets, records, enums, and option/result values. Previously any such reference produced `undefined variable` at codegen (#817)
- Enum values returned from user functions now print as variant names
  (or `Variant(payload)` for ADT enums) instead of raw integers. Simple
  enums, ADT enums, and already-instantiated generic enums are all
  handled. Enum-typed elements stored in `List<Color>` literals also
  propagate correctly. (#820)
- `for i, x in enumerate(...)`, `for a, b in zip(...)`, and
  `for k, v in Map<K, V>` now preserve collection-element metadata on
  destructured variables, so `print` / `sum` / `length` work correctly
  when the elements are themselves `List` / `Map` / `Set` / enum. (#813)
- Generic function type inference now succeeds when the type parameter
  appears inside a container type in the declared parameter. `List<T>`,
  `Map<K, V>`, `Set<T>`, tuples `(T, T)`, and function types
  `function(T) -> T` now infer their type arguments from the call site,
  including nested combinations and cross-parameter unification. Previously
  calls such as `first_of([1, 2, 3])` for
  `function first_of<T>(xs: List<T>)` failed with
  "could not infer type parameter 'T'" even though the shape was
  unambiguous. The existing `name[T](args)` explicit syntax continues to
  work for cases where inference cannot determine the type (e.g., empty
  containers) (#823).
- `thread_join(t)` now returns the worker's value wrapped in `Ok(v)`
  instead of always `Ok(0)`. Workers using an expression-bodied lambda
  may return `int`, `float`, `bool`, or `Unit`. Joining an
  already-joined thread returns `Err("thread already joined")`. ARC
  types (`str`, `List`, `Map`, `Set`, records) and sum types
  (`Option`, `Result`, enums), block-bodied lambdas with a non-`Unit`
  return value, and panic-to-`Err` propagation remain unsupported and
  are tracked as follow-up issues. (#828)
- Type aliases targeting union types (e.g., `type Simple = int | str | bool`) now work correctly in variable annotations, function parameters, and function return types. Previously the compiler reported `annotation 'Simple' does not match expression type` because the union check examined the unresolved alias name instead of its target (#833)
- Nested type aliases over union types are now fully flattened. Previously, given `type A = int | str; type B = A | bool`, declaring `x: B = 42` failed with *"type is not in union"* because the alias `A` inside the union was not expanded. `B` is now equivalent to `int | str | bool`, and overlapping members are deduplicated — so `type C = A | int` collapses to `int | str`, and `type D = B | A` (where `B` already transitively includes `A`) flattens to `bool | int | str` (#835)
- Compiler now rejects a `type` alias whose name collides with an existing `record`, `enum`, generic `enum`, or previously-defined `type` alias, in either declaration order. This extends the cross-category duplicate check added in #815 to type aliases (including named unions such as `type Foo = int | str`). Duplicate error messages also now point at the offending declaration instead of a stale location. (#850)
- Chained writes through nested collections (`a[i][j] = v`, `r.items[i] = v`, `m[k1][k2] = v`) no longer leak through aliases. Path copy-on-write walks the LHS from root to leaf and clones every level whose reference count is greater than one before the mutation (#854)
- Record-to-record assignment (`r2 = r1`) now retains ARC-managed fields (`List<T>`, `Map<K, V>`, `Set<T>`) so both aliases share ownership of the inner containers. A subsequent mutation through one alias is isolated from the other by path copy-on-write (#854)
- `list[i] = v`, `m[k] = v`, and their compound forms now release the
  previously-held value before storing the new one when the element type
  is itself an ARC-managed collection (`List<List<T>>`, `List<Map<K,V>>`,
  `Map<K, List<V>>`, `List<Set<T>>`, and nested combinations). Previously
  every overwrite leaked the prior inner collection's heap allocation.
  The fix is safe under self-assignment (`xs[i] = xs[i]`) and cross-slot
  copy (`xs[i] = ys[j]`) by retaining the new value before releasing the
  old one. (#855)
- `rec.arcField = newList` now releases the previously-stored ARC-managed
  collection (`List`/`Map`/`Set`) before the overwrite, matching the
  element-slot fix from #855. Applies to plain and compound assignment on
  `VariableExpr`, `FieldAccessExpr` (chained `outer.inner.items = ...`),
  and `IndexExpr` (`list[i].arcField = ...`) left-hand sides. Sibling
  `fieldTypeIsArcManaged` predicate added so record field types are
  classified from their declared AST type rather than container metadata.
  (#857)
- `xs[i] += v` and `m[k] += v` now dispatch correctly when the element
  type is itself an ARC-managed collection (`List<List<T>>`,
  `Map<K, List<V>>`, and nested combinations reached via chained LHS such
  as `rec.items[i] += v`). Previously the loaded slot value lost its
  type metadata, so `emitArithmeticOp`'s list-concat dispatch fell
  through to the string path and produced a misleading
  `operator '+' not supported between str and non-str types` error.
  The fix propagates the container's element type name onto the loaded
  SSA value via `propagateTypeMeta` — the same pattern the formatter
  already uses for nested element loads. As a secondary fix, the
  empty-declaration path (`xs: List<List<int>> = []`) now records
  `list_elem_type_name` symmetric to the existing `List<Map>` /
  `List<Set>` branches so compound ops work on append-grown containers
  as well. (#858)
- `rec.arcField += v` now dispatches correctly when the field type is
  itself an ARC-managed collection (`List<T>`, `List<List<T>>`, etc.).
  This covers plain record field assignment (`b.items += [3]`), nested
  record field access (`outer.inner.items += [3]`), and chained LHS
  through a list of records (`lst[0].items += [3]`). Previously the
  field extracted from the struct lost its type metadata, so
  `emitArithmeticOp`'s list-concat dispatch fell through to the string
  path and produced a misleading `operator '+' not supported between
  str and non-str types` error. The fix propagates the field's declared
  type name onto the extracted SSA value via `propagateTypeMeta` at all
  three `FieldAssignStmt` compound branches — sibling fix to #858,
  which addressed the same class of metadata-loss bug on the
  `IndexAssignStmt` compound path. (#862)
- `+` applied to `Map` or `Set` operands now produces a clear error that names the actual collection type instead of the misleading `"operator '+' not supported between str and non-str types"` message. Mixed cases such as `List<int> + Map<str, int>` also name both operand types. (#863)
- `rwlock_unlock` now dispatches between shared and exclusive release via
  a `thread_local` counter per RWLock, eliminating the two-step window in
  `rwlock_read_lock` where `std::shared_mutex::lock_shared()` was held
  but the tracking map had not yet been updated. Under the previous
  implementation an unlock that observed the transient state would have
  fallen through to exclusive `unlock()`, corrupting `std::shared_mutex`
  state. (#871, follow-up to #630 P1)
- `ThreadHandle::has_error` is now a `std::atomic<bool>`; the worker
  thread's catch blocks store it with `memory_order_release` after
  writing `error_msg`, and `thread_join` loads it with
  `memory_order_acquire`. This makes the error-field publish/subscribe
  contract explicit, TSan-friendly, and robust for any future pre-join
  error polling path. (#871, follow-up to #630 P1)
- Lambdas (expression-body and block-body) that return one of their own
  collection-typed parameters now correctly propagate the parameter's
  declared shape so that `result.length()` and indexing work on the
  returned value (#886).
- Corrected `` `match value:` `` references to `` `case value:` `` in the pattern matching tutorial — the actual keyword is `case` (#889)
- Rewrote the networking example in the concurrency tutorial so the server/client snippets match runnable `net` test code (#889)
- Replaced outdated "struct" phrasing in `README.md` and `docs/README.md` with "record" to match the Ry keyword (#889)
- Updated the install one-liner in `README.md` to the current release version (#889)
- Added the `@describe` / `@it` directive-based test style to the testing tutorial and to the directives reference, so the new preferred syntax is actually documented (#889)
- Expanded the `README.md` feature list to mention pattern matching, the built-in testing framework, union types, GC (`std.gc`), and the `?` error propagation operator (#889)
- Expanded the `README.md` directives line beyond `@deprecated` to include the other common directives (#889)
- Added an explicit "In-Place Mutating Variants" section to the collections reference covering `append!`, `sort!`, `reverse!`, and the non-mutating `appended` counterpart (#889)
- Corrected stdlib `@native` declaration return types that had silently drifted from their codegen dispatcher implementations (#890):
  - `items(map: Map<str, int>)` now declared as `-> List<(str, int)>` (was `-> List<int>`)
  - `enumerate(values: List<int>)` now declared as `-> List<(int, int)>` (was `-> List<int>`)
  - `zip(values: List<int>, other_values: List<int>)` now declared as `-> List<(int, int)>` (was `-> List<int>`)
  The dispatchers (`emitCollOp_items`, `emitBuiltinQuery` for `enumerate`/`zip`) always returned lists of tuples; only the declarations were wrong. No behavior change — this corrects the stdlib documentation to match reality.

## [0.0.7] - 2026-04-03

### Fixed

- Fix Linux (GCC 11) build failure caused by incomplete type in self-referential `FnTypeInfo` struct (#623)

## [0.0.6] - 2026-04-03

### Added

- Empty list literal `[]` is now supported with type annotation (e.g., `xs: List<int> = []`) (#545)
- List concatenation with `+` and `+=` operators (e.g., `[1, 2] + [3, 4]` → `[1, 2, 3, 4]`) (#546)
- Tuple `==` and `!=` comparison now works via element-wise comparison (e.g., `(1, 2) == (1, 2)` → `true`) (#542)
- Single-element tuple type annotation `(int,)` is now supported in variable declarations and function signatures (#561)
- `split(s, "")` now splits a string into individual characters with full UTF-8 support (e.g., `split("hello", "")` → `["h", "e", "l", "l", "o"]`) (#549)
- Match expression syntax with `=>` for single-expression arms, enabling `res = match x: case Some(v) => v case None => 0` — all pattern types (literal, variable, enum, Option, Result, OR, guard) are supported (#499)
- The `as` cast operator now accepts the full type syntax including generic types (e.g., `x as Option<int>`, `x as Map<str, int>`) (#490)
- Regex literal syntax (`/pattern/`) that produces a `Regex` type, enabling type-based overload resolution and UFCS-compatible function calls (e.g., `"hello".is_match(/[a-z]+/)`, `"a1b2".split(/[0-9]/)`) (#458)
- New text-first regex functions: `is_match`, `search`, `replace`, `split`, `find_all` — overloaded to accept `Regex` type patterns alongside existing string functions
- `print()` now accepts multiple arguments with space-separated output (e.g., `print(1, "hello", true)` → `1 hello true`), and calling `print()` with no arguments now prints only a newline
- `body_bytes()` function for `HttpRequest` and `HttpClientResponse` that returns `List<u8>`, enabling binary-safe HTTP body access without NUL-byte truncation (#284)
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
- Mutual recursion and forward function references: functions can now call each other regardless of definition order, as long as they have explicit return type annotations (#550)
- `and_then` and `map` method chaining for `Result` type, enabling flat error handling without nested `match` (#597)
- Parser now accepts keyword tokens (e.g., `and`, `or`, `not`) as method names after `.` for UFCS calls

### Changed

- **Breaking**: Pattern matching syntax renamed from `when value:` to `match value:`; conditional `when:` (without subject) is unchanged (#482)
- **Breaking**: Anonymous function lambda form `function(...) => ...` is no longer supported; use parenthesized lambda syntax `(x: int) => x + 1` instead (#483)
- **Breaking**: Single-expression lambda syntax changed from `(params): expr` to `(params) => expr`; block lambdas `(params):\n  body` are unchanged (#498)
- **Breaking**: Self-update now requires Ed25519 signature verification by default; set `RY_SKIP_SIGNATURE=1` to opt out (#469)
- HTTP and JSON parsing hot paths now use pointer-based parsing to avoid unnecessary `substr` copies and temporary string allocations (#467)
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
- Control-flow syntax now keeps `if`/`else`, removes `elif`, replaces `match` with `when value:`, and replaces ternary `?:` with `when:` expressions
- `char_at()` now uses a single-pass UTF-8 traversal for bounds checking and character extraction, eliminating a redundant full-string scan (#407)
- `ry new` / `ry init` now normalize hyphens to underscores in package names (e.g. `ry new my-app` creates `name = "my_app"` in package.toml)
- `.test.ry` files are excluded from directory package loading
- Fixed-length array type syntax changed from `[T; N]` to `T[N]` (e.g. `buf: i32[4] = [1, 2, 3, 4]`)
- `to_int(str)` now returns `Result<int, Error>` instead of bare `int`, properly detecting invalid input (#543)
- `int` arithmetic (`+`, `-`, `*`, unary `-`) now raises a runtime error on overflow instead of silently wrapping (#544)
- Constant expressions that overflow are caught at compile time (#544)
- Clarified in documentation that closures capture by value in both directions: outer variable changes do not affect the closure, and mutations inside the closure do not affect the outer scope (#552)
- All collection headers (List, Set, Map) now use ARC allocation uniformly, ensuring correct reference counting and CoW behavior (#572)
- `json.keys()` now returns `Result<List<str>, Error>` instead of `List<str>`, with proper null-pointer handling for OOM and non-object inputs (#599)

### Fixed

- Single-element tuple literal `(42,)` and trailing commas in tuple literals now parse correctly (#556)
- `print()` and `to_str()` now support tuples including nested tuples, displaying them as `(elem1, elem2)` (#541)
- `print()` on lists of tuples (e.g., `zip()` result) now correctly displays tuple elements instead of empty entries (#540)
- f-string interpolation now supports collection types (List, Map, Set) and tuples (e.g., `f"items: {xs}"`) (#547)
- Operator overloads (`operator[]`, `operator+`, etc.) now correctly propagate return type metadata for collection types, fixing "cannot determine list element type for index access" when `operator[]` returns `List<T>` (#537)
- `operator as` overload resolution now uses semantic type names instead of LLVM types, preventing false matches between pointer-backed types (`str`, `List<T>`, `Map<K,V>`, etc.) (#537)
- `to_str()` on union-typed values now returns the string representation of the actual value instead of the discriminant index (#536)
- Option/Result type-meta guard now checks all collection metadata keys (`TM_ListElem`, `TM_SetElem`, `TM_TaskResult`), not just `TM_MapKey`, preventing potential metadata overwrites when wrapping collection types (#525)
- Generic function parameters with collection types (`List<T>`, `Map<K, V>`, `Set<T>`) are now properly marked as ARC-managed during instantiation, preventing potential memory leaks (#524)
- Return type inference now correctly resolves user-defined struct types in functions and lambdas without explicit type annotations (#515)
- Return-path analysis now recognizes exhaustive `match` statements on custom enums and `bool`, removing false "does not return a value on all code paths" errors when all variants are covered (#513)
- Indexing into `List`, `Map`, or `Set` fields of a record (e.g., `record.field[idx]`) no longer fails with "cannot determine list element type" (#511)
- `join()` now works correctly with UFCS string receiver (e.g., `",".join(parts)`) (#508)
- Closures returned from functions can now be called, and function-type parameters can be captured in closures — enabling higher-order patterns like `make_adder`, `compose`, and currying (#510)
- **Breaking**: Legacy regex functions (`regex_match`, `regex_search`, `regex_replace`, `regex_split`, `regex_find_all`) now use text-first argument order `(text, pattern)` consistent with the regex literal API; previously `(pattern, text)` which caused silent incorrect results (#512)
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
- Fixed float output examples in operator tutorial to match actual `%g` formatting (e.g. `1024` not `1024.0`)
- Added curl one-liner installer to Getting Started tutorial
- Fixed "struct" terminology to "record" across tutorial and reference docs
- Improved error message when hyphens are used in import paths (e.g. `from my-pkg import foo` now suggests using underscores)
- Binary operations between `str` and non-`str` types (e.g. `"abc" - 2`, `"abc" / 2`) now raise compile-time type errors instead of producing garbage output or LLVM IR verification errors (#396)
- `ry version` now works as an alias for `ry --version` instead of trying to execute the VERSION file on case-insensitive filesystems (#381)
- Dev Release nightly build fails due to missing dependencies (`openssl@3`, `ninja`, `googletest`) and removed schedule trigger (#380)
- Chained Map index access now works correctly for nested Maps (#538)
- `return none` now works correctly in generic functions with `T?` return type (#539)
- UFCS call on list literal (e.g., `[1, 2, 3].map(...)`) no longer fails to parse at statement level (#551)
- Operator overloads now correctly propagate function-type return metadata (#554)
- Fix undefined behavior in collection header deallocation where scope cleanup read invalid memory before plain-malloc headers (#572)
- Fix memory leak when collection operation results (appended, slice, etc.) are discarded as expression statements (#572)
- Heap corruption after Iterator tests caused by leaked iterator headers and states; iterator memory is now freed at scope exit (#577)
- Inline `case` body in `match`/`when` statements now parses correctly (e.g., `case Ok(v): expr`) (#587)
- Fixed double-free heap corruption in JSON `get()`/`at()` child values (#594)
- Arithmetic, comparison, and bitwise operations between high-level `int` and low-level `i64`/`u64` now correctly produce a compile error instead of silently succeeding (#595)
- `print()` now supports `Result` types directly, displaying `Ok(value)` or `Err(error)` (#612)
- `to_str()` now correctly converts `Result` and `Option` types to their string representation instead of returning the internal tag value (#611)

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

[Unreleased]: https://github.com/t0k0sh1/ry/compare/v0.0.9...HEAD
[0.0.9]: https://github.com/t0k0sh1/ry/compare/v0.0.8...v0.0.9
[0.0.8]: https://github.com/t0k0sh1/ry/compare/v0.0.7...v0.0.8
[0.0.7]: https://github.com/t0k0sh1/ry/compare/v0.0.6...v0.0.7
[0.0.6]: https://github.com/t0k0sh1/ry/compare/v0.0.5...v0.0.6
[0.0.5]: https://github.com/t0k0sh1/ry/compare/v0.0.4...v0.0.5
[0.0.4]: https://github.com/t0k0sh1/ry/compare/v0.0.3...v0.0.4
[0.0.3]: https://github.com/t0k0sh1/ry/compare/v0.0.2...v0.0.3
[0.0.2]: https://github.com/t0k0sh1/ry/compare/v0.0.1...v0.0.2
[0.0.1]: https://github.com/t0k0sh1/ry/releases/tag/v0.0.1
