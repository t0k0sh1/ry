---
paths:
  - "tests/spec/arc_*.test.ry"
  - "tests/spec/**/*arc*.test.ry"
---

# Tests — ARC Leak Regression Pattern

### ARC leak regression tests use `runtime_internal.arcLiveCount()` delta assertions

**Source**: #859 (2026-04-16, implementation)
**Tags**: testing, arc, leak-detection, runtime-instrumentation

**Context**: macOS ASan has no LSan; CI runs with `detect_leaks=0`.  The
`runtime_internal` stdlib module (bare `@native`, no separate shared lib —
resolves from the host process's `ry_lib` symbols) exposes a single function:

```ry
from runtime_internal import arcLiveCount
```

It returns the running balance of ARC header allocations minus frees
(`int64_t`, relaxed-atomic, monotonic).

**Rule**: To write a leak regression test for an ARC operation, snapshot
`arcLiveCount()` before and after, then assert the *delta* (not the
absolute value) is at most a small constant:

```ry
before = arcLiveCount()
# ... N iterations that each overwrite an ARC-typed slot ...
delta = arcLiveCount() - before
expect(delta).toEq(k)   # k = #containers still live, not proportional to N
```

Why delta (not absolute): the collection destructor does NOT recursively
release ARC-managed elements (pre-existing "element leak on destructor",
KNOWLEDGE line ≈692).  Absolute counts are therefore always non-zero after
any collection is created.  Delta-based assertions isolate the overwrite
path from this background noise.

**Coverage**: `tests/spec/arc_release_on_index_overwrite.test.ry` contains
the canonical examples.  The counter tracks only ARC *header* allocs/frees
(codegen path via `__ry_arc_alloc_counted` / `__ry_arc_free_counted` and the
C++ helper path in `include/ry/runtime_arc.hpp`).  COW buffer reallocs and
collection internal buffers are NOT counted.

### `List<str>` literals can have empty `list_elem_type_name` — UAF tests on retain paths are silently neutralized

**Source**: #1235 (2026-04-20, observation during ARC retain fix)
**Tags**: codegen, arc, metadata, list_elem_type_name, testing, symmetric-omission

**Rule**: A `List<str>` constructed purely from a list literal (e.g. `xs: List<str> = ["a" + "b", "c" + "d"]`) typically has `list_elem_type_name = ""` rather than `"str"`. Tests that assign `xs = ["dropped"]` after a `take` / `slice` / `concat` and then read the returned list expect to exercise the retain path — but on `List<str>`, the *release* path of the source also skips str destruction (because the destructor only emits `emitStrElemLoop` when `elemSig == "str"`). Retain omission + release omission = leak, not UAF, so the test passes whether or not the retain was implemented. Do NOT rely on `List<str>` UAF tests alone to prove a retain fix is wired up; add a `List<List<int>>` or `List<Map<K,V>>` case (where `list_elem_type_name` *is* populated — see `codegen_stmt.cpp` annotation branch at ~647) or inspect IR for the `cow_*_elem_loop` retain block.

**Why**: `codegen_expr_literal.cpp`'s list-literal path sets `list_elem_type_name = inferCollectionTypeName(vals[0])`, which returns `""` for string values. The annotation branch in `codegen_stmt.cpp:emitVarDecl` (~647-672) fills in `List<Map<…>>`, `List<Set<…>>`, `List<List<…>>`, `List<fn(…)>`, `List<Tuple<…>>`, `List<int>` etc., but has no arm that writes `"str"`. The only path that stamps `list_elem_type_name = "str"` is `emitStringToCharList` for `__ry_split_chars` in `codegen_builtin.cpp:225-234`. `elementTypeIsArcManaged` reads this field, so the retain is skipped for plain `List<str>`. Symmetrically, `getOrCreateCollectionDestructor` in `codegen_arc.cpp:843+` only emits str-element release when `elemSig == "str"`, so the element isn't released either — the heap string leaks (small) but no dangling pointer is produced.

**How to apply**: When writing UAF regression tests for collection-element retain paths, prefer `List<List<int>>` / `List<Map<str,int>>` / `List<fn(…) -> …>` over `List<str>`, or use both. When reviewing such a PR, verify the test discriminates — a test that passes before the fix indicates the test (or the surrounding metadata pipeline) is not catching what it claims to catch. Also note: #1204's own `list_range_index.test.ry` has the same `List<str>` blind spot — any future fix that addresses the missing `list_elem_type_name = "str"` annotation (possibly a follow-up issue) should re-verify #1204's tests then fail pre-fix.

**Related**: #1204 (slice retain fix — same test pattern, same blind spot), #1235 (take retain fix — discovered property), #1046 (str ARC dispatch via side-table, independent of `list_elem_type_name`).

### ConcurrencySpecSuite ASan DISABLED_ was stale after #630's atomic-ARC fix

**Source**: #872
**Tags**: asan, concurrency, parallel_for, arc, testing

**Rule**: `ConcurrencySpecSuite` was disabled under ASan in commit `fb010ea`
(2026-03-31) because non-atomic ARC retain/release ops in `@parallel for`
workers raced with ASan's shadow-memory interceptors, causing a deadlock
(SIGALRM, exit 142 after 300 s on Linux).  After #630's P0 fix made all ARC
ops inside `@parallel for` thunks use `atomicrmw seqcst`, the root cause
was removed.  The `DISABLED_` guard was removed in #872; on macOS the suite
runs in ~55 ms.  If a future change reintroduces non-atomic ARC inside a
`@parallel for` thunk, re-enable the guard and investigate the hang before
merging.
