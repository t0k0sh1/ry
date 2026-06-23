---
paths:
  - "tests/spec/arc_*.test.ry"
  - "tests/spec/**/*arc*.test.ry"
---

# Tests — ARC Leak Regression Pattern

This file covers only hazards that are not visible from reading the code.

### ARC leak regression tests use `runtime_internal.arcLiveCount()` delta assertions

**Tags**: testing, arc, leak-detection, runtime-instrumentation

macOS ASan does not include LSan, and CI runs with `detect_leaks=0`. The absolute value of `arcLiveCount()` is non-zero immediately after collection creation (collection destructors do not recursively release ARC-managed elements — a known "element leak on destructor"). Asserting on the absolute value rather than the delta (before/after difference) will always show a non-zero result due to background noise, making leaks undetectable. Verify that the delta grows proportionally to N iterations to identify leaks on overwrite paths.

### `List<str>` literals can have empty `list_elem_type_name` — UAF tests on retain paths are silently neutralized

**Tags**: codegen, arc, metadata, list_elem_type_name, testing, symmetric-omission

Building a `List<str>` from a list literal produces `list_elem_type_name = ""` because `inferCollectionTypeName` in `codegen_expr_literal.cpp` returns `""` for string values. When `list_elem_type_name` is empty, `getOrCreateCollectionDestructor` skips releasing str elements. This means a missing retain combined with a missing release produces a leak rather than UAF, so **tests pass even when the retain fix is not implemented**. Use `List<List<int>>` / `List<Map<str,int>>` / `List<fn(…) -> …>` for UAF regression tests on retain paths. When using `List<str>`, verify via IR inspection that the test fails before the fix. This blind spot also exists in `list_range_index.test.ry` from #1204.
