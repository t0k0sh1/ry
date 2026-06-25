---
paths:
  - "tests/spec/arc_*.test.ry"
  - "tests/spec/**/*arc*.test.ry"
---

# Tests - ARC Leak Regression Pattern

- ARC leak tests should assert `runtime_internal.arcLiveCount()` deltas, not absolute values.
- macOS ASan lacks LSan and CI uses `detect_leaks=0`, so delta-based runtime instrumentation is the portable signal.
- Avoid `List<str>` literals for retain-path UAF regression tests unless IR inspection proves the test fails before the fix; prefer nested collections or function lists with populated element metadata.
