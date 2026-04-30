---
paths:
  - "src/runtime_regex*.cpp"
  - "include/ry/runtime_regex.hpp"
---

# Regex Engine

### Thompson NFA stores per-thread state on NFAState itself — extending to capture slots requires a separate approach

**Source**: #829 (2026-04-14, implementation)
**Tags**: regex, nfa, capture-groups, design-decision

**Rule**: The `NFASimulator` in `src/runtime_regex.cpp` stores per-thread tracking data
directly on `NFAState` (`matchStartPos`). This is safe because only one value is tracked.
Extending the Thompson NFA to carry per-thread capture slot arrays (RE2 style) would require
moving to a `Thread = {NFAState*, vector<pair<int,int>>}` approach — a significant rewrite.

Instead, capture group extraction for `replace` uses a separate backtracking pass over the
NFA graph, anchored to the match boundaries already found by `findAll`. This:
1. Keeps the Thompson path completely unchanged (no performance impact for non-backreference patterns)
2. Is safe against catastrophic backtracking because the span is bounded by the match

As of #830, `findAll` also uses `CaptureBacktracker` to expose capture groups in `Match.groups`.
The same two-phase approach (NFA finds boundaries, CaptureBacktracker extracts groups) works
well without modifying the NFA simulator. Migrating to per-thread capture slot arrays would
only be necessary if performance profiling shows the per-match backtracking is a bottleneck.

### GroupOpen / GroupClose epsilon states are transparent to the Thompson NFA simulator

**Source**: #829 (2026-04-14, implementation)
**Tags**: regex, nfa, capture-groups, epsilon-states

**Rule**: `GroupOpen` and `GroupClose` are added to `NFAState::Kind` as epsilon states.
`addState()` treats them like `Split` — follows `out1` without consuming input, incrementing
no generation. This means the existing Thompson simulator ignores them at zero cost.
The `CaptureBacktracker` (used only when `$N` appears in the replacement) uses the same NFA
graph and reads these states explicitly.

### UFCS regex functions must dispatch to a dedicated `__ry_regex_<verb>` runtime symbol — never alias to the prefixed `regex*` form

**Source**: #1197 (2026-04-19, implementation)
**Tags**: regex, codegen, api-naming, ufcs, dispatch

**Rule**: Each unprefixed UFCS regex function (`isMatch`, `search`, `replace`, `split`, `findAll`) must dispatch to a dedicated `__ry_regex_<verb>` runtime symbol whose semantics literally match the function's name. Never alias a UFCS form to the prefixed `regex*` form's runtime symbol — the two can have different semantics (e.g. `isMatch` is partial/unanchored search, but `regexMatch` is full-string match). When adding a new UFCS regex function, add a matching `__ry_regex_<verb>` C entry point in `src/runtime_regex.cpp` + `include/ry/runtime_regex.hpp` and cover it directly in `tests/test_regex_runtime.cpp` — do not rely on a shared symbol and a LLVM `Trunc` to paper over a semantic mismatch.
