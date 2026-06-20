---
paths:
  - "src/runtime/core/regex*.cpp"
  - "include/ry/runtime/core/regex.hpp"
---

# Regex Engine

### Thompson NFA stores per-thread state on NFAState itself — extending to capture slots requires a separate approach

**Source**: #829 (2026-04-14, implementation)
**Tags**: regex, nfa, capture-groups, design-decision

**Rule**: The `NFASimulator` in `src/runtime/core/regex.cpp` stores per-thread tracking data
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

**Rule**: Each unprefixed UFCS regex function (`isMatch`, `search`, `replace`, `split`, `findAll`) must dispatch to a dedicated `__ry_regex_<verb>` runtime symbol whose semantics literally match the function's name. Never alias a UFCS form to the prefixed `regex*` form's runtime symbol — the two can have different semantics (e.g. `isMatch` is partial/unanchored search, but `regexMatch` is full-string match). When adding a new UFCS regex function, add a matching `__ry_regex_<verb>` C entry point in `src/runtime/core/regex.cpp` + `include/ry/runtime/core/regex.hpp` and cover it directly in `tests/test_regex_runtime.cpp` — do not rely on a shared symbol and a LLVM `Trunc` to paper over a semantic mismatch.

### Regex APIs that return a numeric position MUST return a character index, not a raw NFA byte offset

**Source**: #2265 (2026-06-20, bug fix)
**Tags**: regex, utf8, char-index, byte-offset, runtime, api-policy

**Rule**: Any regex runtime entry point that returns a numeric position into the subject (currently only `__ry_regex_search`; future `searchAll` / `findIndex` / capture-position APIs follow the same rule) must convert the Thompson NFA's internal byte offset to a character (codepoint) index before returning to Ry code. The canonical conversion is `__ry_utf8_char_index_n(text, textLen, byte_offset)` from `include/ry/runtime/core/utf8.hpp` — the same helper `find()` uses (`src/codegen_call_string.cpp`). This keeps the regex surface consistent with the rest of the string API (`len`, `charAt`, `substr`, `find`, `reverse` are all codepoint-based; only `byteLen` is byte-based).

**Why**: The NFA simulator works in bytes internally; raw byte offsets are correct only for ASCII / NUL-only subjects. Multibyte subjects (Japanese, emoji) silently desync from `docs/reference/regex.md` and from the rest of the string API. The "ASCII tests pass, multibyte production bug" failure mode is invisible until users hit it.

**How to apply**:
- Guard the conversion with `if (byte_offset < 0) return byte_offset;` before calling `__ry_utf8_char_index_n` — the helper's precondition is `0 <= byte_offset <= byte_len` and not-found / sentinel-error returns are out of range.
- Tests must include multibyte subjects (`"あx"`, `"🎉x"`, etc.) in both `tests/spec/regex_*.test.ry` and `tests/test_regex_runtime.cpp`. ASCII-only coverage cannot detect this class of bug.
- `regexSplit` / `regexFindAll` return strings / `Match` structs (not numeric offsets) and are exempt — they expose the matched substrings directly, so no byte/char distinction surfaces. The rule applies only to numeric-offset returns.
