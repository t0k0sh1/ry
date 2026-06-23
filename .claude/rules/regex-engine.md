---
paths:
  - "src/runtime/core/regex*.cpp"
  - "include/ry/runtime/core/regex.hpp"
---

# Regex Engine

This file covers only hazards that are not visible from reading the code.

### Thompson NFA stores per-thread state on NFAState itself — extending to capture slots requires a separate approach

**Tags**: regex, nfa, capture-groups, design-decision

`NFASimulator`'s per-thread tracking data is stored directly on `NFAState` (`matchStartPos`). This is safe because only one value is tracked. Do not migrate to RE2-style per-thread capture slot arrays (`Thread = {NFAState*, vector<pair<int,int>>}`). Instead, capture group extraction is designed (#830) as a two-phase approach: a `CaptureBacktracker` makes a second pass starting from the confirmed match boundaries produced by the NFA. Unifying these into RE2 style on the grounds that the NFA is "redundant" would require a large-scale rewrite of the NFA simulator.

### UFCS regex functions must dispatch to a dedicated `__ry_regex_<verb>` runtime symbol — never alias to the prefixed `regex*` form

**Tags**: regex, codegen, api-naming, ufcs, dispatch

Aliasing UFCS forms (`isMatch`, `search`, `replace`, `split`, `findAll`) to prefixed runtime symbols compiles silently despite semantic differences — e.g., `isMatch` is a partial/unanchored search while `regexMatch` is a full-string match. Because the arguments agree, no codegen type error fires, and ASCII-pattern tests will nearly all pass. Each UFCS function requires its own `__ry_regex_<verb>` C entry point.

### Regex APIs that return a numeric position MUST return a character index, not a raw NFA byte offset

**Tags**: regex, utf8, char-index, byte-offset, runtime, api-policy

The NFA simulator returns byte offsets internally. On ASCII or NUL-only subjects, byte and character indices agree so tests pass; on multibyte subjects (Japanese, emoji), returning raw byte offsets desynchronizes with other string APIs (`len` / `charAt` / `substr` / `find`) and goes undetected until a user encounters it. When an entry point returns a numeric position, convert the byte offset to a character index via `__ry_utf8_char_index_n` before returning (`if (byte_offset < 0) return byte_offset;` to pass sentinels through first). Tests must always include multibyte subjects such as `"あx"` / `"🎉x"` — ASCII-only coverage cannot detect this bug class.
