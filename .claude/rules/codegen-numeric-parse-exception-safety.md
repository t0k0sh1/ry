---
paths:
  - "src/*.cpp"
  - "src/**/*.cpp"
  - "src/*.hpp"
  - "src/**/*.hpp"
  - "include/ry/*.hpp"
  - "include/ry/**/*.hpp"
---

# Numeric Parse Exception Safety

- Do not call `std::stoi` / `std::stol` / `std::stoll` / `std::stoul` / `std::stoull` / `std::stof` / `std::stod` from compiler-side C++ (parser / codegen / loader / runtime helpers). They throw `std::out_of_range` and `std::invalid_argument`, both of which propagate uncaught across the compiler driver and abort the whole process via `libc++abi` (exit 134) instead of producing a structured diagnostic.
- Use the C `strto*` family with the established three-check guard:
  1. reject empty input (`s.empty()`),
  2. reject overflow (`errno == ERANGE` after pre-zeroing `errno`),
  3. reject trailing garbage / non-decimal input (`end != s.c_str() + s.size()`).
- Surface failures via the call site's normal diagnostic path (`parseError` / `codegenError`), not via `throw`.
- Established hit-site patterns to mirror when adding a new call:
  - Parser fixed-length array size: `src/parser/parser_decl.cpp:951-955` (`strtoull`).
  - Type-constraint integer bounds: `parseInt64Bound` in `src/codegen_type.cpp` (`strtoll`).
  - Tuple numeric field index: `src/codegen_expr_literal.cpp:294-301` (`strtoul`).
- A pre-existing `try { std::stoX(s); } catch (...) { return std::nullopt; }` form (e.g. `tryParseSegmentInt` in `src/codegen_call_collection.cpp`) is acceptable only when every escape path is already caught; new code must prefer the `strto*` guard for consistency.
- Background: PR #2343 deleted the original parser-conventions rule file (now gone from .claude/rules/) which had tracked this invariant; #2422 reinstated the rule after `src/codegen_type.cpp:139` was found still using `std::stoull` unprotected.
