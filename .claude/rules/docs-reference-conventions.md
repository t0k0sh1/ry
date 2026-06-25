---
paths:
  - "docs/**/*.md"
  - "README.md"
---

# Documentation Reference Conventions

- `share/std/*.ry` declarations are not always the source of truth for `@native`; compare declaration, dispatcher in `src/codegen_call_*.cpp`, and tests before documenting native return shapes.
- Custom-emitter natives with heterogeneous return types should use `any` as the declaration placeholder and explain the placeholder in the `.ry` file.
- Do not call `weak`, `None`, `Some`, `Ok`, `Err`, or `_` reserved keywords unless they are actually in `src/lexer/lexer.cpp` `keyword_map`; use "contextual identifier" when appropriate.
- Verify multi-file or multi-package documentation with a minimal real layout under `/tmp/` and `./build/ry`; a single-snippet compile is not enough for visibility/import behavior.
- Do not publish aspirational examples. If observed behavior diverges from intended behavior, apply `/triage-side-finding`.
