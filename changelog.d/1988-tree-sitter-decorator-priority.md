### Fixed

- Tree-sitter highlight query (`editor/tree-sitter/queries/highlights.scm`)
  now applies `(#set! "priority" 105)` to the `decorator` pattern so that
  the decorator's identifier (e.g. `my_dec` in `@my_dec`) is highlighted
  as `@attribute` instead of being overridden by the generic
  `(identifier) @variable` fallback. Both patterns previously matched at
  default priority 100, and tree-sitter's last-match-wins tie-breaker
  promoted `@variable` — which was semantically wrong (a decorator name
  is not a variable) and produced no color in colorschemes that leave
  `@variable` unstyled. The fix is `highlights.scm`-only; `ry.so` does
  not need to be rebuilt. (#1988)
