### Added

- `m[k]?` and `xs[i]?` postfix syntax for safe collection access.
  Applied directly to a Map or List index expression, the trailing `?`
  changes the semantics from "abort on miss" to "produce an `Option`":
  `m["a"]?` returns `Some(v): Option<V>` when the key is present and
  `None` otherwise; `xs[i]?` returns `Some(v): Option<T>` when the
  (possibly negative-wrapped) index is in range and `None` otherwise.
  This is a postfix syntax rather than sugar for `get(m, k)` — it
  parses as `IndexExpr` with a new `try_mode` flag and flows through
  the same codegen path on both Map and List. The negative-index wrap
  established by `xs[-1]` is preserved (so `xs[-1]?` on a non-empty
  list always returns `Some(last)`); only the post-wrap out-of-range
  case yields `None`. Write-form `m[k]? = v` (including `m[k]?.x = v`
  and `mm[k]?[k2] = v`), `?` on fixed-length arrays, on `str`, on
  range slice `xs[a..b]?`, and on `any`-typed nested access are
  rejected at compile time. The lexer's greedy tokenization of `??`
  means `m["k"]?? default` (no space) still parses as `m["k"]` +
  `?? default` — write `m["k"]? ?? default` (with a space) for the
  Option-returning form coalesced with a default value. (#1699)
