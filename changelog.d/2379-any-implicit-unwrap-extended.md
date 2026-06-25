### Changed

- **Breaking change**: the `any-implicit-unwrap` rule (#2321 / #2322) now also rejects three structurally similar hazards that were previously carved out of Path 9 ("tracked separately" in [`docs/reference/strict-any.md`](docs/reference/strict-any.md)). The seven sub-cases now covered by `[strict-any/any-implicit-unwrap]` are:

  - Variable declaration `n: int = v` where `v: any` (Path 9a, unchanged).
  - Named-fn call argument `f(v)`, including the default-value branch (Path 9b, unchanged).
  - Lambda-call argument `g(v)` (Path 9c, unchanged).
  - `Ok(v)` / `Err(v)` / `Some(v)` flowing into a typed `Result` / `Option` slot (Path 9d, unchanged).
  - **New**: reassignment of a previously-declared typed variable — both function-local (`x: int = 1; x = v`) and module-global write-through. Also covers `Result` slot widening on reassignment (`r: Result<int, str> = Ok(0); r = produce()` where `produce()` returns `Result<any, str>` or `Result<int, any>`), which previously slipped through `coerceResultType` (Path 9e).
  - **New**: returning an `any` value from a typed function or lambda (`fn f() -> int: return v`, expr-body lambda `() -> int => v`) (Path 9f).
  - **New**: mutating a typed collection with an `any` value — covers `append!` / `appended` / `insert` on `List<T>`, `add` / `remove` on `Set<T>`, `m[k] = v` on `Map<K, V>`, and `xs[i] = v` on `List<T>` (Path 9g).

  Recovery is the same as the existing rule: `case asType[T](v): Ok(x): ... Err(_): ...` before the boundary. Read-only `any → concrete` paths whose surface is not a slot-bound assignment — the `in` / `not in` membership operator and the `get(list, idx, default)` fallback value — continue to unwrap silently and are out of scope for this rule. Explicit `any` boundaries (`v: any = ...`, `from ry.json import load`, FFI `@extern` returns) remain valid. (#2379)
