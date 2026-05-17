### Added

- Added per-overload mocking, spying, and verification for overloaded
  functions across the testing framework. The mock registry is now
  keyed by canonical signature `"name(T1, T2)"` instead of bare name,
  so each overload has an independent slot. `mock` /
  `mockReturnValueOnce` / `spy` / `verify` / `verifyCalledWith` /
  `mockClear` / `mockReset` all accept the signature-form string
  (e.g. `mock("add(int, int)", ...)`, `verify("digits(int, int)")`).
  Custom-emitter `@native` overloads — including the math overload
  set (`abs`, `floor`, `ceil`, `round`, `log`, `pow`, `digits`) —
  are now mockable / spy-able via the same signature form; argument
  recording for `verifyCalledWith` on those natives is not supported
  in v1 (count-based `verify` works). Whitespace inside the signature
  is normalized; type aliases are resolved automatically. (#1682)

### Changed

- Bare-name semantics for the testing API on overloaded functions are
  defined as follows (no change for single-overload functions):
  `mock(n, repl)` auto-dispatches when the replacement lambda's
  signature uniquely matches one overload, otherwise errors with the
  candidate list; `mockReturnValueOnce(n, v)` errors (return-value
  alone cannot disambiguate); `spy(n)` registers spy for **all**
  overloads aggregately; `verify(n)` returns the **sum** of call
  counts across all overloads; `verifyCalledWith(n, ...)` dispatches
  to the arity-matching overload or errors when ambiguous;
  `mockClear(n)` / `mockReset(n)` clear / remove every overload. As
  a consequence, if existing code calls `verify("foo")` and `foo`
  later gains a second overload, the return value silently becomes
  the aggregate count — switch to `verify("foo(int)")` to preserve
  per-overload counting through such a change. (#1682)
