### Removed

- Removed the deprecated lambda call form of `describe("...", ():)` and
  `it("...", ():)` from the parser and codegen. After #1599 (stdlib
  migration) and #1601 (deferred-file migration), all `tests/spec/*.test.ry`
  files use the canonical `@describe("...") fn name():` / `@it("...") fn name():`
  named-function form, so the lambda form is no longer reachable from
  any in-tree source. The trailing-block carve-out for `describe` / `it`
  in the parser and the dedicated lambda-form codegen helpers
  (`extractLambdaArg`, `emitDescribeCall`, `emitItCall`, the lambda
  branches of `emitEachItCall` / `emitPropertyItCall`) were deleted
  along with the `warned_call_deprecations_` warning-dedup state.
  Source that still uses the lambda form now fails compilation with
  `undefined function: describe` / `undefined function: it`. (#1602)
