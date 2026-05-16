### Fixed

- Fixed a JIT crash / use-after-free when calling a higher-order
  function whose return value is an `fn(...) -> T` typed value loaded
  from a parameter (e.g. `fn pick(f: fn() -> Unit) -> fn() -> Unit:
  return f` invoked inline and then called via the bound local).
  Fn-typed parameter allocas are not registered in `arc_managed_vars_`
  because callers own the uniform-closure wrap temp via
  `releaseUniformClosureTemps`. Returning such a value out of the
  callee made the caller's post-call release free the storage while
  the caller still held the returned handle. A new
  `retainFnTypedParamForReturn` helper, called from `emitStmt(ReturnStmt)`,
  retains the value when the source alloca's metadata flags it as a
  uniform-closure fn-typed parameter; non-return load sites
  (pass-through fn args, two-level nesting) are unaffected. (#1770)
