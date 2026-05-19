### Fixed

- `coerceResultType` now handles `Result<any, X>` → `Result<E, X>`
  coercion when the destination Ok or Err slot is an enum type (simple
  enum, `Option<T>`, `Result<T, E>`, or ADT). Previously the per-slot
  `unwrapFromAny` call passed an empty `targetTypeName`, and the
  `canAnyHoldType` gate rejected enum struct destinations entirely, so
  the coercion fell back to "type error: variable '...' cannot be
  reassigned to a different type" or silently took the wrong runtime
  branch. `coerceResultType` gains an optional `dstResTypeName`
  argument (defaulting to `""` for backward compatibility); the five
  call sites — `emitVarDecl`, function-local reassignment,
  module-global reassignment, and the two `mockReturnValueOnce`
  emitters — thread the destination's source-level Result type name so
  the Ok / Err slot can dispatch through the descriptor-driven enum
  unwrap path. Cross-type mismatches continue to trap at runtime via
  the existing `any enum type mismatch` diagnostic from
  `unwrapEnumFromAny`. The `canAnyHoldType` gate remains restricted to
  primitives, preserving the parallel-branch design from #1797 /
  #1798. (#1808)
