---
paths:
  - "src/codegen_expr.cpp"
  - "src/codegen_expr_cast.cpp"
  - "src/codegen_builtin.cpp"
  - "src/codegen.cpp"
  - "src/codegen_test.cpp"
  - "src/codegen_call_user.cpp"
  - "src/codegen_lowering_*.cpp"
  - "src/codegen_emission_*.cpp"
  - "src/jit/jit_runner.cpp"
  - "src/app/main.cpp"
  - "include/ry/codegen.hpp"
  - "include/ry/codegen/**/*.hpp"
  - "tests/filecheck/**/*.ry"
---

# Codegen LLVM IR Conventions

### emitArithmeticOp dispatch order: new type-specific branches go BEFORE the str-vs-non-str reject

`emitArithmeticOp` ends with a str-vs-non-str reject as its last line of defense, but `isStringValue()` returns true for any `ptrTy_` value without `hasAnyMeta()` — effectively "unknown pointer". Branches added AFTER the reject are unreachable for metadata-less pointer operands. Insert new type-specific branches (List concat, Map merge, Set union, etc.) BEFORE the reject. Use typed metadata accessors (`getMapKeyType` / `getListElementType` / etc.), not `isStringValue`. Current order: low-level / `**` / string concat / List+List / Map+Map / Set+Set / mixed-collection reject / str-vs-non-str reject / `//`+`/`+`%` / `+`-`*`.

### Bool reject in arithmetic / bitwise must be at each operator branch entry, not at function entry

`promoteToInt` and `promoteToFloat` silently widen `i1` (bool) via ZExt / SIToFP. Any arithmetic / bitwise branch that calls them — or `CreateSIToFP` directly — must call `rejectBoolInOperator(v, op, "arithmetic"|"bitwise")` first. Do NOT gate inside `promoteToInt` / `promoteToFloat` themselves (ARC and string-repeat paths intentionally widen i1). Do NOT gate at function entry either: `emitArithmeticOp` processes string concat / repeat (`"x" + true` auto-stringifies — must keep working) and collection-concat BEFORE numeric branches, so a top-of-function reject would break legal expressions. Place `rejectBoolInOperator` at the entry of each numeric / bitwise branch (`**`, `//`, `/`, `%`, default `+`-`*`, `emitBitwiseOp`).

### in / not in dispatch order: user overload → Set → Map → List → str → error

The str branch must come AFTER Set/Map/List because `isStringValue(container)` returns true for any `ptrTy_` value without collection/resource metadata. Set/Map/List headers all have `hasAnyMeta() == true` so `isStringValue` correctly excludes them; placing str earlier would fire on plain `str` RHS before collection branches even run. When RHS is `str` and LHS is `any`, unwrap with `unwrapFromAny(elem, ptrTy_)` (no `wrapInAny` direction needed — str has no element type to promote into). Other LHS types vs str RHS: emit a clear compile error.

### FileCheck goldens (`tests/filecheck/*.ry`)

Each file is both Ry source and a FileCheck script. Use `# CHECK:` (Ry comment is `#`, never `//`). Goldens run on unoptimized IR (`alloca`/`store`/`load` for every arg are present, `mem2reg` has not run). All pointer types are `ptr` under opaque pointers — never `i64*` / `i8*`. ARC retain/release only appears in CoW clone, lambda capture, and `@parallel for` paths — pick simple goldens accordingly. Result layout: `%Result = type { i1, i64, ptr }`. Goldens are LLVM-version-sensitive — re-verify after LLVM bumps. CI job is `continue-on-error` (advisory) but fix failures before merge anyway.

### emitStringByteLen requires StringHeader-backed handles

`emitStringByteLen(handle)` reads `handle - 8` (STRING_BYTELEN_OFFSET). Safe only when `handle` came from `cachedGlobalString`, `buildArcGlobal`, or a runtime path that called `makeString` / `makeStringUninit`. `IRBuilder::CreateGlobalString` produces a plain `[N x i8]*` global without a StringHeader prefix; passing it to `emitStringByteLen` reads 8 bytes of unrelated global data as `byte_len`. Always use `cachedGlobalString` for new str constants in codegen. Raw `CreateGlobalString` is safe only for printf-style format strings that are never read back as Ry str handles.

### Collection ops on pointer elements: positive allowlist via list_elem_type_name

When a collection helper's pointer-element branch uses `strcmp` (or any C-string op), guard with a positive allowlist on `list_elem_type_name`, not a single-field blacklist via `getNestedListElementType`. The `NestedListElem` field is set only by `propagateTypeMeta`'s isListTypeName branch; Map/Set/function element kinds live in `map_value_type_name` / `list_elem_fn_type_info`. Blacklist on a single field is structurally incomplete — `List<Map<K,V>>` / `List<fn(...)>` / `List<Set<T>>` slip through and `strcmp` on Map/Set/closure headers is UB. Pattern: classify pointer elements as "non-str" if `list_elem_type_name` is non-empty and not `"str"`, OR if `nested_list_elem != nullptr`, OR if `list_elem_fn_type_info.has_value()`; emit "operation only supported for primitive / str lists". Resource lists (`List<TcpStream>`) get a non-empty type name and are caught. For Set elements (`toContain` / `toNotContain` Set branch and similar helpers), apply the analogous classification using `set_elem_type_name` and `set_elem_fn_type_info` — `ValueMetadata` has no `nested_set_elem` field, so the disjunct collapses to two: `set_elem_type_name` non-empty and not `"str"`, OR `set_elem_fn_type_info.has_value()`. Sets of nested collections still flow through these two fields via `propagateTypeMeta`.

### emitRuntimeError terminates its block — caller pre-splits to continue

`emitRuntimeError` emits `fprintf(stderr, ...)` → `exit(1)` → `unreachable` and does NOT switch to a fresh dead block. If the caller continues emitting IR in the same block, those instructions land after `unreachable` and LLVM verify rejects them. Treat `emitRuntimeError` like `CreateRet` / `CreateBr`: split into err / ok BBs before the call, set insert point to `errBB`, emit error, then set insert point to `okBB` for the happy path. Canonical examples: `emitIntZeroDivGuard`, `?` operator path in `emitExprVariant(ErrorPropagateExpr)`.

### Lowering constant-fold and emission must agree on integer interpretation (i1 sext vs zext)

When a lowered op (`include/ry/codegen/lowered_*.hpp`) splits into a compile-time constant-fold branch (lowering side) and a runtime IR-construction branch (emission side), both must interpret each ConstantInt operand the same way. If emission ZExt-extends `i1 → i64` (so `true` becomes `1`), the lowering fold must read `i1` operands with `getZExtValue()` — switching to `getSExtValue()` only for widths > 1. `getSExtValue()` on `i1` returns `-1` for `true`, which silently wraps to `len - 1` in fold but emits `1` at runtime, breaking the bit-exact extract that the lowering/emission split is supposed to preserve. Canonical example: `lowerBoundsCheck` (`src/codegen_lowering_bounds_check.cpp`) keeps `getSExtValue()` for wider widths and switches to `getZExtValue()` when `getBitWidth() == 1`, mirroring `emission::emitBoundsCheck`'s `CreateZExt(idx, i64Ty_)`. Apply the same rule to any future `lowering::lower<Op>` that constant-folds two ConstantInt operands.
