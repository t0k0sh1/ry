---
paths:
  - "src/codegen_expr.cpp"
  - "src/codegen_expr_cast.cpp"
  - "src/codegen_builtin.cpp"
  - "src/codegen.cpp"
  - "src/codegen_test.cpp"
  - "src/codegen_call_user.cpp"
  - "src/codegen_call_collection.cpp"
  - "src/jit/jit_runner.cpp"
  - "src/app/main.cpp"
  - "crates/emit/src/**/*.rs"
  - "tests/filecheck/**/*.ry"
---

# Codegen LLVM IR Conventions

### emitArithmeticOp dispatch order: new type-specific branches go BEFORE the str-vs-non-str reject

`isStringValue()` returns true for any `ptrTy_` value without `hasAnyMeta()` — effectively "unknown pointer". Branches added AFTER the str-vs-non-str reject are unreachable for metadata-less pointer operands. Insert new type-specific branches (List concat, Map merge, Set union, etc.) BEFORE the reject.

### Bool reject in arithmetic / bitwise must be at each operator branch entry, not at function entry

`promoteToInt` and `promoteToFloat` silently widen `i1` via ZExt / SIToFP. Do NOT gate inside those helpers (ARC and string-repeat paths intentionally widen i1), and NOT at function entry (str concat / repeat processes `"x" + true` before numeric branches). Place `rejectBoolInOperator` at the entry of each numeric / bitwise branch.

### in / not in dispatch order: user overload → Set → Map → List → str → error

The str branch must come AFTER Set/Map/List because `isStringValue(container)` returns true for any `ptrTy_` value without collection/resource metadata. Placing str earlier would fire on plain `str` RHS before collection branches even run.

### FileCheck goldens (`tests/filecheck/*.ry`)

Goldens run on unoptimized IR (`alloca`/`store`/`load` present, `mem2reg` not run). All pointer types are `ptr` — never `i64*` / `i8*`. Goldens are LLVM-version-sensitive — re-verify after LLVM bumps.

Verifying a pure refactor is IR-byte-identical: normalize the ASLR-randomized `__ry_arc_counter_address` (`inttoptr (i64 <addr> to ptr)`) before diffing — otherwise the diff shows false positives on every ARC path. Capture the before/after diff after applying `rustfmt` / `clippy --fix`, not before. Probe-input resolution trap (#2082): `./build-rust/ry --emit-llvm-ir /tmp/probe.ry` resolves the global `~/.ry/` stdlib — feed via stdin from the repo root instead.

**Coverage traps** (empty diff ≠ correct migration):
- An untaken emission path yields a false pass — grep the baseline IR for each block's marker before trusting the diff.
- Constant operands (`sum(1, 2, 3)`) fold at `LLVMBuild*` time, emitting no arithmetic IR — force non-constant loads.
- A shared emission helper with multiple layout×shape cells is a multi-cell coverage trap — enumerate and marker-confirm each cell.
- `emitLambdaCall` has six cells (three calling-convention layouts × two return shapes); `emitSetElementLookup` has hash-path AND linear-scan cells; `emitIntOverflowCheck` has a compile-time APInt fold path that bypasses `Intrinsic::getOrInsertDeclaration` entirely when both operands are `ConstantInt`.

Do not assume a Rust `core::*` helper reproduces its C++ namesake's instruction order — `composite::header::load_list_header` groups geps then loads, whereas `CodeGen::loadListHeader` interleaves them. Confirm against the baseline IR.

### emitStringByteLen requires StringHeader-backed handles

`emitStringByteLen(handle)` reads `handle - 8`. Safe only when `handle` came from `cachedGlobalString`, `buildArcGlobal`, or a runtime path that called `makeString` / `makeStringUninit`. `IRBuilder::CreateGlobalString` produces a plain `[N x i8]*` global without a StringHeader prefix — passing it reads 8 bytes of unrelated global data as `byte_len`.

### Collection ops on pointer elements: positive allowlist via list_elem_type_name

Blacklist on a single field (`getNestedListElementType`) is structurally incomplete — `List<Map<K,V>>` / `List<fn(...)>` / `List<Set<T>>` slip through and `strcmp` on Map/Set/closure headers is UB.

### emitRuntimeError terminates its block — caller pre-splits to continue

`emitRuntimeError` emits `fprintf` → `exit(1)` → `unreachable` and does NOT switch to a fresh dead block. Treat it like `CreateRet` / `CreateBr`: split into err / ok BBs before the call, emit error in `errBB`, continue in `okBB`.

### Compile-time fold and boundary emission must agree on integer interpretation (i1 sext vs zext)

`getSExtValue()` on `i1` returns `-1` for `true`, which silently wraps to `len - 1` in fold but emits `1` at runtime. When the boundary ZExt-extends `i1 → i64`, the fold must use `getZExtValue()` — switch to `getSExtValue()` only for widths > 1.

### ARC weak_count: a non-atomic read paired with atomic RMW writes is a TSan race even on the release path

Reading `weak_count` (or any field updated by `atomicrmw` elsewhere) with a plain `CreateLoad` is a data race per the C++ memory model — TSan flags it even when reasoning suggests the read cannot observe a torn value. Any read of an atomically-updated ARC header field must use an atomic load.

### Atomic primitives are generic LLVM ops; ordering / binop selection stays C++-side (#2190)

The caller (C++) picks the ordering and the binop. The C-side enum order must match `RY_ATOMIC_*` constants in `crates/emit/src/abi.rs` and `include/ry/llvm_emit/api.h` — parity is enforced by paired C++ `static_assert` + Rust `const _: () = assert!`.

### llvm_emit boundary helpers must derive parent function from the builder, not from ctx->function

The cached `ctx->function` reflects `cg.fn_` at set-time; ARC / lambda / thunk / destructor / iterator-next codegen retargets `builder_` to a nested function without mirroring the change into `cg.fn_`. A boundary helper that creates new `BasicBlock`s using `ctx->function` then creates BBs in the outer function while the builder appends in the nested function — LLVM verify rejects. The `ry_emit_ctx_set_function` cached field was removed in #2083 — derive parent from `ctx->builder->GetInsertBlock()->getParent()` unconditionally.

### Re-entrant boundary callbacks move to `core` as a free function with closures, never a `&mut self` method

A boundary extern whose work includes a re-entrant C callback must NOT relocate into a `&mut self` `core` method. A `&mut self` method holds the receiver borrow across the callback call; the callback's `cx(ctx)` creates a second `&mut EmitCtx` aliasing the receiver — UB under Stacked/Tree Borrows the borrow checker cannot see. Move into a free function taking decomposed Copy handles + `&mut dyn FnMut()` closures. Each closure body must be two-step: call the callback first, then `resolve` — inlining `resolve(cx(ctx), build_ok(user_ctx))` re-introduces the alias because arguments evaluate left-to-right.

### `emitRuntimeError` and `bounds_error` look alike but differ in BOTH global shape and instruction order — don't reuse one helper for the other (#2097)

C++ `emitRuntimeError` uses a **StringHeader-prefixed** global named with a `.arc` suffix (via `buildArcGlobal`), with instruction order **stderr load → fprintf → stdout load → fflush stdout → fflush stderr → _Exit → unreachable**. The Rust `bounds_error` uses a plain `[N+1 x i8]` global (no struct prefix, no `.arc` suffix) and loads stderr / stdout adjacently before fprintf. For an `emitRuntimeError`-shaped exit, use `get_or_create_arc_msg_global` and a dedicated free fn that preserves the C++ instruction order. Do NOT call into `bounds_error` from the new helper.

### Rust mirrors of C++ header structs need a cross-language parity guard, not a "keep in sync" comment

A Rust-only `#[cfg(test)]` self-check is insufficient — CI's `lint` job runs no `cargo test`. Two guards are mandatory: (1) single-source the shape and assert it against C++ canonical types from a real `CodeGen` in a C++ test (`tests/test_header_layout.cpp`); (2) behavioral same-type-swap coverage — all header fields are 8 bytes, so a `len↔cap` swap is invisible to layout checks; only deep-copy + read-back catches it.

### Values used only in one branch of a conditional must be emitted inside the target BB (lazy default rule)

**Tags**: codegen, branching, PHI, lazy-evaluation, short-circuit, conditional-emission, emitCollOp_get

Emitting a value used only in one PHI incoming in the dominator block before the branch causes side-effecting expressions (function calls, resource allocation, etc.) to execute unconditionally on all paths. Re-capture the PHI predecessor (`notFoundEndBB` / `oobEndBB`) via `builder_.GetInsertBlock()` after `emitBranchUncond(mergeBB)` — if the `default` expression internally creates new BBs, a handle captured before the call references the wrong BB and LLVM verify rejects the PHI predecessor.
