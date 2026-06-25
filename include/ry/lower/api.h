// Upper codegen (Ry semantic lowering) boundary — kickoff pilot (#2397).
//
// Sibling boundary to `include/ry/llvm_emit/api.h`: this header declares
// the `ry_lower_*` entries exposed by `crates/lower/`. Caller-side C++
// in `src/codegen_*.cpp` calls `ry_lower_*`; the Rust impl calls
// `ry_emit_*` (`api.h`) to construct LLVM IR.
//
// Boundary discipline (same as emit's api.h):
//   - Opaque handles only — no `llvm::*` types.
//   - All signatures `extern "C"` callable.
//   - The lower crate does NOT link `llvm-sys`; the `ry_emit_*`
//     symbols it calls are resolved at runtime from the loaded `emit`
//     cdylib via the host process's symbol table.
//
// See `docs/architecture/upper-codegen-migration.md` for the migration
// design hypothesis and the per-stage boundary growth plan.

#ifndef RY_LOWER_API_H
#define RY_LOWER_API_H

#include "ry/llvm_emit/api.h"

#ifdef __cplusplus
extern "C" {
#endif

// Pilot: lower a Ry `BoolExpr` (`true` / `false`) to its LLVM `i1`
// constant. The caller supplies the `i1` type (`i1Ty_` from `CodeGen`)
// as `i1_ty`; the implementation does not synthesize types. `value` is
// a C-int (any non-zero is `true`, mirroring the `int sign_extend`
// convention used across `ry_emit_*`).
//
// Returns the interned `RyValueId` of `LLVMConstInt(i1, value ? 1 : 0,
// false)`. NULL ctx / type → 0 (delegated to `ry_emit_const_int`).
//
// This is the C++-callable replacement for the inline
// `llvm::ConstantInt::get(i1Ty_, e.value ? 1 : 0, false)` previously in
// `CodeGen::emitExprVariant(const BoolExpr &)`.
RyValueId ry_lower_bool_const(RyEmitCtx *ctx, RyTypeRef i1_ty, int value);

#ifdef __cplusplus
}
#endif

#endif // RY_LOWER_API_H
