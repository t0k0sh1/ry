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

// =========================================================================
// Stage 1 (#2483) — primitive literal lowering.
//
// `NumberExpr` / `FloatExpr` lowering moves from C++
// (`src/codegen_expr.cpp:88-121`) to the lower crate. Suffix-based type
// selection is still resolved on the C++ side (the LLVM type registry
// moves to Rust at Stage 5); the suffix kind is passed as an enum so the
// Rust side can apply the correct range-check arm and frame diagnostics.
// =========================================================================

// Suffix kind discriminator for the int / float const entries. Numeric
// values are a stable boundary contract; do NOT reorder without updating
// the mirroring `SuffixKind` enum in `crates/lower/src/expr.rs`.
typedef enum RyLowerSuffixKind {
    RY_LOWER_SUFFIX_NONE = 0,
    RY_LOWER_SUFFIX_I8   = 1,
    RY_LOWER_SUFFIX_I16  = 2,
    RY_LOWER_SUFFIX_I32  = 3,
    RY_LOWER_SUFFIX_I64  = 4,
    RY_LOWER_SUFFIX_U8   = 5,
    RY_LOWER_SUFFIX_U16  = 6,
    RY_LOWER_SUFFIX_U32  = 7,
    RY_LOWER_SUFFIX_U64  = 8,
    RY_LOWER_SUFFIX_F32  = 9,
    RY_LOWER_SUFFIX_F64  = 10,
} RyLowerSuffixKind;

// Lower a Ry `NumberExpr` to an LLVM integer constant. `llvm_ty` is the
// LLVM type the caller resolved from the suffix (`resolveType(e.suffix)`,
// or `i64Ty_` when `e.suffix` is empty); `magnitude` is the parser's
// unsigned bit pattern (negative literals arrive as `UnaryExpr` and are
// handled by a C++ fast-path before reaching this entry). `suffix_kind`
// is one of `RY_LOWER_SUFFIX_*`; `signed_bit` is non-zero in signed
// contexts (matches the original `ConstantInt::get(.., isSigned)` flag).
//
// On range overflow or bare-int negative bit-pattern, returns 0 and
// populates a thread-local diagnostic string retrievable via
// `ry_lower_get_last_error`. The caller MUST check the return value:
//
//     RyValueId id = ry_lower_int_const(...);
//     if (id == 0) {
//         const char *msg = ry_lower_get_last_error();
//         codegenError(msg ? std::string(msg) : "lower: unspecified error");
//     }
RyValueId ry_lower_int_const(RyEmitCtx *ctx, RyTypeRef llvm_ty,
                             uint64_t magnitude, uint8_t suffix_kind,
                             uint8_t signed_bit);

// Lower a Ry `FloatExpr` to an LLVM floating-point constant. `llvm_ty`
// is the LLVM type the caller selected (`f64Ty_` for empty / `f64`
// suffix, `f32Ty_` for `f32`); `value` is the parser's double.
// `suffix_kind` must be `RY_LOWER_SUFFIX_NONE`, `RY_LOWER_SUFFIX_F32`,
// or `RY_LOWER_SUFFIX_F64`.
//
// No range check (IEEE 754 overflow to infinity is accepted, matching
// the C++ side). Has no error path under normal operation; the
// `ry_lower_get_last_error` channel is consulted only if the caller
// passes a non-float `suffix_kind` (protocol break).
RyValueId ry_lower_float_const(RyEmitCtx *ctx, RyTypeRef llvm_ty,
                               double value, uint8_t suffix_kind);

// Retrieve the most recently set error message for this thread, or NULL
// if none was set since the last `ry_lower_*` entry was called. The
// returned pointer is valid ONLY until the next `ry_lower_*` call on
// the same thread (which clears the slot at entry — see the Stage 1
// boundary contract in `docs/architecture/upper-codegen-migration.md`).
// The caller MUST copy the string before issuing any other boundary
// call.
//
// Cross-stage contract: `RyValueId = 0` from any `ry_lower_*` entry that
// supports error reporting means "this call failed". The paired
// `ry_lower_get_last_error()` return value is always fresh (set inside
// the failing call), never stale.
const char *ry_lower_get_last_error(void);

#ifdef __cplusplus
}
#endif

#endif // RY_LOWER_API_H
