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

// =========================================================================
// Stage 2 (#2484) — string / regex literal lowering.
//
// `StringExpr` / `RegexExpr` lowering moves from C++
// (`src/codegen_expr.cpp:113-124`) to the lower crate. The C++ side's
// `global_string_cache_` / `regex_global_cache_` pair is replaced by a
// thread-local cache pair owned by `crates/lower/src/intern.rs`, keyed on
// the literal byte content (`Vec<u8>` — binary-safe, including embedded
// NULs). Two SEPARATE caches preserve the regex separation invariant from
// #306: a string `"hello"` and a regex `/hello/` MUST produce distinct
// LLVM globals so the C++-side `addResourceKind(rk_regex)` (still applied
// in the regex shim) cannot poison a same-bytes string literal reached
// elsewhere in the program.
//
// The ARC global construction itself stays on the emit side
// (`ry_emit_build_arc_global`) — only the cache-and-key-by-bytes layer
// moved.
// =========================================================================

// Lower a Ry `StringExpr` to an LLVM ARC-immortal global and return the
// interned `RyValueId` of the GEP-to-first-payload-byte pointer. `bytes`
// is the literal content; `len` is the authoritative byte count
// (binary-safe — embedded NULs preserved). `name_hint_bytes[0..
// name_hint_len)` is the LLVM global name prefix (".str", ".fmt",
// ".err_msg", …) passed by length, NOT as a NUL-terminated C string.
// `llvm::Twine::toStringRef` does not guarantee a NUL terminator for
// multi-node Twines, so the `cachedGlobalString` wrapper forwards the
// StringRef length explicitly rather than relying on a NUL contract.
//
// Cache hit returns the cached id; cache miss calls
// `ry_emit_build_arc_global` and stores the result.
//
// No error path under normal operation. Returns 0 only when `bytes` is
// NULL with non-zero `len`, when `name_hint_bytes` is NULL with non-zero
// `name_hint_len`, or when the underlying emit call fails on a NULL
// ctx. `ry_lower_get_last_error` is not populated on this path.
RyValueId ry_lower_string_const(RyEmitCtx *ctx, const uint8_t *bytes,
                                size_t len,
                                const uint8_t *name_hint_bytes,
                                size_t name_hint_len);

// Lower a Ry `RegexExpr`. Same shape as `ry_lower_string_const` but
// uses an INDEPENDENT cache (`REGEX_CACHE`) and a hardcoded `.regex`
// name hint. The C++ shim retains `addResourceKind(gs, rk_regex)` after
// resolving the returned id — `value_metadata_` operations belong to
// the C++ side until Stage 5.
RyValueId ry_lower_regex_const(RyEmitCtx *ctx, const uint8_t *bytes,
                                size_t len);

// Clear the per-module thread-local string / regex caches. MUST be
// called from `CodeGen::CodeGen()` immediately after `emit_ctx_create`.
// Sequential `CodeGen` instances on the same thread would otherwise
// resolve cached `RyValueId`s against the previous (now-dead)
// `RyEmitCtx`, producing miscompiles or null-pointer crashes.
//
// The `ctx` parameter is currently unused (the caches are thread-local,
// not per-ctx), but is part of the boundary so a future per-ctx
// migration can change ownership without breaking the C++ caller.
void ry_lower_reset_module_state(RyEmitCtx *ctx);

#ifdef __cplusplus
}
#endif

#endif // RY_LOWER_API_H
