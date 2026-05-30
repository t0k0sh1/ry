// ABI surface for the LLVM IR emission shared library (libry_llvm_emit).
//
// This header is the candidate boundary between Ry semantic lowering and
// LLVM IR emission (designed in #1820/#1824, implemented incrementally
// starting from #1949). It is C-only so the layer can be reimplemented in
// Rust (#1950) behind the same ABI.
//
// Stage 2-A (scaffolding, #1949):
//   - Opaque handles for LLVM values are defined as `uint32_t` (RyValueId).
//   - Three "category-3" helpers from llvm-ir-emission-boundary.md cross
//     the ABI: ry_emit_get_runtime_fn, ry_emit_build_error_from_runtime,
//     and the BoundsCheck op (ry_emit_bounds_check).
//   - The other two category-3 helpers (wrapPtrAsResult / wrapStatusAsResult)
//     stay as CodeGen methods in this PR because their bodies pull in
//     getResultType (a pointer-identity-sensitive cache), buildOkValue /
//     buildErrValue (which call propagateMeta + tryRetainArcSource), and
//     emitResultBranch (which builds BBs + PHI from caller-supplied
//     callbacks). Migrating them belongs in the same successor PR that
//     ABI's the `ResultWrap` lowered op.
//   - Module / IRBuilder / LLVMContext / Function pointers are still passed
//     as `void*` because categories 1 (LLVM context handles) and 2 (primitive
//     type accessors) have not crossed the ABI yet. Successor PRs will
//     replace these with opaque handles.
//   - The currently-unmigrated callbacks (emitNegativeIndexWrap,
//     emitBoundsError) are reachable through the RyEmitCallbacks slot. This
//     is a transitional shape; future PRs will turn each callback into a
//     proper ABI function.
//
// Constraint enforced for every signature in this header (per #1824 AC):
//   LLVM-owned types (`llvm::Value*`, `llvm::Module&`, `llvm::IRBuilder<>`,
//   `llvm::Function*`, `llvm::BasicBlock*`, `llvm::Type*`, `llvm::Constant*`)
//   must NOT appear in any public signature here. Use opaque handles or
//   transitional `void*` parameters tagged in comments.
//
// Symbol naming:
//   `ry_emit_*` for emission-layer ABI functions. This is distinct from the
//   `__ry_*` runtime prefix used by JIT-resolved runtime symbols
//   (see `runtime-abi-boundary.md`); the two boundaries are orthogonal.

#ifndef RY_LLVM_EMIT_API_H
#define RY_LLVM_EMIT_API_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// Opaque handle representing an LLVM Value. Value 0 is the sentinel "invalid"
// handle; ry_emit_resolve(_, 0) returns NULL. IDs are monotonically allocated
// per RyEmitCtx and are not reused for the lifetime of the context.
typedef uint32_t RyValueId;

// Opaque handle for an LLVM FunctionCallee. Reserved for future use; the
// scaffold uses transitional void* in places that need a callee.
typedef uint32_t RyFuncId;

// Opaque handle for an LLVM Type. Reserved for future use.
typedef uint32_t RyTypeId;

// Opaque per-compile() emission context.
typedef struct RyEmitCtx RyEmitCtx;

// Bounds-check kind selector. Numbers chosen to match the order in
// `lowered::BoundsKind`; do not reorder without updating both sides.
typedef enum {
    RY_BOUNDS_LIST = 0,
    RY_BOUNDS_ARRAY = 1
} RyBoundsKind;

// Callback table for CodeGen-owned helpers that have not crossed the ABI yet.
// Future PRs will replace each callback with a `ry_emit_*` function and shrink
// this struct. user_ctx is opaque to the emission layer; CodeGen passes its
// own `this`. Callback inputs/outputs use RyValueId; thunks on the CodeGen
// side perform the intern/resolve.
typedef struct RyEmitCallbacks {
    void *user_ctx;
    RyValueId (*emit_negative_index_wrap)(void *user, RyValueId idx,
                                          RyValueId len, const char *prefix);
    void (*emit_bounds_error)(void *user, RyValueId orig_idx, RyValueId len,
                              const char *fmt_msg, const char *global_name);
} RyEmitCallbacks;

// Lifecycle. Create at the top of CodeGen::compile() and destroy on exit.
// module_ptr/builder_ptr/context_ptr/function_ptr are `void*` only as a
// transitional shape; they will become opaque handles in a successor PR.
RyEmitCtx *ry_emit_ctx_create(void *module_ptr, void *builder_ptr,
                              void *context_ptr, void *function_ptr);
void ry_emit_ctx_destroy(RyEmitCtx *ctx);

// Update the current LLVM function pointer mid-compile (function_ptr changes
// when CodeGen emits a new function body).
void ry_emit_ctx_set_function(RyEmitCtx *ctx, void *function_ptr);

// Register CodeGen-side callbacks. May be called at any time; the most recent
// values are used by subsequent ABI calls that need them.
void ry_emit_ctx_set_callbacks(RyEmitCtx *ctx, const RyEmitCallbacks *cbs);

// Handle marshalling. ry_emit_intern returns 0 if `value_ptr` is NULL; every
// other value gets a fresh handle. ry_emit_resolve returns NULL for handle 0
// and for any out-of-range handle.
RyValueId ry_emit_intern(RyEmitCtx *ctx, void *value_ptr /* llvm::Value* */);
void *ry_emit_resolve(RyEmitCtx *ctx, RyValueId id);

// Category-3 helpers (from llvm-ir-emission-boundary.md). Each one wraps a
// canonical emission pattern as an atomic ABI call so the LLVM-side BB / PHI
// plumbing stays inside the shared library.

// Build an Error struct by calling err_fn_name (a `const char *(*)()` runtime
// symbol) and packaging the returned pointer with a zero code. error_ty_ptr
// is the LLVM StructType handle (transitional `void*` until categories 1/2
// cross the ABI); the scaffold accepts the value `CodeGen::errorTy_` resolves
// to.
RyValueId ry_emit_build_error_from_runtime(RyEmitCtx *ctx,
                                           const char *err_fn_name,
                                           void *error_ty_ptr /* llvm::StructType* */);

// Resolve or insert a runtime symbol with the given LLVM FunctionType. The
// caller constructs the FunctionType (still LLVM-side until categories 1/2
// cross the ABI) and pairs the returned `Value*` callee with it to
// reconstruct an `llvm::FunctionCallee` struct.
//   fn_ty_ptr : llvm::FunctionType*
// Returns the callee operand (`llvm::Value*`) suitable for the FunctionCallee
// ctor; never NULL because `getOrInsertFunction` always synthesizes a callee.
void *ry_emit_get_runtime_fn(RyEmitCtx *ctx, const char *name, void *fn_ty_ptr);

// Emit a bounds-check IR sequence. Inputs are the un-wrapped index and the
// container length; the return value is the wrapped index suitable for the
// caller's subsequent GEP. emit_negative_index_wrap and emit_bounds_error
// are dispatched through the RyEmitCallbacks slot until those helpers cross
// the ABI in a successor PR.
RyValueId ry_emit_bounds_check(RyEmitCtx *ctx, RyValueId idx_id,
                               RyValueId len_id, RyBoundsKind kind,
                               const char *global_name,
                               const char *bb_prefix);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // RY_LLVM_EMIT_API_H
