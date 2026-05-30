// ABI surface for the LLVM IR emission shared library (libry_llvm_emit).
//
// This header is the candidate boundary between Ry semantic lowering and
// LLVM IR emission (designed in #1820/#1824, implemented incrementally
// starting from #1949). It is C-only so the layer can be reimplemented in
// Rust (#1950) behind the same ABI.
//
// Stage 2-C (#1968) begins per-op migration of category-1 ARC primitives:
//   - ry_emit_arc_retain / ry_emit_arc_release cross the ABI. The destructor
//     parameter is reduced to a single `void *destructor_callee` (the C
//     function pointer); the FunctionType `void(*)(void*)` is reconstructed
//     inside the emission layer. gc_visit_fn is nullable; when NULL the
//     `arc.gc_track` BB is omitted entirely. The atomic flag flows through
//     as `int` (RyArcAtomic), preserving the @parallel for SeqCst contract.
//   - The arcHeader StructType is generated locally inside the emission
//     layer (anonymous `{i64, i64}`); CodeGen's named "ArcHeader" struct is
//     retired. FileCheck CHECK patterns never reference the named struct,
//     so this is a private representation change.
//
// Stage 2-B (#1964) completes the category-3 migration:
//   - All five category-3 helpers from llvm-ir-emission-boundary.md cross
//     the ABI: ry_emit_get_runtime_fn, ry_emit_build_error_from_runtime,
//     ry_emit_bounds_check, ry_emit_result_branch, ry_emit_negative_index_wrap,
//     and ry_emit_bounds_error. wrapPtrAsResult / wrapStatusAsResult are
//     thin wrappers over ry_emit_result_branch on the CodeGen side and do
//     not need their own ABI entry.
//   - getResultType's StructType cache stays in CodeGen (its reverse map
//     `reverse_result_types_` is consumed in codegen_any.cpp/codegen_arc.cpp
//     for ARC release and Any wrapping); resTy crosses as `void*`, mirroring
//     errorTy_ in ry_emit_build_error_from_runtime.
//   - The previous `RyEmitCallbacks` slot is removed — negative-index wrap
//     and bounds error are now proper ABI functions rather than callbacks
//     into CodeGen.
//
// Stage 2-C (in progress, #1965) begins migrating the remaining lowered IR
// ops. The first step (#1967) adds two entry points for OptionWrap —
// ry_emit_option_wrap_some and ry_emit_option_wrap_none. Unlike the BB-
// creating ops above, these do not require ry_emit_ctx_set_function to be
// called first (they emit no basic blocks). getOptionType's StructType
// cache stays in CodeGen for the same reason as getResultType (the
// reverse_option_types_ map is consumed by ARC release / Any wrap on the
// CodeGen side); opt_ty crosses the ABI as `void*`, mirroring resTy_.
//
// Stage 2-A (scaffolding, #1949):
//   - Opaque handles for LLVM values are defined as `uint32_t` (RyValueId).
//   - Module / IRBuilder / LLVMContext / Function pointers are still passed
//     as `void*` because categories 1 (LLVM context handles) and 2 (primitive
//     type accessors) have not crossed the ABI yet. Successor PRs (Stage 2-C
//     and beyond) will replace these with opaque handles.
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

// ARC atomic mode selector. RY_ARC_NONATOMIC uses plain Load/Store/Sub for
// counter updates; RY_ARC_ATOMIC uses atomicrmw with SequentiallyConsistent
// ordering (required inside @parallel for thunks because captured values are
// shared across workers). The immortal-check load stays Monotonic in both
// modes (it tolerates stale reads — INT64_MAX never changes).
typedef enum {
    RY_ARC_NONATOMIC = 0,
    RY_ARC_ATOMIC = 1
} RyArcAtomic;

// Callback type for ok/err value builders consumed by ry_emit_result_branch.
// Stage 2-B keeps callbacks at the C ABI boundary (function pointer +
// user_ctx) — the C++ side translates `llvm::function_ref<>` closures into
// this shape via a trampoline. user_ctx is opaque to the emission layer.
typedef RyValueId (*RyBuildValueFn)(void *user_ctx);

// Lifecycle. Create at the top of CodeGen::compile() and destroy on exit.
// module_ptr/builder_ptr/context_ptr/function_ptr are `void*` only as a
// transitional shape; they will become opaque handles in a successor PR.
RyEmitCtx *ry_emit_ctx_create(void *module_ptr, void *builder_ptr,
                              void *context_ptr, void *function_ptr);
void ry_emit_ctx_destroy(RyEmitCtx *ctx);

// Update the current LLVM function pointer mid-compile (function_ptr changes
// when CodeGen emits a new function body).
void ry_emit_ctx_set_function(RyEmitCtx *ctx, void *function_ptr);

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
// caller's subsequent GEP. Internally invokes ry_emit_negative_index_wrap
// for the wrap step and ry_emit_bounds_error for the OOB exit (both proper
// ABI functions since Stage 2-B).
// Precondition: ry_emit_ctx_set_function must have been called with the
// current LLVM function before this call (BBs are created inside it).
RyValueId ry_emit_bounds_check(RyEmitCtx *ctx, RyValueId idx_id,
                               RyValueId len_id, RyBoundsKind kind,
                               const char *global_name,
                               const char *bb_prefix);

// Emit a Result-branch IR sequence: three BBs (res.ok / res.err / res.merge)
// joined by a PHI. build_ok and build_err run inside okBB / errBB
// respectively (the helper switches the builder's insert point before each
// callback). user_ctx is forwarded to both callbacks unchanged.
// res_ty_ptr is the LLVM StructType handle for Result<T, E>, passed as
// `void*` until the type-handle category crosses the ABI (mirrors
// error_ty_ptr in ry_emit_build_error_from_runtime).
// Returns the PHI handle holding the merged Result value.
// Precondition: ry_emit_ctx_set_function must have been called with the
// current LLVM function before this call (three BBs are created inside it).
RyValueId ry_emit_result_branch(RyEmitCtx *ctx, RyValueId is_err_id,
                                void *res_ty_ptr /* llvm::StructType* */,
                                RyBuildValueFn build_ok,
                                RyBuildValueFn build_err,
                                void *user_ctx);

// Emit a negative-index wrap sequence (used by string/list/array index
// expressions): wrapped = (idx < 0) ? idx + wrap_base : idx. Returns the
// wrapped index handle (i64).
RyValueId ry_emit_negative_index_wrap(RyEmitCtx *ctx, RyValueId idx_id,
                                      RyValueId wrap_base_id,
                                      const char *prefix);

// Emit a bounds-error exit sequence: fprintf(stderr, fmt_msg, orig_idx, len)
// → fflush(stdout) → fflush(stderr) → _Exit(1) → unreachable. fmt_msg must
// contain two %lld format specifiers for orig_idx and len. global_name is
// used as the LLVM global name hint for the format-string global; the
// emission layer deduplicates identical fmt_msg strings within RyEmitCtx so
// repeated calls do not generate redundant globals.
// The caller is responsible for splitting BBs around this call (this helper
// terminates the current block with `unreachable`).
void ry_emit_bounds_error(RyEmitCtx *ctx, RyValueId orig_idx_id,
                          RyValueId len_id, const char *fmt_msg,
                          const char *global_name);

// Stage 2-C entry — Option<T> Some-arm construction.
// Builds UndefValue(opt_ty) + InsertValue(tag=1, 0) + InsertValue(inner, 1)
// and returns a handle to the resulting aggregate. opt_ty_ptr is the LLVM
// StructType handle for Option<T> (transitional `void*` until the type-handle
// category crosses the ABI; mirrors res_ty_ptr in ry_emit_result_branch).
// inner_id must resolve to a non-null payload value whose type matches
// opt_ty's element-1 slot. Creates no basic blocks, no precondition on
// ry_emit_ctx_set_function.
RyValueId ry_emit_option_wrap_some(RyEmitCtx *ctx, RyValueId inner_id,
                                   void *opt_ty_ptr /* llvm::StructType* */);

// Stage 2-C entry — Option<T> None-arm construction.
// Builds UndefValue(opt_ty) + InsertValue(tag=0, 0) + InsertValue(
// UndefValue(opt_ty->getElementType(1)), 1). Creates no basic blocks, no
// precondition on ry_emit_ctx_set_function.
RyValueId ry_emit_option_wrap_none(RyEmitCtx *ctx,
                                   void *opt_ty_ptr /* llvm::StructType* */);

// Emit an ARC retain on header_ptr (`{i64 strong_count, i64 weak_count}`):
//   if (strong_count == INT64_MAX) goto arc.retain.done;  // immortal sentinel
//   else strong_count += 1; goto arc.retain.done;
// The immortal check uses a Monotonic load in both atomic modes; the
// increment uses atomicrmw add SeqCst when atomic == RY_ARC_ATOMIC, plain
// Load/Add/Store otherwise. BB labels are emitted as `arc.retain` and
// `arc.retain.done` (FileCheck-stable).
// Precondition: ry_emit_ctx_set_function must have been called with the
// current LLVM function before this call (BBs are created inside it).
void ry_emit_arc_retain(RyEmitCtx *ctx, RyValueId header_ptr_id,
                        RyArcAtomic atomic);

// Emit an ARC release on header_ptr:
//   if (strong_count == INT64_MAX) goto arc.done;                // immortal
//   old = strong_count.dec(); is_dead = (old == 1);
//   if (gc_visit_fn && !is_dead) {
//       arc.gc_track: __ry_gc_track(header_ptr, gc_visit_fn, destructor);
//       goto arc.done;
//   } else if (!is_dead) goto arc.done;
//   arc.release: __ry_gc_untrack(header_ptr);
//                if (destructor) destructor(header_ptr + ARC_HEADER_SIZE);
//                if (weak_count == 0) goto arc.free; else goto arc.skip_free;
//   arc.free: __ry_arc_counter_delta(-1); free(header_ptr); goto arc.done;
//   arc.skip_free: goto arc.done;
// destructor_callee may be NULL (skip destructor call); gc_visit_fn may be
// NULL (skip the arc.gc_track BB entirely, dead → arc.release, alive → done).
// Both pointers are C function pointers (`void (*)(void *)` for destructor,
// `void (*)(void *)` for gc_visit_fn) — the emission layer reconstructs the
// FunctionType locally so no LLVM type identity crosses the ABI.
// Precondition: ry_emit_ctx_set_function must have been called with the
// current LLVM function before this call (BBs are created inside it). The
// caller must additionally register "gc" in CodeGen.used_native_libraries_
// because release emits __ry_gc_track / __ry_gc_untrack calls.
void ry_emit_arc_release(RyEmitCtx *ctx, RyValueId header_ptr_id,
                         RyArcAtomic atomic, void *destructor_callee,
                         void *gc_visit_fn);

// Stage 2-C entry — RuntimeCall (#1969).
// Resolve `name` against `mod_->getOrInsertFunction(name, fnTy)` where fnTy
// is constructed from `ret_ty_ptr` + `arg_ty_ptrs[0..arg_ty_count]`
// (non-variadic), and emit a single `CreateCall` against it with the
// argument handles supplied via `arg_ids[0..arg_count]`. Returns the call
// result as a RyValueId — for void-returning calls, the handle still
// resolves to the CallInst itself (callers that need to distinguish void
// results should inspect ret_ty_ptr).
//
// `name_hint` is the LLVM SSA name hint for the call result; pass NULL for
// no hint (matches the third positional argument of
// `IRBuilder<>::CreateCall(callee, args, name)`).
//
// Creates no basic blocks, no precondition on ry_emit_ctx_set_function.
//
// Categories 1/2 not migrated yet: ret_ty_ptr (llvm::Type *) and each
// arg_ty_ptrs[i] (llvm::Type *) cross as transitional void* (mirrors
// error_ty_ptr / res_ty_ptr in existing helpers). #1973 (ControlFlow +
// category 1/2) replaces them with typed opaque handles.
RyValueId ry_emit_runtime_call(RyEmitCtx *ctx, const char *name,
                               void *ret_ty_ptr /* llvm::Type * */,
                               void *const *arg_ty_ptrs /* llvm::Type *[] */,
                               uint32_t arg_ty_count,
                               const RyValueId *arg_ids,
                               uint32_t arg_count, const char *name_hint);

// Stage 2-C entry — List append (in-place mutation):
//   len = list.len; cap = list.cap;
//   if (len >= cap) {
//     append.grow: data = __ry_list_grow(list_ptr, elem_size); goto append.store;
//   } else {
//     append.nogrow: data = list.data; goto append.store;
//   }
//   append.store: *(data + len * elem_size) = val; list.len = len + 1;
// list_header_ty_ptr is the `{i64 len, i64 cap, ptr data}` StructType.
// ARC retain on `val` (if the element type warrants it) MUST be emitted by
// the caller BEFORE invoking this ABI — append does not retain internally.
// Precondition: ry_emit_ctx_set_function must have been called with the
// current LLVM function before this call (BBs are created inside it).
void ry_emit_collection_append(RyEmitCtx *ctx, RyValueId list_ptr_id,
                               RyValueId val_id,
                               void *list_header_ty_ptr /* llvm::StructType* */,
                               void *elem_ty_ptr /* llvm::Type* */,
                               uint64_t elem_size);

// Stage 2-C entry — List insert at index (in-place mutation with shift):
//   wrapped_idx = ry_emit_negative_index_wrap(idx, len + 1);
//   ry_emit_bounds_check(wrapped_idx, len + 1, INSERT);   // [0, len] inclusive
//   if (len >= cap) data = __ry_list_grow(list_ptr, elem_size);
//   else            data = list.data;
//   memmove(data + (wrapped_idx + 1) * elem_size,
//           data + wrapped_idx * elem_size,
//           (len - wrapped_idx) * elem_size);
//   *(data + wrapped_idx * elem_size) = val;
//   list.len = len + 1;
// Internally invokes ry_emit_negative_index_wrap + ry_emit_bounds_error.
// ARC retain on `val` (if the element type warrants it) MUST be emitted by
// the caller BEFORE invoking this ABI.
// Precondition: ry_emit_ctx_set_function must have been called with the
// current LLVM function before this call (BBs are created inside it).
void ry_emit_collection_insert(RyEmitCtx *ctx, RyValueId list_ptr_id,
                               RyValueId idx_id, RyValueId val_id,
                               void *list_header_ty_ptr /* llvm::StructType* */,
                               void *elem_ty_ptr /* llvm::Type* */,
                               uint64_t elem_size);

// Stage 2-C entry — List remove at index (in-place mutation with shift):
//   wrapped_idx = ry_emit_negative_index_wrap(idx, len);
//   ry_emit_bounds_check(wrapped_idx, len, REMOVE);       // [0, len) exclusive
//   removed = *(data + wrapped_idx * elem_size);
//   memmove(data + wrapped_idx * elem_size,
//           data + (wrapped_idx + 1) * elem_size,
//           (len - wrapped_idx - 1) * elem_size);
//   list.len = len - 1;
//   return removed;
// Returns the RyValueId of the removed element (caller is responsible for
// any ARC release decision based on its type metadata).
// Internally invokes ry_emit_negative_index_wrap + ry_emit_bounds_error.
// Precondition: ry_emit_ctx_set_function must have been called with the
// current LLVM function before this call (BBs are created inside it).
RyValueId ry_emit_collection_remove_at(
    RyEmitCtx *ctx, RyValueId list_ptr_id, RyValueId idx_id,
    void *list_header_ty_ptr /* llvm::StructType* */,
    void *elem_ty_ptr /* llvm::Type* */, uint64_t elem_size);

// Stage 2-C entry — List slice (produces a new heap data buffer):
//   src_len = list.len; src_data = list.data;
//   // CreateSelect clamps both endpoints to [0, src_len]; if start >= end the
//   // result is an empty slice (count = 0, new_data = malloc(0) sized for at
//   // least 1 elem to keep downstream pointer math defined).
//   s = clamp(start, 0, src_len); e = clamp(end_excl, s, src_len);
//   count = e - s;
//   alloc_bytes = max(count, 1) * elem_size;
//   new_data = malloc(alloc_bytes);
//   memcpy(new_data, src_data + s * elem_size, count * elem_size);
//   *out_count = count; *out_new_data = new_data;
// Branchless (no new BBs): uses CreateSelect for clamping plus a single
// malloc+memcpy call. The new ARC header allocation, type-metadata copy,
// and per-element ARC retain loop stay on the codegen side (after the ABI
// returns) because they need ValueMetadata that does not cross the ABI.
// Precondition: NONE — ry_emit_ctx_set_function need not have been called.
void ry_emit_list_slice(RyEmitCtx *ctx, RyValueId list_ptr_id,
                        RyValueId start_id, RyValueId end_excl_id,
                        void *list_header_ty_ptr /* llvm::StructType* */,
                        void *elem_ty_ptr /* llvm::Type* */,
                        uint64_t elem_size, RyValueId *out_count,
                        RyValueId *out_new_data);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // RY_LLVM_EMIT_API_H
