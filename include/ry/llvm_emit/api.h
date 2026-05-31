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
// Stage 2-C (#1970) adds ry_emit_cow_ensure_unique for the Copy-on-Write
// uniqueness check used by 1-hop List / Map / Set slot writes (path CoW
// via emitPathCowForChain is a later Stage). The op:
//   - Loads strong_count atomically (Acquire if RY_ARC_ATOMIC, NotAtomic
//     otherwise) to pair with the SeqCst atomicrmw used by retain/release.
//   - Treats both strong==1 (unique) and strong==INT64_MAX (immortal) as
//     "skip CoW".
//   - Otherwise allocates a fresh ARC-backed collection header, memcpy's
//     the kind-specific contents (List `{i64,i64,ptr}`, Map
//     `{i64,i64,ptr,ptr,i64,ptr}`, Set `{i64,i64,ptr,i64,ptr}`), runs
//     element-retain loops (always NON-atomic; element retain crosses no
//     thread boundary), calls ry_emit_arc_release internally to drop the
//     old header, and stores the new dataPtr into slot_ptr_id.
//   - Returns a PHI joining the original dataPtr (unique / immortal) with
//     the new dataPtr (copied path).
// The kind-specific header StructTypes are reconstructed locally inside
// the emission layer as anonymous `{i64, i64, ptr}` / `{i64, i64, ptr,
// ptr, i64, ptr}` / `{i64, i64, ptr, i64, ptr}` — CodeGen's named
// "ListHeader" / "MapHeader" / "SetHeader" types stay on the CodeGen side
// and are not consulted at the ABI boundary. emitCowDeepCopy{List,Map,Set}
// in src/codegen_arc_cow.cpp are dissolved into the ABI implementation;
// emitCowRetainArcElements stays in CodeGen because of its 8 non-CoW
// callers.
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

// Opaque per-compile() emission context.
typedef struct RyEmitCtx RyEmitCtx;

// ===========================================================================
// Stage 2-C / #1973: typed opaque handles for category 1 (LLVM context state)
// and category 2 (primitive type accessors).
//
// Each typedef is a pointer to an incomplete struct, so the C/C++ type system
// treats it as a distinct named pointer type. At runtime the pointer value is
// reinterpret_cast'd between the LLVM-owning side (`src/llvm_emit/impl.cpp`)
// and the CodeGen-owning side. No LLVM types and no `void *` appear in any
// public signature — opaque pointer typedefs satisfy the constraint enforced
// by the header-level lint introduced in #1973.
//
// CodeGen-side cast helpers live in `include/ry/llvm_emit/cast_helpers.hpp`
// (a C++-only header that re-includes both api.h and the LLVM headers); use
// `asRyValue` / `asLlvmValue` / `asRyType` / etc. there rather than sprinkling
// reinterpret_cast across call sites.
// ===========================================================================

// === Category 1: LLVM context handles ===
typedef struct RyModuleOpaque   *RyModuleHandle;   // llvm::Module *
typedef struct RyBuilderOpaque  *RyBuilderHandle;  // llvm::IRBuilder<> *
typedef struct RyContextOpaque  *RyContextHandle;  // llvm::LLVMContext *
typedef struct RyFunctionOpaque *RyFunctionHandle; // llvm::Function *

// === Category 2: primitive type accessors ===
// `RyTypeRef` covers both `llvm::Type *` (primitives, pointers) and
// `llvm::StructType *` (named aggregates) — LLVM's StructType IS a Type, so
// no separate typedef is needed. dyn_cast<StructType> happens inside impl.cpp
// when a specific subtype is required.
typedef struct RyTypeOpaque     *RyTypeRef;
typedef struct RyFuncTypeOpaque *RyFuncTypeRef;    // llvm::FunctionType *

// === ABI ingress/egress pointer types ===
// `RyValueRef` is the source-side pointer for `ry_emit_intern` /
// `ry_emit_resolve`. It crosses the boundary as an opaque pointer; inside the
// emission layer it is held in the intern table behind `RyValueId`.
typedef struct RyValueOpaque    *RyValueRef;       // llvm::Value *

// === BasicBlock handle (used by ControlFlow ABI added in #1973) ===
typedef struct RyBasicBlockOpaque *RyBasicBlockRef; // llvm::BasicBlock *
typedef uint32_t RyBasicBlockId;                    // intern handle

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

// Copy-on-Write collection kind selector. Numbers chosen to match the order
// CodeGen's CollectionKind enum uses for List / Map / Set; do not reorder
// without updating both sides. CollectionKind::Str is intentionally absent
// because str is immutable and never reaches the CoW path.
typedef enum {
    RY_COW_LIST = 0,
    RY_COW_MAP = 1,
    RY_COW_SET = 2
} RyCowKind;

// Callback type for ok/err value builders consumed by ry_emit_result_branch.
// Stage 2-B keeps callbacks at the C ABI boundary (function pointer +
// user_ctx) — the C++ side translates `llvm::function_ref<>` closures into
// this shape via a trampoline. user_ctx is opaque to the emission layer.
typedef RyValueId (*RyBuildValueFn)(void *user_ctx);

// Lifecycle. Create at the top of CodeGen::compile() and destroy on exit.
// Categories 1 (LLVM context handles) crossed the ABI as typed opaque handles
// in Stage 2-C / #1973: RyModuleHandle / RyBuilderHandle / RyContextHandle /
// RyFunctionHandle replace the transitional `void *` shape from Stage 2-A.
RyEmitCtx *ry_emit_ctx_create(RyModuleHandle module, RyBuilderHandle builder,
                              RyContextHandle context,
                              RyFunctionHandle function);
void ry_emit_ctx_destroy(RyEmitCtx *ctx);

// Update the current LLVM function handle mid-compile (it changes when
// CodeGen emits a new function body — lambdas, @parallel for thunks,
// destructors, iterator-next bodies, etc.).
void ry_emit_ctx_set_function(RyEmitCtx *ctx, RyFunctionHandle function);

// Handle marshalling. ry_emit_intern returns 0 if `value` is NULL; every
// other value gets a fresh handle. ry_emit_resolve returns NULL for handle 0
// and for any out-of-range handle.
RyValueId ry_emit_intern(RyEmitCtx *ctx, RyValueRef value);
RyValueRef ry_emit_resolve(RyEmitCtx *ctx, RyValueId id);

// Category-3 helpers (from llvm-ir-emission-boundary.md). Each one wraps a
// canonical emission pattern as an atomic ABI call so the LLVM-side BB / PHI
// plumbing stays inside the shared library.

// Build an Error struct by calling err_fn_name (a `const char *(*)()` runtime
// symbol) and packaging the returned pointer with a zero code. `error_ty`
// is the opaque StructType handle for `errorTy_` on the CodeGen side
// (RyTypeRef carries `llvm::StructType *` internally).
RyValueId ry_emit_build_error_from_runtime(RyEmitCtx *ctx,
                                           const char *err_fn_name,
                                           RyTypeRef error_ty);

// Resolve or insert a runtime symbol with the given LLVM FunctionType. The
// caller constructs the FunctionType and pairs the returned callee handle
// with it to reconstruct an `llvm::FunctionCallee` struct on the CodeGen
// side. `fn_ty` is the opaque FunctionType handle. Returns the callee
// operand handle (`RyValueRef` carrying `llvm::Value *` internally) suitable
// for the FunctionCallee ctor; never NULL because `getOrInsertFunction`
// always synthesizes a callee.
RyValueRef ry_emit_get_runtime_fn(RyEmitCtx *ctx, const char *name,
                                  RyFuncTypeRef fn_ty);

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
// `res_ty` is the opaque StructType handle for Result<T, E> (mirrors
// `error_ty` in ry_emit_build_error_from_runtime). Returns the PHI handle
// holding the merged Result value.
// Precondition: ry_emit_ctx_set_function must have been called with the
// current LLVM function before this call (three BBs are created inside it).
// NB: `user_ctx` remains `void *` because it is genuine opaque user data
// (not an LLVM type) — the header lint carves it out.
RyValueId ry_emit_result_branch(RyEmitCtx *ctx, RyValueId is_err_id,
                                RyTypeRef res_ty, RyBuildValueFn build_ok,
                                RyBuildValueFn build_err, void *user_ctx);

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
// and returns a handle to the resulting aggregate. `opt_ty` is the opaque
// StructType handle for Option<T> (mirrors `res_ty` in
// ry_emit_result_branch). inner_id must resolve to a non-null payload value
// whose type matches opt_ty's element-1 slot. Creates no basic blocks, no
// precondition on ry_emit_ctx_set_function.
RyValueId ry_emit_option_wrap_some(RyEmitCtx *ctx, RyValueId inner_id,
                                   RyTypeRef opt_ty);

// Stage 2-C entry — Option<T> None-arm construction.
// Builds UndefValue(opt_ty) + InsertValue(tag=0, 0) + InsertValue(
// UndefValue(opt_ty->getElementType(1)), 1). Creates no basic blocks, no
// precondition on ry_emit_ctx_set_function.
RyValueId ry_emit_option_wrap_none(RyEmitCtx *ctx, RyTypeRef opt_ty);

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
// `destructor_callee` may be NULL (skip destructor call); `gc_visit_fn` may
// be NULL (skip the arc.gc_track BB entirely, dead → arc.release, alive →
// done). Both are opaque LLVM-Value handles (the destructor is the callee
// operand from `FunctionCallee::getCallee()`; the GC-visit slot is an
// `llvm::Function *` cast to RyValueRef). The emission layer reconstructs
// the `void (*)(void *)` LLVM FunctionType locally so no LLVM type identity
// crosses the ABI.
// Precondition: ry_emit_ctx_set_function must have been called with the
// current LLVM function before this call (BBs are created inside it). The
// caller must additionally register "gc" in CodeGen.used_native_libraries_
// because release emits __ry_gc_track / __ry_gc_untrack calls.
void ry_emit_arc_release(RyEmitCtx *ctx, RyValueId header_ptr_id,
                         RyArcAtomic atomic, RyValueRef destructor_callee,
                         RyValueRef gc_visit_fn);

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
// Categories 1/2 migrated in #1973: `ret_ty` and each `arg_tys[i]` cross as
// `RyTypeRef`. The implementation rebuilds the LLVM FunctionType locally.
RyValueId ry_emit_runtime_call(RyEmitCtx *ctx, const char *name,
                               RyTypeRef ret_ty, const RyTypeRef *arg_tys,
                               uint32_t arg_ty_count,
                               const RyValueId *arg_ids, uint32_t arg_count,
                               const char *name_hint);

// Stage 2-C entry — List append (in-place mutation):
//   len = list.len; cap = list.cap;
//   if (len >= cap) {
//     append.grow: data = __ry_list_grow(list_ptr, elem_size); goto append.store;
//   } else {
//     append.nogrow: data = list.data; goto append.store;
//   }
//   append.store: *(data + len * elem_size) = val; list.len = len + 1;
// `list_header_ty` is the `{i64 len, i64 cap, ptr data}` StructType handle.
// ARC retain on `val` (if the element type warrants it) MUST be emitted by
// the caller BEFORE invoking this ABI — append does not retain internally.
// Precondition: ry_emit_ctx_set_function must have been called with the
// current LLVM function before this call (BBs are created inside it).
void ry_emit_collection_append(RyEmitCtx *ctx, RyValueId list_ptr_id,
                               RyValueId val_id, RyTypeRef list_header_ty,
                               RyTypeRef elem_ty, uint64_t elem_size);

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
                               RyTypeRef list_header_ty, RyTypeRef elem_ty,
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
RyValueId ry_emit_collection_remove_at(RyEmitCtx *ctx, RyValueId list_ptr_id,
                                       RyValueId idx_id,
                                       RyTypeRef list_header_ty,
                                       RyTypeRef elem_ty, uint64_t elem_size);

// Stage 2-C entry — List slice (produces a new heap data buffer):
//   src_len = list.len; src_data = list.data;
//   // CreateSelect clamps both endpoints to [0, src_len]; if start >= end the
//   // result is an empty slice (count = 0). For count=0 the malloc receives 0
//   // bytes; the returned pointer is libc-defined (glibc / macOS libmalloc
//   // both return a non-NULL unique pointer) and the subsequent memcpy is a
//   // zero-byte copy (well-defined). This matches the pre-migration behavior
//   // (PR #1971 byte-for-byte parity); tightening to max(count, 1) * elem_size
//   // would be a defensive change worth a separate issue.
//   s = clamp(start, 0, src_len); e = clamp(end_excl, s, src_len);
//   count = e - s;
//   new_data = malloc(count * elem_size);
//   memcpy(new_data, src_data + s * elem_size, count * elem_size);
//   *out_count = count; *out_new_data = new_data;
// Branchless (no new BBs): uses CreateSelect for clamping plus a single
// malloc+memcpy call. The new ARC header allocation, type-metadata copy,
// and per-element ARC retain loop stay on the codegen side (after the ABI
// returns) because they need ValueMetadata that does not cross the ABI.
// Precondition: NONE — ry_emit_ctx_set_function need not have been called.
void ry_emit_list_slice(RyEmitCtx *ctx, RyValueId list_ptr_id,
                        RyValueId start_id, RyValueId end_excl_id,
                        RyTypeRef list_header_ty, RyTypeRef elem_ty,
                        uint64_t elem_size, RyValueId *out_count,
                        RyValueId *out_new_data);

// Descriptor for ry_emit_cow_ensure_unique. CodeGen collects all metadata
// (kind, element sizes, retain flags, destructor) and flattens it to this
// plain-int / handle struct so the ABI stays C-only and LLVM types do not
// cross the boundary. `lowered::CowEnsureUniqueOp` (`include/ry/codegen/
// lowered_cow.hpp`) is the C++-side mirror.
typedef struct {
    // Current ARC-backed dataPtr (handle to llvm::Value*). The op derives
    // headerPtr internally as `data_ptr - ARC_HEADER_SIZE`.
    RyValueId data_ptr_id;
    // Slot (alloca or GEP) receiving the new dataPtr after a copy. The op
    // emits one store into this slot on the copy path; the unique / immortal
    // path leaves the slot untouched.
    RyValueId slot_ptr_id;
    // RyCowKind selector: 0 = List, 1 = Map, 2 = Set.
    int kind;
    // RyArcAtomic — gates the strong_count load ordering (Acquire vs
    // NotAtomic) and the internal ry_emit_arc_release atomic mode.
    int atomic;
    // List: element bytes; Set: element bytes; Map: unused (use val_size).
    uint64_t elem_size;
    // Map: key bytes. 0 for List / Set.
    uint64_t key_size;
    // Map: value bytes. 0 for List / Set.
    uint64_t val_size;
    // 1 = retain each cloned element after memcpy (List element / Map value
    // / Set element). Always done with RY_ARC_NONATOMIC inside the ABI.
    int do_elem_retain;
    // 1 = element retain uses StringHeader (offset -24); 0 = ArcHeader
    // (offset -16). Only consulted when do_elem_retain == 1.
    int elem_is_str;
    // Map only. 1 = retain each cloned Map key after memcpy. Always
    // RY_ARC_NONATOMIC. Independent of do_elem_retain because
    // `elementTypeIsArcManaged` only inspects map_value_type_name.
    int do_key_retain;
    // 1 = key retain uses StringHeader (offset -24); 0 = ArcHeader
    // (offset -16). Only consulted when do_key_retain == 1.
    int key_is_str;
    // Opaque LLVM-Value handle for the per-kind collection destructor passed
    // to the internal ry_emit_arc_release (callee operand of FunctionCallee).
    // May be NULL when the kind has no element-side cleanup to perform.
    RyValueRef destructor_callee;
} RyCowEnsureUniqueDesc;

// Stage 2-C entry — Copy-on-Write uniqueness check (`emitCowCheckSlot`).
// Emits the per-slot CoW sequence (BB labels FileCheck-stable):
//   header_ptr = data_ptr_id - ARC_HEADER_SIZE
//   strong = atomic-load strong_count  (Acquire if atomic, NotAtomic else)
//   skip   = (strong == 1) || (strong == INT64_MAX /* ARC_IMMORTAL */)
//   if (skip) goto cow.cont;
//   cow.copy:
//     // Allocate fresh ARC-backed collection header via the same path
//     // CodeGen's emitArcAlloc + emitArcGetDataPtr use (counter_delta +1,
//     // strong=1, weak=0, GEP+ARC_HEADER_SIZE for the dataPtr).
//     new_data = arc_alloc_header(kind_size)
//     memcpy(new_data, data_ptr_id, kind_size)
//     // For List: malloc(len * elem_size) + memcpy + store into new header
//     // For Map: same for keys (len * key_size), vals (len * val_size),
//     //          buckets (bucket_count * sizeof(i64)); bucket_count is
//     //          loaded from the old map header at field 4.
//     // For Set: same for elems (len * elem_size) and buckets.
//     if (do_elem_retain) for each cloned element retain (NON-atomic)
//     if (do_key_retain)  for each cloned Map key retain (NON-atomic)
//     ry_emit_arc_release(header_ptr, atomic, destructor_callee, NULL)
//     *slot_ptr_id = new_data
//     goto cow.cont;
//   cow.cont:
//     phi = PHI(data_ptr_id from orig BB, new_data from copy-end BB)
//     return phi
// The descriptor's atomic flag governs both the strong_count load ordering
// and the ry_emit_arc_release internal counters. Element retains stay
// NON-atomic because the per-thread thunk that owns the freshly-cloned
// container holds the only alias for the duration of the retain loop.
// destructor_callee may be NULL.
// Precondition: ry_emit_ctx_set_function must have been called with the
// current LLVM function before this call (BBs are created inside it). The
// caller must additionally register "gc" in CodeGen.used_native_libraries_
// because the internal ry_emit_arc_release call emits __ry_gc_track /
// __ry_gc_untrack.
RyValueId ry_emit_cow_ensure_unique(RyEmitCtx *ctx,
                                    const RyCowEnsureUniqueDesc *desc);

// Stage 2-C entry — Any-wrap (#1972).
// Numerically aligned with the C++ enum `ry::RyAnyTag` in
// `include/ry/ry_layout.hpp`:
//   Int=0, Float=1, Bool=2, Str=3, Unit=4, List=5, Map=6, Set=7,
//   Record=8, Enum=9. The descriptor passes `target_tag` as a raw int64_t
//   so the ABI side does not need to share the enum class identifier.
//
// kind selector:
//   0 = NonBox    — primitive (int/float/bool) / str / collection pointer
//                   stored directly in any.data[8]. Bool (i1) is ZExt'd to
//                   i64 by the ABI ("any.bool.zext") when val's LLVM type
//                   is i1. The do_collection_retain / do_str_retain ints
//                   select whether the ABI emits emitArcGetHeaderFromData
//                   (offset -16, "arc_hdr_from_data") or
//                   emitStrGetHeaderFromData (offset -24,
//                   "str_hdr_from_data") + an internal
//                   ry_emit_arc_retain BEFORE the alloca+store sequence.
//   1 = RecordBox — heap-box layout `[ ArcHeader (16B) | desc ptr (8B) |
//                   record struct ]` via emitArcAlloc (malloc + counter
//                   delta +1, strong=1, weak=0). The descriptor and
//                   box_layout_ty come from the per-type lookup on the
//                   CodeGen side. target_tag must be Record=8.
//   2 = EnumBox   — structurally identical to RecordBox. target_tag must
//                   be Enum=9.
//
// CodeGen-side responsibilities NOT carried in the descriptor:
//   - typed_coll registration via __ry_any_register_typed_coll for NonBox
//     wraps whose ValueMetadata says the element type is non-any
//   - Field-wise retain via emitRecordArcFieldsRetain /
//     emitEnumBoxArcFieldsRetain (gated by `!isa<CallInst> &&
//     !isa<InvokeInst>`)
//
// Precondition: ry_emit_ctx_set_function must have been called with the
// current LLVM function before this call. The ABI invokes emitArcAlloc
// (which calls malloc + emits an inline atomic counter delta on the
// process-global ARC live-count balance counter) and ry_emit_arc_retain
// internally for Box arms and NonBox-with-retain arms; both helpers
// derive parent function from the builder so retargeted callers (lambda /
// @parallel for / iterator-next codegen) work safely.
typedef struct {
    // 0 = NonBox, 1 = RecordBox, 2 = EnumBox.
    int kind;
    // RyAnyTag value to store in any.tag. Matches the C++ enum class
    // `ry::RyAnyTag` numerically (0..9).
    int64_t target_tag;
    // Value to wrap. NonBox: primitive / bool / str / collection pointer.
    // Box arms: payload (record struct value or enum payload value).
    RyValueId val_id;
    // NonBox only: 1 = emit emitArcGetHeaderFromData + ry_emit_arc_retain
    // (collection element header at offset -16) before the alloca/store.
    int do_collection_retain;
    // NonBox only: 1 = emit emitStrGetHeaderFromData + ry_emit_arc_retain
    // (StringHeader at offset -24) before the alloca/store. Mutually
    // exclusive with do_collection_retain.
    int do_str_retain;
    // Box arms only: per-type descriptor LLVM Constant handle.
    RyValueId descriptor_id;
    // Box arms only: `{ ptr desc, payload }` StructType handle.
    RyTypeRef box_layout_ty;
    // Box arms only: DataLayout-derived allocation size of box_layout_ty,
    // in bytes. Passed as a plain integer so the ABI does not need to
    // consult DataLayout itself.
    uint64_t box_data_size;
    // Common: the `any` StructType handle (anyTy_ on the CodeGen side).
    RyTypeRef any_ty;
} RyAnyWrapDesc;

RyValueId ry_emit_any_wrap(RyEmitCtx *ctx, const RyAnyWrapDesc *desc);

// Stage 2-C entry — Any-unwrap (#1972).
// kind selector:
//   0 = Standard   — 2-way path. tag check -> matchBB(alloca+store+GEP+load+
//                    optional ARC retain) or mismatchBB(emitRuntimeError
//                    terminating with unreachable). BB labels: `any.match`,
//                    `any.mismatch`.
//   1 = F64Promote — 5-BB path: `any.float` / `any.check_int` /
//                    `any.int2float` / `any.mismatch` / `any.merge`,
//                    returning a PHI(f64) joining the float load with
//                    sitofp(int load).
//   2 = Record     — tag check + descriptor chain walk via
//                    `__ry_record_is_subtype_desc(actualDesc,
//                    expected_desc_id)`. BB labels: `any.rec.tag_ok`,
//                    `any.rec.tag_err`, `any.rec.desc_check`,
//                    `any.rec.desc_err`. Payload GEP/load uses
//                    `record_struct_ty_ptr` (Parent struct is a prefix of
//                    Child by Ry's record-inheritance ABI).
//
// Precondition: ry_emit_ctx_set_function must have been called with the
// current LLVM function before this call (multiple BBs are created
// inside it).
typedef struct {
    // 0 = Standard, 1 = F64Promote, 2 = Record.
    int kind;
    // Source any value.
    RyValueId any_val_id;
    // `any` StructType handle (anyTy_).
    RyTypeRef any_ty;
    // Standard / Record: target LLVM type handle. For Standard: i64 / i1 /
    // ptr / etc. For Record: ignored (record_struct_ty is used). For
    // F64Promote: ignored.
    RyTypeRef target_ty;
    // Standard / Record: expected RyAnyTag value (matches the C++ enum
    // class `ry::RyAnyTag`).
    int64_t expected_tag;
    // Standard only: 1 = emit emitArcGetHeaderFromData + ry_emit_arc_retain
    // after the load inside matchBB.
    int do_collection_retain;
    // Standard only: 1 = emit emitStrGetHeaderFromData + ry_emit_arc_retain
    // after the load. Mutually exclusive with do_collection_retain.
    int do_str_retain;
    // Standard / F64Promote / Record: tag-mismatch error message and global
    // name hint for the cachedGlobalString'd format string.
    const char *mismatch_msg;
    const char *mismatch_global_name;
    // Record only: expected per-type descriptor LLVM Constant handle.
    RyValueId expected_desc_id;
    // Record only: `{ ptr desc, record struct }` StructType handle.
    RyTypeRef box_layout_ty;
    // Record only: the record's struct type for the payload GEP/load.
    RyTypeRef record_struct_ty;
    // Record only: descriptor-mismatch error message and global name hint.
    const char *desc_mismatch_msg;
    const char *desc_mismatch_global_name;
} RyAnyUnwrapDesc;

RyValueId ry_emit_any_unwrap(RyEmitCtx *ctx, const RyAnyUnwrapDesc *desc);

// Stage 2-C entry — Any-try-unwrap (#1972).
// kind selector:
//   0 = Standard   — tag-check primitive arm via ry_emit_result_branch.
//                    Ok-branch: alloca + store + GEP + load + optional ARC
//                    retain + buildOkValue (UndefValue + 3 InsertValues).
//                    Err-branch: buildErrValue with inline Error
//                    `{ err_msg_str, 0 }`. BB labels match the existing
//                    `tryany.*` prefix.
//   1 = F64Promote — f64 target with int auto-promote via
//                    ry_emit_result_branch. Both loads share one alloca;
//                    the Ok branch picks via select(isFloat, fVal,
//                    sitofp(iVal)).
//
// Typed arms (Record / List<T> / Map<str,V> / Option / Result / simple
// enum / Set<T>) stay CodeGen-private and are dispatched before this op
// is reached.
//
// Precondition: ry_emit_ctx_set_function must have been called with the
// current LLVM function before this call (ry_emit_result_branch creates
// three BBs inside it).
typedef struct {
    // 0 = Standard, 1 = F64Promote.
    int kind;
    // Source any value.
    RyValueId any_val_id;
    // `any` StructType handle (anyTy_).
    RyTypeRef any_ty;
    // Result<targetTy, Error> StructType handle.
    RyTypeRef res_ty;
    // Error StructType handle (errorTy_ on the CodeGen side).
    RyTypeRef error_ty;
    // Standard only: target LLVM type (i64 / i1 / ptr / etc.).
    // F64Promote: ignored (f64Ty_ is hard-coded).
    RyTypeRef target_ty;
    // Standard only: expected RyAnyTag value.
    int64_t expected_tag;
    // Standard only: ARC retain flags (same semantics as RyAnyUnwrapDesc).
    int do_collection_retain;
    int do_str_retain;
    // Both kinds: error message string handle (the result of
    // cachedGlobalString on the CodeGen side). Stored into Error slot 0.
    RyValueId err_msg_str_id;
} RyAnyTryUnwrapDesc;

RyValueId ry_emit_any_try_unwrap(RyEmitCtx *ctx,
                                 const RyAnyTryUnwrapDesc *desc);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // RY_LLVM_EMIT_API_H
