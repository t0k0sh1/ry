// boundary surface for the LLVM IR emission shared library (libemit).
//
// This header is the candidate boundary between Ry semantic lowering and
// LLVM IR emission (designed in #1820/#1824, implemented incrementally
// starting from #1949). It is C-only so the layer is implemented in Rust
// (crates/emit/) behind the same C boundary — the Rust implementation is
// now the only one.
//
// Stage 2-C (#1968) begins per-op migration of category-1 ARC primitives:
//   - ry_emit_arc_retain / ry_emit_arc_release cross the boundary. The destructor
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
//     the boundary: ry_emit_get_runtime_fn, ry_emit_build_error_from_runtime,
//     ry_emit_bounds_check, ry_emit_result_branch, ry_emit_negative_index_wrap,
//     and ry_emit_bounds_error. wrapPtrAsResult / wrapStatusAsResult are
//     thin wrappers over ry_emit_result_branch on the CodeGen side and do
//     not need their own boundary entry.
//   - getResultType's StructType cache stays in CodeGen (its reverse map
//     `reverse_result_types_` is consumed in codegen_any.cpp/codegen_arc.cpp
//     for ARC release and Any wrapping); resTy crosses as `void*`, mirroring
//     errorTy_ in ry_emit_build_error_from_runtime.
//   - The previous `RyEmitCallbacks` slot is removed — negative-index wrap
//     and bounds error are now proper boundary functions rather than callbacks
//     into CodeGen.
//
// Stage 2-C (in progress, #1965) begins migrating the remaining lowered IR
// ops. The first step (#1967) adds two entry points for OptionWrap —
// ry_emit_option_wrap_some and ry_emit_option_wrap_none. Unlike the BB-
// creating ops above, these emit no basic blocks, so they need no positioned
// parent function. getOptionType's StructType
// cache stays in CodeGen for the same reason as getResultType (the
// reverse_option_types_ map is consumed by ARC release / Any wrap on the
// CodeGen side); opt_ty crosses the boundary as `void*`, mirroring resTy_.
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
// and are not consulted at the boundary. emitCowDeepCopy{List,Map,Set}
// in src/codegen_arc_cow.cpp are dissolved into the boundary implementation;
// emitCowRetainArcElements stays in CodeGen because of its 8 non-CoW
// callers.
//
// Stage 2-A (scaffolding, #1949):
//   - Opaque handles for LLVM values are defined as `uint32_t` (RyValueId).
//   - Module / IRBuilder / LLVMContext / Function pointers are still passed
//     as `void*` because categories 1 (LLVM context handles) and 2 (primitive
//     type accessors) have not crossed the boundary yet. Successor PRs (Stage 2-C
//     and beyond) will replace these with opaque handles.
//
// Constraint enforced for every signature in this header (per #1824 AC):
//   LLVM-owned types (`llvm::Value*`, `llvm::Module&`, `llvm::IRBuilder<>`,
//   `llvm::Function*`, `llvm::BasicBlock*`, `llvm::Type*`, `llvm::Constant*`)
//   must NOT appear in any public signature here. Use opaque handles or
//   transitional `void*` parameters tagged in comments.
//
// Symbol naming:
//   `ry_emit_*` are the emission entry points. This is distinct from the
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
// reinterpret_cast'd between the LLVM-owning side (`crates/emit/src/lib.rs`)
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
typedef struct RyModuleOpaque   *RyModuleRef;   // llvm::Module *
typedef struct RyBuilderOpaque  *RyBuilderRef;  // llvm::IRBuilder<> *
typedef struct RyContextOpaque  *RyContextRef;  // llvm::LLVMContext *
typedef struct RyFunctionOpaque *RyFunctionRef; // llvm::Function *

// === Category 2: primitive type accessors ===
// `RyTypeRef` covers both `llvm::Type *` (primitives, pointers) and
// `llvm::StructType *` (named aggregates) — LLVM's StructType IS a Type, so
// no separate typedef is needed. Narrowing to a specific subtype happens inside
// the Rust implementation (`crates/emit/src/lib.rs`) when required.
typedef struct RyTypeOpaque     *RyTypeRef;
typedef struct RyFuncTypeOpaque *RyFuncTypeRef;    // llvm::FunctionType *

// === boundary ingress/egress pointer types ===
// `RyValueRef` is the source-side pointer for `ry_emit_intern` /
// `ry_emit_resolve`. It crosses the boundary as an opaque pointer; inside the
// emission layer it is held in the intern table behind `RyValueId`.
typedef struct RyValueOpaque    *RyValueRef;       // llvm::Value *

// === BasicBlock handle (used by ControlFlow boundary added in #1973) ===
typedef struct RyBasicBlockOpaque *RyBasicBlockRef; // llvm::BasicBlock *

// === SwitchInst handle (used by the switch boundary added in #2100) ===
// llvm::SwitchInst* (carried as LLVMValueRef internally). A switch is mutated
// after creation — ry_emit_switch_add_case appends each case — so it crosses as
// an opaque handle (NOT interned), mirroring RyBasicBlockRef / RyFunctionRef.
typedef struct RySwitchOpaque *RySwitchRef;

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

// Atomic RMW binop selector — Ry-concept-free generic LLVM `atomicrmw`
// operations. Added for #2190 (weak ARC capability migration); designed for
// future extension (XOR/AND/OR can be added without reshuffling). Numbers
// must match RY_ATOMIC_BINOP_* in crates/emit/src/abi.rs; do not reorder
// without updating both sides. An unknown binop value falls back to Add.
typedef enum {
    RY_ATOMIC_BINOP_ADD = 0,
    RY_ATOMIC_BINOP_SUB = 1
} RyAtomicBinOp;

// Atomic memory ordering selector — Ry-concept-free generic LLVM
// `AtomicOrdering`. Added for #2190 (weak ARC capability migration). Numbers
// must match RY_ATOMIC_ORDERING_* in crates/emit/src/abi.rs; do not reorder
// without updating both sides. NotAtomic means the load is non-atomic (no
// fence) — for atomic ops (rmw / cmpxchg) it is invalid and the emission
// layer falls back to Monotonic. An unknown ordering value falls back to
// NotAtomic (for loads) or Monotonic (for rmw/cmpxchg).
typedef enum {
    RY_ATOMIC_ORDERING_NOT_ATOMIC      = 0,
    RY_ATOMIC_ORDERING_MONOTONIC       = 1,
    RY_ATOMIC_ORDERING_ACQUIRE         = 2,
    RY_ATOMIC_ORDERING_RELEASE         = 3,
    RY_ATOMIC_ORDERING_ACQUIRE_RELEASE = 4,
    RY_ATOMIC_ORDERING_SEQ_CST         = 5
} RyAtomicOrdering;

// Copy-on-Write collection kind selector. Numbers chosen to match the order
// CodeGen's CollectionKind enum uses for List / Map / Set; do not reorder
// without updating both sides. CollectionKind::Str is intentionally absent
// because str is immutable and never reaches the CoW path.
typedef enum {
    RY_COW_LIST = 0,
    RY_COW_MAP = 1,
    RY_COW_SET = 2
} RyCowKind;

// Call-site selector for ry_emit_list_copy_full (keys / values / take, #2093).
// The three builtins emit the identical mul+malloc+memcpy shape and diverge only
// in their SSA names; this kind picks the name pair so the migrated IR stays
// byte-identical per call site. Numbers must match RY_LISTCOPY_* in abi.rs; do
// not reorder without updating both sides.
typedef enum {
    RY_LISTCOPY_KEYS = 0,
    RY_LISTCOPY_VALUES = 1,
    RY_LISTCOPY_TAKE = 2
} RyListCopyKind;

// Callback type for ok/err value builders consumed by ry_emit_result_branch.
// Stage 2-B keeps callbacks at the C boundary (function pointer +
// user_ctx) — the C++ side translates `llvm::function_ref<>` closures into
// this shape via a trampoline. user_ctx is opaque to the emission layer.
typedef RyValueId (*RyBuildValueFn)(void *user_ctx);

// Lifecycle. Create at the top of CodeGen::compile() and destroy on exit.
// The LLVM context handles cross the boundary as typed opaque handles
// (Stage 2-C / #1973): RyModuleRef / RyBuilderRef / RyContextRef replace the
// transitional `void *` shape from Stage 2-A. There is no current-function
// slot: every BB-creating op derives its parent function from the builder's
// insert block per the rule in `.claude/rules/codegen-llvm-ir-conventions.md`
// (the former `ry_emit_ctx_set_function` setter was removed in #2083).
RyEmitCtx *ry_emit_ctx_create(RyModuleRef module, RyBuilderRef builder,
                              RyContextRef context);
void ry_emit_ctx_destroy(RyEmitCtx *ctx);

// Handle marshalling. ry_emit_intern returns 0 if `value` is NULL; a non-NULL
// value gets a stable handle — interning the same value pointer twice returns
// the same id (#2147). ry_emit_resolve returns NULL for handle 0 and for any
// out-of-range handle.
RyValueId ry_emit_intern(RyEmitCtx *ctx, RyValueRef value);
RyValueRef ry_emit_resolve(RyEmitCtx *ctx, RyValueId id);

// Category-3 helpers (from llvm-ir-emission-boundary.md). Each one wraps a
// canonical emission pattern as an atomic boundary call so the LLVM-side BB / PHI
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
// boundary functions since Stage 2-B).
// Precondition: the builder must be positioned within a function before this
// call (BBs are created inside it).
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
// Precondition: the builder must be positioned within a function before this
// call (three BBs are created inside it).
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
// whose type matches opt_ty's element-1 slot. Creates no basic blocks, so it
// needs no positioned parent function.
RyValueId ry_emit_option_wrap_some(RyEmitCtx *ctx, RyValueId inner_id,
                                   RyTypeRef opt_ty);

// Stage 2-C entry — Option<T> None-arm construction.
// Builds UndefValue(opt_ty) + InsertValue(tag=0, 0) + InsertValue(
// UndefValue(opt_ty->getElementType(1)), 1). Creates no basic blocks, so it
// needs no positioned parent function.
RyValueId ry_emit_option_wrap_none(RyEmitCtx *ctx, RyTypeRef opt_ty);

// Emit an ARC retain on header_ptr (`{i64 strong_count, i64 weak_count}`):
//   if (strong_count == INT64_MAX) goto arc.retain.done;  // immortal sentinel
//   else strong_count += 1; goto arc.retain.done;
// The immortal check uses a Monotonic load in both atomic modes; the
// increment uses atomicrmw add SeqCst when atomic == RY_ARC_ATOMIC, plain
// Load/Add/Store otherwise. BB labels are emitted as `arc.retain` and
// `arc.retain.done` (FileCheck-stable).
// Precondition: the builder must be positioned within a function before this
// call (BBs are created inside it).
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
// crosses the boundary.
// Precondition: the builder must be positioned within a function before this
// call (BBs are created inside it). The
// caller must additionally register "gc" in CodeGen.used_native_libraries_
// because release emits __ry_gc_track / __ry_gc_untrack calls.
void ry_emit_arc_release(RyEmitCtx *ctx, RyValueId header_ptr_id,
                         RyArcAtomic atomic, RyValueRef destructor_callee,
                         RyValueRef gc_visit_fn);

// Emit an inline `atomicrmw add @__ry_arc_counter, delta monotonic` against
// the process-global ARC live-count counter (whose address is JIT-time
// constant via `__ry_arc_counter_address()` baked into an `inttoptr`).
// Counterpart of the existing `arc.free`-internal counter bump inside
// ry_emit_arc_release. Added for #2190 so the weak release free path can
// participate in ARC live-count accounting without re-routing through
// `ry_emit_arc_release`. Composite (not primitive): owns the
// `__ry_arc_counter` global selection and the ASLR-baked-address inttoptr
// shape — both Ry ARC accounting concepts. Creates no basic blocks.
void ry_emit_arc_counter_delta(RyEmitCtx *ctx, int64_t delta);

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
// Creates no basic blocks, so it needs no positioned parent function.
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
// the caller BEFORE invoking this boundary — append does not retain internally.
// Precondition: the builder must be positioned within a function before this
// call (BBs are created inside it).
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
// the caller BEFORE invoking this boundary.
// Precondition: the builder must be positioned within a function before this
// call (BBs are created inside it).
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
// Precondition: the builder must be positioned within a function before this
// call (BBs are created inside it).
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
// and per-element ARC retain loop stay on the codegen side (after the boundary
// returns) because they need ValueMetadata that does not cross the boundary.
// Precondition: NONE — this op creates no basic blocks.
void ry_emit_list_slice(RyEmitCtx *ctx, RyValueId list_ptr_id,
                        RyValueId start_id, RyValueId end_excl_id,
                        RyTypeRef list_header_ty, RyTypeRef elem_ty,
                        uint64_t elem_size, RyValueId *out_count,
                        RyValueId *out_new_data);

// Linear-search-and-memmove `remove(val)` on a List (#2095). Mirrors the C++
// emitListRemove:
//   for (i = 0; i < len && foundIdx < 0; ++i)
//     if (cmp(val, data[i]) == 0) foundIdx = i;
//   if (foundIdx >= 0) {
//     memmove(data + foundIdx, data + foundIdx + 1,
//             (len - foundIdx - 1) * elem_size);
//     list.len = len - 1;
//   }
//   return i64 0;
// `elem_is_str` selects strcmp+ICmpEQ (caller must positively allowlist str
// pointer elements at the call site — Map/Set/closure headers in a list of
// pointers are UB under strcmp); `elem_is_double` selects FCmpOEQ; otherwise
// plain ICmpEQ. Both flags ignored together default to ICmpEQ.
// Precondition: builder positioned within a function (BBs are created inside
// the search and the remove path). Returns the interned `i64 0` sentinel.
RyValueId ry_emit_list_remove(RyEmitCtx *ctx, RyValueId container_ptr_id,
                              RyValueId val_id, RyTypeRef list_header_ty,
                              RyTypeRef list_elem_ty, uint64_t elem_size,
                              int elem_is_str, int elem_is_double);

// O(n²) `distinct(list)` dedup (#2095). The caller pre-loads the source list
// header (loadListHeader) and pre-allocates the ARC collection header via
// emitArcAllocCollectionHeader (which registers the result in
// arc_owned_values_) — in that order, so the C++ baseline's instruction order
// is preserved byte-for-byte. The boundary then:
//   1. malloc(src_len * elem_size); store into new_header.data, src_len into
//      new_header.cap.
//   2. Outer loop scans each source element; inner loop checks for duplicates
//      already written into new_header.data[0..out_len); strcmp / FCmpOEQ /
//      ICmpEQ per element-type flag.
//   3. After loops: store the final dedup count into new_header.len.
// Caller-side allowlist guards str-only pointer elements before this call
// (Map/Set/closure header strcmp is UB). Precondition: builder positioned
// within a function (BBs are created inside).
void ry_emit_list_distinct(RyEmitCtx *ctx, RyValueId src_len_id,
                           RyValueId src_data_id, RyValueId new_header_id,
                           RyTypeRef list_header_ty, RyTypeRef elem_ty,
                           uint64_t elem_size, int elem_is_str,
                           int elem_is_double);

// Side-effecting retain callback for the per-element retain step of
// tuple-producing ops (#2095: enumerate / zip / map_items). Nullable; NULL =
// no retain needed (primitive or unnamed-elem path). The callback receives
// the loaded tuple-component value id and emits the retain IR by re-entering
// the C++ emitter.
typedef void (*RyRetainFn)(RyValueId val_id, void *user_ctx);

// `enumerate(list)` → `List<(int, T)>` (#2095). Caller pre-loads the source
// list header (loadListHeader) and pre-allocates the ARC collection header
// (emitArcAllocCollectionHeader) — in that order to preserve the C++ baseline.
// `retain_fn` is invoked once per loop iteration with the loaded element value
// id to emit emitTupleComponentRetain; NULL = primitive elements (no retain).
// Precondition: builder positioned within a function.
void ry_emit_list_enumerate(RyEmitCtx *ctx, RyValueId src_len_id,
                            RyValueId src_data_id, RyValueId new_header_id,
                            RyTypeRef list_header_ty, RyTypeRef elem_ty,
                            RyTypeRef tuple_ty, uint64_t tuple_size,
                            RyRetainFn retain_fn, void *user_ctx);

// `reverse(list)` (#2095). Caller pre-loads list header, pre-allocates the
// ARC collection header AND the data buffer (so rev_dsize / rev_data land at
// the C++ baseline's positions); engine emits the reverse loop + named
// StructGEPs rev_new_len/cap/data via LIST_FIELD_LEN/CAP/DATA. Post-loop ARC
// retain dispatch stays C++-side (emitTupleElemRetainLoop /
// emitCowRetainArcElements operate on ValueMetadata). Precondition: builder
// positioned within a function.
void ry_emit_list_reverse(RyEmitCtx *ctx, RyValueId len_id,
                          RyValueId src_data_id, RyValueId new_data_id,
                          RyValueId new_header_id,
                          RyTypeRef list_header_ty, RyTypeRef elem_ty);

// `items(map)` → `List<(K, V)>` (#2095). Caller pre-loads the map header
// (mf.len/keys/vals) and pre-allocates the ARC collection header. Engine
// emits the loop body + 2 retain callbacks + storeListHeaderFields.
void ry_emit_map_items(RyEmitCtx *ctx, RyValueId map_len_id,
                       RyValueId map_keys_id, RyValueId map_vals_id,
                       RyValueId new_header_id, RyTypeRef list_header_ty,
                       RyTypeRef key_ty, RyTypeRef val_ty,
                       RyTypeRef tuple_ty, uint64_t tuple_size,
                       RyRetainFn retain_key_fn, RyRetainFn retain_val_fn,
                       void *user_ctx);

// `zip(list1, list2)` → `List<(T1, T2)>` (#2095). Same shape as enumerate but
// 2 sources with min(len1, len2) clamp via Select named `zip_minlen`. retain1
// fires on e1, retain2 on e2 inside the loop body — both nullable.
// Precondition: builder positioned within a function.
void ry_emit_list_zip(RyEmitCtx *ctx, RyValueId min_len_id,
                      RyValueId data1_id, RyValueId data2_id,
                      RyValueId new_header_id, RyTypeRef list_header_ty,
                      RyTypeRef elem_ty1, RyTypeRef elem_ty2,
                      RyTypeRef tuple_ty, uint64_t tuple_size,
                      RyRetainFn retain1_fn, RyRetainFn retain2_fn,
                      void *user_ctx);

// Two-pass `flat(list<list<T>>)` (#2095). Caller pre-loads the outer list
// header (so loadListHeader order matches C++ baseline) and passes outer_len /
// outer_data. The boundary then:
//   1. Pass 1 loop: sum inner.len fields (LIST_FIELD_LEN) into `flat_total`.
//   2. Inline ARC alloc (mirrors `emitArcAllocCollectionHeader` byte-for-byte:
//      `arc_hdr` malloc(40), counter +1, strong=1, weak=0, GEP +ARC_HEADER_SIZE
//      named `arc_data`).
//   3. malloc(total * inner_elem_size) named `flat_data`; storeListHeaderFields
//      writes total / total / new_data via LIST_FIELD_LEN/CAP/DATA.
//   4. Pass 2 loop: memcpy each inner.data (LIST_FIELD_DATA) into new_data at
//      the running offset.
// Returns the new collection header pointer. The caller MUST register the
// returned value in `arc_owned_values_` to match the original
// `emitArcGetDataPtr` bookkeeping. Precondition: builder positioned within
// a function (BBs created inside).
RyValueId ry_emit_list_flatten(RyEmitCtx *ctx, RyValueId outer_len_id,
                               RyValueId outer_data_id,
                               RyTypeRef list_header_ty,
                               RyTypeRef arc_header_ty,
                               RyTypeRef inner_elem_ty,
                               uint64_t inner_elem_size);

// Collection copy-generation ops (#2093) — the malloc+memcpy chain that copies a
// source buffer into a fresh List buffer for `keys` / `values` / `take` /
// `appended` / list `+` concat. The codegen side keeps the header load (so the
// length / clamped-count stays live for the retain loop), the new ARC header
// allocation, the per-element ARC retain loop, and metadata propagation — all of
// which need ValueMetadata that does not cross the boundary. These ops create no
// basic blocks (Precondition: NONE). Each returns the malloc'd new buffer.

// keys / values / take: alloc `count * elem_size` bytes and memcpy the whole
// `src_data` range (alloc count == copy count). `kind` (RyListCopyKind) selects
// the call site so the SSA names (keys_ds/keys_nd, vals_ds/vals_nd,
// tk_dsize/tk_data) stay byte-identical. Returns the new buffer.
RyValueId ry_emit_list_copy_full(RyEmitCtx *ctx, RyValueId src_data_id,
                                 RyValueId count_id, uint64_t elem_size,
                                 int kind);

// appended: alloc `new_len * elem_size` bytes (apd_ds/apd_nd) but memcpy only the
// `old_len`-element source range (apd_ods). The appended element is stored by the
// codegen side after its retain loop, so it is not part of this op. `new_len`
// (apd_new_len) is passed in so storeListHeaderFields reuses the same SSA value.
// Returns the new buffer.
RyValueId ry_emit_list_appended(RyEmitCtx *ctx, RyValueId new_len_id,
                                RyValueId old_len_id, RyValueId src_data_id,
                                uint64_t elem_size);

// list `+` concat: alloc `new_len * elem_size` bytes (cat_ds/cat_data), memcpy
// the lhs buffer at offset 0 (cat_ls), then memcpy the rhs buffer at the
// element-typed GEP offset `lhs_len` (cat_rhs_dst/cat_rs). `elem_ty` is required
// for the mid-buffer GEP; `new_len` (cat_len) is passed in for SSA reuse.
// Returns the new buffer.
RyValueId ry_emit_list_concat(RyEmitCtx *ctx, RyValueId lhs_len_id,
                              RyValueId lhs_data_id, RyValueId rhs_len_id,
                              RyValueId rhs_data_id, RyValueId new_len_id,
                              RyTypeRef elem_ty, uint64_t elem_size);

// Numeric reduce builtins (#2092) — sum / min / max, the ARC-independent batch.
// `elem_ty` is the element LLVM type: i64 / f64 / i8 for sum, i64 / f64 for
// min/max; only float-vs-int picks FAdd/FCmp over Add/ICmp.

// sum([..]): emit the list-sum loop — interleaved `sum_*` header load + a
// zero-seeded accumulator + the sum.cond/body/end loop — and return the
// accumulated `sum_result`. `list_header_ty` is CodeGen::listHeaderTy_.
// Precondition: the builder must be positioned within a function (BBs created
// inside this call).
RyValueId ry_emit_reduce_sum_list(RyEmitCtx *ctx, RyValueId list_ptr_id,
                                  RyTypeRef elem_ty, RyTypeRef list_header_ty);

// sum(a, b, ..): emit one straight-line fold step `acc + v` (named `sum_v`). The
// C++ variadic loop seeds with arg[0] then calls this once per remaining
// argument, so operand evaluation stays interleaved with the adds. No BBs.
RyValueId ry_emit_reduce_sum_step(RyEmitCtx *ctx, RyValueId acc_id,
                                  RyValueId val_id, RyTypeRef elem_ty);

// min/max([..]): emit the seed + loop (`mm_first` + the
// mm.cond/body/update/next/end blocks) at the `mm.ok` block the caller is
// positioned at. The empty-list guard and emitRuntimeError stay on the C++ side
// (they build an ARC string global — out of scope for this ARC-free batch), so
// this entry receives the already-loaded `data_id` (mm_data) and `len_id`
// (mm_len). `is_max != 0` selects max. Returns `mm_result`. Precondition: the
// builder must be positioned at mm.ok (BBs created inside this call).
RyValueId ry_emit_reduce_minmax_list_loop(RyEmitCtx *ctx, RyValueId data_id,
                                          RyValueId len_id, RyTypeRef elem_ty,
                                          int is_max);

// min/max(a, b, ..): emit one fold step — `mm_cmp = cmp(v, best)` then
// `mm_best = select(mm_cmp, v, best)`. `is_max != 0` selects max (OGT/SGT) vs
// min (OLT/SLT). The C++ variadic loop calls this once per argument. No BBs.
RyValueId ry_emit_reduce_minmax_step(RyEmitCtx *ctx, RyValueId best_id,
                                     RyValueId val_id, RyTypeRef elem_ty,
                                     int is_max);

// Descriptor for ry_emit_cow_ensure_unique. CodeGen collects all metadata
// (kind, element sizes, retain flags, destructor) and flattens it to this
// plain-int / handle struct so the boundary stays C-only and LLVM types do not
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
    // / Set element). Always done with RY_ARC_NONATOMIC inside the boundary.
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
// Precondition: the builder must be positioned within a function before this
// call (BBs are created inside it). The
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
//   so the boundary side does not need to share the enum class identifier.
//
// kind selector:
//   0 = NonBox    — primitive (int/float/bool) / str / collection pointer
//                   stored directly in any.data[8]. Bool (i1) is ZExt'd to
//                   i64 by the boundary ("any.bool.zext") when val's LLVM type
//                   is i1. The do_collection_retain / do_str_retain ints
//                   select whether the boundary emits emitArcGetHeaderFromData
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
// Precondition: the builder must be positioned within a function before this
// call. The boundary invokes emitArcAlloc
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
    // in bytes. Passed as a plain integer so the boundary does not need to
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
//                    Child by Ry's record-inheritance layout).
//
// Precondition: the builder must be positioned within a function before this
// call (multiple BBs are created
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
// Precondition: the builder must be positioned within a function before this
// call (ry_emit_result_branch creates
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

// ===========================================================================
// Stage 2-C / #1973 — ControlFlow primitives.
//
// These thin boundary entries wrap the IRBuilder<>::Create* calls that previously
// appeared in src/codegen_*.cpp for control-flow construction (basic blocks,
// conditional / unconditional branches, PHI assembly). They are intentionally
// minimal — the lowering layer has no semantic decision to make for primitive
// CF per `docs/architecture/codegen-layering-plan.md` §"Explicit non-inclusion"
// — so no `lowered_control_flow.hpp` struct layer is introduced. The CodeGen-
// side inline wrappers in `include/ry/codegen.hpp` (createBB / emitBranchCond
// / emitBranchUncond / createPhi) bridge call sites to these entries without
// per-site reinterpret_cast clutter.
//
// SetInsertPoint / GetInsertBlock are intentionally NOT exposed: they are
// IRBuilder *state* operations that emit no IR (no `Create*` call), so they
// fall outside the "no IRBuilder<>::Create* in src/codegen_*.cpp" AC. Call
// sites continue to use `builder_.SetInsertPoint(bb)` / `builder_.GetInsertBlock()`
// directly.
// ===========================================================================

// Create a fresh basic block named `name` inside the function `fn`. Returns
// an opaque handle to the new BB. The caller chooses the parent function
// explicitly — for the common "current function" case the CodeGen-side
// wrapper `createBB` derives it from `builder_.GetInsertBlock()->getParent()`
// per the rule in `.claude/rules/codegen-llvm-ir-conventions.md`.
RyBasicBlockRef ry_emit_create_basic_block(RyEmitCtx *ctx, const char *name,
                                           RyFunctionRef fn);

// Emit a conditional branch at the builder's current insert point. `cond` is
// an interned i1 value; `true_bb` / `false_bb` are the destination blocks.
// Does not change the builder's insert point.
void ry_emit_branch_cond(RyEmitCtx *ctx, RyValueId cond,
                         RyBasicBlockRef true_bb, RyBasicBlockRef false_bb);

// Emit an unconditional branch to `target` at the builder's current insert
// point. Does not change the builder's insert point.
void ry_emit_branch_uncond(RyEmitCtx *ctx, RyBasicBlockRef target);

// Emit a PHI node at the builder's current insert point with the supplied
// incoming pairs and `ty` as the result type. `count` is the number of
// (value, block) pairs in `incoming_values[]` / `incoming_blocks[]` (parallel
// arrays). `name_hint` is the LLVM SSA name hint (NULL for none). Returns the
// interned PHI value handle.
RyValueId ry_emit_create_phi(RyEmitCtx *ctx, RyTypeRef ty,
                             const RyValueId *incoming_values,
                             const RyBasicBlockRef *incoming_blocks,
                             uint32_t count, const char *name_hint);

// Emit `ret val_id` at the builder's current insert point. `val_id == 0`
// (sentinel) emits `ret void`. The return terminator that completes a function
// body built via ry_emit_create_function (#2098).
void ry_emit_ret(RyEmitCtx *ctx, RyValueId val_id);

// === Switch boundary (#2100, [B] = (ii) boundary move) ===
// The switch-construction capability for non-ADT enum variant-name lowering
// (str(List<enum>) tostring, src/codegen_tostring.cpp). These are generic — they
// know nothing about enums — mirroring how create_basic_block / create_phi are
// generic CF primitives. The C++ side selects WHICH cases to add (from the
// EnumInfo it already holds); the ValueMetadata that drives that choice never
// crosses the boundary.

// Build `switch val, default_bb [ num_cases ]` at the builder's current insert
// point and return an opaque RySwitchRef. `num_cases` is the case-count hint
// (LLVMBuildSwitch's NumCases). The handle is NOT interned — it mirrors
// ry_emit_create_basic_block's RyBasicBlockRef shape — and must be passed to
// ry_emit_switch_add_case for each case. NULL ctx / default_bb, or an
// unresolvable val, → NULL. Does not change the builder's insert point.
RySwitchRef ry_emit_create_switch(RyEmitCtx *ctx, RyValueId val,
                                  RyBasicBlockRef default_bb,
                                  unsigned int num_cases);

// Append a case to `sw`: when the switch operand equals the interned ConstantInt
// `on_val` (same integer type as the operand), control branches to `dest_bb`
// (`LLVMAddCase`). NULL ctx / sw / dest_bb, or an unresolvable on_val, → no-op.
void ry_emit_switch_add_case(RyEmitCtx *ctx, RySwitchRef sw, RyValueId on_val,
                             RyBasicBlockRef dest_bb);

// ===========================================================================
// #2072 — scalar / memory IR primitives ([C] = (ii) boundary move).
//
// These fine-grained entries wrap a single IRBuilder<>::Create* (or
// ConstantInt::get) call each, so that the string byte-ops (toUpper / toLower /
// trim / trimStart / trimEnd) — the first migration under the (ii) "boundary
// move" decision — can emit their inline loop bodies WITHOUT any
// IRBuilder<>::Create* on the CodeGen side. Unlike the coarse ops above, the
// lowering side has no semantic decision to make for a primitive, so no
// `lowered_*` struct layer is introduced; the CodeGen-side inline wrappers in
// `include/ry/codegen.hpp` (emitAlloca / emitLoad / emitStore / emitGEP /
// emitICmp* / emitAnd / emitOr / emitAdd / emitSub / emitSelect / emitConstInt)
// bridge call sites without per-site intern/resolve clutter.
//
// This DELIBERATELY supersedes the (i)-era "Explicit non-inclusion" carve-out
// in `docs/architecture/codegen-layering-plan.md` (which kept primitive
// arithmetic / comparison / Alloca/Load/Store as a C++ carve-out to keep
// emission near-1:1). The decision and its trade-off (physical vs semantic
// boundary) is recorded there; this is its boundary surface. Runtime-symbol
// calls (`__ry_string_make_uninit`, `memcpy`) reuse ry_emit_runtime_call rather
// than a new entry; SetInsertPoint / GetInsertBlock stay on the CodeGen side
// (they emit no IR — see the ControlFlow note above).
// ===========================================================================

// Integer-comparison predicate selector for ry_emit_icmp. Numbers are a stable
// boundary contract; do not reorder without updating the RY_ICMP_* constants in
// `crates/emit/src/abi.rs` (which map to the core `IcmpPred`). Cross-language
// parity is enforced by tests/test_abi_layout.cpp
// (AbiLayout.RyICmpPredRustMirrorMatchesCanonical) via the test-only extern
// `ry_emit_test_icmp_pred_values` (#2143).
typedef enum {
    RY_ICMP_EQ = 0,
    RY_ICMP_NE = 1,
    RY_ICMP_SLT = 2,
    RY_ICMP_SLE = 3,
    RY_ICMP_SGT = 4,
    RY_ICMP_SGE = 5,
    RY_ICMP_ULT = 6,
    RY_ICMP_ULE = 7,
    RY_ICMP_UGT = 8,
    RY_ICMP_UGE = 9
} RyICmpPred;

// Emit `alloca ty` at the builder's current insert point; return the interned
// result handle. NULL `name` → empty SSA name. No basic blocks created.
RyValueId ry_emit_alloca(RyEmitCtx *ctx, RyTypeRef ty, const char *name);

// Emit `load ty, ptr` (element-typed under opaque pointers); return the interned
// result. NULL `name` → empty SSA name.
RyValueId ry_emit_load(RyEmitCtx *ctx, RyTypeRef ty, RyValueId ptr_id,
                       const char *name);

// Emit `store val, ptr` (produces no value).
void ry_emit_store(RyEmitCtx *ctx, RyValueId val_id, RyValueId ptr_id);

// Emit `atomicrmw <binop> ptr, val <ordering>` — the generic LLVM atomic
// read-modify-write instruction. `binop` is a RyAtomicBinOp; `ordering` is a
// RyAtomicOrdering (NotAtomic is invalid for rmw and falls back to
// Monotonic). Alignment is LLVM-auto-computed from the datalayout (matches
// the C++ `CreateAtomicRMW(..., MaybeAlign(), ...)` baseline). NULL `name` →
// empty SSA name. Added for #2190 (weak ARC capability migration); also
// usable for any future generic atomic RMW need. Creates no basic blocks.
RyValueId ry_emit_atomic_rmw(RyEmitCtx *ctx, int binop,
                              RyValueId ptr_id, RyValueId val_id,
                              int ordering, const char *name);

// Emit `load ty, ptr` and apply the given atomic ordering — the generic
// LLVM atomic load primitive. `ordering` is a RyAtomicOrdering; the
// NotAtomic case skips `LLVMSetOrdering` entirely (returns a plain load).
// `alignment > 0` triggers `LLVMSetAlignment(load, alignment)`; alignment=0
// leaves the LLVM default in place (matches the C++ `CreateLoad +
// setAtomic` baseline, which yields `align 4` on the host data layout for
// `i64` loads). Added for #2190 (weak ARC capability migration); also
// usable for any future ordered load. Creates no basic blocks.
RyValueId ry_emit_atomic_load(RyEmitCtx *ctx, RyTypeRef ty, RyValueId ptr_id,
                               int ordering, uint32_t alignment,
                               const char *name);

// Emit `cmpxchg ptr, expected, desired <success_ordering> <failure_ordering>`
// — the generic LLVM compare-and-swap primitive. Returns the `{T, i1}`
// aggregate (caller decomposes via ry_emit_extract_value, #2099). Both
// orderings are RyAtomicOrdering values; NotAtomic falls back to Monotonic.
// Alignment is LLVM-auto-computed from the datalayout (matches the C++
// `CreateAtomicCmpXchg(..., MaybeAlign(), ...)` baseline). NULL `name` →
// empty SSA name. Added for #2190 (weak ARC upgrade CAS loop). Creates no
// basic blocks.
RyValueId ry_emit_atomic_cmpxchg(RyEmitCtx *ctx, RyValueId ptr_id,
                                  RyValueId expected_id,
                                  RyValueId desired_id,
                                  int success_ordering,
                                  int failure_ordering,
                                  const char *name);

// Emit a single-index `getelementptr base_ty, ptr, idx`; return the interned
// result. NULL `name` → empty SSA name.
RyValueId ry_emit_gep(RyEmitCtx *ctx, RyTypeRef base_ty, RyValueId ptr_id,
                      RyValueId idx_id, const char *name);

// Emit a struct-field `getelementptr struct_ty, ptr, 0, field_idx` — the
// compile-time field-index GEP (distinct from the runtime-index ry_emit_gep
// above). `field_idx` is the aggregate field index. NULL `name` → empty SSA
// name. Added for #2098 (function-creation capability; struct-field access is
// pervasive in the migrated function bodies).
RyValueId ry_emit_struct_gep(RyEmitCtx *ctx, RyTypeRef struct_ty,
                             RyValueId ptr_id, uint32_t field_idx,
                             const char *name);

// Emit a two-index `getelementptr array_ty, base, i64 0, idx` — the array-element
// GEP that indexes an `[N x T]` aggregate (e.g. an enum name-array global),
// distinct from the single-index ry_emit_gep and the field-index
// ry_emit_struct_gep. The leading `i64 0` constant is synthesized inside the
// emission layer. When `base` and `idx` are both constants the IRBuilder folds
// the result to a ConstantExpr (no instruction emitted), byte-for-byte matching
// the C++ builder_.CreateGEP it replaces. NULL ctx / array_ty, or unresolvable
// base / idx, → 0. NULL `name` → empty. Added for #2100 (enum variant-name
// lookup in str(List<enum>)). Creates no basic blocks.
RyValueId ry_emit_array_gep(RyEmitCtx *ctx, RyTypeRef array_ty,
                            RyValueId base_id, RyValueId idx_id,
                            const char *name);

// Emit `extractvalue agg, idx` — read field `idx` out of an aggregate *value*
// (`LLVMBuildExtractValue`), distinct from the pointer-addressing
// ry_emit_struct_gep. NULL `name` → empty SSA name. Added for #2099 (closure /
// FnTypeInfo crossing: the filter / map iterator next-fn destructures the source
// Option in-register). Creates no basic blocks.
RyValueId ry_emit_extract_value(RyEmitCtx *ctx, RyValueId agg_id, uint32_t idx,
                                const char *name);

// Emit `zext val to dest_ty` — zero-extend an integer value to the wider
// integer type `dest_ty` (`LLVMBuildZExt`). NULL `name` → empty SSA name.
// Added for #2101 (hash-table lookup capability: the conditional i1→i64 key
// widening in emitHashTableLookup). Creates no basic blocks.
RyValueId ry_emit_zext(RyEmitCtx *ctx, RyValueId val_id, RyTypeRef dest_ty,
                       const char *name);

// Emit `trunc val to dest_ty` — truncate an integer value to the narrower
// integer type `dest_ty` (`LLVMBuildTrunc`). NULL `name` → empty SSA name.
// Added for #2194 (concurrency capability migration): thread join i64→i1 bool
// unwrap, AtomicInt CAS status i64→i1 truncate, AtomicBool load i64→i1.
// Creates no basic blocks.
RyValueId ry_emit_trunc(RyEmitCtx *ctx, RyValueId val_id, RyTypeRef dest_ty,
                        const char *name);

// Emit `icmp <predicate> lhs, rhs` where `predicate` is a RyICmpPred value;
// return the interned i1 result. An unknown predicate yields sentinel 0.
RyValueId ry_emit_icmp(RyEmitCtx *ctx, int predicate, RyValueId lhs_id,
                       RyValueId rhs_id, const char *name);

// Emit `and` / `or` / `add` / `sub` / `mul` / `sdiv` of two operands;
// return the interned result. (`mul` and `sdiv` added for #2096 string-build
// op migration: str * n / join sep total multiply, repeat overflow-bound
// `INT64_MAX / strLen` divide. `sdiv`'s divisor is guarded `> 0` upstream.)
RyValueId ry_emit_and(RyEmitCtx *ctx, RyValueId lhs_id, RyValueId rhs_id,
                      const char *name);
RyValueId ry_emit_or(RyEmitCtx *ctx, RyValueId lhs_id, RyValueId rhs_id,
                     const char *name);
RyValueId ry_emit_add(RyEmitCtx *ctx, RyValueId lhs_id, RyValueId rhs_id,
                      const char *name);
RyValueId ry_emit_sub(RyEmitCtx *ctx, RyValueId lhs_id, RyValueId rhs_id,
                      const char *name);
RyValueId ry_emit_mul(RyEmitCtx *ctx, RyValueId lhs_id, RyValueId rhs_id,
                      const char *name);
RyValueId ry_emit_sdiv(RyEmitCtx *ctx, RyValueId lhs_id, RyValueId rhs_id,
                       const char *name);

// Emit `select cond, then_val, else_val`; return the interned result.
RyValueId ry_emit_select(RyEmitCtx *ctx, RyValueId cond_id, RyValueId then_id,
                         RyValueId else_id, const char *name);

// Materialize an integer constant (ConstantInt::get equivalent); return the
// interned constant handle. `sign_extend` is the LLVMConstInt SignExtend flag
// (0 = zero-extend the value into `ty`'s width). Emits no instruction.
RyValueId ry_emit_const_int(RyEmitCtx *ctx, RyTypeRef ty, uint64_t value,
                            int sign_extend);

// Materialize a null constant (LLVMConstNull). For pointer types this is the
// typed null pointer (`ConstantPointerNull::get` equivalent) used in null-guard
// `icmp eq` comparisons; works for any type. Added for pilot G (#2196): the GC
// visitor thunk null-guard reads `fieldVal == null` on the per-field ARC
// pointer. Emits no instruction; the result is interned for handle threading.
RyValueId ry_emit_const_null(RyEmitCtx *ctx, RyTypeRef ty);

// ===========================================================================
// #2098 — function definition + indirect call ([C] = (ii) boundary move).
//
// The function-CREATION capability: until now the emission ABI could only emit
// instructions / basic blocks INSIDE a function the C++ side already had open
// (create_basic_block took an explicit parent handle; there was no way to MAKE
// the parent). These three entries close that gap — `ry_emit_create_function`
// is the `llvm::Function::Create` equivalent (LLVMAddFunction + linkage),
// `ry_emit_get_param` reads a parameter, and `ry_emit_call_indirect` calls
// through a loaded function-pointer value (vs. ry_emit_runtime_call's
// name-keyed getOrInsertFunction). They are generic and semantically trivial —
// nothing here knows about iterators or any Ry concept — so they generalize to
// every C++ Function::Create site (iterator-next / tostring / closure /
// destructor / thunk), unlike a coarse per-op `ry_emit_iter_take_next` which
// would leak Ry semantics into emission. The pilot consumer is the `take`
// iterator's next-fn (src/codegen_call_iterator.cpp); see
// docs/architecture/codegen-layering-plan.md for the capability decision.
// ===========================================================================

// Function linkage selector for ry_emit_create_function. Numbers are a stable
// boundary contract; do not reorder without updating the RY_LINKAGE_* constants
// in `crates/emit/src/abi.rs` (which map to the core LLVMLinkage).
typedef enum {
    RY_LINKAGE_EXTERNAL = 0,
    RY_LINKAGE_INTERNAL = 1,
    RY_LINKAGE_PRIVATE = 2
} RyLinkage;

// Create a fresh function `name` of type `fn_ty` with `linkage` (a RyLinkage
// value) in the module, and return its opaque handle (NOT interned — same shape
// as ry_emit_create_basic_block's RyBasicBlockRef). Must be interned via
// ry_emit_intern before use as ry_emit_call_indirect's callee_id (RyFunctionRef
// is not a RyValueId). Always adds a new function (the caller owns name
// uniqueness). NULL ctx / fn_ty / name, or an unknown linkage, → NULL. Creates
// no basic blocks.
RyFunctionRef ry_emit_create_function(RyEmitCtx *ctx, const char *name,
                                      RyFuncTypeRef fn_ty, int linkage);

// Add the `nounwind` attribute to `fn_handle` (LLVM `NoUnwind`; mirrors C++
// `llvm::Function::setDoesNotThrow`). NULL ctx / fn_handle → no-op. Added for
// pilot G (#2196): the GC visitor thunk generator marks the per-type
// `__ry_gc_visit_<TypeName>` functions nothrow.
void ry_emit_function_set_nounwind(RyEmitCtx *ctx, RyFunctionRef fn_handle);

// Read the `idx`-th parameter value of `fn`; return the interned result.
// NULL ctx / fn → 0.
RyValueId ry_emit_get_param(RyEmitCtx *ctx, RyFunctionRef fn, uint32_t idx);

// Emit a call through the runtime function-pointer value `callee_id` (a loaded
// `ptr`), typed by `fn_ty`, with `arg_ids[0..arg_count]`; return the interned
// result. Distinct from ry_emit_runtime_call, whose callee is a module-keyed
// name. A void return type drops the SSA name (LLVM forbids naming a void call).
// NULL ctx / fn_ty / callee, a NULL arg array for `arg_count > 0`, or any arg
// resolving to NULL → 0. NULL `name` → empty SSA name.
RyValueId ry_emit_call_indirect(RyEmitCtx *ctx, RyFuncTypeRef fn_ty,
                                RyValueId callee_id, const RyValueId *arg_ids,
                                uint32_t arg_count, const char *name);

// ===========================================================================
// #2102 — LLVM intrinsic call ([D] = (ii) boundary move).
//
// The intrinsic-call capability: declaration acquisition + the call itself
// cross the boundary as a SINGLE primitive. The engine calls
// `LLVMGetIntrinsicDeclaration` for the overloaded `llvm::Function*`, derives
// the FunctionType via `LLVMIntrinsicGetType` engine-side, and emits the call
// with `LLVMBuildCall2`. Nothing about the resulting `llvm::Function*` or its
// FunctionType leaks back to C++ — the alternative (a `get_intrinsic_decl` +
// reuse-`call_indirect` 2-step split) would force C++ to call `getFunctionType`
// on the returned function handle, defeating the boundary-ownership purpose.
//
// `{T, i1}` aggregate results (the `*_with_overflow` family) are then
// decomposed via the existing `ry_emit_extract_value` (#2099). Pilot consumer
// is `emitIntOverflowCheck`'s non-constant path (`src/codegen_arith.cpp`); the
// constant-fold path stays C++-side (APInt compile-time evaluation).
// ===========================================================================

// Emit a call to the overloaded LLVM intrinsic identified by `intrinsic_id`
// (`llvm::Intrinsic::ID` cast to uint32_t — same numeric value across the
// process because `ry` and the cdylib share ONE libLLVM via llvm-sys
// `force-dynamic`), parameterised by `overload_tys[0..overload_count]`, with
// operand `arg_ids[0..arg_count]`. The engine acquires the declaration, derives
// the FunctionType, and emits the call. Return the interned result.
//
// `overload_count == 0` is valid (non-overloaded intrinsics like `llvm.memcpy`
// with explicit type args, or scalar-only ones; the caller must supply exactly
// the type parameters the intrinsic declaration requires — same caller-
// contract as `LLVMBuildCall2`'s arg count).
//
// NULL ctx, a NULL overload-type array for `overload_count > 0`, any NULL
// overload-type element, a NULL arg array for `arg_count > 0`, or any arg
// resolving to NULL → 0. NULL `name` → empty SSA name. A void return type
// drops the SSA name (LLVM forbids naming a void call).
RyValueId ry_emit_intrinsic_call(RyEmitCtx *ctx, uint32_t intrinsic_id,
                                 const RyTypeRef *overload_tys,
                                 uint32_t overload_count,
                                 const RyValueId *arg_ids, uint32_t arg_count,
                                 const char *name);

// ===========================================================================
// #2097 — checked FP→int conversion ([B] = (ii) boundary move).
//
// The shared helper that every `int(<float>)` / `<float> as <int>` cast
// emits — `emitCheckedFPToInt`. The 9 int-target cast cases in
// `src/codegen_expr_cast.cpp` plus the int branch of
// `src/codegen_call_user.cpp::coerceToLowLevelType` all delegate to this
// one helper (10 callsites), so migrating the helper pulls every callsite
// onto the cdylib path without touching the call sites themselves.
//
// The op shape is "BB scaffold + range check + runtime-error exit +
// happy-path cast" — same shape as `ry_emit_bounds_check` (#1996). The
// engine emits FPExt(f32→f64) if needed, an unordered `FCmpULT(lo) |
// FCmpUGE(hi)` (NaN / ±inf / out-of-range all hit failBB with one OR),
// `CondBr` to a fresh failBB / okBB pair, then `emitRuntimeError` in
// failBB (terminator) and FPToSI / FPToUI on the ORIGINAL value in okBB.
//
// `target_width` is the destination integer bit width (8/16/32/64);
// `is_signed != 0` selects FPToSI vs FPToUI and the `[-2^(W-1), 2^(W-1))`
// range (vs `[0, 2^W)` for unsigned). `bb_prefix` names the generated
// blocks and the SSA values inside them (`<prefix>_lo` / `_hi` /
// `_invalid` / `_f64ext` and `<prefix>.fail` / `.ok`). `msg` is the
// fully-formed `runtime error: ...\n` format string the C++ caller built
// from the typeName / siteLabel; `global_name` is the unique global-name
// hint with the CodeGen-owned `fptoi_err_counter_++` baked in. Each
// invocation reuses the existing per-msg cached global; `global_name` is
// a hint only.
//
// The fprintf call uses an inline variadic FunctionType (`isVarArg=1`)
// rather than `ry_emit_runtime_call` (#2100 variadic carve-out). The
// instruction order of the runtime-error exit matches C++
// `emitRuntimeError` byte-for-byte (stderr load → fprintf → stdout load
// → fflush stdout → fflush stderr → _Exit → unreachable) — distinct from
// `ry_emit_bounds_error`'s shape, where the loads are adjacent (#2092
// `load_list_header` instruction-order lesson).
//
// Precondition: the builder must be positioned within a function before
// this call (BBs are created inside it).
// ===========================================================================
RyValueId ry_emit_checked_fp_to_int(RyEmitCtx *ctx, RyValueId val_id,
                                    int target_width, int is_signed,
                                    const char *bb_prefix, const char *msg,
                                    const char *global_name);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // RY_LLVM_EMIT_API_H
