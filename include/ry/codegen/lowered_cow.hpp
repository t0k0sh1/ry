#pragma once

#include <cstdint>

namespace llvm {
class Value;
}

namespace ry {
class CodeGen;
}

namespace ry::codegen::lowered {

// Pre-lowered Copy-on-Write uniqueness check (`emitCowCheckSlot`). Carries
// every input the boundary side needs after CodeGen has resolved kind-specific
// metadata. The struct lives on the CodeGen side; the emission layer
// flattens it to the C-only `RyCowEnsureUniqueDesc` before calling
// `ry_emit_cow_ensure_unique` so no LLVM-typed pointers cross the boundary
// boundary other than the opaque `Value *` operands that are interned to
// `RyValueId` handles.
struct CowEnsureUniqueOp {
    llvm::Value *data_ptr;          // current ARC-backed dataPtr
    llvm::Value *slot_ptr;          // alloca / GEP receiving the new dataPtr
    int kind;                        // RyCowKind (0=List, 1=Map, 2=Set)
    bool atomic;                     // mirrors CodeGen::isArcAtomic(dataPtr)
    uint64_t elem_size;              // List/Set element bytes (0 for Map)
    uint64_t key_size;               // Map key bytes (0 for List/Set)
    uint64_t val_size;               // Map value bytes (0 for List/Set)
    bool do_elem_retain;             // retain each cloned element after memcpy
    bool elem_is_str;                // element header offset: -24 vs -16
    bool do_key_retain;              // Map only: retain each cloned key
    bool key_is_str;                 // Map only: key header offset
    llvm::Value *destructor_callee;  // for the internal arc_release; nullable
};

} // namespace ry::codegen::lowered

namespace ry::codegen::lowering {

// Passthrough lowering — collects the descriptor without emitting any IR.
// Mirrors lowerArcRetain / lowerArcRelease (#1968) in shape; the only
// non-trivial work is the FunctionCallee → Value* unwrap that the caller
// has already performed.
lowered::CowEnsureUniqueOp lowerCowEnsureUnique(
    CodeGen &cg, llvm::Value *data_ptr, llvm::Value *slot_ptr, int kind,
    bool atomic, uint64_t elem_size, uint64_t key_size, uint64_t val_size,
    bool do_elem_retain, bool elem_is_str, bool do_key_retain,
    bool key_is_str, llvm::Value *destructor_callee);

} // namespace ry::codegen::lowering

namespace ry::codegen::emission {

// Emit the CoW uniqueness check via the libemit boundary
// (`ry_emit_cow_ensure_unique`). Returns the PHI joining the original
// dataPtr (skip-CoW path) with the freshly-cloned dataPtr (copy path);
// downstream callers feed this into `propagateMeta` / `propagateMetaWide`.
//
// Preconditions:
//   - The caller has called `ry_emit_ctx_set_function(cg.emit_ctx_, cg.fn_)`
//     because the helper creates basic blocks inside the current function.
//   - The caller has inserted `"gc"` into `cg.used_native_libraries_`
//     because the internal `ry_emit_arc_release` emits __ry_gc_track /
//     __ry_gc_untrack calls (CodeGen-side bookkeeping not visible to the
//     boundary).
llvm::Value *emitCowEnsureUnique(CodeGen &cg,
                                 const lowered::CowEnsureUniqueOp &op);

} // namespace ry::codegen::emission
