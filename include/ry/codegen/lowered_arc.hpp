#pragma once

namespace llvm {
class Function;
class Value;
}

namespace ry {
class CodeGen;
}

namespace ry::codegen::lowered {

// Pre-lowered ARC retain op. Carries the header pointer (the start of the
// `{i64 strong_count, i64 weak_count}` ARC header) and the atomic mode.
// `atomic` follows CodeGen::isArcAtomic — true inside @parallel for thunks
// where captured ARC ops must use atomicrmw SeqCst to avoid races between
// workers (#630).
struct ArcRetainOp {
    llvm::Value *header_ptr;
    bool atomic;
};

// Pre-lowered ARC release op. Both `destructor_callee` and `gc_visit_fn` are
// nullable: a null destructor skips the user-supplied dtor call; a null
// gc_visit_fn omits the `arc.gc_track` BB entirely (dead → arc.release,
// alive → arc.done). The pair maps directly to the codegen-side
// `(llvm::FunctionCallee, llvm::Function *)` shape — the lowering layer
// extracts the C-fnptr `Value*` from FunctionCallee so the LLVM-typed pair
// does not need to cross the boundary.
struct ArcReleaseOp {
    llvm::Value *header_ptr;
    bool atomic;
    llvm::Value *destructor_callee; // nullable
    llvm::Function *gc_visit_fn;    // nullable
};

} // namespace ry::codegen::lowered

namespace ry::codegen::lowering {

// Passthrough lowering — no IRBuilder usage, just struct construction.
// `destructor_callee` is the result of `FunctionCallee::getCallee()`; pass
// nullptr when no destructor is needed. Mirrors lowerResultBranch (#1964) /
// lowerBoundsCheck (#1961) in shape.
lowered::ArcRetainOp lowerArcRetain(CodeGen &cg, llvm::Value *header_ptr,
                                    bool atomic);
lowered::ArcReleaseOp lowerArcRelease(CodeGen &cg, llvm::Value *header_ptr,
                                      bool atomic,
                                      llvm::Value *destructor_callee,
                                      llvm::Function *gc_visit_fn);

} // namespace ry::codegen::lowering

namespace ry::codegen::emission {

// Emit an ARC retain via the libemit boundary (ry_emit_arc_retain).
// Precondition: the caller has called ry_emit_ctx_set_function(cg.emit_ctx_,
// cg.fn_) before invoking this helper, because two BBs (`arc.retain`,
// `arc.retain.done`) are created inside the current function.
void emitArcRetain(CodeGen &cg, const lowered::ArcRetainOp &op);

// Emit an ARC release via the libemit boundary (ry_emit_arc_release).
// Precondition: same as emitArcRetain (up to six BBs are created depending on
// gc_visit_fn). The caller must additionally register "gc" in
// CodeGen::used_native_libraries_ because release emits __ry_gc_track /
// __ry_gc_untrack calls (CodeGen-side bookkeeping not visible to the boundary).
void emitArcRelease(CodeGen &cg, const lowered::ArcReleaseOp &op);

} // namespace ry::codegen::emission
