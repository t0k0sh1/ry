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
