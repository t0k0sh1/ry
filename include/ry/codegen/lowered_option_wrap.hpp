#pragma once

namespace llvm {
class StructType;
class Value;
}

namespace ry {
class CodeGen;
}

namespace ry::codegen::lowered {

// Pre-lowered Option-wrap op. A single struct carries both Some/None forms:
// is_some selects which arm to emit, inner is the payload for the Some arm
// (must be non-null when is_some, ignored otherwise — the None arm derives
// its payload slot from opt_ty->getElementType(1) at emission time so the
// caller does not need to synthesize an UndefValue ahead of time).
struct OptionWrapOp {
    llvm::Value *inner;
    llvm::StructType *opt_ty;
    bool is_some;
};

} // namespace ry::codegen::lowered

namespace ry::codegen::lowering {

// No constant-fold path today — the struct + helper shape is kept symmetric
// with BoundsCheck (#1961) and ResultBranch (#1964) so a future fold (e.g.
// when inner is a ConstantAggregate so the whole Option<T> can be a constant
// initializer) can be added without reshuffling callers.
lowered::OptionWrapOp lowerOptionWrap(CodeGen &cg, llvm::Value *inner,
                                      llvm::StructType *opt_ty, bool is_some);

} // namespace ry::codegen::lowering

namespace ry::codegen::emission {

// Emit an Option<T> wrap producing either Some(inner) or None, depending on
// op.is_some. Unlike emitBoundsCheck / emitResultBranch, this op creates no
// basic blocks (it is a pure UndefValue + two InsertValue sequence), so it
// does NOT call ry_emit_ctx_set_function — the current LLVM function pointer
// is irrelevant. No trampoline is needed because there are no caller-side
// closures to bridge.
//
// The Ry-semantic side effects of the Some arm (propagateMeta and
// tryRetainArcSource on the inner, #999 ARC retain contract) are intentionally
// NOT emitted here; they stay in the CodeGen-level shim (see
// CodeGen::buildSomeValue / buildNoneValue in src/codegen_type.cpp). This
// keeps the emission layer a pure intern → boundary → resolve transit, so the Rust
// implementation (crates/ry_codegen/) lives behind the boundary without touching
// the side-table layer (value_metadata_ and the ARC source map).
llvm::Value *emitOptionWrap(CodeGen &cg, const lowered::OptionWrapOp &op);

} // namespace ry::codegen::emission
