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
