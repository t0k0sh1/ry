#pragma once

namespace llvm {
class StructType;
class Value;
}

namespace ry {
class CodeGen;
}

namespace ry::codegen::lowered {

// Pre-lowered Result-branch op. Carries the i1 condition and the Result<T, E>
// struct type; the actual ok/err value builders are passed at the call site
// (they are caller-side lambdas that may reference local IRBuilder state and
// thus cannot be embedded in the lowered op).
struct ResultBranchOp {
    llvm::Value *is_err;
    llvm::StructType *res_ty;
};

} // namespace ry::codegen::lowered

namespace ry::codegen::lowering {

// No constant-fold path today — the ok/err builders are runtime lambdas, so
// the lowering layer cannot collapse the branch into a single side at compile
// time. The struct + helper shape is kept symmetric with BoundsCheck (#1961)
// so a future fold (e.g. when is_err is a ConstantInt and both builders are
// pure) can be added without reshuffling callers.
lowered::ResultBranchOp lowerResultBranch(CodeGen &cg, llvm::Value *is_err,
                                          llvm::StructType *res_ty);

} // namespace ry::codegen::lowering
