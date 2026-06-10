#pragma once

#include <llvm/ADT/STLFunctionalExtras.h>

namespace llvm {
class StructType;
class Value;
}

namespace ry {
class CodeGen;
}

namespace ry::codegen::lowered {

// Pre-lowered Result-branch op. Carries the i1 condition and the Result<T, E>
// struct type; the actual ok/err value builders are passed at emission time
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

namespace ry::codegen::emission {

// Emit a Result-branch IR sequence (res.ok / res.err / res.merge + PHI).
// build_ok and build_err run inside their respective BBs (the emission helper
// switches the builder's insert point before calling them). Returns the PHI
// holding the merged Result<T, E> value, so callers can attach metadata via
// propagateMeta (e.g. emitResultBranchWithMeta in src/codegen_call_io.cpp).
//
// Precondition: the builder must be positioned within a function before
// invoking this helper, because three BBs are created — their parent is
// derived from the builder's insert block (same as emission::emitBoundsCheck).
llvm::Value *emitResultBranch(CodeGen &cg, const lowered::ResultBranchOp &op,
                              llvm::function_ref<llvm::Value *()> build_ok,
                              llvm::function_ref<llvm::Value *()> build_err);

} // namespace ry::codegen::emission
