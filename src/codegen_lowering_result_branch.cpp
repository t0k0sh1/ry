#include "ry/codegen/lowered_result_branch.hpp"
#include "ry/codegen.hpp"

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Value.h>

namespace ry::codegen::lowering {

lowered::ResultBranchOp lowerResultBranch(CodeGen &cg, llvm::Value *is_err,
                                          llvm::StructType *res_ty) {
    (void)cg;
    return lowered::ResultBranchOp{is_err, res_ty};
}

} // namespace ry::codegen::lowering
