#include "ry/codegen/lowered_bounds_check.hpp"
#include "ry/codegen.hpp"

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/IRBuilder.h>

#include <string>

namespace ry::codegen::emission {

llvm::Value *emitBoundsCheck(CodeGen &cg, const lowered::BoundsCheckOp &op,
                             const std::string &bb_prefix) {
    llvm::Value *idx = op.idx;
    if (idx->getType() == cg.i1Ty_)
        idx = cg.builder_.CreateZExt(idx, cg.i64Ty_, "idx_ext");

    llvm::Value *origIndex = idx;
    idx = cg.emitNegativeIndexWrap(idx, op.len, bb_prefix);

    llvm::Value *zero = llvm::ConstantInt::get(cg.i64Ty_, 0);
    llvm::Value *negCheck = cg.builder_.CreateICmpSLT(idx, zero, bb_prefix + "_neg");
    llvm::Value *overCheck = cg.builder_.CreateICmpSGE(idx, op.len, bb_prefix + "_over");
    llvm::Value *oob = cg.builder_.CreateOr(negCheck, overCheck, bb_prefix + "_oob");
    llvm::BasicBlock *oobBB = llvm::BasicBlock::Create(*cg.ctx_, bb_prefix + ".oob", cg.fn_);
    llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*cg.ctx_, bb_prefix + ".ok", cg.fn_);
    cg.builder_.CreateCondBr(oob, oobBB, okBB);
    cg.builder_.SetInsertPoint(oobBB);

    const char *fmtMsg = (op.error_spec.kind == lowered::BoundsKind::List)
        ? "runtime error: index %lld out of bounds for list of length %lld\n"
        : "runtime error: index %lld out of bounds for array of length %lld\n";
    cg.emitBoundsError(origIndex, op.len, fmtMsg, op.error_spec.global_name);
    cg.builder_.SetInsertPoint(okBB);
    return idx;
}

} // namespace ry::codegen::emission
