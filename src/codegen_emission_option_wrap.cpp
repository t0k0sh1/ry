#include "ry/codegen/lowered_option_wrap.hpp"
#include "ry/codegen.hpp"
#include "ry/llvm_emit/api.h"
#include "ry/llvm_emit/cast_helpers.hpp"

#include <llvm/IR/Value.h>

namespace ry::codegen::emission {

llvm::Value *emitOptionWrap(CodeGen &cg, const lowered::OptionWrapOp &op) {
    // This op creates no basic blocks (a pure UndefValue + InsertValue
    // sequence), so there is no parent function to position — unlike the
    // BB-creating boundary ops, which derive their parent from the builder's
    // insert block.
    RyValueId resultId;
    if (op.is_some) {
        RyValueId innerId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(op.inner));
        resultId = ry_emit_option_wrap_some(cg.emit_ctx_, innerId,
                                             ry::llvm_emit::asRyType(op.opt_ty));
    } else {
        resultId = ry_emit_option_wrap_none(cg.emit_ctx_,
                                             ry::llvm_emit::asRyType(op.opt_ty));
    }
    return ry::llvm_emit::asLlvmValue(ry_emit_resolve(cg.emit_ctx_, resultId));
}

} // namespace ry::codegen::emission
