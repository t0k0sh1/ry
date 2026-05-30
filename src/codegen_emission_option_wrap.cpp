#include "ry/codegen/lowered_option_wrap.hpp"
#include "ry/codegen.hpp"
#include "ry/llvm_emit/api.h"

#include <llvm/IR/Value.h>

namespace ry::codegen::emission {

llvm::Value *emitOptionWrap(CodeGen &cg, const lowered::OptionWrapOp &op) {
    // No ry_emit_ctx_set_function call: this op creates no basic blocks
    // (pure UndefValue + InsertValue sequence), so the currently-active LLVM
    // function pointer is not consulted by the ABI side. Difference from
    // emitBoundsCheck / emitResultBranch, which split BBs and therefore must
    // sync cg.fn_ first.
    RyValueId resultId;
    if (op.is_some) {
        RyValueId innerId = ry_emit_intern(cg.emit_ctx_, op.inner);
        resultId = ry_emit_option_wrap_some(cg.emit_ctx_, innerId, op.opt_ty);
    } else {
        resultId = ry_emit_option_wrap_none(cg.emit_ctx_, op.opt_ty);
    }
    return static_cast<llvm::Value *>(ry_emit_resolve(cg.emit_ctx_, resultId));
}

} // namespace ry::codegen::emission
