#include "ry/codegen/lowered_bounds_check.hpp"
#include "ry/codegen.hpp"
#include "ry/llvm_emit/api.h"
#include "ry/llvm_emit/cast_helpers.hpp"

#include <llvm/IR/Value.h>

#include <string>

namespace ry::codegen::emission {

llvm::Value *emitBoundsCheck(CodeGen &cg, const lowered::BoundsCheckOp &op,
                             const std::string &bb_prefix) {
    // Sync the LLVM function pointer with the boundary ctx so internal
    // BasicBlock::Create calls inside ry_emit_bounds_check land in the
    // current function. Other boundary entrypoints (build_error_from_runtime,
    // get_runtime_fn) do not need this because they only emit at the
    // builder's current insert point.
    ry_emit_ctx_set_function(cg.emit_ctx_, ry::llvm_emit::asRyFunction(cg.fn_));

    RyValueId idxId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(op.idx));
    RyValueId lenId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(op.len));
    RyBoundsKind kind = (op.error_spec.kind == lowered::BoundsKind::List)
                            ? RY_BOUNDS_LIST
                            : RY_BOUNDS_ARRAY;
    RyValueId resultId =
        ry_emit_bounds_check(cg.emit_ctx_, idxId, lenId, kind,
                             op.error_spec.global_name.c_str(),
                             bb_prefix.c_str());
    return ry::llvm_emit::asLlvmValue(ry_emit_resolve(cg.emit_ctx_, resultId));
}

} // namespace ry::codegen::emission
