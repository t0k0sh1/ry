#include "ry/codegen/lowered_bounds_check.hpp"
#include "ry/codegen.hpp"
#include "ry/llvm_emit/api.h"
#include "ry/llvm_emit/cast_helpers.hpp"

#include <llvm/IR/Value.h>

#include <string>

namespace ry::codegen::emission {

llvm::Value *emitBoundsCheck(CodeGen &cg, const lowered::BoundsCheckOp &op,
                             const std::string &bb_prefix) {
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
