#include "ry/codegen/lowered_reduce.hpp"
#include "ry/codegen.hpp"
#include "ry/llvm_emit/api.h"
#include "ry/llvm_emit/cast_helpers.hpp"

#include <llvm/IR/Value.h>

namespace ry::codegen::emission {

llvm::Value *emitReduceSumList(CodeGen &cg, llvm::Value *list_ptr,
                               llvm::Type *elem_ty, llvm::Type *list_header_ty) {
    RyValueId listId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(list_ptr));
    RyValueId resultId =
        ry_emit_reduce_sum_list(cg.emit_ctx_, listId,
                                ry::llvm_emit::asRyType(elem_ty),
                                ry::llvm_emit::asRyType(list_header_ty));
    return ry::llvm_emit::asLlvmValue(ry_emit_resolve(cg.emit_ctx_, resultId));
}

llvm::Value *emitReduceSumStep(CodeGen &cg, llvm::Value *acc, llvm::Value *v,
                               llvm::Type *elem_ty) {
    RyValueId accId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(acc));
    RyValueId vId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(v));
    RyValueId resultId = ry_emit_reduce_sum_step(cg.emit_ctx_, accId, vId,
                                                 ry::llvm_emit::asRyType(elem_ty));
    return ry::llvm_emit::asLlvmValue(ry_emit_resolve(cg.emit_ctx_, resultId));
}

llvm::Value *emitReduceMinmaxListLoop(CodeGen &cg, llvm::Value *data,
                                      llvm::Value *len, llvm::Type *elem_ty,
                                      bool is_max) {
    RyValueId dataId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(data));
    RyValueId lenId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(len));
    RyValueId resultId = ry_emit_reduce_minmax_list_loop(
        cg.emit_ctx_, dataId, lenId, ry::llvm_emit::asRyType(elem_ty),
        is_max ? 1 : 0);
    return ry::llvm_emit::asLlvmValue(ry_emit_resolve(cg.emit_ctx_, resultId));
}

llvm::Value *emitReduceMinmaxStep(CodeGen &cg, llvm::Value *best, llvm::Value *v,
                                  llvm::Type *elem_ty, bool is_max) {
    RyValueId bestId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(best));
    RyValueId vId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(v));
    RyValueId resultId = ry_emit_reduce_minmax_step(
        cg.emit_ctx_, bestId, vId, ry::llvm_emit::asRyType(elem_ty),
        is_max ? 1 : 0);
    return ry::llvm_emit::asLlvmValue(ry_emit_resolve(cg.emit_ctx_, resultId));
}

} // namespace ry::codegen::emission
