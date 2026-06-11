#include "ry/codegen/lowered_collection_mutate.hpp"
#include "ry/codegen.hpp"
#include "ry/llvm_emit/api.h"
#include "ry/llvm_emit/cast_helpers.hpp"

#include <llvm/IR/Value.h>

namespace ry::codegen::emission {

void emitCollectionAppend(CodeGen &cg,
                          const lowered::CollectionAppendOp &op) {
    RyValueId listId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(op.list_ptr));
    RyValueId valId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(op.val));
    ry_emit_collection_append(cg.emit_ctx_, listId, valId,
                              ry::llvm_emit::asRyType(op.list_header_ty),
                              ry::llvm_emit::asRyType(op.elem_ty), op.elem_size);
}

void emitCollectionInsert(CodeGen &cg,
                          const lowered::CollectionInsertOp &op) {
    RyValueId listId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(op.list_ptr));
    RyValueId idxId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(op.idx));
    RyValueId valId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(op.val));
    ry_emit_collection_insert(cg.emit_ctx_, listId, idxId, valId,
                              ry::llvm_emit::asRyType(op.list_header_ty),
                              ry::llvm_emit::asRyType(op.elem_ty), op.elem_size);
}

llvm::Value *emitCollectionRemoveAt(CodeGen &cg,
                                    const lowered::CollectionRemoveAtOp &op) {
    RyValueId listId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(op.list_ptr));
    RyValueId idxId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(op.idx));
    RyValueId removedId = ry_emit_collection_remove_at(
        cg.emit_ctx_, listId, idxId,
        ry::llvm_emit::asRyType(op.list_header_ty),
        ry::llvm_emit::asRyType(op.elem_ty), op.elem_size);
    return ry::llvm_emit::asLlvmValue(ry_emit_resolve(cg.emit_ctx_, removedId));
}

lowered::ListSliceResult emitListSlice(CodeGen &cg,
                                       const lowered::ListSliceOp &op) {
    RyValueId listId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(op.list_ptr));
    RyValueId startId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(op.start));
    RyValueId endId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(op.end_excl));
    RyValueId countId = 0;
    RyValueId newDataId = 0;
    ry_emit_list_slice(cg.emit_ctx_, listId, startId, endId,
                       ry::llvm_emit::asRyType(op.list_header_ty),
                       ry::llvm_emit::asRyType(op.elem_ty), op.elem_size,
                       &countId, &newDataId);
    lowered::ListSliceResult result{};
    result.count =
        ry::llvm_emit::asLlvmValue(ry_emit_resolve(cg.emit_ctx_, countId));
    result.new_data =
        ry::llvm_emit::asLlvmValue(ry_emit_resolve(cg.emit_ctx_, newDataId));
    return result;
}

llvm::Value *emitListCopyFull(CodeGen &cg, llvm::Value *src_data,
                              llvm::Value *count, uint64_t elem_size, int kind) {
    RyValueId srcId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(src_data));
    RyValueId countId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(count));
    RyValueId newDataId =
        ry_emit_list_copy_full(cg.emit_ctx_, srcId, countId, elem_size, kind);
    return ry::llvm_emit::asLlvmValue(ry_emit_resolve(cg.emit_ctx_, newDataId));
}

llvm::Value *emitListAppendedCopy(CodeGen &cg, llvm::Value *new_len,
                                  llvm::Value *old_len, llvm::Value *src_data,
                                  uint64_t elem_size) {
    RyValueId newLenId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(new_len));
    RyValueId oldLenId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(old_len));
    RyValueId srcId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(src_data));
    RyValueId newDataId =
        ry_emit_list_appended(cg.emit_ctx_, newLenId, oldLenId, srcId, elem_size);
    return ry::llvm_emit::asLlvmValue(ry_emit_resolve(cg.emit_ctx_, newDataId));
}

llvm::Value *emitListConcatCopy(CodeGen &cg, llvm::Value *lhs_len,
                                llvm::Value *lhs_data, llvm::Value *rhs_len,
                                llvm::Value *rhs_data, llvm::Value *new_len,
                                llvm::Type *elem_ty, uint64_t elem_size) {
    RyValueId lhsLenId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(lhs_len));
    RyValueId lhsDataId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(lhs_data));
    RyValueId rhsLenId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(rhs_len));
    RyValueId rhsDataId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(rhs_data));
    RyValueId newLenId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(new_len));
    RyValueId newDataId =
        ry_emit_list_concat(cg.emit_ctx_, lhsLenId, lhsDataId, rhsLenId, rhsDataId,
                            newLenId, ry::llvm_emit::asRyType(elem_ty), elem_size);
    return ry::llvm_emit::asLlvmValue(ry_emit_resolve(cg.emit_ctx_, newDataId));
}

} // namespace ry::codegen::emission
