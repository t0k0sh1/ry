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

} // namespace ry::codegen::emission
