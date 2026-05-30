#include "ry/codegen/lowered_collection_mutate.hpp"
#include "ry/codegen.hpp"

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

namespace ry::codegen::lowering {

lowered::CollectionAppendOp lowerCollectionAppend(CodeGen &cg,
                                                  llvm::Value *list_ptr,
                                                  llvm::Value *val,
                                                  llvm::StructType *list_header_ty,
                                                  llvm::Type *elem_ty,
                                                  uint64_t elem_size) {
    (void)cg;
    return lowered::CollectionAppendOp{list_ptr, val, list_header_ty, elem_ty,
                                       elem_size};
}

lowered::CollectionInsertOp lowerCollectionInsert(CodeGen &cg,
                                                  llvm::Value *list_ptr,
                                                  llvm::Value *idx,
                                                  llvm::Value *val,
                                                  llvm::StructType *list_header_ty,
                                                  llvm::Type *elem_ty,
                                                  uint64_t elem_size) {
    (void)cg;
    return lowered::CollectionInsertOp{list_ptr, idx, val, list_header_ty,
                                       elem_ty, elem_size};
}

lowered::CollectionRemoveAtOp lowerCollectionRemoveAt(CodeGen &cg,
                                                     llvm::Value *list_ptr,
                                                     llvm::Value *idx,
                                                     llvm::StructType *list_header_ty,
                                                     llvm::Type *elem_ty,
                                                     uint64_t elem_size) {
    (void)cg;
    return lowered::CollectionRemoveAtOp{list_ptr, idx, list_header_ty,
                                         elem_ty, elem_size};
}

lowered::ListSliceOp lowerListSlice(CodeGen &cg, llvm::Value *list_ptr,
                                    llvm::Value *start,
                                    llvm::Value *end_excl,
                                    llvm::StructType *list_header_ty,
                                    llvm::Type *elem_ty, uint64_t elem_size) {
    (void)cg;
    return lowered::ListSliceOp{list_ptr, start, end_excl, list_header_ty,
                                elem_ty, elem_size};
}

} // namespace ry::codegen::lowering
