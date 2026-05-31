#include "ry/codegen/lowered_any.hpp"
#include "ry/codegen.hpp"

#include <llvm/IR/Constant.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Value.h>

namespace ry::codegen::lowering {

lowered::AnyWrapOp lowerAnyWrap(CodeGen &cg, lowered::AnyWrapKind kind,
                                int64_t target_tag, llvm::Value *val,
                                bool do_collection_retain, bool do_str_retain,
                                llvm::Constant *descriptor,
                                llvm::StructType *box_layout_ty,
                                uint64_t box_data_size,
                                llvm::StructType *any_ty) {
    (void)cg;
    return lowered::AnyWrapOp{kind,           target_tag,
                              val,            do_collection_retain,
                              do_str_retain,  descriptor,
                              box_layout_ty,  box_data_size,
                              any_ty};
}

lowered::AnyUnwrapOp lowerAnyUnwrap(
    CodeGen &cg, lowered::AnyUnwrapKind kind, llvm::Value *any_val,
    llvm::StructType *any_ty, llvm::Type *target_ty, int64_t expected_tag,
    bool do_collection_retain, bool do_str_retain, std::string mismatch_msg,
    std::string mismatch_global_name, llvm::Constant *expected_desc,
    llvm::StructType *box_layout_ty, llvm::StructType *record_struct_ty,
    std::string desc_mismatch_msg, std::string desc_mismatch_global_name) {
    (void)cg;
    return lowered::AnyUnwrapOp{kind,
                                any_val,
                                any_ty,
                                target_ty,
                                expected_tag,
                                do_collection_retain,
                                do_str_retain,
                                std::move(mismatch_msg),
                                std::move(mismatch_global_name),
                                expected_desc,
                                box_layout_ty,
                                record_struct_ty,
                                std::move(desc_mismatch_msg),
                                std::move(desc_mismatch_global_name)};
}

lowered::AnyTryUnwrapOp lowerAnyTryUnwrap(
    CodeGen &cg, lowered::AnyTryUnwrapKind kind, llvm::Value *any_val,
    llvm::StructType *any_ty, llvm::StructType *res_ty,
    llvm::StructType *error_ty, llvm::Type *target_ty, int64_t expected_tag,
    bool do_collection_retain, bool do_str_retain, llvm::Value *err_msg_str) {
    (void)cg;
    return lowered::AnyTryUnwrapOp{kind,
                                   any_val,
                                   any_ty,
                                   res_ty,
                                   error_ty,
                                   target_ty,
                                   expected_tag,
                                   do_collection_retain,
                                   do_str_retain,
                                   err_msg_str};
}

} // namespace ry::codegen::lowering
