#include "ry/codegen/lowered_any.hpp"
#include "ry/codegen.hpp"
#include "ry/llvm_emit/api.h"

#include <llvm/IR/Constant.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Value.h>

namespace ry::codegen::emission {

llvm::Value *emitAnyWrap(CodeGen &cg, const lowered::AnyWrapOp &op) {
    ry_emit_ctx_set_function(cg.emit_ctx_, cg.fn_);

    RyAnyWrapDesc desc{};
    desc.kind = static_cast<int>(op.kind);
    desc.target_tag = op.target_tag;
    desc.val_id = ry_emit_intern(cg.emit_ctx_, op.val);
    desc.do_collection_retain = op.do_collection_retain ? 1 : 0;
    desc.do_str_retain = op.do_str_retain ? 1 : 0;
    desc.descriptor_id =
        op.descriptor ? ry_emit_intern(cg.emit_ctx_, op.descriptor) : 0;
    desc.box_layout_ty_ptr = static_cast<void *>(op.box_layout_ty);
    desc.box_data_size = op.box_data_size;
    desc.any_ty_ptr = static_cast<void *>(op.any_ty);

    RyValueId resultId = ry_emit_any_wrap(cg.emit_ctx_, &desc);
    return static_cast<llvm::Value *>(ry_emit_resolve(cg.emit_ctx_, resultId));
}

llvm::Value *emitAnyUnwrap(CodeGen &cg, const lowered::AnyUnwrapOp &op) {
    ry_emit_ctx_set_function(cg.emit_ctx_, cg.fn_);

    RyAnyUnwrapDesc desc{};
    desc.kind = static_cast<int>(op.kind);
    desc.any_val_id = ry_emit_intern(cg.emit_ctx_, op.any_val);
    desc.any_ty_ptr = static_cast<void *>(op.any_ty);
    desc.target_ty_ptr = static_cast<void *>(op.target_ty);
    desc.expected_tag = op.expected_tag;
    desc.do_collection_retain = op.do_collection_retain ? 1 : 0;
    desc.do_str_retain = op.do_str_retain ? 1 : 0;
    desc.mismatch_msg = op.mismatch_msg.c_str();
    desc.mismatch_global_name = op.mismatch_global_name.c_str();
    desc.expected_desc_id =
        op.expected_desc ? ry_emit_intern(cg.emit_ctx_, op.expected_desc) : 0;
    desc.box_layout_ty_ptr = static_cast<void *>(op.box_layout_ty);
    desc.record_struct_ty_ptr = static_cast<void *>(op.record_struct_ty);
    desc.desc_mismatch_msg = op.desc_mismatch_msg.c_str();
    desc.desc_mismatch_global_name = op.desc_mismatch_global_name.c_str();

    RyValueId resultId = ry_emit_any_unwrap(cg.emit_ctx_, &desc);
    return static_cast<llvm::Value *>(ry_emit_resolve(cg.emit_ctx_, resultId));
}

llvm::Value *emitAnyTryUnwrap(CodeGen &cg, const lowered::AnyTryUnwrapOp &op) {
    ry_emit_ctx_set_function(cg.emit_ctx_, cg.fn_);

    RyAnyTryUnwrapDesc desc{};
    desc.kind = static_cast<int>(op.kind);
    desc.any_val_id = ry_emit_intern(cg.emit_ctx_, op.any_val);
    desc.any_ty_ptr = static_cast<void *>(op.any_ty);
    desc.res_ty_ptr = static_cast<void *>(op.res_ty);
    desc.error_ty_ptr = static_cast<void *>(op.error_ty);
    desc.target_ty_ptr = static_cast<void *>(op.target_ty);
    desc.expected_tag = op.expected_tag;
    desc.do_collection_retain = op.do_collection_retain ? 1 : 0;
    desc.do_str_retain = op.do_str_retain ? 1 : 0;
    desc.err_msg_str_id =
        op.err_msg_str ? ry_emit_intern(cg.emit_ctx_, op.err_msg_str) : 0;

    RyValueId resultId = ry_emit_any_try_unwrap(cg.emit_ctx_, &desc);
    return static_cast<llvm::Value *>(ry_emit_resolve(cg.emit_ctx_, resultId));
}

} // namespace ry::codegen::emission
