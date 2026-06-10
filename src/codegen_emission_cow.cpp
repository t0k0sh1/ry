#include "ry/codegen/lowered_cow.hpp"
#include "ry/codegen.hpp"
#include "ry/llvm_emit/api.h"
#include "ry/llvm_emit/cast_helpers.hpp"

#include <llvm/IR/Value.h>

namespace ry::codegen::emission {

llvm::Value *emitCowEnsureUnique(CodeGen &cg,
                                 const lowered::CowEnsureUniqueOp &op) {
    cg.used_native_libraries_.insert("gc");

    RyCowEnsureUniqueDesc desc{};
    desc.data_ptr_id = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(op.data_ptr));
    desc.slot_ptr_id = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(op.slot_ptr));
    desc.kind = op.kind;
    desc.atomic = op.atomic ? RY_ARC_ATOMIC : RY_ARC_NONATOMIC;
    desc.elem_size = op.elem_size;
    desc.key_size = op.key_size;
    desc.val_size = op.val_size;
    desc.do_elem_retain = op.do_elem_retain ? 1 : 0;
    desc.elem_is_str = op.elem_is_str ? 1 : 0;
    desc.do_key_retain = op.do_key_retain ? 1 : 0;
    desc.key_is_str = op.key_is_str ? 1 : 0;
    desc.destructor_callee = ry::llvm_emit::asRyValue(op.destructor_callee);

    RyValueId newDataId = ry_emit_cow_ensure_unique(cg.emit_ctx_, &desc);
    return ry::llvm_emit::asLlvmValue(ry_emit_resolve(cg.emit_ctx_, newDataId));
}

} // namespace ry::codegen::emission
