#include "ry/codegen/lowered_runtime_call.hpp"
#include "ry/codegen.hpp"
#include "ry/llvm_emit/api.h"
#include "ry/llvm_emit/cast_helpers.hpp"

#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

#include <vector>

namespace ry::codegen::emission {

llvm::Value *emitRuntimeCall(CodeGen &cg, const lowered::RuntimeCallOp &op,
                             const char *name_hint) {
    // Marshal arg type pointers and arg value handles into ABI-friendly
    // parallel arrays. Type pointers cross the ABI as `RyTypeRef` (#1973);
    // value pointers cross as opaque `RyValueId` handles via intern/resolve.
    std::vector<RyTypeRef> arg_ty_refs;
    arg_ty_refs.reserve(op.arg_tys.size());
    for (auto *t : op.arg_tys)
        arg_ty_refs.push_back(ry::llvm_emit::asRyType(t));

    std::vector<RyValueId> arg_ids;
    arg_ids.reserve(op.args.size());
    for (auto *v : op.args)
        arg_ids.push_back(ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(v)));

    RyValueId resultId = ry_emit_runtime_call(
        cg.emit_ctx_, op.name, ry::llvm_emit::asRyType(op.ret_ty),
        arg_ty_refs.data(), static_cast<uint32_t>(arg_ty_refs.size()),
        arg_ids.data(), static_cast<uint32_t>(arg_ids.size()), name_hint);
    return ry::llvm_emit::asLlvmValue(ry_emit_resolve(cg.emit_ctx_, resultId));
}

} // namespace ry::codegen::emission
