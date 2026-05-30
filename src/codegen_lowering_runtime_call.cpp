#include "ry/codegen/lowered_runtime_call.hpp"
#include "ry/codegen.hpp"

#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

namespace ry::codegen::lowering {

lowered::RuntimeCallOp lowerRuntimeCall(CodeGen &cg, const char *name,
                                        llvm::Type *ret_ty,
                                        llvm::ArrayRef<llvm::Type *> arg_tys,
                                        llvm::ArrayRef<llvm::Value *> args) {
    (void)cg;
    return lowered::RuntimeCallOp{name, ret_ty, arg_tys, args};
}

} // namespace ry::codegen::lowering
