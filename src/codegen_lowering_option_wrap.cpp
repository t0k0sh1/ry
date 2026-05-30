#include "ry/codegen/lowered_option_wrap.hpp"
#include "ry/codegen.hpp"

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Value.h>

namespace ry::codegen::lowering {

lowered::OptionWrapOp lowerOptionWrap(CodeGen &cg, llvm::Value *inner,
                                      llvm::StructType *opt_ty, bool is_some) {
    (void)cg;
    return lowered::OptionWrapOp{inner, opt_ty, is_some};
}

} // namespace ry::codegen::lowering
