#include "ry/codegen/lowered_arc.hpp"
#include "ry/codegen.hpp"

#include <llvm/IR/Function.h>
#include <llvm/IR/Value.h>

namespace ry::codegen::lowering {

lowered::ArcRetainOp lowerArcRetain(CodeGen &cg, llvm::Value *header_ptr,
                                    bool atomic) {
    (void)cg;
    return lowered::ArcRetainOp{header_ptr, atomic};
}

lowered::ArcReleaseOp lowerArcRelease(CodeGen &cg, llvm::Value *header_ptr,
                                      bool atomic,
                                      llvm::Value *destructor_callee,
                                      llvm::Function *gc_visit_fn) {
    (void)cg;
    return lowered::ArcReleaseOp{header_ptr, atomic, destructor_callee,
                                 gc_visit_fn};
}

} // namespace ry::codegen::lowering
