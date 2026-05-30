#include "ry/codegen/lowered_cow.hpp"
#include "ry/codegen.hpp"

#include <llvm/IR/Value.h>

namespace ry::codegen::lowering {

lowered::CowEnsureUniqueOp lowerCowEnsureUnique(
    CodeGen &cg, llvm::Value *data_ptr, llvm::Value *slot_ptr, int kind,
    bool atomic, uint64_t elem_size, uint64_t key_size, uint64_t val_size,
    bool do_elem_retain, bool elem_is_str, bool do_key_retain,
    bool key_is_str, llvm::Value *destructor_callee) {
    (void)cg;
    return lowered::CowEnsureUniqueOp{data_ptr,       slot_ptr,
                                      kind,           atomic,
                                      elem_size,      key_size,
                                      val_size,       do_elem_retain,
                                      elem_is_str,    do_key_retain,
                                      key_is_str,     destructor_callee};
}

} // namespace ry::codegen::lowering
