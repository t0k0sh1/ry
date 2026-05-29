#include "ry/codegen/lowered_bounds_check.hpp"
#include "ry/codegen.hpp"

#include <llvm/IR/Constants.h>

#include <string>
#include <utility>

namespace ry::codegen::lowering {

std::optional<lowered::BoundsCheckOp>
lowerBoundsCheck(CodeGen &cg, llvm::Value *&idx, llvm::Value *len,
                 lowered::BoundsKind kind, std::string global_name) {
    if (auto *ci = llvm::dyn_cast<llvm::ConstantInt>(idx)) {
        if (auto *cs = llvm::dyn_cast<llvm::ConstantInt>(len)) {
            // i1 は emission 側で ZExt → i64 されるので fold も zext 相当に揃える
            int64_t original = (ci->getBitWidth() == 1)
                                   ? static_cast<int64_t>(ci->getZExtValue())
                                   : ci->getSExtValue();
            int64_t sz = static_cast<int64_t>(cs->getZExtValue());
            int64_t i = original;
            if (i < 0) i += sz;
            if (i < 0 || i >= sz)
                cg.codegenError("index " + std::to_string(original) +
                                " out of bounds (size " + std::to_string(sz) + ")");
            idx = llvm::ConstantInt::get(cg.i64Ty_, static_cast<uint64_t>(i));
            return std::nullopt;
        }
    }

    return lowered::BoundsCheckOp{idx, len,
                                  {kind, std::move(global_name)}};
}

} // namespace ry::codegen::lowering
