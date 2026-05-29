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
            int64_t i = ci->getSExtValue();
            int64_t sz = static_cast<int64_t>(cs->getZExtValue());
            if (i < 0) i += sz;
            if (i < 0 || i >= sz)
                cg.codegenError("index " + std::to_string(ci->getSExtValue()) +
                                " out of bounds (size " + std::to_string(sz) + ")");
            idx = llvm::ConstantInt::get(cg.i64Ty_, static_cast<uint64_t>(i));
            return std::nullopt;
        }
    }

    return lowered::BoundsCheckOp{idx, len,
                                  {kind, std::move(global_name)}};
}

} // namespace ry::codegen::lowering
