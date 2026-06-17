#pragma once

#include <optional>
#include <string>

namespace llvm {
class Value;
}

namespace ry {
class CodeGen;
}

namespace ry::codegen::lowered {

enum class BoundsKind { List, Array };

// Structured error spec for a bounds-check op.
// A future PR will add a SourceLocation field once emitBoundsError consumes
// position metadata; see docs/architecture/codegen-layering-plan.md.
struct BoundsCheckErrorSpec {
    BoundsKind kind;
    std::string global_name;
};

struct BoundsCheckOp {
    llvm::Value *idx;
    llvm::Value *len;
    BoundsCheckErrorSpec error_spec;
};

} // namespace ry::codegen::lowered

namespace ry::codegen::lowering {

// Constant-fold case: both idx and len are ConstantInt — fold statically
// (apply negative-index wrap, reject OOB via codegenError, overwrite *idx with
// the folded constant) and return std::nullopt.
// Runtime case: return a BoundsCheckOp carrying the un-wrapped idx and len.
// Caller passes the BoundsCheckOp through ry_emit_bounds_check and assigns
// its return value back to idx for the subsequent GEP.
std::optional<lowered::BoundsCheckOp>
lowerBoundsCheck(CodeGen &cg, llvm::Value *&idx, llvm::Value *len,
                 lowered::BoundsKind kind, std::string global_name);

} // namespace ry::codegen::lowering
