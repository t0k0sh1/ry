#include "ry/codegen/lowered_arc.hpp"
#include "ry/codegen.hpp"
#include "ry/llvm_emit/api.h"
#include "ry/llvm_emit/cast_helpers.hpp"

#include <llvm/IR/Value.h>

namespace ry::codegen::emission {

void emitArcRetain(CodeGen &cg, const lowered::ArcRetainOp &op) {
    RyValueId headerId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(op.header_ptr));
    ry_emit_arc_retain(cg.emit_ctx_, headerId,
                       op.atomic ? RY_ARC_ATOMIC : RY_ARC_NONATOMIC);
}

void emitArcRelease(CodeGen &cg, const lowered::ArcReleaseOp &op) {
    cg.used_native_libraries_.insert("gc");
    RyValueId headerId = ry_emit_intern(cg.emit_ctx_, ry::llvm_emit::asRyValue(op.header_ptr));
    ry_emit_arc_release(cg.emit_ctx_, headerId,
                        op.atomic ? RY_ARC_ATOMIC : RY_ARC_NONATOMIC,
                        ry::llvm_emit::asRyValue(op.destructor_callee),
                        ry::llvm_emit::asRyValue(op.gc_visit_fn));
}

} // namespace ry::codegen::emission
