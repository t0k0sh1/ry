#include "ry/codegen/lowered_arc.hpp"
#include "ry/codegen.hpp"
#include "ry/llvm_emit/api.h"

#include <llvm/IR/Function.h>
#include <llvm/IR/Value.h>

namespace ry::codegen::emission {

void emitArcRetain(CodeGen &cg, const lowered::ArcRetainOp &op) {
    ry_emit_ctx_set_function(cg.emit_ctx_, cg.fn_);
    RyValueId headerId = ry_emit_intern(cg.emit_ctx_, op.header_ptr);
    ry_emit_arc_retain(cg.emit_ctx_, headerId,
                       op.atomic ? RY_ARC_ATOMIC : RY_ARC_NONATOMIC);
}

void emitArcRelease(CodeGen &cg, const lowered::ArcReleaseOp &op) {
    ry_emit_ctx_set_function(cg.emit_ctx_, cg.fn_);
    cg.used_native_libraries_.insert("gc");
    RyValueId headerId = ry_emit_intern(cg.emit_ctx_, op.header_ptr);
    ry_emit_arc_release(cg.emit_ctx_, headerId,
                        op.atomic ? RY_ARC_ATOMIC : RY_ARC_NONATOMIC,
                        static_cast<void *>(op.destructor_callee),
                        static_cast<void *>(op.gc_visit_fn));
}

} // namespace ry::codegen::emission
