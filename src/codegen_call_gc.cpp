#include "ry/codegen.hpp"

static const CodeGen::NativeDispatchEntry gc_table[] = {
    {"collect",       nullptr, CodeGen::ReturnWrapping::Direct, 0, nullptr},
    {"enable",        nullptr, CodeGen::ReturnWrapping::Direct, 0, nullptr},
    {"disable",       nullptr, CodeGen::ReturnWrapping::Direct, 0, nullptr},
    {"set_threshold", nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr},
};

llvm::Value *CodeGen::emitBuiltinGc(const CallExpr &e) {
    return emitTableDrivenNativeCall(e, "gc", gc_table, std::size(gc_table));
}
