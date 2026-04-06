#include "ry/codegen.hpp"
#include "ry/stdlib_registry.hpp"

static const CodeGen::NativeDispatchEntry path_table[] = {
    {"join",        nullptr, CodeGen::ReturnWrapping::Direct,      -1, nullptr},
    {"basename",    nullptr, CodeGen::ReturnWrapping::Direct,        1, nullptr},
    {"dirname",     nullptr, CodeGen::ReturnWrapping::Direct,        1, nullptr},
    {"extension",   nullptr, CodeGen::ReturnWrapping::Direct,        1, nullptr},
    {"resolve",     nullptr, CodeGen::ReturnWrapping::ResultPtr,     1, nullptr},
    {"is_absolute", nullptr, CodeGen::ReturnWrapping::BoolFromI64,   1, nullptr},
};

RY_REGISTER_STDLIB_PACKAGE(path, "share/std/path/path.ry", dispatchPath)
static llvm::Value *dispatchPath(CodeGen &cg, const CallExpr &e) {
    return cg.emitTableDrivenNativeCall(e, "path", path_table, std::size(path_table));
}
