#include "ry/codegen.hpp"

static const CodeGen::NativeDispatchEntry path_table[] = {
    {"join",        nullptr, CodeGen::ReturnWrapping::Direct,      -1, nullptr},
    {"basename",    nullptr, CodeGen::ReturnWrapping::Direct,        1, nullptr},
    {"dirname",     nullptr, CodeGen::ReturnWrapping::Direct,        1, nullptr},
    {"extension",   nullptr, CodeGen::ReturnWrapping::Direct,        1, nullptr},
    {"resolve",     nullptr, CodeGen::ReturnWrapping::ResultPtr,     1, nullptr},
    {"is_absolute", nullptr, CodeGen::ReturnWrapping::BoolFromI64,   1, nullptr},
};

llvm::Value *CodeGen::emitBuiltinPath(const CallExpr &e) {
    return emitTableDrivenNativeCall(e, "path", path_table, std::size(path_table));
}
