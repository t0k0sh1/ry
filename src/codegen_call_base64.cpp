#include "ry/codegen.hpp"

static const CodeGen::NativeDispatchEntry base64_table[] = {
    {"encode",          nullptr, CodeGen::ReturnWrapping::Direct,    1, nullptr},
    {"encode_url_safe", nullptr, CodeGen::ReturnWrapping::Direct,    1, nullptr},
    {"decode",          nullptr, CodeGen::ReturnWrapping::ResultPtr, 1, nullptr},
    {"decode_url_safe", nullptr, CodeGen::ReturnWrapping::ResultPtr, 1, nullptr},
};

llvm::Value *CodeGen::emitBuiltinBase64(const CallExpr &e) {
    return emitTableDrivenNativeCall(e, "base64", base64_table,
                                     sizeof(base64_table) / sizeof(base64_table[0]));
}
