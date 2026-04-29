#include "ry/codegen.hpp"
#include "ry/stdlib_registry.hpp"


namespace ry {

static constexpr const char *BASE64_ERR = "__ry_base64_get_last_error";

static const CodeGen::NativeDispatchEntry base64_table[] = {
    // str → str / Result<str, Error>
    {"encode",              nullptr, CodeGen::ReturnWrapping::Direct,    1, nullptr,
     nullptr, "__ry_base64_encode",               nullptr,    CodeGen::ListElemMeta::None, -1},
    {"decode",              nullptr, CodeGen::ReturnWrapping::ResultPtr, 1, nullptr,
     nullptr, "__ry_base64_decode",               BASE64_ERR, CodeGen::ListElemMeta::None, -1},
    {"encodeUrlSafe",       nullptr, CodeGen::ReturnWrapping::Direct,    1, nullptr,
     nullptr, "__ry_base64_encode_url_safe",      nullptr,    CodeGen::ListElemMeta::None, -1},
    {"decodeUrlSafe",       nullptr, CodeGen::ReturnWrapping::ResultPtr, 1, nullptr,
     nullptr, "__ry_base64_decode_url_safe",      BASE64_ERR, CodeGen::ListElemMeta::None, -1},

    // List<u8> → str
    {"encodeBytes",         nullptr, CodeGen::ReturnWrapping::Direct,    1, nullptr,
     nullptr, "__ry_base64_encode_bytes",          nullptr,    CodeGen::ListElemMeta::None, 0},
    {"encodeBytesUrlSafe",  nullptr, CodeGen::ReturnWrapping::Direct,    1, nullptr,
     nullptr, "__ry_base64_encode_bytes_url_safe", nullptr,    CodeGen::ListElemMeta::None, 0},

    // str → Result<List<u8>, Error>
    {"decodeBytes",         nullptr, CodeGen::ReturnWrapping::ResultPtr, 1, nullptr,
     nullptr, "__ry_base64_decode_bytes",          BASE64_ERR, CodeGen::ListElemMeta::I8,  -1},
    {"decodeBytesUrlSafe",  nullptr, CodeGen::ReturnWrapping::ResultPtr, 1, nullptr,
     nullptr, "__ry_base64_decode_bytes_url_safe", BASE64_ERR, CodeGen::ListElemMeta::I8,  -1},
};

RY_REGISTER_STDLIB_PACKAGE(base64, "share/std/base64/base64.ry", dispatchBase64)
static llvm::Value *dispatchBase64(CodeGen &cg, const CallExpr &e) {
    return cg.emitTableDrivenNativeCall(e, "base64", base64_table, std::size(base64_table));
}

} // namespace ry
