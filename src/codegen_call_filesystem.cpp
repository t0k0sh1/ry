#include "ry/codegen.hpp"

static const CodeGen::NativeDispatchEntry filesystem_table[] = {
    // (str) -> Result<List<str>, Error>
    {"list_dir",    nullptr, CodeGen::ReturnWrapping::ResultPtrWithListMeta, 1, nullptr},
    {"walk",        nullptr, CodeGen::ReturnWrapping::ResultPtrWithListMeta, 1, nullptr},
    {"glob_files",  nullptr, CodeGen::ReturnWrapping::ResultPtrWithListMeta, 1, nullptr},
    // (str, str) -> Result<Unit, Error>
    {"copy",        nullptr, CodeGen::ReturnWrapping::ResultStatus,  2, nullptr},
    {"move",        nullptr, CodeGen::ReturnWrapping::ResultStatus,  2, nullptr},
    {"symlink",     nullptr, CodeGen::ReturnWrapping::ResultStatus,  2, nullptr},
    // (str) -> Result<Unit, Error>
    {"remove",      nullptr, CodeGen::ReturnWrapping::ResultStatus,  1, nullptr},
    {"remove_all",  nullptr, CodeGen::ReturnWrapping::ResultStatus,  1, nullptr},
    {"make_dir",    nullptr, CodeGen::ReturnWrapping::ResultStatus,  1, nullptr},
    {"make_dir_all",nullptr, CodeGen::ReturnWrapping::ResultStatus,  1, nullptr},
    // (str) -> bool
    {"is_file",     nullptr, CodeGen::ReturnWrapping::BoolFromI64,   1, nullptr},
    {"is_dir",      nullptr, CodeGen::ReturnWrapping::BoolFromI64,   1, nullptr},
    {"is_symlink",  nullptr, CodeGen::ReturnWrapping::BoolFromI64,   1, nullptr},
    // (str) -> Result<str, Error>
    {"read_link",   nullptr, CodeGen::ReturnWrapping::ResultPtr,     1, nullptr},
    // (str) -> Result<int, Error>
    {"file_size",   nullptr, CodeGen::ReturnWrapping::ResultOutParam,1, "int"},
    // (str, int) -> Result<Unit, Error>
    {"chmod",       nullptr, CodeGen::ReturnWrapping::ResultStatus,  2, nullptr},
};

llvm::Value *CodeGen::emitBuiltinFilesystem(const CallExpr &e) {
    return emitTableDrivenNativeCall(e, "filesystem", filesystem_table,
                                     sizeof(filesystem_table) / sizeof(filesystem_table[0]));
}
