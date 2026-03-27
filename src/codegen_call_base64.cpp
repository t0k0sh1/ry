#include "ry/codegen.hpp"

llvm::Value *CodeGen::emitBuiltinBase64(const CallExpr &e) {
    if (!native_fn_arg_counts_.count(e.callee))
        return nullptr;

    // Helper: call a __ry_base64_* function with 1 str arg, returning str
    auto emitBase64Call = [&](const std::string &rtName) -> llvm::Value * {
        requireArgs(e, 1);
        llvm::Value *input = emitExpr(*e.args[0]);
        if (input->getType() != ptrTy_)
            codegenError(e.callee + "() requires str argument");
        auto fnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction(rtName, fnTy);
        return builder_.CreateCall(fn, {input}, e.callee);
    };

    // encode(str) -> str
    if (e.callee == "encode")
        return emitBase64Call("__ry_base64_encode");

    // encode_url_safe(str) -> str
    if (e.callee == "encode_url_safe")
        return emitBase64Call("__ry_base64_encode_url_safe");

    // decode(str) -> Result<str, Error>
    if (e.callee == "decode") {
        llvm::Value *ptr = emitBase64Call("__ry_base64_decode");
        return wrapPtrAsResult(ptr, "__ry_base64_get_last_error");
    }

    // decode_url_safe(str) -> Result<str, Error>
    if (e.callee == "decode_url_safe") {
        llvm::Value *ptr = emitBase64Call("__ry_base64_decode_url_safe");
        return wrapPtrAsResult(ptr, "__ry_base64_get_last_error");
    }

    return nullptr;
}
