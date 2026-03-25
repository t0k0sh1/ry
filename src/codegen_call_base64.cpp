#include "ry/codegen.hpp"

llvm::Value *CodeGen::emitBuiltinBase64(const CallExpr &e) {
    if (!native_fn_arg_counts_.count(e.callee))
        return nullptr;

    // Helper: call a __ry_base64_* function with 1 str arg, returning str
    auto emitBase64Call = [&](const std::string &rtName) -> llvm::Value * {
        if (e.args.size() != 1)
            codegenError(e.callee + "() takes exactly 1 argument");
        llvm::Value *input = emitExpr(*e.args[0]);
        if (input->getType() != ptrTy_)
            codegenError(e.callee + "() requires str argument");
        auto fnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction(rtName, fnTy);
        return builder_.CreateCall(fn, {input}, e.callee);
    };

    // Helper: wrap nullable ptr into Result<str, Error> using base64-specific error
    auto wrapBase64Result = [&](llvm::Value *ptr) -> llvm::Value * {
        llvm::StructType *resTy = getResultType(ptrTy_, errorTy_);
        llvm::Value *isNull = builder_.CreateICmpEQ(
            ptr,
            llvm::ConstantPointerNull::get(
                llvm::cast<llvm::PointerType>(ptrTy_)),
            "is_null");

        llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "b64.ok", fn_);
        llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, "b64.err", fn_);
        llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "b64.merge", fn_);
        builder_.CreateCondBr(isNull, errBB, okBB);

        builder_.SetInsertPoint(okBB);
        llvm::Value *okVal = buildOkValue(ptr, resTy);
        builder_.CreateBr(mergeBB);
        okBB = builder_.GetInsertBlock();

        builder_.SetInsertPoint(errBB);
        // Get error message from base64-specific error buffer
        auto errFnTy = llvm::FunctionType::get(ptrTy_, {}, false);
        auto errFn = mod_->getOrInsertFunction("__ry_base64_get_last_error", errFnTy);
        llvm::Value *errMsg = builder_.CreateCall(errFn, {}, "b64_err_msg");
        llvm::Value *errStruct = llvm::UndefValue::get(errorTy_);
        errStruct = builder_.CreateInsertValue(errStruct, errMsg, 0, "err.msg");
        errStruct = builder_.CreateInsertValue(errStruct, llvm::ConstantInt::get(i64Ty_, 0), 1, "err.code");
        llvm::Value *errVal = buildErrValue(errStruct, resTy);
        builder_.CreateBr(mergeBB);
        errBB = builder_.GetInsertBlock();

        builder_.SetInsertPoint(mergeBB);
        llvm::PHINode *phi = builder_.CreatePHI(resTy, 2, "b64_result");
        phi->addIncoming(okVal, okBB);
        phi->addIncoming(errVal, errBB);
        return phi;
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
        return wrapBase64Result(ptr);
    }

    // decode_url_safe(str) -> Result<str, Error>
    if (e.callee == "decode_url_safe") {
        llvm::Value *ptr = emitBase64Call("__ry_base64_decode_url_safe");
        return wrapBase64Result(ptr);
    }

    return nullptr;
}
