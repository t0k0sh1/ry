#include "ry/codegen.hpp"

llvm::Value *CodeGen::emitBuiltinPath(const CallExpr &e) {
    if (!native_fn_arg_counts_.count(e.callee))
        return nullptr;

    // Helper: call a __ry_path_* function with 1 str arg, returning str
    auto emitPathCall1 = [&](const std::string &rtName) -> llvm::Value * {
        requireArgs(e, 1);
        llvm::Value *arg = emitExpr(*e.args[0]);
        if (arg->getType() != ptrTy_)
            codegenError(e.callee + "() requires str argument");
        auto fn = mod_->getOrInsertFunction(rtName, fnTy_ptr_to_ptr_);
        return builder_.CreateCall(fn, {arg}, e.callee);
    };

    // join(str, str) -> str | join(str, str, str) -> str | join(str, str, str, str) -> str
    if (e.callee == "join") {
        size_t n = e.args.size();
        if (n < 2 || n > 4)
            codegenError("join() requires 2, 3, or 4 arguments");

        std::vector<llvm::Value *> args;
        for (size_t i = 0; i < n; i++) {
            llvm::Value *arg = emitExpr(*e.args[i]);
            if (arg->getType() != ptrTy_)
                codegenError("join() requires str arguments");
            args.push_back(arg);
        }

        std::string rtName;
        llvm::FunctionType *fnTy;
        if (n == 2) {
            rtName = "__ry_path_join2";
            fnTy = fnTy_ptr_ptr_to_ptr_;
        } else if (n == 3) {
            rtName = "__ry_path_join3";
            fnTy = fnTy_ptr_ptr_ptr_to_ptr_;
        } else {
            rtName = "__ry_path_join4";
            fnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, ptrTy_, ptrTy_}, false);
        }
        auto fn = mod_->getOrInsertFunction(rtName, fnTy);
        return builder_.CreateCall(fn, args, "join");
    }

    // basename(str) -> str
    if (e.callee == "basename")
        return emitPathCall1("__ry_path_basename");

    // dirname(str) -> str
    if (e.callee == "dirname")
        return emitPathCall1("__ry_path_dirname");

    // extension(str) -> str
    if (e.callee == "extension")
        return emitPathCall1("__ry_path_extension");

    // resolve(str) -> Result<str, Error>
    if (e.callee == "resolve") {
        llvm::Value *ptr = emitPathCall1("__ry_path_resolve");
        return wrapPtrAsResult(ptr, "__ry_path_get_last_error");
    }

    // is_absolute(str) -> bool
    if (e.callee == "is_absolute") {
        requireArgs(e, 1);
        llvm::Value *arg = emitExpr(*e.args[0]);
        if (arg->getType() != ptrTy_)
            codegenError("is_absolute() requires str argument");
        auto fn = mod_->getOrInsertFunction("__ry_path_is_absolute", fnTy_ptr_to_i64_);
        llvm::Value *result = builder_.CreateCall(fn, {arg}, "is_abs");
        return builder_.CreateTrunc(result, i1Ty_, "is_abs_bool");
    }

    return nullptr;
}
