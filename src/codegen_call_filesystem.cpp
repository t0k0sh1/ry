#include "ry/codegen.hpp"

llvm::Value *CodeGen::emitBuiltinFilesystem(const CallExpr &e) {
    if (!native_fn_arg_counts_.count(e.callee))
        return nullptr;

    auto emitFS1 = [&](const std::string &name,
                        llvm::Type *retTy) -> llvm::Value * {
        requireArgs(e, 1);
        llvm::Value *arg = emitExpr(*e.args[0]);
        if (arg->getType() != ptrTy_)
            codegenError(e.callee + "() requires str argument");
        auto fnTy = llvm::FunctionType::get(retTy, {ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_filesystem_" + name, fnTy);
        return builder_.CreateCall(fn, {arg}, name);
    };

    auto emitFS2 = [&](const std::string &name,
                        llvm::Type *retTy) -> llvm::Value * {
        requireArgs(e, 2);
        llvm::Value *a = emitExpr(*e.args[0]);
        llvm::Value *b = emitExpr(*e.args[1]);
        if (a->getType() != ptrTy_ || b->getType() != ptrTy_)
            codegenError(e.callee + "() requires str arguments");
        auto fnTy = llvm::FunctionType::get(retTy, {ptrTy_, ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_filesystem_" + name, fnTy);
        return builder_.CreateCall(fn, {a, b}, name);
    };

    // ---- Pattern 1: (str) -> Result<List<str>, Error> ----
    if (e.callee == "list_dir" || e.callee == "walk" || e.callee == "glob_files") {
        llvm::Value *ptr = emitFS1(e.callee, ptrTy_);
        llvm::Value *result = wrapPtrAsResult(ptr, "__ry_filesystem_get_last_error");
        type_meta_[TM_ListElem][result] = ptrTy_;
        return result;
    }

    // ---- Pattern 2: (str, str) -> Result<Unit, Error> ----
    if (e.callee == "copy" || e.callee == "move" || e.callee == "symlink") {
        llvm::Value *status = emitFS2(e.callee, i64Ty_);
        return wrapStatusAsResult(status, "__ry_filesystem_get_last_error");
    }

    // ---- Pattern 3: (str) -> Result<Unit, Error> ----
    if (e.callee == "remove" || e.callee == "remove_all" ||
        e.callee == "make_dir" || e.callee == "make_dir_all") {
        llvm::Value *status = emitFS1(e.callee, i64Ty_);
        return wrapStatusAsResult(status, "__ry_filesystem_get_last_error");
    }

    // ---- Pattern 4: (str) -> bool ----
    if (e.callee == "is_file" || e.callee == "is_dir" || e.callee == "is_symlink") {
        llvm::Value *result = emitFS1(e.callee, i64Ty_);
        return builder_.CreateTrunc(result, i1Ty_, e.callee + "_bool");
    }

    // ---- Pattern 5: (str) -> Result<str, Error> ----
    if (e.callee == "read_link") {
        llvm::Value *ptr = emitFS1(e.callee, ptrTy_);
        return wrapPtrAsResult(ptr, "__ry_filesystem_get_last_error");
    }

    // ---- Pattern 6: (str) -> Result<int, Error> ----
    if (e.callee == "file_size") {
        requireArgs(e, 1);
        llvm::Value *arg = emitExpr(*e.args[0]);
        if (arg->getType() != ptrTy_)
            codegenError("file_size() requires str argument");
        llvm::AllocaInst *outSlot = builder_.CreateAlloca(i64Ty_, nullptr, "file_size_out");
        auto fnTy = llvm::FunctionType::get(i64Ty_, {ptrTy_, ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_filesystem_file_size", fnTy);
        llvm::Value *status = builder_.CreateCall(fn, {arg, outSlot}, "file_size_status");
        llvm::Value *isErr = builder_.CreateICmpNE(status,
            llvm::ConstantInt::get(i64Ty_, 0), "file_size_err");
        llvm::StructType *resTy = getResultType(i64Ty_, errorTy_);
        return emitResultBranch(isErr, resTy,
            [&]() {
                llvm::Value *loaded = builder_.CreateLoad(i64Ty_, outSlot, "file_size_val");
                return buildOkValue(loaded, resTy);
            },
            [&]() { return buildErrValue(
                buildErrorFromRuntime("__ry_filesystem_get_last_error"), resTy); });
    }

    // ---- Pattern 7: (str, int) -> Result<Unit, Error> ----
    if (e.callee == "chmod") {
        requireArgs(e, 2);
        llvm::Value *path = emitExpr(*e.args[0]);
        llvm::Value *mode = emitExpr(*e.args[1]);
        if (path->getType() != ptrTy_)
            codegenError("chmod() path must be str");
        if (mode->getType() != i64Ty_)
            codegenError("chmod() mode must be int");
        auto fnTy = llvm::FunctionType::get(i64Ty_, {ptrTy_, i64Ty_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_filesystem_chmod", fnTy);
        llvm::Value *status = builder_.CreateCall(fn, {path, mode}, "chmod_status");
        return wrapStatusAsResult(status, "__ry_filesystem_get_last_error");
    }

    return nullptr;
}
