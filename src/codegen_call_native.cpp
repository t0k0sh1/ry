#include "ry/codegen.hpp"

llvm::Value *CodeGen::emitTableDrivenNativeCall(
    const CallExpr &e,
    const char *package,
    const NativeDispatchEntry *table,
    size_t table_size) {

    if (!native_fn_arg_counts_.count(e.callee))
        return nullptr;

    // Lookup entry in dispatch table
    const NativeDispatchEntry *entry = nullptr;
    for (size_t i = 0; i < table_size; i++) {
        if (e.callee == table[i].fn_name) {
            entry = &table[i];
            break;
        }
    }
    if (!entry)
        return nullptr;

    // Lazily build error function name (only for wrappings that need it)
    auto getErrFnName = [&]() {
        return deriveRuntimeFnName(package, "get_last_error");
    };

    // Resolve the effective runtime suffix
    const char *rtSuffix = entry->rt_suffix ? entry->rt_suffix : entry->fn_name;

    // --- Special case: variadic arity (e.g. path::join) ---
    if (entry->arity == -1) {
        size_t n = e.args.size();
        if (n < 2 || n > 4)
            codegenError(std::string(entry->fn_name) + "() requires 2, 3, or 4 arguments");

        // Match signature by arity (same validation as fixed-arity path)
        std::string sigKey = nativeSigKey(package, e.callee);
        auto sigIt = native_fn_sigs_.find(sigKey);
        const NativeFnSignature *matchedSig = nullptr;
        if (sigIt != native_fn_sigs_.end()) {
            for (const auto &sig : sigIt->second) {
                if (sig.params.size() == n) {
                    matchedSig = &sig;
                    break;
                }
            }
        }
        if (!matchedSig)
            codegenError(std::string(package) + "::" + e.callee +
                         " has no @native signature with arity " +
                         std::to_string(n));

        std::vector<llvm::Value *> args;
        std::vector<llvm::Type *> argTypes;
        for (size_t i = 0; i < n; i++) {
            llvm::Value *arg = emitExpr(*e.args[i]);
            llvm::Type *expectedTy = resolveType(matchedSig->params[i].type_name);
            if (arg->getType() != expectedTy)
                codegenError(e.callee + "() argument " + std::to_string(i) +
                             " requires " + matchedSig->params[i].type_name);
            args.push_back(arg);
            argTypes.push_back(expectedTy);
        }

        std::string rtName = deriveRuntimeFnName(package, rtSuffix)
            + std::to_string(n);
        auto *fnTy = llvm::FunctionType::get(
            resolveType(matchedSig->return_type_name), argTypes, false);
        auto fn = mod_->getOrInsertFunction(rtName, fnTy);
        return builder_.CreateCall(fn, args, entry->fn_name);
    }

    // --- Normal path ---
    requireArgs(e, static_cast<size_t>(entry->arity));

    // Look up NativeFnSignature matching this call's arity
    std::string sigKey = nativeSigKey(package, e.callee);
    auto sigIt = native_fn_sigs_.find(sigKey);
    const NativeFnSignature *matchedSig = nullptr;
    if (sigIt != native_fn_sigs_.end()) {
        for (const auto &sig : sigIt->second) {
            if (static_cast<int>(sig.params.size()) == entry->arity) {
                matchedSig = &sig;
                break;
            }
        }
    }
    if (!matchedSig)
        codegenError(std::string(package) + "::" + e.callee +
                     " has no @native signature with arity " +
                     std::to_string(entry->arity));

    // Emit and validate arguments
    std::vector<llvm::Value *> args;
    std::vector<llvm::Type *> paramLLVMTypes;
    for (int i = 0; i < entry->arity; i++) {
        llvm::Value *arg = emitExpr(*e.args[i]);
        const std::string &expectedTN = matchedSig->params[i].type_name;
        llvm::Type *expectedTy = resolveType(expectedTN);
        if (arg->getType() != expectedTy)
            codegenError(e.callee + "() argument " + std::to_string(i) +
                         " requires " + expectedTN);
        args.push_back(arg);
        paramLLVMTypes.push_back(expectedTy);
    }

    // Derive runtime function name
    std::string rtName = deriveRuntimeFnName(package, rtSuffix);

    // Pre-compute out-param type for ResultOutParam (used in two places)
    llvm::Type *outTy = nullptr;
    if (entry->wrapping == ReturnWrapping::ResultOutParam)
        outTy = entry->out_param_type ? resolveType(entry->out_param_type) : i64Ty_;

    // Determine C-level return type and build function type
    llvm::Type *cRetTy;
    switch (entry->wrapping) {
    case ReturnWrapping::Direct: {
        cRetTy = resolveType(matchedSig->return_type_name);
        break;
    }
    case ReturnWrapping::ResultPtr:
    case ReturnWrapping::ResultPtrWithListMeta:
        cRetTy = ptrTy_;
        break;
    case ReturnWrapping::ResultStatus:
    case ReturnWrapping::BoolFromI64:
        cRetTy = i64Ty_;
        break;
    case ReturnWrapping::ResultOutParam:
        paramLLVMTypes.push_back(ptrTy_);
        cRetTy = i64Ty_;
        break;
    }

    // For ResultOutParam, create alloca and add to args
    llvm::AllocaInst *outSlot = nullptr;
    if (entry->wrapping == ReturnWrapping::ResultOutParam) {
        outSlot = builder_.CreateAlloca(outTy, nullptr,
            std::string(entry->fn_name) + "_out");
        args.push_back(outSlot);
    }

    auto *fnTy = llvm::FunctionType::get(cRetTy, paramLLVMTypes, false);
    auto fn = mod_->getOrInsertFunction(rtName, fnTy);
    llvm::Value *callResult;
    if (cRetTy->isVoidTy())
        callResult = builder_.CreateCall(fn, args);
    else
        callResult = builder_.CreateCall(fn, args, entry->fn_name);

    // Apply return wrapping
    switch (entry->wrapping) {
    case ReturnWrapping::Direct:
        if (cRetTy->isVoidTy())
            return llvm::ConstantInt::get(i8Ty_, 0); // Unit
        return callResult;

    case ReturnWrapping::ResultPtr: {
        std::string errFn = getErrFnName();
        return wrapPtrAsResult(callResult, errFn.c_str());
    }

    case ReturnWrapping::ResultPtrWithListMeta: {
        std::string errFn = getErrFnName();
        llvm::Value *result = wrapPtrAsResult(callResult, errFn.c_str());
        type_meta_[TM_ListElem][result] = ptrTy_;
        return result;
    }

    case ReturnWrapping::ResultStatus: {
        std::string errFn = getErrFnName();
        return wrapStatusAsResult(callResult, errFn.c_str());
    }

    case ReturnWrapping::BoolFromI64:
        return builder_.CreateTrunc(callResult, i1Ty_,
            std::string(entry->fn_name) + "_bool");

    case ReturnWrapping::ResultOutParam: {
        std::string errFn = getErrFnName();
        llvm::Value *isErr = builder_.CreateICmpNE(callResult,
            llvm::ConstantInt::get(i64Ty_, 0),
            std::string(entry->fn_name) + "_err");
        llvm::StructType *resTy = getResultType(outTy, errorTy_);
        return emitResultBranch(isErr, resTy,
            [&]() {
                llvm::Value *loaded = builder_.CreateLoad(outTy, outSlot,
                    std::string(entry->fn_name) + "_val");
                return buildOkValue(loaded, resTy);
            },
            [&]() {
                return buildErrValue(
                    buildErrorFromRuntime(errFn.c_str()), resTy);
            });
    }
    }

    llvm_unreachable("unhandled ReturnWrapping variant");
}
