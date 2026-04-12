#include "ry/codegen.hpp"


namespace ry {

llvm::Value *CodeGen::emitTableDrivenNativeCall(
    const CallExpr &e,
    const char *package,
    const NativeDispatchEntry *table,
    size_t table_size) {

    // Guard: check if this callee has any registered @native signature
    // for this package (or as a bare name for inline declarations).
    // The sigKey is reused later for signature lookup (variadic + normal paths).
    std::string sigKey = nativeSigKey(package, e.callee);
    if (!native_fn_sigs_.count(sigKey) && !native_fn_sigs_.count(e.callee))
        return nullptr;

    // Lookup entry in dispatch table
    const NativeDispatchEntry *entry = nullptr;
    for (size_t i = 0; i < table_size; i++) {
        if (e.callee == table[i].fnName) {
            entry = &table[i];
            break;
        }
    }
    if (!entry)
        return nullptr;

    // Lazily build error function name (only for wrappings that need it)
    auto getErrFnName = [&]() -> std::string {
        if (entry->errFnOverride)
            return entry->errFnOverride;
        return deriveRuntimeFnName(package, "get_last_error");
    };

    // Resolve the effective runtime suffix
    const char *rtSuffix = entry->rtSuffix ? entry->rtSuffix : entry->fnName;

    // --- Special case: variadic arity (e.g. path::join) ---
    if (entry->arity == -1) {
        size_t n = e.args.size();

        // Check sig key BEFORE arity validation so that a name collision
        // with a different-library function falls through instead of erroring.
        auto sigIt = native_fn_sigs_.find(sigKey);
        if (sigIt == native_fn_sigs_.end()) {
            // Fallback: try bare name for inline @native declarations without package
            sigIt = native_fn_sigs_.find(e.callee);
            if (sigIt == native_fn_sigs_.end())
                return nullptr;  // No sig for this package — fall through
        }

        bool hasArityMatch = false;
        for (const auto &sig : sigIt->second) {
            if (sig.params.size() == n) { hasArityMatch = true; break; }
        }
        if (!hasArityMatch)
            return nullptr;  // Arity mismatch — fall through

        if (n < 2 || n > 4)
            codegenError(std::string(entry->fnName) + "() requires 2, 3, or 4 arguments");

        // Emit args once, then find the sig whose types match
        std::vector<llvm::Value *> args;
        for (size_t i = 0; i < n; i++)
            args.push_back(emitExpr(*e.args[i]));

        const NativeFnSignature *matchedSig = nullptr;
        std::vector<llvm::Type *> argTypes;
        for (const auto &sig : sigIt->second) {
            if (sig.params.size() != n) continue;
            bool typesMatch = true;
            std::vector<llvm::Type *> candidateTypes;
            for (size_t i = 0; i < n; i++) {
                llvm::Type *expectedTy = resolveType(sig.params[i].typeName);
                if (args[i]->getType() != expectedTy) {
                    typesMatch = false;
                    break;
                }
                candidateTypes.push_back(expectedTy);
            }
            if (typesMatch) {
                matchedSig = &sig;
                argTypes = std::move(candidateTypes);
                break;
            }
        }
        if (!matchedSig) {
            for (const auto &sig : sigIt->second) {
                if (sig.params.size() == n) {
                    for (size_t i = 0; i < n; i++) {
                        llvm::Type *expectedTy = resolveType(sig.params[i].typeName);
                        if (args[i]->getType() != expectedTy)
                            codegenError(e.callee + "() argument " + std::to_string(i) +
                                         " requires " + sig.params[i].typeName);
                    }
                    break;
                }
            }
            codegenError(e.callee + "() argument type mismatch");
        }

        if (!matchedSig->library.empty())
            used_native_libraries_.insert(matchedSig->library);

        std::string rtName = (entry->rtNameOverride
            ? std::string(entry->rtNameOverride)
            : deriveRuntimeFnName(package, rtSuffix))
            + std::to_string(n);
        auto *fnTy = llvm::FunctionType::get(
            resolveType(matchedSig->returnTypeName), argTypes, false);
        auto fn = mod_->getOrInsertFunction(rtName, fnTy);
        return builder_.CreateCall(fn, args, entry->fnName);
    }

    // --- Normal path ---

    // Look up NativeFnSignature matching this call's arity.
    // Check sig key BEFORE requireArgs so that a name collision with a
    // different-library function falls through instead of erroring.
    auto sigIt = native_fn_sigs_.find(sigKey);
    if (sigIt == native_fn_sigs_.end()) {
        // Fallback: try bare name for inline @native declarations without package
        sigIt = native_fn_sigs_.find(e.callee);
        if (sigIt == native_fn_sigs_.end())
            return nullptr;  // No sig for this package — fall through
    }

    // Custom emitter escape hatch: runs only after sig key AND call-arity
    // validation.  The arity check uses the CALL's actual arg count against
    // registered signatures so that same-package overloads with different
    // arities are not hijacked (e.g. a 2-arg overload won't be grabbed by
    // a 1-arg custom emitter entry).
    if (entry->customEmitter) {
        bool hasCallArityMatch = false;
        for (const auto &sig : sigIt->second) {
            if (sig.params.size() == e.args.size()) {
                // Track native library for the JIT — custom emitters return
                // before the normal matchedSig tracking below.
                if (!sig.library.empty())
                    used_native_libraries_.insert(sig.library);
                hasCallArityMatch = true;
                break;
            }
        }
        if (!hasCallArityMatch)
            return nullptr;  // No sig with this arity — fall through
        return entry->customEmitter(*this, e);
    }

    // Check if any sig matches the arity before emitting args
    bool hasArityMatch = false;
    for (const auto &sig : sigIt->second) {
        if (static_cast<int>(sig.params.size()) == entry->arity) {
            hasArityMatch = true;
            break;
        }
    }
    if (!hasArityMatch)
        return nullptr;  // Arity mismatch — fall through

    requireArgs(e, static_cast<size_t>(entry->arity));

    // Emit args once, then find the sig whose types match
    std::vector<llvm::Value *> args;
    for (size_t i = 0; i < static_cast<size_t>(entry->arity); i++)
        args.push_back(emitExpr(*e.args[i]));

    const NativeFnSignature *matchedSig = nullptr;
    std::vector<llvm::Type *> paramLLVMTypes;
    for (const auto &sig : sigIt->second) {
        if (static_cast<int>(sig.params.size()) != entry->arity) continue;
        bool typesMatch = true;
        std::vector<llvm::Type *> candidateTypes;
        for (size_t i = 0; i < static_cast<size_t>(entry->arity); i++) {
            llvm::Type *expectedTy = resolveType(sig.params[i].typeName);
            if (args[i]->getType() != expectedTy) {
                typesMatch = false;
                break;
            }
            candidateTypes.push_back(expectedTy);
        }
        if (typesMatch) {
            matchedSig = &sig;
            paramLLVMTypes = std::move(candidateTypes);
            break;
        }
    }
    if (!matchedSig) {
        for (const auto &sig : sigIt->second) {
            if (static_cast<int>(sig.params.size()) == entry->arity) {
                for (size_t i = 0; i < static_cast<size_t>(entry->arity); i++) {
                    llvm::Type *expectedTy = resolveType(sig.params[i].typeName);
                    if (args[i]->getType() != expectedTy)
                        codegenError(e.callee + "() argument " + std::to_string(i) +
                                     " requires " + sig.params[i].typeName);
                }
                break;
            }
        }
        codegenError(e.callee + "() argument type mismatch");
    }

    if (!matchedSig->library.empty())
        used_native_libraries_.insert(matchedSig->library);

    // Derive runtime function name
    std::string rtName = entry->rtNameOverride
        ? entry->rtNameOverride
        : deriveRuntimeFnName(package, rtSuffix);

    // Pre-compute out-param type for ResultOutParam (used in two places)
    llvm::Type *outTy = nullptr;
    if (entry->wrapping == ReturnWrapping::ResultOutParam)
        outTy = entry->outParamType ? resolveType(entry->outParamType) : i64Ty_;

    // Determine C-level return type and build function type
    llvm::Type *cRetTy;
    switch (entry->wrapping) {
    case ReturnWrapping::Direct: {
        cRetTy = resolveType(matchedSig->returnTypeName);
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
            std::string(entry->fnName) + "_out");
        args.push_back(outSlot);
    }

    auto *fnTy = llvm::FunctionType::get(cRetTy, paramLLVMTypes, false);
    auto fn = mod_->getOrInsertFunction(rtName, fnTy);
    llvm::Value *callResult;
    if (cRetTy->isVoidTy())
        callResult = builder_.CreateCall(fn, args);
    else
        callResult = builder_.CreateCall(fn, args, entry->fnName);

    // Apply return wrapping
    switch (entry->wrapping) {
    case ReturnWrapping::Direct:
        if (cRetTy->isVoidTy())
            return llvm::ConstantInt::get(i8Ty_, 0); // Unit
        if (entry->listElemMeta == ListElemMeta::I8)
            setTypeMeta(TypeMeta::ListElem, callResult, i8Ty_);
        else if (entry->listElemMeta == ListElemMeta::Ptr)
            setTypeMeta(TypeMeta::ListElem, callResult, ptrTy_);
        return callResult;

    case ReturnWrapping::ResultPtr: {
        std::string errFn = getErrFnName();
        llvm::Value *result = wrapPtrAsResult(callResult, errFn.c_str());
        if (entry->listElemMeta == ListElemMeta::I8)
            setTypeMeta(TypeMeta::ListElem, result, i8Ty_);
        else if (entry->listElemMeta == ListElemMeta::Ptr)
            setTypeMeta(TypeMeta::ListElem, result, ptrTy_);
        return result;
    }

    case ReturnWrapping::ResultPtrWithListMeta: {
        std::string errFn = getErrFnName();
        llvm::Value *result = wrapPtrAsResult(callResult, errFn.c_str());
        setTypeMeta(TypeMeta::ListElem, result, ptrTy_);
        return result;
    }

    case ReturnWrapping::ResultStatus: {
        std::string errFn = getErrFnName();
        return wrapStatusAsResult(callResult, errFn.c_str());
    }

    case ReturnWrapping::BoolFromI64:
        return builder_.CreateTrunc(callResult, i1Ty_,
            std::string(entry->fnName) + "_bool");

    case ReturnWrapping::ResultOutParam: {
        std::string errFn = getErrFnName();
        llvm::Value *isErr = builder_.CreateICmpNE(callResult,
            llvm::ConstantInt::get(i64Ty_, 0),
            std::string(entry->fnName) + "_err");
        llvm::StructType *resTy = getResultType(outTy, errorTy_);
        return emitResultBranch(isErr, resTy,
            [&]() {
                llvm::Value *loaded = builder_.CreateLoad(outTy, outSlot,
                    std::string(entry->fnName) + "_val");
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

// ===== Generic dispatch for @native("libname") functions =====
//
// This handles calls to functions declared with @native("libname") that are
// NOT covered by the hardcoded stdlib dispatch tables. It uses the signature
// registry to derive the C calling convention from the Ry type annotations.

namespace {

// Infer the ReturnWrapping from the Ry return type name.
// Returns {wrapping, out_param_type_name} where out_param_type_name is non-empty
// only for ResultOutParam.
std::pair<CodeGen::ReturnWrapping, std::string>
inferReturnWrapping(const std::string &returnType) {
    using RW = CodeGen::ReturnWrapping;

    // Result<T, Error> patterns
    if (returnType.size() > 7 && returnType.substr(0, 7) == "Result<") {
        // Extract the Ok type from Result<OkType, Error>, handling nested
        // generics like Result<Map<K, V>, Error> by counting bracket depth.
        int depth = 0;
        size_t commaPos = std::string::npos;
        for (size_t i = 7; i < returnType.size(); ++i) {
            if (returnType[i] == '<') ++depth;
            else if (returnType[i] == '>') --depth;
            else if (returnType[i] == ',' && depth == 0) { commaPos = i; break; }
        }
        if (commaPos == std::string::npos) return {RW::Direct, ""};
        std::string okType = returnType.substr(7, commaPos - 7);
        while (!okType.empty() && okType.back() == ' ') okType.pop_back();

        if (okType == "Unit")  return {RW::ResultStatus, ""};
        if (okType == "int")   return {RW::ResultOutParam, "int"};
        if (okType == "float") return {RW::ResultOutParam, "float"};
        if (okType == "bool")  return {RW::ResultOutParam, "int"};
        // str, List<...>, Map<...>, or any pointer type → ResultPtr
        return {RW::ResultPtr, ""};
    }

    if (returnType == "bool") return {RW::BoolFromI64, ""};

    // str, int, float, Unit, or any other type → Direct
    return {RW::Direct, ""};
}

} // namespace

llvm::Value *CodeGen::emitGenericNativeCall(const CallExpr &e) {
    // O(1) lookup via secondary index: fn_name → candidate library names
    auto libIt = native_lib_index_.find(e.callee);
    if (libIt == native_lib_index_.end())
        return nullptr;

    // Emit args once, then try each candidate library for a type match.
    // This avoids committing to the first arity match when a different
    // library's signature may match the actual argument types.
    std::vector<llvm::Value *> args;
    for (size_t i = 0; i < e.args.size(); i++)
        args.push_back(emitExpr(*e.args[i]));

    const NativeFnSignature *matchedSig = nullptr;
    std::string matchedPackage;
    std::vector<llvm::Type *> paramTypes;
    for (const auto &lib : libIt->second) {
        std::string sigKey = nativeSigKey(lib, e.callee);
        auto sigIt = native_fn_sigs_.find(sigKey);
        if (sigIt == native_fn_sigs_.end()) continue;
        for (const auto &sig : sigIt->second) {
            if (sig.params.size() != e.args.size()) continue;
            bool typesMatch = true;
            std::vector<llvm::Type *> candidateTypes;
            for (size_t i = 0; i < args.size(); i++) {
                llvm::Type *expectedTy = resolveType(sig.params[i].typeName);
                if (args[i]->getType() != expectedTy) {
                    typesMatch = false;
                    break;
                }
                candidateTypes.push_back(expectedTy);
            }
            if (typesMatch) {
                if (matchedSig)
                    codegenError("ambiguous @native call: '" + e.callee +
                                 "' matches both @native(\"" + matchedPackage +
                                 "\") and @native(\"" + lib + "\")");
                matchedSig = &sig;
                matchedPackage = lib;
                paramTypes = std::move(candidateTypes);
            }
        }
    }

    // No library matched both arity and types — fall through to user functions
    if (!matchedSig) return nullptr;

    used_native_libraries_.insert(matchedPackage);

    // Adjust bool params to match C ABI: native functions pass bools as i64.
    // Widen i1→i64 in both the prototype and the arg values.
    for (size_t i = 0; i < paramTypes.size(); i++) {
        if (paramTypes[i] == i1Ty_) {
            args[i] = builder_.CreateZExt(args[i], i64Ty_, "bool_zext");
            paramTypes[i] = i64Ty_;
        }
    }

    // Derive runtime function name: __ry_<package>_<fn_name>
    std::string rtName = deriveRuntimeFnName(matchedPackage, e.callee);

    // Determine C-level calling convention from the Ry return type
    auto [wrapping, outParamType] = inferReturnWrapping(matchedSig->returnTypeName);

    // Error function name for Result wrappings
    std::string errFnName = deriveRuntimeFnName(matchedPackage, "get_last_error");

    // Build C-level return type and handle out-param
    llvm::Type *outTy = nullptr;
    llvm::Type *cRetTy;

    switch (wrapping) {
    case ReturnWrapping::Direct:
        cRetTy = resolveType(matchedSig->returnTypeName);
        break;
    case ReturnWrapping::ResultPtr:
        cRetTy = ptrTy_;
        break;
    case ReturnWrapping::ResultStatus:
    case ReturnWrapping::BoolFromI64:
        cRetTy = i64Ty_;
        break;
    case ReturnWrapping::ResultOutParam:
        outTy = resolveType(outParamType);
        paramTypes.push_back(ptrTy_);
        cRetTy = i64Ty_;
        break;
    case ReturnWrapping::ResultPtrWithListMeta:
        llvm_unreachable("ResultPtrWithListMeta not used in generic native dispatch");
    }

    // Create alloca for out-param
    llvm::AllocaInst *outSlot = nullptr;
    if (wrapping == ReturnWrapping::ResultOutParam) {
        outSlot = builder_.CreateAlloca(outTy, nullptr, e.callee + "_out");
        args.push_back(outSlot);
    }

    // Emit the call
    auto *fnTy = llvm::FunctionType::get(cRetTy, paramTypes, false);
    auto fn = mod_->getOrInsertFunction(rtName, fnTy);
    llvm::Value *callResult;
    if (cRetTy->isVoidTy())
        callResult = builder_.CreateCall(fn, args);
    else
        callResult = builder_.CreateCall(fn, args, e.callee);

    // Apply return wrapping
    llvm::Value *result;
    switch (wrapping) {
    case ReturnWrapping::Direct:
        if (cRetTy->isVoidTy())
            return llvm::ConstantInt::get(i8Ty_, 0);
        result = callResult;
        break;

    case ReturnWrapping::ResultPtr:
        result = wrapPtrAsResult(callResult, errFnName.c_str());
        break;

    case ReturnWrapping::ResultStatus:
        result = wrapStatusAsResult(callResult, errFnName.c_str());
        break;

    case ReturnWrapping::BoolFromI64:
        return builder_.CreateTrunc(callResult, i1Ty_, e.callee + "_bool");

    case ReturnWrapping::ResultOutParam: {
        llvm::Value *isErr = builder_.CreateICmpNE(callResult,
            llvm::ConstantInt::get(i64Ty_, 0), e.callee + "_err");

        bool isBoolResult = (matchedSig->returnTypeName.find("Result<bool") == 0);
        llvm::Type *okTy = isBoolResult ? i1Ty_ : outTy;
        llvm::StructType *resTy = getResultType(okTy, errorTy_);
        result = emitResultBranch(isErr, resTy,
            [&]() {
                llvm::Value *loaded = builder_.CreateLoad(outTy, outSlot, e.callee + "_val");
                if (isBoolResult)
                    loaded = builder_.CreateTrunc(loaded, i1Ty_, e.callee + "_bool");
                return buildOkValue(loaded, resTy);
            },
            [&]() {
                return buildErrValue(
                    buildErrorFromRuntime(errFnName.c_str()), resTy);
            });
        break;
    }

    case ReturnWrapping::ResultPtrWithListMeta:
        llvm_unreachable("ResultPtrWithListMeta not used in generic native dispatch");
    }

    // Propagate collection type metadata from the Ry return type annotation.
    // For Result<T, Error>, propagate the metadata of the inner Ok type T
    // onto the Result value so downstream operations can inspect elements.
    const std::string &retType = matchedSig->returnTypeName;
    if (retType.size() > 7 && retType.compare(0, 7, "Result<") == 0) {
        // Extract Ok type using the same depth-aware comma finder as inferReturnWrapping
        int depth = 0;
        size_t commaPos = std::string::npos;
        for (size_t i = 7; i < retType.size(); ++i) {
            if (retType[i] == '<') ++depth;
            else if (retType[i] == '>') --depth;
            else if (retType[i] == ',' && depth == 0) { commaPos = i; break; }
        }
        if (commaPos != std::string::npos) {
            std::string okType = retType.substr(7, commaPos - 7);
            while (!okType.empty() && okType.back() == ' ') okType.pop_back();
            propagateTypeMeta(okType, result);
        }
    } else {
        propagateTypeMeta(retType, result);
    }

    return result;
}

} // namespace ry
