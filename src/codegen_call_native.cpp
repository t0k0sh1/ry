#include "ry/codegen.hpp"
#include "ry/stdlib_registry.hpp"


namespace ry {

llvm::Value *CodeGen::emitTableDrivenNativeCall(
    const CallExpr &e,
    const char *package,
    const NativeDispatchEntry *table,
    size_t table_size) {

    // Guard: check if this callee has any registered @native signature
    // for this package (or as a bare name for inline declarations).
    // The sigKey is reused later for signature lookup (variadic + normal paths).
    std::string sigKey = ry::util::nativeSigKey(package, e.callee);
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
        return ry::util::deriveRuntimeFnName(package, "get_last_error");
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
            // Fallback: try bare name for inline @native declarations without module
            sigIt = native_fn_sigs_.find(e.callee);
            if (sigIt == native_fn_sigs_.end())
                return nullptr;  // No sig for this module — fall through
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
        args.reserve(n);
        for (size_t i = 0; i < n; i++)
            args.push_back(emitExpr(*e.args[i]));

        // Two-pass overload resolution (see normal path for rationale): exact
        // match first, widening (e.g. int->float) as fallback.
        const NativeFnSignature *matchedSig = nullptr;
        std::vector<llvm::Type *> argTypes;
        std::vector<bool> needsWidening(n, false);
        std::vector<llvm::Type *> candidateTypes;
        candidateTypes.reserve(n);

        for (const auto &sig : sigIt->second) {
            if (sig.params.size() != n) continue;
            bool typesMatch = true;
            candidateTypes.clear();
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
            std::vector<bool> candidateNeedsWidening(n, false);
            for (const auto &sig : sigIt->second) {
                if (sig.params.size() != n) continue;
                bool typesMatch = true;
                candidateTypes.clear();
                candidateNeedsWidening.assign(n, false);
                for (size_t i = 0; i < n; i++) {
                    llvm::Type *expectedTy = resolveType(sig.params[i].typeName);
                    if (args[i]->getType() == expectedTy) {
                        // exact
                    } else if (isWideningConversion(args[i], expectedTy,
                                                    sig.params[i].typeName)) {
                        candidateNeedsWidening[i] = true;
                    } else {
                        typesMatch = false;
                        break;
                    }
                    candidateTypes.push_back(expectedTy);
                }
                if (typesMatch) {
                    matchedSig = &sig;
                    argTypes = std::move(candidateTypes);
                    needsWidening = std::move(candidateNeedsWidening);
                    break;
                }
            }
        }

        if (!matchedSig) {
            std::vector<std::string> actualTypes;
            actualTypes.reserve(n);
            for (auto *v : args)
                actualTypes.push_back(formatActualArgTypeName(v));
            codegenErrorNoMatchingOverload(
                e.callee,
                collectNativeOverloadCandidateSigs(e.callee),
                actualTypes);
        }

        // Apply widening coercions to matched args (no-op when all false).
        for (size_t i = 0; i < n; i++) {
            if (needsWidening[i])
                args[i] = emitWideningConversion(args[i], argTypes[i]);
        }

        if (!matchedSig->library.empty())
            used_native_libraries_.insert(matchedSig->library);

        std::string rtName = (entry->rtNameOverride
            ? std::string(entry->rtNameOverride)
            : ry::util::deriveRuntimeFnName(package, rtSuffix))
            + std::to_string(n);

        // For ResultPtr, the C runtime returns a raw pointer; wrap after call.
        llvm::Type *cRetTyVariadic = (entry->wrapping == ReturnWrapping::ResultPtr)
            ? ptrTy_
            : resolveType(matchedSig->returnTypeName);
        auto *fnTy = llvm::FunctionType::get(cRetTyVariadic, argTypes, false);
        auto fn = mod_->getOrInsertFunction(rtName, fnTy);
        llvm::Value *callResult = builder_.CreateCall(fn, args, entry->fnName);

        if (entry->wrapping == ReturnWrapping::ResultPtr) {
            std::string errFn = getErrFnName();
            return wrapPtrAsResult(callResult, errFn.c_str());
        }
        return callResult;
    }

    // --- Normal path ---

    // Look up NativeFnSignature matching this call's arity.
    // Check sig key BEFORE requireArgs so that a name collision with a
    // different-library function falls through instead of erroring.
    auto sigIt = native_fn_sigs_.find(sigKey);
    if (sigIt == native_fn_sigs_.end()) {
        // Fallback: try bare name for inline @native declarations without module
        sigIt = native_fn_sigs_.find(e.callee);
        if (sigIt == native_fn_sigs_.end())
            return nullptr;  // No sig for this module — fall through
    }

    // Custom emitter escape hatch: runs only after sig key AND call-arity
    // validation.  The arity check uses the CALL's actual arg count against
    // registered signatures so that same-module overloads with different
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

        // #1682: mock/spy interception for customEmitter natives. We resolve
        // the call's target overload at AST level (arity-only when arities
        // differ — sufficient for math.digits-style overloads), build the
        // canonical sig, and only divert through the tri-block dispatch if
        // the sig was registered via mock() / spy(). Otherwise fall straight
        // through to the customEmitter (zero-overhead path).
        if (test_mode_) {
            const NativeFnSignature *picked =
                pickNativeOverloadByCallShape(e, sigIt->second);
            if (picked) {
                std::vector<std::string> pn;
                pn.reserve(picked->params.size());
                for (const auto &p : picked->params) pn.push_back(p.typeName);
                const std::string canonicalSig = buildCanonicalSig(e.callee, pn);
                const bool isMocked = mocked_functions_.count(canonicalSig) > 0;
                const bool isSpied = spied_functions_.count(canonicalSig) > 0;
                if (isMocked || isSpied) {
                    return emitNativeCustomEmitterMockDispatch(
                        e, entry, *picked, canonicalSig, isMocked, isSpied);
                }
            }
        }
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
    args.reserve(static_cast<size_t>(entry->arity));
    for (size_t i = 0; i < static_cast<size_t>(entry->arity); i++)
        args.push_back(emitExpr(*e.args[i]));

    // Two-pass overload resolution: exact match first, then widening fallback
    // (e.g. int->float). Mirrors resolveOverload() so user-defined and
    // @native overload sets behave consistently.
    const NativeFnSignature *matchedSig = nullptr;
    std::vector<llvm::Type *> paramLLVMTypes;
    std::vector<bool> needsWidening(static_cast<size_t>(entry->arity), false);
    std::vector<llvm::Type *> candidateTypes;
    candidateTypes.reserve(static_cast<size_t>(entry->arity));

    for (const auto &sig : sigIt->second) {
        if (static_cast<int>(sig.params.size()) != entry->arity) continue;
        bool typesMatch = true;
        candidateTypes.clear();
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
        std::vector<bool> candidateNeedsWidening(
            static_cast<size_t>(entry->arity), false);
        for (const auto &sig : sigIt->second) {
            if (static_cast<int>(sig.params.size()) != entry->arity) continue;
            bool typesMatch = true;
            candidateTypes.clear();
            candidateNeedsWidening.assign(
                static_cast<size_t>(entry->arity), false);
            for (size_t i = 0; i < static_cast<size_t>(entry->arity); i++) {
                llvm::Type *expectedTy = resolveType(sig.params[i].typeName);
                if (args[i]->getType() == expectedTy) {
                    // exact
                } else if (isWideningConversion(args[i], expectedTy,
                                                sig.params[i].typeName)) {
                    candidateNeedsWidening[i] = true;
                } else {
                    typesMatch = false;
                    break;
                }
                candidateTypes.push_back(expectedTy);
            }
            if (typesMatch) {
                matchedSig = &sig;
                paramLLVMTypes = std::move(candidateTypes);
                needsWidening = std::move(candidateNeedsWidening);
                break;
            }
        }
    }

    if (!matchedSig) {
        std::vector<std::string> actualTypes;
        actualTypes.reserve(args.size());
        for (auto *v : args)
            actualTypes.push_back(formatActualArgTypeName(v));
        codegenErrorNoMatchingOverload(
            e.callee,
            collectNativeOverloadCandidateSigs(e.callee),
            actualTypes);
    }

    // Apply widening coercions to matched args (no-op when all false).
    for (size_t i = 0; i < static_cast<size_t>(entry->arity); i++) {
        if (needsWidening[i])
            args[i] = emitWideningConversion(args[i], paramLLVMTypes[i]);
    }

    if (!matchedSig->library.empty())
        used_native_libraries_.insert(matchedSig->library);

    // Derive runtime function name
    std::string rtName = entry->rtNameOverride
        ? entry->rtNameOverride
        : ry::util::deriveRuntimeFnName(package, rtSuffix);

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

    // Reject list arguments whose element type is not i8 (e.g. bare int literals
    // produce i64-stride lists that are incompatible with IOListHeader).
    if (entry->requireListU8Arg >= 0) {
        const auto argIdx = static_cast<size_t>(entry->requireListU8Arg);
        if (argIdx >= args.size())
            codegenError(std::string(entry->fnName) +
                         "() has invalid requireListU8Arg dispatch metadata");
        llvm::Type *elemTy = getListElementType(args[argIdx]);
        if (!elemTy || elemTy != i8Ty_)
            codegenError(std::string(entry->fnName) + "() requires List<u8> as argument " +
                         std::to_string(argIdx) + "; use [97u8, 0u8, 98u8]"
                         " (explicit u8 literals) or toBytes(\"...\") to produce a byte list");
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
// only for ResultOutParam. List<T> element-stride metadata for
// `Result<List<T>, _>` is set later via propagateTypeMeta on the wrapped
// result, so no extra return field is needed here.
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
    args.reserve(e.args.size());
    for (size_t i = 0; i < e.args.size(); i++)
        args.push_back(emitExpr(*e.args[i]));

    // Two-pass overload resolution across libraries: exact match first,
    // widening (e.g. int->float) as fallback. Ambiguity is detected per tier
    // (two exact matches across libs = ambiguous; two widening matches across
    // libs = ambiguous). An exact match in one lib beats a widening match in
    // another — Pass 2 only runs on empty Pass 1.
    const NativeFnSignature *matchedSig = nullptr;
    std::string matchedPackage;
    std::vector<llvm::Type *> paramTypes;
    std::vector<bool> needsWidening(e.args.size(), false);
    std::vector<llvm::Type *> candidateTypes;
    candidateTypes.reserve(e.args.size());

    for (const auto &lib : libIt->second) {
        std::string sigKey = ry::util::nativeSigKey(lib, e.callee);
        auto sigIt = native_fn_sigs_.find(sigKey);
        if (sigIt == native_fn_sigs_.end()) continue;
        for (const auto &sig : sigIt->second) {
            if (sig.params.size() != e.args.size()) continue;
            bool typesMatch = true;
            candidateTypes.clear();
            for (size_t i = 0; i < args.size(); i++) {
                llvm::Type *expectedTy = resolveType(sig.params[i].typeName);
                if (args[i]->getType() != expectedTy) {
                    typesMatch = false;
                    break;
                }
                candidateTypes.push_back(expectedTy);
            }
            if (typesMatch) {
                if (matchedSig) {
                    std::string ambigMsg = "ambiguous @native call: '";
                    ambigMsg += e.callee;
                    ambigMsg += "' matches both @native(\"";
                    ambigMsg += matchedPackage;
                    ambigMsg += "\") and @native(\"";
                    ambigMsg += lib;
                    ambigMsg += "\")";
                    codegenError(ambigMsg);
                }
                matchedSig = &sig;
                matchedPackage = lib;
                paramTypes = std::move(candidateTypes);
            }
        }
    }

    if (!matchedSig) {
        std::vector<bool> candidateNeedsWidening(e.args.size(), false);
        for (const auto &lib : libIt->second) {
            std::string sigKey = ry::util::nativeSigKey(lib, e.callee);
            auto sigIt = native_fn_sigs_.find(sigKey);
            if (sigIt == native_fn_sigs_.end()) continue;
            for (const auto &sig : sigIt->second) {
                if (sig.params.size() != e.args.size()) continue;
                bool typesMatch = true;
                candidateTypes.clear();
                candidateNeedsWidening.assign(e.args.size(), false);
                for (size_t i = 0; i < args.size(); i++) {
                    llvm::Type *expectedTy = resolveType(sig.params[i].typeName);
                    if (args[i]->getType() == expectedTy) {
                        // exact
                    } else if (isWideningConversion(args[i], expectedTy,
                                                    sig.params[i].typeName)) {
                        candidateNeedsWidening[i] = true;
                    } else {
                        typesMatch = false;
                        break;
                    }
                    candidateTypes.push_back(expectedTy);
                }
                if (typesMatch) {
                    if (matchedSig) {
                        std::string ambigMsg = "ambiguous @native call: '";
                        ambigMsg += e.callee;
                        ambigMsg += "' matches both @native(\"";
                        ambigMsg += matchedPackage;
                        ambigMsg += "\") and @native(\"";
                        ambigMsg += lib;
                        ambigMsg += "\") via implicit widening";
                        codegenError(ambigMsg);
                    }
                    matchedSig = &sig;
                    matchedPackage = lib;
                    paramTypes = std::move(candidateTypes);
                    needsWidening = std::move(candidateNeedsWidening);
                }
            }
        }
    }

    // No library matched both arity and types — fall through to user functions
    if (!matchedSig) return nullptr;

    used_native_libraries_.insert(matchedPackage);

    // Reject list arguments whose declared param type is `List<u8>` but
    // whose actual element stride is not i8 (e.g. bare int literals
    // produce i64-stride lists incompatible with IOListHeader). Sig-level
    // type match cannot catch this because `List<u8>` and `List<int>`
    // both resolve to `ptrTy_`; the byte-stride invariant lives in
    // TypeMeta::ListElem. Mirrors the table-driven enforcement at
    // `emitTableDrivenNativeCall:364-374` so generic-dispatch callers
    // see the same error wording. Resolve type aliases before comparing
    // so a `type Bytes = List<u8>` declaration is still gated.
    for (size_t i = 0; i < matchedSig->params.size(); i++) {
        const std::string declared = resolveTypeAlias(
            ry::util::trimTypeNameSpaces(matchedSig->params[i].typeName));
        if (declared != "List<u8>") continue;
        llvm::Type *elemTy = getListElementType(args[i]);
        if (!elemTy || elemTy != i8Ty_)
            codegenError(e.callee + "() requires List<u8> as argument "
                         + std::to_string(i)
                         + "; use [97u8, 0u8, 98u8] (explicit u8 literals) or"
                           " toBytes(\"...\") to produce a byte list");
    }

    // Apply widening coercions to matched args (no-op when all false).
    for (size_t i = 0; i < e.args.size(); i++) {
        if (needsWidening[i])
            args[i] = emitWideningConversion(args[i], paramTypes[i]);
    }

    // Adjust bool params to match the C calling convention: native functions pass bools as i64.
    // Widen i1→i64 in both the prototype and the arg values.
    for (size_t i = 0; i < paramTypes.size(); i++) {
        if (paramTypes[i] == i1Ty_) {
            args[i] = builder_.CreateZExt(args[i], i64Ty_, "bool_zext");
            paramTypes[i] = i64Ty_;
        }
    }

    // Derive runtime function name: __ry_<module>_<fn_name>.
    // Apply per-module naming convention (snake_case for legacy modules
    // whose C runtime predates Ry's camelCase identifiers, e.g. base64).
    const auto *pkgEntry = StdlibRegistry::instance().findPackage(matchedPackage);
    std::string symbolName = (pkgEntry && pkgEntry->snake_case_symbols)
        ? ry::util::camelToSnakeCase(e.callee)
        : e.callee;
    std::string rtName = ry::util::deriveRuntimeFnName(matchedPackage, symbolName);

    // Determine C-level calling convention from the Ry return type
    auto [wrapping, outParamType] = inferReturnWrapping(matchedSig->returnTypeName);

    // Error function name for Result wrappings
    std::string errFnName = ry::util::deriveRuntimeFnName(matchedPackage, "get_last_error");

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

// ===== #1682: mock/spy for @native customEmitter overloads =====

const CodeGen::NativeFnSignature *CodeGen::pickNativeOverloadByCallShape(
    const CallExpr &e, const std::vector<NativeFnSignature> &sigs) {
    // Pass 1: arity-unique match. Covers the digits(n) / digits(n, base)
    // pattern where each arity has exactly one overload. Most existing
    // customEmitter natives are arity-disjoint, so this short-circuit
    // resolves them without needing AST type inference.
    const NativeFnSignature *picked = nullptr;
    int matchCount = 0;
    for (const auto &sig : sigs) {
        if (sig.params.size() == e.args.size()) {
            picked = &sig;
            ++matchCount;
        }
    }
    if (matchCount == 1) return picked;
    if (matchCount == 0) return nullptr;

    // Pass 2: multiple same-arity overloads (e.g. abs(int) / abs(float),
    // pow(int, int) / pow(float, float)). Resolve by inferring each arg's
    // type at the AST level and matching against sig.params[i].typeName
    // verbatim. Widening (int → float fallback) is NOT applied here — the
    // unmocked customEmitter path handles widening internally, so for mock
    // dispatch we only intercept exact-match calls. Mismatched-type calls
    // fall through to the customEmitter (unchanged behavior).
    std::unordered_map<std::string, llvm::Type *> emptyParamMap;
    std::unordered_map<std::string, std::string> emptyParamNameMap;
    std::vector<std::string> actualNames;
    actualNames.reserve(e.args.size());
    for (const auto &arg : e.args) {
        std::string n = inferExprTypeName(*arg, emptyParamMap, emptyParamNameMap);
        actualNames.push_back(resolveTypeAlias(ry::util::trimTypeNameSpaces(n)));
    }

    picked = nullptr;
    matchCount = 0;
    for (const auto &sig : sigs) {
        if (sig.params.size() != e.args.size()) continue;
        bool eq = true;
        for (size_t i = 0; i < sig.params.size(); ++i) {
            if (resolveTypeAlias(ry::util::trimTypeNameSpaces(sig.params[i].typeName))
                    != actualNames[i]) {
                eq = false; break;
            }
        }
        if (eq) { picked = &sig; ++matchCount; }
    }
    return matchCount == 1 ? picked : nullptr;
}

llvm::Value *CodeGen::emitNativeCustomEmitterMockDispatch(
    const CallExpr &e,
    const NativeDispatchEntry *entry,
    const NativeFnSignature &sig,
    const std::string &canonicalSig,
    bool isMocked,
    bool isSpied) {
    // Mock/spy intercept for customEmitter natives. Three runtime cases mirror
    // the user-fn dispatch (see codegen_call_user.cpp):
    //   - spy-only: linear increment, fall through to customEmitter
    //   - mocked: tri-block (mockBB calls replacement via __ry_mock_get,
    //             origBB delegates to customEmitter), PHI-merged
    //   - mocked+spied: tri-block; origBB additionally increments before
    //                   delegating (spy semantics when mock is runtime-inactive)
    //
    // v1 limitation: customEmitter-path natives do NOT record args for
    // verifyCalledWith. Only count-style verify("name") is supported. The
    // recording side requires an OverloadEntry (paramTypes/paramTypeNames) to
    // dispatch the per-arg store function; NativeFnSignature.params carry
    // only Ry-level type names, and synthesizing a fake OverloadEntry would
    // duplicate the dispatch logic. Defer to a follow-up.

    auto &nameStr = mock_name_strings_[canonicalSig];
    if (!nameStr) nameStr = cachedGlobalString(canonicalSig, ".mock." + canonicalSig);

    llvm::FunctionCallee mockIncFn =
        getRuntimeFn("__ry_mock_increment_call", llvm::Type::getVoidTy(*ctx_), {ptrTy_});

    // Spy-only: emit linear increment, then delegate to the customEmitter
    // (which emits args + the original call).
    if (isSpied && !isMocked) {
        builder_.CreateCall(mockIncFn, {nameStr});
        return entry->customEmitter(*this, e);
    }

    // Mocked (with or without spy). Use the mock-get probe BEFORE emitting
    // args so we can split into mockBB / origBB and emit args independently
    // in each branch — args may have side effects, so we want exactly one
    // evaluation per runtime path (not two if we pre-emitted).
    llvm::FunctionCallee mockGetFn    = getRuntimeFn("__ry_mock_get",     ptrTy_, {ptrTy_});
    llvm::FunctionCallee mockGetEnvFn = getRuntimeFn("__ry_mock_get_env", ptrTy_, {ptrTy_});

    llvm::Value *mockPtr = builder_.CreateCall(mockGetFn, {nameStr}, "mock_ptr");
    llvm::Value *nullPtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));
    llvm::Value *mockActive = builder_.CreateICmpNE(mockPtr, nullPtr, "is_mocked");

    llvm::BasicBlock *mockBB = createBB("mock_bb");
    llvm::BasicBlock *origBB = createBB("orig_bb");
    llvm::BasicBlock *mergeBB = createBB("merge_bb");
    emitBranchCond(mockActive, mockBB, origBB);

    // Mock path: emit args fresh, increment, dispatch via plain or capture
    // calling convention based on env pointer.
    builder_.SetInsertPoint(mockBB);
    builder_.CreateCall(mockIncFn, {nameStr});

    std::vector<llvm::Value *> mockArgs;
    mockArgs.reserve(e.args.size());
    std::vector<llvm::Type *> mockParamTys;
    mockParamTys.reserve(sig.params.size());
    for (size_t i = 0; i < e.args.size(); ++i) {
        llvm::Value *v = emitExpr(*e.args[i]);
        llvm::Type *want = resolveType(sig.params[i].typeName);
        if (v->getType() != want && isWideningConversion(v, want, sig.params[i].typeName))
            v = emitWideningConversion(v, want);
        mockArgs.push_back(v);
        mockParamTys.push_back(want);
    }
    llvm::Type *mockRetTy = resolveType(sig.returnTypeName);
    llvm::FunctionType *mockFnTy = llvm::FunctionType::get(mockRetTy, mockParamTys, false);

    llvm::Value *envPtr = builder_.CreateCall(mockGetEnvFn, {nameStr}, "mock_env");
    llvm::Value *isCapture = builder_.CreateICmpNE(envPtr, nullPtr, "is_capture_mock");

    llvm::BasicBlock *plainBB = createBB("mock_plain_bb");
    llvm::BasicBlock *captureBB = createBB("mock_capture_bb");
    emitBranchCond(isCapture, captureBB, plainBB);

    std::vector<llvm::Type *> captureParamTys = mockParamTys;
    captureParamTys.push_back(ptrTy_);
    llvm::FunctionType *captureFnTy = llvm::FunctionType::get(
        mockRetTy, captureParamTys, false);
    std::vector<llvm::Value *> captureArgs = mockArgs;
    captureArgs.push_back(envPtr);

    builder_.SetInsertPoint(plainBB);
    llvm::Value *mockResultPlain = mockRetTy->isVoidTy()
        ? nullptr
        : builder_.CreateCall(mockFnTy, mockPtr, mockArgs, "mock_result_plain");
    if (mockRetTy->isVoidTy())
        builder_.CreateCall(mockFnTy, mockPtr, mockArgs);
    emitBranchUncond(mergeBB);
    llvm::BasicBlock *plainEndBB = builder_.GetInsertBlock();

    builder_.SetInsertPoint(captureBB);
    llvm::Value *mockResultCapture = mockRetTy->isVoidTy()
        ? nullptr
        : builder_.CreateCall(captureFnTy, mockPtr, captureArgs, "mock_result_capture");
    if (mockRetTy->isVoidTy())
        builder_.CreateCall(captureFnTy, mockPtr, captureArgs);
    emitBranchUncond(mergeBB);
    llvm::BasicBlock *captureEndBB = builder_.GetInsertBlock();

    // Original path: delegate to customEmitter (which emits its own args).
    builder_.SetInsertPoint(origBB);
    if (isSpied)
        builder_.CreateCall(mockIncFn, {nameStr});
    llvm::Value *origResult = entry->customEmitter(*this, e);
    emitBranchUncond(mergeBB);
    llvm::BasicBlock *origEndBB = builder_.GetInsertBlock();

    builder_.SetInsertPoint(mergeBB);
    if (mockRetTy->isVoidTy())
        return llvm::ConstantInt::get(i8Ty_, 0);

    llvm::PHINode *phi = createPhi(mockRetTy, {}, "call_result");
    phi->addIncoming(mockResultPlain, plainEndBB);
    phi->addIncoming(mockResultCapture, captureEndBB);
    phi->addIncoming(origResult, origEndBB);
    propagateTypeMeta(sig.returnTypeName, phi);
    return phi;
}

} // namespace ry
