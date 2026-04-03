#include "ry/codegen.hpp"
#include "ry/builtin_stdlib_registry.hpp"
#include "ry/diagnostic.hpp"

// ===== CallExpr Dispatcher =====

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<CallExpr> &e) {
    // ADT constructor: Enum::Variant(args...)
    {
        auto colonPos = e->callee.find("::");
        if (colonPos != std::string::npos) {
            std::string enumName = e->callee.substr(0, colonPos);
            std::string variantName = e->callee.substr(colonPos + 2);
            // Try to instantiate generic enum if not found
            if (!enum_types_.count(enumName)) {
                auto ltPos = enumName.find('<');
                if (ltPos != std::string::npos && enumName.back() == '>') {
                    std::string baseName = enumName.substr(0, ltPos);
                    std::string argsStr = enumName.substr(ltPos + 1, enumName.size() - ltPos - 2);
                    auto typeArgs = splitTypeArgs(argsStr);
                    instantiateGenericEnum(enumName, baseName, typeArgs);
                }
            }
            auto eit = enum_types_.find(enumName);
            if (eit != enum_types_.end() && eit->second.isADT) {
                auto &info = eit->second;
                auto vit = info.variants.find(variantName);
                if (vit == info.variants.end())
                    codegenError("unknown variant '" + variantName + "' in enum '" + enumName + "'");
                int64_t tag = vit->second;

                auto fit = info.variantFields.find(variantName);
                if (fit == info.variantFields.end())
                    codegenError("variant '" + variantName + "' has no associated data");
                auto &fieldInfo = fit->second;
                if (e->args.size() != fieldInfo.fieldTypes.size())
                    codegenError("variant '" + variantName + "' expects " +
                        std::to_string(fieldInfo.fieldTypes.size()) + " arguments");

                llvm::Value *adtVal = llvm::UndefValue::get(info.adtType);
                adtVal = builder_.CreateInsertValue(adtVal, llvm::ConstantInt::get(i64Ty_, tag), 0, "adt.tag");

                const llvm::DataLayout &dl = mod_->getDataLayout();
                llvm::AllocaInst *tmpAlloca = builder_.CreateAlloca(info.adtType, nullptr, "adt.tmp");
                builder_.CreateStore(adtVal, tmpAlloca);
                llvm::Value *payloadPtr = builder_.CreateStructGEP(info.adtType, tmpAlloca, 1, "adt.payload");

                size_t offset = 0;
                for (size_t i = 0; i < e->args.size(); ++i) {
                    llvm::Value *argVal = emitExpr(*e->args[i]);
                    uint64_t align = dl.getABITypeAlign(fieldInfo.fieldTypes[i]).value();
                    offset = (offset + align - 1) / align * align;
                    llvm::Value *fieldPtr = builder_.CreateGEP(
                        llvm::Type::getInt8Ty(*ctx_), payloadPtr,
                        {llvm::ConstantInt::get(i64Ty_, offset)}, "adt.field." + std::to_string(i));
                    builder_.CreateStore(argVal, fieldPtr);
                    offset += dl.getTypeAllocSize(fieldInfo.fieldTypes[i]);
                }

                llvm::Value *result = builder_.CreateLoad(info.adtType, tmpAlloca, "adt.val");
                enum_value_types_[result] = enumName;
                return result;
            }
        }
    }

    // verify(fn_name) → call count
    if (e->callee == "verify") {
        if (!test_mode_)
            codegenError("'verify' is only allowed in test mode (use 'ry test')");
        if (e->args.size() != 1)
            codegenError("verify() requires exactly 1 argument");
        auto *strExpr = std::get_if<StringExpr>(&e->args[0]->data);
        if (!strExpr)
            codegenError("verify() argument must be a function name");
        auto vit = functions_.find(strExpr->value);
        if (vit == functions_.end())
            codegenError("verify(): unknown function '" + strExpr->value + "'");
        if (vit->second.size() != 1)
            codegenError("verify(): overloaded functions are not supported");
        auto *getCountTy = fnTy_ptr_to_i64_;
        llvm::FunctionCallee getCountFn = mod_->getOrInsertFunction("__ry_mock_get_call_count", getCountTy);
        llvm::Value *nameStr = cachedGlobalString(strExpr->value, ".verify_name");
        return builder_.CreateCall(getCountFn, {nameStr}, "call_count");
    }

    // Fast path: pre-emit args[0] once for callee names shared by multiple
    // dispatchers, then route through the same try-chain order to avoid
    // emitting dead IR from earlier dispatchers that don't match the type.
    // The else branch ensures the normal chain is skipped when the fast path
    // handles these names, preventing duplicate emission on fallthrough.
    if (e->args.size() >= 2 && (e->callee == "map" || e->callee == "filter")) {
        llvm::Value *arg0 = emitExpr(*e->args[0]);
        if (auto *v = emitBuiltinResult(*e, arg0))      return v;
        if (auto *v = emitBuiltinIterator(*e, arg0))    return v;
        if (auto *v = emitBuiltinHigherOrder(*e, arg0)) return v;
    } else if (e->args.size() == 2 && e->callee == "take") {
        llvm::Value *arg0 = emitExpr(*e->args[0]);
        if (auto *v = emitBuiltinIterator(*e, arg0))    return v;
        if (auto *v = emitBuiltinCollection(*e, arg0))  return v;
    } else {
        // Dispatch to language-builtin helpers (Pattern B: no @native registry)
        if (auto *v = emitBuiltinResult(*e))      return v;
        if (auto *v = emitBuiltinIterator(*e))    return v;
        if (auto *v = emitBuiltinString(*e))      return v;
        if (auto *v = emitBuiltinConversion(*e))  return v;
        if (auto *v = emitBuiltinQuery(*e))       return v;
        if (auto *v = emitBuiltinCore(*e))        return v;
        if (auto *v = emitBuiltinHigherOrder(*e)) return v;
        if (auto *v = emitBuiltinCollection(*e))  return v;
        if (auto *v = emitBuiltinSetOps(*e))      return v;
        if (auto *v = emitBuiltinRegex(*e))       return v;
    }

    // Dispatch to stdlib package helpers (Pattern A: @native registry guard)
    using StdlibDispatcher = llvm::Value *(CodeGen::*)(const CallExpr &);
    static const StdlibDispatcher stdlib_dispatchers[] = {
#define RY_STDLIB_DISPATCHER_ENTRY(pkg, decl, method) &CodeGen::method,
        RY_BUILTIN_STDLIB_PACKAGES(RY_STDLIB_DISPATCHER_ENTRY)
#undef RY_STDLIB_DISPATCHER_ENTRY
    };
    for (auto dispatcher : stdlib_dispatchers) {
        if (auto *v = (this->*dispatcher)(*e)) return v;
    }

    // Struct constructor
    auto sit = struct_types_.find(e->callee);
    if (sit != struct_types_.end()) {
        if (deprecated_types_.count(e->callee))
            emitDeprecationWarning(e->callee);
        return emitStructConstructor(sit->second, e->callee, e->args);
    }

    // Try indirect call via variable (function pointer / lambda)
    if (llvm::AllocaInst *varPtr = findVar(e->callee)) {
        auto fnIt = fn_type_info_.find(varPtr);
        if (fnIt != fn_type_info_.end()) {
            auto &info = fnIt->second;

            std::vector<llvm::Value*> argVals;
            for (auto &arg : e->args)
                argVals.push_back(emitExpr(*arg));
            llvm::Value *loaded = builder_.CreateLoad(ptrTy_, varPtr, e->callee + ".fn");
            return emitLambdaCall(loaded, info, argVals, "indirect_call");
        }

        if (auto *result = tryCallOperator(e->callee, e->args))
            return result;
    }

    // Generic function dispatch (explicit type args or type inference)
    {
        std::string baseName = e->callee;
        std::vector<std::string> typeArgs;

        auto ltPos = baseName.find('<');
        if (ltPos != std::string::npos && baseName.back() == '>') {
            std::string argsStr = baseName.substr(ltPos + 1, baseName.size() - ltPos - 2);
            baseName = baseName.substr(0, ltPos);
            typeArgs = splitTypeArgs(argsStr);
        }

        if (generic_fn_templates_.count(baseName)) {
            if (typeArgs.empty() && !functions_.count(baseName))
                typeArgs = inferTypeArgs(baseName, e->args);
            if (!typeArgs.empty()) {
                instantiateGenericFn(baseName, typeArgs);
                // Build fullName matching instantiateGenericFn's key format
                std::string fullName = baseName + "<";
                for (size_t i = 0; i < typeArgs.size(); ++i) {
                    if (i > 0) fullName += ",";
                    fullName += typeArgs[i];
                }
                fullName += ">";
                return emitUserFnCall(fullName, e->args);
            }
        }
    }

    return emitUserFnCall(e->callee, e->args);
}

// ===== Lambda call helper =====

std::vector<llvm::Value*> CodeGen::coerceCallArgs(const FnTypeInfo &info,
                                                  std::vector<llvm::Value*> args,
                                                  const std::string &context) {
    if (args.size() != info.paramTypes.size()) {
        codegenError(
            context + ": expected " + std::to_string(info.paramTypes.size()) +
            " arguments, got " + std::to_string(args.size()));
    }

    for (size_t i = 0; i < args.size(); ++i) {
        if (args[i]->getType() == info.paramTypes[i])
            continue;

        if (auto *sliced = tryEmitSubtypeCoerce(args[i], info.paramTypes[i])) {
            args[i] = sliced;
            continue;
        }

        if (isAnyType(info.paramTypes[i])) {
            args[i] = wrapInAny(args[i]);
            continue;
        }

        if (isAnyType(args[i]->getType()) && canAnyHoldType(info.paramTypes[i])) {
            args[i] = unwrapFromAny(args[i], info.paramTypes[i]);
            continue;
        }

        if (i < info.paramTypeNames.size() && isUnionType(info.paramTypeNames[i])) {
            args[i] = wrapInUnion(args[i], info.paramTypeNames[i]);
            continue;
        }

        codegenError(context + ": argument " + std::to_string(i) + " type mismatch");
    }

    return args;
}

llvm::Value *CodeGen::emitLambdaCall(llvm::Value *lambdaVal, const FnTypeInfo &info,
                                      std::vector<llvm::Value*> args, const std::string &name) {
    args = coerceCallArgs(info, std::move(args), "lambda call");

    if (info.capturedVars.empty()) {
        llvm::FunctionType *ft = llvm::FunctionType::get(
            info.returnType, info.paramTypes, false);
        if (info.returnType->isVoidTy())
            return builder_.CreateCall(ft, lambdaVal, args);
        return builder_.CreateCall(ft, lambdaVal, args, name);
    } else {
        std::vector<llvm::Type*> closureFields;
        closureFields.push_back(ptrTy_);
        for (auto *ct : info.capturedTypes)
            closureFields.push_back(ct);
        llvm::StructType *closureTy = llvm::StructType::get(*ctx_, closureFields);

        llvm::Value *fnPtrField = builder_.CreateStructGEP(
            closureTy, lambdaVal, 0, "lcall.fn_ptr");
        llvm::Value *fnPtr = builder_.CreateLoad(ptrTy_, fnPtrField, "lcall.fn");

        std::vector<llvm::Value*> fullArgs = args;
        std::vector<llvm::Type*> allParamTypes = info.paramTypes;
        for (size_t i = 0; i < info.capturedTypes.size(); ++i) {
            llvm::Value *capField = builder_.CreateStructGEP(
                closureTy, lambdaVal, i + 1, "lcall.cap." + std::to_string(i));
            llvm::Value *capVal = builder_.CreateLoad(
                info.capturedTypes[i], capField, "lcall.cap_val." + std::to_string(i));
            fullArgs.push_back(capVal);
            allParamTypes.push_back(info.capturedTypes[i]);
        }

        llvm::FunctionType *ft = llvm::FunctionType::get(
            info.returnType, allParamTypes, false);
        if (info.returnType->isVoidTy())
            return builder_.CreateCall(ft, fnPtr, fullArgs);
        return builder_.CreateCall(ft, fnPtr, fullArgs, name);
    }
}


// ===== Shared Result-wrapping helpers =====

llvm::Value *CodeGen::emitResultBranch(llvm::Value *isErr, llvm::StructType *resTy,
                                        std::function<llvm::Value*()> buildOk,
                                        std::function<llvm::Value*()> buildErr) {
    llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "res.ok", fn_);
    llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, "res.err", fn_);
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "res.merge", fn_);
    builder_.CreateCondBr(isErr, errBB, okBB);

    builder_.SetInsertPoint(okBB);
    llvm::Value *okVal = buildOk();
    builder_.CreateBr(mergeBB);
    okBB = builder_.GetInsertBlock();

    builder_.SetInsertPoint(errBB);
    llvm::Value *errVal = buildErr();
    builder_.CreateBr(mergeBB);
    errBB = builder_.GetInsertBlock();

    builder_.SetInsertPoint(mergeBB);
    llvm::PHINode *phi = builder_.CreatePHI(resTy, 2, "result");
    phi->addIncoming(okVal, okBB);
    phi->addIncoming(errVal, errBB);
    return phi;
}

llvm::Value *CodeGen::buildErrorFromRuntime(const char *errFnName) {
    auto errFnTy = llvm::FunctionType::get(ptrTy_, {}, false);
    auto errFn = mod_->getOrInsertFunction(errFnName, errFnTy);
    llvm::Value *errMsg = builder_.CreateCall(errFn, {}, "err_msg");
    llvm::Value *errStruct = llvm::UndefValue::get(errorTy_);
    errStruct = builder_.CreateInsertValue(errStruct, errMsg, 0, "err.msg");
    errStruct = builder_.CreateInsertValue(errStruct, llvm::ConstantInt::get(i64Ty_, 0), 1, "err.code");
    return errStruct;
}

llvm::Value *CodeGen::wrapPtrAsResult(llvm::Value *ptr, const char *errFnName) {
    llvm::StructType *resTy = getResultType(ptrTy_, errorTy_);
    llvm::Value *isNull = builder_.CreateICmpEQ(ptr,
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)), "is_null");
    return emitResultBranch(isNull, resTy,
        [&]() { return buildOkValue(ptr, resTy); },
        [&]() { return buildErrValue(buildErrorFromRuntime(errFnName), resTy); });
}

llvm::Value *CodeGen::wrapStatusAsResult(llvm::Value *status, const char *errFnName) {
    llvm::StructType *resTy = getResultType(i8Ty_, errorTy_);
    llvm::Value *isErr = builder_.CreateICmpNE(status,
        llvm::ConstantInt::get(i64Ty_, 0), "is_err");
    return emitResultBranch(isErr, resTy,
        [&]() { return buildOkValue(llvm::ConstantInt::get(i8Ty_, 0), resTy); },
        [&]() { return buildErrValue(buildErrorFromRuntime(errFnName), resTy); });
}

// ===== Codegen helpers =====

void CodeGen::requireArgs(const CallExpr &e, size_t expected) {
    requireArgs(e.callee, e.args.size(), expected);
}

void CodeGen::requireArgs(const std::string &callee, size_t actual, size_t expected) {
    if (actual != expected)
        codegenError(callee + "() takes exactly " + std::to_string(expected) +
                     " argument" + (expected == 1 ? "" : "s"));
}

llvm::Value *CodeGen::wrapPtrAsOption(llvm::Value *ptr, const std::string &hint) {
    llvm::Twine h(hint);
    llvm::Value *isNull = builder_.CreateICmpEQ(ptr,
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)),
        h + "_null");
    llvm::StructType *optTy = getOptionType(ptrTy_);
    llvm::Value *someVal = buildSomeValue(ptr, optTy);
    llvm::Value *noneVal = buildNoneValue(optTy);
    return builder_.CreateSelect(isNull, noneVal, someVal, h + "_opt");
}

// ===== Native constant registry & emission =====

enum class NativeConstantKind { Value, Infinity, NaN };

struct NativeConstantEntry {
    NativeConstantKind kind;
    double value;  // used only when kind == Value
};

static const std::unordered_map<std::string, NativeConstantEntry> native_constant_registry = {
#define RY_NATIVE_CONSTANT_ENTRY(pkg, name, kind, value) {#name, {NativeConstantKind::kind, value}},
    RY_BUILTIN_STDLIB_CONSTANTS(RY_NATIVE_CONSTANT_ENTRY)
#undef RY_NATIVE_CONSTANT_ENTRY
};

bool CodeGen::isNativeConstant(const std::string &name) {
    return native_constant_registry.count(name);
}

llvm::Value *CodeGen::emitNativeConstant(const std::string &name) {
    auto it = native_constant_registry.find(name);
    if (it == native_constant_registry.end())
        codegenError("unknown native constant: " + name);
    switch (it->second.kind) {
    case NativeConstantKind::Value:    return llvm::ConstantFP::get(f64Ty_, it->second.value);
    case NativeConstantKind::Infinity: return llvm::ConstantFP::getInfinity(f64Ty_);
    case NativeConstantKind::NaN:      return llvm::ConstantFP::getNaN(f64Ty_);
    }
    llvm_unreachable("unhandled NativeConstantKind");
}
