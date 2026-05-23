#include "ry/codegen.hpp"
#include "ry/stdlib_registry.hpp"
#include "ry/diagnostic.hpp"

#include <unordered_set>

namespace ry {

// ===== CallExpr Dispatcher =====

// #1746: returns true when `name` matches a stdlib package registered via
// RY_REGISTER_STDLIB_PACKAGE (e.g. "math", "json", "path"). The registry is
// populated by static initializers and immutable after program startup, so a
// function-local static set caches the snapshot.
bool CodeGen::isStdlibPackageName(const std::string &name) {
    static const std::unordered_set<std::string> kStdlibPackages = []() {
        std::unordered_set<std::string> s;
        for (const auto &pkg : StdlibRegistry::instance().packages())
            s.insert(pkg.package_name);
        return s;
    }();
    return kStdlibPackages.count(name) > 0;
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<CallExpr> &e) {
    // #1746: detect `<mod>.<callee>(...)` where `<mod>` names a stdlib
    // package the user forgot to `import`. The parser, finding `<mod>`
    // absent from `imported_modules_`, leaves `qualified_module` unset and
    // UFCS-lowers the call to `CallExpr{callee, args=[VariableExpr{<mod>},
    // ...]}`. Without this guard the call falls through to the table-driven
    // stdlib dispatcher (which fires "<callee>() takes exactly N argument"
    // because UFCS prepended the receiver) or to `resolveOverload`'s
    // "undefined function" path — both of which mislead users away from the
    // actual root cause. Gate on `!findVar(receiver)` so a local variable
    // happening to share a stdlib name (e.g. `path: str = "/tmp"` then
    // `path.basename()`) is not misdiagnosed: such locals shadow the
    // package and the existing "undefined function" path is correct for
    // them. Skip when `qualified_module` is set: the user wrote a properly
    // imported `<mod>.fn(...)`, and the qualified-module branch below
    // handles the missing-namespace case with its own diagnostic.
    //
    // #1747: when an alias is registered for the canonical module (e.g.
    // `import math as m` was seen earlier), the canonical name is no
    // longer in scope per the Python-style contract documented in
    // docs/reference/modules.md. The "add 'import math'" hint would be
    // misleading — the user already imported it under an alias. Emit a
    // targeted suggestion pointing at the alias instead.
    if (!e->qualified_module.has_value() && !e->args.empty()) {
        if (auto *ve = std::get_if<VariableExpr>(&e->args[0]->data)) {
            if (isStdlibPackageName(ve->name) && !findVar(ve->name)) {
                std::string msg;
                if (auto alias = findAliasForCanonical(ve->name)) {
                    msg = "'" + ve->name + "' is not defined. Did you mean '"
                        + *alias + "' (aliased from '" + ve->name + "')?";
                } else {
                    msg = "module '" + ve->name + "' is not imported (add 'import "
                        + ve->name + "' at the top of the file)";
                }
                codegenError(msg);
            }
        }
    }

    // Qualified call `<effective>.fn(args)` (#1723, #1724, #1730).
    //
    // The parser sets `qualified_module` when the dotted LHS is a single
    // identifier imported via `import <mod>` / `import <mod> as <alias>`.
    // Two dispatch flavors:
    //
    // - stdlib module: fall through to the regular call dispatch chain.
    //   The stdlib loader inlines @native decls at the top level so the
    //   bare-name route (`math.sqrt(2.0)` → callee `sqrt` → emitBuiltinMath)
    //   resolves naturally.
    //
    // - user-defined module: look up the callee in the namespace's
    //   `fn_overloads` bucket populated by NamespaceEmitScope (#1730).
    //   Pass the bucket explicitly to emitUserFnCall to avoid setting
    //   current_namespace_target_ during arg emission (which would
    //   misresolve caller-scope bare fn refs in the args).
    if (e->qualified_module.has_value()) {
        const std::string &mod = *e->qualified_module;
        // findModuleNamespace walks `effective_to_canonical_` so aliases
        // (`import usermod as u` followed by `u.greet()`) resolve to the
        // canonical-keyed bucket populated by the original import (#1730).
        ModuleNamespaceInfo *ns = findModuleNamespace(mod);
        if (!ns)
            codegenError("qualified call to module '" + mod +
                         "' which was not imported via 'import " + mod + "'");
        if (!ns->is_stdlib) {
            // Record constructor `usermod.MyRecord(args)` — look in the
            // namespace's records bucket before fn_overloads. Records and
            // fns share the call-syntax namespace, mirroring the bare-name
            // dispatch order at line 134.
            auto rit = ns->records.find(e->callee);
            if (rit != ns->records.end()) {
                if (deprecated_types_.count(e->callee))
                    emitDeprecationWarning(e->callee);
                return emitRecordConstructor(rit->second, e->callee, e->args);
            }
            auto fit = ns->fn_overloads.find(e->callee);
            if (fit == ns->fn_overloads.end()) {
                codegenError("module '" + ns->original_name +
                             "' has no function or record '" + e->callee + "'");
            }
            return emitUserFnCall(e->callee, e->args, &fit->second);
        }
        // stdlib module → fall through; downstream dispatchers route by callee.
    }

    // ADT constructor: Enum::Variant(args...)
    {
        auto colonPos = e->callee.find("::");
        if (colonPos != std::string::npos) {
            std::string enumName = e->callee.substr(0, colonPos);
            std::string variantName = e->callee.substr(colonPos + 2);
            // Try to instantiate generic enum if not found
            ensureEnumInstantiated(enumName);
            if (auto *einfoPtr = findEnumType(enumName); einfoPtr && einfoPtr->isADT) {
                auto &info = *einfoPtr;
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
                adtVal = builder_.CreateInsertValue(adtVal, llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(tag)), 0, "adt.tag");

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
                getOrCreateMeta(result).enum_value_type = enumName;
                return result;
            }
        }
    }

    // Testing intrinsic: verifyCalledWith (#1677)
    if (e->callee == "verifyCalledWith")
        return emitVerifyCalledWithCall(*e);

    // Fast path: pre-emit args[0] once for callee names shared by multiple
    // dispatchers, then route through the same try-chain order to avoid
    // emitting dead IR from earlier dispatchers that don't match the type.
    // The else branch ensures the normal chain is skipped when the fast path
    // handles these names, preventing duplicate emission on fallthrough.
    if (e->args.size() >= 2 && (e->callee == "map" || e->callee == "filter")) {
        llvm::Value *arg0 = emitExpr(*e->args[0]);
        if (auto *v = emitBuiltinResult(*e, arg0))      return v;
        if (auto *v = emitBuiltinOption(*e, arg0))      return v;
        if (auto *v = emitBuiltinIterator(*e, arg0))    return v;
        if (auto *v = emitBuiltinHigherOrder(*e, arg0)) return v;
    } else if (e->args.size() == 2 && e->callee == "take") {
        llvm::Value *arg0 = emitExpr(*e->args[0]);
        if (auto *v = emitBuiltinIterator(*e, arg0))    return v;
        if (auto *v = emitBuiltinCollection(*e, arg0))  return v;
    } else {
        // Dispatch to language-builtin helpers (Pattern B: no @native registry)
        if (auto *v = emitBuiltinResult(*e))      return v;
        if (auto *v = emitBuiltinOption(*e))      return v;
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

    // Dispatch to self-registering stdlib module helpers
    for (auto &mod : StdlibRegistry::instance().packages()) {
        if (auto *v = mod.dispatch(*this, *e)) return v;
    }

    // Struct constructor
    if (auto *ri = findRecordType(e->callee)) {
        if (deprecated_types_.count(e->callee))
            emitDeprecationWarning(e->callee);
        return emitRecordConstructor(*ri, e->callee, e->args);
    }

    // Try indirect call via variable (function pointer / lambda)
    if (llvm::AllocaInst *varPtr = findVar(e->callee)) {
        auto *fnInfo = lookupFnTypeInfo(varPtr);
        if (fnInfo) {
            auto info = *fnInfo;

            std::vector<llvm::Value*> argVals;
            argVals.reserve(e->args.size());
            for (size_t i = 0; i < e->args.size(); ++i) {
                // #1179: propagate Option<T> inner from callee param so None()/none args resolve correctly.
                llvm::Type *argHintInner = nullptr;
                if (i < info.paramTypes.size() && isOptionType(info.paramTypes[i]))
                    argHintInner = llvm::cast<llvm::StructType>(info.paramTypes[i])->getElementType(1);
                OptionNoneHintGuard argHint(*this, argHintInner);
                argVals.push_back(emitExpr(*e->args[i]));
            }

            auto ucTemps = wrapFnTypedArgs(argVals, info.paramTypeNames);

            llvm::Value *loaded = builder_.CreateLoad(ptrTy_, varPtr, e->callee + ".fn");
            llvm::Value *result = emitLambdaCall(loaded, info, argVals, "indirect_call");

            if (info.returnFnTypeInfo)
                getOrCreateMeta(result).fn_type_info = *info.returnFnTypeInfo;
            if (!info.returnTypeName.empty())
                propagateTypeMeta(info.returnTypeName, result);

            releaseUniformClosureTemps(ucTemps);
            return result;
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
            baseName.resize(ltPos);
            typeArgs = splitTypeArgs(argsStr);
        }

        if (generic_fn_templates_.count(baseName)) {
            // Explicit type args path: keep legacy single-template behavior
            // for now. Cycle 2+ will extend this to pick the right overload
            // when multiple templates share the same arity.
            if (!typeArgs.empty()) {
                instantiateGenericFn(baseName, /*templateIndex=*/0, typeArgs);
                std::string fullName = mangleGenericFnName(
                    baseName, /*templateIndex=*/0, typeArgs);
                return emitUserFnCall(fullName, e->args);
            }

            // Inferred type args path: multi-candidate overload resolution.
            if (!functions_.count(baseName)) {
                auto resolution = resolveGenericOverload(baseName, e->args);
                if (resolution) {
                    instantiateGenericFn(baseName, resolution->templateIndex,
                                         resolution->typeArgs);
                    std::string fullName = mangleGenericFnName(
                        baseName, resolution->templateIndex,
                        resolution->typeArgs);
                    return emitUserFnCall(fullName, e->args);
                }
            }
        }
    }

    // Generic dispatch for @native("libname") functions not covered by
    // hardcoded stdlib dispatch tables. Placed after struct/lambda/generic
    // resolution but before user function fallback, and falls through on
    // type mismatch so user-defined overloads with the same name still work.
    if (auto *v = emitGenericNativeCall(*e)) return v;

    if (!e->named_args.empty())
        codegenError("named arguments are only supported for builtin functions");

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

        if (isAnyType(args[i]->getType()) &&
            llvm::isa<llvm::StructType>(info.paramTypes[i]) &&
            findRecordInfoForType(
                llvm::cast<llvm::StructType>(info.paramTypes[i]))) {
            // #1797: any → record unwrap on function call argument coercion.
            std::string paramName =
                i < info.paramTypeNames.size() ? info.paramTypeNames[i]
                                               : findRecordTypeName(
                                                     llvm::cast<llvm::StructType>(
                                                         info.paramTypes[i]));
            args[i] = unwrapFromAny(args[i], info.paramTypes[i], paramName);
            continue;
        }

        if (i < info.paramTypeNames.size()) {
            std::string resolvedPName = resolveTypeAlias(info.paramTypeNames[i]);
            if (isUnionType(resolvedPName)) {
                args[i] = wrapInUnion(args[i], resolvedPName);
                continue;
            }
        }

        codegenError(context + ": argument " + std::to_string(i) + " type mismatch");
    }

    return args;
}

llvm::Value *CodeGen::emitLambdaCall(llvm::Value *lambdaVal, const FnTypeInfo &info,
                                      std::vector<llvm::Value*> args, const std::string &name) {
    args = coerceCallArgs(info, std::move(args), "lambda call");

    auto annotateResult = [&](llvm::Value *result) -> llvm::Value * {
        if (info.returnFnTypeInfo)
            getOrCreateMeta(result).fn_type_info = *info.returnFnTypeInfo;
        if (!info.returnTypeName.empty())
            propagateTypeMeta(info.returnTypeName, result);
        return result;
    };

    if (info.isUniformClosure) {
        // Uniform closure: {thunk_ptr, env_ptr}
        auto *ucTy = getUniformClosureTy();
        auto *thunkField = builder_.CreateStructGEP(
            ucTy, lambdaVal, 0, "uc.thunk_field");
        auto *thunkPtr = builder_.CreateLoad(ptrTy_, thunkField, "uc.thunk");
        auto *envField = builder_.CreateStructGEP(
            ucTy, lambdaVal, 1, "uc.env_field");
        auto *envPtr = builder_.CreateLoad(ptrTy_, envField, "uc.env");

        std::vector<llvm::Value*> callArgs = args;
        callArgs.push_back(envPtr);
        std::vector<llvm::Type*> callTypes = info.paramTypes;
        callTypes.push_back(ptrTy_);

        auto *ft = llvm::FunctionType::get(info.returnType, callTypes, false);
        if (info.returnType->isVoidTy())
            return builder_.CreateCall(ft, thunkPtr, callArgs);
        return annotateResult(builder_.CreateCall(ft, thunkPtr, callArgs, name));
    }

    if (info.capturedVars.empty()) {
        llvm::FunctionType *ft = llvm::FunctionType::get(
            info.returnType, info.paramTypes, false);
        if (info.returnType->isVoidTy())
            return builder_.CreateCall(ft, lambdaVal, args);
        return annotateResult(builder_.CreateCall(ft, lambdaVal, args, name));
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
                closureTy, lambdaVal, static_cast<unsigned>(i + 1), "lcall.cap." + std::to_string(i));
            llvm::Value *capVal = builder_.CreateLoad(
                info.capturedTypes[i], capField, "lcall.cap_val." + std::to_string(i));
            fullArgs.push_back(capVal);
            allParamTypes.push_back(info.capturedTypes[i]);
        }

        llvm::FunctionType *ft = llvm::FunctionType::get(
            info.returnType, allParamTypes, false);
        if (info.returnType->isVoidTy())
            return builder_.CreateCall(ft, fnPtr, fullArgs);
        return annotateResult(builder_.CreateCall(ft, fnPtr, fullArgs, name));
    }
}


// ===== Shared Result-wrapping helpers =====

llvm::Value *CodeGen::emitResultBranch(llvm::Value *isErr, llvm::StructType *resTy,
                                        llvm::function_ref<llvm::Value*()> buildOk,
                                        llvm::function_ref<llvm::Value*()> buildErr) {
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

bool CodeGen::isNativeConstant(const std::string &name) {
    return StdlibRegistry::instance().constants().count(name);
}

llvm::Value *CodeGen::emitNativeConstant(const std::string &name) {
    auto &constants = StdlibRegistry::instance().constants();
    auto it = constants.find(name);
    if (it == constants.end())
        codegenError("unknown native constant: " + name);
    switch (it->second.kind) {
    case NativeConstantKind::Value:    return llvm::ConstantFP::get(f64Ty_, it->second.value);
    case NativeConstantKind::Infinity: return llvm::ConstantFP::getInfinity(f64Ty_);
    case NativeConstantKind::NaN:      return llvm::ConstantFP::getNaN(f64Ty_);
    }
    llvm_unreachable("unhandled NativeConstantKind");
}

} // namespace ry
