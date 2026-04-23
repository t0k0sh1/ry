#include "ry/codegen.hpp"

#include <cmath>

namespace ry {

// ===== B4: emitUserFnCall =====

llvm::Function *CodeGen::resolveOverload(const std::string &callee,
                                          const std::vector<ExprPtr> &args,
                                          std::vector<llvm::Value*> &outArgVals) {
    auto *overloadsPtr = findFunction(callee);
    if (!overloadsPtr) {
        if (native_fn_sigs_.count(callee) || native_lib_index_.count(callee)) {
            codegenError("no matching overload for @native function '" + callee + "'");
        } else {
            codegenError("undefined function: " + callee +
                " (hint: if this function is defined later in the file, "
                "add an explicit return type to enable forward references)");
        }
    }

    auto &overloads = *overloadsPtr;

    // Identify which args are None literals
    std::vector<bool> isNone(args.size(), false);
    for (size_t i = 0; i < args.size(); ++i) {
        if (isNoneLiteral(*args[i]))
            isNone[i] = true;
    }

    // Emit non-None args to get their types
    std::vector<llvm::Value*> emittedArgs(args.size(), nullptr);
    for (size_t i = 0; i < args.size(); ++i) {
        if (!isNone[i])
            emittedArgs[i] = emitExpr(*args[i]);
    }

    struct RankedCandidate {
        OverloadEntry *entry;
        int exactMatches = 0;
        int subtypeMatches = 0;
        int wideningMatches = 0;
        int unionMatches = 0;
        int anyMatches = 0;
        int defaultsUsed = 0;
    };

    auto isBetterCandidate = [](const RankedCandidate &lhs, const RankedCandidate &rhs) {
        if (lhs.exactMatches != rhs.exactMatches)
            return lhs.exactMatches > rhs.exactMatches;
        if (lhs.subtypeMatches != rhs.subtypeMatches)
            return lhs.subtypeMatches > rhs.subtypeMatches;
        if (lhs.wideningMatches != rhs.wideningMatches)
            return lhs.wideningMatches > rhs.wideningMatches;
        if (lhs.unionMatches != rhs.unionMatches)
            return lhs.unionMatches > rhs.unionMatches;
        if (lhs.anyMatches != rhs.anyMatches)
            return lhs.anyMatches < rhs.anyMatches;
        if (lhs.defaultsUsed != rhs.defaultsUsed)
            return lhs.defaultsUsed < rhs.defaultsUsed;
        return false;
    };

    // Filter and rank candidates
    std::vector<RankedCandidate> candidates;
    for (auto &entry : overloads) {
        if (args.size() < entry.minArity || args.size() > entry.paramTypes.size())
            continue;
        bool match = true;
        RankedCandidate candidate{&entry, 0, 0, 0, 0, 0,
                                  static_cast<int>(entry.paramTypes.size() - args.size())};
        for (size_t i = 0; i < args.size(); ++i) {
            std::string resolvedParamTypeName =
                i < entry.paramTypeNames.size() ? resolveTypeAlias(entry.paramTypeNames[i]) : "";
            if (isNone[i]) {
                if (!isOptionType(entry.paramTypes[i])) { match = false; break; }
                continue;
            }

            if (emittedArgs[i]->getType() == entry.paramTypes[i]) {
                candidate.exactMatches++;
                continue;
            }

            if (auto *argST = llvm::dyn_cast<llvm::StructType>(emittedArgs[i]->getType())) {
                if (auto *paramST = llvm::dyn_cast<llvm::StructType>(entry.paramTypes[i])) {
                    if (isSubtypeOf(argST->getName().str(), paramST->getName().str())) {
                        candidate.subtypeMatches++;
                        continue;
                    }
                }
            }

            if (isWideningConversion(emittedArgs[i], entry.paramTypes[i], resolvedParamTypeName)) {
                candidate.wideningMatches++;
                continue;
            }

            if (isAnyType(entry.paramTypes[i])) { // NOLINT(bugprone-branch-clone)
                // Match: any type accepts all primitives; wrapping deferred to arg building
                candidate.anyMatches++;
            } else if (isUnionType(resolvedParamTypeName)) {
                std::string norm = flattenUnionWithAliases(entry.paramTypeNames[i]);
                auto uIt = union_type_info_.find(norm);
                if (uIt != union_type_info_.end()) {
                    bool found = false;
                    for (auto *ct : uIt->second.componentTypes) {
                        if (ct == emittedArgs[i]->getType()) { found = true; break; }
                    }
                    if (!found) { match = false; break; }
                    candidate.unionMatches++;
                } else { match = false; break; }
            } else if (isAnyType(emittedArgs[i]->getType()) &&
                       canAnyHoldType(entry.paramTypes[i])) {
                // Matching a concrete parameter from an any-typed value requires runtime unwrap,
                // so treat it with the same low specificity as an any fallback.
                candidate.anyMatches++;
            } else { match = false; break; }
        }
        if (match)
            candidates.push_back(candidate);
    }

    if (candidates.empty())
        codegenError("no matching overload for '" + callee + "'");

    RankedCandidate *best = &candidates[0];
    bool ambiguous = false;
    for (size_t i = 1; i < candidates.size(); ++i) {
        if (isBetterCandidate(candidates[i], *best)) {
            best = &candidates[i];
            ambiguous = false;
        } else if (!isBetterCandidate(*best, candidates[i])) {
            ambiguous = true;
        }
    }

    if (ambiguous)
        codegenError("ambiguous call to '" + callee + "'");

    auto *chosen = best->entry;

    // Build final arg values (fill in None args with proper Option type, wrap union args)
    outArgVals.clear();
    for (size_t i = 0; i < args.size(); ++i) {
        std::string resolvedParamTypeName =
            i < chosen->paramTypeNames.size() ? resolveTypeAlias(chosen->paramTypeNames[i]) : "";
        if (isNone[i]) {
            outArgVals.push_back(buildNoneValue(chosen->paramTypes[i]));
        } else if (emittedArgs[i]->getType() != chosen->paramTypes[i] &&
                   isWideningConversion(emittedArgs[i], chosen->paramTypes[i], resolvedParamTypeName)) {
            outArgVals.push_back(emitWideningConversion(emittedArgs[i], chosen->paramTypes[i]));
        } else if (auto *sliced = tryEmitSubtypeCoerce(emittedArgs[i], chosen->paramTypes[i])) {
            outArgVals.push_back(sliced);
        } else if (emittedArgs[i]->getType() != chosen->paramTypes[i] &&
                   isAnyType(chosen->paramTypes[i])) {
            outArgVals.push_back(wrapInAny(emittedArgs[i]));
        } else if (isAnyType(emittedArgs[i]->getType()) &&
                   emittedArgs[i]->getType() != chosen->paramTypes[i]) {
            outArgVals.push_back(unwrapFromAny(emittedArgs[i], chosen->paramTypes[i]));
        } else if (emittedArgs[i]->getType() != chosen->paramTypes[i] &&
                   isUnionType(resolvedParamTypeName)) {
            outArgVals.push_back(wrapInUnion(emittedArgs[i], resolvedParamTypeName));
        } else {
            outArgVals.push_back(emittedArgs[i]);
        }
    }

    // Fill in default values for omitted parameters
    for (size_t i = args.size(); i < chosen->paramTypes.size(); ++i) {
        if (isNoneLiteral(*chosen->defaultValues[i])) {
            outArgVals.push_back(buildNoneValue(chosen->paramTypes[i]));
            continue;
        }
        llvm::Value *defVal = emitExpr(*chosen->defaultValues[i]);
        std::string resolvedParamTypeName =
            i < chosen->paramTypeNames.size() ? resolveTypeAlias(chosen->paramTypeNames[i]) : "";
        if (defVal->getType() != chosen->paramTypes[i] &&
            isWideningConversion(defVal, chosen->paramTypes[i], resolvedParamTypeName)) {
            outArgVals.push_back(emitWideningConversion(defVal, chosen->paramTypes[i]));
        } else if (auto *sliced = tryEmitSubtypeCoerce(defVal, chosen->paramTypes[i])) {
            outArgVals.push_back(sliced);
        } else if (defVal->getType() != chosen->paramTypes[i] &&
                   isAnyType(chosen->paramTypes[i])) {
            outArgVals.push_back(wrapInAny(defVal));
        } else if (isAnyType(defVal->getType()) &&
                   defVal->getType() != chosen->paramTypes[i]) {
            outArgVals.push_back(unwrapFromAny(defVal, chosen->paramTypes[i]));
        } else if (defVal->getType() != chosen->paramTypes[i] &&
                   isUnionType(resolvedParamTypeName)) {
            outArgVals.push_back(wrapInUnion(defVal, resolvedParamTypeName));
        } else {
            outArgVals.push_back(defVal);
        }
    }

    return chosen->func;
}

llvm::Value *CodeGen::emitUserFnCall(const std::string &callee, const std::vector<ExprPtr> &args) {
    if (deprecated_functions_.count(callee))
        emitDeprecationWarning(callee);
    std::vector<llvm::Value*> argVals;
    llvm::Function *fn = resolveOverload(callee, args, argVals);

    // Find the matching overload entry (single scan for constraints + result type)
    OverloadEntry *matchedEntry = nullptr;
    auto *fnOverloads = findFunction(callee);
    if (fnOverloads) {
        for (auto &entry : *fnOverloads) {
            if (entry.func == fn) { matchedEntry = &entry; break; }
        }
    }

    // Append captured variable values for nested functions with closures
    if (matchedEntry && !matchedEntry->capturedNames.empty()) {
        for (auto &capName : matchedEntry->capturedNames) {
            llvm::AllocaInst *capAlloca = findVar(capName);
            if (!capAlloca)
                codegenError("captured variable '" + capName + "' not found in calling scope");
            llvm::Value *capVal = builder_.CreateLoad(
                capAlloca->getAllocatedType(), capAlloca, capName + ".cap_pass");
            argVals.push_back(capVal);
        }
    }

    // Wrap function-typed arguments as uniform closures for function(...) params
    std::vector<llvm::Value*> uniformClosureTemps;
    if (matchedEntry)
        uniformClosureTemps = wrapFnTypedArgs(argVals, matchedEntry->paramTypeNames);

    // ARC: retain arguments that are ARC-managed before passing to callee
    for (auto *argVal : argVals)
        tryRetainArcSource(argVal);

    // Check literal/range constraints on arguments at call site
    if (matchedEntry) {
        for (size_t i = 0; i < matchedEntry->paramTypeNames.size() && i < argVals.size(); ++i) {
            std::string resolvedPtype = resolveTypeAlias(matchedEntry->paramTypeNames[i]);
            auto constraint = parseTypeConstraint(resolvedPtype);
            if (constraint) {
                std::string paramName = fn->getArg(static_cast<unsigned>(i))->getName().str();
                emitConstraintCheck(argVals[i], *constraint, paramName);
            }
        }
    }

    auto bindContractValue = [&](const std::string &name, llvm::Value *val,
                                 const std::string *typeName) {
        llvm::AllocaInst *alloca = builder_.CreateAlloca(val->getType(), nullptr, name);
        builder_.CreateStore(val, alloca);
        scope_stack_.back()[name] = alloca;
        immutable_scope_stack_.back().insert(name);
        if (!typeName) return;

        std::string resolvedType = resolveTypeAlias(*typeName);
        if (isLowLevelTypeName(resolvedType))
            getOrCreateMeta(alloca).low_level_type_name = resolvedType;
        if (isFunctionTypeName(resolvedType))
            getOrCreateMeta(alloca).fn_type_info = parseFnTypeAnnotation(resolvedType);
        auto constraint = parseTypeConstraint(resolvedType);
        if (constraint)
            getOrCreateMeta(alloca).type_constraint = *constraint;
        else if (isUnionType(resolvedType))
            storeFlattenedUnionMeta(alloca, *typeName);
    };

    auto bindMockContractParams = [&]() {
        if (!matchedEntry) return;
        for (size_t i = 0; i < matchedEntry->paramNames.size() && i < argVals.size(); ++i) {
            const std::string *typeName = i < matchedEntry->paramTypeNames.size()
                ? &matchedEntry->paramTypeNames[i]
                : nullptr;
            bindContractValue(matchedEntry->paramNames[i], argVals[i], typeName);
        }
    };

    auto emitMockRequireChecks = [&]() {
        if (!matchedEntry || !matchedEntry->preconditions || matchedEntry->preconditions->empty())
            return;
        pushScope();
        bindMockContractParams();
        for (const auto &precondition : *matchedEntry->preconditions)
            emitContractCheck("require", callee, precondition);
        popScope();
    };

    auto emitMockEnsureChecks = [&](llvm::Value *retVal) {
        if (!matchedEntry || !matchedEntry->postconditions || matchedEntry->postconditions->empty() ||
            !matchedEntry->ensureBindings)
            return;

        pushScope();
        bindMockContractParams();
        auto &bindings = *matchedEntry->ensureBindings;
        if (bindings.size() == 1) {
            bindContractValue(bindings[0], retVal, nullptr);
        } else {
            auto *structTy = llvm::dyn_cast<llvm::StructType>(retVal->getType());
            if (!structTy || structTy->isLiteral() || structTy->getNumElements() != bindings.size())
                codegenError("ensure destructuring requires tuple return; binding count does not match tuple element count");
            for (unsigned i = 0; i < bindings.size(); ++i) {
                llvm::Value *elem = builder_.CreateExtractValue(retVal, i);
                bindContractValue(bindings[i], elem, nullptr);
            }
        }

        bool savedInEnsureContext = in_ensure_context_;
        in_ensure_context_ = true;
        for (const auto &postcondition : *matchedEntry->postconditions)
            emitContractCheck("ensure", callee, postcondition);
        in_ensure_context_ = savedInEnsureContext;
        popScope();
    };

    // In test mode, inject mock dispatch only for functions targeted by mock()
    if (test_mode_ && mocked_functions_.count(callee)) {
        llvm::FunctionType *mockGetTy = llvm::FunctionType::get(ptrTy_, {ptrTy_}, false);
        llvm::FunctionCallee mockGetFn = mod_->getOrInsertFunction("__ry_mock_get", mockGetTy);
        llvm::FunctionType *mockIncTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
        llvm::FunctionCallee mockIncFn = mod_->getOrInsertFunction("__ry_mock_increment_call", mockIncTy);

        auto &nameStr = mock_name_strings_[callee];
        if (!nameStr) nameStr = cachedGlobalString(callee, ".mock." + callee);
        llvm::Value *mockPtr = builder_.CreateCall(mockGetFn, {nameStr}, "mock_ptr");
        llvm::Value *nullPtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));
        llvm::Value *isMocked = builder_.CreateICmpNE(mockPtr, nullPtr, "is_mocked");

        llvm::BasicBlock *mockBB = llvm::BasicBlock::Create(*ctx_, "mock_bb", fn_);
        llvm::BasicBlock *origBB = llvm::BasicBlock::Create(*ctx_, "orig_bb", fn_);
        llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "merge_bb", fn_);

        builder_.CreateCondBr(isMocked, mockBB, origBB);

        // Mock path
        builder_.SetInsertPoint(mockBB);
        emitMockRequireChecks();
        builder_.CreateCall(mockIncFn, {nameStr});
        llvm::FunctionType *fnTy = fn->getFunctionType();
        if (fn->getReturnType()->isVoidTy()) {
            builder_.CreateCall(fnTy, mockPtr, argVals);
            builder_.CreateBr(mergeBB);

            // Original path (void case)
            builder_.SetInsertPoint(origBB);
            builder_.CreateCall(fn, argVals);
            builder_.CreateBr(mergeBB);

            builder_.SetInsertPoint(mergeBB);
            releaseUniformClosureTemps(uniformClosureTemps);
            return nullptr;
        }

        llvm::Value *mockResult = builder_.CreateCall(fnTy, mockPtr, argVals, "mock_result");
        emitMockEnsureChecks(mockResult);
        builder_.CreateBr(mergeBB);
        llvm::BasicBlock *mockEndBB = builder_.GetInsertBlock();

        // Original path
        builder_.SetInsertPoint(origBB);
        llvm::Value *origResult = builder_.CreateCall(fn, argVals, "orig_result");
        builder_.CreateBr(mergeBB);
        llvm::BasicBlock *origEndBB = builder_.GetInsertBlock();

        // Merge
        builder_.SetInsertPoint(mergeBB);
        llvm::PHINode *phi = builder_.CreatePHI(fn->getReturnType(), 2, "call_result");
        phi->addIncoming(mockResult, mockEndBB);
        phi->addIncoming(origResult, origEndBB);
        propagateReturnTypeMeta(matchedEntry, phi);
        propagateReturnFnTypeMeta(matchedEntry, fn, phi);
        releaseUniformClosureTemps(uniformClosureTemps);
        return phi;
    }

    if (fn->getReturnType()->isVoidTy()) {
        builder_.CreateCall(fn, argVals);
        releaseUniformClosureTemps(uniformClosureTemps);
        return nullptr;
    }
    llvm::Value *callResult = builder_.CreateCall(fn, argVals, "calltmp");
    releaseUniformClosureTemps(uniformClosureTemps);

    propagateReturnTypeMeta(matchedEntry, callResult);

    propagateReturnFnTypeMeta(matchedEntry, fn, callResult);

    return callResult;
}

void CodeGen::emitStmt(CallStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;
    emitCoverage(s.loc);
    validateDirectives(s.directives);
    if (!s.named_args.empty() && builtins_.find(s.callee) == builtins_.end())
        codegenError(s.loc, "named arguments are only supported for builtin functions");
    if (s.callee == "describe") {
        emitDescribeCall(s);
        return;
    }
    if (s.callee == "it") {
        emitItCall(s);
        return;
    }
    if (s.callee == "mock") {
        emitMockCall(s);
        return;
    }
    if (s.callee == "fail") {
        emitFailCall(s);
        return;
    }
    auto it = builtins_.find(s.callee);
    if (it != builtins_.end()) {
        it->second(s.args, s.named_args);
        return;
    }
    auto sit = record_types_.find(s.callee);
    if (sit != record_types_.end()) {
        emitRecordConstructor(sit->second, s.callee, s.args);
        return;
    }
    // Intercept collection operations and route through CallExpr emitter
    if (!s.args.empty()) {
        bool intercept = false;
        if (auto *ve = std::get_if<VariableExpr>(&s.args[0]->data)) {
            llvm::AllocaInst *alloca = findVar(ve->name);
            if (alloca) {
                bool isList = getListElementType(alloca) != nullptr;
                bool isSet = !isList && getSetElementType(alloca) != nullptr;
                bool isMap = !isList && !isSet && getMapKeyType(alloca) != nullptr;
                size_t nargs = s.args.size();

                if (isList && // NOLINT(bugprone-branch-clone)
                    ((s.callee == "append" && nargs == 2) ||
                     (s.callee == "append!" && nargs == 2) ||
                     (s.callee == "pop" && nargs == 1) ||
                     (s.callee == "insert" && nargs == 3) ||
                     (s.callee == "remove_at" && nargs == 2) ||
                     (s.callee == "remove" && nargs == 2) ||
                     (s.callee == "sort!" && (nargs == 1 || nargs == 2)) ||
                     (s.callee == "reverse!" && nargs == 1))) { // NOLINT(bugprone-branch-clone)
                    intercept = true;
                } else if (isSet &&
                    ((s.callee == "add" && nargs == 2) ||
                     (s.callee == "remove" && nargs == 2) ||
                     (s.callee == "union" && nargs == 2) ||
                     (s.callee == "intersection" && nargs == 2) ||
                     (s.callee == "difference" && nargs == 2) ||
                     (s.callee == "symmetric_difference" && nargs == 2) ||
                     (s.callee == "is_subset" && nargs == 2) ||
                     (s.callee == "is_superset" && nargs == 2))) {
                    intercept = true;
                } else if (isMap &&
                    ((s.callee == "remove" && nargs == 2) ||
                     (s.callee == "items" && nargs == 1) ||
                     (s.callee == "get" && (nargs == 2 || nargs == 3)))) {
                    intercept = true;
                }
            }
        }
        if (intercept) {
            auto ce = std::make_unique<CallExpr>();
            ce->callee = s.callee;
            ce->args = std::move(s.args);
            emitExprVariant(ce);
            return;
        }
    }
    if (tryCallOperator(s.callee, s.args))
        return;
    // Route all remaining calls through the unified CallExpr dispatch chain.
    // This covers @native stdlib functions, language builtins (close, range,
    // sleep, etc.), and user-defined functions without a hardcoded whitelist.
    auto ce = std::make_unique<CallExpr>();
    ce->callee = s.callee;
    ce->args = std::move(s.args);
    emitExprVariant(ce);
}

llvm::Value *CodeGen::toBool(llvm::Value *v) {
    if (v->getType() == i1Ty_)
        return v;
    if (v->getType()->isDoubleTy())
        return builder_.CreateFCmpONE(
            v, llvm::ConstantFP::get(f64Ty_, 0.0), "ftobool");
    // Anything else must be a plain integer. ARC-managed ptr-backed values
    // (List, Map, Set, str, iterator, closure, record handle, ...) used to
    // fall through to `icmp ne ptr, ConstantInt::get(ptrTy_, 0)`, which LLVM
    // rendered as `icmp ne ptr, i0 0` and rejected in IR verification (#818).
    // Reject at the frontend with a clearer, type-aware diagnostic.
    if (!v->getType()->isIntegerTy()) {
        if (v->getType() == ptrTy_)
            codegenError(
                "value of this type cannot be used as a boolean condition; "
                "use `length(x) > 0` or `not is_empty(x)` for collections/strings, "
                "or pattern-match Option/Result explicitly");
        codegenError("value of this type cannot be used as a boolean condition");
    }
    return builder_.CreateICmpNE(
        v, llvm::ConstantInt::get(v->getType(), 0), "itobool");
}

// ===== C stdlib function helpers =====

llvm::FunctionCallee CodeGen::getStdlibMalloc() {
    auto ty = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
    return mod_->getOrInsertFunction("malloc", ty);
}

llvm::FunctionCallee CodeGen::getStdlibRealloc() {
    auto ty = llvm::FunctionType::get(ptrTy_, {ptrTy_, i64Ty_}, false);
    return mod_->getOrInsertFunction("realloc", ty);
}

llvm::FunctionCallee CodeGen::getStdlibFree() {
    auto ty = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
    return mod_->getOrInsertFunction("free", ty);
}

llvm::FunctionCallee CodeGen::getStdlibStrlen() {
    auto ty = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
    return mod_->getOrInsertFunction("strlen", ty);
}

llvm::FunctionCallee CodeGen::getStdlibMemcpy() {
    auto ty = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, i64Ty_}, false);
    return mod_->getOrInsertFunction("memcpy", ty);
}

llvm::FunctionCallee CodeGen::getStdlibMemmove() {
    auto ty = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, i64Ty_}, false);
    return mod_->getOrInsertFunction("memmove", ty);
}

llvm::FunctionCallee CodeGen::getStdlibMemset() {
    auto ty = llvm::FunctionType::get(ptrTy_, {ptrTy_, i32Ty_, i64Ty_}, false);
    return mod_->getOrInsertFunction("memset", ty);
}

llvm::FunctionCallee CodeGen::getStdlibMemcmp() {
    auto ty = llvm::FunctionType::get(i32Ty_, {ptrTy_, ptrTy_, i64Ty_}, false);
    return mod_->getOrInsertFunction("memcmp", ty);
}

llvm::FunctionCallee CodeGen::getStdlibStrcmp() {
    auto ty = llvm::FunctionType::get(i32Ty_, {ptrTy_, ptrTy_}, false);
    return mod_->getOrInsertFunction("strcmp", ty);
}

llvm::FunctionCallee CodeGen::getStdlibStrncmp() {
    auto ty = llvm::FunctionType::get(i32Ty_, {ptrTy_, ptrTy_, i64Ty_}, false);
    return mod_->getOrInsertFunction("strncmp", ty);
}

llvm::FunctionCallee CodeGen::getStdlibStrstr() {
    auto ty = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_}, false);
    return mod_->getOrInsertFunction("strstr", ty);
}

llvm::FunctionCallee CodeGen::getStdlibStrcasestr() {
    auto ty = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_}, false);
    return mod_->getOrInsertFunction("strcasestr", ty);
}

llvm::FunctionCallee CodeGen::getStdlibStrncasecmp() {
    auto ty = llvm::FunctionType::get(i32Ty_, {ptrTy_, ptrTy_, i64Ty_}, false);
    return mod_->getOrInsertFunction("strncasecmp", ty);
}

llvm::FunctionCallee CodeGen::getStdlibStrcpy() {
    auto ty = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_}, false);
    return mod_->getOrInsertFunction("strcpy", ty);
}

llvm::FunctionCallee CodeGen::getStdlibStrcat() {
    auto ty = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_}, false);
    return mod_->getOrInsertFunction("strcat", ty);
}

llvm::FunctionCallee CodeGen::getStdlibSnprintf() {
    auto ty = llvm::FunctionType::get(i32Ty_, {ptrTy_, i64Ty_, ptrTy_}, true);
    return mod_->getOrInsertFunction("snprintf", ty);
}

llvm::FunctionCallee CodeGen::getStdlibPrintf() {
    auto ty = llvm::FunctionType::get(i32Ty_, {ptrTy_}, true);
    return mod_->getOrInsertFunction("printf", ty);
}

llvm::FunctionCallee CodeGen::getBufferedPrintf() {
    auto ty = llvm::FunctionType::get(i32Ty_, {ptrTy_}, true);
    return mod_->getOrInsertFunction("__ry_print_printf", ty);
}

llvm::FunctionCallee CodeGen::getSprintPrintf() {
    auto ty = llvm::FunctionType::get(i32Ty_, {ptrTy_}, true);
    return mod_->getOrInsertFunction("__ry_sprint_printf", ty);
}

llvm::FunctionCallee CodeGen::getStdlibExit() {
    auto ty = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), {i32Ty_}, false);
    return mod_->getOrInsertFunction("exit", ty);
}

void CodeGen::emitRuntimeError(const std::string &message, const std::string &globalName,
                                llvm::ArrayRef<llvm::Value *> extraArgs) {
    auto fprintfTy = llvm::FunctionType::get(i32Ty_, {ptrTy_, ptrTy_}, true);
    auto fprintfFn = mod_->getOrInsertFunction("fprintf", fprintfTy);
#ifdef __APPLE__
    const char *stderrName = "__stderrp";
#else
    const char *stderrName = "stderr";
#endif
    auto *stderrGlobal = mod_->getOrInsertGlobal(stderrName, ptrTy_);
    llvm::Value *stderrVal = builder_.CreateLoad(ptrTy_, stderrGlobal, "stderr");
    llvm::Constant *errMsg = cachedGlobalString(message, globalName);
    llvm::SmallVector<llvm::Value *, 4> args = {stderrVal, errMsg};
    args.append(extraArgs.begin(), extraArgs.end());
    builder_.CreateCall(fprintfFn, args);
    auto exitFn = getStdlibExit();
    builder_.CreateCall(exitFn, {llvm::ConstantInt::get(i32Ty_, 1)});
    builder_.CreateUnreachable();
}

void CodeGen::emitBoundsError(llvm::Value *index, llvm::Value *size,
                               const std::string &fmtMsg, const std::string &globalName) {
    emitRuntimeError(fmtMsg, globalName, {index, size});
}

void CodeGen::emitIntZeroDivGuard(llvm::Value *divisor, const std::string &bbPrefix,
                                   const std::string &errMsg) {
    llvm::Value *isZero = builder_.CreateICmpEQ(
        divisor, llvm::ConstantInt::get(divisor->getType(), 0), bbPrefix + "_zero");
    llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, bbPrefix + ".err", fn_);
    llvm::BasicBlock *okBB  = llvm::BasicBlock::Create(*ctx_, bbPrefix + ".ok", fn_);
    builder_.CreateCondBr(isZero, errBB, okBB);
    builder_.SetInsertPoint(errBB);
    emitRuntimeError(errMsg,
                      "." + bbPrefix + "_err_" + std::to_string(arith_zero_err_counter_++));
    builder_.SetInsertPoint(okBB);
}

llvm::Value *CodeGen::emitNegativeIndexWrap(llvm::Value *idx, llvm::Value *wrapBase,
                                              const std::string &prefix) {
    llvm::Value *zero = llvm::ConstantInt::get(i64Ty_, 0);
    llvm::Value *isNeg = builder_.CreateICmpSLT(idx, zero, prefix + "_is_neg");
    llvm::Value *wrapped = builder_.CreateAdd(idx, wrapBase, prefix + "_wrapped");
    return builder_.CreateSelect(isNeg, wrapped, idx, prefix + "_idx");
}

void CodeGen::emitBoundsCheck(llvm::Value *&index, llvm::Value *size,
                               const std::string &errMsg, const std::string &globalName,
                               const std::string &bbPrefix) {
    if (index->getType() == i1Ty_)
        index = builder_.CreateZExt(index, i64Ty_, "idx_ext");

    // Compile-time constant check with negative index wrap-around
    if (auto *ci = llvm::dyn_cast<llvm::ConstantInt>(index)) {
        if (auto *cs = llvm::dyn_cast<llvm::ConstantInt>(size)) {
            int64_t idx = ci->getSExtValue();
            int64_t sz = static_cast<int64_t>(cs->getZExtValue());
            if (idx < 0) idx += sz;
            if (idx < 0 || idx >= sz)
                codegenError("index " + std::to_string(ci->getSExtValue()) +
                             " out of bounds (size " + std::to_string(sz) + ")");
            index = llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(idx));
            return;
        }
    }

    llvm::Value *origIndex = index;
    index = emitNegativeIndexWrap(index, size, bbPrefix);

    llvm::Value *zero = llvm::ConstantInt::get(i64Ty_, 0);
    llvm::Value *negCheck = builder_.CreateICmpSLT(
        index, zero, bbPrefix + "_neg");
    llvm::Value *overCheck = builder_.CreateICmpSGE(index, size, bbPrefix + "_over");
    llvm::Value *oob = builder_.CreateOr(negCheck, overCheck, bbPrefix + "_oob");
    llvm::BasicBlock *oobBB = llvm::BasicBlock::Create(*ctx_, bbPrefix + ".oob", fn_);
    llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, bbPrefix + ".ok", fn_);
    builder_.CreateCondBr(oob, oobBB, okBB);
    builder_.SetInsertPoint(oobBB);
    emitBoundsError(origIndex, size, errMsg, globalName);
    builder_.SetInsertPoint(okBB);
}

llvm::Value *CodeGen::emitCheckedFPToInt(llvm::Value *val, llvm::Type *targetTy,
                                          const std::string &typeName,
                                          const std::string &bbPrefix,
                                          const std::string &siteLabel) {
    assert(val->getType()->isFloatingPointTy());
    assert(targetTy->isIntegerTy());

    // Check in f64 for simplicity; the final cast uses the original value so
    // sub-64-bit targets still see the correct rounding direction.
    llvm::Value *valF64 = (val->getType() == f64Ty_)
        ? val
        : builder_.CreateFPExt(val, f64Ty_, bbPrefix + "_f64ext");

    const bool isUnsigned = isUnsignedLowLevelName(typeName);
    const unsigned bits = targetTy->getIntegerBitWidth();

    // Accept range: signed → [-2^(W-1), 2^(W-1)), unsigned → [0, 2^W).
    // 2^W for W ≤ 64 is exactly representable in f64 (powers of two below
    // 2^1024). INT_MIN values such as -2^63 are exact; INT_MAX like 2^63-1
    // rounds up to 2^W in f64, so we reject the upper bound (half-open).
    double lo = isUnsigned ? 0.0 : -std::ldexp(1.0, static_cast<int>(bits - 1));
    double hi = isUnsigned ? std::ldexp(1.0, static_cast<int>(bits))
                           :  std::ldexp(1.0, static_cast<int>(bits - 1));

    // Unordered comparisons fold the NaN check into the range check: NaN
    // makes both `ULT` and `UGE` true, so NaN / ±inf / out-of-range all
    // hit the failBB with one OR.
    llvm::Value *loC = llvm::ConstantFP::get(f64Ty_, lo);
    llvm::Value *hiC = llvm::ConstantFP::get(f64Ty_, hi);
    llvm::Value *tooLow = builder_.CreateFCmpULT(valF64, loC, bbPrefix + "_lo");
    llvm::Value *tooHigh = builder_.CreateFCmpUGE(valF64, hiC, bbPrefix + "_hi");
    llvm::Value *invalid = builder_.CreateOr(tooLow, tooHigh, bbPrefix + "_invalid");

    llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, bbPrefix + ".fail", fn_);
    llvm::BasicBlock *okBB   = llvm::BasicBlock::Create(*ctx_, bbPrefix + ".ok",   fn_);
    builder_.CreateCondBr(invalid, failBB, okBB);

    builder_.SetInsertPoint(failBB);
    std::string msg = siteLabel.empty()
        ? ("runtime error: cannot convert %g to " + typeName + "\n")
        : ("runtime error: " + siteLabel + ": cannot convert %g to " + typeName + "\n");
    std::string globalName = "." + bbPrefix + "_err_" +
                             std::to_string(fptoi_err_counter_++);
    emitRuntimeError(msg, globalName, {valF64});

    builder_.SetInsertPoint(okBB);
    return isUnsigned
        ? builder_.CreateFPToUI(val, targetTy, bbPrefix)
        : builder_.CreateFPToSI(val, targetTy, bbPrefix);
}

llvm::Value *CodeGen::coerceToLowLevelType(llvm::Value *val, llvm::Type *targetTy,
                                            const std::string &typeName,
                                            const std::string &context,
                                            const std::string &truncName) {
    if (val->getType() == f64Ty_ && targetTy == f32Ty_)
        return builder_.CreateFPTrunc(val, f32Ty_, truncName);

    if (val->getType() == i64Ty_ && (targetTy == i8Ty_ || targetTy == i16Ty_ || targetTy == i32Ty_)) {
        if (auto *ci = llvm::dyn_cast<llvm::ConstantInt>(val)) {
            int64_t v = ci->getSExtValue();
            bool isUnsigned = (!typeName.empty() && typeName[0] == 'u');
            bool outOfRange = false;
            if (targetTy == i8Ty_) {
                outOfRange = isUnsigned ? (v < 0 || v > 255) : (v < INT8_MIN || v > INT8_MAX);
            } else if (targetTy == i16Ty_) {
                outOfRange = isUnsigned ? (v < 0 || v > (int64_t)UINT16_MAX) : (v < INT16_MIN || v > INT16_MAX);
            } else {
                outOfRange = isUnsigned ? (v < 0 || v > (int64_t)UINT32_MAX) : (v < INT32_MIN || v > INT32_MAX);
            }
            if (outOfRange)
                codegenError(typeName + " value out of range" + context + ": " + std::to_string(v));
        }
        return builder_.CreateTrunc(val, targetTy, truncName);
    }

    if (val->getType() == f64Ty_ && targetTy == i64Ty_ &&
        !isLowLevelTypeName(typeName)) {
        return emitCheckedFPToInt(val, i64Ty_, "int", truncName);
    }
    if (val->getType() == i64Ty_ && targetTy == f64Ty_ &&
        !isLowLevelTypeName(typeName)) {
        return builder_.CreateSIToFP(val, f64Ty_, truncName);
    }

    return nullptr;
}

CodeGen::FnTypeInfo *CodeGen::lookupFnTypeInfo(llvm::Value *val) {
    auto *meta = getMeta(val);
    if (meta && meta->fn_type_info)
        return &*meta->fn_type_info;
    return nullptr;
}

} // namespace ry
