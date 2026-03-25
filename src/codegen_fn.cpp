#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>

void CodeGen::registerResourceByTypeName(const std::string &typeName, llvm::Value *val) {
    if (typeName == "TcpListener") tcp_listener_values_.insert(val);
    else if (typeName == "TcpStream")   tcp_stream_values_.insert(val);
    else if (typeName == "TlsStream")   tls_stream_values_.insert(val);
    else if (typeName == "HttpRequest")  http_request_values_.insert(val);
    else if (typeName == "HttpResponse") http_response_values_.insert(val);
    else if (typeName == "HttpClientResponse") http_client_response_values_.insert(val);
}

void CodeGen::emitStmt(AwaitStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;
    emitCoverage(s.loc);

    auto awaitExpr = std::make_unique<AwaitExpr>();
    awaitExpr->operand = std::move(s.operand);
    auto node = std::make_unique<ExprNode>();
    node->data = std::move(awaitExpr);
    node->loc = s.loc;
    (void)emitExpr(*node);
}

void CodeGen::emitStmt(std::unique_ptr<SelectStmt> &s) {
    if (s->loc.isValid()) current_loc_ = s->loc;
    emitCoverage(s->loc);

    llvm::FunctionType *beginTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
    llvm::FunctionType *addRecvTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {ptrTy_, ptrTy_, ptrTy_}, false);
    llvm::FunctionType *addRecvOptTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {ptrTy_, ptrTy_, ptrTy_, ptrTy_}, false);
    llvm::FunctionType *addSendTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {ptrTy_, ptrTy_, ptrTy_}, false);
    llvm::FunctionType *waitTy = llvm::FunctionType::get(i64Ty_, {ptrTy_, i64Ty_, i64Ty_, i64Ty_}, false);
    llvm::FunctionType *destroyTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);

    llvm::FunctionCallee beginFn = mod_->getOrInsertFunction("__ry_select_begin", beginTy);
    llvm::FunctionCallee addRecvFn = mod_->getOrInsertFunction("__ry_select_add_recv", addRecvTy);
    llvm::FunctionCallee addRecvOptFn = mod_->getOrInsertFunction("__ry_select_add_recv_opt", addRecvOptTy);
    llvm::FunctionCallee addSendFn = mod_->getOrInsertFunction("__ry_select_add_send", addSendTy);
    llvm::FunctionCallee waitFn = mod_->getOrInsertFunction("__ry_select_wait", waitTy);
    llvm::FunctionCallee destroyFn = mod_->getOrInsertFunction("__ry_select_destroy", destroyTy);

    llvm::Value *state = builder_.CreateCall(
        beginFn, {llvm::ConstantInt::get(i64Ty_, static_cast<int64_t>(s->cases.size()))}, "select_state");

    struct CaseCodegenInfo {
        llvm::Type *recvTy = nullptr;
        llvm::AllocaInst *recvSlot = nullptr;
        llvm::AllocaInst *recvOptFlagSlot = nullptr;
        std::string recvName;
        SelectRecvMode recvMode = SelectRecvMode::Strict;
    };
    std::vector<CaseCodegenInfo> caseInfos(s->cases.size());

    for (size_t i = 0; i < s->cases.size(); ++i) {
        std::visit([&](auto &selectCase) {
            using T = std::decay_t<decltype(selectCase)>;
            if constexpr (std::is_same_v<T, SelectRecvCase>) {
                if (selectCase.loc.isValid()) current_loc_ = selectCase.loc;
                llvm::Value *channelVal = emitExpr(*selectCase.channel);
                llvm::Type *elemTy = getChannelElementType(channelVal);
                if (!elemTy)
                    codegenError(selectCase.loc,
                        selectCase.mode == SelectRecvMode::Optional
                            ? "select recv_opt requires Channel<T>"
                            : "select recv requires Channel<T>");

                llvm::Value *outPtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));
                if (!elemTy->isVoidTy()) {
                    caseInfos[i].recvSlot = builder_.CreateAlloca(elemTy, nullptr, "select_recv");
                    if (selectCase.mode == SelectRecvMode::Optional)
                        builder_.CreateStore(llvm::Constant::getNullValue(elemTy), caseInfos[i].recvSlot);
                    outPtr = caseInfos[i].recvSlot;
                }
                caseInfos[i].recvTy = elemTy;
                caseInfos[i].recvName = selectCase.name;
                caseInfos[i].recvMode = selectCase.mode;
                if (selectCase.mode == SelectRecvMode::Optional) {
                    caseInfos[i].recvOptFlagSlot = builder_.CreateAlloca(i1Ty_, nullptr, "select_recv_opt_flag");
                    builder_.CreateCall(addRecvOptFn, {state, channelVal, outPtr, caseInfos[i].recvOptFlagSlot});
                } else {
                    builder_.CreateCall(addRecvFn, {state, channelVal, outPtr});
                }
            } else if constexpr (std::is_same_v<T, SelectSendCase>) {
                if (selectCase.loc.isValid()) current_loc_ = selectCase.loc;
                llvm::Value *channelVal = emitExpr(*selectCase.channel);
                llvm::Type *elemTy = getChannelElementType(channelVal);
                if (!elemTy)
                    codegenError(selectCase.loc, "select send requires Channel<T>");
                llvm::Value *valueVal = emitExpr(*selectCase.value);
                if (valueVal->getType() != elemTy)
                    codegenError(selectCase.loc, "select send value type does not match channel element type");

                llvm::Value *valuePtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));
                if (!elemTy->isVoidTy()) {
                    llvm::AllocaInst *valueSlot = builder_.CreateAlloca(elemTy, nullptr, "select_send");
                    builder_.CreateStore(valueVal, valueSlot);
                    valuePtr = valueSlot;
                }
                builder_.CreateCall(addSendFn, {state, channelVal, valuePtr});
            }
        }, s->cases[i]);
    }

    const int64_t elseIndex = s->else_body.empty() ? -1 : static_cast<int64_t>(s->cases.size());
    const int64_t timeoutIndex = s->timeout_ms ? static_cast<int64_t>(s->cases.size()) : -1;
    llvm::Value *timeoutMsVal = llvm::ConstantInt::get(i64Ty_, -1);
    if (s->timeout_ms) {
        if (s->timeout_loc.isValid()) current_loc_ = s->timeout_loc;
        timeoutMsVal = emitExpr(*s->timeout_ms);
        if (timeoutMsVal->getType() != i64Ty_)
            codegenError(s->timeout_loc, "select timeout requires int milliseconds");
    }
    llvm::Value *selected = builder_.CreateCall(
        waitFn,
        {state,
         llvm::ConstantInt::get(i64Ty_, elseIndex),
         timeoutMsVal,
         llvm::ConstantInt::get(i64Ty_, timeoutIndex)},
        "select_index");
    builder_.CreateCall(destroyFn, {state});

    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "select.end", fn_);
    llvm::BasicBlock *defaultBB = s->else_body.empty() ? mergeBB : llvm::BasicBlock::Create(*ctx_, "select.else", fn_);
    llvm::BasicBlock *timeoutBB = s->timeout_body.empty() ? mergeBB : llvm::BasicBlock::Create(*ctx_, "select.timeout", fn_);
    llvm::SwitchInst *switchInst = builder_.CreateSwitch(selected, mergeBB,
        static_cast<unsigned>(s->cases.size() + (s->else_body.empty() ? 0 : 1) + (s->timeout_body.empty() ? 0 : 1)));

    for (size_t i = 0; i < s->cases.size(); ++i) {
        llvm::BasicBlock *caseBB = llvm::BasicBlock::Create(*ctx_, "select.case", fn_);
        switchInst->addCase(
            llvm::cast<llvm::ConstantInt>(llvm::ConstantInt::get(i64Ty_, static_cast<int64_t>(i))),
            caseBB);
        builder_.SetInsertPoint(caseBB);

        pushScope();
        std::visit([&](auto &selectCase) {
            using T = std::decay_t<decltype(selectCase)>;
            if constexpr (std::is_same_v<T, SelectRecvCase>) {
                if (selectCase.name != "_") {
                    llvm::Type *recvTy = caseInfos[i].recvTy;
                    llvm::Value *boundValue = nullptr;
                    llvm::Type *boundTy = nullptr;
                    if (caseInfos[i].recvMode == SelectRecvMode::Optional) {
                        llvm::Value *hasValue = builder_.CreateLoad(i1Ty_, caseInfos[i].recvOptFlagSlot, "select_recv_opt_has");
                        if (recvTy->isVoidTy()) {
                            boundValue = hasValue;
                            boundTy = i1Ty_;
                        } else {
                            llvm::StructType *optTy = getOptionType(recvTy);
                            llvm::Value *recvVal = builder_.CreateLoad(recvTy, caseInfos[i].recvSlot, "select_received");
                            llvm::Value *optInner = builder_.CreateSelect(
                                hasValue, recvVal, llvm::UndefValue::get(recvTy), "select_recv_opt_inner");
                            llvm::Value *opt = llvm::UndefValue::get(optTy);
                            opt = builder_.CreateInsertValue(opt, hasValue, 0, "select_recv_opt_has_field");
                            opt = builder_.CreateInsertValue(opt, optInner, 1, "select_recv_opt_value_field");
                            boundValue = opt;
                            boundTy = optTy;
                        }
                    } else {
                        if (recvTy->isVoidTy())
                            codegenError(selectCase.loc, "select recv binding requires a non-Unit channel value");
                        boundValue = builder_.CreateLoad(recvTy, caseInfos[i].recvSlot, "select_received");
                        boundTy = recvTy;
                    }

                    llvm::AllocaInst *varPtr = getOrCreateVar(selectCase.name, boundTy);
                    builder_.CreateStore(boundValue, varPtr);
                    immutable_scope_stack_.back().insert(selectCase.name);
                }
                for (auto &stmt : selectCase.body)
                    std::visit([this](auto &st) { emitStmt(st); }, stmt);
            } else if constexpr (std::is_same_v<T, SelectSendCase>) {
                for (auto &stmt : selectCase.body)
                    std::visit([this](auto &st) { emitStmt(st); }, stmt);
            }
        }, s->cases[i]);
        popScope();

        if (!builder_.GetInsertBlock()->getTerminator())
            builder_.CreateBr(mergeBB);
    }

    if (!s->else_body.empty()) {
        switchInst->addCase(
            llvm::cast<llvm::ConstantInt>(llvm::ConstantInt::get(i64Ty_, elseIndex)),
            defaultBB);
        builder_.SetInsertPoint(defaultBB);
        pushScope();
        for (auto &stmt : s->else_body)
            std::visit([this](auto &st) { emitStmt(st); }, stmt);
        popScope();
        if (!builder_.GetInsertBlock()->getTerminator())
            builder_.CreateBr(mergeBB);
    }

    if (!s->timeout_body.empty()) {
        switchInst->addCase(
            llvm::cast<llvm::ConstantInt>(llvm::ConstantInt::get(i64Ty_, timeoutIndex)),
            timeoutBB);
        builder_.SetInsertPoint(timeoutBB);
        pushScope();
        for (auto &stmt : s->timeout_body)
            std::visit([this](auto &st) { emitStmt(st); }, stmt);
        popScope();
        if (!builder_.GetInsertBlock()->getTerminator())
            builder_.CreateBr(mergeBB);
    }

    builder_.SetInsertPoint(mergeBB);
}

void CodeGen::emitStmt(ReturnStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;
    emitCoverage(s.loc);
    if (!s.value) {
        if (!fn_->getReturnType()->isVoidTy())
            codegenError("return without value in non-Unit function");
        builder_.CreateRetVoid();
    } else {
        llvm::Value *val = emitExpr(*s.value);
        llvm::Type *retTy = fn_->getReturnType();
        if (retTy->isVoidTy())
            codegenError("cannot return a value from Unit function '" +
                                     std::string(fn_->getName()) + "'");
        if (val->getType() != retTy) {
            if (isUnionType(current_fn_return_type_)) {
                val = wrapInUnion(val, current_fn_return_type_);
            } else {
                // Try tuple element coercion (e.g., Option<int> none → Option<Error>)
                auto *retST = llvm::dyn_cast<llvm::StructType>(retTy);
                auto *valST = llvm::dyn_cast<llvm::StructType>(val->getType());
                if (!retST || !valST || retST->getNumElements() != valST->getNumElements())
                    codegenError("return type mismatch");

                // Find which elements need coercion
                bool needsCoercion = false;
                for (unsigned i = 0; i < retST->getNumElements(); ++i) {
                    if (valST->getElementType(i) != retST->getElementType(i)) {
                        if (!(isOptionType(valST->getElementType(i)) &&
                              isOptionType(retST->getElementType(i))))
                            codegenError("return type mismatch");
                        needsCoercion = true;
                    }
                }
                if (needsCoercion) {
                    llvm::Value *coerced = llvm::UndefValue::get(retTy);
                    for (unsigned i = 0; i < retST->getNumElements(); ++i) {
                        llvm::Value *elem = builder_.CreateExtractValue(val, i);
                        if (valST->getElementType(i) != retST->getElementType(i))
                            elem = buildNoneValue(retST->getElementType(i));
                        coerced = builder_.CreateInsertValue(coerced, elem, i);
                    }
                    val = coerced;
                }
            }
        }

        // Emit ensure checks (postconditions) before return
        emitEnsureChecks(val);

        builder_.CreateRet(val);
    }
    llvm::BasicBlock *deadBB = llvm::BasicBlock::Create(*ctx_, "dead", fn_);
    builder_.SetInsertPoint(deadBB);
}

// ===== B5: FnStmt using FnScope RAII =====

void CodeGen::emitStmt(std::unique_ptr<FnStmt> &s) {
    if (s->loc.isValid()) current_loc_ = s->loc;
    emitCoverage(s->loc);
    if (hasDirective(s->directives, "native")) {
        if (s->is_async)
            codegenError("async native functions are not supported");
        if (hasDirective(s->directives, "deprecated"))
            deprecated_functions_.insert(s->name);

        // Register argument count for native function overload
        native_fn_arg_counts_[s->name].push_back(s->params.size());
        return;
    }

    std::vector<llvm::Type*> paramTypes;
    for (auto &p : s->params)
        paramTypes.push_back(resolveType(p.type));
    llvm::Type *bodyRetTy = resolveType(s->return_type);
    std::string exposedReturnTypeName = s->is_async ? "Task<" + s->return_type + ">" : s->return_type;
    llvm::Type *exposedRetTy = s->is_async ? resolveType(exposedReturnTypeName) : bodyRetTy;

    // Check for duplicate signatures
    auto &overloads = functions_[s->name];
    for (auto &entry : overloads) {
        if (entry.paramTypes == paramTypes) {
            if (entry.func->getReturnType() == exposedRetTy)
                codegenError("function '" + s->name +
                    "' is already defined with the same signature");
            else
                codegenError("function '" + s->name +
                    "': overloads with same parameter types but different return types");
        }
    }

    // LLVM IR function name: first overload uses original name, subsequent use name.N
    std::string irName = s->name;
    if (!overloads.empty())
        irName = s->name + "." + std::to_string(overloads.size());

    llvm::FunctionType *ft = llvm::FunctionType::get(exposedRetTy, paramTypes, false);
    llvm::Function *func = llvm::Function::Create(
        ft, llvm::Function::ExternalLinkage, irName, *mod_);

    std::vector<std::string> paramTypeNames;
    for (auto &p : s->params)
        paramTypeNames.push_back(p.type);
    overloads.push_back({func, paramTypes, paramTypeNames, exposedReturnTypeName});

    if (hasDirective(s->directives, "deprecated"))
        deprecated_functions_.insert(s->name);

    auto emitFunctionBody = [&](llvm::Function *targetFunc, llvm::Type *retTy,
                                const std::string &returnTypeName, const std::string &fnNameForErrors) {
        FnScope guard(*this);
        fn_ = targetFunc;
        current_fn_return_type_ = returnTypeName;
        pushScope();

        llvm::BasicBlock *entry = llvm::BasicBlock::Create(*ctx_, "entry", targetFunc);
        builder_.SetInsertPoint(entry);

        unsigned idx = 0;
        for (auto &arg : targetFunc->args()) {
            arg.setName(s->params[idx].name);
            llvm::AllocaInst *alloca = builder_.CreateAlloca(
                paramTypes[idx], nullptr, s->params[idx].name);
            builder_.CreateStore(&arg, alloca);
            scope_stack_.back()[s->params[idx].name] = alloca;
            // Track list element type for list parameters
            const std::string &ptype = s->params[idx].type;
            if (ptype.size() > 5 && ptype.substr(0, 5) == "List<" && ptype.back() == '>') {
                std::string inner = ptype.substr(5, ptype.size() - 6);
                list_element_types_[alloca] = resolveType(inner);
            }
            // Track set element type for set parameters
            if (ptype.size() > 4 && ptype.substr(0, 4) == "Set<" && ptype.back() == '>') {
                std::string inner = ptype.substr(4, ptype.size() - 5);
                set_element_types_[alloca] = resolveType(inner);
            }
            // Track enum type for enum parameters
            if (enum_types_.count(ptype)) {
                enum_value_types_[alloca] = ptype;
            }
            // Track map key/value types for map parameters
            if (ptype.size() > 4 && ptype.substr(0, 4) == "Map<" && ptype.back() == '>') {
                auto [kTy, vTy] = parseMapTypeAnnotation(ptype);
                if (kTy) map_key_types_[alloca] = kTy;
                if (vTy) map_value_types_[alloca] = vTy;
            }
            if (ptype.size() > 5 && ptype.substr(0, 5) == "Task<" && ptype.back() == '>') {
                std::string inner = ptype.substr(5, ptype.size() - 6);
                task_result_types_[alloca] = resolveType(inner);
            }
            if (ptype.size() > 8 && ptype.substr(0, 8) == "Channel<" && ptype.back() == '>') {
                std::string inner = ptype.substr(8, ptype.size() - 9);
                channel_element_types_[alloca] = resolveType(inner);
            }
            registerResourceByTypeName(ptype, alloca);
            // Track fn type info and constraint check (shared alias resolution)
            {
                std::string resolvedPtype = resolveTypeAlias(ptype);
                if (resolvedPtype.size() > 3 && resolvedPtype.substr(0, 3) == "fn(") {
                    fn_type_info_[alloca] = parseFnTypeAnnotation(resolvedPtype);
                }
                auto constraint = parseTypeConstraint(resolvedPtype);
                if (constraint) {
                    type_constraints_[alloca] = *constraint;
                    llvm::Value *argVal = builder_.CreateLoad(
                        paramTypes[idx], alloca, s->params[idx].name + ".load");
                    emitConstraintCheck(argVal, *constraint, s->params[idx].name);
                } else {
                    // Track union type only for non-literal unions
                    if (isUnionType(ptype))
                        union_value_types_[alloca] = normalizeUnionType(ptype);
                }
            }
            ++idx;
        }

        // Emit require checks (preconditions)
        for (int i = 0; i < static_cast<int>(s->preconditions.size()); ++i)
            emitContractCheck("require", s->name, s->preconditions[i]);

        // Set up postcondition context
        current_postconditions_ = s->postconditions.empty() ? nullptr : &s->postconditions;
        ensure_bindings_ = s->ensure_bindings.empty() ? nullptr : &s->ensure_bindings;

        for (auto &stmt : s->body)
            std::visit([this](auto &st) { emitStmt(st); }, stmt);

        if (!builder_.GetInsertBlock()->getTerminator()) {
            llvm::Value *defaultRet = nullptr;
            if (retTy->isVoidTy()) {
                // no default value needed
            } else if (retTy == i64Ty_) {
                defaultRet = llvm::ConstantInt::get(i64Ty_, 0);
            } else if (retTy == i8Ty_) {
                defaultRet = llvm::ConstantInt::get(i8Ty_, 0);
            } else if (retTy == f64Ty_) {
                defaultRet = llvm::ConstantFP::get(f64Ty_, 0.0);
            } else if (retTy == i1Ty_) {
                defaultRet = llvm::ConstantInt::get(i1Ty_, 0);
            } else if (retTy == ptrTy_) {
                defaultRet = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));
            } else if (llvm::isa<llvm::StructType>(retTy)) {
                defaultRet = llvm::UndefValue::get(retTy);
            }

            // Emit ensure checks on implicit return path
            if (defaultRet)
                emitEnsureChecks(defaultRet);

            if (retTy->isVoidTy())
                builder_.CreateRetVoid();
            else if (defaultRet)
                builder_.CreateRet(defaultRet);
            else
                builder_.CreateRet(llvm::UndefValue::get(retTy));
        }

        std::string err;
        llvm::raw_string_ostream errStream(err);
        if (llvm::verifyFunction(*targetFunc, &errStream))
            codegenError("IR verify error in function '" + fnNameForErrors + "': " + err);
    };

    if (!s->is_async) {
        emitFunctionBody(func, bodyRetTy, s->return_type, s->name);
        return;
    }

    llvm::FunctionType *bodyFt = llvm::FunctionType::get(bodyRetTy, paramTypes, false);
    llvm::Function *bodyFunc = llvm::Function::Create(
        bodyFt, llvm::Function::InternalLinkage, irName + ".__async_body", *mod_);
    emitFunctionBody(bodyFunc, bodyRetTy, s->return_type, s->name);

    std::vector<llvm::Type*> envFields = paramTypes;
    if (envFields.empty())
        envFields.push_back(i8Ty_);
    llvm::StructType *envTy = llvm::StructType::get(*ctx_, envFields);

    llvm::FunctionType *thunkTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {ptrTy_, ptrTy_}, false);
    llvm::Function *thunk = llvm::Function::Create(
        thunkTy, llvm::Function::InternalLinkage,
        "__ry_async." + std::to_string(lambda_counter_++), *mod_);

    // FnScope destructor restores fn_, scope_stack_, immutable_scope_stack_, builder_ insert point,
    // and contract state (current_postconditions_, result_alloca_, in_ensure_context_, old_value_map_)

    {
        FnScope guard(*this);
        fn_ = thunk;
        llvm::BasicBlock *entry = llvm::BasicBlock::Create(*ctx_, "entry", thunk);
        builder_.SetInsertPoint(entry);

        auto argIt = thunk->arg_begin();
        llvm::Value *envRaw = &*argIt++;
        envRaw->setName("env_raw");
        llvm::Value *outRaw = &*argIt;
        outRaw->setName("out_raw");

        llvm::Value *typedEnv = builder_.CreateBitCast(envRaw, ptrTy_, "async_env_typed");
        std::vector<llvm::Value*> thunkArgs;
        for (size_t i = 0; i < paramTypes.size(); ++i) {
            llvm::Value *argField = builder_.CreateStructGEP(
                envTy, typedEnv, i, "async_arg_field." + std::to_string(i));
            thunkArgs.push_back(builder_.CreateLoad(paramTypes[i], argField, "async_arg." + std::to_string(i)));
        }

        llvm::Value *result = builder_.CreateCall(bodyFunc, thunkArgs, bodyRetTy->isVoidTy() ? "" : "async_result");
        if (!bodyRetTy->isVoidTy()) {
            llvm::Value *outTyped = builder_.CreateBitCast(outRaw, ptrTy_, "async_out_typed");
            builder_.CreateStore(result, outTyped);
        }
        builder_.CreateRetVoid();
    }

    {
        FnScope guard(*this);
        fn_ = func;
        current_fn_return_type_ = exposedReturnTypeName;
        llvm::BasicBlock *entry = llvm::BasicBlock::Create(*ctx_, "entry", func);
        builder_.SetInsertPoint(entry);

        llvm::FunctionType *mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
        llvm::FunctionCallee mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
        const llvm::DataLayout &dl = mod_->getDataLayout();
        llvm::Value *envPtr = builder_.CreateCall(
            mallocFn,
            {llvm::ConstantInt::get(i64Ty_, std::max<uint64_t>(1, dl.getTypeAllocSize(envTy)))},
            "async_env");

        if (paramTypes.empty()) {
            llvm::Value *dummyField = builder_.CreateStructGEP(envTy, envPtr, 0, "async_env_dummy");
            builder_.CreateStore(llvm::ConstantInt::get(i8Ty_, 0), dummyField);
        } else {
            size_t idx = 0;
            for (auto &arg : func->args()) {
                llvm::Value *argField = builder_.CreateStructGEP(
                    envTy, envPtr, idx++, "async_env_arg");
                builder_.CreateStore(&arg, argField);
            }
        }

        llvm::FunctionType *spawnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, i64Ty_}, false);
        llvm::FunctionCallee spawnFn = mod_->getOrInsertFunction("__ry_task_spawn", spawnTy);
        llvm::Value *task = builder_.CreateCall(
            spawnFn,
            {
                builder_.CreateBitCast(thunk, ptrTy_),
                envPtr,
                llvm::ConstantInt::get(i64Ty_, bodyRetTy->isVoidTy() ? 0 : dl.getTypeAllocSize(bodyRetTy))
            },
            "task");
        builder_.CreateRet(task);

        std::string err;
        llvm::raw_string_ostream errStream(err);
        if (llvm::verifyFunction(*func, &errStream))
            codegenError("IR verify error in function '" + s->name + "': " + err);
    }
}

// ===== Contract helpers =====

void CodeGen::emitContractCheck(const std::string &kind, const std::string &fn_name,
                                 const ExprPtr &cond) {
    llvm::Value *condVal = emitExpr(*cond);
    condVal = toBool(condVal);

    std::string errName = ".contract_err_" + std::to_string(contract_err_counter_++);
    std::string suffix = (kind == "invariant") ? "" : "()";
    std::string preposition = (kind == "invariant") ? " for " : " in ";
    std::string msg = "Contract violation: " + kind + " failed" + preposition + fn_name + suffix + "\n";

    llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, kind + ".fail", fn_);
    llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, kind + ".ok", fn_);

    builder_.CreateCondBr(condVal, nextBB, failBB);

    builder_.SetInsertPoint(failBB);
    emitRuntimeError(msg, errName);

    builder_.SetInsertPoint(nextBB);
}

void CodeGen::emitInvariantCheck(const std::string &typeName, const StructInfo &info,
                                  llvm::Value *structVal) {
    if (info.invariants.empty()) return;

    pushScope();
    for (unsigned f = 0; f < info.fields.size(); ++f) {
        llvm::Type *fieldTy = info.llvmType->getElementType(f);
        llvm::AllocaInst *fieldAlloca = builder_.CreateAlloca(fieldTy, nullptr, info.fields[f].name);
        llvm::Value *fieldVal = builder_.CreateExtractValue(structVal, f, info.fields[f].name + "_val");
        builder_.CreateStore(fieldVal, fieldAlloca);
        scope_stack_.back()[info.fields[f].name] = fieldAlloca;
    }
    for (int i = 0; i < static_cast<int>(info.invariants.size()); ++i)
        emitContractCheck("invariant", typeName, info.invariants[i]);
    popScope();
}

void CodeGen::emitEnsureChecks(llvm::Value *retVal) {
    if (!current_postconditions_ || current_postconditions_->empty() || !ensure_bindings_)
        return;
    pushScope();
    auto &bindings = *ensure_bindings_;
    if (bindings.size() == 1) {
        llvm::AllocaInst *alloca = builder_.CreateAlloca(retVal->getType(), nullptr, bindings[0]);
        builder_.CreateStore(retVal, alloca);
        scope_stack_.back()[bindings[0]] = alloca;
        immutable_scope_stack_.back().insert(bindings[0]);
    } else {
        auto *structTy = llvm::dyn_cast<llvm::StructType>(retVal->getType());
        if (!structTy || !structTy->isLiteral() || structTy->getNumElements() != bindings.size())
            codegenError("ensure destructuring requires tuple return; binding count does not match tuple element count");
        for (unsigned i = 0; i < bindings.size(); ++i) {
            llvm::Value *elem = builder_.CreateExtractValue(retVal, i);
            llvm::AllocaInst *alloca = builder_.CreateAlloca(elem->getType(), nullptr, bindings[i]);
            builder_.CreateStore(elem, alloca);
            scope_stack_.back()[bindings[i]] = alloca;
            immutable_scope_stack_.back().insert(bindings[i]);
        }
    }
    in_ensure_context_ = true;
    std::string fnName = fn_->getName().str();
    for (int i = 0; i < static_cast<int>(current_postconditions_->size()); ++i)
        emitContractCheck("ensure", fnName, (*current_postconditions_)[i]);
    in_ensure_context_ = false;
    popScope();
}

void CodeGen::instantiateGenericEnum(const std::string &fullName, const std::string &baseName,
                                      const std::vector<std::string> &typeArgs) {
    if (enum_types_.count(fullName))
        return; // already instantiated

    auto it = generic_enum_templates_.find(baseName);
    if (it == generic_enum_templates_.end())
        codegenError("unknown generic enum: " + baseName);

    auto &tmpl = it->second;
    if (typeArgs.size() != tmpl.typeParams.size())
        codegenError("generic enum '" + baseName + "' expects " +
            std::to_string(tmpl.typeParams.size()) + " type parameters");

    // Build type parameter mapping
    std::unordered_map<std::string, std::string> typeMap;
    for (size_t i = 0; i < tmpl.typeParams.size(); ++i)
        typeMap[tmpl.typeParams[i]] = typeArgs[i];

    // Create a concrete EnumStmt by substituting type parameters
    EnumInfo info;
    info.name = fullName;
    info.variantCount = tmpl.variants.size();

    bool hasADT = false;
    std::vector<llvm::Constant*> nameStrings;
    for (size_t i = 0; i < tmpl.variants.size(); ++i) {
        auto &v = tmpl.variants[i];
        info.variants[v.name] = static_cast<int64_t>(i);
        llvm::Constant *str = builder_.CreateGlobalString(
            v.name, ".enum_" + fullName + "_" + v.name);
        nameStrings.push_back(str);

        if (!v.field_types.empty()) {
            hasADT = true;
            VariantFieldInfo vfi;
            for (auto &ft : v.field_types) {
                std::string resolved = ft;
                auto mit = typeMap.find(ft);
                if (mit != typeMap.end()) resolved = mit->second;
                vfi.fieldTypes.push_back(resolveType(resolved));
                vfi.fieldTypeNames.push_back(resolved);
            }
            info.variantFields[v.name] = std::move(vfi);
        }
    }
    info.isADT = hasADT;

    auto *arrTy = llvm::ArrayType::get(ptrTy_, tmpl.variants.size());
    auto *init = llvm::ConstantArray::get(arrTy, nameStrings);
    auto *gv = new llvm::GlobalVariable(
        *mod_, arrTy, true, llvm::GlobalValue::PrivateLinkage,
        init, ".enum_names_" + fullName);
    info.nameArray = gv;

    if (hasADT) {
        const llvm::DataLayout &dl = mod_->getDataLayout();
        size_t maxPayload = 0;
        for (auto &[vname, vfi] : info.variantFields) {
            size_t payloadSize = 0;
            for (auto *ty : vfi.fieldTypes) {
                uint64_t align = dl.getABITypeAlign(ty).value();
                payloadSize = (payloadSize + align - 1) / align * align;
                payloadSize += dl.getTypeAllocSize(ty);
            }
            if (payloadSize > maxPayload) maxPayload = payloadSize;
        }
        info.maxPayloadSize = maxPayload;
        llvm::Type *payloadTy = llvm::ArrayType::get(
            llvm::Type::getInt8Ty(*ctx_), maxPayload > 0 ? maxPayload : 1);
        info.adtType = llvm::StructType::create(
            *ctx_, {i64Ty_, payloadTy}, "enum." + fullName);
    }

    enum_types_[fullName] = std::move(info);
}
