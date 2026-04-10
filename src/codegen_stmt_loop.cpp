#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"


namespace ry {

void CodeGen::emitStmt(std::unique_ptr<WhileStmt> &s) {
    emitCoverage(s->loc);
    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "while.cond", fn_);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "while.body", fn_);
    llvm::BasicBlock *endBB  = llvm::BasicBlock::Create(*ctx_, "while.end", fn_);

    builder_.CreateBr(condBB);

    builder_.SetInsertPoint(condBB);
    llvm::Value *cond = emitExpr(*s->condition);
    cond = toBool(cond);
    builder_.CreateCondBr(cond, bodyBB, endBB);

    builder_.SetInsertPoint(bodyBB);
    loop_stack_.push_back({condBB, endBB, scope_stack_.size()});
    pushScope();
    for (auto &stmt : s->body)
        std::visit([this](auto &st) { emitStmt(st); }, stmt);
    popScope();
    loop_stack_.pop_back();
    if (!builder_.GetInsertBlock()->getTerminator())
        builder_.CreateBr(condBB);

    builder_.SetInsertPoint(endBB);
}

void CodeGen::emitStmt(std::unique_ptr<ForStmt> &s) {
    emitCoverage(s->loc);
    current_loc_ = s->loc;
    validateDirectives(s->directives);
    if (hasDirective(s->directives, "parallel")) {
        if (s->var_names.size() > 1)
            codegenError(s->loc, "@parallel for does not support destructuring iteration");

        validateParallelFor(*s);

        llvm::Value *begin = nullptr;
        llvm::Value *end = nullptr;
        llvm::Value *step = llvm::ConstantInt::get(i64Ty_, 1);

        if (auto *rangeExpr = std::get_if<std::unique_ptr<RangeExpr>>(&s->iterable->data)) {
            begin = emitExpr(*(*rangeExpr)->start);
            llvm::Value *inclusiveEnd = emitExpr(*(*rangeExpr)->end);
            end = builder_.CreateAdd(inclusiveEnd, llvm::ConstantInt::get(i64Ty_, 1), "parallel_inclusive_end");
        } else if (auto *callExpr = std::get_if<std::unique_ptr<CallExpr>>(&s->iterable->data)) {
            if ((*callExpr)->callee != "range")
                codegenError(s->loc, "@parallel for only supports range(...) or .. iterables");
            if ((*callExpr)->args.size() < 1 || (*callExpr)->args.size() > 3)
                codegenError(s->loc, "range() takes 1, 2, or 3 arguments");
            if ((*callExpr)->args.size() == 1) {
                begin = llvm::ConstantInt::get(i64Ty_, 0);
                end = emitExpr(*(*callExpr)->args[0]);
            } else {
                begin = emitExpr(*(*callExpr)->args[0]);
                end = emitExpr(*(*callExpr)->args[1]);
            }
            if ((*callExpr)->args.size() == 3)
                step = emitExpr(*(*callExpr)->args[2]);
        } else {
            codegenError(s->loc, "@parallel for only supports range(...) or .. iterables");
        }

        if (begin->getType() != i64Ty_ || end->getType() != i64Ty_ || step->getType() != i64Ty_)
            codegenError(s->loc, "@parallel for requires integer range bounds");

        emitParallelForRange(*s, begin, end, step);
        return;
    }

    // Evaluate iterable
    llvm::Value *iterable = emitExpr(*s->iterable);

    // Check if this is a pointer-backed iterable (list/set/map)
    if (iterable->getType() != ptrTy_)
        codegenError("for loop requires list, set, map, or iterator iterable");

    // Check if iterable is an iterator
    llvm::Type *iterElemTy = getIteratorElementType(iterable);
    if (iterElemTy) {
        llvm::Value *nextFnField = builder_.CreateStructGEP(iteratorHeaderTy_, iterable, 0, "for_iter_nf");
        llvm::Value *nextFnPtr = builder_.CreateLoad(ptrTy_, nextFnField, "for_iter_next_fn");
        llvm::Value *stateField = builder_.CreateStructGEP(iteratorHeaderTy_, iterable, 1, "for_iter_st");
        llvm::Value *statePtr = builder_.CreateLoad(ptrTy_, stateField, "for_iter_state");

        llvm::StructType *optTy = getOptionType(iterElemTy);
        llvm::FunctionType *nextCallTy = llvm::FunctionType::get(optTy, {ptrTy_}, false);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "foriter.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "foriter.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "foriter.end", fn_);

        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *opt = builder_.CreateCall(nextCallTy, nextFnPtr, {statePtr}, "foriter_opt");
        llvm::Value *hasVal = builder_.CreateExtractValue(opt, 0, "foriter_has");
        builder_.CreateCondBr(hasVal, bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        loop_stack_.push_back({condBB, endBB, scope_stack_.size()});
        pushScope();

        llvm::Value *elem = builder_.CreateExtractValue(opt, 1, "foriter_elem");

        if (s->var_names.size() > 1) {
            auto *structTy = llvm::dyn_cast<llvm::StructType>(iterElemTy);
            if (!structTy)
                codegenError("for loop destructuring requires tuple elements");
            emitTupleDestructure(s->var_names, elem, structTy);
        } else {
            llvm::AllocaInst *loopVar = getOrCreateVar(s->var_names[0], iterElemTy);
            builder_.CreateStore(elem, loopVar);
        }

        for (auto &stmt : s->body)
            std::visit([this](auto &st) { emitStmt(st); }, stmt);

        popScope();
        loop_stack_.pop_back();
        if (!builder_.GetInsertBlock()->getTerminator())
            builder_.CreateBr(condBB);

        builder_.SetInsertPoint(endBB);
        return;
    }

    // Multi-variable iteration: for k, v in map  OR  for a, b, c in list_of_tuples
    if (s->var_names.size() > 1) {
        llvm::Type *keyTy = getMapKeyType(iterable);
        llvm::Type *valTy = getMapValueType(iterable);
        if (!keyTy || !valTy) {
            llvm::Type *elemTy = getListElementType(iterable);
            auto *structTy = llvm::dyn_cast_or_null<llvm::StructType>(elemTy);
            if (!structTy)
                codegenError("for loop destructuring requires a list of tuples");

            llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, iterable, 0, "for_len_ptr");
            llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "for_len");
            llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, iterable, 2, "for_data_ptr");
            llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataPtrField, "for_data");

            emitIndexedForLoop(length, s->body, [&](llvm::Value *iCur) {
                llvm::Value *tuplePtr = builder_.CreateGEP(structTy, dataPtr, {iCur}, "for_tuple_ptr");
                llvm::Value *tuple = builder_.CreateLoad(structTy, tuplePtr, "for_tuple");
                emitTupleDestructure(s->var_names, tuple, structTy);
            });
            return;
        }

        // Map iteration: always exactly 2 variables (key, value)
        if (s->var_names.size() != 2)
            codegenError("map iteration requires exactly 2 variables (key, value), got " +
                         std::to_string(s->var_names.size()));

        llvm::Value *lenPtr = builder_.CreateStructGEP(mapHeaderTy_, iterable, 0, "map_len_ptr");
        llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "map_len");
        llvm::Value *keysPtrField = builder_.CreateStructGEP(mapHeaderTy_, iterable, 2, "keys_ptr_field");
        llvm::Value *keysPtr = builder_.CreateLoad(ptrTy_, keysPtrField, "keys_ptr");
        llvm::Value *valsPtrField = builder_.CreateStructGEP(mapHeaderTy_, iterable, 3, "vals_ptr_field");
        llvm::Value *valsPtr = builder_.CreateLoad(ptrTy_, valsPtrField, "vals_ptr");

        emitIndexedForLoop(length, s->body, [&](llvm::Value *iCur) {
            llvm::Value *keyPtr = builder_.CreateGEP(keyTy, keysPtr, {iCur}, "for_key_ptr");
            llvm::Value *key = builder_.CreateLoad(keyTy, keyPtr, "for_key");
            llvm::Value *valPtr = builder_.CreateGEP(valTy, valsPtr, {iCur}, "for_val_ptr");
            llvm::Value *val = builder_.CreateLoad(valTy, valPtr, "for_val");
            llvm::AllocaInst *keyVar = getOrCreateVar(s->var_names[0], keyTy);
            builder_.CreateStore(key, keyVar);
            llvm::AllocaInst *valVar = getOrCreateVar(s->var_names[1], valTy);
            builder_.CreateStore(val, valVar);
        });
        return;
    }

    // Try set first, then list
    llvm::Type *elemTy = getSetElementType(iterable);
    llvm::StructType *headerTy = setHeaderTy_;
    if (!elemTy) {
        elemTy = getListElementType(iterable);
        headerTy = listHeaderTy_;
    }
    if (!elemTy)
        codegenError("cannot determine element type for for loop iterable");

    llvm::Value *lenPtr = builder_.CreateStructGEP(headerTy, iterable, 0, "for_len_ptr");
    llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "for_len");
    llvm::Value *dataPtrField = builder_.CreateStructGEP(headerTy, iterable, 2, "for_data_ptr");
    llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataPtrField, "for_data");

    // Copy list element metadata before entering the loop body to avoid
    // pointer invalidation from unordered_map rehash inside propagateTypeMeta/getOrCreateMeta.
    std::string elemTypeName;
    std::optional<FnTypeInfo> elemFnTypeInfo;
    if (auto *iterMeta = getMeta(iterable)) {
        elemTypeName    = iterMeta->list_elem_type_name;
        elemFnTypeInfo  = iterMeta->list_elem_fn_type_info;
    }
    emitIndexedForLoop(length, s->body, [&](llvm::Value *iCur) {
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, dataPtr, {iCur}, "for_elem_ptr");
        llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "for_elem");
        llvm::AllocaInst *loopVar = getOrCreateVar(s->var_names[0], elemTy);
        builder_.CreateStore(elem, loopVar);
        // Propagate Map/Set/closure element metadata for List<Map>, List<Set>, List<closure>
        if (!elemTypeName.empty())
            propagateTypeMeta(elemTypeName, loopVar);
        if (elemFnTypeInfo)
            getOrCreateMeta(loopVar).fn_type_info = *elemFnTypeInfo;
    });
}

void CodeGen::emitTupleDestructure(const std::vector<std::string> &var_names,
                                    llvm::Value *tupleVal, llvm::StructType *structTy) {
    if (structTy->getNumElements() != var_names.size())
        codegenError("for loop destructuring: expected " +
                     std::to_string(var_names.size()) +
                     "-element tuple, but got " +
                     std::to_string(structTy->getNumElements()) +
                     " elements");
    for (size_t i = 0; i < var_names.size(); ++i) {
        if (var_names[i] == "_") continue;
        llvm::Value *v = builder_.CreateExtractValue(tupleVal, i, "for_elem_" + std::to_string(i));
        llvm::AllocaInst *var = getOrCreateVar(var_names[i], structTy->getElementType(i));
        builder_.CreateStore(v, var);
    }
}



void CodeGen::emitIndexedForLoop(llvm::Value *length,
                                  std::vector<StmtNode> &body,
                                  std::function<void(llvm::Value *iCur)> bindVars) {
    llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "for_i");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "for.cond", fn_);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "for.body", fn_);
    llvm::BasicBlock *stepBB = llvm::BasicBlock::Create(*ctx_, "for.step", fn_);
    llvm::BasicBlock *endBB  = llvm::BasicBlock::Create(*ctx_, "for.end", fn_);

    builder_.CreateBr(condBB);
    builder_.SetInsertPoint(condBB);
    llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "i");
    llvm::Value *cond = builder_.CreateICmpSLT(iVal, length, "for_cond");
    builder_.CreateCondBr(cond, bodyBB, endBB);

    builder_.SetInsertPoint(bodyBB);
    loop_stack_.push_back({stepBB, endBB, scope_stack_.size()});
    pushScope();

    llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "i_cur");
    bindVars(iCur);

    for (auto &stmt : body)
        std::visit([this](auto &st) { emitStmt(st); }, stmt);

    popScope();
    loop_stack_.pop_back();
    if (!builder_.GetInsertBlock()->getTerminator())
        builder_.CreateBr(stepBB);

    builder_.SetInsertPoint(stepBB);
    llvm::Value *iNext = builder_.CreateAdd(
        builder_.CreateLoad(i64Ty_, iVar, "i_step"), llvm::ConstantInt::get(i64Ty_, 1), "i_next");
    builder_.CreateStore(iNext, iVar);
    builder_.CreateBr(condBB);

    builder_.SetInsertPoint(endBB);
}

void CodeGen::emitStmt(BreakStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;
    emitCoverage(s.loc);
    if (loop_stack_.empty())
        codegenError("break outside of loop");
    // Release ARC vars from current scope down to loop's scope depth
    size_t loopDepth = std::get<2>(loop_stack_.back());
    emitScopeCleanupToDepth(loopDepth);
    builder_.CreateBr(std::get<1>(loop_stack_.back()));
    // Create unreachable block for subsequent code
    llvm::BasicBlock *deadBB = llvm::BasicBlock::Create(*ctx_, "break.dead", fn_);
    builder_.SetInsertPoint(deadBB);
}

void CodeGen::emitStmt(ContinueStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;
    emitCoverage(s.loc);
    if (loop_stack_.empty())
        codegenError("continue outside of loop");
    // Release ARC vars from current scope down to loop's scope depth
    size_t loopDepth = std::get<2>(loop_stack_.back());
    emitScopeCleanupToDepth(loopDepth);
    builder_.CreateBr(std::get<0>(loop_stack_.back()));
    llvm::BasicBlock *deadBB = llvm::BasicBlock::Create(*ctx_, "continue.dead", fn_);
    builder_.SetInsertPoint(deadBB);
}

void CodeGen::emitStmt(EllipsisStmt &) {
    // no-op: intentionally does nothing
}

void CodeGen::validateParallelFor(const ForStmt &s) {
    std::vector<std::unordered_set<std::string>> localScopes(1);
    for (const auto &name : s.var_names)
        localScopes.back().insert(name);

    auto isLocal = [&](const std::string &name) {
        for (auto it = localScopes.rbegin(); it != localScopes.rend(); ++it) {
            if (it->count(name))
                return true;
        }
        return false;
    };

    std::function<void(const std::vector<StmtNode>&)> scanBlock;
    std::function<void(const StmtNode&)> scanStmt;

    scanBlock = [&](const std::vector<StmtNode> &body) {
        localScopes.push_back({});
        for (const auto &stmt : body)
            scanStmt(stmt);
        localScopes.pop_back();
    };

    scanStmt = [&](const StmtNode &stmt) {
        std::visit([&](const auto &node) {
            using T = std::decay_t<decltype(node)>;
            if constexpr (std::is_same_v<T, AssignStmt>) {
                if (!isLocal(node.name)) {
                    // Check if this is a first assignment (new local) or outer mutation
                    // If the variable already exists in the outer codegen scope, it's outer mutation
                    if (findVar(node.name))
                        codegenError(s.loc, "parallel for cannot assign to outer variable '" + node.name + "'");
                    // Top-level module globals (#817) are also outer variables
                    // from a parallel-for's perspective — mutating them would
                    // introduce a data race. Only plain assignments count as
                    // mutation: explicit local declarations (`x: T = ...` or
                    // `@const x = ...`) inside a parallel-for body are a
                    // new local that happens to share a name with a module
                    // global and should be allowed to shadow it.
                    if (!node.type_annotation && !hasDirective(node.directives, "const") &&
                        findModuleGlobal(node.name))
                        codegenError(s.loc, "parallel for cannot assign to outer variable '" + node.name + "'");
                    // Otherwise it's a new local variable — register it
                    localScopes.back().insert(node.name);
                }
            } else if constexpr (std::is_same_v<T, TupleDestructStmt>) {
                for (const auto &name : node.names) {
                    if (name != "_")
                        localScopes.back().insert(name);
                }
            } else if constexpr (std::is_same_v<T, IndexAssignStmt>) {
                codegenError(node.loc, "parallel for does not allow indexed assignment");
            } else if constexpr (std::is_same_v<T, FieldAssignStmt>) {
                codegenError(node.loc, "parallel for does not allow field assignment");
            } else if constexpr (std::is_same_v<T, BreakStmt>) {
                codegenError(node.loc, "parallel for does not allow break");
            } else if constexpr (std::is_same_v<T, ContinueStmt>) {
                codegenError(node.loc, "parallel for does not allow continue");
            } else if constexpr (std::is_same_v<T, std::unique_ptr<IfStmt>>) {
                scanBlock(node->branch.body);
                scanBlock(node->else_body);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<WhenCondStmt>>) {
                for (const auto &arm : node->arms)
                    scanBlock(arm.body);
                scanBlock(node->else_body);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<WhileStmt>>) {
                scanBlock(node->body);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<ForStmt>>) {
                // Seed the nested loop's induction variables into the local
                // scope BEFORE scanning the body, otherwise assignments to
                // them (e.g. `for g in xs: g = ...`) are incorrectly
                // classified as outer/module-global mutations when a
                // same-named top-level binding exists (#817 follow-up).
                localScopes.push_back({});
                for (const auto &name : node->var_names) {
                    if (name != "_")
                        localScopes.back().insert(name);
                }
                for (const auto &innerStmt : node->body)
                    scanStmt(innerStmt);
                localScopes.pop_back();
            } else if constexpr (std::is_same_v<T, std::unique_ptr<MatchStmt>>) {
                for (const auto &arm : node->arms)
                    scanBlock(arm.body);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<FnStmt>>) {
                codegenError(node->loc, "parallel for does not allow nested function definitions");
            }
        }, stmt);
    };

    for (const auto &stmt : s.body)
        scanStmt(stmt);
}

void CodeGen::emitParallelForRange(ForStmt &s, llvm::Value *begin, llvm::Value *end, llvm::Value *step) {
    std::vector<std::pair<std::string, llvm::AllocaInst*>> captures;
    std::unordered_set<std::string> seen;
    for (auto scopeIt = scope_stack_.rbegin(); scopeIt != scope_stack_.rend(); ++scopeIt) {
        for (const auto &[name, alloca] : *scopeIt) {
            if (name == s.var_names[0] || seen.count(name))
                continue;
            seen.insert(name);
            captures.push_back({name, alloca});
        }
    }

    std::vector<llvm::Type*> envFields;
    if (captures.empty())
        envFields.push_back(i8Ty_);
    else
        for (const auto &[_, alloca] : captures)
            envFields.push_back(alloca->getAllocatedType());
    llvm::StructType *envTy = llvm::StructType::get(*ctx_, envFields);

    llvm::FunctionType *mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
    llvm::FunctionCallee mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t envSize = std::max<uint64_t>(1, dl.getTypeAllocSize(envTy));
    llvm::Value *envPtr = builder_.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty_, envSize)}, "parallel_env");

    if (captures.empty()) {
        llvm::Value *dummyField = builder_.CreateStructGEP(envTy, envPtr, 0, "parallel_env_dummy");
        builder_.CreateStore(llvm::ConstantInt::get(i8Ty_, 0), dummyField);
    } else {
        for (size_t i = 0; i < captures.size(); ++i) {
            llvm::Value *fieldPtr = builder_.CreateStructGEP(envTy, envPtr, i, "parallel_env_field");
            llvm::AllocaInst *src = captures[i].second;
            builder_.CreateStore(
                builder_.CreateLoad(src->getAllocatedType(), src, captures[i].first + ".par_cap"),
                fieldPtr);
        }
    }

    llvm::FunctionType *thunkTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {ptrTy_, i64Ty_, i64Ty_, i64Ty_}, false);
    llvm::Function *thunk = llvm::Function::Create(
        thunkTy, llvm::Function::InternalLinkage,
        "__ry_parallel_for." + std::to_string(lambda_counter_++), *mod_);

    {
        FnScope guard(*this);
        fn_ = thunk;
        pushScope();

        llvm::BasicBlock *entryBB = llvm::BasicBlock::Create(*ctx_, "entry", thunk);
        builder_.SetInsertPoint(entryBB);

        auto argIt = thunk->arg_begin();
        llvm::Value *envRaw = &*argIt++;
        envRaw->setName("env_raw");
        llvm::Value *chunkBegin = &*argIt++;
        chunkBegin->setName("chunk_begin");
        llvm::Value *chunkEnd = &*argIt++;
        chunkEnd->setName("chunk_end");
        llvm::Value *stepArg = &*argIt;
        stepArg->setName("step");

        llvm::Value *typedEnv = builder_.CreateBitCast(envRaw, ptrTy_, "parallel_env_typed");

        if (!captures.empty()) {
            for (size_t i = 0; i < captures.size(); ++i) {
                const auto &[name, src] = captures[i];
                llvm::Type *capTy = src->getAllocatedType();
                llvm::Value *fieldPtr = builder_.CreateStructGEP(envTy, typedEnv, i, name + ".field");
                llvm::AllocaInst *dst = builder_.CreateAlloca(capTy, nullptr, name);
                builder_.CreateStore(builder_.CreateLoad(capTy, fieldPtr, name + ".cap"), dst);
                scope_stack_.back()[name] = dst;

                propagateMeta(src, dst);
            }
        }

        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, s.var_names[0]);
        builder_.CreateStore(chunkBegin, iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "parallel.cond", thunk);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "parallel.body", thunk);
        llvm::BasicBlock *stepBB = llvm::BasicBlock::Create(*ctx_, "parallel.step", thunk);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "parallel.end", thunk);

        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(condBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "parallel_i");
        llvm::Value *stepPos = builder_.CreateICmpSGT(stepArg, llvm::ConstantInt::get(i64Ty_, 0), "parallel_step_pos");
        llvm::Value *posCond = builder_.CreateICmpSLT(iCur, chunkEnd, "parallel_pos_cond");
        llvm::Value *negCond = builder_.CreateICmpSGT(iCur, chunkEnd, "parallel_neg_cond");
        llvm::Value *loopCond = builder_.CreateSelect(stepPos, posCond, negCond, "parallel_cond");
        builder_.CreateCondBr(loopCond, bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        pushScope();
        scope_stack_.back()[s.var_names[0]] = iVar;
        for (auto &stmt : s.body)
            std::visit([this](auto &st) { emitStmt(st); }, stmt);
        popScope();
        if (!builder_.GetInsertBlock()->getTerminator())
            builder_.CreateBr(stepBB);

        builder_.SetInsertPoint(stepBB);
        llvm::Value *iNext = builder_.CreateAdd(
            builder_.CreateLoad(i64Ty_, iVar, "parallel_i_step"), stepArg, "parallel_i_next");
        builder_.CreateStore(iNext, iVar);
        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(endBB);
        builder_.CreateRetVoid();
    }

    llvm::FunctionType *parallelTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {i64Ty_, i64Ty_, i64Ty_, ptrTy_, ptrTy_}, false);
    llvm::FunctionCallee parallelFn = mod_->getOrInsertFunction("__ry_parallel_for_i64", parallelTy);
    builder_.CreateCall(parallelFn, {begin, end, step, envPtr, builder_.CreateBitCast(thunk, ptrTy_)});
}

} // namespace ry
