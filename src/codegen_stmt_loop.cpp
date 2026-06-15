#include "ry/codegen.hpp"
#include "ry/diagnostic/diagnostic.hpp"
#include "ry/stdlib_registry.hpp"


namespace ry {

// Returns true when the iterable expression carries an external alias that
// could trigger in-place CoW mutations of the underlying collection during
// the loop body. These paths need the retain+snapshot guard (#1021, #1041, #1091):
//   - VariableExpr: a named binding in scope (e.g. `for x in ys:`).
//   - FieldAccessExpr: a record field access (e.g. `for x in obj.items:`).
//   - IndexExpr: a container slot access (e.g. `for x in xs[i]:`, `for k, v in m["key"]:`).
// Temporaries — range(), call results, literals, emitStringToCharList output —
// carry no aliasable binding, so pre-loop SSA values are safe without a guard.
static bool iterableHasExternalAlias(const ExprNode &e) {
    return std::get_if<VariableExpr>(&e.data) != nullptr
        || std::get_if<std::unique_ptr<FieldAccessExpr>>(&e.data) != nullptr
        || std::get_if<std::unique_ptr<IndexExpr>>(&e.data) != nullptr;
}

static void collectForBindingNames(const Pattern &pattern,
                                   std::vector<std::string> &out) {
    std::visit([&](const auto &pat) {
        using T = std::decay_t<decltype(pat)>;
        if constexpr (std::is_same_v<T, VariablePattern>) {
            out.push_back(pat.name);
        } else if constexpr (std::is_same_v<T, std::unique_ptr<TuplePattern>>) {
            for (const auto &elem : pat->elements)
                collectForBindingNames(elem, out);
        }
    }, pattern);
}

static size_t forBindingArity(const Pattern &pattern) {
    if (std::holds_alternative<std::unique_ptr<TuplePattern>>(pattern))
        return std::get<std::unique_ptr<TuplePattern>>(pattern)->elements.size();
    return 1;
}

static bool isSingleVariableForBinding(const Pattern &pattern, std::string *name = nullptr) {
    if (auto *var = std::get_if<VariablePattern>(&pattern)) {
        if (name)
            *name = var->name;
        return true;
    }
    return false;
}

static bool isWildcardForBinding(const Pattern &pattern) {
    return std::holds_alternative<WildcardPattern>(pattern);
}

void CodeGen::emitStmt(std::unique_ptr<WhileStmt> &s) {
    emitCoverage(s->loc);
    llvm::BasicBlock *condBB = createBB("while.cond");
    llvm::BasicBlock *bodyBB = createBB("while.body");
    llvm::BasicBlock *endBB  = createBB("while.end");

    emitBranchUncond(condBB);

    builder_.SetInsertPoint(condBB);
    llvm::Value *cond = emitExpr(*s->condition);
    cond = toBool(cond);
    emitBranchCond(cond, bodyBB, endBB);

    builder_.SetInsertPoint(bodyBB);
    loop_stack_.push_back({condBB, endBB, scope_stack_.size()});
    pushScope();
    for (auto &stmt : s->body)
        std::visit([this](auto &st) { emitStmt(st); }, stmt);
    popScope();
    loop_stack_.pop_back();
    if (!builder_.GetInsertBlock()->getTerminator())
        emitBranchUncond(condBB);

    builder_.SetInsertPoint(endBB);
}

void CodeGen::emitStmt(std::unique_ptr<ForStmt> &s) {
    emitCoverage(s->loc);
    current_loc_ = s->loc;
    validateDirectives(s->directives, DirectiveTarget::ForLoop);
    if (hasDirective(s->directives, "parallel")) {
        if (!isSingleVariableForBinding(s->binding))
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

        llvm::BasicBlock *condBB = createBB("foriter.cond");
        llvm::BasicBlock *bodyBB = createBB("foriter.body");
        llvm::BasicBlock *endBB = createBB("foriter.end");

        emitBranchUncond(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *opt = builder_.CreateCall(nextCallTy, nextFnPtr, {statePtr}, "foriter_opt");
        llvm::Value *hasVal = builder_.CreateExtractValue(opt, 0, "foriter_has");
        emitBranchCond(hasVal, bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        loop_stack_.push_back({condBB, endBB, scope_stack_.size()});
        pushScope();

        llvm::Value *elem = builder_.CreateExtractValue(opt, 1, "foriter_elem");
        emitForBindingPattern(s->binding, elem, iterElemTy, /*valueTypeName=*/"");

        for (auto &stmt : s->body)
            std::visit([this](auto &st) { emitStmt(st); }, stmt);

        popScope();
        loop_stack_.pop_back();
        if (!builder_.GetInsertBlock()->getTerminator())
            emitBranchUncond(condBB);

        builder_.SetInsertPoint(endBB);
        return;
    }

    // Multi-variable iteration: for k, v in map  OR  for a, b, c in list_of_tuples
    if (forBindingArity(s->binding) > 1) {
        llvm::Type *keyTy = getMapKeyType(iterable);
        llvm::Type *valTy = getMapValueType(iterable);
        if (!keyTy || !valTy) {
            // Set and list share the same {len, cap, data*} prefix, so once
            // headerTy is picked the GEP arithmetic is identical.
            llvm::Type *elemTy = getSetElementType(iterable);
            llvm::StructType *headerTy = setHeaderTy_;
            if (!elemTy) {
                elemTy = getListElementType(iterable);
                headerTy = listHeaderTy_;
            }
            const bool isSet = (headerTy == setHeaderTy_);
            auto *structTy = llvm::dyn_cast_or_null<llvm::StructType>(elemTy);
            if (!structTy)
                codegenError("for loop destructuring requires a list or set of tuples");

            // Snapshot the iterable to prevent UAF when the source alias is
            // mutated inside the loop body (#1021, #1041). Applies when the
            // iterable carries an external alias (VariableExpr or
            // FieldAccessExpr); temporaries (range(), function calls, etc.)
            // have no external alias and are safe without a guard.
            const bool tupleIterHasAlias = iterableHasExternalAlias(*s->iterable);
            llvm::AllocaInst *tupleSnapAlloca = nullptr;
            llvm::Value *tupleDataPtr = nullptr;
            llvm::Value *length;
            if (tupleIterHasAlias) {
                llvm::Value *arcHdr = emitArcGetHeaderFromData(iterable);
                emitArcRetain(arcHdr);
                std::string snapName =
                    "__for_iter_snap_" + std::to_string(for_snap_counter_++);
                tupleSnapAlloca = getOrCreateVar(snapName, ptrTy_);
                builder_.CreateStore(iterable, tupleSnapAlloca);
                setTypeMeta(isSet ? TypeMeta::SetElem : TypeMeta::ListElem,
                            tupleSnapAlloca, structTy);
                markArcManaged(tupleSnapAlloca);
                llvm::Value *snap0 =
                    builder_.CreateLoad(ptrTy_, tupleSnapAlloca, "for_snap");
                llvm::Value *lenPtr =
                    builder_.CreateStructGEP(headerTy, snap0, 0, "for_len_ptr");
                length = builder_.CreateLoad(i64Ty_, lenPtr, "for_len");
            } else {
                llvm::Value *lenPtr =
                    builder_.CreateStructGEP(headerTy, iterable, 0, "for_len_ptr");
                length = builder_.CreateLoad(i64Ty_, lenPtr, "for_len");
                llvm::Value *dataPtrField =
                    builder_.CreateStructGEP(headerTy, iterable, 2, "for_data_ptr");
                tupleDataPtr = builder_.CreateLoad(ptrTy_, dataPtrField, "for_data");
            }

            // Snapshot source-level tuple type name before entering the loop
            // body lambda — unordered_map rehash inside propagateTypeMeta may
            // invalidate any pointer we hold across the boundary (same pattern
            // as the single-variable path below).
            std::string tupleTypeName;
            if (auto *iterMeta = getMeta(iterable)) {
                if (isSet && !iterMeta->set_elem_type_name.empty())
                    tupleTypeName = iterMeta->set_elem_type_name;
                else
                    tupleTypeName = iterMeta->list_elem_type_name;
            }

            emitIndexedForLoop(length, s->body, [&](llvm::Value *iCur) {
                llvm::Value *dataPtr;
                if (tupleSnapAlloca) {
                    llvm::Value *snap =
                        builder_.CreateLoad(ptrTy_, tupleSnapAlloca, "for_snap");
                    llvm::Value *dataPtrField =
                        builder_.CreateStructGEP(headerTy, snap, 2, "for_data_ptr");
                    dataPtr = builder_.CreateLoad(ptrTy_, dataPtrField, "for_data");
                } else {
                    dataPtr = tupleDataPtr;
                }
                llvm::Value *tuplePtr =
                    builder_.CreateGEP(structTy, dataPtr, {iCur}, "for_tuple_ptr");
                llvm::Value *tuple = builder_.CreateLoad(structTy, tuplePtr, "for_tuple");
                emitForBindingPattern(s->binding, tuple, structTy, tupleTypeName);
            });
            return;
        }

        // Map iteration: always exactly 2 variables (key, value)
        if (forBindingArity(s->binding) != 2)
            codegenError("map iteration requires exactly 2 variables (key, value), got " +
                         std::to_string(forBindingArity(s->binding)));

        // Snapshot the iterable to prevent UAF when the source alias is
        // mutated inside the loop body (#1021, #1041). Applies when the
        // iterable carries an external alias (VariableExpr or
        // FieldAccessExpr); temporaries have no external alias.
        const bool mapIterHasAlias = iterableHasExternalAlias(*s->iterable);
        llvm::AllocaInst *mapSnapAlloca = nullptr;
        llvm::Value *mapKeysPtr = nullptr;
        llvm::Value *mapValsPtr = nullptr;
        llvm::Value *length;
        if (mapIterHasAlias) {
            llvm::Value *mapArcHdr = emitArcGetHeaderFromData(iterable);
            emitArcRetain(mapArcHdr);
            std::string mapSnapName =
                "__for_iter_snap_" + std::to_string(for_snap_counter_++);
            mapSnapAlloca = getOrCreateVar(mapSnapName, ptrTy_);
            builder_.CreateStore(iterable, mapSnapAlloca);
            setTypeMeta(TypeMeta::MapKey, mapSnapAlloca, keyTy);
            setTypeMeta(TypeMeta::MapValue, mapSnapAlloca, valTy);
            markArcManaged(mapSnapAlloca);
            llvm::Value *mapSnap0 =
                builder_.CreateLoad(ptrTy_, mapSnapAlloca, "for_snap");
            llvm::Value *lenPtr =
                builder_.CreateStructGEP(mapHeaderTy_, mapSnap0, 0, "map_len_ptr");
            length = builder_.CreateLoad(i64Ty_, lenPtr, "map_len");
        } else {
            llvm::Value *lenPtr =
                builder_.CreateStructGEP(mapHeaderTy_, iterable, 0, "map_len_ptr");
            length = builder_.CreateLoad(i64Ty_, lenPtr, "map_len");
            llvm::Value *keysPtrField =
                builder_.CreateStructGEP(mapHeaderTy_, iterable, 2, "keys_ptr_field");
            mapKeysPtr = builder_.CreateLoad(ptrTy_, keysPtrField, "keys_ptr");
            llvm::Value *valsPtrField =
                builder_.CreateStructGEP(mapHeaderTy_, iterable, 3, "vals_ptr_field");
            mapValsPtr = builder_.CreateLoad(ptrTy_, valsPtrField, "vals_ptr");
        }

        // Snapshot source-level key/value type names before entering the loop
        // body lambda so we can propagate nested collection/enum metadata onto
        // the bound variables (#813). propagateTypeMeta() may rehash the
        // metadata map and invalidate pointers taken inside the lambda.
        std::string keyTypeName, valTypeName;
        if (auto *iterMeta = getMeta(iterable)) {
            keyTypeName = iterMeta->map_key_type_name;
            valTypeName = iterMeta->map_value_type_name;
        }

        emitIndexedForLoop(length, s->body, [&](llvm::Value *iCur) {
            llvm::Value *keysPtr;
            llvm::Value *valsPtr;
            if (mapSnapAlloca) {
                llvm::Value *mapSnap =
                    builder_.CreateLoad(ptrTy_, mapSnapAlloca, "for_snap");
                llvm::Value *keysPtrField =
                    builder_.CreateStructGEP(mapHeaderTy_, mapSnap, 2, "keys_ptr_field");
                keysPtr = builder_.CreateLoad(ptrTy_, keysPtrField, "keys_ptr");
                llvm::Value *valsPtrField =
                    builder_.CreateStructGEP(mapHeaderTy_, mapSnap, 3, "vals_ptr_field");
                valsPtr = builder_.CreateLoad(ptrTy_, valsPtrField, "vals_ptr");
            } else {
                keysPtr = mapKeysPtr;
                valsPtr = mapValsPtr;
            }
            llvm::Value *keyPtr = builder_.CreateGEP(keyTy, keysPtr, {iCur}, "for_key_ptr");
            llvm::Value *key = builder_.CreateLoad(keyTy, keyPtr, "for_key");
            llvm::Value *valPtr = builder_.CreateGEP(valTy, valsPtr, {iCur}, "for_val_ptr");
            llvm::Value *val = builder_.CreateLoad(valTy, valPtr, "for_val");
            llvm::Value *pair = llvm::UndefValue::get(llvm::StructType::get(*ctx_, {keyTy, valTy}));
            pair = builder_.CreateInsertValue(pair, key, 0, "for_pair_key");
            pair = builder_.CreateInsertValue(pair, val, 1, "for_pair_val");
            emitForBindingPattern(s->binding, pair, pair->getType(),
                                  "(" + keyTypeName + ", " + valTypeName + ")");
        });
        return;
    }

    // Only snapshot (retain+alloca) when the iterable carries an external alias
    // that can trigger CoW mutations during the loop (VariableExpr or
    // FieldAccessExpr). Temporaries (range(), function calls,
    // string-to-char conversion, etc.) have no external alias (#1021, #1041).
    bool elemIterHasAlias = iterableHasExternalAlias(*s->iterable);

    // Try set first, then list
    llvm::Type *elemTy = getSetElementType(iterable);
    llvm::StructType *headerTy = setHeaderTy_;
    if (!elemTy) {
        elemTy = getListElementType(iterable);
        headerTy = listHeaderTy_;
    }
    // String iteration (#746, #827): `for c in s:` desugars to iterating
    // the List<str> produced by __ry_split_chars, so each loop step yields
    // one UTF-8 code point rather than a raw byte.
    if (!elemTy && isStringValue(iterable)) {
        iterable = emitStringToCharList(iterable, "for_str_chars");
        elemTy = ptrTy_;
        headerTy = listHeaderTy_;
        elemIterHasAlias = false;  // fresh temp — no external alias, no retain needed
    }
    if (!elemTy)
        codegenError("cannot determine element type for for loop iterable");

    // Snapshot the iterable to prevent UAF when the source alias is mutated
    // inside the loop body (#1021, #1041). Applies for VariableExpr and
    // FieldAccessExpr iterables.
    llvm::AllocaInst *elemSnapAlloca = nullptr;
    llvm::Value *elemDataPtr = nullptr;
    llvm::Value *length;
    if (elemIterHasAlias) {
        llvm::Value *elemArcHdr = emitArcGetHeaderFromData(iterable);
        emitArcRetain(elemArcHdr);
        std::string elemSnapName =
            "__for_iter_snap_" + std::to_string(for_snap_counter_++);
        elemSnapAlloca = getOrCreateVar(elemSnapName, ptrTy_);
        builder_.CreateStore(iterable, elemSnapAlloca);
        if (headerTy == setHeaderTy_)
            setTypeMeta(TypeMeta::SetElem, elemSnapAlloca, elemTy);
        else
            setTypeMeta(TypeMeta::ListElem, elemSnapAlloca, elemTy);
        markArcManaged(elemSnapAlloca);
        llvm::Value *elemSnap0 =
            builder_.CreateLoad(ptrTy_, elemSnapAlloca, "for_snap");
        llvm::Value *lenPtr =
            builder_.CreateStructGEP(headerTy, elemSnap0, 0, "for_len_ptr");
        length = builder_.CreateLoad(i64Ty_, lenPtr, "for_len");
    } else {
        llvm::Value *lenPtr =
            builder_.CreateStructGEP(headerTy, iterable, 0, "for_len_ptr");
        length = builder_.CreateLoad(i64Ty_, lenPtr, "for_len");
        llvm::Value *dataPtrField =
            builder_.CreateStructGEP(headerTy, iterable, 2, "for_data_ptr");
        elemDataPtr = builder_.CreateLoad(ptrTy_, dataPtrField, "for_data");
    }

    // Copy list/set element metadata before entering the loop body to avoid
    // pointer invalidation from unordered_map rehash inside propagateTypeMeta/getOrCreateMeta.
    std::string elemTypeName;
    std::optional<FnTypeInfo> elemFnTypeInfo;
    if (auto *iterMeta = getMeta(iterable)) {
        if (headerTy == setHeaderTy_ && !iterMeta->set_elem_type_name.empty())
            elemTypeName = iterMeta->set_elem_type_name;
        else
            elemTypeName = iterMeta->list_elem_type_name;
        elemFnTypeInfo  = iterMeta->list_elem_fn_type_info;
    }
    emitIndexedForLoop(length, s->body, [&](llvm::Value *iCur) {
        llvm::Value *dataPtr;
        if (elemSnapAlloca) {
            llvm::Value *elemSnap =
                builder_.CreateLoad(ptrTy_, elemSnapAlloca, "for_snap");
            llvm::Value *dataPtrField =
                builder_.CreateStructGEP(headerTy, elemSnap, 2, "for_data_ptr");
            dataPtr = builder_.CreateLoad(ptrTy_, dataPtrField, "for_data");
        } else {
            dataPtr = elemDataPtr;
        }
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, dataPtr, {iCur}, "for_elem_ptr");
        llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "for_elem");
        emitForBindingPattern(s->binding, elem, elemTy, elemTypeName);
        if (elemFnTypeInfo) {
            std::string loopVarName;
            if (isSingleVariableForBinding(s->binding, &loopVarName)) {
                if (llvm::AllocaInst *loopVar = findVar(loopVarName))
                    getOrCreateMeta(loopVar).fn_type_info = *elemFnTypeInfo;
            }
        }
    });
}

void CodeGen::emitForBindingPattern(const Pattern &pattern, llvm::Value *value,
                                    llvm::Type *valueTy,
                                    const std::string &valueTypeName) {
    if (isWildcardForBinding(pattern))
        return;

    if (const auto *var = std::get_if<VariablePattern>(&pattern)) {
        llvm::AllocaInst *loopVar = getOrCreateVar(var->name, valueTy);
        builder_.CreateStore(value, loopVar);
        if (!valueTypeName.empty())
            propagateTypeMeta(valueTypeName, loopVar);
        return;
    }

    auto *tuplePat = std::get_if<std::unique_ptr<TuplePattern>>(&pattern);
    if (!tuplePat)
        codegenError("for loop pattern only supports tuple destructuring");
    auto *structTy = llvm::dyn_cast<llvm::StructType>(valueTy);
    if (!structTy || !isTupleStructType(structTy))
        codegenError("for loop destructuring requires tuple elements");

    const auto elemSigs = splitTupleSig(valueTypeName);
    if (structTy->getNumElements() != (*tuplePat)->elements.size())
        codegenError("for loop destructuring: expected " +
                     std::to_string((*tuplePat)->elements.size()) +
                     "-element tuple, but got " +
                     std::to_string(structTy->getNumElements()) +
                     " elements");
    for (size_t i = 0; i < (*tuplePat)->elements.size(); ++i) {
        llvm::Value *elem = builder_.CreateExtractValue(
            value, static_cast<unsigned>(i), "for_elem_" + std::to_string(i));
        llvm::Type *elemTy = structTy->getElementType(static_cast<unsigned>(i));
        const std::string elemSig =
            (i < elemSigs.size()) ? elemSigs[i] : std::string{};
        emitForBindingPattern((*tuplePat)->elements[i], elem, elemTy, elemSig);
    }
}



void CodeGen::emitIndexedForLoop(llvm::Value *length,
                                  std::vector<StmtNode> &body,
                                  std::function<void(llvm::Value *iCur)> bindVars) { // NOLINT(performance-unnecessary-value-param)
    llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "for_i");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

    llvm::BasicBlock *condBB = createBB("for.cond");
    llvm::BasicBlock *bodyBB = createBB("for.body");
    llvm::BasicBlock *stepBB = createBB("for.step");
    llvm::BasicBlock *endBB  = createBB("for.end");

    emitBranchUncond(condBB);
    builder_.SetInsertPoint(condBB);
    llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "i");
    llvm::Value *cond = builder_.CreateICmpSLT(iVal, length, "for_cond");
    emitBranchCond(cond, bodyBB, endBB);

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
        emitBranchUncond(stepBB);

    builder_.SetInsertPoint(stepBB);
    llvm::Value *iNext = builder_.CreateAdd(
        builder_.CreateLoad(i64Ty_, iVar, "i_step"), llvm::ConstantInt::get(i64Ty_, 1), "i_next");
    builder_.CreateStore(iNext, iVar);
    emitBranchUncond(condBB);

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
    emitBranchUncond(std::get<1>(loop_stack_.back()));
    // Create unreachable block for subsequent code
    llvm::BasicBlock *deadBB = createBB("break.dead");
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
    emitBranchUncond(std::get<0>(loop_stack_.back()));
    llvm::BasicBlock *deadBB = createBB("continue.dead");
    builder_.SetInsertPoint(deadBB);
}

void CodeGen::emitStmt(EllipsisStmt &) {
    // no-op: intentionally does nothing
}

void CodeGen::validateParallelFor(const ForStmt &s) {
    std::vector<std::unordered_set<std::string>> localScopes(1);
    std::vector<std::string> bindingNames;
    collectForBindingNames(s.binding, bindingNames);
    for (const auto &name : bindingNames)
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
            } else if constexpr (std::is_same_v<T, std::unique_ptr<CaseCondStmt>>) {
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
                std::vector<std::string> nestedNames;
                collectForBindingNames(node->binding, nestedNames);
                for (const auto &name : nestedNames) {
                    if (name != "_")
                        localScopes.back().insert(name);
                }
                for (const auto &innerStmt : node->body)
                    scanStmt(innerStmt);
                localScopes.pop_back();
            } else if constexpr (std::is_same_v<T, std::unique_ptr<CaseStmt>>) {
                for (const auto &arm : node->arms)
                    scanBlock(arm.body);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<UsingStmt>>) {
                // Treat the using binding as a new local in its own scope so
                // assignments inside the body are checked against outer
                // variables (#1842 review).
                localScopes.push_back({});
                localScopes.back().insert(node->name);
                for (const auto &innerStmt : node->body)
                    scanStmt(innerStmt);
                localScopes.pop_back();
            } else if constexpr (std::is_same_v<T, std::unique_ptr<FnStmt>>) {
                codegenError(node->loc, "parallel for does not allow nested function definitions");
            }
        }, stmt);
    };

    for (const auto &stmt : s.body)
        scanStmt(stmt);
}

void CodeGen::emitParallelForRange(ForStmt &s, llvm::Value *begin, llvm::Value *end, llvm::Value *step) {
    std::string loopVarName;
    if (!isSingleVariableForBinding(s.binding, &loopVarName))
        codegenError(s.loc, "@parallel for does not support destructuring iteration");

    std::vector<std::pair<std::string, llvm::AllocaInst*>> captures;
    std::unordered_set<std::string> seen;
    for (auto scopeIt = scope_stack_.rbegin(); scopeIt != scope_stack_.rend(); ++scopeIt) {
        for (const auto &[name, alloca] : *scopeIt) {
            if (name == loopVarName || seen.count(name))
                continue;
            seen.insert(name);
            captures.push_back({name, alloca});
        }
    }

    // Snapshot per-capture ARC flags in the PARENT context: FnScope below
    // will clear arc_managed_vars_ / arc_backed_vars_, so the source
    // alloca's status is only observable here. Both flags matter:
    // arc_managed drives the atomic retain/release in the worker thunk,
    // arc_backed drives CoW privatization on writes to the captured slot.
    std::vector<bool> capIsArcManaged(captures.size(), false);
    std::vector<bool> capIsArcBacked(captures.size(), false);
    std::vector<bool> capIsArcStr(captures.size(), false);
    for (size_t i = 0; i < captures.size(); ++i) {
        llvm::AllocaInst *src = captures[i].second;
        capIsArcManaged[i] = isArcManaged(src);
        capIsArcBacked[i] = arc_backed_vars_.count(src) > 0;
        capIsArcStr[i] = arc_str_managed_vars_.count(src) > 0;
    }

    std::vector<llvm::Type*> envFields;
    if (captures.empty())
        envFields.push_back(i8Ty_);
    else
        for (const auto &[_, alloca] : captures)
            envFields.push_back(alloca->getAllocatedType());
    llvm::StructType *envTy = llvm::StructType::get(*ctx_, envFields);

    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t envSize = std::max<uint64_t>(1, dl.getTypeAllocSize(envTy));
    llvm::Value *envPtr = emitRuntimeCallDirect(
        "malloc", ptrTy_, {i64Ty_},
        {emitConstInt(i64Ty_, envSize)}, "parallel_env");

    if (captures.empty()) {
        llvm::Value *dummyField = emitStructGEP(envTy, envPtr, 0, "parallel_env_dummy");
        emitStore(emitConstInt(i8Ty_, 0), dummyField);
    } else {
        for (size_t i = 0; i < captures.size(); ++i) {
            llvm::Value *fieldPtr = emitStructGEP(envTy, envPtr, static_cast<unsigned>(i), "parallel_env_field");
            llvm::AllocaInst *src = captures[i].second;
            emitStore(
                emitLoad(src->getAllocatedType(), src, (captures[i].first + ".par_cap").c_str()),
                fieldPtr);
        }
    }

    llvm::FunctionType *thunkTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {ptrTy_, i64Ty_, i64Ty_, i64Ty_}, false);
    std::string thunkName = "__ry_parallel_for." + std::to_string(lambda_counter_++);
    llvm::Function *thunk = emitCreateFunction(
        thunkTy, llvm::Function::InternalLinkage, thunkName.c_str());

    {
        FnScope guard(*this);
        fn_ = thunk;

        // RAII bump of parallel_for_depth_ so isArcAtomic() returns true for
        // every ARC op emitted inside the thunk body (#630). Restored on
        // unwind too, so a codegenError thrown mid-emission does not leak
        // depth into subsequent unrelated codegen.
        ParallelForScope parScope(*this);

        pushScope();

        llvm::BasicBlock *entryBB = createBBInFn("entry", thunk);
        builder_.SetInsertPoint(entryBB);

        llvm::Value *envRaw = emitGetParam(thunk, 0);
        envRaw->setName("env_raw");
        llvm::Value *chunkBegin = emitGetParam(thunk, 1);
        chunkBegin->setName("chunk_begin");
        llvm::Value *chunkEnd = emitGetParam(thunk, 2);
        chunkEnd->setName("chunk_end");
        llvm::Value *stepArg = emitGetParam(thunk, 3);
        stepArg->setName("step");

        // Under opaque pointers `bitcast envRaw to ptr` is the identity, so the
        // IRBuilder folded the original `parallel_env_typed` cast to a no-op
        // (IR baseline has zero `bitcast` lines). Use envRaw directly as the
        // env pointer below — same byte-exact IR, one fewer instruction to
        // synthesize.

        if (!captures.empty()) {
            for (size_t i = 0; i < captures.size(); ++i) {
                const auto &[name, src] = captures[i];
                llvm::Type *capTy = src->getAllocatedType();
                llvm::Value *fieldPtr = emitStructGEP(envTy, envRaw, static_cast<unsigned>(i), (name + ".field").c_str());
                llvm::AllocaInst *dst = llvm::cast<llvm::AllocaInst>(emitAlloca(capTy, name.c_str()));
                emitStore(emitLoad(capTy, fieldPtr, (name + ".cap").c_str()), dst);
                scope_stack_.back()[name] = dst;

                propagateMeta(src, dst);

                // FnScope cleared arc_managed_vars_ / arc_backed_vars_, so
                // propagateMeta's internal isArcManaged(src) check no-ops
                // and we must re-apply the flags from the pre-FnScope
                // snapshot. Without arc_managed_vars_ membership the
                // thunk's popScope() below will not release the capture;
                // without arc_backed_vars_ membership emitCowCheck bails
                // out and workers mutate the shared buffer in place (#630).
                if (capIsArcBacked[i])
                    arc_backed_vars_.insert(dst);
                if (!capIsArcManaged[i])
                    continue;
                markArcManaged(dst);
                // str captures need the -24 offset side-table entry so that
                // emitArcReleaseVar (called by popScope below) uses the
                // correct emitStrGetHeaderFromData path (#1046).
                if (capIsArcStr[i])
                    arc_str_managed_vars_.insert(dst);

                // Retain the captured ARC value so strong_count stays
                // >= 2 while workers run: that makes emitCowCheck's
                // `> 1` test reliably pick the deep-copy path instead of
                // "unique → mutate in place". The matching release is
                // emitted by popScope() below while ParallelForScope is
                // still live, so it uses the atomic path too (#630).
                auto *dataPtr = emitLoad(ptrTy_, dst, (name + ".par_retain_load").c_str());
                auto *isNull = emitICmpEQ(
                    dataPtr,
                    llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)),
                    "par_retain_null");
                auto *retainBB = createBBInFn("arc.par_retain", thunk);
                auto *skipBB = createBBInFn("arc.par_retain.skip", thunk);
                emitBranchCond(isNull, skipBB, retainBB);

                builder_.SetInsertPoint(retainBB);
                // str handles have StringHeader at offset -24; use the
                // correct helper to avoid corrupting weak_count (#1046).
                auto *hdr = capIsArcStr[i] ? emitStrGetHeaderFromData(dataPtr)
                                           : emitArcGetHeaderFromData(dataPtr);
                emitArcRetain(hdr, /*atomic=*/true);
                emitBranchUncond(skipBB);

                builder_.SetInsertPoint(skipBB);
            }
        }

        llvm::AllocaInst *iVar = llvm::cast<llvm::AllocaInst>(emitAlloca(i64Ty_, loopVarName.c_str()));
        emitStore(chunkBegin, iVar);

        llvm::BasicBlock *condBB = createBBInFn("parallel.cond", thunk);
        llvm::BasicBlock *bodyBB = createBBInFn("parallel.body", thunk);
        llvm::BasicBlock *stepBB = createBBInFn("parallel.step", thunk);
        llvm::BasicBlock *endBB = createBBInFn("parallel.end", thunk);

        emitBranchUncond(condBB);

        builder_.SetInsertPoint(condBB);
        llvm::Value *iCur = emitLoad(i64Ty_, iVar, "parallel_i");
        llvm::Value *zero = emitConstInt(i64Ty_, 0);
        llvm::Value *stepPos = emitICmpSGT(stepArg, zero, "parallel_step_pos");
        llvm::Value *posCond = emitICmpSLT(iCur, chunkEnd, "parallel_pos_cond");
        llvm::Value *negCond = emitICmpSGT(iCur, chunkEnd, "parallel_neg_cond");
        llvm::Value *loopCond = emitSelect(stepPos, posCond, negCond, "parallel_cond");
        emitBranchCond(loopCond, bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        pushScope();
        scope_stack_.back()[loopVarName] = iVar;
        for (auto &stmt : s.body)
            std::visit([this](auto &st) { emitStmt(st); }, stmt);
        popScope();
        if (!builder_.GetInsertBlock()->getTerminator())
            emitBranchUncond(stepBB);

        builder_.SetInsertPoint(stepBB);
        llvm::Value *iNext = emitAdd(
            emitLoad(i64Ty_, iVar, "parallel_i_step"), stepArg, "parallel_i_next");
        emitStore(iNext, iVar);
        emitBranchUncond(condBB);

        builder_.SetInsertPoint(endBB);
        // Pop the captures scope to run emitScopeCleanup → emitArcReleaseVar
        // for each ARC-managed capture, matching the atomic retains from
        // worker entry. Must happen while ParallelForScope is still live
        // so the release uses the atomic path (#630).
        popScope();
        emitRet(nullptr);
    }

    // `bitcast thunk to ptr` is the identity under opaque pointers (the IR
    // baseline carries no such bitcast on this site either); pass the Function*
    // directly as `ptr`.
    emitRuntimeCallDirect(
        "__ry_parallel_for_i64", llvm::Type::getVoidTy(*ctx_),
        {i64Ty_, i64Ty_, i64Ty_, ptrTy_, ptrTy_},
        {begin, end, step, envPtr, thunk}, "");
}

void CodeGen::emitStmt(std::unique_ptr<UsingStmt> &s) {
    emitCoverage(s->loc);
    current_loc_ = s->loc;

    // Evaluate the init expression BEFORE pushScope.  This ordering is
    // load-bearing: if the init contains `?` propagation and produces Err,
    // the early `return` fires here while no `using` binding exists, so
    // the resource's auto-close is correctly skipped.
    llvm::Value *val = emitExpr(*s->value);

    int rk = detectResourceKind(val);
    if (rk == ResourceKindRegistry::NONE || !isFile(val))
        codegenError(s->loc, "using requires an io.File value");

    pushScope();

    if (scope_stack_.back().count(s->name))
        codegenError(s->loc, "redeclared variable: " + s->name);

    llvm::AllocaInst *ptr = getOrCreateVar(s->name, ptrTy_);
    builder_.CreateStore(val, ptr);
    markArcManaged(ptr);
    resource_managed_vars_[ptr] = rk;
    propagateMetaWide(val, ptr);

    // Alias semantics: retain when the source is itself a tracked binding
    // (e.g. `using fh = f` where f came from `Ok(f)`). Without the retain,
    // the source's scope-end release would underflow the strong count and
    // double-free. Fresh temps (`using f = open(...)`) are already at
    // strong=1 from the runtime alloc, so no retain is needed.
    if (auto *varExpr = std::get_if<VariableExpr>(&s->value->data)) {
        if (auto *srcAlloca = findVar(varExpr->name)) {
            if (resource_managed_vars_.count(srcAlloca)) {
                auto *hdr = emitArcGetHeaderFromData(val);
                // Match the ARC mode of the surrounding scope so a
                // @parallel-for worker (atomic ARC) does not race the
                // scope-end release. isArcAtomic falls back to the worker
                // scope counter when no per-value flag is set.
                emitArcRetain(hdr, isArcAtomic(val));
            }
        }
    }

    for (auto &stmt : s->body)
        std::visit([this](auto &st) { emitStmt(st); }, stmt);

    popScope();
}

} // namespace ry
