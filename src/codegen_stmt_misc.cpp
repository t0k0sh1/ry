#include "ry/codegen.hpp"


namespace ry {

// ===== Chained LHS helpers (#812) =====
//
// These helpers support the generalized assignment dispatch: an LHS is now a
// postfix-expression chain whose root is a variable and whose hops are any
// mix of `.field` and `[index]`. The codegen for `FieldAssignStmt` and
// `IndexAssignStmt` branches on the type of the outermost chain node and
// routes to the appropriate write path.

// Null-guarded release of an already-loaded ARC element pointer. The
// builder's insertion point is left at the join block on return so the
// caller can continue emitting the store. Mirrors the retain-then-release
// shape of `emitStmt(AssignStmt&)` (src/codegen_stmt.cpp ARC branch) but
// operates on a slot value rather than an alloca-backed variable.
void CodeGen::emitArcReleaseLoadedElement(llvm::Value *oldElemVal,
                                          CollectionKind elemKind,
                                          const llvm::Twine &label) {
    auto *isOldNull = builder_.CreateICmpEQ(
        oldElemVal,
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)),
        label + ".arc_old_null");

    auto *parentFn = builder_.GetInsertBlock()->getParent();
    auto *releaseBB = llvm::BasicBlock::Create(*ctx_, "elem.arc_release", parentFn);
    auto *joinBB = llvm::BasicBlock::Create(*ctx_, "elem.arc_release_done", parentFn);
    builder_.CreateCondBr(isOldNull, joinBB, releaseBB);

    builder_.SetInsertPoint(releaseBB);
    auto *oldHdr = emitArcGetHeaderFromData(oldElemVal);
    // Element slots in lists/maps/sets are not marked atomic; collections
    // themselves are not @atomic by default. The destructor for the inner
    // kind walks its own data buffer and cascades releases to its elements.
    emitArcRelease(oldHdr, /*atomic=*/false,
                   getOrCreateCollectionDestructor(elemKind),
                   /*gcVisitFn=*/nullptr);
    builder_.CreateBr(joinBB);

    builder_.SetInsertPoint(joinBB);
}

llvm::Value *CodeGen::applyCompoundOp(const std::string &op,
                                       llvm::Value *currentVal,
                                       llvm::Value *rhs,
                                       ExprNode &rhsExpr,
                                       llvm::Type *targetTy,
                                       const std::string &contextName) {
    // Priority 1: user-defined compound assignment operator (e.g. operator+=)
    std::string compoundOpName = "operator" + op + "=";
    llvm::Value *result = tryOperatorCall(compoundOpName, currentVal, rhs);
    if (!result) {
        // Priority 2: user-defined binary operator or built-in (e.g. operator+)
        std::string rhsHint = getExprLowLevelSuffix(rhsExpr);
        result = emitBinaryOp(op, currentVal, rhs, "", rhsHint);
    }
    if (targetTy && result->getType() != targetTy) {
        if (targetTy == i8Ty_ && result->getType() == i64Ty_) {
            result = builder_.CreateTrunc(result, i8Ty_, contextName + ".bytetrunc");
        } else if (isAnyType(targetTy)) {
            result = wrapInAny(result);
        } else if (auto *sliced = tryEmitSubtypeCoerce(result, targetTy)) {
            result = sliced;
        } else {
            codegenError("type error: compound assignment on '" + contextName +
                         "' produces incompatible type");
        }
    }
    return result;
}

void CodeGen::writeBackFieldChain(ExprNode &chainTarget,
                                   llvm::Value *newInnerVal) {
    if (auto *ve = std::get_if<VariableExpr>(&chainTarget.data)) {
        llvm::Value *storagePtr = nullptr;
        llvm::Type *varTy = nullptr;
        if (llvm::AllocaInst *ptr = findVar(ve->name)) {
            storagePtr = ptr;
            varTy = ptr->getAllocatedType();
        } else if (auto *b = findModuleGlobal(ve->name)) {
            storagePtr = loadModuleGlobalStorage(*b, ve->name);
            varTy = b->valueTy();
        } else {
            codegenError("undefined variable: " + ve->name);
        }
        if (isImmutable(ve->name))
            codegenError("cannot modify field of @const variable: " + ve->name);
        if (!captured_vars_.empty()) {
            if (auto *ptr = llvm::dyn_cast<llvm::AllocaInst>(storagePtr))
                if (isCapturedVar(ptr))
                    codegenError("cannot modify field of captured variable '" +
                                 ve->name + "' inside closure");
        }
        if (newInnerVal->getType() != varTy)
            codegenError("internal: chained field assignment produced mismatched struct type for '" + ve->name + "'");
        builder_.CreateStore(newInnerVal, storagePtr);
        if (auto *rootSt = llvm::dyn_cast<llvm::StructType>(varTy)) {
            auto rit = struct_types_.find(rootSt->getName().str());
            if (rit != struct_types_.end())
                emitInvariantCheck(rit->first, rit->second, newInnerVal);
        }
        return;
    }

    if (auto *faPtr = std::get_if<std::unique_ptr<FieldAccessExpr>>(&chainTarget.data)) {
        FieldAccessExpr *fa = faPtr->get();
        llvm::Value *outerVal = emitExpr(*fa->object);
        auto *outerSt = llvm::dyn_cast<llvm::StructType>(outerVal->getType());
        if (!outerSt)
            codegenError("field access on non-struct type in chained assignment");
        auto it = struct_types_.find(outerSt->getName().str());
        if (it == struct_types_.end())
            codegenError("unknown struct type: " + outerSt->getName().str());
        int idx = it->second.findField(fa->field);
        if (idx < 0)
            codegenError("type '" + it->first + "' has no field '" + fa->field + "'");
        llvm::Type *expectedTy = outerSt->getElementType(idx);
        if (newInnerVal->getType() != expectedTy) {
            if (auto *sliced = tryEmitSubtypeCoerce(newInnerVal, expectedTy))
                newInnerVal = sliced;
            else
                codegenError("field '" + fa->field + "' type mismatch in chained assignment");
        }
        llvm::Value *updated = builder_.CreateInsertValue(outerVal, newInnerVal, idx, "chain_upd");
        writeBackFieldChain(*fa->object, updated);
        return;
    }

    // Anything else (IndexExpr, CallExpr, literal, ...) is not a simple
    // record field chain. The `FieldAssignStmt` dispatcher handles those
    // cases (`list[i].field = v` etc.) directly and never reaches this
    // helper, so treat a stray call here as an internal invariant break.
    codegenError("cannot chain-assign through non-record expression");
}

void CodeGen::emitStmt(FieldAssignStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;
    emitCoverage(s.loc);

    // Dispatch based on the shape of s.object: VariableExpr (plain),
    // FieldAccessExpr (deep record chain), IndexExpr (struct living inside
    // a heap collection element). Anything else is rejected as a non-lvalue.

    if (auto *varExpr = std::get_if<VariableExpr>(&s.object->data)) {
        llvm::Value *storagePtr = nullptr;
        llvm::Type *varTy = nullptr;
        if (llvm::AllocaInst *ptr = findVar(varExpr->name)) {
            storagePtr = ptr;
            varTy = ptr->getAllocatedType();
        } else if (auto *b = findModuleGlobal(varExpr->name)) {
            storagePtr = loadModuleGlobalStorage(*b, varExpr->name);
            varTy = b->valueTy();
        } else {
            codegenError("undefined variable: " + varExpr->name);
        }
        if (isImmutable(varExpr->name))
            codegenError("cannot modify field of @const variable: " + varExpr->name);

        llvm::StructType *structTy = llvm::dyn_cast<llvm::StructType>(varTy);
        if (!structTy)
            codegenError("field assignment on non-struct type");
        std::string typeName = structTy->getName().str();
        auto it = struct_types_.find(typeName);
        if (it == struct_types_.end())
            codegenError("unknown struct type: " + typeName);
        const auto &info = it->second;
        int fieldIdx = info.findField(s.field);
        if (fieldIdx < 0)
            codegenError("type '" + typeName + "' has no field '" + s.field + "'");
        llvm::Type *expectedTy = structTy->getElementType(fieldIdx);

        llvm::Value *newFieldVal = nullptr;
        llvm::Value *currentStruct = builder_.CreateLoad(varTy, storagePtr, "struct_cur");
        if (s.compound_op) {
            llvm::Value *currentField = builder_.CreateExtractValue(
                currentStruct, fieldIdx, s.field + ".compound_cur");
            llvm::Value *rhs = emitExpr(*s.value);
            newFieldVal = applyCompoundOp(*s.compound_op, currentField, rhs, *s.value,
                                           expectedTy, varExpr->name + "." + s.field);
        } else {
            newFieldVal = emitExpr(*s.value);
            if (newFieldVal->getType() != expectedTy) {
                if (auto *sliced = tryEmitSubtypeCoerce(newFieldVal, expectedTy))
                    newFieldVal = sliced;
                else
                    codegenError("field '" + s.field + "' type mismatch");
            }
        }

        llvm::Value *updated = builder_.CreateInsertValue(
            currentStruct, newFieldVal, fieldIdx, "struct_upd");
        builder_.CreateStore(updated, storagePtr);
        emitInvariantCheck(typeName, info, updated);
        return;
    }

    if (std::holds_alternative<std::unique_ptr<FieldAccessExpr>>(s.object->data)) {
        llvm::Value *innerStruct = emitExpr(*s.object);
        auto *innerSt = llvm::dyn_cast<llvm::StructType>(innerStruct->getType());
        if (!innerSt)
            codegenError("field assignment on non-struct type");
        auto it = struct_types_.find(innerSt->getName().str());
        if (it == struct_types_.end())
            codegenError("unknown struct type: " + innerSt->getName().str());
        const auto &info = it->second;
        int fieldIdx = info.findField(s.field);
        if (fieldIdx < 0)
            codegenError("type '" + it->first + "' has no field '" + s.field + "'");
        llvm::Type *expectedTy = innerSt->getElementType(fieldIdx);

        llvm::Value *newFieldVal = nullptr;
        if (s.compound_op) {
            llvm::Value *currentField = builder_.CreateExtractValue(
                innerStruct, fieldIdx, s.field + ".compound_cur");
            llvm::Value *rhs = emitExpr(*s.value);
            newFieldVal = applyCompoundOp(*s.compound_op, currentField, rhs, *s.value,
                                           expectedTy, it->first + "." + s.field);
        } else {
            newFieldVal = emitExpr(*s.value);
            if (newFieldVal->getType() != expectedTy) {
                if (auto *sliced = tryEmitSubtypeCoerce(newFieldVal, expectedTy))
                    newFieldVal = sliced;
                else
                    codegenError("field '" + s.field + "' type mismatch");
            }
        }

        llvm::Value *updatedInner = builder_.CreateInsertValue(
            innerStruct, newFieldVal, fieldIdx, "nested_upd");
        writeBackFieldChain(*s.object, updatedInner);
        return;
    }

    // Shallow CoW only on the outermost alloca anchor for chained forms
    // (`list[i].field = v`, `grid[i][j].field = v`). Deep CoW write-back
    // through record / nested-collection chains is tracked by issue #854.
    if (auto *idxPtr = std::get_if<std::unique_ptr<IndexExpr>>(&s.object->data)) {
        IndexExpr *idxExpr = idxPtr->get();
        llvm::AllocaInst *receiverAlloca = tryGetReceiverAlloca(*idxExpr->object);
        llvm::Value *containerPtr = emitExpr(*idxExpr->object);
        if (idxExpr->indices.size() != 1)
            codegenError("multi-index chained field assignment not supported");
        llvm::Value *indexVal = emitExpr(*idxExpr->indices[0]);
        if (containerPtr->getType() != ptrTy_)
            codegenError("chained field assignment requires a list or map base");
        llvm::Type *mapKeyTy = getMapKeyType(containerPtr);
        llvm::Value *elemSlot = nullptr;
        llvm::Type *elemTy = nullptr;
        if (mapKeyTy) {
            containerPtr = emitCowCheck(containerPtr, receiverAlloca, CollectionKind::Map);
            llvm::Type *mapValTy = getMapValueType(containerPtr);
            if (!mapValTy)
                codegenError("cannot determine map value type for chained field assignment");
            if (indexVal->getType() != mapKeyTy)
                codegenError("map key type mismatch");
            llvm::Value *slot = emitMapKeyLookup(containerPtr, indexVal, mapKeyTy);
            llvm::Value *missing = builder_.CreateICmpSLT(slot, llvm::ConstantInt::get(i64Ty_, 0), "map_key_missing");
            auto *fn = builder_.GetInsertBlock()->getParent();
            auto *errBB = llvm::BasicBlock::Create(*ctx_, "map_chain_missing", fn);
            auto *okBB = llvm::BasicBlock::Create(*ctx_, "map_chain_ok", fn);
            builder_.CreateCondBr(missing, errBB, okBB);
            builder_.SetInsertPoint(errBB);
            emitRuntimeError("runtime error: missing map key in chained field assignment\n",
                             ".map_chain_missing");
            builder_.SetInsertPoint(okBB);
            llvm::Value *valsPtrField = builder_.CreateStructGEP(mapHeaderTy_, containerPtr, 3, "map_vals_ptr");
            llvm::Value *valsPtr = builder_.CreateLoad(ptrTy_, valsPtrField, "map_vals");
            elemSlot = builder_.CreateGEP(mapValTy, valsPtr, {slot}, "map_val_elem_ptr");
            elemTy = mapValTy;
        } else {
            containerPtr = emitCowCheck(containerPtr, receiverAlloca, CollectionKind::List);
            elemTy = getListElementType(containerPtr);
            if (!elemTy)
                codegenError("cannot determine list element type for chained field assignment");
            llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, containerPtr, 0, "len_ptr");
            llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "length");
            emitBoundsCheck(indexVal, length,
                            "runtime error: index %lld out of bounds for list of length %lld\n",
                            ".idx_chain_err", "idx_chain");
            llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, containerPtr, 2, "data_ptr");
            llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataPtrField, "data");
            elemSlot = builder_.CreateGEP(elemTy, dataPtr, {indexVal}, "list_elem_ptr");
        }

        auto *structTy = llvm::dyn_cast<llvm::StructType>(elemTy);
        if (!structTy)
            codegenError("chained field assignment requires a struct element type");
        auto it = struct_types_.find(structTy->getName().str());
        if (it == struct_types_.end())
            codegenError("unknown struct type: " + structTy->getName().str());
        const auto &info = it->second;
        int fieldIdx = info.findField(s.field);
        if (fieldIdx < 0)
            codegenError("type '" + it->first + "' has no field '" + s.field + "'");
        llvm::Type *expectedTy = structTy->getElementType(fieldIdx);
        llvm::Value *curStruct = builder_.CreateLoad(elemTy, elemSlot, "elem_struct_cur");

        llvm::Value *newFieldVal = nullptr;
        if (s.compound_op) {
            llvm::Value *curField = builder_.CreateExtractValue(curStruct, fieldIdx, s.field + ".compound_cur");
            llvm::Value *rhs = emitExpr(*s.value);
            newFieldVal = applyCompoundOp(*s.compound_op, curField, rhs, *s.value,
                                           expectedTy, it->first + "." + s.field);
        } else {
            newFieldVal = emitExpr(*s.value);
            if (newFieldVal->getType() != expectedTy) {
                if (auto *sliced = tryEmitSubtypeCoerce(newFieldVal, expectedTy))
                    newFieldVal = sliced;
                else
                    codegenError("field '" + s.field + "' type mismatch");
            }
        }

        llvm::Value *updatedStruct = builder_.CreateInsertValue(
            curStruct, newFieldVal, fieldIdx, "elem_struct_upd");
        builder_.CreateStore(updatedStruct, elemSlot);
        emitInvariantCheck(it->first, info, updatedStruct);
        return;
    }

    codegenError("left side of field assignment is not an lvalue");
}

void CodeGen::rejectIfTypeNameTakenByOtherKind(const std::string &name) {
    if (struct_types_.count(name))
        codegenError("type '" + name + "' is already defined as a record");
    if (enum_types_.count(name))
        codegenError("type '" + name + "' is already defined as an enum");
    if (generic_enum_templates_.count(name))
        codegenError("type '" + name + "' is already defined as a generic enum");
    if (type_aliases_.count(name))
        codegenError("type '" + name + "' is already defined as a type alias");
}

void CodeGen::emitStmt(EnumStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;
    emitTraceSymbolDefine("enum", s.name, s.loc);
    // Generic enum: save as template, don't instantiate yet
    if (!s.type_params.empty()) {
        if (generic_enum_templates_.count(s.name))
            codegenError("generic enum '" + s.name + "' is already defined");
        rejectIfTypeNameTakenByOtherKind(s.name);
        GenericEnumTemplate tmpl;
        tmpl.name = s.name;
        tmpl.typeParams = s.type_params;
        tmpl.variants = std::move(s.variants);
        generic_enum_templates_[s.name] = std::move(tmpl);
        return;
    }

    if (enum_types_.count(s.name))
        codegenError("enum '" + s.name + "' is already defined");
    rejectIfTypeNameTakenByOtherKind(s.name);

    EnumInfo info;
    info.name = s.name;
    info.variantCount = s.variants.size();
    info.type_id = next_type_id_++;

    // Check if any variant has associated data
    bool hasADT = false;
    for (auto &v : s.variants) {
        if (!v.field_types.empty()) { hasADT = true; break; }
    }
    info.isADT = hasADT;

    // Check if any variant has explicit values
    bool hasExplicit = false;
    for (auto &v : s.variants) {
        if (v.explicit_value.has_value()) { hasExplicit = true; break; }
    }
    info.hasExplicitValues = hasExplicit;

    // Create global string array for variant names (for printing)
    std::vector<llvm::Constant*> nameStrings;
    nameStrings.reserve(s.variants.size());
    info.variantOrder.reserve(s.variants.size());
    std::unordered_set<int64_t> seenValues;
    for (size_t i = 0; i < s.variants.size(); ++i) {
        int64_t val = s.variants[i].explicit_value.value_or(static_cast<int64_t>(i));
        if (!seenValues.insert(val).second)
            codegenError("duplicate enum value " + std::to_string(val) + " in enum '" + s.name + "'");
        info.variants[s.variants[i].name] = val;
        info.variantOrder.push_back(s.variants[i].name);
        llvm::Constant *str = cachedGlobalString(
            s.variants[i].name, ".enum_" + s.name + "_" + s.variants[i].name);
        nameStrings.push_back(str);

        // Resolve field types for ADT variants
        if (!s.variants[i].field_types.empty()) {
            VariantFieldInfo vfi;
            for (auto &ft : s.variants[i].field_types) {
                std::string ftStr = ft->toString();
                vfi.fieldTypes.push_back(resolveType(ftStr));
                vfi.fieldTypeNames.push_back(ftStr);
            }
            info.variantFields[s.variants[i].name] = std::move(vfi);
        }
    }

    // Create global array of name pointers
    auto *arrTy = llvm::ArrayType::get(ptrTy_, s.variants.size());
    auto *init = llvm::ConstantArray::get(arrTy, nameStrings);
    auto *gv = new llvm::GlobalVariable(
        *mod_, arrTy, true, llvm::GlobalValue::PrivateLinkage,
        init, ".enum_names_" + s.name);
    info.nameArray = gv;

    // For ADT enums, create a struct type: { i64 tag, [maxPayloadSize x i8] }
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
            *ctx_, {i64Ty_, payloadTy}, "enum." + s.name);
    }

    enum_types_[s.name] = std::move(info);
}

void CodeGen::emitStmt(TupleDestructStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;
    emitCoverage(s.loc);
    validateDirectives(s.directives);
    llvm::Value *tupleVal = emitExpr(*s.value);
    llvm::StructType *structTy = llvm::dyn_cast<llvm::StructType>(tupleVal->getType());
    if (!structTy)
        codegenError("tuple destructuring requires a tuple value");
    if (structTy->getNumElements() != s.names.size())
        codegenError("tuple destructuring: expected " +
            std::to_string(s.names.size()) + " elements but got " +
            std::to_string(structTy->getNumElements()));

    for (size_t i = 0; i < s.names.size(); ++i) {
        if (s.names[i] == "_")
            continue;
        // Redeclaration check (consistent with emitVarDecl)
        if (scope_stack_.back().count(s.names[i]))
            codegenError("variable '" + s.names[i] + "' already declared in this scope");
        llvm::Value *elem = builder_.CreateExtractValue(tupleVal, i);
        llvm::AllocaInst *ptr = getOrCreateVar(s.names[i], elem->getType());
        builder_.CreateStore(elem, ptr);
        if (s.is_immutable)
            immutable_scope_stack_.back().insert(s.names[i]);
    }
}

void CodeGen::emitStmt(std::unique_ptr<IfStmt> &s) {
    emitCoverage(s->loc);
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "if.end", fn_);
    llvm::Value *cond = emitExpr(*s->branch.condition);
    cond = toBool(cond);
    emitTraceIfBranch(cond, s->loc);

    llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(*ctx_, "if.then", fn_);
    llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(*ctx_, "if.else", fn_);
    builder_.CreateCondBr(cond, thenBB, elseBB);

    builder_.SetInsertPoint(thenBB);
    pushScope();
    for (auto &stmt : s->branch.body)
        std::visit([this](auto &st) { emitStmt(st); }, stmt);
    popScope();
    if (!builder_.GetInsertBlock()->getTerminator())
        builder_.CreateBr(mergeBB);

    builder_.SetInsertPoint(elseBB);

    if (!s->else_body.empty()) {
        pushScope();
        for (auto &stmt : s->else_body)
            std::visit([this](auto &st) { emitStmt(st); }, stmt);
        popScope();
    }
    if (!builder_.GetInsertBlock()->getTerminator())
        builder_.CreateBr(mergeBB);

    builder_.SetInsertPoint(mergeBB);
}

void CodeGen::emitStmt(std::unique_ptr<WhenCondStmt> &s) {
    emitCoverage(s->loc);
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "when.end", fn_);
    int armIndex = 0;

    for (auto &arm : s->arms) {
        llvm::Value *cond = emitExpr(*arm.condition);
        cond = toBool(cond);

        llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(*ctx_, "when.then", fn_);
        llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "when.next", fn_);
        builder_.CreateCondBr(cond, thenBB, nextBB);

        builder_.SetInsertPoint(thenBB);
        emitTraceWhenBranch(armIndex++, s->loc);
        pushScope();
        for (auto &stmt : arm.body)
            std::visit([this](auto &st) { emitStmt(st); }, stmt);
        popScope();
        if (!builder_.GetInsertBlock()->getTerminator())
            builder_.CreateBr(mergeBB);

        builder_.SetInsertPoint(nextBB);
    }

    if (!s->else_body.empty()) {
        pushScope();
        emitTraceWhenBranch(-1, s->loc);
        for (auto &stmt : s->else_body)
            std::visit([this](auto &st) { emitStmt(st); }, stmt);
        popScope();
    }
    if (!builder_.GetInsertBlock()->getTerminator())
        builder_.CreateBr(mergeBB);

    builder_.SetInsertPoint(mergeBB);
}


void CodeGen::emitStmt(ImportStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;
    codegenError("unresolved import: " + s.module_path +
                             " (ModuleLoader should have resolved this)");
}

void CodeGen::emitStmt(IndexAssignStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;
    emitCoverage(s.loc);

    // Resolve the container's CoW anchor. Only plain `var[i] = v` forms
    // support CoW today: the anchor must be the alloca that holds the
    // container pointer so emitCowCheck can write-back a new pointer on
    // copy. For chained forms (`rec.field[i] = v`, `grid[i][j] = v`) the
    // container pointer is reached through a field extract or a nested
    // index load, so there is no single alloca slot to write-back to
    // without deep record / nested-collection CoW. Those cases are scoped
    // out to a follow-up issue — we fall through with anchor == nullptr so
    // emitCowCheck is a no-op and mutation happens in place on whatever
    // heap state is reached.
    llvm::AllocaInst *receiverAlloca = nullptr;
    if (std::get_if<VariableExpr>(&s.object->data)) {
        receiverAlloca = tryGetReceiverAlloca(*s.object);
    } else if (!std::holds_alternative<std::unique_ptr<FieldAccessExpr>>(s.object->data) &&
               !std::holds_alternative<std::unique_ptr<IndexExpr>>(s.object->data)) {
        codegenError("left side of index assignment is not an lvalue");
    }

    llvm::Value *objPtr = emitExpr(*s.object);

    llvm::SmallVector<llvm::Value*, 2> indexValues;
    for (auto &idx : s.indices)
        indexValues.push_back(emitExpr(*idx));

    // For non-compound assignment we can still route through a user-defined
    // `operator[]=` overload (which expects the replacement value directly
    // and bypasses the built-in list/map paths). Compound forms don't go
    // through the overload because the operand on the LHS is the live
    // element, not a plain rhs — let the built-in paths below do the
    // load-modify-store.
    llvm::Value *rhsVal = nullptr;
    if (!s.compound_op) {
        rhsVal = emitExpr(*s.value);
        if (trySubscriptAssignOperatorCall(objPtr, indexValues, rhsVal))
            return;
    }
    if (indexValues.size() > 1)
        codegenError("multi-index requires operator[]= overload");

    llvm::Value *key = indexValues[0];

    // Fixed-length array index assignment
    if (auto *ai = llvm::dyn_cast<llvm::AllocaInst>(objPtr)) {
        if (auto *arrTy = llvm::dyn_cast<llvm::ArrayType>(ai->getAllocatedType())) {
            llvm::Type *elemTy = arrTy->getElementType();
            uint64_t arrSize = arrTy->getNumElements();

            emitBoundsCheck(key, llvm::ConstantInt::get(i64Ty_, arrSize),
                            "runtime error: index %lld out of bounds for array of length %lld\n", ".arr_assign_err", "arr_assign");

            llvm::Value *elemPtr = builder_.CreateGEP(
                arrTy, ai, {llvm::ConstantInt::get(i64Ty_, 0), key}, "arr_assign_ptr");

            llvm::Value *val = nullptr;
            if (s.compound_op) {
                llvm::Value *oldVal = builder_.CreateLoad(elemTy, elemPtr, "arr_elem_cur");
                llvm::Value *rhs = emitExpr(*s.value);
                val = applyCompoundOp(*s.compound_op, oldVal, rhs, *s.value, elemTy, "array element");
            } else {
                val = rhsVal;
            }

            if (val->getType() != elemTy) {
                auto nit = array_elem_type_names_.find(ai);
                std::string tn = (nit != array_elem_type_names_.end()) ? nit->second : "i32";
                llvm::Value *coerced = coerceToLowLevelType(
                    val, elemTy, tn, "", "arr_assign_trunc");
                if (coerced) {
                    val = coerced;
                } else {
                    codegenError("array element type mismatch in index assignment");
                }
            }

            builder_.CreateStore(val, elemPtr);
            return;
        }
    }

    if (objPtr->getType() != ptrTy_)
        codegenError("index assignment requires list or map");

    llvm::Type *mapKeyTy = getMapKeyType(objPtr);
    if (mapKeyTy) {
        // CoW check for map index assignment
        objPtr = emitCowCheck(objPtr, receiverAlloca, CollectionKind::Map);

        // Map index assignment
        llvm::Type *mapValTy = getMapValueType(objPtr);
        if (!mapValTy)
            codegenError("cannot determine map value type");
        if (key->getType() != mapKeyTy)
            codegenError("map key type mismatch");

        // Determine whether the value type is itself an ARC-managed
        // collection so we know whether to release the previously-stored
        // pointer when overwriting an existing key (#855).
        CollectionKind mapValArcKind = CollectionKind::List;
        bool mapValIsArc = elementTypeIsArcManaged(objPtr, CollectionKind::Map, &mapValArcKind);

        // Compound assignment on a map requires the key to already exist —
        // an "insert default then apply op" behavior is not yet supported
        // and would silently paper over typos. Reject at runtime with a
        // clear message (#812 user decision).
        if (s.compound_op) {
            llvm::Value *idx = emitMapKeyLookup(objPtr, key, mapKeyTy);
            llvm::Value *found = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "found");
            auto *parentFn = builder_.GetInsertBlock()->getParent();
            auto *missBB = llvm::BasicBlock::Create(*ctx_, "map.compound_missing", parentFn);
            auto *okBB = llvm::BasicBlock::Create(*ctx_, "map.compound_ok", parentFn);
            builder_.CreateCondBr(found, okBB, missBB);
            builder_.SetInsertPoint(missBB);
            emitRuntimeError("runtime error: compound assignment to missing map key\n",
                             ".map_compound_missing");
            builder_.SetInsertPoint(okBB);
            llvm::Value *valsPtrField = builder_.CreateStructGEP(mapHeaderTy_, objPtr, 3, "map_vals_ptr");
            llvm::Value *valsPtr = builder_.CreateLoad(ptrTy_, valsPtrField, "map_vals");
            llvm::Value *valElemPtr = builder_.CreateGEP(mapValTy, valsPtr, {idx}, "val_elem_ptr");
            llvm::Value *oldVal = builder_.CreateLoad(mapValTy, valElemPtr, "map_val_cur");
            // Snapshot map_value_type_name before propagateTypeMeta: the
            // getOrCreateMeta inside it may rehash value_metadata_ and
            // invalidate the raw pointer from getMeta. See KNOWLEDGE.md
            // "Compound-op loaded slot values must propagate container
            // metadata" and #858.
            std::string mapValTypeName;
            if (auto *containerMeta = getMeta(objPtr))
                mapValTypeName = containerMeta->map_value_type_name;
            if (!mapValTypeName.empty())
                propagateTypeMeta(mapValTypeName, oldVal);
            llvm::Value *rhs = emitExpr(*s.value);
            llvm::Value *newVal = applyCompoundOp(*s.compound_op, oldVal, rhs, *s.value, mapValTy, "map element");
            // Compound result is a fresh alloc (never aliases the old
            // slot); reuse the pre-op load rather than re-loading (#812).
            if (mapValIsArc)
                emitArcReleaseLoadedElement(oldVal, mapValArcKind, "map_val_compound");
            builder_.CreateStore(newVal, valElemPtr);
            return;
        }

        if (rhsVal->getType() != mapValTy)
            codegenError("map value type mismatch");

        llvm::Value *idx = emitMapKeyLookup(objPtr, key, mapKeyTy);
        llvm::Value *found = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "found");

        llvm::BasicBlock *updateBB = llvm::BasicBlock::Create(*ctx_, "map.update", fn_);
        llvm::BasicBlock *insertBB = llvm::BasicBlock::Create(*ctx_, "map.insert", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "map.assign_end", fn_);

        builder_.CreateCondBr(found, updateBB, insertBB);

        builder_.SetInsertPoint(updateBB);
        llvm::Value *valsPtrField = builder_.CreateStructGEP(mapHeaderTy_, objPtr, 3, "map_vals_ptr");
        llvm::Value *valsPtr = builder_.CreateLoad(ptrTy_, valsPtrField, "map_vals");
        llvm::Value *valElemPtr = builder_.CreateGEP(mapValTy, valsPtr, {idx}, "val_elem_ptr");
        // Same retain-then-release protocol as the list path below; see
        // the explanation there. The insert branch stores into a fresh
        // slot and has no old value to release.
        if (mapValIsArc) {
            retainArcValue(rhsVal);
            llvm::Value *oldVal = builder_.CreateLoad(ptrTy_, valElemPtr, "map_val_arc_old");
            emitArcReleaseLoadedElement(oldVal, mapValArcKind, "map_val");
        }
        builder_.CreateStore(rhsVal, valElemPtr);
        builder_.CreateBr(endBB);

        builder_.SetInsertPoint(insertBB);
        llvm::Value *lenPtr = builder_.CreateStructGEP(mapHeaderTy_, objPtr, 0, "map_len_ptr");
        llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "map_len");
        llvm::Value *capPtr = builder_.CreateStructGEP(mapHeaderTy_, objPtr, 1, "map_cap_ptr");
        llvm::Value *cap = builder_.CreateLoad(i64Ty_, capPtr, "map_cap");

        // Check if we need to grow
        llvm::Value *needGrow = builder_.CreateICmpEQ(length, cap, "need_grow");
        llvm::BasicBlock *growBB = llvm::BasicBlock::Create(*ctx_, "map.grow", fn_);
        llvm::BasicBlock *storeBB = llvm::BasicBlock::Create(*ctx_, "map.store", fn_);
        builder_.CreateCondBr(needGrow, growBB, storeBB);

        // Grow: realloc keys and values arrays
        builder_.SetInsertPoint(growBB);
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t keySize = dl.getTypeAllocSize(mapKeyTy);
        uint64_t valSize = dl.getTypeAllocSize(mapValTy);

        llvm::Value *newCap = builder_.CreateMul(cap, llvm::ConstantInt::get(i64Ty_, 2), "new_cap");

        auto mallocFn = getStdlibMalloc();

        // New keys array
        llvm::Value *newKeySize = builder_.CreateMul(newCap, llvm::ConstantInt::get(i64Ty_, keySize), "new_key_size");
        llvm::Value *newKeysPtr = builder_.CreateCall(mallocFn, {newKeySize}, "new_keys");

        // New values array
        llvm::Value *newValSize = builder_.CreateMul(newCap, llvm::ConstantInt::get(i64Ty_, valSize), "new_val_size");
        llvm::Value *newValsPtr = builder_.CreateCall(mallocFn, {newValSize}, "new_vals");

        // memcpy old data
        auto memcpyFn = getStdlibMemcpy();

        llvm::Value *keysPtrField2 = builder_.CreateStructGEP(mapHeaderTy_, objPtr, 2, "keys_field");
        llvm::Value *oldKeysPtr = builder_.CreateLoad(ptrTy_, keysPtrField2, "old_keys");
        llvm::Value *oldKeySize = builder_.CreateMul(length, llvm::ConstantInt::get(i64Ty_, keySize), "old_key_size");
        builder_.CreateCall(memcpyFn, {newKeysPtr, oldKeysPtr, oldKeySize});

        llvm::Value *valsPtrField2 = builder_.CreateStructGEP(mapHeaderTy_, objPtr, 3, "vals_field");
        llvm::Value *oldValsPtr = builder_.CreateLoad(ptrTy_, valsPtrField2, "old_vals");
        llvm::Value *oldValSize = builder_.CreateMul(length, llvm::ConstantInt::get(i64Ty_, valSize), "old_val_size");
        builder_.CreateCall(memcpyFn, {newValsPtr, oldValsPtr, oldValSize});

        // Free old arrays
        auto freeFn = getStdlibFree();
        builder_.CreateCall(freeFn, {oldKeysPtr});
        builder_.CreateCall(freeFn, {oldValsPtr});

        // Update header pointers and capacity
        builder_.CreateStore(newKeysPtr, keysPtrField2);
        builder_.CreateStore(newValsPtr, valsPtrField2);
        builder_.CreateStore(newCap, capPtr);

        builder_.CreateBr(storeBB);

        // Store new key-value at index = length
        builder_.SetInsertPoint(storeBB);
        llvm::Value *curLen = builder_.CreateLoad(i64Ty_, lenPtr, "cur_len");
        llvm::Value *keysPtrField3 = builder_.CreateStructGEP(mapHeaderTy_, objPtr, 2, "keys_field3");
        llvm::Value *curKeysPtr = builder_.CreateLoad(ptrTy_, keysPtrField3, "cur_keys");
        llvm::Value *newKeyPtr = builder_.CreateGEP(mapKeyTy, curKeysPtr, {curLen}, "new_key_ptr");
        builder_.CreateStore(key, newKeyPtr);

        llvm::Value *valsPtrField3 = builder_.CreateStructGEP(mapHeaderTy_, objPtr, 3, "vals_field3");
        llvm::Value *curValsPtr = builder_.CreateLoad(ptrTy_, valsPtrField3, "cur_vals");
        llvm::Value *newValPtr = builder_.CreateGEP(mapValTy, curValsPtr, {curLen}, "new_val_ptr");
        builder_.CreateStore(rhsVal, newValPtr);

        // length++
        llvm::Value *newLen = builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1), "new_len");
        builder_.CreateStore(newLen, lenPtr);

        // Insert into hash table buckets and check rehash
        emitBucketInsertAndRehashCheck(objPtr, mapHeaderTy_, kMapLayout.lenIdx, kMapLayout.bucketCountIdx, kMapLayout.bucketsPtrIdx, key, mapKeyTy, curLen);

        builder_.CreateBr(endBB);

        builder_.SetInsertPoint(endBB);
        return;
    }

    // List index assignment
    objPtr = emitCowCheck(objPtr, receiverAlloca, CollectionKind::List);
    llvm::Type *elemTy = getListElementType(objPtr);
    if (!elemTy)
        codegenError("cannot determine list element type for index assignment");

    llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, objPtr, 0, "len_ptr");
    llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "length");

    emitBoundsCheck(key, length,
                    "runtime error: index %lld out of bounds for list of length %lld\n", ".idx_assign_err", "idx_assign");
    llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, objPtr, 2, "data_ptr");
    llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataPtrField, "data");
    llvm::Value *elemPtr = builder_.CreateGEP(elemTy, dataPtr, {key}, "elem_ptr");

    // Determine whether the element type owns an ARC allocation per slot.
    // For ARC-managed nested collection elements (`List<List<T>>` etc.) the
    // slot holds the sole strong ref to the inner collection — overwriting
    // it without releasing the old pointer leaks the inner allocation.
    // Records and record fields with ARC fields are intentionally excluded
    // (different fix path; tracked separately).
    CollectionKind elemArcKind = CollectionKind::List;
    bool elemIsArc = elementTypeIsArcManaged(objPtr, CollectionKind::List, &elemArcKind);

    llvm::Value *finalVal = nullptr;
    llvm::Value *compoundOldVal = nullptr;  // captured for the ARC release path
    if (s.compound_op) {
        compoundOldVal = builder_.CreateLoad(elemTy, elemPtr, "list_elem_cur");
        // Snapshot list_elem_type_name before propagateTypeMeta: the
        // getOrCreateMeta inside it may rehash value_metadata_ and
        // invalidate the raw pointer from getMeta. See KNOWLEDGE.md
        // "Compound-op loaded slot values must propagate container
        // metadata" and #858.
        std::string elemTypeName;
        if (auto *containerMeta = getMeta(objPtr))
            elemTypeName = containerMeta->list_elem_type_name;
        if (!elemTypeName.empty())
            propagateTypeMeta(elemTypeName, compoundOldVal);
        llvm::Value *rhs = emitExpr(*s.value);
        finalVal = applyCompoundOp(*s.compound_op, compoundOldVal, rhs, *s.value, elemTy, "list element");
    } else {
        finalVal = rhsVal;
    }
    if (finalVal->getType() != elemTy) {
        if (auto *sliced = tryEmitSubtypeCoerce(finalVal, elemTy))
            finalVal = sliced;
        else
            codegenError("list element type mismatch in index assignment");
    }

    // Retain-then-release-old on slot overwrite (#855). Mirrors the
    // canonical pattern in `emitStmt(AssignStmt&)` adapted to a heap slot.
    //
    // Plain form: retain first so self-assignment `xs[i] = xs[i]` cannot
    // transiently drop the refcount to zero. `retainArcValue` handles
    // both the `tryRetainArcSource` fast path (LoadInst from an alloca,
    // arc_owned literals) and the manual header-based fallback for
    // GEP-loaded values (cross-slot copies like `xs[i] = ys[j]`).
    //
    // Compound form: reuse `compoundOldVal` already loaded for the op
    // rather than re-loading — preserves the "evaluate LHS slot exactly
    // once" rule (#812). The compound op produces a fresh ARC allocation
    // that never aliases the slot, so no retain of the new value is
    // needed.
    if (elemIsArc) {
        if (s.compound_op) {
            emitArcReleaseLoadedElement(compoundOldVal, elemArcKind, "list_elem_compound");
        } else {
            retainArcValue(finalVal);
            llvm::Value *oldVal = builder_.CreateLoad(ptrTy_, elemPtr, "list_elem_arc_old");
            emitArcReleaseLoadedElement(oldVal, elemArcKind, "list_elem");
        }
    }

    builder_.CreateStore(finalVal, elemPtr);
}

} // namespace ry
