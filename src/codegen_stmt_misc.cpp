#include "ry/codegen.hpp"


namespace ry {

// Recursive predicate: does `tn` embed `target` inline, without going through
// a pointer-backed indirection? Used by the `emitStmt(EnumStmt)` pre-pass to
// reject self-referential ADT variants whose payload struct would be of
// infinite size. Indirection wrappers (`List<T>`, `Map<K,V>`, `Set<T>`,
// `Task<T>`, `Channel<T>`, `weak T`, `fn(...) -> T`) stop the descent
// — their runtime representation is a pointer so embedding `target` through
// them keeps the outer layout finite.
static bool containsInlineSelfReference(const TypeNode &tn,
                                        const std::string &target) {
    return std::visit(
        [&](auto &alt) -> bool {
            using T = std::decay_t<decltype(alt)>;
            if constexpr (std::is_same_v<T, BasicType>) {
                return alt.name == target;
            } else if constexpr (std::is_same_v<T, GenericType>) {
                if (alt.name == target) return true;
                if (alt.name == "List" || alt.name == "Map" ||
                    alt.name == "Set"  || alt.name == "Task" ||
                    alt.name == "Channel")
                    return false;
                for (auto &arg : alt.type_args)
                    if (containsInlineSelfReference(*arg, target)) return true;
                return false;
            } else if constexpr (std::is_same_v<T, OptionalType>) {
                return containsInlineSelfReference(*alt.inner, target);
            } else if constexpr (std::is_same_v<T, TupleType>) {
                for (auto &e : alt.elements)
                    if (containsInlineSelfReference(*e, target)) return true;
                return false;
            } else if constexpr (std::is_same_v<T, UnionType>) {
                for (auto &c : alt.components)
                    if (containsInlineSelfReference(*c, target)) return true;
                return false;
            } else if constexpr (std::is_same_v<T, ArrayType>) {
                return containsInlineSelfReference(*alt.element_type, target);
            } else {
                // WeakType, FnType, RangeType: pointer-backed or non-structural.
                return false;
            }
        },
        tn.data);
}

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
                                          const std::string &elemTypeName,
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
    if (elemKind == CollectionKind::Str) {
        // str element: header is at handle-STRING_HEADER_SIZE (24); no inner destructor.
        auto *oldHdr = emitStrGetHeaderFromData(oldElemVal);
        emitArcRelease(oldHdr, /*atomic=*/false, /*destructor=*/{}, /*gcVisitFn=*/nullptr);
    } else {
        auto *oldHdr = emitArcGetHeaderFromData(oldElemVal);
        // Element slots in lists/maps/sets are not marked atomic; collections
        // themselves are not @atomic by default. Resolve the inner type
        // signatures from `elemTypeName` so that the destructor for the inner
        // collection releases its own str/collection elements (#1108).
        std::string innerElemSig;
        std::string innerValSig;
        if (!elemTypeName.empty()) {
            std::string resolved = resolveTypeAlias(elemTypeName);
            std::string head;
            std::vector<std::string> innerArgs;
            if (splitGenericTypeName(resolved, head, innerArgs)) {
                if ((elemKind == CollectionKind::List || elemKind == CollectionKind::Set) &&
                    !innerArgs.empty()) {
                    innerElemSig = resolveTypeAlias(innerArgs[0]);
                } else if (elemKind == CollectionKind::Map && innerArgs.size() >= 2) {
                    innerElemSig = resolveTypeAlias(innerArgs[0]);
                    innerValSig  = resolveTypeAlias(innerArgs[1]);
                }
            }
        }
        emitArcRelease(oldHdr, /*atomic=*/false,
                       getOrCreateCollectionDestructor(elemKind, innerElemSig, innerValSig),
                       /*gcVisitFn=*/nullptr);
    }
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
        } else if (targetTy == i64Ty_ && result->getType() == f64Ty_ &&
                   getLowLevelTypeName(currentVal).empty()) {
            result = emitCheckedFPToInt(result, i64Ty_, "int", contextName + ".f64toi64");
        } else if (targetTy == f64Ty_ && result->getType() == i64Ty_ &&
                   getLowLevelTypeName(currentVal).empty()) {
            result = builder_.CreateSIToFP(result, f64Ty_, contextName + ".i64tof64");
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
            codegenError("internal: chained field assignment produced mismatched record type for '" + ve->name + "'");
        builder_.CreateStore(newInnerVal, storagePtr);
        if (auto *rootSt = llvm::dyn_cast<llvm::StructType>(varTy)) {
            auto rit = record_types_.find(rootSt->getName().str());
            if (rit != record_types_.end())
                emitInvariantCheck(rit->first, rit->second, newInnerVal);
        }
        return;
    }

    if (auto *faPtr = std::get_if<std::unique_ptr<FieldAccessExpr>>(&chainTarget.data)) {
        FieldAccessExpr *fa = faPtr->get();
        llvm::Value *outerVal = emitExpr(*fa->object);
        auto *outerSt = llvm::dyn_cast<llvm::StructType>(outerVal->getType());
        if (!outerSt)
            codegenError("field access on non-record type in chained assignment");
        auto it = record_types_.find(outerSt->getName().str());
        if (it == record_types_.end())
            codegenError("unknown record type: " + outerSt->getName().str());
        int idx = it->second.findField(fa->field);
        if (idx < 0)
            codegenError("type '" + it->first + "' has no field '" + fa->field + "'");
        llvm::Type *expectedTy = outerSt->getElementType(static_cast<unsigned>(idx));
        if (newInnerVal->getType() != expectedTy) {
            if (auto *sliced = tryEmitSubtypeCoerce(newInnerVal, expectedTy))
                newInnerVal = sliced;
            else
                codegenError("field '" + fa->field + "' type mismatch in chained assignment");
        }
        llvm::Value *updated = builder_.CreateInsertValue(outerVal, newInnerVal, static_cast<unsigned>(idx), "chain_upd");
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

    // Retain-then-release-old on field overwrite, mirroring the canonical
    // slot-overwrite pattern adapted to a struct field reached via
    // ExtractValue. Order matters for self-assignment `r.f = r.f`: retain
    // new before releasing old so the refcount can never transiently drop
    // to zero. Plain form retains the new value before releasing the old;
    // compound form reuses `currentField` (already extracted for
    // applyCompoundOp) because the compound op yields a fresh allocation
    // that cannot alias the old field.
    auto emitArcFieldReleaseOnOverwrite = [&](llvm::Value *structVal,
                                               int fieldIdx,
                                               CollectionKind fieldArcKind,
                                               const std::string &fieldTypeName,
                                               llvm::Value *currentField,
                                               llvm::Value *newFieldVal,
                                               const std::string &label) {
        if (s.compound_op) {
            emitArcReleaseLoadedElement(currentField, fieldArcKind, fieldTypeName,
                                         label + "_compound");
        } else {
            retainArcValue(newFieldVal);
            llvm::Value *oldField = builder_.CreateExtractValue(
                structVal, static_cast<unsigned>(fieldIdx), s.field + ".arc_old");
            emitArcReleaseLoadedElement(oldField, fieldArcKind, fieldTypeName, label);
        }
    };

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
            codegenError("field assignment on non-record type");
        std::string typeName = structTy->getName().str();
        auto it = record_types_.find(typeName);
        if (it == record_types_.end())
            codegenError("unknown record type: " + typeName);
        const auto &info = it->second;
        int fieldIdx = info.findField(s.field);
        if (fieldIdx < 0)
            codegenError("type '" + typeName + "' has no field '" + s.field + "'");
        llvm::Type *expectedTy = structTy->getElementType(static_cast<unsigned>(fieldIdx));

        // Determine whether the field's declared type owns an ARC allocation
        // per slot (non-weak List/Map/Set). If so, we release the old field
        // pointer before the InsertValue+Store overwrite (#857).
        std::string fieldTypeName;
        if (info.fields[static_cast<size_t>(fieldIdx)].type)
            fieldTypeName = info.fields[static_cast<size_t>(fieldIdx)].type->toString();
        CollectionKind fieldArcKind = CollectionKind::List;
        bool fieldIsArc = fieldTypeIsArcManaged(fieldTypeName, &fieldArcKind);

        llvm::Value *newFieldVal = nullptr;
        llvm::Value *currentStruct = builder_.CreateLoad(varTy, storagePtr, "record_cur");
        llvm::Value *currentField = nullptr;  // extracted iff compound_op; reused for ARC release
        if (s.compound_op) {
            currentField = builder_.CreateExtractValue(
                currentStruct, static_cast<unsigned>(fieldIdx), s.field + ".compound_cur");
            // Propagate the field's declared type onto the extracted SSA
            // value so downstream emitArithmeticOp can dispatch list concat
            // / map / set metadata correctly. The bare ExtractValue result
            // is metadata-less, and emitArithmeticOp keys dispatch on
            // TypeMeta::* slots on the SSA value — not on hint parameters.
            if (!fieldTypeName.empty())
                propagateTypeMeta(fieldTypeName, currentField);
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

        if (fieldIsArc)
            emitArcFieldReleaseOnOverwrite(currentStruct, fieldIdx, fieldArcKind, fieldTypeName,
                                            currentField, newFieldVal,
                                            varExpr->name + "." + s.field);

        llvm::Value *updated = builder_.CreateInsertValue(
            currentStruct, newFieldVal, static_cast<unsigned>(fieldIdx), "record_upd");
        builder_.CreateStore(updated, storagePtr);
        emitInvariantCheck(typeName, info, updated);
        return;
    }

    if (std::holds_alternative<std::unique_ptr<FieldAccessExpr>>(s.object->data)) {
        llvm::Value *innerStruct = emitExpr(*s.object);
        auto *innerSt = llvm::dyn_cast<llvm::StructType>(innerStruct->getType());
        if (!innerSt)
            codegenError("field assignment on non-record type");
        auto it = record_types_.find(innerSt->getName().str());
        if (it == record_types_.end())
            codegenError("unknown record type: " + innerSt->getName().str());
        const auto &info = it->second;
        int fieldIdx = info.findField(s.field);
        if (fieldIdx < 0)
            codegenError("type '" + it->first + "' has no field '" + s.field + "'");
        llvm::Type *expectedTy = innerSt->getElementType(static_cast<unsigned>(fieldIdx));

        // ARC release on the deepest field of the chain. Middle hops of a
        // `writeBackFieldChain` walk are shallow InsertValue updates that do
        // not overwrite any existing slot, so releasing only this field is
        // sufficient for #857. Deep-chain ARC ownership through middle hops
        // is tracked with #854 deep-CoW work.
        std::string fieldTypeName;
        if (info.fields[static_cast<size_t>(fieldIdx)].type)
            fieldTypeName = info.fields[static_cast<size_t>(fieldIdx)].type->toString();
        CollectionKind fieldArcKind = CollectionKind::List;
        bool fieldIsArc = fieldTypeIsArcManaged(fieldTypeName, &fieldArcKind);

        llvm::Value *newFieldVal = nullptr;
        llvm::Value *currentField = nullptr;
        if (s.compound_op) {
            currentField = builder_.CreateExtractValue(
                innerStruct, static_cast<unsigned>(fieldIdx), s.field + ".compound_cur");
            // Propagate the field's declared type onto the extracted SSA value.
            // See the VariableExpr branch above and #862 for the rationale.
            if (!fieldTypeName.empty())
                propagateTypeMeta(fieldTypeName, currentField);
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

        if (fieldIsArc)
            emitArcFieldReleaseOnOverwrite(innerStruct, fieldIdx, fieldArcKind, fieldTypeName,
                                            currentField, newFieldVal,
                                            it->first + "." + s.field);

        llvm::Value *updatedInner = builder_.CreateInsertValue(
            innerStruct, newFieldVal, static_cast<unsigned>(fieldIdx), "nested_upd");
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
            codegenError("chained field assignment requires a record element type");
        auto it = record_types_.find(structTy->getName().str());
        if (it == record_types_.end())
            codegenError("unknown record type: " + structTy->getName().str());
        const auto &info = it->second;
        int fieldIdx = info.findField(s.field);
        if (fieldIdx < 0)
            codegenError("type '" + it->first + "' has no field '" + s.field + "'");
        llvm::Type *expectedTy = structTy->getElementType(static_cast<unsigned>(fieldIdx));
        llvm::Value *curStruct = builder_.CreateLoad(elemTy, elemSlot, "elem_record_cur");

        // ARC release on the struct field loaded from a heap element slot
        // (`list[i].field = v` etc.). Same protocol as the other branches.
        std::string fieldTypeName;
        if (info.fields[static_cast<size_t>(fieldIdx)].type)
            fieldTypeName = info.fields[static_cast<size_t>(fieldIdx)].type->toString();
        CollectionKind fieldArcKind = CollectionKind::List;
        bool fieldIsArc = fieldTypeIsArcManaged(fieldTypeName, &fieldArcKind);

        llvm::Value *newFieldVal = nullptr;
        llvm::Value *curField = nullptr;
        if (s.compound_op) {
            curField = builder_.CreateExtractValue(curStruct, static_cast<unsigned>(fieldIdx), s.field + ".compound_cur");
            // Propagate the field's declared type onto the extracted SSA value.
            // See the VariableExpr branch above and #862 for the rationale.
            if (!fieldTypeName.empty())
                propagateTypeMeta(fieldTypeName, curField);
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

        if (fieldIsArc)
            emitArcFieldReleaseOnOverwrite(curStruct, fieldIdx, fieldArcKind, fieldTypeName,
                                            curField, newFieldVal,
                                            it->first + "." + s.field);

        llvm::Value *updatedStruct = builder_.CreateInsertValue(
            curStruct, newFieldVal, static_cast<unsigned>(fieldIdx), "elem_record_upd");
        builder_.CreateStore(updatedStruct, elemSlot);
        emitInvariantCheck(it->first, info, updatedStruct);
        return;
    }

    codegenError("left side of field assignment is not an lvalue");
}

void CodeGen::rejectIfTypeNameTakenByOtherKind(const std::string &name) {
    if (record_types_.count(name))
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

    // Reject direct inline self-reference in variant fields. Recurses through
    // inline layouts (`T?`, tuples, unions, arrays, and generic applications
    // that are not pointer-backed) so `enum Tree: Node(Tree?)`, `Node((Tree,
    // Tree))`, and `Node(Option<Tree>)` all get the same diagnostic as
    // `Node(Tree)`. Stops at true indirections (`List`, `Map`, `Set`, `Task`,
    // `Channel`, `weak T`, `fn(...)`) — those store the payload behind
    // a pointer so the enum layout stays finite. The recommendation message
    // below only suggests container indirections (`List`/`Map`/`Set`/`Task`/
    // `Channel`): `weak T` is omitted because it requires an ARC-managed
    // payload (a separate diagnostic rejects `weak <ADT>`), and `fn(...)` is
    // omitted as an unusual choice for data storage. Must run before the
    // generic-template save below so `generic_enum_templates_` never stores a
    // self-ref template that would crash at instantiation with `unknown
    // type: T`. Auto-boxing is the proper fix for the general case and is
    // tracked as a follow-up.
    for (auto &v : s.variants) {
        for (auto &ft : v.field_types) {
            if (!containsInlineSelfReference(*ft, s.name)) continue;

            std::string qualifiedName = s.name;
            if (!s.type_params.empty()) {
                qualifiedName += "<";
                for (size_t i = 0; i < s.type_params.size(); ++i) {
                    if (i) qualifiedName += ", ";
                    qualifiedName += s.type_params[i].name;
                }
                qualifiedName += ">";
            }
            std::string msg = "enum '";
            msg += s.name;
            msg += "' contains a direct self-referential field in variant '";
            msg += v.name;
            msg += "' which would require infinite storage; ";
            msg += "wrap the field in an indirection type such as ";
            msg += "`List<"; msg += qualifiedName; msg += ">`, ";
            msg += "`Map<K, "; msg += qualifiedName; msg += ">`, ";
            msg += "`Set<"; msg += qualifiedName; msg += ">`, ";
            msg += "`Task<"; msg += qualifiedName; msg += ">`, or ";
            msg += "`Channel<"; msg += qualifiedName; msg += ">`";
            codegenError(msg);
        }
    }

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
    validateDirectives(s.directives, DirectiveTarget::Statement);

    // Redeclaration check (compile-time, shared by tuple and list dispatch).
    // Hoisted above emitExpr so a duplicate name aborts before any IR or RHS
    // side effects are emitted. Also rejects same-pattern duplicates like
    // `a, a = ...` whose two stores would silently race on the same alloca.
    std::unordered_set<std::string> seen_names;
    for (size_t i = 0; i < s.names.size(); ++i) {
        if (s.names[i] == "_") continue;
        if (scope_stack_.back().count(s.names[i]))
            codegenError("variable '" + s.names[i] + "' already declared in this scope");
        if (!seen_names.insert(s.names[i]).second)
            codegenError("variable '" + s.names[i] + "' bound twice in destructuring pattern");
    }

    llvm::Value *val = emitExpr(*s.value);

    // List destructuring path: runtime length check + ARC-retained element loads.
    // Selected when the RHS resolves to a List<T> value. The left-hand syntax
    // (bare `a, b = ...` or paren `(a, b) = ...`) is identical to tuple form;
    // dispatch is value-driven.
    if (llvm::Type *elemTy = getListElementType(val)) {
        ListFields lf = loadListHeader(val, "destruct");
        llvm::Value *expected = llvm::ConstantInt::get(i64Ty_, s.names.size());
        llvm::Value *mismatch = builder_.CreateICmpNE(lf.len, expected, "destruct_len_mismatch");
        llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, "destruct.len_err", fn_);
        llvm::BasicBlock *okBB  = llvm::BasicBlock::Create(*ctx_, "destruct.ok", fn_);
        builder_.CreateCondBr(mismatch, errBB, okBB);

        builder_.SetInsertPoint(errBB);
        emitRuntimeError(
            "runtime error: list destructuring expected %lld elements but got %lld\n",
            ".list_destruct_len_err", {expected, lf.len});

        builder_.SetInsertPoint(okBB);

        // Snapshot source list metadata once into stack locals: same for every
        // element, and rehash-safe across the per-element propagateTypeMeta /
        // getOrCreateMeta calls that follow (see #858 gotcha).
        std::string elemTypeName;
        std::optional<FnTypeInfo> elemFnTypeInfo;
        bool listElemIsStr = false;
        llvm::Type *nestedElemTy = nullptr;
        if (auto *listMeta = getMeta(val)) {
            elemTypeName   = listMeta->list_elem_type_name;
            elemFnTypeInfo = listMeta->list_elem_fn_type_info;
            listElemIsStr  = listMeta->list_elem_is_str;
            if (elemTy == ptrTy_) nestedElemTy = listMeta->nested_list_elem;
        }

        for (size_t i = 0; i < s.names.size(); ++i) {
            if (s.names[i] == "_") continue;
            llvm::Value *idx = llvm::ConstantInt::get(i64Ty_, i);
            llvm::Value *elemPtr = builder_.CreateGEP(elemTy, lf.data, {idx},
                                                       "destruct_elem_ptr_" + std::to_string(i));
            llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr,
                                                     "destruct_elem_" + std::to_string(i));

            // Stamp metadata on the loaded SSA value so tryRetainArcSource Case 4
            // can detect ARC-managed elements via list_elem / map_key / set_elem
            // (#1266 metadata gate). Without this stamp the retain is skipped and
            // the rebind-source UAF protection is lost.
            if (elemTy == ptrTy_) {
                if (nestedElemTy)
                    setTypeMeta(TypeMeta::ListElem, elem, nestedElemTy);
                if (!elemTypeName.empty())
                    propagateTypeMeta(elemTypeName, elem);
                if (elemFnTypeInfo)
                    getOrCreateMeta(elem).fn_type_info = *elemFnTypeInfo;
                if (listElemIsStr)
                    getOrCreateMeta(elem).str_elem = true;
            }

            llvm::AllocaInst *ptr = getOrCreateVar(s.names[i], elem->getType());
            builder_.CreateStore(elem, ptr);

            // Propagate metadata onto the destination alloca so subsequent loads
            // (e.g. `a[0]` after `a, b = xs`) inherit element type information
            // through getMeta's LoadInst→PointerOperand resolution.
            if (elemTy == ptrTy_) {
                if (nestedElemTy)
                    setTypeMeta(TypeMeta::ListElem, ptr, nestedElemTy);
                if (!elemTypeName.empty())
                    propagateTypeMeta(elemTypeName, ptr);
                if (elemFnTypeInfo)
                    getOrCreateMeta(ptr).fn_type_info = *elemFnTypeInfo;
            }

            if (elemTy == ptrTy_ && tryRetainArcSource(elem)) {
                if (auto *meta = getMeta(elem); meta && meta->str_elem)
                    arc_str_managed_vars_.insert(ptr);
                else
                    arc_backed_vars_.insert(ptr);
                markArcManaged(ptr);
            }

            if (s.is_immutable)
                immutable_scope_stack_.back().insert(s.names[i]);
        }
        return;
    }

    llvm::StructType *structTy = llvm::dyn_cast<llvm::StructType>(val->getType());
    if (!structTy)
        codegenError("destructuring requires a tuple or list value");
    if (structTy->getNumElements() != s.names.size())
        codegenError("tuple destructuring: expected " +
            std::to_string(s.names.size()) + " elements but got " +
            std::to_string(structTy->getNumElements()));

    for (size_t i = 0; i < s.names.size(); ++i) {
        if (s.names[i] == "_")
            continue;
        llvm::Value *elem = builder_.CreateExtractValue(val, static_cast<unsigned>(i));
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

void CodeGen::emitStmt(std::unique_ptr<CaseCondStmt> &s) {
    emitCoverage(s->loc);
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "case.end", fn_);
    int armIndex = 0;

    for (auto &arm : s->arms) {
        llvm::Value *cond = emitExpr(*arm.condition);
        cond = toBool(cond);

        llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(*ctx_, "case.then", fn_);
        llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "case.next", fn_);
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

    // Resolve the container's CoW strategy.
    //
    // For plain `var[i] = v` we keep the existing 1-hop CoW via
    // `emitCowCheck(ptr, alloca, kind)` — cheap, preserves the shallow
    // top-level aliasing semantics tested by `tests/spec/cow.test.ry`.
    //
    // For chained forms (`rec.field[i] = v`, `grid[i][j] = v`, nested
    // map index, etc.) we walk the LHS chain inside-out via
    // `emitPathCowForChain(*s.object)` (#854). Path CoW privatizes every
    // level whose strong_count > 1 and returns the privatized leaf
    // container pointer. We then run the usual list/map mutation body
    // against that privatized container with `receiverAlloca == nullptr`
    // so the in-body `emitCowCheck` call becomes a no-op (privatization
    // already happened).
    llvm::AllocaInst *receiverAlloca = nullptr;
    llvm::Value *objPtr = nullptr;
    if (std::get_if<VariableExpr>(&s.object->data)) {
        receiverAlloca = tryGetReceiverAlloca(*s.object);
        objPtr = emitExpr(*s.object);
    } else if (std::holds_alternative<std::unique_ptr<FieldAccessExpr>>(s.object->data) ||
               std::holds_alternative<std::unique_ptr<IndexExpr>>(s.object->data)) {
        // Path CoW handles the chain walk and returns the privatized
        // leaf container pointer directly — do NOT also call emitExpr
        // on s.object, that would re-load the chain against stale
        // parents.
        objPtr = emitPathCowForChain(*s.object);
    } else {
        codegenError("left side of index assignment is not an lvalue");
    }

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
                auto nit = array_elem_type_names_.find(ai);
                if (nit != array_elem_type_names_.end() && !nit->second.empty())
                    propagateTypeMeta(nit->second, oldVal);
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

        // Snapshot map_value_type_name before any propagateTypeMeta call:
        // getOrCreateMeta inside it may rehash value_metadata_ and
        // invalidate a raw pointer from getMeta. Extracted here (before
        // the compound/plain branch) so both paths can pass it to
        // emitArcReleaseLoadedElement for the specialized str-aware
        // destructor.
        std::string mapValTypeName;
        if (auto *containerMeta = getMeta(objPtr))
            mapValTypeName = containerMeta->map_value_type_name;

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
            if (!mapValTypeName.empty())
                propagateTypeMeta(mapValTypeName, oldVal);
            llvm::Value *rhs = emitExpr(*s.value);
            llvm::Value *newVal = applyCompoundOp(*s.compound_op, oldVal, rhs, *s.value, mapValTy, "map element");
            // Compound result is a fresh alloc (never aliases the old
            // slot); reuse the pre-op load rather than re-loading (#812).
            if (mapValIsArc)
                emitArcReleaseLoadedElement(oldVal, mapValArcKind, mapValTypeName, "map_val_compound");
            builder_.CreateStore(newVal, valElemPtr);
            return;
        }

        if (rhsVal->getType() != mapValTy) {
            if (isAnyType(mapValTy))
                rhsVal = wrapInAny(rhsVal);
            else if (isAnyType(rhsVal->getType()) && canAnyHoldType(mapValTy))
                rhsVal = unwrapFromAny(rhsVal, mapValTy);
            else
                codegenError("map value type mismatch");
        }

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
            emitArcReleaseLoadedElement(oldVal, mapValArcKind, mapValTypeName, "map_val");
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

        // Store new key-value at index = length.  Retain to give the map
        // an independent strong reference (#1242).  `retainArcValue` routes
        // str through the StringHeader path via `arc_str_owned_values_`, so
        // str keys/values must also be retained here (the old #1266 carve-out
        // was destructor-only).
        builder_.SetInsertPoint(storeBB);
        llvm::Value *curLen = builder_.CreateLoad(i64Ty_, lenPtr, "cur_len");
        llvm::Value *keysPtrField3 = builder_.CreateStructGEP(mapHeaderTy_, objPtr, 2, "keys_field3");
        llvm::Value *curKeysPtr = builder_.CreateLoad(ptrTy_, keysPtrField3, "cur_keys");
        llvm::Value *newKeyPtr = builder_.CreateGEP(mapKeyTy, curKeysPtr, {curLen}, "new_key_ptr");
        if (mapKeyTy == ptrTy_) {
            std::string mapKeyTypeName;
            if (auto *containerMeta = getMeta(objPtr))
                mapKeyTypeName = containerMeta->map_key_type_name;
            CollectionKind mapKeyArcKind = CollectionKind::Str;
            if (!mapKeyTypeName.empty() &&
                fieldTypeIsArcManaged(mapKeyTypeName, &mapKeyArcKind)) {
                retainArcValue(key);
            }
        }
        builder_.CreateStore(key, newKeyPtr);

        llvm::Value *valsPtrField3 = builder_.CreateStructGEP(mapHeaderTy_, objPtr, 3, "vals_field3");
        llvm::Value *curValsPtr = builder_.CreateLoad(ptrTy_, valsPtrField3, "cur_vals");
        llvm::Value *newValPtr = builder_.CreateGEP(mapValTy, curValsPtr, {curLen}, "new_val_ptr");
        if (mapValIsArc)
            retainArcValue(rhsVal);
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
    if (isStringValue(objPtr))
        codegenError("str does not support index assignment; strings are immutable");
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

    // Snapshot list_elem_type_name before propagateTypeMeta: the
    // getOrCreateMeta inside it may rehash value_metadata_ and invalidate a
    // raw pointer from getMeta. Extracted here (before the compound/plain
    // branch) so both paths can pass it to emitArcReleaseLoadedElement for
    // the specialized str-aware destructor.
    std::string elemTypeName;
    if (auto *containerMeta = getMeta(objPtr))
        elemTypeName = containerMeta->list_elem_type_name;

    llvm::Value *finalVal = nullptr;
    llvm::Value *compoundOldVal = nullptr;  // captured for the ARC release path
    if (s.compound_op) {
        compoundOldVal = builder_.CreateLoad(elemTy, elemPtr, "list_elem_cur");
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
        else if (isAnyType(elemTy))
            finalVal = wrapInAny(finalVal);
        else if (isAnyType(finalVal->getType()) && canAnyHoldType(elemTy))
            finalVal = unwrapFromAny(finalVal, elemTy);
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
            emitArcReleaseLoadedElement(compoundOldVal, elemArcKind, elemTypeName, "list_elem_compound");
        } else {
            retainArcValue(finalVal);
            llvm::Value *oldVal = builder_.CreateLoad(ptrTy_, elemPtr, "list_elem_arc_old");
            emitArcReleaseLoadedElement(oldVal, elemArcKind, elemTypeName, "list_elem");
        }
    }

    // Tuple-element slot overwrite (#1670): mirror #855's retain-before-
    // release-old protocol for tuple components. `elementTypeIsArcManaged`
    // returns false for tuples (inline struct, not pointer), so the
    // `elemIsArc` block above never fires here. Compound-op is N/A for
    // tuples — no arithmetic operator is defined on tuple types, so the
    // parser/typechecker rejects compound `IndexAssignStmt` upstream.
    //
    // Resolve `elemTypeName` via `resolveTypeAlias` first so that user-
    // defined aliases (`type Pair = (int, str)` → `Pair`) canonicalize to
    // the literal tuple shape `"(int, str)"` before the shape gate. Without
    // this, alias-typed `List<Pair>` would skip the gate entirely and leak
    // tuple components on overwrite. Empty resolution (literal-built
    // `List<(K, V)>` whose `list_elem_type_name` is empty) preserves
    // pre-fix behaviour, matching the #1667 caveat on the destructor side.
    const std::string resolvedElemTypeName = resolveTypeAlias(elemTypeName);
    const bool elemIsTuple = resolvedElemTypeName.size() >= 2 &&
                              resolvedElemTypeName.front() == '(' &&
                              resolvedElemTypeName.back() == ')';
    if (elemIsTuple) {
        if (auto *tupleTy = llvm::dyn_cast<llvm::StructType>(elemTy)) {
            std::vector<std::string> components =
                splitTupleSig(resolvedElemTypeName);
            const unsigned n = static_cast<unsigned>(
                std::min<size_t>(components.size(),
                                  tupleTy->getNumElements()));
            // Retain new components from finalVal first (self-assignment
            // safety: `xs[i] = xs[i]`). `emitTupleComponentRetainTraced`
            // walks the InsertValue chain on `finalVal` recursively so that
            // nested tuple shapes like `xs[i] = (0, ([1], 2))` correctly
            // detect ownership at each leaf (the fresh `[1]` allocation is
            // in `arc_owned_values_` and must NOT be retained — the
            // `+1` from its construction transfers to the slot).
            for (unsigned i = 0; i < n; ++i) {
                llvm::Value *fieldVal = builder_.CreateExtractValue(
                    finalVal, {i},
                    "tup_new_f" + std::to_string(i));
                emitTupleComponentRetainTraced(fieldVal, finalVal, i,
                                                components[i]);
            }
            // Release old components from the existing slot.
            emitTupleElemReleaseSlot(elemPtr, "list_tup_old",
                                      resolvedElemTypeName, tupleTy);
        }
    }

    builder_.CreateStore(finalVal, elemPtr);
}

namespace {
DirectiveSignature fromDirectiveDef(const DirectiveDefStmt &s) {
    DirectiveSignature sig;
    sig.name = s.name;
    uint8_t mask = 0;
    for (const auto &t : s.targets)
        mask |= directiveTargetMask(t);
    sig.allowed_targets = mask;

    for (const auto &p : s.params) {
        sig.positional_param_names.push_back(p.name);
        if (p.default_value)
            sig.defaulted_params.push_back(p.name);
    }
    sig.min_positional = 0;
    sig.max_positional = static_cast<int>(sig.positional_param_names.size());
    sig.custom_validator = nullptr;
    return sig;
}
}  // namespace

void CodeGen::emitStmt(DirectiveDefStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;

    const auto &builtin = builtinDirectiveRegistry();
    if (builtin.find(s.name) != builtin.end())
        codegenError("@directive '" + s.name + "' collides with a built-in directive");

    if (user_directive_registry_.count(s.name))
        codegenError("@directive '" + s.name + "' is already defined");

    user_directive_registry_.emplace(s.name, fromDirectiveDef(s));
}

} // namespace ry
