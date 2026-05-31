#include "ry/codegen.hpp"


namespace ry {

llvm::Value *CodeGen::valueToString(llvm::Value *val, bool inCollection) {
    llvm::Type *ty = val->getType();

    if (ty == anyTy_)
        return emitAnyToString(val, inCollection);

    // Type value (from typeOf): extract and return the name ptr directly
    if (ty == typeTy_) {
        return builder_.CreateExtractValue(val, 1, "type_name");
    }

    // Enum value → variant name string
    {
        auto *evMeta = getMeta(val);
        if (evMeta && !evMeta->enum_value_type.empty()) {
            auto *einfoPtr = findEnumType(evMeta->enum_value_type);
            if (!einfoPtr)
                codegenError("valueToString: unknown enum type: " + evMeta->enum_value_type);
            auto &einfo = *einfoPtr;
            if (!einfo.isADT) {
                if (einfo.hasExplicitValues) {
                    llvm::BasicBlock *mergeBB = createBB("vts.enum.merge");
                    llvm::BasicBlock *defaultBB = createBB("vts.enum.default");
                    auto *sw = builder_.CreateSwitch(val, defaultBB, static_cast<unsigned>(einfo.variantCount));
                    builder_.SetInsertPoint(mergeBB);
                    auto *namePhi = builder_.CreatePHI(ptrTy_, static_cast<unsigned>(einfo.variantCount + 1), "vts.enum.name");
                    for (size_t i = 0; i < einfo.variantOrder.size(); ++i) {
                        const auto &vname = einfo.variantOrder[i];
                        int64_t vval = einfo.variants.at(vname);
                        llvm::BasicBlock *caseBB = createBBInFn(("vts.enum." + vname).c_str(), fn_);
                        sw->addCase(llvm::cast<llvm::ConstantInt>(llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(vval))), caseBB);
                        builder_.SetInsertPoint(caseBB);
                        llvm::Value *namePtr = builder_.CreateGEP(
                            llvm::ArrayType::get(ptrTy_, einfo.variantCount),
                            einfo.nameArray,
                            {llvm::ConstantInt::get(i64Ty_, 0), llvm::ConstantInt::get(i64Ty_, i)},
                            "enum_name_ptr");
                        llvm::Value *nameStr = builder_.CreateLoad(ptrTy_, namePtr, "enum_name");
                        namePhi->addIncoming(nameStr, caseBB);
                        emitBranchUncond(mergeBB);
                    }
                    builder_.SetInsertPoint(defaultBB);
                    llvm::Value *unknownStr = cachedGlobalString("?", ".enum_unknown");
                    namePhi->addIncoming(unknownStr, defaultBB);
                    emitBranchUncond(mergeBB);
                    builder_.SetInsertPoint(mergeBB);
                    return namePhi;
                }
                llvm::Value *namePtr = builder_.CreateGEP(
                    llvm::ArrayType::get(ptrTy_, einfo.variantCount),
                    einfo.nameArray,
                    {llvm::ConstantInt::get(i64Ty_, 0), val},
                    "enum_name_ptr");
                return builder_.CreateLoad(ptrTy_, namePtr, "enum_name");
            }

            // ADT enum: delegate to a per-type helper function that is
            // emitted lazily on first reference and cached thereafter.
            // This indirection breaks codegen-time recursion for self-
            // referential ADTs like `enum Tree: Node(int, List<Tree>)`:
            // inside the helper body, nested references to the same ADT
            // become an ordinary IR `call` instead of re-inlining the
            // switch body, so codegen terminates.  See
            // `getOrCreateADTToStringFn` below (#1238).
            llvm::Function *adtFn = getOrCreateADTToStringFn(evMeta->enum_value_type);
            return builder_.CreateCall(adtFn, {val}, "vts.adt.call");
        }
    }

    if (ty == errorTy_) {
        auto spf = getSprintPrintf();
        builder_.CreateCall(getRuntimeFn("__ry_sprint_begin",
            llvm::Type::getVoidTy(*ctx_), {}));
        llvm::Value *msg = builder_.CreateExtractValue(val, 0, "vts_err_msg");
        llvm::Value *code = builder_.CreateExtractValue(val, 1, "vts_err_code");
        llvm::Constant *fmt = cachedGlobalString("Error: %s (code: %ld)", ".vts_err_fmt");
        builder_.CreateCall(spf, {fmt, msg, code});
        return emitSprintEnd("vts_err_str");
    }

    if (auto *structTy = llvm::dyn_cast<llvm::StructType>(ty)) {
        for (auto &[uname, uinfo] : union_type_info_) {
            if (uinfo.llvmType != structTy) continue;

            llvm::Value *tag = builder_.CreateExtractValue(val, 0, "vts.union.tag");
            llvm::Value *dataBytes = builder_.CreateExtractValue(val, 1, "vts.union.data");
            auto *dataTy = uinfo.llvmType->getElementType(1);
            llvm::AllocaInst *dataTmp = builder_.CreateAlloca(dataTy, nullptr, "vts.union.data.tmp");
            dataTmp->setAlignment(mod_->getDataLayout().getABITypeAlign(uinfo.llvmType));
            builder_.CreateStore(dataBytes, dataTmp);

            llvm::BasicBlock *mergeBB = createBB("vts.union.merge");
            llvm::BasicBlock *defaultBB = createBB("vts.union.default");
            llvm::SwitchInst *sw = builder_.CreateSwitch(tag, defaultBB, static_cast<unsigned>(uinfo.componentTypes.size()));

            builder_.SetInsertPoint(defaultBB);
            llvm::Constant *unknownStr = cachedGlobalString("?", ".vts_union_unknown");
            emitBranchUncond(mergeBB);

            builder_.SetInsertPoint(mergeBB);
            auto *phi = builder_.CreatePHI(ptrTy_, static_cast<unsigned>(uinfo.componentTypes.size() + 1), "vts.union.str");
            phi->addIncoming(unknownStr, defaultBB);

            for (size_t i = 0; i < uinfo.componentTypes.size(); ++i) {
                llvm::BasicBlock *caseBB =
                    createBB(("vts.union.case" + std::to_string(i)).c_str());
                sw->addCase(llvm::ConstantInt::get(
                    llvm::cast<llvm::IntegerType>(i64Ty_), i), caseBB);
                builder_.SetInsertPoint(caseBB);

                const auto &compName = uinfo.componentNames[i];

                // Closure/function variants: return "<closure>" directly.
                // Union component names are normalized type strings like "fn(int) -> int",
                // so use ry::util::isFunctionTypeName() rather than comparing against the literal "closure".
                if (uinfo.componentTypes[i]->isPointerTy() && ry::util::isFunctionTypeName(compName)) {
                    phi->addIncoming(cachedGlobalString("<closure>", ".vts_closure"),
                                     builder_.GetInsertBlock());
                    emitBranchUncond(mergeBB);
                    continue;
                }

                // Reject non-stringifiable pointer-backed variants (str/collection/function are OK).
                // This rejection branch is kept so that future additions of ARC-managed
                // ptr-backed types without a formatter handler fail fast at compile time
                // rather than producing garbage output.
                if (uinfo.componentTypes[i]->isPointerTy() &&
                    compName != "str" &&
                    !ry::util::isListTypeName(compName) &&
                    !ry::util::isMapTypeName(compName) &&
                    !ry::util::isSetTypeName(compName)) {
                    codegenError("cannot convert " + compName +
                                 " variant of union to string");
                }

                llvm::Value *innerVal = builder_.CreateLoad(
                    uinfo.componentTypes[i], dataTmp, "vts.union.inner");

                // Propagate low-level type metadata for correct signedness formatting
                if (ry::util::isLowLevelTypeName(compName))
                    getOrCreateMeta(innerVal).low_level_type_name = compName;

                // Propagate collection type metadata so List/Map/Set variants format
                // their element types correctly (matches List/Map/Set branches below).
                if (ry::util::isListTypeName(compName) || ry::util::isMapTypeName(compName) || ry::util::isSetTypeName(compName))
                    propagateTypeMeta(compName, innerVal);

                llvm::Value *innerStr = valueToString(innerVal, inCollection);

                phi->addIncoming(innerStr, builder_.GetInsertBlock());
                emitBranchUncond(mergeBB);
            }

            builder_.SetInsertPoint(mergeBB);
            return phi;
        }

        if (isOptionType(ty)) {
            auto spf = getSprintPrintf();
            emitSprintBegin();

            llvm::Value *hasValue = builder_.CreateExtractValue(val, 0, "vts.opt.has");
            llvm::BasicBlock *someBB = createBB("vts.opt.some");
            llvm::BasicBlock *noneBB = createBB("vts.opt.none");
            llvm::BasicBlock *endBB  = createBB("vts.opt.end");

            emitBranchCond(hasValue, someBB, noneBB);

            builder_.SetInsertPoint(noneBB);
            builder_.CreateCall(spf, {cachedGlobalString("None", ".vts_none")});
            emitBranchUncond(endBB);

            builder_.SetInsertPoint(someBB);
            llvm::Value *innerVal = builder_.CreateExtractValue(val, 1, "vts.opt.val");
            propagateMeta(val, innerVal);
            builder_.CreateCall(spf, {cachedGlobalString("Some(", ".vts_some_pre")});
            llvm::Value *innerStr = valueToString(innerVal, inCollection);
            builder_.CreateCall(spf, {cachedGlobalString("%s", ".vts_opt_s"), innerStr});
            builder_.CreateCall(spf, {cachedGlobalString(")", ".vts_some_post")});
            emitBranchUncond(endBB);

            builder_.SetInsertPoint(endBB);
            return emitSprintEnd("vts.opt.str");
        }

        if (isResultType(ty)) {
            auto spf = getSprintPrintf();
            emitSprintBegin();

            llvm::Value *isOk = builder_.CreateExtractValue(val, 0, "vts.res.is_ok");
            llvm::BasicBlock *okBB  = createBB("vts.res.ok");
            llvm::BasicBlock *errBB = createBB("vts.res.err");
            llvm::BasicBlock *endBB = createBB("vts.res.end");

            emitBranchCond(isOk, okBB, errBB);

            builder_.SetInsertPoint(okBB);
            llvm::Value *okVal = builder_.CreateExtractValue(val, 1, "vts.res.ok_val");
            propagateMeta(val, okVal);
            builder_.CreateCall(spf, {cachedGlobalString("Ok(", ".vts_ok_pre")});
            llvm::Value *okStr = valueToString(okVal, inCollection);
            builder_.CreateCall(spf, {cachedGlobalString("%s", ".vts_res_s"), okStr});
            builder_.CreateCall(spf, {cachedGlobalString(")", ".vts_ok_post")});
            emitBranchUncond(endBB);

            builder_.SetInsertPoint(errBB);
            llvm::Value *errVal = builder_.CreateExtractValue(val, 2, "vts.res.err_val");
            propagateMeta(val, errVal);
            builder_.CreateCall(spf, {cachedGlobalString("Err(", ".vts_err_pre")});
            llvm::Value *errStr = valueToString(errVal, inCollection);
            builder_.CreateCall(spf, {cachedGlobalString("%s", ".vts_res_e"), errStr});
            builder_.CreateCall(spf, {cachedGlobalString(")", ".vts_err_post")});
            emitBranchUncond(endBB);

            builder_.SetInsertPoint(endBB);
            return emitSprintEnd("vts.res.str");
        }

        std::string name = structTy->getName().str();
        if (record_types_.count(name))
            return recordToString(val);
        if (isTupleStructType(structTy))
            return tupleToString(val, structTy);
        codegenError("cannot convert this record type to string: " + name);
    }

    // Fixed-length array: sprint buffer + IR loop
    if (auto *ai = llvm::dyn_cast<llvm::AllocaInst>(val)) {
        if (auto *arrTy = llvm::dyn_cast<llvm::ArrayType>(ai->getAllocatedType())) {
            llvm::Type *elemTy = arrTy->getElementType();
            uint64_t arrSize = arrTy->getNumElements();
            auto spf = getSprintPrintf();

            emitSprintBegin();
            builder_.CreateCall(spf, {cachedGlobalString("[", ".vts_arr_lb")});

            llvm::BasicBlock *condBB = createBB("vts_arr.cond");
            llvm::BasicBlock *bodyBB = createBB("vts_arr.body");
            llvm::BasicBlock *endBB  = createBB("vts_arr.end");

            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "vts_arr_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            emitBranchUncond(condBB);

            builder_.SetInsertPoint(condBB);
            llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "i");
            builder_.CreateCondBr(
                builder_.CreateICmpSLT(iVal, llvm::ConstantInt::get(i64Ty_, arrSize)),
                bodyBB, endBB);

            builder_.SetInsertPoint(bodyBB);
            llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "i_cur");

            // Comma separator if not first element
            llvm::BasicBlock *commaBB = createBB("vts_arr.comma");
            llvm::BasicBlock *elemBB  = createBB("vts_arr.elem");
            builder_.CreateCondBr(
                builder_.CreateICmpSGT(iCur, llvm::ConstantInt::get(i64Ty_, 0)),
                commaBB, elemBB);

            builder_.SetInsertPoint(commaBB);
            builder_.CreateCall(spf, {cachedGlobalString(", ", ".vts_arr_comma")});
            emitBranchUncond(elemBB);

            builder_.SetInsertPoint(elemBB);
            llvm::Value *elemPtr = builder_.CreateGEP(
                arrTy, ai,
                {llvm::ConstantInt::get(i64Ty_, 0), iCur},
                "vts_arr_ptr");
            llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "vts_arr_elem");
            llvm::Value *elemStr = valueToString(elem, true);
            builder_.CreateCall(spf, {cachedGlobalString("%s", ".vts_arr_s"), elemStr});

            builder_.CreateStore(
                builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1)),
                iVar);
            emitBranchUncond(condBB);

            builder_.SetInsertPoint(endBB);
            builder_.CreateCall(spf, {cachedGlobalString("]", ".vts_arr_rb")});
            return emitSprintEnd("vts_arr_str");
        }
    }

    if (ty->isPointerTy()) {
        // Propagate element-type metadata onto a freshly-loaded collection
        // element so valueToString(elem) can detect nested collections / closures
        // instead of falling through to the raw string-pointer path.  Snapshots
        // are taken before any getOrCreateMeta() call because an insert into
        // value_metadata_ may rehash and invalidate pointers from getMeta().
        auto propagateElemMeta = [this](
                llvm::Value *outer, llvm::Value *elem,
                std::string ValueMetadata::*typeNameField,
                std::optional<FnTypeInfo> ValueMetadata::*fnInfoField) {
            std::string typeName;
            std::optional<FnTypeInfo> fnInfo;
            if (auto *outerMeta = getMeta(outer)) {
                typeName = outerMeta->*typeNameField;
                fnInfo   = outerMeta->*fnInfoField;
            }
            if (!typeName.empty())
                propagateTypeMeta(typeName, elem);
            if (fnInfo)
                getOrCreateMeta(elem).fn_type_info = *fnInfo;
        };

        // Set: sprint buffer + IR loop → {elem, elem, ...}
        if (llvm::Type *setElemTy = getSetElementType(val)) {
            auto spf = getSprintPrintf();
            emitSprintBegin();

            auto sf = loadSetHeader(val, "vts_set");

            builder_.CreateCall(spf, {cachedGlobalString("{", ".vts_set_lb")});

            llvm::BasicBlock *condBB = createBB("vts_set.cond");
            llvm::BasicBlock *bodyBB = createBB("vts_set.body");
            llvm::BasicBlock *endBB  = createBB("vts_set.end");

            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "vts_set_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            emitBranchUncond(condBB);

            builder_.SetInsertPoint(condBB);
            llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "i");
            emitBranchCond(builder_.CreateICmpSLT(iVal, sf.len), bodyBB, endBB);

            builder_.SetInsertPoint(bodyBB);
            llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "i_cur");
            llvm::BasicBlock *commaBB = createBB("vts_set.comma");
            llvm::BasicBlock *elemBB  = createBB("vts_set.elem");
            builder_.CreateCondBr(
                builder_.CreateICmpSGT(iCur, llvm::ConstantInt::get(i64Ty_, 0)),
                commaBB, elemBB);

            builder_.SetInsertPoint(commaBB);
            builder_.CreateCall(spf, {cachedGlobalString(", ", ".vts_set_comma")});
            emitBranchUncond(elemBB);

            builder_.SetInsertPoint(elemBB);
            llvm::Value *elemPtr = builder_.CreateGEP(setElemTy, sf.elems, {iCur}, "vts_set_elem_ptr");
            llvm::Value *elem = builder_.CreateLoad(setElemTy, elemPtr, "vts_set_elem");
            propagateElemMeta(val, elem,
                              &ValueMetadata::set_elem_type_name,
                              &ValueMetadata::set_elem_fn_type_info);
            llvm::Value *elemStr = valueToString(elem, true);
            builder_.CreateCall(spf, {cachedGlobalString("%s", ".vts_set_s"), elemStr});

            builder_.CreateStore(
                builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
            emitBranchUncond(condBB);

            builder_.SetInsertPoint(endBB);
            builder_.CreateCall(spf, {cachedGlobalString("}", ".vts_set_rb")});
            return emitSprintEnd("vts_set_str");
        }

        // Map: sprint buffer + IR loop → {key: val, key: val, ...}
        llvm::Type *mapKeyTy = getMapKeyType(val);
        llvm::Type *mapValTy = getMapValueType(val);
        if (mapKeyTy && mapValTy) {
            auto spf = getSprintPrintf();
            emitSprintBegin();

            auto mf = loadMapHeader(val, "vts_map");

            builder_.CreateCall(spf, {cachedGlobalString("{", ".vts_map_lb")});

            llvm::BasicBlock *condBB = createBB("vts_map.cond");
            llvm::BasicBlock *bodyBB = createBB("vts_map.body");
            llvm::BasicBlock *endBB  = createBB("vts_map.end");

            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "vts_map_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            emitBranchUncond(condBB);

            builder_.SetInsertPoint(condBB);
            llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "i");
            emitBranchCond(builder_.CreateICmpSLT(iVal, mf.len), bodyBB, endBB);

            builder_.SetInsertPoint(bodyBB);
            llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "i_cur");
            llvm::BasicBlock *commaBB = createBB("vts_map.comma");
            llvm::BasicBlock *kvBB    = createBB("vts_map.kv");
            builder_.CreateCondBr(
                builder_.CreateICmpSGT(iCur, llvm::ConstantInt::get(i64Ty_, 0)),
                commaBB, kvBB);

            builder_.SetInsertPoint(commaBB);
            builder_.CreateCall(spf, {cachedGlobalString(", ", ".vts_map_comma")});
            emitBranchUncond(kvBB);

            builder_.SetInsertPoint(kvBB);
            llvm::Value *keyPtr = builder_.CreateGEP(mapKeyTy, mf.keys, {iCur}, "vts_map_key_ptr");
            llvm::Value *keyVal = builder_.CreateLoad(mapKeyTy, keyPtr, "vts_map_key");
            llvm::Value *keyStr = valueToString(keyVal, true);
            builder_.CreateCall(spf, {cachedGlobalString("%s: ", ".vts_map_kv_fmt"), keyStr});

            llvm::Value *valPtr = builder_.CreateGEP(mapValTy, mf.vals, {iCur}, "vts_map_val_ptr");
            llvm::Value *valVal = builder_.CreateLoad(mapValTy, valPtr, "vts_map_val");
            propagateElemMeta(val, valVal,
                              &ValueMetadata::map_value_type_name,
                              &ValueMetadata::map_value_fn_type_info);
            llvm::Value *valStr = valueToString(valVal, true);
            builder_.CreateCall(spf, {cachedGlobalString("%s", ".vts_map_v_s"), valStr});

            builder_.CreateStore(
                builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
            emitBranchUncond(condBB);

            builder_.SetInsertPoint(endBB);
            builder_.CreateCall(spf, {cachedGlobalString("}", ".vts_map_rb")});
            return emitSprintEnd("vts_map_str");
        }

        // List: sprint buffer + IR loop → [elem, elem, ...]
        if (llvm::Type *listElemTy = getListElementType(val)) {
            auto spf = getSprintPrintf();
            emitSprintBegin();

            auto lf = loadListHeader(val, "vts_list");

            builder_.CreateCall(spf, {cachedGlobalString("[", ".vts_list_lb")});

            llvm::BasicBlock *condBB = createBB("vts_list.cond");
            llvm::BasicBlock *bodyBB = createBB("vts_list.body");
            llvm::BasicBlock *endBB  = createBB("vts_list.end");

            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "vts_list_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            emitBranchUncond(condBB);

            builder_.SetInsertPoint(condBB);
            llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "i");
            emitBranchCond(builder_.CreateICmpSLT(iVal, lf.len), bodyBB, endBB);

            builder_.SetInsertPoint(bodyBB);
            llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "i_cur");
            llvm::BasicBlock *commaBB = createBB("vts_list.comma");
            llvm::BasicBlock *elemBB  = createBB("vts_list.elem");
            builder_.CreateCondBr(
                builder_.CreateICmpSGT(iCur, llvm::ConstantInt::get(i64Ty_, 0)),
                commaBB, elemBB);

            builder_.SetInsertPoint(commaBB);
            builder_.CreateCall(spf, {cachedGlobalString(", ", ".vts_list_comma")});
            emitBranchUncond(elemBB);

            builder_.SetInsertPoint(elemBB);
            llvm::Value *elemPtr = builder_.CreateGEP(listElemTy, lf.data, {iCur}, "vts_list_elem_ptr");
            llvm::Value *elem = builder_.CreateLoad(listElemTy, elemPtr, "vts_list_elem");
            propagateElemMeta(val, elem,
                              &ValueMetadata::list_elem_type_name,
                              &ValueMetadata::list_elem_fn_type_info);
            llvm::Value *elemStr = valueToString(elem, true);
            builder_.CreateCall(spf, {cachedGlobalString("%s", ".vts_list_s"), elemStr});

            builder_.CreateStore(
                builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
            emitBranchUncond(condBB);

            builder_.SetInsertPoint(endBB);
            builder_.CreateCall(spf, {cachedGlobalString("]", ".vts_list_rb")});
            return emitSprintEnd("vts_list_str");
        }

        if (auto *fnMeta = getMeta(val); fnMeta && fnMeta->fn_type_info)
            return cachedGlobalString("<closure>", ".vts_closure");
        if (inCollection) {
            llvm::FunctionCallee escapeFn =
                getRuntimeFn("__ry_print_str_quote_escape", ptrTy_, {ptrTy_});
            return builder_.CreateCall(escapeFn, {val}, "vts_str_escaped");
        }
        return val; // string pointer
    }
    auto snprintfFn = getStdlibSnprintf();
    // All numeric-to-string conversions produce StringHeader-managed strings so
    // that emitStringByteLen() works correctly when the result is used in str
    // operations (e.g. "prefix" + 42).  We allocate via __ry_string_make_uninit
    // (which sets byte_len = capacity), then overwrite byte_len with the actual
    // character count from snprintf's return value.
    auto makeUninitFn = getRuntimeFn("__ry_string_make_uninit", ptrTy_, {i64Ty_});
    // snprintfStr: emit buf = makeUninit(32); nw = snprintf(buf, 32, fmt, ...);
    //              byte_len(buf) = nw;  return buf.
    auto snprintfStr = [&](llvm::Constant *fmt,
                           llvm::ArrayRef<llvm::Value *> args) -> llvm::Value * {
        llvm::Value *buf = builder_.CreateCall(
            makeUninitFn, {llvm::ConstantInt::get(i64Ty_, 32)}, "vts_buf");
        llvm::SmallVector<llvm::Value *, 8> snArgs = {
            buf, llvm::ConstantInt::get(i64Ty_, 32), fmt};
        snArgs.append(args.begin(), args.end());
        llvm::Value *nw = builder_.CreateCall(snprintfFn, snArgs, "vts_nw");
        llvm::Value *actualLen = builder_.CreateSExt(nw, i64Ty_, "vts_len");
        auto *bytelenPtr = builder_.CreateGEP(
            i8Ty_, buf,
            llvm::ConstantInt::get(
                i64Ty_,
                static_cast<uint64_t>(-static_cast<int64_t>(STRING_BYTELEN_OFFSET))),
            "vts_bl_ptr");
        builder_.CreateStore(actualLen, bytelenPtr);
        arc_str_owned_values_.insert(buf);
        return buf;
    };

    if (ty == i1Ty_) {
        llvm::Constant *trueStr = cachedGlobalString("true", ".vts_true");
        llvm::Constant *falseStr = cachedGlobalString("false", ".vts_false");
        return builder_.CreateSelect(val, trueStr, falseStr, "vts_bool");
    }
    if (ty->isDoubleTy()) {
        // Delegate to runtime helper so Python-style ".0" suffix is added
        // for whole-number floats (see __ry_any_fmt_float in runtime_any.cpp, #808).
        llvm::FunctionCallee fmtFn = getRuntimeFn("__ry_any_fmt_float", ptrTy_, {f64Ty_});
        return builder_.CreateCall(fmtFn, {val}, "vts_f64_str");
    }
    // Check low-level type metadata for ambiguous LLVM types
    std::string llName = getLowLevelTypeName(val);

    if (ty == i8Ty_) {
        if (llName == "i8") {
            llvm::Constant *fmt = cachedGlobalString("%d", ".vts_i8_fmt");
            llvm::Value *ext = builder_.CreateSExt(val, i32Ty_, "i8_ext");
            return snprintfStr(fmt, {ext});
        }
        // u8
        llvm::Constant *fmt = cachedGlobalString("%u", ".vts_u8_fmt");
        llvm::Value *ext = builder_.CreateZExt(val, i32Ty_, "u8_ext");
        return snprintfStr(fmt, {ext});
    }
    if (ty == i16Ty_) {
        if (llName == "u16") {
            llvm::Constant *fmt = cachedGlobalString("%u", ".vts_u16_fmt");
            llvm::Value *ext = builder_.CreateZExt(val, i32Ty_, "u16_ext");
            return snprintfStr(fmt, {ext});
        }
        llvm::Constant *fmt = cachedGlobalString("%d", ".vts_i16_fmt");
        llvm::Value *ext = builder_.CreateSExt(val, i32Ty_, "i16_ext");
        return snprintfStr(fmt, {ext});
    }
    if (ty == i32Ty_) {
        if (llName == "u32") {
            llvm::Constant *fmt = cachedGlobalString("%u", ".vts_u32_fmt");
            return snprintfStr(fmt, {val});
        }
        llvm::Constant *fmt = cachedGlobalString("%d", ".vts_i32_fmt");
        return snprintfStr(fmt, {val});
    }
    if (ty == f32Ty_) {
        // Use f32-specific formatter for shortest f32 round-trip (#1031).
        // Calling the f64 path after FPExt would change the shortest
        // representation (e.g. "3.14f32" → "3.140000104904175").
        llvm::FunctionCallee fmtFn = getRuntimeFn("__ry_any_fmt_f32", ptrTy_, {f32Ty_});
        return builder_.CreateCall(fmtFn, {val}, "vts_f32_str");
    }
    // default: int (i64) or i64/u64
    if (llName == "u64") {
        llvm::Constant *fmt = cachedGlobalString("%lu", ".vts_u64_fmt");
        return snprintfStr(fmt, {val});
    }
    llvm::Constant *fmt = cachedGlobalString("%ld", ".vts_int_fmt");
    return snprintfStr(fmt, {val});
}

llvm::Value *CodeGen::recordToString(llvm::Value *val) {
    auto *structTy = llvm::cast<llvm::StructType>(val->getType());
    std::string typeName = structTy->getName().str();
    auto it = record_types_.find(typeName);
    if (it == record_types_.end())
        codegenError("recordToString: unknown record type: " + typeName);

    const auto &info = it->second;

    // Check for user-defined toStr overload
    auto *fit = findFunction("toStr");
    if (fit) {
        for (auto &entry : *fit) {
            if (entry.paramTypes.size() == 1 && entry.paramTypes[0] == structTy) {
                return builder_.CreateCall(entry.func, {val}, "user_to_str");
            }
        }
    }

    // Auto-generate: "TypeName(field1: val1, field2: val2)"
    auto strlenFn = getStdlibStrlen();

    using SP = std::pair<llvm::Value*, llvm::Value*>;
    std::vector<SP> parts;

    auto addLiteral = [&](const std::string &s, const char *label) {
        parts.push_back({cachedGlobalString(s, label),
                         llvm::ConstantInt::get(i64Ty_, s.size())});
    };

    addLiteral(typeName + "(", ".sts_prefix");

    for (unsigned i = 0; i < info.fields.size(); ++i) {
        if (i > 0)
            addLiteral(", ", ".sts_sep");
        addLiteral(info.fields[i].name + ": ", ".sts_fname");
        llvm::Value *field = builder_.CreateExtractValue(val, i, info.fields[i].name);
        llvm::Value *fieldStr = valueToString(field, true);
        parts.push_back({fieldStr, builder_.CreateCall(strlenFn, {fieldStr}, "sts_len")});
    }

    addLiteral(")", ".sts_suffix");

    return concatStringParts(parts, "sts");
}

llvm::Value *CodeGen::tupleToString(llvm::Value *val, llvm::StructType *st) {
    auto strlenFn = getStdlibStrlen();

    using SP = std::pair<llvm::Value*, llvm::Value*>;
    std::vector<SP> parts;

    auto addLiteral = [&](const std::string &s, const char *label) {
        parts.push_back({cachedGlobalString(s, label),
                         llvm::ConstantInt::get(i64Ty_, s.size())});
    };

    unsigned n = st->getNumElements();
    addLiteral("(", ".tts_prefix");
    for (unsigned i = 0; i < n; ++i) {
        if (i > 0)
            addLiteral(", ", ".tts_sep");
        llvm::Value *elem = builder_.CreateExtractValue(val, i, "tts_elem");
        llvm::Value *elemStr = valueToString(elem, true);
        parts.push_back({elemStr, builder_.CreateCall(strlenFn, {elemStr}, "tts_len")});
    }
    if (n == 1)
        addLiteral(",", ".tts_trail");
    addLiteral(")", ".tts_suffix");

    return concatStringParts(parts, "tts");
}

void CodeGen::emitSprintBegin() {
    builder_.CreateCall(getRuntimeFn("__ry_sprint_begin",
        llvm::Type::getVoidTy(*ctx_), {}));
}

llvm::Value *CodeGen::emitSprintEnd(const llvm::Twine &name) {
    return builder_.CreateCall(
        getRuntimeFn("__ry_sprint_end", ptrTy_, {}), {}, name);
}

llvm::Value *CodeGen::concatStringParts(
    const std::vector<std::pair<llvm::Value*, llvm::Value*>> &parts,
    const std::string &prefix) {
    auto memcpyFn = getStdlibMemcpy();
    auto makeUninitFn = getRuntimeFn("__ry_string_make_uninit", ptrTy_, {i64Ty_});

    llvm::Value *totalLen = llvm::ConstantInt::get(i64Ty_, 0);
    for (auto &p : parts)
        totalLen = builder_.CreateAdd(totalLen, p.second, prefix + "_total");

    // __ry_string_make_uninit allocates STRING_HEADER_SIZE + totalLen + 1 bytes,
    // sets byte_len = totalLen and writes '\0' at buf[totalLen].  No explicit NUL
    // write or +1 to bufSize needed.
    llvm::Value *buf = builder_.CreateCall(makeUninitFn, {totalLen}, prefix + "_buf");
    llvm::Value *off = llvm::ConstantInt::get(i64Ty_, 0);
    for (auto &p : parts) {
        llvm::Value *dst = builder_.CreateGEP(
            builder_.getInt8Ty(), buf, {off}, prefix + "_dst");
        builder_.CreateCall(memcpyFn, {dst, p.first, p.second});
        off = builder_.CreateAdd(off, p.second, prefix + "_off");
    }
    // NUL at buf[totalLen] already written by __ry_string_make_uninit
    arc_str_owned_values_.insert(buf);
    return buf;
}

// Lazily emit (and cache) a per-ADT-type formatter.  The helper has signature
// `char *__ry_adt_to_str_<EnumName>(<ADTStructType>)` and contains the same
// switch/field-formatting body that used to live inline in valueToString().
// The key trick: the function is stored in `adt_to_string_fns_` *before* the
// body is emitted, so when the body's own recursive `valueToString` call
// revisits the same ADT (via e.g. `List<Tree>` inside `Tree::Node`), the cache
// hit yields an ordinary IR `call` to this function and codegen terminates.
//
// Without this indirection, codegen would walk the ADT switch body for Tree,
// inline a List formatter, inline the element's ADT body for Tree, etc., and
// blow the C++ stack (observed as SIGILL / exit 132) for #1238's `Tree::Node(1,
// [Tree::Leaf(2), Tree::Leaf(3)])` repro.
llvm::Function *CodeGen::getOrCreateADTToStringFn(const std::string &enumName) {
    auto cached = adt_to_string_fns_.find(enumName);
    if (cached != adt_to_string_fns_.end())
        return cached->second;

    auto *einfoPtr = findEnumType(enumName);
    if (!einfoPtr)
        return nullptr;
    auto &einfo = *einfoPtr;

    // enumName may contain '<', '>', ',' (e.g. "Option<int>") — not valid in LLVM identifiers.
    std::string sanitized;
    sanitized.reserve(enumName.size());
    for (char c : enumName) {
        if (std::isalnum(static_cast<unsigned char>(c)) || c == '_')
            sanitized.push_back(c);
        else
            sanitized.push_back('_');
    }

    llvm::FunctionType *fnTy = llvm::FunctionType::get(
        ptrTy_, {einfo.adtType}, false);
    llvm::Function *fn = llvm::Function::Create(
        fnTy, llvm::Function::InternalLinkage,
        "__ry_adt_to_str_" + sanitized, mod_.get());
    fn->addFnAttr(llvm::Attribute::NoUnwind);

    // Store BEFORE body emission to break codegen-time recursion.
    adt_to_string_fns_[enumName] = fn;

    // Save and switch code-generation context.
    llvm::Function *savedFn = fn_;
    llvm::BasicBlock *savedBB = builder_.GetInsertBlock();
    auto savedPt = builder_.GetInsertPoint();

    fn_ = fn;
    auto *entryBB = createBBInFn("entry", fn);
    builder_.SetInsertPoint(entryBB);

    llvm::Value *val = fn->getArg(0);
    val->setName("adt");

    auto spf = getSprintPrintf();
    emitSprintBegin();

    llvm::Value *tag = builder_.CreateExtractValue(val, 0, "vts.adt.tag");
    llvm::Value *namePtr = builder_.CreateGEP(
        llvm::ArrayType::get(ptrTy_, einfo.variantCount),
        einfo.nameArray,
        {llvm::ConstantInt::get(i64Ty_, 0), tag},
        "vts.adt.name_ptr");
    llvm::Value *nameStr = builder_.CreateLoad(ptrTy_, namePtr, "vts.adt.name");

    llvm::AllocaInst *adtAlloca = builder_.CreateAlloca(einfo.adtType, nullptr, "vts.adt.tmp");
    builder_.CreateStore(val, adtAlloca);
    llvm::Value *payloadPtr = builder_.CreateStructGEP(einfo.adtType, adtAlloca, 1, "vts.adt.payload");

    bool anyFields = false;
    for (auto &[vn, vf] : einfo.variantFields)
        if (!vf.fieldTypes.empty()) { anyFields = true; break; }

    if (anyFields) {
        llvm::BasicBlock *endBB = createBBInFn("vts.adt.end", fn);
        llvm::BasicBlock *defaultBB = createBBInFn("vts.adt.default", fn);
        auto *switchInst = builder_.CreateSwitch(tag, defaultBB, static_cast<unsigned>(einfo.variantCount));

        for (auto &[vname, vtag] : einfo.variants) {
            llvm::BasicBlock *caseBB = createBBInFn(("vts.adt." + vname).c_str(), fn);
            switchInst->addCase(
                llvm::cast<llvm::ConstantInt>(llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(vtag))), caseBB);
            builder_.SetInsertPoint(caseBB);

            auto fit = einfo.variantFields.find(vname);
            if (fit != einfo.variantFields.end() && !fit->second.fieldTypes.empty()) {
                llvm::Constant *openFmt = cachedGlobalString("%s(", ".vts_adt_open");
                builder_.CreateCall(spf, {openFmt, nameStr});

                const llvm::DataLayout &dl = mod_->getDataLayout();
                size_t offset = 0;
                for (size_t fi = 0; fi < fit->second.fieldTypes.size(); ++fi) {
                    llvm::Type *fieldTy = fit->second.fieldTypes[fi];
                    uint64_t align = dl.getABITypeAlign(fieldTy).value();
                    offset = (offset + align - 1) / align * align;
                    llvm::Value *fieldPtr = builder_.CreateGEP(
                        llvm::Type::getInt8Ty(*ctx_), payloadPtr,
                        {llvm::ConstantInt::get(i64Ty_, offset)},
                        "vts.adt.field." + std::to_string(fi));
                    llvm::Value *fieldVal = builder_.CreateLoad(fieldTy, fieldPtr, "vts.adt.fval");

                    // Propagate full type metadata (low-level signedness,
                    // List/Map/Set element types, enum spec, Option/Result
                    // inner types) so the recursive valueToString dispatch
                    // can format the field correctly.  CreateLoad yields a
                    // metadata-less SSA value; without propagation, ADT
                    // field loads print enums as raw tag ints and nested
                    // collections as garbage.
                    if (fi < fit->second.fieldTypeNames.size()) {
                        const auto &ftName = fit->second.fieldTypeNames[fi];
                        if (!ftName.empty())
                            propagateTypeMeta(ftName, fieldVal);
                    }

                    if (fi > 0) {
                        llvm::Constant *commaFmt = cachedGlobalString(", ", ".vts_adt_comma");
                        builder_.CreateCall(spf, {commaFmt});
                    }

                    llvm::Value *fieldStr = valueToString(fieldVal, true);
                    llvm::Constant *sfmt = cachedGlobalString("%s", ".vts_adt_s");
                    builder_.CreateCall(spf, {sfmt, fieldStr});

                    offset += dl.getTypeAllocSize(fieldTy);
                }

                llvm::Constant *closeFmt = cachedGlobalString(")", ".vts_adt_close");
                builder_.CreateCall(spf, {closeFmt});
            } else {
                llvm::Constant *fmt = cachedGlobalString("%s", ".vts_adt_nodata");
                builder_.CreateCall(spf, {fmt, nameStr});
            }
            emitBranchUncond(endBB);
        }

        builder_.SetInsertPoint(defaultBB);
        emitBranchUncond(endBB);
        builder_.SetInsertPoint(endBB);
    } else {
        llvm::Constant *fmt = cachedGlobalString("%s", ".vts_adt_simple");
        builder_.CreateCall(spf, {fmt, nameStr});
    }

    llvm::Value *result = emitSprintEnd("vts.adt.str");
    builder_.CreateRet(result);

    // Restore code-generation context.
    fn_ = savedFn;
    if (savedBB)
        builder_.SetInsertPoint(savedBB, savedPt);

    return fn;
}

} // namespace ry
