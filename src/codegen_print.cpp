#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"

// ===== emitPrint (variadic) =====

void CodeGen::emitPrint(const std::vector<ExprPtr> &args) {
    builder_.CreateCall(getRuntimeFn("__ry_print_begin",
        llvm::Type::getVoidTy(*ctx_), {}));

    auto printfFn = getBufferedPrintf();
    llvm::Constant *space = args.size() > 1
        ? cachedGlobalString(" ", ".fmt_space") : nullptr;

    for (size_t i = 0; i < args.size(); ++i) {
        if (i > 0)
            builder_.CreateCall(printfFn, {space});
        llvm::Value *val = emitExpr(*args[i]);
        emitPrintSingle(val, printfFn);
    }

    llvm::Constant *nl = cachedGlobalString("\n", ".fmt_nl");
    builder_.CreateCall(printfFn, {nl});

    builder_.CreateCall(getRuntimeFn("__ry_print_end",
        llvm::Type::getVoidTy(*ctx_), {}));
}

// ===== emitPrintSingle (one value, no trailing newline) =====

void CodeGen::emitPrintSingle(llvm::Value *val, llvm::FunctionCallee printfFn) {

    // Enum printing: check if value is tracked as an enum
    {
        auto evIt = enum_value_types_.find(val);
        if (evIt == enum_value_types_.end()) {
            // Try to find via LoadInst source
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
                evIt = enum_value_types_.find(load->getPointerOperand());
            }
        }
        if (evIt != enum_value_types_.end()) {
            auto &einfo = enum_types_[evIt->second];
            if (einfo.isADT) {
                // ADT enum: extract tag, print variant name + payload
                llvm::Value *tag = builder_.CreateExtractValue(val, 0, "adt.tag");
                llvm::Value *namePtr = builder_.CreateGEP(
                    llvm::ArrayType::get(ptrTy_, einfo.variantCount),
                    einfo.nameArray,
                    {llvm::ConstantInt::get(i64Ty_, 0), tag},
                    "enum_name_ptr");
                llvm::Value *nameStr = builder_.CreateLoad(ptrTy_, namePtr, "enum_name");

                // Check if this variant has associated data
                // We need to branch for each variant to print payload
                // Simple approach: store to alloca, iterate variants
                llvm::AllocaInst *adtAlloca = builder_.CreateAlloca(einfo.adtType, nullptr, "adt.print.tmp");
                builder_.CreateStore(val, adtAlloca);
                llvm::Value *payloadPtr = builder_.CreateStructGEP(einfo.adtType, adtAlloca, 1, "adt.print.payload");

                // Check if any variant has fields — create conditional printing
                bool anyFields = false;
                for (auto &[vn, vf] : einfo.variantFields)
                    if (!vf.fieldTypes.empty()) { anyFields = true; break; }

                if (anyFields) {
                    // Create blocks for each variant
                    llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "adt.print.end", fn_);
                    llvm::BasicBlock *defaultBB = llvm::BasicBlock::Create(*ctx_, "adt.print.default", fn_);

                    // Print variant name for data-less variants or with parentheses for data variants
                    auto *switchInst = builder_.CreateSwitch(tag, defaultBB, einfo.variantCount);

                    for (auto &[vname, vtag] : einfo.variants) {
                        llvm::BasicBlock *caseBB = llvm::BasicBlock::Create(*ctx_, "adt.print." + vname, fn_);
                        switchInst->addCase(llvm::cast<llvm::ConstantInt>(llvm::ConstantInt::get(i64Ty_, vtag)), caseBB);
                        builder_.SetInsertPoint(caseBB);

                        auto fit = einfo.variantFields.find(vname);
                        if (fit != einfo.variantFields.end() && !fit->second.fieldTypes.empty()) {
                            // Print "VariantName("
                            llvm::Constant *openFmt = cachedGlobalString("%s(", ".fmt_adt_open");
                            builder_.CreateCall(printfFn, {openFmt, nameStr});

                            const llvm::DataLayout &dl = mod_->getDataLayout();
                            size_t offset = 0;
                            for (size_t fi = 0; fi < fit->second.fieldTypes.size(); ++fi) {
                                llvm::Type *fieldTy = fit->second.fieldTypes[fi];
                                uint64_t align = dl.getABITypeAlign(fieldTy).value();
                                offset = (offset + align - 1) / align * align;
                                llvm::Value *fieldPtr = builder_.CreateGEP(
                                    llvm::Type::getInt8Ty(*ctx_), payloadPtr,
                                    {llvm::ConstantInt::get(i64Ty_, offset)},
                                    "adt.print.field." + std::to_string(fi));
                                llvm::Value *fieldVal = builder_.CreateLoad(fieldTy, fieldPtr, "field_val");

                                if (fi > 0) {
                                    llvm::Constant *commaFmt = cachedGlobalString(", ", ".fmt_comma");
                                    builder_.CreateCall(printfFn, {commaFmt});
                                }

                                if (fieldTy == i64Ty_) {
                                    llvm::Constant *fmt = cachedGlobalString("%lld", ".fmt_adt_int");
                                    builder_.CreateCall(printfFn, {fmt, fieldVal});
                                } else if (fieldTy == f64Ty_) {
                                    llvm::Constant *fmt = cachedGlobalString("%g", ".fmt_adt_float");
                                    builder_.CreateCall(printfFn, {fmt, fieldVal});
                                } else if (fieldTy == ptrTy_) {
                                    llvm::Constant *fmt = cachedGlobalString("%s", ".fmt_adt_str");
                                    builder_.CreateCall(printfFn, {fmt, fieldVal});
                                } else if (fieldTy == i1Ty_) {
                                    llvm::Value *ext = builder_.CreateZExt(fieldVal, i64Ty_);
                                    llvm::Value *trueStr = cachedGlobalString("true", ".true");
                                    llvm::Value *falseStr = cachedGlobalString("false", ".false");
                                    llvm::Value *str = builder_.CreateSelect(
                                        builder_.CreateICmpNE(ext, llvm::ConstantInt::get(i64Ty_, 0)),
                                        trueStr, falseStr);
                                    llvm::Constant *fmt = cachedGlobalString("%s", ".fmt_adt_bool");
                                    builder_.CreateCall(printfFn, {fmt, str});
                                }
                                offset += dl.getTypeAllocSize(fieldTy);
                            }

                            llvm::Constant *closeFmt = cachedGlobalString(")", ".fmt_adt_close");
                            builder_.CreateCall(printfFn, {closeFmt});
                        } else {
                            // No data — just print variant name
                            llvm::Constant *fmt = cachedGlobalString("%s", ".fmt_enum_nodata");
                            builder_.CreateCall(printfFn, {fmt, nameStr});
                        }
                        builder_.CreateBr(endBB);
                    }

                    builder_.SetInsertPoint(defaultBB);
                    builder_.CreateBr(endBB);
                    builder_.SetInsertPoint(endBB);
                } else {
                    llvm::Constant *fmt = cachedGlobalString("%s", ".fmt_enum");
                    builder_.CreateCall(printfFn, {fmt, nameStr});
                }
                return;
            }
            if (einfo.hasExplicitValues) {
                // Explicit values: use switch to map value → name string
                llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "enum.print.merge", fn_);
                llvm::BasicBlock *defaultBB = llvm::BasicBlock::Create(*ctx_, "enum.print.default", fn_);
                auto *sw = builder_.CreateSwitch(val, defaultBB, einfo.variantCount);

                // Build PHI node in merge block for the name string
                builder_.SetInsertPoint(mergeBB);
                auto *namePhi = builder_.CreatePHI(ptrTy_, einfo.variantCount, "enum.name");
                llvm::Constant *fmt = cachedGlobalString("%s", ".fmt_enum_explicit");
                builder_.CreateCall(printfFn, {fmt, namePhi});

                // Create a case for each variant
                for (size_t i = 0; i < einfo.variantOrder.size(); ++i) {
                    const auto &vname = einfo.variantOrder[i];
                    int64_t vval = einfo.variants.at(vname);
                    llvm::BasicBlock *caseBB = llvm::BasicBlock::Create(*ctx_, "enum.print." + vname, fn_);
                    sw->addCase(llvm::cast<llvm::ConstantInt>(llvm::ConstantInt::get(i64Ty_, vval, true)), caseBB);
                    builder_.SetInsertPoint(caseBB);
                    llvm::Value *namePtr = builder_.CreateGEP(
                        llvm::ArrayType::get(ptrTy_, einfo.variantCount),
                        einfo.nameArray,
                        {llvm::ConstantInt::get(i64Ty_, 0), llvm::ConstantInt::get(i64Ty_, i)},
                        "enum_name_ptr");
                    llvm::Value *nameStr = builder_.CreateLoad(ptrTy_, namePtr, "enum_name");
                    namePhi->addIncoming(nameStr, caseBB);
                    builder_.CreateBr(mergeBB);
                }

                // Default block (unreachable in practice)
                builder_.SetInsertPoint(defaultBB);
                llvm::Value *unknownStr = builder_.CreateGlobalString("?", ".enum_unknown");
                namePhi->addIncoming(unknownStr, defaultBB);
                builder_.CreateBr(mergeBB);

                // Continue after merge
                builder_.SetInsertPoint(mergeBB);
                // Move insert point after the print call
                return;
            }
            // Non-ADT enum (sequential values): use tag directly as index
            llvm::Value *namePtr = builder_.CreateGEP(
                llvm::ArrayType::get(ptrTy_, einfo.variantCount),
                einfo.nameArray,
                {llvm::ConstantInt::get(i64Ty_, 0), val},
                "enum_name_ptr");
            llvm::Value *nameStr = builder_.CreateLoad(ptrTy_, namePtr, "enum_name");
            llvm::Constant *fmt = cachedGlobalString("%s", ".fmt_enum_seq");
            builder_.CreateCall(printfFn, {fmt, nameStr});
            return;
        }
    }

    // Option type printing
    if (isOptionType(val->getType())) {
        llvm::Value *hasValue = builder_.CreateExtractValue(val, 0, "has_value");
        llvm::BasicBlock *someBB = llvm::BasicBlock::Create(*ctx_, "print.some", fn_);
        llvm::BasicBlock *noneBB = llvm::BasicBlock::Create(*ctx_, "print.none", fn_);
        llvm::BasicBlock *endBB  = llvm::BasicBlock::Create(*ctx_, "print.end", fn_);

        builder_.CreateCondBr(hasValue, someBB, noneBB);

        // None branch
        builder_.SetInsertPoint(noneBB);
        llvm::Constant *noneFmt = cachedGlobalString("None", ".fmt_none");
        builder_.CreateCall(printfFn, {noneFmt});
        builder_.CreateBr(endBB);

        // Some branch
        builder_.SetInsertPoint(someBB);
        llvm::Value *innerVal = builder_.CreateExtractValue(val, 1, "opt_value");
        llvm::Type *innerTy = innerVal->getType();

        llvm::Constant *somePrefix = cachedGlobalString("Some(", ".fmt_some_pre");
        builder_.CreateCall(printfFn, {somePrefix});

        emitPrintValue(innerVal, innerTy, printfFn, "_opt");

        llvm::Constant *someSuffix = cachedGlobalString(")", ".fmt_some_post");
        builder_.CreateCall(printfFn, {someSuffix});
        builder_.CreateBr(endBB);

        builder_.SetInsertPoint(endBB);
        return;
    }

    // Fixed-length array printing via IR loop (avoids O(N) inline unrolling)
    if (auto *ai = llvm::dyn_cast<llvm::AllocaInst>(val)) {
        if (auto *arrTy = llvm::dyn_cast<llvm::ArrayType>(ai->getAllocatedType())) {
            llvm::Type *elemTy = arrTy->getElementType();
            uint64_t arrSize = arrTy->getNumElements();

            llvm::Constant *lbracket = cachedGlobalString("[", ".fmt_arr_lb");
            llvm::Constant *rbracket = cachedGlobalString("]", ".fmt_arr_rb");
            llvm::Constant *comma = cachedGlobalString(", ", ".fmt_arr_comma");
            builder_.CreateCall(printfFn, {lbracket});

            llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "arr_print.cond", fn_);
            llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "arr_print.body", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "arr_print.end", fn_);

            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "arr_print_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            builder_.CreateBr(condBB);

            builder_.SetInsertPoint(condBB);
            llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "i");
            llvm::Value *cond = builder_.CreateICmpSLT(iVal, llvm::ConstantInt::get(i64Ty_, arrSize), "arr_print_cond");
            builder_.CreateCondBr(cond, bodyBB, endBB);

            builder_.SetInsertPoint(bodyBB);
            llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "i_cur");

            // Print comma separator if not first element
            llvm::Value *notFirst = builder_.CreateICmpSGT(iCur, llvm::ConstantInt::get(i64Ty_, 0), "not_first");
            llvm::BasicBlock *commaBB = llvm::BasicBlock::Create(*ctx_, "arr_print.comma", fn_);
            llvm::BasicBlock *elemBB = llvm::BasicBlock::Create(*ctx_, "arr_print.elem", fn_);
            builder_.CreateCondBr(notFirst, commaBB, elemBB);

            builder_.SetInsertPoint(commaBB);
            builder_.CreateCall(printfFn, {comma});
            builder_.CreateBr(elemBB);

            builder_.SetInsertPoint(elemBB);
            llvm::Value *iElem = builder_.CreateLoad(i64Ty_, iVar, "i_elem");
            llvm::Value *elemPtr = builder_.CreateGEP(
                arrTy, ai,
                {llvm::ConstantInt::get(i64Ty_, 0), iElem},
                "arr_print_ptr");
            llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "arr_print_elem");
            emitPrintValue(elem, elemTy, printfFn, "_arr");

            llvm::Value *iNext = builder_.CreateAdd(iElem, llvm::ConstantInt::get(i64Ty_, 1), "i_next");
            builder_.CreateStore(iNext, iVar);
            builder_.CreateBr(condBB);

            builder_.SetInsertPoint(endBB);
            builder_.CreateCall(printfFn, {rbracket});
            return;
        }
    }

    // Set/Map/List printing: check if ptr type
    if (val->getType() == ptrTy_) {
        // Check if it's a set
        llvm::Type *setElemTy = getSetElementType(val);
        if (setElemTy) {
            llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, val, 0, "set_len_ptr");
            llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "set_len");
            llvm::Value *elemsPtrField = builder_.CreateStructGEP(setHeaderTy_, val, 2, "set_elems_ptr");
            llvm::Value *elemsPtr = builder_.CreateLoad(ptrTy_, elemsPtrField, "set_elems");

            llvm::Constant *lbrace = cachedGlobalString("{", ".fmt_set_lb");
            llvm::Constant *rbrace = cachedGlobalString("}", ".fmt_set_rb");
            llvm::Constant *comma = cachedGlobalString(", ", ".fmt_set_comma");
            builder_.CreateCall(printfFn, {lbrace});

            llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "print_set.cond", fn_);
            llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "print_set.body", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "print_set.end", fn_);

            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "print_set_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            builder_.CreateBr(condBB);

            builder_.SetInsertPoint(condBB);
            llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "i");
            llvm::Value *cond = builder_.CreateICmpSLT(iVal, length, "cond");
            builder_.CreateCondBr(cond, bodyBB, endBB);

            builder_.SetInsertPoint(bodyBB);
            llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "i_cur");

            llvm::Value *notFirst = builder_.CreateICmpSGT(iCur, llvm::ConstantInt::get(i64Ty_, 0), "not_first");
            llvm::BasicBlock *commaBB = llvm::BasicBlock::Create(*ctx_, "print_set.comma", fn_);
            llvm::BasicBlock *elemBB = llvm::BasicBlock::Create(*ctx_, "print_set.elem", fn_);
            builder_.CreateCondBr(notFirst, commaBB, elemBB);

            builder_.SetInsertPoint(commaBB);
            builder_.CreateCall(printfFn, {comma});
            builder_.CreateBr(elemBB);

            builder_.SetInsertPoint(elemBB);
            llvm::Value *iElem = builder_.CreateLoad(i64Ty_, iVar, "i_elem");
            llvm::Value *elemPtr = builder_.CreateGEP(setElemTy, elemsPtr, {iElem}, "elem_ptr");
            llvm::Value *elem = builder_.CreateLoad(setElemTy, elemPtr, "elem");
            emitPrintValue(elem, setElemTy, printfFn, "_s");

            llvm::Value *iNext = builder_.CreateAdd(iElem, llvm::ConstantInt::get(i64Ty_, 1), "i_next");
            builder_.CreateStore(iNext, iVar);
            builder_.CreateBr(condBB);

            builder_.SetInsertPoint(endBB);
            builder_.CreateCall(printfFn, {rbrace});
            return;
        }

        // Check if it's a map first
        llvm::Type *mapKeyTy = getMapKeyType(val);
        llvm::Type *mapValTy = getMapValueType(val);
        if (mapKeyTy && mapValTy) {
            // Print map as {key: value, key: value}
            llvm::Value *lenPtr = builder_.CreateStructGEP(mapHeaderTy_, val, 0, "map_len_ptr");
            llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "map_len");
            llvm::Value *keysPtrField = builder_.CreateStructGEP(mapHeaderTy_, val, 2, "map_keys_ptr");
            llvm::Value *keysPtr = builder_.CreateLoad(ptrTy_, keysPtrField, "map_keys");
            llvm::Value *valsPtrField = builder_.CreateStructGEP(mapHeaderTy_, val, 3, "map_vals_ptr");
            llvm::Value *valsPtr = builder_.CreateLoad(ptrTy_, valsPtrField, "map_vals");

            llvm::Constant *lbrace = cachedGlobalString("{", ".fmt_lbrace");
            llvm::Constant *rbrace = cachedGlobalString("}", ".fmt_rbrace");
            llvm::Constant *comma = cachedGlobalString(", ", ".fmt_comma_m");
            llvm::Constant *colon = cachedGlobalString(": ", ".fmt_colon");
            builder_.CreateCall(printfFn, {lbrace});

            // Loop through entries
            llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "print_map.cond", fn_);
            llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "print_map.body", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "print_map.end", fn_);

            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "print_map_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            builder_.CreateBr(condBB);

            builder_.SetInsertPoint(condBB);
            llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "i");
            llvm::Value *cond = builder_.CreateICmpSLT(iVal, length, "cond");
            builder_.CreateCondBr(cond, bodyBB, endBB);

            builder_.SetInsertPoint(bodyBB);
            llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "i_cur");

            // Print comma if not first
            llvm::Value *notFirst = builder_.CreateICmpSGT(iCur, llvm::ConstantInt::get(i64Ty_, 0), "not_first");
            llvm::BasicBlock *commaBB = llvm::BasicBlock::Create(*ctx_, "print_map.comma", fn_);
            llvm::BasicBlock *kvBB = llvm::BasicBlock::Create(*ctx_, "print_map.kv", fn_);
            builder_.CreateCondBr(notFirst, commaBB, kvBB);

            builder_.SetInsertPoint(commaBB);
            builder_.CreateCall(printfFn, {comma});
            builder_.CreateBr(kvBB);

            builder_.SetInsertPoint(kvBB);
            llvm::Value *iKV = builder_.CreateLoad(i64Ty_, iVar, "i_kv");

            // Print key
            llvm::Value *keyPtr = builder_.CreateGEP(mapKeyTy, keysPtr, {iKV}, "key_ptr");
            llvm::Value *keyVal = builder_.CreateLoad(mapKeyTy, keyPtr, "key_val");
            emitPrintValue(keyVal, mapKeyTy, printfFn, "_mk");

            builder_.CreateCall(printfFn, {colon});

            // Print value
            llvm::Value *valPtr = builder_.CreateGEP(mapValTy, valsPtr, {iKV}, "val_ptr");
            llvm::Value *valVal = builder_.CreateLoad(mapValTy, valPtr, "val_val");
            emitPrintValue(valVal, mapValTy, printfFn, "_mv");

            // i++
            llvm::Value *iNext = builder_.CreateAdd(iKV, llvm::ConstantInt::get(i64Ty_, 1), "i_next");
            builder_.CreateStore(iNext, iVar);
            builder_.CreateBr(condBB);

            builder_.SetInsertPoint(endBB);
            builder_.CreateCall(printfFn, {rbrace});
            return;
        }

        // Check if it's a list - try to find element type
        llvm::Type *elemTy = getListElementType(val);
        if (elemTy) {
            // Print list as [elem, elem, ...]
            llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, val, 0, "len_ptr");
            llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "length");
            llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, val, 2, "data_ptr");
            llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataPtrField, "data");

            llvm::Constant *lbracket = cachedGlobalString("[", ".fmt_lb");
            llvm::Constant *rbracket = cachedGlobalString("]", ".fmt_rb");
            llvm::Constant *comma = cachedGlobalString(", ", ".fmt_comma");
            builder_.CreateCall(printfFn, {lbracket});

            // Loop through elements
            llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "print_list.cond", fn_);
            llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "print_list.body", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "print_list.end", fn_);

            // i = 0
            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "print_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            builder_.CreateBr(condBB);

            // cond: i < length
            builder_.SetInsertPoint(condBB);
            llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "i");
            llvm::Value *cond = builder_.CreateICmpSLT(iVal, length, "cond");
            builder_.CreateCondBr(cond, bodyBB, endBB);

            // body: print comma if i > 0, then print element
            builder_.SetInsertPoint(bodyBB);
            llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "i_cur");
            llvm::Value *notFirst = builder_.CreateICmpSGT(iCur, llvm::ConstantInt::get(i64Ty_, 0), "not_first");

            llvm::BasicBlock *commaBB = llvm::BasicBlock::Create(*ctx_, "print_list.comma", fn_);
            llvm::BasicBlock *elemBB = llvm::BasicBlock::Create(*ctx_, "print_list.elem", fn_);
            builder_.CreateCondBr(notFirst, commaBB, elemBB);

            builder_.SetInsertPoint(commaBB);
            builder_.CreateCall(printfFn, {comma});
            builder_.CreateBr(elemBB);

            builder_.SetInsertPoint(elemBB);
            llvm::Value *iElem = builder_.CreateLoad(i64Ty_, iVar, "i_elem");
            llvm::Value *elemPtr = builder_.CreateGEP(elemTy, dataPtr, {iElem}, "elem_ptr");
            llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "elem");

            emitPrintValue(elem, elemTy, printfFn, "_l");

            // i = i + 1
            llvm::Value *iNext = builder_.CreateAdd(iElem, llvm::ConstantInt::get(i64Ty_, 1), "i_next");
            builder_.CreateStore(iNext, iVar);
            builder_.CreateBr(condBB);

            builder_.SetInsertPoint(endBB);
            builder_.CreateCall(printfFn, {rbracket});
            return;
        }
    }

    // Union type printing: check if value is a union
    if (auto *st = llvm::dyn_cast<llvm::StructType>(val->getType())) {
        std::string unionName;
        // Try to find union info by struct type
        for (auto &[name, info] : union_type_info_) {
            if (info.llvmType == st) { unionName = name; break; }
        }
        if (!unionName.empty()) {
            auto &info = union_type_info_[unionName];
            llvm::Value *tag = builder_.CreateExtractValue(val, 0, "union.tag");

            // Alloca only the data part (not the full union struct) for type punning
            llvm::Value *dataBytes = builder_.CreateExtractValue(val, 1, "union.data");
            auto *dataTy = info.llvmType->getElementType(1);
            llvm::AllocaInst *dataTmp = builder_.CreateAlloca(dataTy, nullptr, "union.data.tmp");
            dataTmp->setAlignment(mod_->getDataLayout().getABITypeAlign(info.llvmType));
            builder_.CreateStore(dataBytes, dataTmp);

            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "union.print.end", fn_);
            llvm::SwitchInst *sw = builder_.CreateSwitch(tag, endBB, info.componentTypes.size());

            for (size_t i = 0; i < info.componentTypes.size(); ++i) {
                llvm::BasicBlock *caseBB = llvm::BasicBlock::Create(
                    *ctx_, "union.print.case" + std::to_string(i), fn_);
                sw->addCase(llvm::ConstantInt::get(llvm::cast<llvm::IntegerType>(i64Ty_), i), caseBB);
                builder_.SetInsertPoint(caseBB);

                llvm::Value *innerVal = builder_.CreateLoad(
                    info.componentTypes[i], dataTmp, "union.inner");

                emitPrintValue(innerVal, info.componentTypes[i], printfFn, "_union" + std::to_string(i));

                builder_.CreateBr(endBB);
            }

            builder_.SetInsertPoint(endBB);
            return;
        }
    }

    if (val->getType() == errorTy_) {
        emitPrintValue(val, errorTy_, printfFn, "_err");
        return;
    }

    if (val->getType() == anyTy_) {
        llvm::Value *str = emitAnyToString(val);
        llvm::Constant *fmt = cachedGlobalString("%s", ".fmt_any_np");
        builder_.CreateCall(printfFn, {fmt, str});
        return;
    }

    if (auto *structTy = llvm::dyn_cast<llvm::StructType>(val->getType())) {
        std::string name = structTy->getName().str();
        if (struct_types_.count(name)) {
            llvm::Value *str = structToString(val);
            llvm::Constant *fmt = cachedGlobalString("%s", ".fmt_struct_np");
            builder_.CreateCall(printfFn, {fmt, str});
            return;
        }
        codegenError("print() does not support this struct type");
    }

    // Delegate primitive types to emitPrintValue (handles signed/unsigned correctly)
    emitPrintValue(val, val->getType(), printfFn, "_ps");
}
