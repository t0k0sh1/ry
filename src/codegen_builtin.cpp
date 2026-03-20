#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <stdexcept>

// ===== Collection helpers =====

// Step 2: Unified collection type lookup helper
llvm::Type *CodeGen::lookupCollectionType(
    const std::unordered_map<llvm::Value*, llvm::Type*> &map, llvm::Value *val) {
    auto it = map.find(val);
    if (it != map.end()) return it->second;
    if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
        auto it2 = map.find(load->getPointerOperand());
        if (it2 != map.end()) return it2->second;
    }
    return nullptr;
}

llvm::Type *CodeGen::getListElementType(llvm::Value *listAlloca) {
    return lookupCollectionType(list_element_types_, listAlloca);
}

llvm::Type *CodeGen::getMapKeyType(llvm::Value *mapVal) {
    return lookupCollectionType(map_key_types_, mapVal);
}

llvm::Type *CodeGen::getMapValueType(llvm::Value *mapVal) {
    return lookupCollectionType(map_value_types_, mapVal);
}

llvm::Type *CodeGen::getSetElementType(llvm::Value *setVal) {
    return lookupCollectionType(set_element_types_, setVal);
}

llvm::Type *CodeGen::getNestedListElementType(llvm::Value *listVal) {
    return lookupCollectionType(nested_list_element_types_, listVal);
}

// Step 1: Hash function resolution helper
CodeGen::HashFnInfo CodeGen::resolveHashFn(llvm::Type *keyTy) {
    if (keyTy == ptrTy_)
        return {"__ry_hash_str", "__ry_ht_rehash_str", ptrTy_};
    if (keyTy->isDoubleTy())
        return {"__ry_hash_f64", "__ry_ht_rehash_f64", f64Ty_};
    return {"__ry_hash_i64", "__ry_ht_rehash_i64", i64Ty_};
}

// Step 3: Unified hash table lookup
llvm::Value *CodeGen::emitHashTableLookup(llvm::Value *containerPtr, llvm::StructType *headerTy,
                                            const HashTableLayout &layout,
                                            llvm::Value *key, llvm::Type *keyTy) {
    llvm::Value *bucketCountPtr = builder_.CreateStructGEP(headerTy, containerPtr, layout.bucketCountIdx, "ht_bc_ptr");
    llvm::Value *bucketCount = builder_.CreateLoad(i64Ty_, bucketCountPtr, "ht_bc");
    llvm::Value *bucketMask = builder_.CreateSub(bucketCount, llvm::ConstantInt::get(i64Ty_, 1), "ht_bmask");
    llvm::Value *bucketsField = builder_.CreateStructGEP(headerTy, containerPtr, layout.bucketsPtrIdx, "ht_buckets_ptr");
    llvm::Value *bucketsPtr = builder_.CreateLoad(ptrTy_, bucketsField, "ht_buckets");

    llvm::Value *lenPtr = builder_.CreateStructGEP(headerTy, containerPtr, layout.lenIdx, "ht_len_ptr");
    llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "ht_len");
    llvm::Value *keysPtrField = builder_.CreateStructGEP(headerTy, containerPtr, layout.keysPtrIdx, "ht_keys_ptr");
    llvm::Value *keysPtr = builder_.CreateLoad(ptrTy_, keysPtrField, "ht_keys");

    std::string fnName;
    llvm::Type *keyArgTy;
    if (keyTy == ptrTy_) {
        fnName = "__ry_ht_find_str";
        keyArgTy = ptrTy_;
    } else if (keyTy->isDoubleTy()) {
        fnName = "__ry_ht_find_f64";
        keyArgTy = f64Ty_;
    } else {
        fnName = "__ry_ht_find_i64";
        keyArgTy = i64Ty_;
    }

    llvm::FunctionType *findTy = llvm::FunctionType::get(
        i64Ty_, {ptrTy_, i64Ty_, ptrTy_, i64Ty_, keyArgTy}, false);
    llvm::FunctionCallee findFn = mod_->getOrInsertFunction(fnName, findTy);

    llvm::Value *keyArg = key;
    if (keyTy != keyArgTy && keyTy->isIntegerTy() && keyArgTy->isIntegerTy())
        keyArg = builder_.CreateZExt(key, keyArgTy, "key_ext");

    return builder_.CreateCall(findFn, {bucketsPtr, bucketMask, keysPtr, length, keyArg}, "ht_lookup_idx");
}

llvm::Value *CodeGen::emitSetElementLookup(llvm::Value *setPtr, llvm::Value *elem, llvm::Type *elemTy) {
    return emitHashTableLookup(setPtr, setHeaderTy_, {0, 3, 4, 2}, elem, elemTy);
}

llvm::Value *CodeGen::emitMapKeyLookup(llvm::Value *mapPtr, llvm::Value *key, llvm::Type *keyTy) {
    return emitHashTableLookup(mapPtr, mapHeaderTy_, {0, 4, 5, 2}, key, keyTy);
}

// Helper: initialize bucket array fields in a header
void CodeGen::emitBucketInit(llvm::Value *headerPtr, llvm::StructType *headerTy,
                              unsigned bucketCountIdx, unsigned bucketsPtrIdx,
                              int64_t initialBucketCount) {
    llvm::FunctionType *mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
    llvm::FunctionCallee mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
    llvm::FunctionType *memsetTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, i32Ty_, i64Ty_}, false);
    llvm::FunctionCallee memsetFn = mod_->getOrInsertFunction("memset", memsetTy);

    int64_t bucketBytes = initialBucketCount * 8; // sizeof(int64_t)
    llvm::Value *bucketsPtr = builder_.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty_, bucketBytes)}, "buckets");
    // Fill with 0xFF bytes → each int64_t becomes -1 (EMPTY)
    builder_.CreateCall(memsetFn, {bucketsPtr,
        llvm::ConstantInt::get(i32Ty_, 0xFF),
        llvm::ConstantInt::get(i64Ty_, bucketBytes)});

    llvm::Value *bcPtr = builder_.CreateStructGEP(headerTy, headerPtr, bucketCountIdx, "bc_ptr");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, initialBucketCount), bcPtr);
    llvm::Value *bpPtr = builder_.CreateStructGEP(headerTy, headerPtr, bucketsPtrIdx, "bp_ptr");
    builder_.CreateStore(bucketsPtr, bpPtr);
}

// Helper: insert key into bucket + check load factor and rehash if needed
void CodeGen::emitBucketInsertAndRehashCheck(llvm::Value *headerPtr, llvm::StructType *headerTy,
                                              unsigned lenIdx, unsigned bucketCountIdx, unsigned bucketsPtrIdx,
                                              llvm::Value *key, llvm::Type *keyTy, llvm::Value *denseIndex) {
    auto hfi = resolveHashFn(keyTy);

    // Coerce key to match hash function argument type (e.g. i1 → i64)
    llvm::Value *hashKey = key;
    if (keyTy != hfi.hashArgTy && keyTy->isIntegerTy() && hfi.hashArgTy->isIntegerTy())
        hashKey = builder_.CreateZExt(key, hfi.hashArgTy, "hash_key_zext");

    // Compute hash
    llvm::FunctionType *hashTy = llvm::FunctionType::get(i64Ty_, {hfi.hashArgTy}, false);
    llvm::FunctionCallee hashFn = mod_->getOrInsertFunction(hfi.hashFnName, hashTy);
    llvm::Value *hashVal = builder_.CreateCall(hashFn, {hashKey}, "hash_val");

    // Insert into buckets
    llvm::Value *bucketsField = builder_.CreateStructGEP(headerTy, headerPtr, bucketsPtrIdx, "bp_field");
    llvm::Value *bucketsPtr = builder_.CreateLoad(ptrTy_, bucketsField, "buckets");
    llvm::Value *bcField = builder_.CreateStructGEP(headerTy, headerPtr, bucketCountIdx, "bc_field");
    llvm::Value *bucketCount = builder_.CreateLoad(i64Ty_, bcField, "bc");
    llvm::Value *bucketMask = builder_.CreateSub(bucketCount, llvm::ConstantInt::get(i64Ty_, 1), "bmask");

    llvm::FunctionType *insertTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {ptrTy_, i64Ty_, i64Ty_, i64Ty_}, false);
    llvm::FunctionCallee insertFn = mod_->getOrInsertFunction("__ry_ht_insert", insertTy);
    builder_.CreateCall(insertFn, {bucketsPtr, bucketMask, hashVal, denseIndex});

    // Check load factor: len * 4 > bucketCount * 3 (i.e. len/bucketCount > 75%)
    llvm::Value *lenPtr = builder_.CreateStructGEP(headerTy, headerPtr, lenIdx, "len_for_rehash");
    llvm::Value *len = builder_.CreateLoad(i64Ty_, lenPtr, "len_rehash");
    llvm::Value *len4 = builder_.CreateMul(len, llvm::ConstantInt::get(i64Ty_, 4), "len4");
    llvm::Value *bc3 = builder_.CreateMul(bucketCount, llvm::ConstantInt::get(i64Ty_, 3), "bc3");
    llvm::Value *needRehash = builder_.CreateICmpSGT(len4, bc3, "need_rehash");

    llvm::BasicBlock *rehashBB = llvm::BasicBlock::Create(*ctx_, "rehash", fn_);
    llvm::BasicBlock *doneRehashBB = llvm::BasicBlock::Create(*ctx_, "rehash.done", fn_);
    builder_.CreateCondBr(needRehash, rehashBB, doneRehashBB);

    builder_.SetInsertPoint(rehashBB);
    // newBucketCount = bucketCount * 2
    llvm::Value *bcCur = builder_.CreateLoad(i64Ty_, bcField, "bc_cur");
    llvm::Value *newBc = builder_.CreateMul(bcCur, llvm::ConstantInt::get(i64Ty_, 2), "new_bc");

    // Get keys/elems pointer (field index 2 for both map and set)
    llvm::Value *keysField = builder_.CreateStructGEP(headerTy, headerPtr, 2, "keys_for_rehash");
    llvm::Value *keysPtr = builder_.CreateLoad(ptrTy_, keysField, "keys_rehash");
    llvm::Value *lenForRehash = builder_.CreateLoad(i64Ty_, lenPtr, "len_for_rehash2");

    llvm::FunctionType *rehashTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, i64Ty_, i64Ty_}, false);
    llvm::FunctionCallee rehashFn = mod_->getOrInsertFunction(hfi.rehashFnName, rehashTy);
    llvm::Value *newBuckets = builder_.CreateCall(rehashFn, {keysPtr, lenForRehash, newBc}, "new_buckets");

    // Free old buckets
    llvm::FunctionType *freeTy = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
    llvm::FunctionCallee freeFn = mod_->getOrInsertFunction("free", freeTy);
    llvm::Value *oldBuckets = builder_.CreateLoad(ptrTy_, bucketsField, "old_buckets");
    builder_.CreateCall(freeFn, {oldBuckets});

    // Store new buckets and count
    builder_.CreateStore(newBuckets, bucketsField);
    builder_.CreateStore(newBc, bcField);

    builder_.CreateBr(doneRehashBB);
    builder_.SetInsertPoint(doneRehashBB);
}

// ===== emitPrint =====

void CodeGen::emitPrint(const std::vector<ExprPtr> &args) {
    if (args.size() != 1)
        codegenError("print() takes exactly 1 argument");

    llvm::FunctionType *printfTy = llvm::FunctionType::get(
        i32Ty_, {llvm::PointerType::getUnqual(*ctx_)}, /*isVarArg=*/true);
    llvm::FunctionCallee printfFn = mod_->getOrInsertFunction("printf", printfTy);

    llvm::Value *val = emitExpr(*args[0]);

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
                            llvm::Constant *openFmt = builder_.CreateGlobalString("%s(", ".fmt_adt_open");
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
                                    llvm::Constant *commaFmt = builder_.CreateGlobalString(", ", ".fmt_comma");
                                    builder_.CreateCall(printfFn, {commaFmt});
                                }

                                if (fieldTy == i64Ty_) {
                                    llvm::Constant *fmt = builder_.CreateGlobalString("%lld", ".fmt_adt_int");
                                    builder_.CreateCall(printfFn, {fmt, fieldVal});
                                } else if (fieldTy == f64Ty_) {
                                    llvm::Constant *fmt = builder_.CreateGlobalString("%g", ".fmt_adt_float");
                                    builder_.CreateCall(printfFn, {fmt, fieldVal});
                                } else if (fieldTy == ptrTy_) {
                                    llvm::Constant *fmt = builder_.CreateGlobalString("%s", ".fmt_adt_str");
                                    builder_.CreateCall(printfFn, {fmt, fieldVal});
                                } else if (fieldTy == i1Ty_) {
                                    llvm::Value *ext = builder_.CreateZExt(fieldVal, i64Ty_);
                                    llvm::Value *trueStr = builder_.CreateGlobalString("true", ".true");
                                    llvm::Value *falseStr = builder_.CreateGlobalString("false", ".false");
                                    llvm::Value *str = builder_.CreateSelect(
                                        builder_.CreateICmpNE(ext, llvm::ConstantInt::get(i64Ty_, 0)),
                                        trueStr, falseStr);
                                    llvm::Constant *fmt = builder_.CreateGlobalString("%s", ".fmt_adt_bool");
                                    builder_.CreateCall(printfFn, {fmt, str});
                                }
                                offset += dl.getTypeAllocSize(fieldTy);
                            }

                            llvm::Constant *closeFmt = builder_.CreateGlobalString(")\n", ".fmt_adt_close");
                            builder_.CreateCall(printfFn, {closeFmt});
                        } else {
                            // No data — just print variant name
                            llvm::Constant *fmt = builder_.CreateGlobalString("%s\n", ".fmt_enum_nodata");
                            builder_.CreateCall(printfFn, {fmt, nameStr});
                        }
                        builder_.CreateBr(endBB);
                    }

                    builder_.SetInsertPoint(defaultBB);
                    builder_.CreateBr(endBB);
                    builder_.SetInsertPoint(endBB);
                } else {
                    llvm::Constant *fmt = builder_.CreateGlobalString("%s\n", ".fmt_enum");
                    builder_.CreateCall(printfFn, {fmt, nameStr});
                }
                return;
            }
            // Non-ADT enum: use tag directly as index
            llvm::Value *namePtr = builder_.CreateGEP(
                llvm::ArrayType::get(ptrTy_, einfo.variantCount),
                einfo.nameArray,
                {llvm::ConstantInt::get(i64Ty_, 0), val},
                "enum_name_ptr");
            llvm::Value *nameStr = builder_.CreateLoad(ptrTy_, namePtr, "enum_name");
            llvm::Constant *fmt = builder_.CreateGlobalString("%s\n", ".fmt_enum");
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
        llvm::Constant *noneFmt = builder_.CreateGlobalString("None\n", ".fmt_none");
        builder_.CreateCall(printfFn, {noneFmt});
        builder_.CreateBr(endBB);

        // Some branch
        builder_.SetInsertPoint(someBB);
        llvm::Value *innerVal = builder_.CreateExtractValue(val, 1, "opt_value");
        llvm::Type *innerTy = innerVal->getType();

        llvm::Constant *somePrefix = builder_.CreateGlobalString("Some(", ".fmt_some_pre");
        builder_.CreateCall(printfFn, {somePrefix});

        emitPrintValue(innerVal, innerTy, printfFn, "_opt");

        llvm::Constant *someSuffix = builder_.CreateGlobalString(")\n", ".fmt_some_post");
        builder_.CreateCall(printfFn, {someSuffix});
        builder_.CreateBr(endBB);

        builder_.SetInsertPoint(endBB);
        return;
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

            llvm::Constant *lbrace = builder_.CreateGlobalString("{", ".fmt_set_lb");
            llvm::Constant *rbrace = builder_.CreateGlobalString("}\n", ".fmt_set_rb");
            llvm::Constant *comma = builder_.CreateGlobalString(", ", ".fmt_set_comma");
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

            llvm::Constant *lbrace = builder_.CreateGlobalString("{", ".fmt_lbrace");
            llvm::Constant *rbrace = builder_.CreateGlobalString("}\n", ".fmt_rbrace");
            llvm::Constant *comma = builder_.CreateGlobalString(", ", ".fmt_comma_m");
            llvm::Constant *colon = builder_.CreateGlobalString(": ", ".fmt_colon");
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

            llvm::Constant *lbracket = builder_.CreateGlobalString("[", ".fmt_lb");
            llvm::Constant *rbracketNl = builder_.CreateGlobalString("]\n", ".fmt_rb");
            llvm::Constant *comma = builder_.CreateGlobalString(", ", ".fmt_comma");
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
            builder_.CreateCall(printfFn, {rbracketNl});
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

            // Store the union value in memory so we can extract data via GEP
            llvm::AllocaInst *unionTmp = builder_.CreateAlloca(info.llvmType, nullptr, "union.print.tmp");
            builder_.CreateStore(val, unionTmp);
            auto *dataPtr = builder_.CreateStructGEP(info.llvmType, unionTmp, 1, "union.data.ptr");

            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "union.print.end", fn_);
            llvm::SwitchInst *sw = builder_.CreateSwitch(tag, endBB, info.componentTypes.size());

            for (size_t i = 0; i < info.componentTypes.size(); ++i) {
                llvm::BasicBlock *caseBB = llvm::BasicBlock::Create(
                    *ctx_, "union.print.case" + std::to_string(i), fn_);
                sw->addCase(llvm::ConstantInt::get(llvm::cast<llvm::IntegerType>(i64Ty_), i), caseBB);
                builder_.SetInsertPoint(caseBB);

                llvm::Value *innerVal = builder_.CreateLoad(
                    info.componentTypes[i], dataPtr, "union.inner");

                emitPrintValue(innerVal, info.componentTypes[i], printfFn, "_union" + std::to_string(i));

                llvm::Constant *nl = builder_.CreateGlobalString("\n", ".fmt_nl_union" + std::to_string(i));
                builder_.CreateCall(printfFn, {nl});
                builder_.CreateBr(endBB);
            }

            builder_.SetInsertPoint(endBB);
            return;
        }
    }

    if (val->getType() == errorTy_) {
        emitPrintValue(val, errorTy_, printfFn, "_err");
        llvm::Constant *nl = builder_.CreateGlobalString("\n", ".fmt_nl_err");
        builder_.CreateCall(printfFn, {nl});
        return;
    }

    if (llvm::isa<llvm::StructType>(val->getType()))
        codegenError("print() does not support struct types");

    if (val->getType() == i1Ty_) {
        llvm::Constant *trueStr  = builder_.CreateGlobalString("true\n",  ".fmt_true");
        llvm::Constant *falseStr = builder_.CreateGlobalString("false\n", ".fmt_false");
        llvm::Value *fmtPtr = builder_.CreateSelect(val, trueStr, falseStr, "bool_fmt");
        builder_.CreateCall(printfFn, {fmtPtr});
        return;
    }

    if (val->getType()->isPointerTy()) {
        llvm::Constant *fmt = builder_.CreateGlobalString("%s\n", ".fmt_s");
        builder_.CreateCall(printfFn, {fmt, val});
        return;
    }

    llvm::Constant *fmt;
    if (val->getType()->isDoubleTy())
        fmt = builder_.CreateGlobalString("%g\n", ".fmt_f");
    else
        fmt = builder_.CreateGlobalString("%ld\n", ".fmt_i");

    builder_.CreateCall(printfFn, {fmt, val});
}

// ===== Test: describe/it (trailing block) =====

static LambdaExpr &extractTrailingLambda(CallStmt &s, const std::string &callee) {
    if (s.args.size() != 2)
        throw std::runtime_error(callee + "() requires exactly one description string and a trailing block");
    auto *lambda = std::get_if<std::unique_ptr<LambdaExpr>>(&s.args.back()->data);
    if (!lambda)
        throw std::runtime_error(callee + "() last argument must be a trailing block");
    return **lambda;
}

void CodeGen::emitDescribeCall(CallStmt &s) {
    if (!test_mode_)
        codegenError("'describe' is only allowed in test mode (use 'ry test')");

    auto &lambda = extractTrailingLambda(s, "describe");

    llvm::FunctionType *voidStrTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
    llvm::FunctionType *voidTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), false);

    llvm::FunctionCallee descBeginFn = mod_->getOrInsertFunction("__ry_test_describe_begin", voidStrTy);
    llvm::FunctionCallee descEndFn   = mod_->getOrInsertFunction("__ry_test_describe_end", voidTy);

    llvm::Value *descName = emitExpr(*s.args[0]);
    if (!descName->getType()->isPointerTy())
        codegenError("describe() first argument must be a string");
    builder_.CreateCall(descBeginFn, {descName});

    for (auto &stmt : lambda.body)
        std::visit([this](auto &st) { emitStmt(st); }, stmt);

    builder_.CreateCall(descEndFn);
}

void CodeGen::emitItCall(CallStmt &s) {
    if (!test_mode_)
        codegenError("'it' is only allowed in test mode (use 'ry test')");

    auto &lambda = extractTrailingLambda(s, "it");

    llvm::FunctionType *voidStrTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
    llvm::FunctionType *voidTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), false);

    llvm::FunctionCallee itBeginFn = mod_->getOrInsertFunction("__ry_test_it_begin", voidStrTy);
    llvm::FunctionCallee itEndFn   = mod_->getOrInsertFunction("__ry_test_it_end", voidTy);

    llvm::Value *itName = emitExpr(*s.args[0]);
    if (!itName->getType()->isPointerTy())
        codegenError("it() first argument must be a string");

    // Create a test function for this it-block
    std::string testFnName = "__test_" + std::to_string(test_fn_counter_++);
    llvm::FunctionType *testFt = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), false);
    llvm::Function *testFunc = llvm::Function::Create(
        testFt, llvm::Function::InternalLinkage, testFnName, *mod_);

    {
        FnScope guard(*this);
        fn_ = testFunc;
        pushScope();

        llvm::BasicBlock *entry = llvm::BasicBlock::Create(*ctx_, "entry", testFunc);
        builder_.SetInsertPoint(entry);

        for (auto &stmt : lambda.body)
            std::visit([this](auto &st) { emitStmt(st); }, stmt);

        if (!builder_.GetInsertBlock()->getTerminator())
            builder_.CreateRetVoid();

        std::string err;
        llvm::raw_string_ostream errStream(err);
        if (llvm::verifyFunction(*testFunc, &errStream))
            codegenError("IR verify error in test: " + err);
    }

    builder_.CreateCall(itBeginFn, {itName});
    builder_.CreateCall(testFunc);
    builder_.CreateCall(itEndFn);
}

// ===== Test: ExpectStmt =====

void CodeGen::emitStmt(ExpectStmt &s) {
    if (!test_mode_)
        codegenError("'expect' is only allowed in test mode (use 'ry test')");

    llvm::Value *actualVal = emitExpr(*s.actual);
    llvm::Type *actualTy = actualVal->getType();

    llvm::Value *cmpResult = nullptr;

    if (s.matcher == "to_eq" || s.matcher == "to_not_eq") {
        llvm::Value *expectedVal = emitExpr(*s.expected);
        llvm::Type *expectedTy = expectedVal->getType();

        llvm::Value *eqResult = nullptr;
        if (actualTy == i64Ty_ && expectedTy == i64Ty_) {
            eqResult = builder_.CreateICmpEQ(actualVal, expectedVal, "eq");
        } else if (actualTy == f64Ty_ && expectedTy == f64Ty_) {
            eqResult = builder_.CreateFCmpOEQ(actualVal, expectedVal, "eq");
        } else if (actualTy == i1Ty_ && expectedTy == i1Ty_) {
            eqResult = builder_.CreateICmpEQ(actualVal, expectedVal, "eq");
        } else if (actualTy == ptrTy_ && expectedTy == ptrTy_) {
            llvm::FunctionType *strcmpTy = llvm::FunctionType::get(i32Ty_, {ptrTy_, ptrTy_}, false);
            llvm::FunctionCallee strcmpFn = mod_->getOrInsertFunction("strcmp", strcmpTy);
            llvm::Value *result = builder_.CreateCall(strcmpFn, {actualVal, expectedVal}, "strcmp");
            eqResult = builder_.CreateICmpEQ(result, llvm::ConstantInt::get(i32Ty_, 0), "eq");
        } else if ((actualTy == i64Ty_ && expectedTy == f64Ty_) ||
                   (actualTy == f64Ty_ && expectedTy == i64Ty_)) {
            auto [lf, rf] = promoteToFloat(actualVal, expectedVal);
            eqResult = builder_.CreateFCmpOEQ(lf, rf, "eq");
        } else {
            codegenError("line " + std::to_string(s.loc.line) +
                                     ": " + s.matcher + ": unsupported types for comparison");
        }
        cmpResult = (s.matcher == "to_not_eq")
            ? builder_.CreateNot(eqResult, "not_eq")
            : eqResult;
    } else if (s.matcher == "to_be_true") {
        if (actualTy != i1Ty_)
            codegenError("line " + std::to_string(s.loc.line) +
                                     ": to_be_true: expected bool");
        cmpResult = actualVal;
    } else if (s.matcher == "to_be_false") {
        if (actualTy != i1Ty_)
            codegenError("line " + std::to_string(s.loc.line) +
                                     ": to_be_false: expected bool");
        cmpResult = builder_.CreateNot(actualVal, "not");
    } else if (s.matcher == "to_be_none") {
        if (!isOptionType(actualTy))
            codegenError("line " + std::to_string(s.loc.line) +
                                     ": to_be_none: expected Option type");
        llvm::Value *hasVal = builder_.CreateExtractValue(actualVal, {0}, "has_val");
        cmpResult = builder_.CreateNot(hasVal, "is_none");
    } else if (s.matcher == "to_be_some") {
        if (!isOptionType(actualTy))
            codegenError("line " + std::to_string(s.loc.line) +
                                     ": to_be_some: expected Option type");
        cmpResult = builder_.CreateExtractValue(actualVal, {0}, "is_some");
    } else if (s.matcher == "to_contain") {
        llvm::Value *expectedVal = emitExpr(*s.expected);

        if (actualTy == ptrTy_ && expectedVal->getType() == ptrTy_) {
            // String contains: use strstr
            llvm::FunctionType *strstrTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_}, false);
            llvm::FunctionCallee strstrFn = mod_->getOrInsertFunction("strstr", strstrTy);
            llvm::Value *result = builder_.CreateCall(strstrFn, {actualVal, expectedVal}, "strstr");
            cmpResult = builder_.CreateICmpNE(result, llvm::ConstantPointerNull::get(
                llvm::PointerType::getUnqual(*ctx_)), "contains");
        } else if (actualTy == ptrTy_) {
            // List/Set contains: use "in" operator logic
            llvm::Type *elemTy = getListElementType(actualVal);
            llvm::StructType *headerTy = listHeaderTy_;
            if (!elemTy) {
                elemTy = getSetElementType(actualVal);
                headerTy = setHeaderTy_;
            }
            if (!elemTy)
                codegenError("line " + std::to_string(s.loc.line) +
                                         ": to_contain: expected list, set, or string");

            llvm::Value *lenPtr = builder_.CreateStructGEP(headerTy, actualVal, 0, "len_ptr");
            llvm::Value *len = builder_.CreateLoad(i64Ty_, lenPtr, "len");
            llvm::Value *dataField = builder_.CreateStructGEP(headerTy, actualVal, 2, "data_field");
            llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataField, "data_ptr");

            llvm::AllocaInst *foundVar = builder_.CreateAlloca(i1Ty_, nullptr, "found");
            builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 0), foundVar);
            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

            llvm::Function *currentFnContain = builder_.GetInsertBlock()->getParent();
            llvm::BasicBlock *cBB = llvm::BasicBlock::Create(*ctx_, "contain.cond", currentFnContain);
            llvm::BasicBlock *bBB = llvm::BasicBlock::Create(*ctx_, "contain.body", currentFnContain);
            llvm::BasicBlock *nBB = llvm::BasicBlock::Create(*ctx_, "contain.next", currentFnContain);
            llvm::BasicBlock *eBB = llvm::BasicBlock::Create(*ctx_, "contain.end", currentFnContain);

            builder_.CreateBr(cBB);
            builder_.SetInsertPoint(cBB);
            llvm::Value *ci = builder_.CreateLoad(i64Ty_, iVar, "ci");
            builder_.CreateCondBr(builder_.CreateICmpSLT(ci, len, "clt"), bBB, eBB);

            builder_.SetInsertPoint(bBB);
            llvm::Value *curI = builder_.CreateLoad(i64Ty_, iVar, "cur_i");
            llvm::Value *ePtr = builder_.CreateGEP(elemTy, dataPtr, {curI}, "elem_ptr");
            llvm::Value *elem = builder_.CreateLoad(elemTy, ePtr, "elem");
            if (expectedVal->getType() != elemTy)
                codegenError("line " + std::to_string(s.loc.line) +
                                         ": to_contain: element type mismatch");
            llvm::Value *eq;
            if (elemTy == i64Ty_)
                eq = builder_.CreateICmpEQ(elem, expectedVal, "eq");
            else if (elemTy == ptrTy_) {
                llvm::FunctionType *strcmpTy = llvm::FunctionType::get(i32Ty_, {ptrTy_, ptrTy_}, false);
                llvm::FunctionCallee strcmpFn = mod_->getOrInsertFunction("strcmp", strcmpTy);
                llvm::Value *cmp = builder_.CreateCall(strcmpFn, {elem, expectedVal}, "strcmp");
                eq = builder_.CreateICmpEQ(cmp, llvm::ConstantInt::get(i32Ty_, 0), "eq");
            } else
                eq = builder_.CreateICmpEQ(elem, expectedVal, "eq");

            llvm::BasicBlock *foundBB = llvm::BasicBlock::Create(*ctx_, "contain.found", currentFnContain);
            builder_.CreateCondBr(eq, foundBB, nBB);
            builder_.SetInsertPoint(foundBB);
            builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 1), foundVar);
            builder_.CreateBr(eBB);

            builder_.SetInsertPoint(nBB);
            llvm::Value *nextI = builder_.CreateAdd(
                builder_.CreateLoad(i64Ty_, iVar, "ni"), llvm::ConstantInt::get(i64Ty_, 1), "next_i");
            builder_.CreateStore(nextI, iVar);
            builder_.CreateBr(cBB);

            builder_.SetInsertPoint(eBB);
            cmpResult = builder_.CreateLoad(i1Ty_, foundVar, "contain_result");
        } else {
            codegenError("line " + std::to_string(s.loc.line) +
                                     ": to_contain: expected list, set, or string");
        }
    }

    // Branch: if cmpResult is false, call __ry_test_expect_fail
    llvm::Function *currentFn = builder_.GetInsertBlock()->getParent();
    llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, "expect.fail", currentFn);
    llvm::BasicBlock *contBB = llvm::BasicBlock::Create(*ctx_, "expect.cont", currentFn);

    builder_.CreateCondBr(cmpResult, contBB, failBB);

    // Fail block: call __ry_test_expect_fail(line, actual_str, expected_str)
    builder_.SetInsertPoint(failBB);

    llvm::FunctionType *failFnTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {i32Ty_, ptrTy_, ptrTy_}, false);
    llvm::FunctionCallee failFn = mod_->getOrInsertFunction("__ry_test_expect_fail", failFnTy);

    // For now, format actual and expected as string representations
    // Use snprintf to format values at runtime
    llvm::FunctionType *snprintfTy = llvm::FunctionType::get(i32Ty_, {ptrTy_, i64Ty_, ptrTy_}, true);
    llvm::FunctionCallee snprintfFn = mod_->getOrInsertFunction("snprintf", snprintfTy);

    auto formatValue = [&](llvm::Value *val, llvm::Type *ty, const std::string &bufName) -> llvm::Value* {
        llvm::Value *buf = builder_.CreateAlloca(
            llvm::ArrayType::get(llvm::Type::getInt8Ty(*ctx_), 64), nullptr, bufName);
        llvm::Value *bufSize = llvm::ConstantInt::get(i64Ty_, 64);

        if (ty == i64Ty_) {
            llvm::Value *fmt = builder_.CreateGlobalString("%ld", ".fmt_i");
            builder_.CreateCall(snprintfFn, {buf, bufSize, fmt, val});
        } else if (ty == f64Ty_) {
            llvm::Value *fmt = builder_.CreateGlobalString("%g", ".fmt_f");
            builder_.CreateCall(snprintfFn, {buf, bufSize, fmt, val});
        } else if (ty == i1Ty_) {
            llvm::Value *trueStr = builder_.CreateGlobalString("true", ".true");
            llvm::Value *falseStr = builder_.CreateGlobalString("false", ".false");
            return builder_.CreateSelect(val, trueStr, falseStr, "bool_str");
        } else if (ty == ptrTy_) {
            // Assume string pointer, return directly
            return val;
        } else {
            llvm::Value *fmt = builder_.CreateGlobalString("<value>", ".fmt_val");
            builder_.CreateCall(snprintfFn, {buf, bufSize, fmt});
        }
        return buf;
    };

    llvm::Value *actualStr = formatValue(actualVal, actualTy, "actual_buf");

    llvm::Value *expectedStr;
    if (s.matcher == "to_eq" || s.matcher == "to_not_eq") {
        llvm::Value *expectedVal = emitExpr(*s.expected);
        expectedStr = formatValue(expectedVal, expectedVal->getType(), "expected_buf");
    } else if (s.matcher == "to_be_true") {
        expectedStr = builder_.CreateGlobalString("true", ".exp_true");
    } else if (s.matcher == "to_be_false") {
        expectedStr = builder_.CreateGlobalString("false", ".exp_false");
    } else if (s.matcher == "to_be_some") {
        expectedStr = builder_.CreateGlobalString("Some(...)", ".exp_some");
    } else if (s.matcher == "to_contain") {
        llvm::Value *expectedVal = emitExpr(*s.expected);
        expectedStr = formatValue(expectedVal, expectedVal->getType(), "expected_buf");
    } else {
        expectedStr = builder_.CreateGlobalString("None", ".exp_none");
    }

    builder_.CreateCall(failFn, {llvm::ConstantInt::get(i32Ty_, s.loc.line), actualStr, expectedStr});
    builder_.CreateBr(contBB);

    // Continue block
    builder_.SetInsertPoint(contBB);
}

// ===== MatchStmt =====

void CodeGen::emitStmt(std::unique_ptr<MatchStmt> &s) {
    llvm::Value *subject = emitExpr(*s->subject);
    llvm::Type *subjectTy = subject->getType();

    // --- Exhaustiveness check ---
    bool hasWildcardOrVar = false;
    bool hasGuardedArm = false;
    auto checkWildcardOrVar = [](const Pattern &p) {
        return std::holds_alternative<WildcardPattern>(p) ||
               std::holds_alternative<VariablePattern>(p);
    };
    for (auto &arm : s->arms) {
        if (!arm.guard) {
            if (checkWildcardOrVar(arm.pattern)) {
                hasWildcardOrVar = true;
            } else if (auto *op = std::get_if<std::unique_ptr<OrPattern>>(&arm.pattern)) {
                for (auto &alt : (*op)->alternatives) {
                    if (checkWildcardOrVar(alt)) {
                        hasWildcardOrVar = true;
                        break;
                    }
                }
            }
        }
        if (arm.guard)
            hasGuardedArm = true;
    }

    if (!hasWildcardOrVar) {
        // Check enum exhaustiveness
        std::string enumName;
        for (auto &arm : s->arms) {
            if (auto *ep = std::get_if<EnumPattern>(&arm.pattern)) {
                enumName = ep->enum_name;
                break;
            }
            if (auto *ecp = std::get_if<EnumConstructorPattern>(&arm.pattern)) {
                enumName = ecp->enum_name;
                break;
            }
            if (auto *op = std::get_if<std::unique_ptr<OrPattern>>(&arm.pattern)) {
                for (auto &alt : (*op)->alternatives) {
                    if (auto *ep = std::get_if<EnumPattern>(&alt)) {
                        enumName = ep->enum_name;
                        break;
                    }
                    if (auto *ecp = std::get_if<EnumConstructorPattern>(&alt)) {
                        enumName = ecp->enum_name;
                        break;
                    }
                }
                if (!enumName.empty()) break;
            }
        }
        if (!enumName.empty()) {
            // Resolve enumName for generic enums using subject type
            if (!enum_types_.count(enumName)) {
                std::string subEnumType;
                auto evIt2 = enum_value_types_.find(subject);
                if (evIt2 != enum_value_types_.end()) {
                    subEnumType = evIt2->second;
                } else if (auto *load = llvm::dyn_cast<llvm::LoadInst>(subject)) {
                    auto evIt3 = enum_value_types_.find(load->getPointerOperand());
                    if (evIt3 != enum_value_types_.end())
                        subEnumType = evIt3->second;
                }
                if (!subEnumType.empty()) {
                    auto ltPos = subEnumType.find('<');
                    if (ltPos != std::string::npos && subEnumType.substr(0, ltPos) == enumName)
                        enumName = subEnumType;
                }
            }
            auto it = enum_types_.find(enumName);
            if (it != enum_types_.end()) {
                std::unordered_set<std::string> covered;
                for (auto &arm : s->arms) {
                    if (auto *ep = std::get_if<EnumPattern>(&arm.pattern)) {
                        if (!arm.guard)
                            covered.insert(ep->variant_name);
                    }
                    if (auto *ecp = std::get_if<EnumConstructorPattern>(&arm.pattern)) {
                        if (!arm.guard)
                            covered.insert(ecp->variant_name);
                    }
                    if (auto *op = std::get_if<std::unique_ptr<OrPattern>>(&arm.pattern)) {
                        if (!arm.guard) {
                            for (auto &alt : (*op)->alternatives) {
                                if (auto *ep = std::get_if<EnumPattern>(&alt))
                                    covered.insert(ep->variant_name);
                                if (auto *ecp = std::get_if<EnumConstructorPattern>(&alt))
                                    covered.insert(ecp->variant_name);
                            }
                        }
                    }
                }
                for (auto &[vname, _] : it->second.variants) {
                    if (!covered.count(vname))
                        codegenError("non-exhaustive match: missing variant '" +
                            enumName + "::" + vname + "'");
                }
            }
        }

        // Check Option exhaustiveness
        bool hasSome = false, hasNone = false;
        auto checkOptionPattern = [&](const Pattern &p) {
            if (std::holds_alternative<SomePattern>(p)) hasSome = true;
            if (std::holds_alternative<NonePattern>(p)) hasNone = true;
        };
        for (auto &arm : s->arms) {
            if (!arm.guard) {
                checkOptionPattern(arm.pattern);
                if (auto *op = std::get_if<std::unique_ptr<OrPattern>>(&arm.pattern)) {
                    for (auto &alt : (*op)->alternatives)
                        checkOptionPattern(alt);
                }
            }
        }
        if ((hasSome && !hasNone) || (!hasSome && hasNone))
            codegenError("non-exhaustive match: Option requires both Some and None cases (or use '_')");

        // Check bool exhaustiveness
        bool hasTrue = false, hasFalse = false;
        auto checkBoolPattern = [&](const Pattern &p) {
            if (auto *lp = std::get_if<LiteralPattern>(&p)) {
                if (auto *be = std::get_if<BoolExpr>(&lp->value->data)) {
                    if (be->value) hasTrue = true;
                    if (!be->value) hasFalse = true;
                }
            }
        };
        for (auto &arm : s->arms) {
            if (!arm.guard) {
                checkBoolPattern(arm.pattern);
                if (auto *op = std::get_if<std::unique_ptr<OrPattern>>(&arm.pattern)) {
                    for (auto &alt : (*op)->alternatives)
                        checkBoolPattern(alt);
                }
            }
        }
        if (subjectTy == i1Ty_ && !(hasTrue && hasFalse) && !hasWildcardOrVar)
            codegenError("non-exhaustive match: bool requires both true and false cases (or use '_')");

        // For int/float/string literals without wildcard
        if (enumName.empty() && !hasSome && !hasNone && !hasTrue && !hasFalse)
            codegenError("non-exhaustive match: literal patterns require a wildcard '_' case");
    }

    // --- Code generation: chain of conditional branches ---
    llvm::BasicBlock *matchEndBB = llvm::BasicBlock::Create(*ctx_, "match.end", fn_);

    // Store subject for potential repeated use
    llvm::AllocaInst *subjectAlloca = builder_.CreateAlloca(subjectTy, nullptr, "match.subject");
    builder_.CreateStore(subject, subjectAlloca);

    // Track enum type for subject
    std::string subjectEnumType;
    {
        auto evIt = enum_value_types_.find(subject);
        if (evIt != enum_value_types_.end()) {
            subjectEnumType = evIt->second;
        } else if (auto *load = llvm::dyn_cast<llvm::LoadInst>(subject)) {
            evIt = enum_value_types_.find(load->getPointerOperand());
            if (evIt != enum_value_types_.end())
                subjectEnumType = evIt->second;
        }
        if (!subjectEnumType.empty())
            enum_value_types_[subjectAlloca] = subjectEnumType;
    }

    for (size_t i = 0; i < s->arms.size(); ++i) {
        auto &arm = s->arms[i];
        llvm::BasicBlock *armBodyBB = llvm::BasicBlock::Create(*ctx_, "match.arm.body", fn_);
        llvm::BasicBlock *nextArmBB = (i + 1 < s->arms.size())
            ? llvm::BasicBlock::Create(*ctx_, "match.arm.test", fn_)
            : matchEndBB;

        llvm::Value *subjectVal = builder_.CreateLoad(subjectTy, subjectAlloca, "match.subj");

        // Generate pattern test
        llvm::Value *testResult = nullptr;
        std::visit([&](auto &pat) {
            using T = std::decay_t<decltype(pat)>;
            if constexpr (std::is_same_v<T, WildcardPattern>) {
                testResult = llvm::ConstantInt::get(i1Ty_, 1);
            } else if constexpr (std::is_same_v<T, LiteralPattern>) {
                llvm::Value *litVal = emitExpr(*pat.value);
                if (subjectTy == i64Ty_ && litVal->getType() == i64Ty_) {
                    testResult = builder_.CreateICmpEQ(subjectVal, litVal, "match.eq");
                } else if (subjectTy == f64Ty_ && litVal->getType() == f64Ty_) {
                    testResult = builder_.CreateFCmpOEQ(subjectVal, litVal, "match.feq");
                } else if (subjectTy == i1Ty_ && litVal->getType() == i1Ty_) {
                    testResult = builder_.CreateICmpEQ(subjectVal, litVal, "match.beq");
                } else if (subjectTy == ptrTy_ && litVal->getType() == ptrTy_) {
                    auto strcmpTy = llvm::FunctionType::get(i32Ty_, {ptrTy_, ptrTy_}, false);
                    auto strcmpFn = mod_->getOrInsertFunction("strcmp", strcmpTy);
                    llvm::Value *cmp = builder_.CreateCall(strcmpFn, {subjectVal, litVal}, "strcmp");
                    testResult = builder_.CreateICmpEQ(cmp, llvm::ConstantInt::get(i32Ty_, 0), "match.streq");
                } else {
                    codegenError("match: incompatible types in literal pattern");
                }
            } else if constexpr (std::is_same_v<T, VariablePattern>) {
                testResult = llvm::ConstantInt::get(i1Ty_, 1);
            } else if constexpr (std::is_same_v<T, EnumPattern>) {
                std::string resolvedEnum = pat.enum_name;
                auto enumIt = enum_types_.find(resolvedEnum);
                if (enumIt == enum_types_.end() && !subjectEnumType.empty()) {
                    // Fallback: try subject's enum type (for generic enums)
                    auto ltPos = subjectEnumType.find('<');
                    if (ltPos != std::string::npos && subjectEnumType.substr(0, ltPos) == pat.enum_name) {
                        resolvedEnum = subjectEnumType;
                        enumIt = enum_types_.find(resolvedEnum);
                    }
                }
                if (enumIt == enum_types_.end())
                    codegenError("match: unknown enum '" + pat.enum_name + "'");
                auto varIt = enumIt->second.variants.find(pat.variant_name);
                if (varIt == enumIt->second.variants.end())
                    codegenError("match: unknown variant '" + pat.enum_name + "::" + pat.variant_name + "'");
                if (enumIt->second.isADT) {
                    llvm::Value *subjectTag = builder_.CreateExtractValue(subjectVal, 0, "adt.tag");
                    testResult = builder_.CreateICmpEQ(subjectTag, llvm::ConstantInt::get(i64Ty_, varIt->second), "match.adt_eq");
                } else {
                    llvm::Value *tag = llvm::ConstantInt::get(i64Ty_, varIt->second);
                    testResult = builder_.CreateICmpEQ(subjectVal, tag, "match.enum_eq");
                }
            } else if constexpr (std::is_same_v<T, EnumConstructorPattern>) {
                std::string resolvedEnum = pat.enum_name;
                auto enumIt = enum_types_.find(resolvedEnum);
                if (enumIt == enum_types_.end() && !subjectEnumType.empty()) {
                    auto ltPos = subjectEnumType.find('<');
                    if (ltPos != std::string::npos && subjectEnumType.substr(0, ltPos) == pat.enum_name) {
                        resolvedEnum = subjectEnumType;
                        enumIt = enum_types_.find(resolvedEnum);
                    }
                }
                if (enumIt == enum_types_.end())
                    codegenError("match: unknown enum '" + pat.enum_name + "'");
                if (!enumIt->second.isADT)
                    codegenError("match: constructor pattern requires ADT enum, but '" + pat.enum_name + "' is not ADT");
                auto varIt = enumIt->second.variants.find(pat.variant_name);
                if (varIt == enumIt->second.variants.end())
                    codegenError("match: unknown variant '" + pat.enum_name + "::" + pat.variant_name + "'");
                llvm::Value *subjectTag = builder_.CreateExtractValue(subjectVal, 0, "adt.tag");
                testResult = builder_.CreateICmpEQ(subjectTag, llvm::ConstantInt::get(i64Ty_, varIt->second), "match.adt_eq");
            } else if constexpr (std::is_same_v<T, SomePattern>) {
                if (!isOptionType(subjectTy))
                    codegenError("match: Some pattern requires Option type");
                llvm::Value *hasValue = builder_.CreateExtractValue(subjectVal, 0, "has_value");
                testResult = hasValue;
            } else if constexpr (std::is_same_v<T, NonePattern>) {
                if (!isOptionType(subjectTy))
                    codegenError("match: None pattern requires Option type");
                llvm::Value *hasValue = builder_.CreateExtractValue(subjectVal, 0, "has_value");
                testResult = builder_.CreateNot(hasValue, "is_none");
            } else if constexpr (std::is_same_v<T, std::unique_ptr<OrPattern>>) {
                // OR pattern: test each alternative and combine with OR
                testResult = llvm::ConstantInt::get(i1Ty_, 0);
                for (auto &alt : pat->alternatives) {
                    llvm::Value *altResult = nullptr;
                    std::visit([&](auto &altPat) {
                        using U = std::decay_t<decltype(altPat)>;
                        if constexpr (std::is_same_v<U, LiteralPattern>) {
                            llvm::Value *litVal = emitExpr(*altPat.value);
                            if (subjectTy == i64Ty_ && litVal->getType() == i64Ty_)
                                altResult = builder_.CreateICmpEQ(subjectVal, litVal, "or.eq");
                            else if (subjectTy == f64Ty_ && litVal->getType() == f64Ty_)
                                altResult = builder_.CreateFCmpOEQ(subjectVal, litVal, "or.feq");
                            else if (subjectTy == i1Ty_ && litVal->getType() == i1Ty_)
                                altResult = builder_.CreateICmpEQ(subjectVal, litVal, "or.beq");
                            else if (subjectTy == ptrTy_ && litVal->getType() == ptrTy_) {
                                auto strcmpTy = llvm::FunctionType::get(i32Ty_, {ptrTy_, ptrTy_}, false);
                                auto strcmpFn = mod_->getOrInsertFunction("strcmp", strcmpTy);
                                llvm::Value *cmp = builder_.CreateCall(strcmpFn, {subjectVal, litVal}, "strcmp");
                                altResult = builder_.CreateICmpEQ(cmp, llvm::ConstantInt::get(i32Ty_, 0), "or.streq");
                            } else {
                                codegenError("match: incompatible types in OR literal pattern");
                            }
                        } else if constexpr (std::is_same_v<U, EnumPattern>) {
                            auto enumIt = enum_types_.find(altPat.enum_name);
                            if (enumIt == enum_types_.end())
                                codegenError("match: unknown enum '" + altPat.enum_name + "'");
                            auto varIt = enumIt->second.variants.find(altPat.variant_name);
                            if (varIt == enumIt->second.variants.end())
                                codegenError("match: unknown variant '" + altPat.enum_name + "::" + altPat.variant_name + "'");
                            llvm::Value *tag = llvm::ConstantInt::get(i64Ty_, varIt->second);
                            altResult = builder_.CreateICmpEQ(subjectVal, tag, "or.enum_eq");
                        } else if constexpr (std::is_same_v<U, WildcardPattern>) {
                            altResult = llvm::ConstantInt::get(i1Ty_, 1);
                        } else if constexpr (std::is_same_v<U, NonePattern>) {
                            if (!isOptionType(subjectTy))
                                codegenError("match: None pattern requires Option type");
                            llvm::Value *hasValue = builder_.CreateExtractValue(subjectVal, 0, "has_value");
                            altResult = builder_.CreateNot(hasValue, "is_none");
                        } else {
                            codegenError("match: unsupported pattern type in OR pattern");
                        }
                    }, alt);
                    testResult = builder_.CreateOr(testResult, altResult, "or.comb");
                }
            }
        }, arm.pattern);

        // For guarded arms with variable/Some bindings, we need to create
        // bindings before evaluating the guard
        if (arm.guard) {
            // Create a pre-guard block for bindings
            llvm::BasicBlock *guardBB = llvm::BasicBlock::Create(*ctx_, "match.guard", fn_);
            builder_.CreateCondBr(testResult, guardBB, nextArmBB);
            builder_.SetInsertPoint(guardBB);

            pushScope();
            // Create bindings
            std::visit([&](auto &pat) {
                using T = std::decay_t<decltype(pat)>;
                if constexpr (std::is_same_v<T, VariablePattern>) {
                    llvm::Value *sv = builder_.CreateLoad(subjectTy, subjectAlloca, pat.name);
                    llvm::AllocaInst *varAlloca = getOrCreateVar(pat.name, subjectTy);
                    builder_.CreateStore(sv, varAlloca);
                    if (!subjectEnumType.empty())
                        enum_value_types_[varAlloca] = subjectEnumType;
                } else if constexpr (std::is_same_v<T, SomePattern>) {
                    llvm::Value *sv = builder_.CreateLoad(subjectTy, subjectAlloca, "opt_val");
                    llvm::Value *inner = builder_.CreateExtractValue(sv, 1, "some_val");
                    llvm::AllocaInst *varAlloca = getOrCreateVar(pat.binding, inner->getType());
                    builder_.CreateStore(inner, varAlloca);
                }
            }, arm.pattern);

            // Evaluate guard
            llvm::Value *guardVal = emitExpr(*arm.guard);
            guardVal = toBool(guardVal);
            popScope();

            builder_.CreateCondBr(guardVal, armBodyBB, nextArmBB);
        } else {
            builder_.CreateCondBr(testResult, armBodyBB, nextArmBB);
        }

        // Arm body
        builder_.SetInsertPoint(armBodyBB);
        pushScope();

        // Create pattern bindings in body scope
        std::visit([&](auto &pat) {
            using T = std::decay_t<decltype(pat)>;
            if constexpr (std::is_same_v<T, VariablePattern>) {
                llvm::Value *sv = builder_.CreateLoad(subjectTy, subjectAlloca, pat.name);
                llvm::AllocaInst *varAlloca = getOrCreateVar(pat.name, subjectTy);
                builder_.CreateStore(sv, varAlloca);
                if (!subjectEnumType.empty())
                    enum_value_types_[varAlloca] = subjectEnumType;
            } else if constexpr (std::is_same_v<T, SomePattern>) {
                llvm::Value *sv = builder_.CreateLoad(subjectTy, subjectAlloca, "opt_val");
                llvm::Value *inner = builder_.CreateExtractValue(sv, 1, "some_val");
                llvm::AllocaInst *varAlloca = getOrCreateVar(pat.binding, inner->getType());
                builder_.CreateStore(inner, varAlloca);
            } else if constexpr (std::is_same_v<T, EnumConstructorPattern>) {
                std::string resolvedEnum = pat.enum_name;
                if (!enum_types_.count(resolvedEnum) && !subjectEnumType.empty()) {
                    auto ltPos = subjectEnumType.find('<');
                    if (ltPos != std::string::npos && subjectEnumType.substr(0, ltPos) == pat.enum_name)
                        resolvedEnum = subjectEnumType;
                }
                auto enumIt = enum_types_.find(resolvedEnum);
                if (enumIt != enum_types_.end()) {
                    auto fit = enumIt->second.variantFields.find(pat.variant_name);
                    if (fit != enumIt->second.variantFields.end()) {
                        llvm::Value *sv = builder_.CreateLoad(subjectTy, subjectAlloca, "adt.val");
                        // Get payload pointer via alloca
                        llvm::AllocaInst *tmpAlloca = builder_.CreateAlloca(subjectTy, nullptr, "adt.tmp");
                        builder_.CreateStore(sv, tmpAlloca);
                        llvm::Value *payloadPtr = builder_.CreateStructGEP(
                            enumIt->second.adtType, tmpAlloca, 1, "adt.payload");
                        const llvm::DataLayout &dl = mod_->getDataLayout();
                        size_t offset = 0;
                        for (size_t bi = 0; bi < pat.bindings.size() && bi < fit->second.fieldTypes.size(); ++bi) {
                            llvm::Type *fieldTy = fit->second.fieldTypes[bi];
                            uint64_t align = dl.getABITypeAlign(fieldTy).value();
                            offset = (offset + align - 1) / align * align;
                            llvm::Value *fieldPtr = builder_.CreateGEP(
                                llvm::Type::getInt8Ty(*ctx_), payloadPtr,
                                {llvm::ConstantInt::get(i64Ty_, offset)},
                                "adt.bind." + std::to_string(bi));
                            llvm::Value *fieldVal = builder_.CreateLoad(fieldTy, fieldPtr, pat.bindings[bi]);
                            llvm::AllocaInst *bindAlloca = getOrCreateVar(pat.bindings[bi], fieldTy);
                            builder_.CreateStore(fieldVal, bindAlloca);
                            offset += dl.getTypeAllocSize(fieldTy);
                        }
                    }
                }
            }
        }, arm.pattern);

        for (auto &stmt : arm.body)
            std::visit([this](auto &st) { emitStmt(st); }, stmt);

        popScope();
        if (!builder_.GetInsertBlock()->getTerminator())
            builder_.CreateBr(matchEndBB);

        if (i + 1 < s->arms.size())
            builder_.SetInsertPoint(nextArmBB);
    }

    builder_.SetInsertPoint(matchEndBB);
}

// ===== Union type helpers =====

std::vector<std::string> CodeGen::parseUnionComponents(const std::string &typeName) {
    std::vector<std::string> components;
    size_t start = 0;
    while (start < typeName.size()) {
        size_t pos = typeName.find(" | ", start);
        if (pos == std::string::npos) {
            std::string comp = typeName.substr(start);
            size_t s = comp.find_first_not_of(' ');
            size_t e = comp.find_last_not_of(' ');
            if (s != std::string::npos)
                components.push_back(comp.substr(s, e - s + 1));
            break;
        }
        std::string comp = typeName.substr(start, pos - start);
        size_t s = comp.find_first_not_of(' ');
        size_t e = comp.find_last_not_of(' ');
        if (s != std::string::npos)
            components.push_back(comp.substr(s, e - s + 1));
        start = pos + 3;
    }
    return components;
}

std::string CodeGen::normalizeUnionType(const std::string &typeName) {
    auto components = parseUnionComponents(typeName);
    std::sort(components.begin(), components.end());
    std::string result;
    for (size_t i = 0; i < components.size(); ++i) {
        if (i > 0) result += " | ";
        result += components[i];
    }
    return result;
}

bool CodeGen::isUnionType(const std::string &typeName) {
    return typeName.find(" | ") != std::string::npos;
}

llvm::Value *CodeGen::wrapInUnion(llvm::Value *val, const std::string &unionTypeName) {
    std::string norm = normalizeUnionType(unionTypeName);
    auto infoIt = union_type_info_.find(norm);
    if (infoIt == union_type_info_.end()) {
        resolveType(norm);
        infoIt = union_type_info_.find(norm);
    }
    auto &info = infoIt->second;
    int tagIdx = -1;
    for (size_t i = 0; i < info.componentTypes.size(); ++i) {
        if (info.componentTypes[i] == val->getType()) { tagIdx = i; break; }
    }
    if (tagIdx < 0)
        codegenError("type is not in union " + norm);

    llvm::AllocaInst *tmp = builder_.CreateAlloca(info.llvmType, nullptr, "union.tmp");
    auto *tagPtr = builder_.CreateStructGEP(info.llvmType, tmp, 0, "union.tag");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, tagIdx), tagPtr);
    auto *dataPtr = builder_.CreateStructGEP(info.llvmType, tmp, 1, "union.data");
    builder_.CreateStore(val, dataPtr);
    return builder_.CreateLoad(info.llvmType, tmp, "union.val");
}

// ===== exit(code) =====

void CodeGen::emitExit(const std::vector<ExprPtr> &args) {
    if (args.size() != 1)
        codegenError("exit() takes exactly 1 argument");
    llvm::Value *code = emitExpr(*args[0]);
    if (!code->getType()->isIntegerTy())
        codegenError("exit() argument must be an integer");
    if (code->getType() != i32Ty_)
        code = builder_.CreateIntCast(code, i32Ty_, true, "exit_code");
    llvm::FunctionType *exitTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {i32Ty_}, false);
    llvm::FunctionCallee exitFn = mod_->getOrInsertFunction("exit", exitTy);
    builder_.CreateCall(exitFn, {code});
    builder_.CreateUnreachable();
}
