#include "ry/codegen.hpp"
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
    return lookupCollectionType(type_meta_[TM_ListElem], listAlloca);
}

llvm::Type *CodeGen::getMapKeyType(llvm::Value *mapVal) {
    return lookupCollectionType(type_meta_[TM_MapKey], mapVal);
}

llvm::Type *CodeGen::getMapValueType(llvm::Value *mapVal) {
    return lookupCollectionType(type_meta_[TM_MapValue], mapVal);
}

llvm::Type *CodeGen::getSetElementType(llvm::Value *setVal) {
    return lookupCollectionType(type_meta_[TM_SetElem], setVal);
}

llvm::Type *CodeGen::getNestedListElementType(llvm::Value *listVal) {
    return lookupCollectionType(type_meta_[TM_NestedListElem], listVal);
}

llvm::Type *CodeGen::getIteratorElementType(llvm::Value *iterVal) {
    return lookupCollectionType(type_meta_[TM_IteratorElem], iterVal);
}

// ===== TCP socket type tracking helpers =====

static bool lookupValueSet(const std::unordered_set<llvm::Value*> &set, llvm::Value *val) {
    if (set.count(val)) return true;
    if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
        if (load->getType()->isPointerTy())
            return set.count(load->getPointerOperand()) > 0;
    }
    return false;
}

// Relaxed variant: resolves LoadInst to alloca regardless of loaded type.
// Used for resource propagation where the value may be a Result<T, Error> struct.
static bool lookupValueSetWide(const std::unordered_set<llvm::Value*> &set, llvm::Value *val) {
    if (set.count(val)) return true;
    if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val))
        return set.count(load->getPointerOperand()) > 0;
    return false;
}

bool CodeGen::isTcpListener(llvm::Value *val) { return lookupValueSet(resource_sets_[RK_TcpListener], val); }
bool CodeGen::isTcpStream(llvm::Value *val) { return lookupValueSet(resource_sets_[RK_TcpStream], val); }
bool CodeGen::isTlsStream(llvm::Value *val) { return lookupValueSet(resource_sets_[RK_TlsStream], val); }
bool CodeGen::isHttpRequest(llvm::Value *val) { return lookupValueSet(resource_sets_[RK_HttpRequest], val); }
bool CodeGen::isHttpResponse(llvm::Value *val) { return lookupValueSet(resource_sets_[RK_HttpResponse], val); }
bool CodeGen::isHttpClientResponse(llvm::Value *val) { return lookupValueSet(resource_sets_[RK_HttpClientResponse], val); }
bool CodeGen::isThread(llvm::Value *val) { return lookupValueSet(resource_sets_[RK_Thread], val); }
bool CodeGen::isLock(llvm::Value *val) { return lookupValueSet(resource_sets_[RK_Lock], val); }
bool CodeGen::isRWLock(llvm::Value *val) { return lookupValueSet(resource_sets_[RK_RWLock], val); }
bool CodeGen::isSemaphore(llvm::Value *val) { return lookupValueSet(resource_sets_[RK_Semaphore], val); }
bool CodeGen::isBarrier(llvm::Value *val) { return lookupValueSet(resource_sets_[RK_Barrier], val); }
bool CodeGen::isAtomicInt(llvm::Value *val) { return lookupValueSet(resource_sets_[RK_AtomicInt], val); }
bool CodeGen::isAtomicBool(llvm::Value *val) { return lookupValueSet(resource_sets_[RK_AtomicBool], val); }
bool CodeGen::isRegex(llvm::Value *val) { return lookupValueSet(resource_sets_[RK_Regex], val); }

void CodeGen::propagateResourceTracking(llvm::Value *src, llvm::Value *dst) {
    for (int i = 0; i < RK_COUNT; ++i)
        if (resource_sets_[i].count(src)) resource_sets_[i].insert(dst);
}

void CodeGen::propagateResourceTrackingWide(llvm::Value *src, llvm::Value *dst) {
    for (int i = 0; i < RK_COUNT; ++i)
        if (lookupValueSetWide(resource_sets_[i], src)) resource_sets_[i].insert(dst);
}

void CodeGen::propagateCollectionMetadata(llvm::Value *src, llvm::Value *dst) {
    // Resolve through LoadInst to find metadata on the pointer operand
    llvm::Value *resolved = src;
    if (auto *load = llvm::dyn_cast<llvm::LoadInst>(src))
        resolved = load->getPointerOperand();
    auto tryPropagate = [&](auto &map) {
        auto it = map.find(src);
        if (it != map.end()) { map[dst] = it->second; return; }
        if (resolved != src) {
            it = map.find(resolved);
            if (it != map.end()) map[dst] = it->second;
        }
    };
    for (int i = 0; i < TM_COUNT; ++i)
        tryPropagate(type_meta_[i]);
    tryPropagate(fn_type_info_);
    tryPropagate(union_value_types_);
    tryPropagate(enum_value_types_);

    // Propagate ARC managed status
    auto *dstAlloca = llvm::dyn_cast<llvm::AllocaInst>(dst);
    if (dstAlloca) {
        auto *srcAlloca = llvm::dyn_cast<llvm::AllocaInst>(resolved);
        if (srcAlloca && isArcManaged(srcAlloca))
            markArcManaged(dstAlloca);
    }
}

void CodeGen::propagateTypeMeta(const std::string &typeName, llvm::Value *val) {
    if (typeName.size() > 5 && typeName.compare(0, 5, "Task<") == 0 && typeName.back() == '>') {
        std::string inner = typeName.substr(5, typeName.size() - 6);
        type_meta_[TM_TaskResult][val] = resolveType(inner);
    } else if (typeName.size() > 5 && typeName.compare(0, 5, "List<") == 0 && typeName.back() == '>') {
        std::string inner = typeName.substr(5, typeName.size() - 6);
        type_meta_[TM_ListElem][val] = resolveType(inner);
        if (inner.size() > 5 && inner.compare(0, 5, "List<") == 0 && inner.back() == '>') {
            std::string nested = inner.substr(5, inner.size() - 6);
            type_meta_[TM_NestedListElem][val] = resolveType(nested);
        }
    } else if (typeName.size() > 4 && typeName.compare(0, 4, "Map<") == 0 && typeName.back() == '>') {
        auto [keyTy, valTy] = parseMapTypeAnnotation(typeName);
        if (keyTy) type_meta_[TM_MapKey][val] = keyTy;
        if (valTy) type_meta_[TM_MapValue][val] = valTy;
    } else if (typeName.size() > 4 && typeName.compare(0, 4, "Set<") == 0 && typeName.back() == '>') {
        std::string inner = typeName.substr(4, typeName.size() - 5);
        type_meta_[TM_SetElem][val] = resolveType(inner);
    } else if (isLowLevelTypeName(typeName)) {
        low_level_type_names_[val] = typeName;
    }
}

void CodeGen::propagateReturnTypeMeta(const OverloadEntry *entry, llvm::Value *val) {
    if (!entry) return;
    propagateTypeMeta(entry->returnTypeName, val);
}

void CodeGen::propagateAllMetadata(llvm::Value *src, llvm::Value *dst) {
    propagateCollectionMetadata(src, dst);
    propagateResourceTracking(src, dst);
}

void CodeGen::propagateAllMetadataWide(llvm::Value *src, llvm::Value *dst) {
    propagateCollectionMetadata(src, dst);
    propagateResourceTrackingWide(src, dst);
}

// Step 1: Hash function resolution helper
CodeGen::HashFnInfo CodeGen::resolveHashFn(llvm::Type *keyTy) {
    if (keyTy == ptrTy_)
        return {"__ry_hash_str", "__ry_ht_rehash_str", ptrTy_};
    if (keyTy->isDoubleTy())
        return {"__ry_hash_f64", "__ry_ht_rehash_f64", f64Ty_};
    return {"__ry_hash_i64", "__ry_ht_rehash_i64", i64Ty_};
}

llvm::Value *CodeGen::coerceHashKey(llvm::Value *key, llvm::Type *keyTy,
                                     llvm::Type *hashArgTy, const llvm::Twine &prefix) {
    if (keyTy != hashArgTy && keyTy->isIntegerTy() && hashArgTy->isIntegerTy())
        return builder_.CreateZExt(key, hashArgTy, prefix + "_hash_zext");
    return key;
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
    return emitHashTableLookup(setPtr, setHeaderTy_, kSetLayout, elem, elemTy);
}

llvm::Value *CodeGen::emitMapKeyLookup(llvm::Value *mapPtr, llvm::Value *key, llvm::Type *keyTy) {
    return emitHashTableLookup(mapPtr, mapHeaderTy_, kMapLayout, key, keyTy);
}

// Helper: initialize bucket array fields in a header
void CodeGen::emitBucketInit(llvm::Value *headerPtr, llvm::StructType *headerTy,
                              unsigned bucketCountIdx, unsigned bucketsPtrIdx,
                              int64_t initialBucketCount) {
    auto mallocFn = getStdlibMalloc();
    auto memsetFn = getStdlibMemset();

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
    llvm::Value *hashKey = coerceHashKey(key, keyTy, hfi.hashArgTy, "hash_key");

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
    auto freeFn = getStdlibFree();
    llvm::Value *oldBuckets = builder_.CreateLoad(ptrTy_, bucketsField, "old_buckets");
    builder_.CreateCall(freeFn, {oldBuckets});

    // Store new buckets and count
    builder_.CreateStore(newBuckets, bucketsField);
    builder_.CreateStore(newBc, bcField);

    builder_.CreateBr(doneRehashBB);
    builder_.SetInsertPoint(doneRehashBB);
}

