#include "ry/codegen.hpp"
#include "ry/stdlib_registry.hpp"
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <stdexcept>


namespace ry {

// ===== Collection helpers =====

llvm::Type *CodeGen::getListElementType(llvm::Value *listAlloca) {
    return getTypeMeta(TypeMeta::ListElem, listAlloca);
}

llvm::Type *CodeGen::getMapKeyType(llvm::Value *mapVal) {
    return getTypeMeta(TypeMeta::MapKey, mapVal);
}

llvm::Type *CodeGen::getMapValueType(llvm::Value *mapVal) {
    return getTypeMeta(TypeMeta::MapValue, mapVal);
}

llvm::Type *CodeGen::getSetElementType(llvm::Value *setVal) {
    return getTypeMeta(TypeMeta::SetElem, setVal);
}

llvm::Type *CodeGen::getNestedListElementType(llvm::Value *listVal) {
    return getTypeMeta(TypeMeta::NestedListElem, listVal);
}

llvm::Type *CodeGen::getIteratorElementType(llvm::Value *iterVal) {
    return getTypeMeta(TypeMeta::IteratorElem, iterVal);
}

// ===== Resource type tracking helpers =====

bool CodeGen::isResourceKind(int rk, llvm::Value *val) {
    if (rk < 0) return false;
    return hasResourceKind(val, rk);
}

// Resource kind IDs are stable after static init; cache on first call.
#define RY_IS_RESOURCE(method, typeName)                                       \
    bool CodeGen::method(llvm::Value *val) {                                   \
        static const int rk =                                                  \
            ResourceKindRegistry::instance().lookupByTypeName(typeName);       \
        return rk != ResourceKindRegistry::NONE && isResourceKind(rk, val);    \
    }
RY_IS_RESOURCE(isTcpListener,        "TcpListener")
RY_IS_RESOURCE(isTcpStream,          "TcpStream")
RY_IS_RESOURCE(isTlsStream,          "TlsStream")
RY_IS_RESOURCE(isHttpRequest,        "HttpRequest")
RY_IS_RESOURCE(isHttpResponse,       "HttpResponse")
RY_IS_RESOURCE(isHttpClientResponse, "HttpClientResponse")
RY_IS_RESOURCE(isThread,             "Thread")
RY_IS_RESOURCE(isLock,               "Lock")
RY_IS_RESOURCE(isRWLock,             "RWLock")
RY_IS_RESOURCE(isSemaphore,          "Semaphore")
RY_IS_RESOURCE(isBarrier,            "Barrier")
RY_IS_RESOURCE(isAtomicInt,          "AtomicInt")
RY_IS_RESOURCE(isAtomicBool,         "AtomicBool")
RY_IS_RESOURCE(isRegex,              "Regex")
#undef RY_IS_RESOURCE


void CodeGen::propagateTypeMeta(const std::string &typeName, llvm::Value *val) {
    if (typeName.size() > 5 && typeName.compare(0, 5, "Task<") == 0 && typeName.back() == '>') {
        std::string inner = typeName.substr(5, typeName.size() - 6);
        setTypeMeta(TypeMeta::TaskResult, val, resolveType(inner));
    } else if (isListTypeName(typeName) && typeName.back() == '>') {
        std::string inner = typeName.substr(5, typeName.size() - 6);
        setTypeMeta(TypeMeta::ListElem, val, resolveType(inner));
        getOrCreateMeta(val).list_elem_type_name = inner;
        if (isFunctionTypeName(inner))
            getOrCreateMeta(val).list_elem_fn_type_info = parseFnTypeAnnotation(inner);
        if (isListTypeName(inner) && inner.back() == '>') {
            std::string nested = inner.substr(5, inner.size() - 6);
            setTypeMeta(TypeMeta::NestedListElem, val, resolveType(nested));
        }
    } else if (isMapTypeName(typeName) && typeName.back() == '>') {
        auto [keyTy, valTy] = parseMapTypeAnnotation(typeName);
        if (keyTy) setTypeMeta(TypeMeta::MapKey, val, keyTy);
        if (valTy) setTypeMeta(TypeMeta::MapValue, val, valTy);
        std::string vtn = extractMapValueTypeName(typeName);
        if (!vtn.empty()) {
            getOrCreateMeta(val).map_value_type_name = vtn;
            if (isFunctionTypeName(vtn))
                getOrCreateMeta(val).map_value_fn_type_info = parseFnTypeAnnotation(vtn);
        }
    } else if (isSetTypeName(typeName) && typeName.back() == '>') {
        std::string inner = typeName.substr(4, typeName.size() - 5);
        setTypeMeta(TypeMeta::SetElem, val, resolveType(inner));
        getOrCreateMeta(val).set_elem_type_name = inner;
        if (isFunctionTypeName(inner))
            getOrCreateMeta(val).set_elem_fn_type_info = parseFnTypeAnnotation(inner);
    } else if (isLowLevelTypeName(typeName)) {
        getOrCreateMeta(val).low_level_type_name = typeName;
    }
}

void CodeGen::propagateReturnTypeMeta(const OverloadEntry *entry, llvm::Value *val) {
    if (!entry) return;
    propagateTypeMeta(entry->returnTypeName, val);
}

void CodeGen::propagateReturnFnTypeMeta(const OverloadEntry *entry, llvm::Function *fn, llvm::Value *result) {
    auto retFnIt = return_fn_type_info_.find(fn);
    if (retFnIt != return_fn_type_info_.end()) {
        getOrCreateMeta(result).fn_type_info = retFnIt->second;
        return;
    }
    if (!entry) return;
    std::string resolved = resolveTypeAlias(entry->returnTypeName);
    if (resolved.size() <= 9 || resolved.compare(0, 9, "function(") != 0) return;
    getOrCreateMeta(result).fn_type_info = parseFnTypeAnnotation(resolved);
}

std::string CodeGen::extractMapValueTypeName(const std::string &mapTypeName) {
    std::string inner = mapTypeName.substr(4, mapTypeName.size() - 5);
    auto parts = splitTypeArgs(inner);
    if (parts.size() != 2) return "";
    std::string vStr = parts[1];
    while (!vStr.empty() && vStr.front() == ' ') vStr = vStr.substr(1);
    return vStr;
}

std::string CodeGen::inferCollectionTypeName(llvm::Value *val) {
    if (auto *keyTy = getMapKeyType(val)) {
        std::string keyName = reverseResolveTypeName(keyTy);
        auto *meta = getMeta(val);
        std::string valName = (meta && !meta->map_value_type_name.empty())
            ? meta->map_value_type_name : reverseResolveTypeName(getMapValueType(val));
        return "Map<" + keyName + ", " + valName + ">";
    }
    if (auto *elemTy = getListElementType(val))
        return "List<" + reverseResolveTypeName(elemTy) + ">";
    if (auto *setTy = getSetElementType(val))
        return "Set<" + reverseResolveTypeName(setTy) + ">";
    return "";
}

// Reconstruct a canonical source-level type name (e.g. "List<int>",
// "Map<str, bool>", "function(int) -> str") from a value's collection /
// function metadata.  Used by wrapInUnion() to disambiguate same-LLVM-type
// variants like `List<int> | List<str>` by comparing the reconstructed name
// against each component name.  Returns "" if the value has no collection /
// function metadata.
std::string CodeGen::buildTypeNameFromMeta(llvm::Value *val) {
    auto *meta = getMeta(val);
    if (!meta) return "";

    // Prefer the stored source-level type name (populated via
    // propagateTypeMeta / emitVarDecl from annotations or literals).  Fall
    // back to reverseResolveTypeName on the llvm::Type when unavailable — this
    // only happens for primitive inner types, which reverseResolveTypeName
    // handles correctly.
    if (meta->list_elem) {
        std::string elemName = !meta->list_elem_type_name.empty()
            ? meta->list_elem_type_name
            : reverseResolveTypeName(meta->list_elem);
        return "List<" + elemName + ">";
    }
    if (meta->map_key || meta->map_value) {
        std::string keyName = meta->map_key
            ? reverseResolveTypeName(meta->map_key) : "";
        std::string valName;
        if (!meta->map_value_type_name.empty())
            valName = meta->map_value_type_name;
        else if (meta->map_value)
            valName = reverseResolveTypeName(meta->map_value);
        if (keyName.empty() || valName.empty()) return "";
        return "Map<" + keyName + ", " + valName + ">";
    }
    if (meta->set_elem) {
        std::string elemName = !meta->set_elem_type_name.empty()
            ? meta->set_elem_type_name
            : reverseResolveTypeName(meta->set_elem);
        return "Set<" + elemName + ">";
    }
    if (meta->fn_type_info) {
        const auto &info = *meta->fn_type_info;
        std::string result = "function(";
        for (size_t i = 0; i < info.paramTypes.size(); ++i) {
            if (i > 0) result += ", ";
            result += reverseResolveTypeName(info.paramTypes[i]);
        }
        result += ") -> ";
        result += reverseResolveTypeName(info.returnType);
        return result;
    }
    return "";
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

} // namespace ry
