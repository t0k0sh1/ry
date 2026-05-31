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


bool CodeGen::ensureEnumInstantiated(const std::string &typeName) {
    if (enum_types_.count(typeName)) return true;
    auto lt = typeName.find('<');
    if (lt == std::string::npos || typeName.back() != '>') return false;
    std::string base = typeName.substr(0, lt);
    if (!generic_enum_templates_.count(base)) return false;
    std::string argsStr = typeName.substr(lt + 1, typeName.size() - lt - 2);
    instantiateGenericEnum(typeName, base, splitTypeArgs(argsStr));
    return enum_types_.count(typeName) > 0;
}

void CodeGen::propagateTypeMeta(const std::string &typeName, llvm::Value *val) {
    // Resolve type aliases once so surface names like `type ColorAlias = Color`
    // or `type MaybeInt = Option<int>` propagate to the same metadata slots as
    // their canonical spellings. resolveTypeAlias returns the input unchanged
    // when no alias matches. (PR #853 review)
    const std::string resolved = resolveTypeAlias(typeName);
    auto propagateResourceLikeMeta = [&](const std::string &resolvedType) {
        registerResourceByTypeName(resolvedType, val);
    };
    if (resolved.size() > 5 && resolved.compare(0, 5, "Task<") == 0 && resolved.back() == '>') {
        std::string inner = resolved.substr(5, resolved.size() - 6);
        setTypeMeta(TypeMeta::TaskResult, val, resolveType(inner));
    } else if (ry::util::isListTypeName(resolved) && resolved.back() == '>') {
        std::string inner = resolved.substr(5, resolved.size() - 6);
        setTypeMeta(TypeMeta::ListElem, val, resolveType(inner));
        getOrCreateMeta(val).list_elem_type_name = inner;
        if (ry::util::isFunctionTypeName(inner))
            getOrCreateMeta(val).list_elem_fn_type_info = parseFnTypeAnnotation(inner);
        if (ry::util::isListTypeName(inner) && inner.back() == '>') {
            std::string nested = inner.substr(5, inner.size() - 6);
            setTypeMeta(TypeMeta::NestedListElem, val, resolveType(nested));
        }
    } else if (ry::util::isMapTypeName(resolved) && resolved.back() == '>') {
        auto [keyTy, valTy] = parseMapTypeAnnotation(resolved);
        if (keyTy) setTypeMeta(TypeMeta::MapKey, val, keyTy);
        if (valTy) setTypeMeta(TypeMeta::MapValue, val, valTy);
        // Split the inner "K, V" form exactly once and record both names
        // (key name is used by map-iteration destructure in #813).
        auto parts = splitTypeArgs(resolved.substr(4, resolved.size() - 5));
        if (parts.size() == 2) {
            std::string ktn = ry::util::trimTypeNameSpaces(parts[0]);
            std::string vtn = ry::util::trimTypeNameSpaces(parts[1]);
            if (!ktn.empty()) {
                getOrCreateMeta(val).map_key_type_name = ktn;
                if (ry::util::isFunctionTypeName(ktn))
                    getOrCreateMeta(val).map_key_fn_type_info = parseFnTypeAnnotation(ktn);
            }
            if (!vtn.empty()) {
                getOrCreateMeta(val).map_value_type_name = vtn;
                if (ry::util::isFunctionTypeName(vtn))
                    getOrCreateMeta(val).map_value_fn_type_info = parseFnTypeAnnotation(vtn);
            }
        }
    } else if (ry::util::isSetTypeName(resolved) && resolved.back() == '>') {
        std::string inner = resolved.substr(4, resolved.size() - 5);
        setTypeMeta(TypeMeta::SetElem, val, resolveType(inner));
        getOrCreateMeta(val).set_elem_type_name = inner;
        if (ry::util::isFunctionTypeName(inner))
            getOrCreateMeta(val).set_elem_fn_type_info = parseFnTypeAnnotation(inner);
    } else if (resolved.size() > 7 && resolved.compare(0, 7, "Result<") == 0 && resolved.back() == '>') {
        // Stamp the lossless full type name so pattern bindings can recover
        // the precise Ok/Err inner types for ARC retain dispatch and
        // metadata reconstruction (#1638). This must be set before the
        // recursive Ok/Err propagateTypeMeta calls below, since those
        // calls reset the value's collection metadata for the active
        // payload but leave source_type_name intact.
        getOrCreateMeta(val).source_type_name = resolved;
        // Propagate the active payload's collection metadata onto this value so
        // buildTypeNameFromMeta() works for Result<Collection,E> call results
        // even when no explicit type annotation is present (#985).
        // Strategy: try the Ok type first; if it's not a collection (sets no metadata),
        // fall back to the Err type.  When both are collections we keep
        // the Ok type — Result has only one metadata slot per kind, so
        // resolving this would require per-variant slots on ValueMetadata.
        std::string params = resolved.substr(7, resolved.size() - 8);
        int depth = 0;
        size_t commaIdx = std::string::npos;
        for (size_t i = 0; i < params.size(); ++i) {
            if (params[i] == '<') ++depth;
            else if (params[i] == '>') --depth;
            else if (params[i] == ',' && depth == 0) { commaIdx = i; break; }
        }
        if (commaIdx != std::string::npos) {
            std::string okType  = params.substr(0, commaIdx);
            // Snapshot: getMeta before trying ok type so we can detect if metadata was added.
            bool hadMeta = (getMeta(val) != nullptr &&
                            (getMeta(val)->list_elem || getMeta(val)->map_key ||
                             getMeta(val)->set_elem));
            propagateTypeMeta(okType, val);
            propagateResourceLikeMeta(resolveTypeAlias(ry::util::trimTypeNameSpaces(okType)));
            bool hasMeta = (getMeta(val) != nullptr &&
                            (getMeta(val)->list_elem || getMeta(val)->map_key ||
                             getMeta(val)->set_elem));
            // If the Ok type did not contribute collection metadata, try the Err type.
            if (!hadMeta && !hasMeta && commaIdx + 1 < params.size()) {
                std::string errType = params.substr(commaIdx + 1);
                // Trim leading whitespace
                size_t start = errType.find_first_not_of(' ');
                if (start != std::string::npos) errType = errType.substr(start);
                propagateTypeMeta(errType, val);
                propagateResourceLikeMeta(resolveTypeAlias(ry::util::trimTypeNameSpaces(errType)));
            }
        }
    } else if (resolved.size() > 7 && resolved.compare(0, 7, "Option<") == 0 && resolved.back() == '>') {
        // Stamp the lossless full type name (#1638) — see Result branch above.
        getOrCreateMeta(val).source_type_name = resolved;
        // Same treatment for Option<Collection> (#985 — mirrors Result handling above).
        std::string inner = resolved.substr(7, resolved.size() - 8);
        propagateTypeMeta(inner, val);
        propagateResourceLikeMeta(resolveTypeAlias(ry::util::trimTypeNameSpaces(inner)));
    } else if (resolved.size() > 1 && resolved.back() == '?') {
        // T? suffix is OptionalType::toString() shorthand for Option<T> (#1003).
        // Normalize to Option<inner> form before stamping source_type_name so
        // downstream consumers see one canonical spelling regardless of which
        // syntactic shape the user wrote (#1638).
        std::string inner = resolved.substr(0, resolved.size() - 1);
        getOrCreateMeta(val).source_type_name = "Option<" + inner + ">";
        propagateTypeMeta(inner, val);
        propagateResourceLikeMeta(resolveTypeAlias(ry::util::trimTypeNameSpaces(inner)));
    } else if (ry::util::isLowLevelTypeName(resolved)) {
        getOrCreateMeta(val).low_level_type_name = resolved;
    } else if (ResourceKindRegistry::instance().lookupByTypeName(resolved) !=
               ResourceKindRegistry::NONE) {
        propagateResourceLikeMeta(resolved);
    } else if (ensureEnumInstantiated(resolved)) {
        // Concrete enum or generic enum instantiation: tag the value so
        // valueToString() dispatches on enum_value_type metadata (#820).
        getOrCreateMeta(val).enum_value_type = resolved;
    }
    registerResourceByTypeName(resolved, val);
}

void CodeGen::propagateReturnTypeMeta(const OverloadEntry *entry, llvm::Value *val) {
    if (!entry) return;
    propagateTypeMeta(entry->returnTypeName, val);
    auto taskIt = return_task_result_types_.find(entry->func);
    if (taskIt != return_task_result_types_.end() && taskIt->second)
        setTypeMeta(TypeMeta::TaskResult, val, taskIt->second);
    auto threadIt = return_thread_result_types_.find(entry->func);
    if (threadIt != return_thread_result_types_.end() && threadIt->second)
        setTypeMeta(TypeMeta::ThreadResult, val, threadIt->second);
}

void CodeGen::propagateReturnFnTypeMeta(const OverloadEntry *entry, llvm::Function *fn, llvm::Value *result) {
    auto retFnIt = return_fn_type_info_.find(fn);
    if (retFnIt != return_fn_type_info_.end()) {
        getOrCreateMeta(result).fn_type_info = retFnIt->second;
        return;
    }
    if (!entry) return;
    std::string resolved = resolveTypeAlias(entry->returnTypeName);
    if (!ry::util::isFunctionTypeName(resolved)) return;
    getOrCreateMeta(result).fn_type_info = parseFnTypeAnnotation(resolved);
}

std::string CodeGen::extractMapKeyTypeName(const std::string &mapTypeName) {
    std::string inner = mapTypeName.substr(4, mapTypeName.size() - 5);
    auto parts = splitTypeArgs(inner);
    if (parts.size() != 2) return "";
    return ry::util::trimTypeNameSpaces(parts[0]);
}

std::string CodeGen::extractMapValueTypeName(const std::string &mapTypeName) {
    std::string inner = mapTypeName.substr(4, mapTypeName.size() - 5);
    auto parts = splitTypeArgs(inner);
    if (parts.size() != 2) return "";
    return ry::util::trimTypeNameSpaces(parts[1]);
}

std::string CodeGen::snapshotListElemName(llvm::Value *listVal, llvm::Type *elemTy) {
    // Prefer the list's recorded source-level element name; fall back to a
    // reverse-resolved LLVM type name so primitives still produce a usable
    // tuple component name. Shared by enumerate()/zip() so the tuple result
    // carries list_elem_type_name = "(int, <elem>)" / "(<a>, <b>)" (#813).
    std::string name;
    if (auto *sm = getMeta(listVal))
        name = sm->list_elem_type_name;
    if (name.empty())
        name = reverseResolveTypeName(elemTy);
    return name;
}

llvm::Value *CodeGen::emitStringToCharList(llvm::Value *s, const char *label) {
    // Runtime returns a List<str> of UTF-8 code points. See
    // src/runtime/core/utf8.cpp:__ry_split_chars. The result list is ARC-managed
    // exactly like any other List<str> (#746, #827).
    auto fn = getRuntimeFn("__ry_split_chars", ptrTy_, {ptrTy_, i64Ty_});
    llvm::Value *result = builder_.CreateCall(fn, {s, emitStringByteLen(s)}, label);
    setTypeMeta(TypeMeta::ListElem, result, ptrTy_);
    getOrCreateMeta(result).list_elem_type_name = "str";
    return result;
}

// Returns a source-level type name for a value that can be stored in a
// container literal's element-name slot. Despite the "Collection" in the
// name, enum type names are also returned (needed by #820 so
// `List<Color>` printing propagates enum metadata). Returns "" for plain
// primitives (int/bool/str/float) — callers should fall back to
// reverseResolveTypeName if they need a name for those.
std::string CodeGen::inferCollectionTypeName(llvm::Value *val) {
    if (auto *keyTy = getMapKeyType(val)) {
        auto *meta = getMeta(val);
        std::string keyName = (meta && !meta->map_key_type_name.empty())
            ? meta->map_key_type_name
            : reverseResolveTypeName(keyTy);
        std::string valName = (meta && !meta->map_value_type_name.empty())
            ? meta->map_value_type_name : reverseResolveTypeName(getMapValueType(val));
        return "Map<" + keyName + ", " + valName + ">";
    }
    if (auto *elemTy = getListElementType(val)) {
        // Prefer stored source-level element name (mirrors Map branch above).
        // reverseResolveTypeName(ptrTy_) loses nested-list info ("List<List<int>>"
        // collapses to "List<str>") — so check the stored name first (#1095).
        if (auto *meta = getMeta(val); meta && !meta->list_elem_type_name.empty())
            return "List<" + meta->list_elem_type_name + ">";
        // Unnamed StructType (tuple) has no canonical name from LLVM type alone.
        // Return "" so callers (emitVarDecl) can derive the correct name from the
        // type annotation (#1094).
        if (auto *st = llvm::dyn_cast<llvm::StructType>(elemTy);
                st && findRecordTypeName(st).empty())
            return "";
        return "List<" + reverseResolveTypeName(elemTy) + ">";
    }
    if (auto *setTy = getSetElementType(val))
        return "Set<" + reverseResolveTypeName(setTy) + ">";
    // Enum element types: needed so container literals whose first element is
    // an enum (e.g. [Color::Red]) propagate enum_value_type to printed
    // elements via list_elem_type_name → propagateTypeMeta (#820).
    if (auto *meta = getMeta(val); meta && !meta->enum_value_type.empty())
        return meta->enum_value_type;
    return "";
}

// Reconstruct a canonical source-level type name (e.g. "List<int>",
// "Map<str, bool>", "fn(int) -> str") from a value's collection /
// function metadata.  Used by wrapInUnion() to disambiguate same-LLVM-type
// variants like `List<int> | List<str>` by comparing the reconstructed name
// against each component name.  Returns "" if the value has no collection /
// function metadata.
std::string CodeGen::buildTypeNameFromMeta(llvm::Value *val) {
    auto *meta = getMeta(val);
    if (!meta) return "";

    if (!meta->resource_kinds.empty()) {
        if (const auto *info = ResourceKindRegistry::instance().getInfo(meta->resource_kinds[0]))
            return info->typeName;
    }
    if (!meta->enum_value_type.empty())
        return meta->enum_value_type;

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
        std::string keyName = !meta->map_key_type_name.empty()
            ? meta->map_key_type_name
            : (meta->map_key ? reverseResolveTypeName(meta->map_key) : "");
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
        std::string setName = "Set<";
        setName += elemName;
        setName += '>';
        return setName;
    }
    if (meta->fn_type_info) {
        const auto &info = *meta->fn_type_info;
        std::string result = "fn(";
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

llvm::Value *CodeGen::emitSetElementLookup(llvm::Value *setPtr, llvm::Value *elem,
                                            llvm::Type *elemTy, const std::string &elemName) {
    // StructType (records/tuples) and nested collections (List<T>, Map<K,V>, Set<T>
    // as elements) have no runtime hash function.  Use a structural O(n) linear scan.
    // For nested collection types, propagateTypeMeta rebuilds ValueMetadata on the
    // GEP-loaded candidate before emitComparisonOp dispatches — without it the
    // load is a metadata-less SSA value and dispatch falls through to strcmp
    // on raw collection-header bytes.
    const bool needsLinearScan =
        llvm::isa<llvm::StructType>(elemTy) ||
        (!elemName.empty() && elemName != "str" &&
         elemName != "int" && elemName != "float" && elemName != "bool");
    if (needsLinearScan) {
        const bool elemIsAny = isAnyType(elemTy);
        auto sf = loadSetHeader(setPtr, "slin");
        llvm::AllocaInst *resVar = builder_.CreateAlloca(i64Ty_, nullptr, "slin_res");
        builder_.CreateStore(llvm::ConstantInt::getSigned(i64Ty_, -1), resVar);
        llvm::AllocaInst *jVar = builder_.CreateAlloca(i64Ty_, nullptr, "slin_j");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), jVar);
        // For Set<any>, hoist scratch alloca/store for the search element outside
        // the loop so emitAnyBinaryOp does not create new allocas every iteration.
        llvm::AllocaInst *anyElemPtr = nullptr;
        llvm::AllocaInst *anyCandPtr = nullptr;
        llvm::FunctionCallee anyEqFn;
        if (elemIsAny) {
            anyElemPtr = builder_.CreateAlloca(anyTy_, nullptr, "slin.any.elem");
            builder_.CreateStore(elem, anyElemPtr);
            anyCandPtr = builder_.CreateAlloca(anyTy_, nullptr, "slin.any.cand");
            anyEqFn = getRuntimeFn("__ry_any_eq", i64Ty_, {ptrTy_, ptrTy_});
        }
        llvm::BasicBlock *condBB  = createBB("slin.cond");
        llvm::BasicBlock *bodyBB  = createBB("slin.body");
        llvm::BasicBlock *matchBB = createBB("slin.match");
        llvm::BasicBlock *nextBB  = createBB("slin.next");
        llvm::BasicBlock *endBB   = createBB("slin.end");
        emitBranchUncond(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *j = builder_.CreateLoad(i64Ty_, jVar, "slin_cj");
        emitBranchCond(builder_.CreateICmpSLT(j, sf.len), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *cp   = builder_.CreateGEP(elemTy, sf.elems, {j}, "slin_cp");
        llvm::Value *cand = builder_.CreateLoad(elemTy, cp, "slin_cand");
        if (!elemName.empty())
            propagateTypeMeta(elemName, cand);
        llvm::Value *eq;
        if (elemIsAny) {
            builder_.CreateStore(cand, anyCandPtr);
            llvm::Value *r = builder_.CreateCall(anyEqFn, {anyElemPtr, anyCandPtr}, "slin.any.eq");
            eq = builder_.CreateICmpNE(r, builder_.getInt64(0), "slin.any.eq.bool");
        } else {
            eq = emitComparisonOp("==", elem, cand, "", "");
        }
        emitBranchCond(eq, matchBB, nextBB);
        builder_.SetInsertPoint(matchBB);
        builder_.CreateStore(j, resVar);
        emitBranchUncond(endBB);
        builder_.SetInsertPoint(nextBB);
        builder_.CreateStore(builder_.CreateAdd(j, llvm::ConstantInt::get(i64Ty_, 1)), jVar);
        emitBranchUncond(condBB);
        builder_.SetInsertPoint(endBB);
        return builder_.CreateLoad(i64Ty_, resVar, "slin_result");
    }
    return emitHashTableLookup(setPtr, setHeaderTy_, kSetLayout, elem, elemTy);
}

llvm::Value *CodeGen::emitMapKeyLookup(llvm::Value *mapPtr, llvm::Value *key, llvm::Type *keyTy,
                                        const std::string &keyName) {
    // Named non-primitive keys (records, tuples, List<T>, Map<K,V>, Set<T>, any) have no
    // runtime hash function.  Use a structural O(n) linear scan over mf.keys.
    // `any` keys always require linear scan regardless of whether the caller
    // passed a keyName, since emitHashTableLookup has no hash path for anyTy_.
    const bool keyIsAny = isAnyType(keyTy);
    const bool keyIsStruct = !keyIsAny && llvm::isa<llvm::StructType>(keyTy);
    const bool needsLinearScan =
        keyIsAny || keyIsStruct ||
        (!keyName.empty() && keyName != "str" &&
         keyName != "int" && keyName != "float" && keyName != "bool");
    if (needsLinearScan) {
        // key is loop-invariant: propagate type metadata once before the loop.
        // The "__record__" sentinel opts into the linear scan for StructType keys
        // when map_key_type_name is absent; skip propagateTypeMeta in that case
        // since the sentinel is not a valid Ry type name.
        if (!keyIsAny && keyName != "__record__")
            propagateTypeMeta(keyName, key);
        auto mf = loadMapHeader(mapPtr, "mklin");
        llvm::AllocaInst *resVar = builder_.CreateAlloca(i64Ty_, nullptr, "mklin_res");
        builder_.CreateStore(llvm::ConstantInt::getSigned(i64Ty_, -1), resVar);
        llvm::AllocaInst *jVar = builder_.CreateAlloca(i64Ty_, nullptr, "mklin_j");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), jVar);
        // For Map<any, V>, hoist scratch alloca/store for the search key outside
        // the loop so the comparison does not create new allocas every iteration.
        llvm::AllocaInst *anyKeyPtr = nullptr;
        llvm::AllocaInst *anyKeyCandPtr = nullptr;
        llvm::FunctionCallee anyEqFn;
        if (keyIsAny) {
            anyKeyPtr = builder_.CreateAlloca(anyTy_, nullptr, "mklin.any.key");
            builder_.CreateStore(key, anyKeyPtr);
            anyKeyCandPtr = builder_.CreateAlloca(anyTy_, nullptr, "mklin.any.cand");
            anyEqFn = getRuntimeFn("__ry_any_eq", i64Ty_, {ptrTy_, ptrTy_});
        }
        llvm::BasicBlock *condBB  = createBB("mklin.cond");
        llvm::BasicBlock *bodyBB  = createBB("mklin.body");
        llvm::BasicBlock *matchBB = createBB("mklin.match");
        llvm::BasicBlock *nextBB  = createBB("mklin.next");
        llvm::BasicBlock *endBB   = createBB("mklin.end");
        emitBranchUncond(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *j = builder_.CreateLoad(i64Ty_, jVar, "mklin_cj");
        emitBranchCond(builder_.CreateICmpSLT(j, mf.len), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *cp   = builder_.CreateGEP(keyTy, mf.keys, {j}, "mklin_cp");
        llvm::Value *cand = builder_.CreateLoad(keyTy, cp, "mklin_cand");
        // cand changes each iteration; propagate per-iteration (skip any and sentinel).
        if (!keyIsAny && keyName != "__record__")
            propagateTypeMeta(keyName, cand);
        llvm::Value *eq;
        if (keyIsAny) {
            builder_.CreateStore(cand, anyKeyCandPtr);
            llvm::Value *r = builder_.CreateCall(anyEqFn, {anyKeyPtr, anyKeyCandPtr}, "mklin.any.eq");
            eq = builder_.CreateICmpNE(r, builder_.getInt64(0), "mklin.any.eq.bool");
        } else {
            eq = emitComparisonOp("==", key, cand, "", "");
        }
        emitBranchCond(eq, matchBB, nextBB);
        builder_.SetInsertPoint(matchBB);
        builder_.CreateStore(j, resVar);
        emitBranchUncond(endBB);
        builder_.SetInsertPoint(nextBB);
        builder_.CreateStore(builder_.CreateAdd(j, llvm::ConstantInt::get(i64Ty_, 1)), jVar);
        emitBranchUncond(condBB);
        builder_.SetInsertPoint(endBB);
        return builder_.CreateLoad(i64Ty_, resVar, "mklin_result");
    }
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
        mallocFn, {llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(bucketBytes))}, "buckets");
    // Fill with 0xFF bytes → each int64_t becomes -1 (EMPTY)
    builder_.CreateCall(memsetFn, {bucketsPtr,
        llvm::ConstantInt::get(i32Ty_, 0xFF),
        llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(bucketBytes))});

    llvm::Value *bcPtr = builder_.CreateStructGEP(headerTy, headerPtr, bucketCountIdx, "bc_ptr");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(initialBucketCount)), bcPtr);
    llvm::Value *bpPtr = builder_.CreateStructGEP(headerTy, headerPtr, bucketsPtrIdx, "bp_ptr");
    builder_.CreateStore(bucketsPtr, bpPtr);
}

// Helper: insert key into bucket + check load factor and rehash if needed
void CodeGen::emitBucketInsertAndRehashCheck(llvm::Value *headerPtr, llvm::StructType *headerTy,
                                              unsigned lenIdx, unsigned bucketCountIdx, unsigned bucketsPtrIdx,
                                              llvm::Value *key, llvm::Type *keyTy, llvm::Value *denseIndex) {
    // Records and tuples are StructType values with no hash function.
    // The dense elems array is maintained by the caller; skip hash-table bookkeeping.
    if (llvm::isa<llvm::StructType>(keyTy))
        return;
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

    llvm::FunctionCallee insertFn = getRuntimeFn("__ry_ht_insert", llvm::Type::getVoidTy(*ctx_), {ptrTy_, i64Ty_, i64Ty_, i64Ty_});
    builder_.CreateCall(insertFn, {bucketsPtr, bucketMask, hashVal, denseIndex});

    // Check load factor: len * 4 > bucketCount * 3 (i.e. len/bucketCount > 75%)
    llvm::Value *lenPtr = builder_.CreateStructGEP(headerTy, headerPtr, lenIdx, "len_for_rehash");
    llvm::Value *len = builder_.CreateLoad(i64Ty_, lenPtr, "len_rehash");
    llvm::Value *len4 = builder_.CreateMul(len, llvm::ConstantInt::get(i64Ty_, 4), "len4");
    llvm::Value *bc3 = builder_.CreateMul(bucketCount, llvm::ConstantInt::get(i64Ty_, 3), "bc3");
    llvm::Value *needRehash = builder_.CreateICmpSGT(len4, bc3, "need_rehash");

    llvm::BasicBlock *rehashBB = createBB("rehash");
    llvm::BasicBlock *doneRehashBB = createBB("rehash.done");
    emitBranchCond(needRehash, rehashBB, doneRehashBB);

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

    emitBranchUncond(doneRehashBB);
    builder_.SetInsertPoint(doneRehashBB);
}

} // namespace ry
