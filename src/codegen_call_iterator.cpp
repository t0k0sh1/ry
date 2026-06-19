#include "ry/codegen.hpp"
#include "ry/diagnostic/diagnostic.hpp"


namespace ry {

// ===== Builtin Iterator =====

// Helper: allocate IteratorHeader {next_fn, state} and track element type
//
// `elemName` carries the source-level Ry type name for the iterator element
// (e.g. "Map<str, int>" for List/Set iter over nested containers, or
// "(K, V)" for Map iter where the next-fn returns a tuple). Stamped into
// `ValueMetadata::iterator_elem_type_name` so for-in / toList consumers can
// recover collection metadata across the next-fn boundary. Empty when the
// source had no source-level metadata (e.g. List<int> where primitives
// don't need a name) — leaving the field empty is intentional and safe.
// (#2261)
static llvm::Value *emitIteratorHeaderAlloc(
    CodeGen &cg,
    llvm::IRBuilder<> &builder, llvm::Module &mod,
    llvm::StructType *iterHeaderTy, llvm::Type *i64Ty,
    llvm::FunctionCallee mallocFn,
    llvm::Function *nextFn, llvm::Value *stateAlloc, llvm::Type *elemTy,
    std::vector<llvm::Value*> &iterMallocs,
    const std::string &name,
    const std::string &elemName = "") {
    uint64_t headerSize = mod.getDataLayout().getTypeAllocSize(iterHeaderTy);
    llvm::Value *header = builder.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty, headerSize)}, name);
    builder.CreateStore(nextFn, builder.CreateStructGEP(iterHeaderTy, header, 0));
    builder.CreateStore(stateAlloc, builder.CreateStructGEP(iterHeaderTy, header, 1));
    cg.setTypeMeta(CodeGen::TypeMeta::IteratorElem, header, elemTy);
    if (!elemName.empty())
        cg.getOrCreateMeta(header).iterator_elem_type_name = elemName;
    iterMallocs.push_back(header);
    return header;
}

// Helper: load {next_fn, state} from an IteratorHeader
static std::pair<llvm::Value*, llvm::Value*> loadIteratorFields(
    llvm::IRBuilder<> &builder, llvm::StructType *iterHeaderTy,
    llvm::Type *ptrTy, llvm::Value *iterVal, const std::string &prefix) {
    llvm::Value *nfField = builder.CreateStructGEP(iterHeaderTy, iterVal, 0, prefix + "_nf");
    llvm::Value *nf = builder.CreateLoad(ptrTy, nfField, prefix + "_next_fn");
    llvm::Value *stField = builder.CreateStructGEP(iterHeaderTy, iterVal, 1, prefix + "_st");
    llvm::Value *st = builder.CreateLoad(ptrTy, stField, prefix + "_state");
    return {nf, st};
}

llvm::Value *CodeGen::emitBuiltinIterator(const CallExpr &e, llvm::Value *preEmittedArg0) {
    // iter(collection) → Iterator
    if (e.callee == "iter" && e.args.size() == 1) {
        llvm::Value *collVal = preEmittedArg0 ? preEmittedArg0 : emitExpr(*e.args[0]);
        if (collVal->getType() != ptrTy_)
            return nullptr;

        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();

        // Helper lambda: generate a dense-array next function for List/Set
        // State: { ptr data, i64 length, i64 index }
        // `elemName` carries the source-level Ry element type name snapshotted
        // from the source container (`list_elem_type_name` for List, etc.) so
        // the produced IteratorHeader can stamp it via #2261's channel. (#2261)
        auto emitDenseIterator = [&](llvm::Type *elemTy, llvm::StructType *collHeaderTy,
                                     unsigned dataPtrIdx, unsigned lenIdx,
                                     const std::string &kind,
                                     const std::string &elemName) -> llvm::Value* {
            llvm::StructType *stateTy = llvm::StructType::get(*ctx_, {ptrTy_, i64Ty_, i64Ty_});
            uint64_t stateSize = dl.getTypeAllocSize(stateTy);

            std::string fnName = "__iter_" + kind + "_next." + std::to_string(iterator_fn_counter_++);
            llvm::StructType *optTy = getOptionType(elemTy);
            llvm::FunctionType *nextFnTy = llvm::FunctionType::get(optTy, {ptrTy_}, false);
            llvm::Function *nextFn = emitCreateFunction(
                nextFnTy, llvm::Function::ExternalLinkage, fnName.c_str());

            {
                FnScope guard(*this);
                fn_ = nextFn;
                pushScope();
                llvm::BasicBlock *entry = createBBInFn("entry", nextFn);
                builder_.SetInsertPoint(entry);

                // #2186 (A/A2 follow-on): the next-fn body is built entirely
                // through the emission boundary — emitGetParam / emitStructGEP /
                // emitLoad / emitICmpSLT / emitGEP / emitConstInt / emitAdd /
                // emitStore / emitRet — so it carries no IRBuilder<>::Create*.
                llvm::Value *statePtr = emitGetParam(nextFn, 0);
                llvm::Value *data = emitLoad(ptrTy_,
                    emitStructGEP(stateTy, statePtr, 0, ""), "data");
                llvm::Value *len = emitLoad(i64Ty_,
                    emitStructGEP(stateTy, statePtr, 1, ""), "len");
                llvm::Value *idxField = emitStructGEP(stateTy, statePtr, 2, "idx_field");
                llvm::Value *idx = emitLoad(i64Ty_, idxField, "idx");

                llvm::BasicBlock *someBB = createBBInFn("some", nextFn);
                llvm::BasicBlock *noneBB = createBBInFn("none", nextFn);
                emitBranchCond(emitICmpSLT(idx, len, "in_bounds"), someBB, noneBB);

                builder_.SetInsertPoint(someBB);
                llvm::Value *elem = emitLoad(elemTy,
                    emitGEP(elemTy, data, idx, "elem_ptr"), "elem");
                emitStore(
                    emitAdd(idx, emitConstInt(i64Ty_, 1), "next_idx"), idxField);
                emitRet(buildSomeValue(elem, optTy));

                builder_.SetInsertPoint(noneBB);
                emitRet(buildNoneValue(optTy));
                popScope();
            }

            // Allocate and fill state
            llvm::Value *stateAlloc = builder_.CreateCall(
                mallocFn, {llvm::ConstantInt::get(i64Ty_, stateSize)}, "iter_state");
            iterator_malloc_stack_.back().push_back(stateAlloc);
            llvm::Value *srcData = builder_.CreateLoad(ptrTy_,
                builder_.CreateStructGEP(collHeaderTy, collVal, dataPtrIdx), "src_data");
            llvm::Value *srcLen = builder_.CreateLoad(i64Ty_,
                builder_.CreateStructGEP(collHeaderTy, collVal, lenIdx), "src_len");
            builder_.CreateStore(srcData, builder_.CreateStructGEP(stateTy, stateAlloc, 0));
            builder_.CreateStore(srcLen, builder_.CreateStructGEP(stateTy, stateAlloc, 1));
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0),
                builder_.CreateStructGEP(stateTy, stateAlloc, 2));

            return emitIteratorHeaderAlloc(*this, builder_, *mod_, iteratorHeaderTy_,
                i64Ty_, mallocFn, nextFn, stateAlloc, elemTy,
                iterator_malloc_stack_.back(), "iter_header", elemName);
        };

        // Snapshot source container element type names BEFORE next-fn emission.
        // getMeta() returns a pointer into value_metadata_ which may rehash
        // when emitDenseIterator() touches metadata for the produced header.
        // (#2261, rule "Metadata rebuilds: every load / extract / PHI ...
        // snapshot pattern" in codegen-type-and-metadata.md)
        std::string listElemName, setElemName, mapKeyName, mapValName;
        if (auto *meta = getMeta(collVal)) {
            listElemName = meta->list_elem_type_name;
            setElemName  = meta->set_elem_type_name;
            mapKeyName   = meta->map_key_type_name;
            mapValName   = meta->map_value_type_name;
        }

        // Try List (data at index 2, len at index 0)
        if (llvm::Type *elemTy = getListElementType(collVal))
            return emitDenseIterator(elemTy, listHeaderTy_, 2, 0, "list", listElemName);

        // Try Set (data at index 2, len at index 0)
        if (llvm::Type *setElemTy = getSetElementType(collVal))
            return emitDenseIterator(setElemTy, setHeaderTy_, 2, 0, "set", setElemName);

        // Try Map → Iterator over (K, V) tuples
        llvm::Type *keyTy = getMapKeyType(collVal);
        llvm::Type *valTy = getMapValueType(collVal);
        if (keyTy && valTy) {
            // #2261: build "(K, V)" so the produced IteratorHeader carries a
            // tuple sig — `splitTupleSig` inside `emitForBindingPattern`
            // decomposes it onto per-binding loop vars (`for k, v in iter(m):`).
            std::string mapElemName;
            if (!mapKeyName.empty() && !mapValName.empty())
                mapElemName = "(" + mapKeyName + ", " + mapValName + ")";

            llvm::StructType *tupleTy = llvm::StructType::get(*ctx_, {keyTy, valTy});
            llvm::StructType *stateTy = llvm::StructType::get(*ctx_, {ptrTy_, ptrTy_, i64Ty_, i64Ty_});
            uint64_t stateSize = dl.getTypeAllocSize(stateTy);

            std::string fnName = "__iter_map_next." + std::to_string(iterator_fn_counter_++);
            llvm::StructType *optTy = getOptionType(tupleTy);
            llvm::FunctionType *nextFnTy = llvm::FunctionType::get(optTy, {ptrTy_}, false);
            llvm::Function *nextFn = emitCreateFunction(
                nextFnTy, llvm::Function::ExternalLinkage, fnName.c_str());

            {
                FnScope guard(*this);
                fn_ = nextFn;
                pushScope();
                llvm::BasicBlock *entry = createBBInFn("entry", nextFn);
                builder_.SetInsertPoint(entry);

                // #2186 (A/A2 follow-on): same boundary-routed next-fn body as
                // the List/Set dense iter above. The {key, val} tuple-build adds
                // emitUndef + emitInsertValue (#2186 primitives, the symmetric
                // partner of #2099's emitExtractValue).
                llvm::Value *statePtr = emitGetParam(nextFn, 0);
                llvm::Value *keys = emitLoad(ptrTy_,
                    emitStructGEP(stateTy, statePtr, 0, ""), "keys");
                llvm::Value *vals = emitLoad(ptrTy_,
                    emitStructGEP(stateTy, statePtr, 1, ""), "vals");
                llvm::Value *len = emitLoad(i64Ty_,
                    emitStructGEP(stateTy, statePtr, 2, ""), "len");
                llvm::Value *idxField = emitStructGEP(stateTy, statePtr, 3, "idx_field");
                llvm::Value *idx = emitLoad(i64Ty_, idxField, "idx");

                llvm::BasicBlock *someBB = createBBInFn("some", nextFn);
                llvm::BasicBlock *noneBB = createBBInFn("none", nextFn);
                emitBranchCond(emitICmpSLT(idx, len, "in_bounds"), someBB, noneBB);

                builder_.SetInsertPoint(someBB);
                llvm::Value *key = emitLoad(keyTy,
                    emitGEP(keyTy, keys, idx, "key_ptr"), "key");
                llvm::Value *val = emitLoad(valTy,
                    emitGEP(valTy, vals, idx, "val_ptr"), "val");
                llvm::Value *tuple = emitUndef(tupleTy);
                tuple = emitInsertValue(tuple, key, 0, "tuple_k");
                tuple = emitInsertValue(tuple, val, 1, "tuple_kv");
                emitStore(
                    emitAdd(idx, emitConstInt(i64Ty_, 1), "next_idx"), idxField);
                emitRet(buildSomeValue(tuple, optTy));

                builder_.SetInsertPoint(noneBB);
                emitRet(buildNoneValue(optTy));
                popScope();
            }

            llvm::Value *stateAlloc = builder_.CreateCall(
                mallocFn, {llvm::ConstantInt::get(i64Ty_, stateSize)}, "iter_state");
            iterator_malloc_stack_.back().push_back(stateAlloc);
            builder_.CreateStore(
                builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(mapHeaderTy_, collVal, 2)),
                builder_.CreateStructGEP(stateTy, stateAlloc, 0));
            builder_.CreateStore(
                builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(mapHeaderTy_, collVal, 3)),
                builder_.CreateStructGEP(stateTy, stateAlloc, 1));
            builder_.CreateStore(
                builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(mapHeaderTy_, collVal, 0)),
                builder_.CreateStructGEP(stateTy, stateAlloc, 2));
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0),
                builder_.CreateStructGEP(stateTy, stateAlloc, 3));

            return emitIteratorHeaderAlloc(*this, builder_, *mod_, iteratorHeaderTy_,
                i64Ty_, mallocFn, nextFn, stateAlloc, tupleTy,
                iterator_malloc_stack_.back(), "iter_header", mapElemName);
        }

        codegenError("iter() argument must be a List, Set, or Map");
    }

    // toList() → collect Iterator into List
    if (e.callee == "toList" && e.args.size() == 1) {
        llvm::Value *iterVal = preEmittedArg0 ? preEmittedArg0 : emitExpr(*e.args[0]);
        llvm::Type *elemTy = getIteratorElementType(iterVal);
        if (!elemTy) return nullptr;

        auto mallocFn = getStdlibMalloc();
        auto reallocFn = getStdlibRealloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t elemSize = dl.getTypeAllocSize(elemTy);

        auto [nextFnPtr, statePtr] = loadIteratorFields(
            builder_, iteratorHeaderTy_, ptrTy_, iterVal, "tl");

        // Allocate list header
        llvm::Value *headerPtr = emitArcAllocCollectionHeader(listHeaderTy_);

        // Initial capacity = 8
        llvm::AllocaInst *capVar = builder_.CreateAlloca(i64Ty_, nullptr, "tl_cap");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 8), capVar);
        llvm::AllocaInst *lenVar = builder_.CreateAlloca(i64Ty_, nullptr, "tl_len");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), lenVar);
        llvm::AllocaInst *dataVar = builder_.CreateAlloca(ptrTy_, nullptr, "tl_data_var");
        llvm::Value *initData = builder_.CreateCall(
            mallocFn, {llvm::ConstantInt::get(i64Ty_, elemSize * 8)}, "tl_init_data");
        builder_.CreateStore(initData, dataVar);

        llvm::StructType *optTy = getOptionType(elemTy);
        llvm::FunctionType *nextCallTy = llvm::FunctionType::get(optTy, {ptrTy_}, false);

        llvm::BasicBlock *condBB = createBB("tl.cond");
        llvm::BasicBlock *bodyBB = createBB("tl.body");
        llvm::BasicBlock *growBB = createBB("tl.grow");
        llvm::BasicBlock *storeBB = createBB("tl.store");
        llvm::BasicBlock *endBB = createBB("tl.end");

        emitBranchUncond(condBB);

        builder_.SetInsertPoint(condBB);
        llvm::Value *opt = builder_.CreateCall(nextCallTy, nextFnPtr, {statePtr}, "tl_opt");
        llvm::Value *hasVal = builder_.CreateExtractValue(opt, 0, "tl_has");
        emitBranchCond(hasVal, bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        llvm::Value *elem = builder_.CreateExtractValue(opt, 1, "tl_elem");
        llvm::Value *curLen = builder_.CreateLoad(i64Ty_, lenVar, "tl_cur_len");
        llvm::Value *curCap = builder_.CreateLoad(i64Ty_, capVar, "tl_cur_cap");
        emitBranchCond(builder_.CreateICmpEQ(curLen, curCap, "tl_need_grow"), growBB, storeBB);

        builder_.SetInsertPoint(growBB);
        llvm::Value *newCap = builder_.CreateMul(curCap, llvm::ConstantInt::get(i64Ty_, 2), "tl_new_cap");
        builder_.CreateStore(newCap, capVar);
        llvm::Value *newData = builder_.CreateCall(reallocFn, {
            builder_.CreateLoad(ptrTy_, dataVar, "tl_old_data"),
            builder_.CreateMul(newCap, llvm::ConstantInt::get(i64Ty_, elemSize), "tl_new_size")
        }, "tl_new_data");
        builder_.CreateStore(newData, dataVar);
        emitBranchUncond(storeBB);

        builder_.SetInsertPoint(storeBB);
        llvm::Value *storeLen = builder_.CreateLoad(i64Ty_, lenVar, "tl_store_len");
        llvm::Value *storeData = builder_.CreateLoad(ptrTy_, dataVar, "tl_store_data");
        builder_.CreateStore(elem, builder_.CreateGEP(elemTy, storeData, {storeLen}, "tl_dst_ptr"));
        builder_.CreateStore(
            builder_.CreateAdd(storeLen, llvm::ConstantInt::get(i64Ty_, 1), "tl_new_len"), lenVar);
        emitBranchUncond(condBB);

        builder_.SetInsertPoint(endBB);
        builder_.CreateStore(builder_.CreateLoad(i64Ty_, lenVar, "tl_final_len"),
            builder_.CreateStructGEP(listHeaderTy_, headerPtr, 0));
        builder_.CreateStore(builder_.CreateLoad(i64Ty_, capVar, "tl_final_cap"),
            builder_.CreateStructGEP(listHeaderTy_, headerPtr, 1));
        builder_.CreateStore(builder_.CreateLoad(ptrTy_, dataVar, "tl_final_data"),
            builder_.CreateStructGEP(listHeaderTy_, headerPtr, 2));

        setTypeMeta(TypeMeta::ListElem, headerPtr, elemTy);

        // Propagate nested list metadata for flat() support
        {
            llvm::Type *nestedTy = getNestedListElementType(iterVal);
            if (nestedTy)
                setTypeMeta(TypeMeta::NestedListElem, headerPtr, nestedTy);
        }

        // #2261: propagate the iterator's source-level element type name onto
        // the result List header so downstream `ys[i]` / `len(ys[i])` /
        // method dispatch resolves the original collection type. Without
        // this, `toList(iter(xs: List<Map<...>>))[0]["k"]` fails with
        // `str does not support index access` because the result List
        // header carries no `list_elem_type_name`.
        if (auto *meta = getMeta(iterVal); meta && !meta->iterator_elem_type_name.empty())
            getOrCreateMeta(headerPtr).list_elem_type_name = meta->iterator_elem_type_name;

        return headerPtr;
    }

    // next() → call next_fn(state) on Iterator
    if (e.callee == "next" && e.args.size() == 1) {
        llvm::Value *iterVal = preEmittedArg0 ? preEmittedArg0 : emitExpr(*e.args[0]);
        llvm::Type *elemTy = getIteratorElementType(iterVal);
        if (!elemTy) return nullptr;

        auto [nextFnPtr, statePtr] = loadIteratorFields(
            builder_, iteratorHeaderTy_, ptrTy_, iterVal, "next");
        llvm::StructType *optTy = getOptionType(elemTy);
        llvm::FunctionType *nextCallTy = llvm::FunctionType::get(optTy, {ptrTy_}, false);
        return builder_.CreateCall(nextCallTy, nextFnPtr, {statePtr}, "next_result");
    }

    // filter(iter, predicate) → new Iterator
    if (e.callee == "filter" && e.args.size() == 2) {
        llvm::Value *iterVal = preEmittedArg0 ? preEmittedArg0 : emitExpr(*e.args[0]);
        llvm::Type *elemTy = getIteratorElementType(iterVal);
        if (!elemTy) return nullptr;

        llvm::Value *lambdaVal = emitExpr(*e.args[1]);
        auto *fnInfo = lookupFnTypeInfo(lambdaVal);
        if (!fnInfo)
            codegenError("filter() on iterator requires a predicate function");
        auto info = *fnInfo;

        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();

        // State: { ptr src_next_fn, ptr src_state, ptr predicate }
        llvm::StructType *stateTy = llvm::StructType::get(*ctx_, {ptrTy_, ptrTy_, ptrTy_});

        std::string fnName = "__iter_filter_next." + std::to_string(iterator_fn_counter_++);
        llvm::StructType *optTy = getOptionType(elemTy);
        llvm::FunctionType *nextFnTy = llvm::FunctionType::get(optTy, {ptrTy_}, false);
        // #2186: same emitCreateFunction surface as map / take.
        llvm::Function *filterNextFn = emitCreateFunction(
            nextFnTy, llvm::Function::ExternalLinkage, fnName.c_str());

        {
            FnScope guard(*this);
            fn_ = filterNextFn;
            pushScope();
            llvm::BasicBlock *entry = createBBInFn("entry", filterNextFn);
            builder_.SetInsertPoint(entry);

            // #2099 pilot: the next-fn body is built entirely through the
            // emission boundary — emitGetParam / emitStructGEP / emitLoad /
            // emitCallIndirect / emitExtractValue (#2099) / emitRet + existing
            // emitBranchCond / buildSomeValue / buildNoneValue — so it carries no
            // IRBuilder<>::Create*. The closure call goes through emitLambdaCall
            // (also boundary-routed). FnScope / fn_ / pushScope / SetInsertPoint
            // stay C++ (codegen state / builder position, not IR). The outer
            // state-setup below (malloc + the three stores + header alloc) stays
            // C++ — that is the ValueMetadata-crossing surface (#2100).
            llvm::Value *statePtr = emitGetParam(filterNextFn, 0);
            llvm::Value *srcNextFn = emitLoad(ptrTy_,
                emitStructGEP(stateTy, statePtr, 0, ""), "src_next_fn");
            llvm::Value *srcState = emitLoad(ptrTy_,
                emitStructGEP(stateTy, statePtr, 1, ""), "src_state");
            llvm::Value *predPtr = emitLoad(ptrTy_,
                emitStructGEP(stateTy, statePtr, 2, ""), "pred_ptr");

            llvm::FunctionType *srcNextCallTy = llvm::FunctionType::get(optTy, {ptrTy_}, false);
            llvm::BasicBlock *loopBB = createBBInFn("loop", filterNextFn);
            emitBranchUncond(loopBB);

            builder_.SetInsertPoint(loopBB);
            llvm::Value *opt = emitCallIndirect(srcNextCallTy, srcNextFn, {srcState}, "src_opt");
            llvm::Value *hasVal = emitExtractValue(opt, 0, "has_val");
            llvm::BasicBlock *checkBB = createBBInFn("check", filterNextFn);
            llvm::BasicBlock *noneBB = createBBInFn("none", filterNextFn);
            emitBranchCond(hasVal, checkBB, noneBB);

            builder_.SetInsertPoint(checkBB);
            llvm::Value *elem = emitExtractValue(opt, 1, "elem");
            llvm::Value *predResult = emitLambdaCall(predPtr, info, {elem}, "pred_result");
            llvm::BasicBlock *matchBB = createBBInFn("match", filterNextFn);
            emitBranchCond(predResult, matchBB, loopBB);

            builder_.SetInsertPoint(matchBB);
            emitRet(buildSomeValue(elem, optTy));

            builder_.SetInsertPoint(noneBB);
            emitRet(buildNoneValue(optTy));
            popScope();
        }

        auto [srcNf, srcSt] = loadIteratorFields(
            builder_, iteratorHeaderTy_, ptrTy_, iterVal, "filter");
        uint64_t stateSize = dl.getTypeAllocSize(stateTy);
        llvm::Value *stateAlloc = builder_.CreateCall(
            mallocFn, {llvm::ConstantInt::get(i64Ty_, stateSize)}, "filter_state");
        iterator_malloc_stack_.back().push_back(stateAlloc);
        builder_.CreateStore(srcNf, builder_.CreateStructGEP(stateTy, stateAlloc, 0));
        builder_.CreateStore(srcSt, builder_.CreateStructGEP(stateTy, stateAlloc, 1));
        builder_.CreateStore(lambdaVal, builder_.CreateStructGEP(stateTy, stateAlloc, 2));

        // #2261: filter preserves element type, so copy iterator_elem_type_name
        // from the source iterator onto the new header.
        std::string filterElemName;
        if (auto *meta = getMeta(iterVal))
            filterElemName = meta->iterator_elem_type_name;

        return emitIteratorHeaderAlloc(*this, builder_, *mod_, iteratorHeaderTy_,
            i64Ty_, mallocFn, filterNextFn, stateAlloc, elemTy,
            iterator_malloc_stack_.back(), "filter_iter", filterElemName);
    }

    // map(iter, transform) → new Iterator with transformed element type
    if (e.callee == "map" && e.args.size() == 2) {
        llvm::Value *iterVal = preEmittedArg0 ? preEmittedArg0 : emitExpr(*e.args[0]);
        llvm::Type *elemTy = getIteratorElementType(iterVal);
        if (!elemTy) return nullptr;

        llvm::Value *lambdaVal = emitExpr(*e.args[1]);
        auto *fnInfo = lookupFnTypeInfo(lambdaVal);
        if (!fnInfo)
            codegenError("map() on iterator requires a transform function");
        auto info = *fnInfo;
        llvm::Type *outElemTy = info.returnType;

        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();

        llvm::StructType *stateTy = llvm::StructType::get(*ctx_, {ptrTy_, ptrTy_, ptrTy_});

        std::string fnName = "__iter_map_next." + std::to_string(iterator_fn_counter_++);
        llvm::StructType *srcOptTy = getOptionType(elemTy);
        llvm::StructType *outOptTy = getOptionType(outElemTy);
        llvm::FunctionType *nextFnTy = llvm::FunctionType::get(outOptTy, {ptrTy_}, false);
        // #2186: same emitCreateFunction surface as filter / take.
        llvm::Function *mapNextFn = emitCreateFunction(
            nextFnTy, llvm::Function::ExternalLinkage, fnName.c_str());

        {
            FnScope guard(*this);
            fn_ = mapNextFn;
            pushScope();
            llvm::BasicBlock *entry = createBBInFn("entry", mapNextFn);
            builder_.SetInsertPoint(entry);

            // #2099 pilot: same boundary-routed next-fn body as filter above —
            // zero IRBuilder<>::Create*; the transform closure call goes through
            // emitLambdaCall. Outer state-setup stays C++ (#2100).
            llvm::Value *statePtr = emitGetParam(mapNextFn, 0);
            llvm::Value *srcNextFn = emitLoad(ptrTy_,
                emitStructGEP(stateTy, statePtr, 0, ""), "src_next_fn");
            llvm::Value *srcState = emitLoad(ptrTy_,
                emitStructGEP(stateTy, statePtr, 1, ""), "src_state");
            llvm::Value *transPtr = emitLoad(ptrTy_,
                emitStructGEP(stateTy, statePtr, 2, ""), "trans_ptr");

            llvm::FunctionType *srcNextCallTy = llvm::FunctionType::get(srcOptTy, {ptrTy_}, false);
            llvm::Value *opt = emitCallIndirect(srcNextCallTy, srcNextFn, {srcState}, "src_opt");
            llvm::Value *hasVal = emitExtractValue(opt, 0, "has_val");

            llvm::BasicBlock *someBB = createBBInFn("some", mapNextFn);
            llvm::BasicBlock *noneBB = createBBInFn("none", mapNextFn);
            emitBranchCond(hasVal, someBB, noneBB);

            builder_.SetInsertPoint(someBB);
            llvm::Value *elem = emitExtractValue(opt, 1, "elem");
            emitRet(buildSomeValue(emitLambdaCall(transPtr, info, {elem}, "mapped"), outOptTy));

            builder_.SetInsertPoint(noneBB);
            emitRet(buildNoneValue(outOptTy));
            popScope();
        }

        auto [srcNf, srcSt] = loadIteratorFields(
            builder_, iteratorHeaderTy_, ptrTy_, iterVal, "map");
        uint64_t stateSize = dl.getTypeAllocSize(stateTy);
        llvm::Value *stateAlloc = builder_.CreateCall(
            mallocFn, {llvm::ConstantInt::get(i64Ty_, stateSize)}, "map_state");
        iterator_malloc_stack_.back().push_back(stateAlloc);
        builder_.CreateStore(srcNf, builder_.CreateStructGEP(stateTy, stateAlloc, 0));
        builder_.CreateStore(srcSt, builder_.CreateStructGEP(stateTy, stateAlloc, 1));
        builder_.CreateStore(lambdaVal, builder_.CreateStructGEP(stateTy, stateAlloc, 2));

        // #2261: map produces an element of `info.returnTypeName`. Identity
        // lambdas (`m => m`) leave `info.returnTypeName` empty because lambda
        // return inference cannot resolve through a param without an inferred
        // type — fall back to the source iterator's `iterator_elem_type_name`
        // when the LLVM type is unchanged AND ptrTy_ (so the result is the
        // same container type). General lambda inference fix is out of scope
        // (see plan's "scope 外 follow-up").
        std::string mapElemName = info.returnTypeName;
        if (mapElemName.empty() && outElemTy == elemTy && elemTy == ptrTy_) {
            if (auto *meta = getMeta(iterVal))
                mapElemName = meta->iterator_elem_type_name;
        }

        return emitIteratorHeaderAlloc(*this, builder_, *mod_, iteratorHeaderTy_,
            i64Ty_, mallocFn, mapNextFn, stateAlloc, outElemTy,
            iterator_malloc_stack_.back(), "map_iter", mapElemName);
    }

    // take(iter, n) → new Iterator that yields at most n elements
    if (e.callee == "take" && e.args.size() == 2) {
        llvm::Value *iterVal = preEmittedArg0 ? preEmittedArg0 : emitExpr(*e.args[0]);
        llvm::Type *elemTy = getIteratorElementType(iterVal);
        if (!elemTy) return nullptr;

        llvm::Value *n = emitExpr(*e.args[1]);

        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();

        llvm::StructType *stateTy = llvm::StructType::get(*ctx_, {ptrTy_, ptrTy_, i64Ty_});

        std::string fnName = "__iter_take_next." + std::to_string(iterator_fn_counter_++);
        llvm::StructType *optTy = getOptionType(elemTy);
        llvm::FunctionType *nextFnTy = llvm::FunctionType::get(optTy, {ptrTy_}, false);
        // #2098 pilot: the next-fn is built entirely through the emission
        // boundary — emitCreateFunction (llvm::Function::Create), emitGetParam,
        // emitStructGEP / emitLoad / emitICmpSGT / emitConstInt / emitSub /
        // emitStore (existing primitives), emitCallIndirect, emitRet — so Part A
        // carries no IRBuilder<>::Create*. FnScope / fn_ / pushScope / popScope /
        // SetInsertPoint stay C++ (codegen state / builder position, not IR).
        llvm::Function *takeNextFn = emitCreateFunction(
            nextFnTy, llvm::Function::ExternalLinkage, fnName.c_str());

        {
            FnScope guard(*this);
            fn_ = takeNextFn;
            pushScope();
            llvm::BasicBlock *entry = createBBInFn("entry", takeNextFn);
            builder_.SetInsertPoint(entry);

            llvm::Value *statePtr = emitGetParam(takeNextFn, 0);
            llvm::Value *srcNextFn = emitLoad(ptrTy_,
                emitStructGEP(stateTy, statePtr, 0, ""), "src_next_fn");
            llvm::Value *srcState = emitLoad(ptrTy_,
                emitStructGEP(stateTy, statePtr, 1, ""), "src_state");
            llvm::Value *remField = emitStructGEP(stateTy, statePtr, 2, "rem_f");
            llvm::Value *remaining = emitLoad(i64Ty_, remField, "remaining");

            llvm::BasicBlock *callBB = createBBInFn("call", takeNextFn);
            llvm::BasicBlock *noneBB = createBBInFn("none", takeNextFn);
            emitBranchCond(
                emitICmpSGT(remaining, emitConstInt(i64Ty_, 0), "has_rem"),
                callBB, noneBB);

            builder_.SetInsertPoint(callBB);
            emitStore(
                emitSub(remaining, emitConstInt(i64Ty_, 1), "new_rem"), remField);
            llvm::FunctionType *srcNextCallTy = llvm::FunctionType::get(optTy, {ptrTy_}, false);
            emitRet(emitCallIndirect(srcNextCallTy, srcNextFn, {srcState}, "src_opt"));

            builder_.SetInsertPoint(noneBB);
            emitRet(buildNoneValue(optTy));
            popScope();
        }

        auto [srcNf, srcSt] = loadIteratorFields(
            builder_, iteratorHeaderTy_, ptrTy_, iterVal, "take");
        uint64_t stateSize = dl.getTypeAllocSize(stateTy);
        llvm::Value *stateAlloc = builder_.CreateCall(
            mallocFn, {llvm::ConstantInt::get(i64Ty_, stateSize)}, "take_state");
        iterator_malloc_stack_.back().push_back(stateAlloc);
        builder_.CreateStore(srcNf, builder_.CreateStructGEP(stateTy, stateAlloc, 0));
        builder_.CreateStore(srcSt, builder_.CreateStructGEP(stateTy, stateAlloc, 1));
        builder_.CreateStore(n, builder_.CreateStructGEP(stateTy, stateAlloc, 2));

        // #2261: take preserves element type — copy iterator_elem_type_name.
        std::string takeElemName;
        if (auto *meta = getMeta(iterVal))
            takeElemName = meta->iterator_elem_type_name;

        return emitIteratorHeaderAlloc(*this, builder_, *mod_, iteratorHeaderTy_,
            i64Ty_, mallocFn, takeNextFn, stateAlloc, elemTy,
            iterator_malloc_stack_.back(), "take_iter", takeElemName);
    }

    return nullptr;
}

} // namespace ry
