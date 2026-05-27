#include "ry/codegen.hpp"
#include "ry/diagnostic/diagnostic.hpp"


namespace ry {

// ===== Builtin Iterator =====

// Helper: allocate IteratorHeader {next_fn, state} and track element type
static llvm::Value *emitIteratorHeaderAlloc(
    CodeGen &cg,
    llvm::IRBuilder<> &builder, llvm::Module &mod,
    llvm::StructType *iterHeaderTy, llvm::Type *i64Ty,
    llvm::FunctionCallee mallocFn,
    llvm::Function *nextFn, llvm::Value *stateAlloc, llvm::Type *elemTy,
    std::vector<llvm::Value*> &iterMallocs,
    const std::string &name) {
    uint64_t headerSize = mod.getDataLayout().getTypeAllocSize(iterHeaderTy);
    llvm::Value *header = builder.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty, headerSize)}, name);
    builder.CreateStore(nextFn, builder.CreateStructGEP(iterHeaderTy, header, 0));
    builder.CreateStore(stateAlloc, builder.CreateStructGEP(iterHeaderTy, header, 1));
    cg.setTypeMeta(CodeGen::TypeMeta::IteratorElem, header, elemTy);
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
        auto emitDenseIterator = [&](llvm::Type *elemTy, llvm::StructType *collHeaderTy,
                                     unsigned dataPtrIdx, unsigned lenIdx,
                                     const std::string &kind) -> llvm::Value* {
            llvm::StructType *stateTy = llvm::StructType::get(*ctx_, {ptrTy_, i64Ty_, i64Ty_});
            uint64_t stateSize = dl.getTypeAllocSize(stateTy);

            std::string fnName = "__iter_" + kind + "_next." + std::to_string(iterator_fn_counter_++);
            llvm::StructType *optTy = getOptionType(elemTy);
            llvm::FunctionType *nextFnTy = llvm::FunctionType::get(optTy, {ptrTy_}, false);
            llvm::Function *nextFn = llvm::Function::Create(
                nextFnTy, llvm::Function::ExternalLinkage, fnName, *mod_);

            {
                FnScope guard(*this);
                fn_ = nextFn;
                pushScope();
                llvm::BasicBlock *entry = llvm::BasicBlock::Create(*ctx_, "entry", nextFn);
                builder_.SetInsertPoint(entry);

                llvm::Value *statePtr = nextFn->getArg(0);
                llvm::Value *data = builder_.CreateLoad(ptrTy_,
                    builder_.CreateStructGEP(stateTy, statePtr, 0), "data");
                llvm::Value *len = builder_.CreateLoad(i64Ty_,
                    builder_.CreateStructGEP(stateTy, statePtr, 1), "len");
                llvm::Value *idxField = builder_.CreateStructGEP(stateTy, statePtr, 2, "idx_field");
                llvm::Value *idx = builder_.CreateLoad(i64Ty_, idxField, "idx");

                llvm::BasicBlock *someBB = llvm::BasicBlock::Create(*ctx_, "some", nextFn);
                llvm::BasicBlock *noneBB = llvm::BasicBlock::Create(*ctx_, "none", nextFn);
                builder_.CreateCondBr(builder_.CreateICmpSLT(idx, len, "in_bounds"), someBB, noneBB);

                builder_.SetInsertPoint(someBB);
                llvm::Value *elem = builder_.CreateLoad(elemTy,
                    builder_.CreateGEP(elemTy, data, {idx}, "elem_ptr"), "elem");
                builder_.CreateStore(
                    builder_.CreateAdd(idx, llvm::ConstantInt::get(i64Ty_, 1), "next_idx"), idxField);
                builder_.CreateRet(buildSomeValue(elem, optTy));

                builder_.SetInsertPoint(noneBB);
                builder_.CreateRet(buildNoneValue(optTy));
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
                iterator_malloc_stack_.back(), "iter_header");
        };

        // Try List (data at index 2, len at index 0)
        if (llvm::Type *elemTy = getListElementType(collVal))
            return emitDenseIterator(elemTy, listHeaderTy_, 2, 0, "list");

        // Try Set (data at index 2, len at index 0)
        if (llvm::Type *setElemTy = getSetElementType(collVal))
            return emitDenseIterator(setElemTy, setHeaderTy_, 2, 0, "set");

        // Try Map → Iterator over (K, V) tuples
        llvm::Type *keyTy = getMapKeyType(collVal);
        llvm::Type *valTy = getMapValueType(collVal);
        if (keyTy && valTy) {
            llvm::StructType *tupleTy = llvm::StructType::get(*ctx_, {keyTy, valTy});
            llvm::StructType *stateTy = llvm::StructType::get(*ctx_, {ptrTy_, ptrTy_, i64Ty_, i64Ty_});
            uint64_t stateSize = dl.getTypeAllocSize(stateTy);

            std::string fnName = "__iter_map_next." + std::to_string(iterator_fn_counter_++);
            llvm::StructType *optTy = getOptionType(tupleTy);
            llvm::FunctionType *nextFnTy = llvm::FunctionType::get(optTy, {ptrTy_}, false);
            llvm::Function *nextFn = llvm::Function::Create(
                nextFnTy, llvm::Function::ExternalLinkage, fnName, *mod_);

            {
                FnScope guard(*this);
                fn_ = nextFn;
                pushScope();
                llvm::BasicBlock *entry = llvm::BasicBlock::Create(*ctx_, "entry", nextFn);
                builder_.SetInsertPoint(entry);

                llvm::Value *statePtr = nextFn->getArg(0);
                llvm::Value *keys = builder_.CreateLoad(ptrTy_,
                    builder_.CreateStructGEP(stateTy, statePtr, 0), "keys");
                llvm::Value *vals = builder_.CreateLoad(ptrTy_,
                    builder_.CreateStructGEP(stateTy, statePtr, 1), "vals");
                llvm::Value *len = builder_.CreateLoad(i64Ty_,
                    builder_.CreateStructGEP(stateTy, statePtr, 2), "len");
                llvm::Value *idxField = builder_.CreateStructGEP(stateTy, statePtr, 3, "idx_field");
                llvm::Value *idx = builder_.CreateLoad(i64Ty_, idxField, "idx");

                llvm::BasicBlock *someBB = llvm::BasicBlock::Create(*ctx_, "some", nextFn);
                llvm::BasicBlock *noneBB = llvm::BasicBlock::Create(*ctx_, "none", nextFn);
                builder_.CreateCondBr(builder_.CreateICmpSLT(idx, len, "in_bounds"), someBB, noneBB);

                builder_.SetInsertPoint(someBB);
                llvm::Value *key = builder_.CreateLoad(keyTy,
                    builder_.CreateGEP(keyTy, keys, {idx}, "key_ptr"), "key");
                llvm::Value *val = builder_.CreateLoad(valTy,
                    builder_.CreateGEP(valTy, vals, {idx}, "val_ptr"), "val");
                llvm::Value *tuple = llvm::UndefValue::get(tupleTy);
                tuple = builder_.CreateInsertValue(tuple, key, 0, "tuple_k");
                tuple = builder_.CreateInsertValue(tuple, val, 1, "tuple_kv");
                builder_.CreateStore(
                    builder_.CreateAdd(idx, llvm::ConstantInt::get(i64Ty_, 1), "next_idx"), idxField);
                builder_.CreateRet(buildSomeValue(tuple, optTy));

                builder_.SetInsertPoint(noneBB);
                builder_.CreateRet(buildNoneValue(optTy));
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
                iterator_malloc_stack_.back(), "iter_header");
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

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "tl.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "tl.body", fn_);
        llvm::BasicBlock *growBB = llvm::BasicBlock::Create(*ctx_, "tl.grow", fn_);
        llvm::BasicBlock *storeBB = llvm::BasicBlock::Create(*ctx_, "tl.store", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "tl.end", fn_);

        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(condBB);
        llvm::Value *opt = builder_.CreateCall(nextCallTy, nextFnPtr, {statePtr}, "tl_opt");
        llvm::Value *hasVal = builder_.CreateExtractValue(opt, 0, "tl_has");
        builder_.CreateCondBr(hasVal, bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        llvm::Value *elem = builder_.CreateExtractValue(opt, 1, "tl_elem");
        llvm::Value *curLen = builder_.CreateLoad(i64Ty_, lenVar, "tl_cur_len");
        llvm::Value *curCap = builder_.CreateLoad(i64Ty_, capVar, "tl_cur_cap");
        builder_.CreateCondBr(builder_.CreateICmpEQ(curLen, curCap, "tl_need_grow"), growBB, storeBB);

        builder_.SetInsertPoint(growBB);
        llvm::Value *newCap = builder_.CreateMul(curCap, llvm::ConstantInt::get(i64Ty_, 2), "tl_new_cap");
        builder_.CreateStore(newCap, capVar);
        llvm::Value *newData = builder_.CreateCall(reallocFn, {
            builder_.CreateLoad(ptrTy_, dataVar, "tl_old_data"),
            builder_.CreateMul(newCap, llvm::ConstantInt::get(i64Ty_, elemSize), "tl_new_size")
        }, "tl_new_data");
        builder_.CreateStore(newData, dataVar);
        builder_.CreateBr(storeBB);

        builder_.SetInsertPoint(storeBB);
        llvm::Value *storeLen = builder_.CreateLoad(i64Ty_, lenVar, "tl_store_len");
        llvm::Value *storeData = builder_.CreateLoad(ptrTy_, dataVar, "tl_store_data");
        builder_.CreateStore(elem, builder_.CreateGEP(elemTy, storeData, {storeLen}, "tl_dst_ptr"));
        builder_.CreateStore(
            builder_.CreateAdd(storeLen, llvm::ConstantInt::get(i64Ty_, 1), "tl_new_len"), lenVar);
        builder_.CreateBr(condBB);

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
        llvm::Function *filterNextFn = llvm::Function::Create(
            nextFnTy, llvm::Function::ExternalLinkage, fnName, *mod_);

        {
            FnScope guard(*this);
            fn_ = filterNextFn;
            pushScope();
            llvm::BasicBlock *entry = llvm::BasicBlock::Create(*ctx_, "entry", filterNextFn);
            builder_.SetInsertPoint(entry);

            llvm::Value *statePtr = filterNextFn->getArg(0);
            llvm::Value *srcNextFn = builder_.CreateLoad(ptrTy_,
                builder_.CreateStructGEP(stateTy, statePtr, 0), "src_next_fn");
            llvm::Value *srcState = builder_.CreateLoad(ptrTy_,
                builder_.CreateStructGEP(stateTy, statePtr, 1), "src_state");
            llvm::Value *predPtr = builder_.CreateLoad(ptrTy_,
                builder_.CreateStructGEP(stateTy, statePtr, 2), "pred_ptr");

            llvm::FunctionType *srcNextCallTy = llvm::FunctionType::get(optTy, {ptrTy_}, false);
            llvm::BasicBlock *loopBB = llvm::BasicBlock::Create(*ctx_, "loop", filterNextFn);
            builder_.CreateBr(loopBB);

            builder_.SetInsertPoint(loopBB);
            llvm::Value *opt = builder_.CreateCall(srcNextCallTy, srcNextFn, {srcState}, "src_opt");
            llvm::Value *hasVal = builder_.CreateExtractValue(opt, 0, "has_val");
            llvm::BasicBlock *checkBB = llvm::BasicBlock::Create(*ctx_, "check", filterNextFn);
            llvm::BasicBlock *noneBB = llvm::BasicBlock::Create(*ctx_, "none", filterNextFn);
            builder_.CreateCondBr(hasVal, checkBB, noneBB);

            builder_.SetInsertPoint(checkBB);
            llvm::Value *elem = builder_.CreateExtractValue(opt, 1, "elem");
            llvm::Value *predResult = emitLambdaCall(predPtr, info, {elem}, "pred_result");
            llvm::BasicBlock *matchBB = llvm::BasicBlock::Create(*ctx_, "match", filterNextFn);
            builder_.CreateCondBr(predResult, matchBB, loopBB);

            builder_.SetInsertPoint(matchBB);
            builder_.CreateRet(buildSomeValue(elem, optTy));

            builder_.SetInsertPoint(noneBB);
            builder_.CreateRet(buildNoneValue(optTy));
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

        return emitIteratorHeaderAlloc(*this, builder_, *mod_, iteratorHeaderTy_,
            i64Ty_, mallocFn, filterNextFn, stateAlloc, elemTy,
            iterator_malloc_stack_.back(), "filter_iter");
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
        llvm::Function *mapNextFn = llvm::Function::Create(
            nextFnTy, llvm::Function::ExternalLinkage, fnName, *mod_);

        {
            FnScope guard(*this);
            fn_ = mapNextFn;
            pushScope();
            llvm::BasicBlock *entry = llvm::BasicBlock::Create(*ctx_, "entry", mapNextFn);
            builder_.SetInsertPoint(entry);

            llvm::Value *statePtr = mapNextFn->getArg(0);
            llvm::Value *srcNextFn = builder_.CreateLoad(ptrTy_,
                builder_.CreateStructGEP(stateTy, statePtr, 0), "src_next_fn");
            llvm::Value *srcState = builder_.CreateLoad(ptrTy_,
                builder_.CreateStructGEP(stateTy, statePtr, 1), "src_state");
            llvm::Value *transPtr = builder_.CreateLoad(ptrTy_,
                builder_.CreateStructGEP(stateTy, statePtr, 2), "trans_ptr");

            llvm::FunctionType *srcNextCallTy = llvm::FunctionType::get(srcOptTy, {ptrTy_}, false);
            llvm::Value *opt = builder_.CreateCall(srcNextCallTy, srcNextFn, {srcState}, "src_opt");
            llvm::Value *hasVal = builder_.CreateExtractValue(opt, 0, "has_val");

            llvm::BasicBlock *someBB = llvm::BasicBlock::Create(*ctx_, "some", mapNextFn);
            llvm::BasicBlock *noneBB = llvm::BasicBlock::Create(*ctx_, "none", mapNextFn);
            builder_.CreateCondBr(hasVal, someBB, noneBB);

            builder_.SetInsertPoint(someBB);
            llvm::Value *elem = builder_.CreateExtractValue(opt, 1, "elem");
            builder_.CreateRet(buildSomeValue(emitLambdaCall(transPtr, info, {elem}, "mapped"), outOptTy));

            builder_.SetInsertPoint(noneBB);
            builder_.CreateRet(buildNoneValue(outOptTy));
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

        return emitIteratorHeaderAlloc(*this, builder_, *mod_, iteratorHeaderTy_,
            i64Ty_, mallocFn, mapNextFn, stateAlloc, outElemTy,
            iterator_malloc_stack_.back(), "map_iter");
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
        llvm::Function *takeNextFn = llvm::Function::Create(
            nextFnTy, llvm::Function::ExternalLinkage, fnName, *mod_);

        {
            FnScope guard(*this);
            fn_ = takeNextFn;
            pushScope();
            llvm::BasicBlock *entry = llvm::BasicBlock::Create(*ctx_, "entry", takeNextFn);
            builder_.SetInsertPoint(entry);

            llvm::Value *statePtr = takeNextFn->getArg(0);
            llvm::Value *srcNextFn = builder_.CreateLoad(ptrTy_,
                builder_.CreateStructGEP(stateTy, statePtr, 0), "src_next_fn");
            llvm::Value *srcState = builder_.CreateLoad(ptrTy_,
                builder_.CreateStructGEP(stateTy, statePtr, 1), "src_state");
            llvm::Value *remField = builder_.CreateStructGEP(stateTy, statePtr, 2, "rem_f");
            llvm::Value *remaining = builder_.CreateLoad(i64Ty_, remField, "remaining");

            llvm::BasicBlock *callBB = llvm::BasicBlock::Create(*ctx_, "call", takeNextFn);
            llvm::BasicBlock *noneBB = llvm::BasicBlock::Create(*ctx_, "none", takeNextFn);
            builder_.CreateCondBr(
                builder_.CreateICmpSGT(remaining, llvm::ConstantInt::get(i64Ty_, 0), "has_rem"),
                callBB, noneBB);

            builder_.SetInsertPoint(callBB);
            builder_.CreateStore(
                builder_.CreateSub(remaining, llvm::ConstantInt::get(i64Ty_, 1), "new_rem"), remField);
            llvm::FunctionType *srcNextCallTy = llvm::FunctionType::get(optTy, {ptrTy_}, false);
            builder_.CreateRet(builder_.CreateCall(srcNextCallTy, srcNextFn, {srcState}, "src_opt"));

            builder_.SetInsertPoint(noneBB);
            builder_.CreateRet(buildNoneValue(optTy));
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

        return emitIteratorHeaderAlloc(*this, builder_, *mod_, iteratorHeaderTy_,
            i64Ty_, mallocFn, takeNextFn, stateAlloc, elemTy,
            iterator_malloc_stack_.back(), "take_iter");
    }

    return nullptr;
}

} // namespace ry
