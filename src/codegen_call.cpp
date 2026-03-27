#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"

// ===== Builtin Conversion =====

llvm::Value *CodeGen::emitBuiltinConversion(const CallExpr &e) {
    // to_int(s) → int
    if (e.callee == "to_int") {
        requireArgs(e, 1);
        llvm::Value *s = emitExpr(*e.args[0]);
        if (s->getType() != ptrTy_)
            codegenError("to_int() requires str argument");
        auto atolTy = fnTy_ptr_to_i64_;
        auto atolFn = mod_->getOrInsertFunction("atol", atolTy);
        return builder_.CreateCall(atolFn, {s}, "to_int");
    }

    // to_float(s) → float
    if (e.callee == "to_float") {
        requireArgs(e, 1);
        llvm::Value *s = emitExpr(*e.args[0]);
        if (s->getType() != ptrTy_)
            codegenError("to_float() requires str argument");
        auto atofTy = llvm::FunctionType::get(f64Ty_, {ptrTy_}, false);
        auto atofFn = mod_->getOrInsertFunction("atof", atofTy);
        return builder_.CreateCall(atofFn, {s}, "to_float");
    }

    // to_str(v) → str (int/float/bool/str → str)
    if (e.callee == "to_str") {
        requireArgs(e, 1);
        return valueToString(emitExpr(*e.args[0]));
    }

    return nullptr;
}

// ===== Builtin Query =====

llvm::Value *CodeGen::emitBuiltinQuery(const CallExpr &e) {
    // ===== keys(map) =====
    if (e.callee == "keys") {
        requireArgs(e, 1);
        llvm::Value *mapVal = emitExpr(*e.args[0]);
        llvm::Type *keyTy = getMapKeyType(mapVal);
        if (!keyTy) codegenError("keys() requires a map");

        auto mf = loadMapHeader(mapVal, "keys");
        llvm::Value *mapLen = mf.len;
        llvm::Value *keysData = mf.keys;

        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "keys_header");
        uint64_t elemSize = dl.getTypeAllocSize(keyTy);
        llvm::Value *dataSize = builder_.CreateMul(mapLen, llvm::ConstantInt::get(i64Ty_, elemSize), "keys_ds");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "keys_nd");
        auto memcpyFn = getStdlibMemcpy();
        builder_.CreateCall(memcpyFn, {newData, keysData, dataSize});

        builder_.CreateStore(mapLen, builder_.CreateStructGEP(listHeaderTy_, newHeader, 0));
        builder_.CreateStore(mapLen, builder_.CreateStructGEP(listHeaderTy_, newHeader, 1));
        builder_.CreateStore(newData, builder_.CreateStructGEP(listHeaderTy_, newHeader, 2));
        list_element_types_[newHeader] = keyTy;
        return newHeader;
    }

    // ===== values(map) =====
    if (e.callee == "values") {
        requireArgs(e, 1);
        llvm::Value *mapVal = emitExpr(*e.args[0]);
        llvm::Type *valTy = getMapValueType(mapVal);
        if (!valTy) codegenError("values() requires a map");

        auto mf = loadMapHeader(mapVal, "vals");
        llvm::Value *mapLen = mf.len;
        llvm::Value *valsData = mf.vals;

        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "vals_header");
        uint64_t elemSize = dl.getTypeAllocSize(valTy);
        llvm::Value *dataSize = builder_.CreateMul(mapLen, llvm::ConstantInt::get(i64Ty_, elemSize), "vals_ds");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "vals_nd");
        auto memcpyFn = getStdlibMemcpy();
        builder_.CreateCall(memcpyFn, {newData, valsData, dataSize});

        builder_.CreateStore(mapLen, builder_.CreateStructGEP(listHeaderTy_, newHeader, 0));
        builder_.CreateStore(mapLen, builder_.CreateStructGEP(listHeaderTy_, newHeader, 1));
        builder_.CreateStore(newData, builder_.CreateStructGEP(listHeaderTy_, newHeader, 2));
        list_element_types_[newHeader] = valTy;
        return newHeader;
    }

    // ===== first(list) → Option<T> =====
    if (e.callee == "first") {
        requireArgs(e, 1);
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy) codegenError("first() requires a list");
        llvm::StructType *optTy = getOptionType(elemTy);
        auto lf = loadListHeader(listVal, "first");
        llvm::Value *srcLen = lf.len;
        llvm::Value *srcData = lf.data;

        llvm::Value *isEmptyF = builder_.CreateICmpEQ(srcLen, llvm::ConstantInt::get(i64Ty_, 0), "first_empty");
        llvm::BasicBlock *emptyBB = llvm::BasicBlock::Create(*ctx_, "first.empty", fn_);
        llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "first.ok", fn_);
        llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "first.merge", fn_);
        builder_.CreateCondBr(isEmptyF, emptyBB, okBB);

        builder_.SetInsertPoint(emptyBB);
        llvm::Value *noneVal = buildNoneValue(optTy);
        builder_.CreateBr(mergeBB);
        llvm::BasicBlock *emptyEndBB = builder_.GetInsertBlock();

        builder_.SetInsertPoint(okBB);
        llvm::Value *firstVal = builder_.CreateLoad(elemTy, srcData, "first_val");
        llvm::Value *someVal = buildSomeValue(firstVal, optTy);
        builder_.CreateBr(mergeBB);
        llvm::BasicBlock *okEndBB = builder_.GetInsertBlock();

        builder_.SetInsertPoint(mergeBB);
        llvm::PHINode *phi = builder_.CreatePHI(optTy, 2, "first_result");
        phi->addIncoming(noneVal, emptyEndBB);
        phi->addIncoming(someVal, okEndBB);
        return phi;
    }

    // ===== last(list) → Option<T> =====
    if (e.callee == "last") {
        requireArgs(e, 1);
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy) codegenError("last() requires a list");
        llvm::StructType *optTy = getOptionType(elemTy);
        auto lf = loadListHeader(listVal, "last");
        llvm::Value *srcLen = lf.len;
        llvm::Value *srcData = lf.data;

        llvm::Value *isEmptyL = builder_.CreateICmpEQ(srcLen, llvm::ConstantInt::get(i64Ty_, 0), "last_empty");
        llvm::BasicBlock *emptyBBL = llvm::BasicBlock::Create(*ctx_, "last.empty", fn_);
        llvm::BasicBlock *okBBL = llvm::BasicBlock::Create(*ctx_, "last.ok", fn_);
        llvm::BasicBlock *mergeBBL = llvm::BasicBlock::Create(*ctx_, "last.merge", fn_);
        builder_.CreateCondBr(isEmptyL, emptyBBL, okBBL);

        builder_.SetInsertPoint(emptyBBL);
        llvm::Value *noneValL = buildNoneValue(optTy);
        builder_.CreateBr(mergeBBL);
        llvm::BasicBlock *emptyEndBBL = builder_.GetInsertBlock();

        builder_.SetInsertPoint(okBBL);
        llvm::Value *lastIdx = builder_.CreateSub(srcLen, llvm::ConstantInt::get(i64Ty_, 1), "last_idx");
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, srcData, {lastIdx}, "last_ep");
        llvm::Value *lastVal = builder_.CreateLoad(elemTy, elemPtr, "last_val");
        llvm::Value *someValL = buildSomeValue(lastVal, optTy);
        builder_.CreateBr(mergeBBL);
        llvm::BasicBlock *okEndBBL = builder_.GetInsertBlock();

        builder_.SetInsertPoint(mergeBBL);
        llvm::PHINode *phiL = builder_.CreatePHI(optTy, 2, "last_result");
        phiL->addIncoming(noneValL, emptyEndBBL);
        phiL->addIncoming(someValL, okEndBBL);
        return phiL;
    }

    // ===== is_empty(list/map/set) =====
    if (e.callee == "is_empty") {
        requireArgs(e, 1);
        llvm::Value *val = emitExpr(*e.args[0]);
        llvm::Type *headerTy = nullptr;
        if (getListElementType(val)) headerTy = listHeaderTy_;
        else if (getMapKeyType(val)) headerTy = mapHeaderTy_;
        else if (getSetElementType(val)) headerTy = setHeaderTy_;
        if (!headerTy)
            codegenError("is_empty() requires a collection (list, map, or set)");
        llvm::Value *len = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(headerTy, val, 0), "ie_len");
        return builder_.CreateICmpEQ(len, llvm::ConstantInt::get(i64Ty_, 0), "is_empty");
    }

    // ===== enumerate(list) =====
    if (e.callee == "enumerate") {
        requireArgs(e, 1);
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy) codegenError("enumerate() requires a list");

        auto lf = loadListHeader(listVal, "enum");
        llvm::Value *srcLen = lf.len;
        llvm::Value *srcData = lf.data;

        llvm::StructType *tupleTy = llvm::StructType::get(*ctx_, {i64Ty_, elemTy});
        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "enum_header");
        uint64_t tupleSize = dl.getTypeAllocSize(tupleTy);
        llvm::Value *dataSize = builder_.CreateMul(srcLen, llvm::ConstantInt::get(i64Ty_, tupleSize), "enum_ds");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "enum_nd");

        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "enum_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "enum.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "enum.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "enum.end", fn_);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "ei");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, srcLen), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, srcData, {i}, "enum_ep");
        llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "enum_elem");
        llvm::Value *tupleVal = llvm::UndefValue::get(tupleTy);
        tupleVal = builder_.CreateInsertValue(tupleVal, i, 0);
        tupleVal = builder_.CreateInsertValue(tupleVal, elem, 1);
        llvm::Value *dstPtr = builder_.CreateGEP(tupleTy, newData, {i}, "enum_dp");
        builder_.CreateStore(tupleVal, dstPtr);
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(endBB);

        builder_.CreateStore(srcLen, builder_.CreateStructGEP(listHeaderTy_, newHeader, 0));
        builder_.CreateStore(srcLen, builder_.CreateStructGEP(listHeaderTy_, newHeader, 1));
        builder_.CreateStore(newData, builder_.CreateStructGEP(listHeaderTy_, newHeader, 2));
        list_element_types_[newHeader] = tupleTy;
        return newHeader;
    }

    // ===== zip(list1, list2) =====
    if (e.callee == "zip") {
        requireArgs(e, 2);
        llvm::Value *list1 = emitExpr(*e.args[0]);
        llvm::Value *list2 = emitExpr(*e.args[1]);
        llvm::Type *elemTy1 = getListElementType(list1);
        llvm::Type *elemTy2 = getListElementType(list2);
        if (!elemTy1 || !elemTy2) codegenError("zip() requires two lists");

        auto lf1 = loadListHeader(list1, "zip1");
        auto lf2 = loadListHeader(list2, "zip2");
        llvm::Value *len1 = lf1.len;
        llvm::Value *len2 = lf2.len;
        llvm::Value *data1 = lf1.data;
        llvm::Value *data2 = lf2.data;

        llvm::Value *minLen = builder_.CreateSelect(builder_.CreateICmpSLT(len1, len2), len1, len2, "zip_minlen");

        llvm::StructType *tupleTy = llvm::StructType::get(*ctx_, {elemTy1, elemTy2});
        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "zip_header");
        uint64_t tupleSize = dl.getTypeAllocSize(tupleTy);
        llvm::Value *dataSize = builder_.CreateMul(minLen, llvm::ConstantInt::get(i64Ty_, tupleSize), "zip_ds");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "zip_nd");

        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "zip_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "zip.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "zip.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "zip.end", fn_);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "zi");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, minLen), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *ep1 = builder_.CreateGEP(elemTy1, data1, {i}, "zip_ep1");
        llvm::Value *ep2 = builder_.CreateGEP(elemTy2, data2, {i}, "zip_ep2");
        llvm::Value *e1 = builder_.CreateLoad(elemTy1, ep1, "zip_e1");
        llvm::Value *e2 = builder_.CreateLoad(elemTy2, ep2, "zip_e2");
        llvm::Value *tupleVal = llvm::UndefValue::get(tupleTy);
        tupleVal = builder_.CreateInsertValue(tupleVal, e1, 0);
        tupleVal = builder_.CreateInsertValue(tupleVal, e2, 1);
        llvm::Value *dstPtr = builder_.CreateGEP(tupleTy, newData, {i}, "zip_dp");
        builder_.CreateStore(tupleVal, dstPtr);
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(endBB);

        builder_.CreateStore(minLen, builder_.CreateStructGEP(listHeaderTy_, newHeader, 0));
        builder_.CreateStore(minLen, builder_.CreateStructGEP(listHeaderTy_, newHeader, 1));
        builder_.CreateStore(newData, builder_.CreateStructGEP(listHeaderTy_, newHeader, 2));
        list_element_types_[newHeader] = tupleTy;
        return newHeader;
    }

    return nullptr;
}

// ===== Builtin Core =====

llvm::Value *CodeGen::emitBuiltinCore(const CallExpr &e) {
    if (e.callee.size() > 8 && e.callee.substr(0, 8) == "channel<" && e.callee.back() == '>') {
        if (e.args.size() > 1)
            codegenError("channel[T]() takes 0 or 1 arguments");

        std::string inner = e.callee.substr(8, e.callee.size() - 9);
        llvm::Type *elemTy = resolveType(inner);
        llvm::Value *capacity = llvm::ConstantInt::get(i64Ty_, 0);
        if (e.args.size() == 1) {
            capacity = emitExpr(*e.args[0]);
            if (capacity->getType() != i64Ty_)
                codegenError("channel[T](capacity) requires int capacity");
        }

        const llvm::DataLayout &dl = mod_->getDataLayout();
        int64_t elemSize = elemTy->isVoidTy() ? 0 : static_cast<int64_t>(dl.getTypeAllocSize(elemTy));
        llvm::FunctionType *fnTy = llvm::FunctionType::get(ptrTy_, {i64Ty_, i64Ty_}, false);
        llvm::FunctionCallee fn = mod_->getOrInsertFunction("__ry_channel_new", fnTy);
        llvm::Value *result = builder_.CreateCall(
            fn,
            {llvm::ConstantInt::get(i64Ty_, elemSize), capacity},
            "channel");
        channel_element_types_[result] = elemTy;
        return result;
    }

    // exit(code) as expression — emit exit, then create dead block for subsequent IR
    if (e.callee == "exit") {
        emitExit(e.args);
        llvm::BasicBlock *deadBB = llvm::BasicBlock::Create(*ctx_, "exit.dead", fn_);
        builder_.SetInsertPoint(deadBB);
        return llvm::UndefValue::get(i64Ty_);
    }

    // args() → List<str>
    if (e.callee == "args") {
        if (!e.args.empty())
            codegenError("args() takes no arguments");

        // Call __ry_args_count()
        llvm::FunctionType *countTy = llvm::FunctionType::get(i32Ty_, false);
        llvm::FunctionCallee countFn = mod_->getOrInsertFunction("__ry_args_count", countTy);
        llvm::Value *count32 = builder_.CreateCall(countFn, {}, "argc");
        llvm::Value *count = builder_.CreateSExt(count32, i64Ty_, "argc64");

        // Allocate list header
        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        llvm::Value *headerPtr = builder_.CreateCall(
            mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "args_header");

        // Allocate data array (ptr per element)
        uint64_t elemSize = dl.getTypeAllocSize(ptrTy_);
        llvm::Value *dataSize = builder_.CreateMul(count, llvm::ConstantInt::get(i64Ty_, elemSize), "args_data_size");
        llvm::Value *dataPtr = builder_.CreateCall(mallocFn, {dataSize}, "args_data");

        // Loop: for i in 0..count, get arg pointer
        llvm::Value *zero = llvm::ConstantInt::get(i64Ty_, 0);
        llvm::Value *one = llvm::ConstantInt::get(i64Ty_, 1);
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "args_i");
        builder_.CreateStore(zero, iVar);

        // __ry_args_get function type
        llvm::FunctionType *getTy = llvm::FunctionType::get(ptrTy_, {i32Ty_}, false);
        llvm::FunctionCallee getFn = mod_->getOrInsertFunction("__ry_args_get", getTy);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "args.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "args.body", fn_);
        llvm::BasicBlock *endBB  = llvm::BasicBlock::Create(*ctx_, "args.end", fn_);

        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(condBB);
        llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "ai");
        llvm::Value *cond = builder_.CreateICmpSLT(iVal, count, "args_cond");
        builder_.CreateCondBr(cond, bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "ai_cur");
        llvm::Value *iCur32 = builder_.CreateTrunc(iCur, i32Ty_, "ai_cur32");
        llvm::Value *argStr = builder_.CreateCall(getFn, {iCur32}, "arg_str");
        llvm::Value *elemPtr = builder_.CreateGEP(ptrTy_, dataPtr, {iCur}, "args_elem_ptr");
        builder_.CreateStore(argStr, elemPtr);
        llvm::Value *iNext = builder_.CreateAdd(iCur, one, "ai_next");
        builder_.CreateStore(iNext, iVar);
        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(endBB);

        // Store header fields
        llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 0, "args_len_ptr");
        builder_.CreateStore(count, lenPtr);
        llvm::Value *capPtr = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 1, "args_cap_ptr");
        builder_.CreateStore(count, capPtr);
        llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 2, "args_data_field");
        builder_.CreateStore(dataPtr, dataPtrField);

        list_element_types_[headerPtr] = ptrTy_;
        return headerPtr;
    }

    // available_parallelism() -> int
    if (e.callee == "available_parallelism") {
        if (!e.args.empty())
            codegenError("available_parallelism() takes no arguments");

        llvm::FunctionType *fnTy = llvm::FunctionType::get(i64Ty_, false);
        llvm::FunctionCallee fn = mod_->getOrInsertFunction("__ry_available_parallelism", fnTy);
        return builder_.CreateCall(fn, {}, "available_parallelism");
    }

    // sleep(duration_ms) -> Unit
    if (e.callee == "sleep") {
        requireArgs(e, 1);
        llvm::Value *duration = emitExpr(*e.args[0]);
        if (duration->getType() != i64Ty_)
            codegenError("sleep() requires int argument");

        llvm::FunctionType *fnTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*ctx_), {i64Ty_}, false);
        llvm::FunctionCallee fn = mod_->getOrInsertFunction("__ry_sleep", fnTy);
        return builder_.CreateCall(fn, {duration});
    }

    if (e.callee == "env") {
        if (e.args.empty() || e.args.size() > 2)
            codegenError("env() takes 1 or 2 arguments");
        llvm::Value *key = emitExpr(*e.args[0]);
        if (key->getType() != ptrTy_)
            codegenError("env() key must be str");

        auto getenvTy = llvm::FunctionType::get(ptrTy_, {ptrTy_}, false);
        auto getenvFn = mod_->getOrInsertFunction("getenv", getenvTy);
        llvm::Value *result = builder_.CreateCall(getenvFn, {key}, "env_result");
        llvm::Value *isNull = builder_.CreateICmpEQ(result,
            llvm::ConstantPointerNull::get(
                llvm::cast<llvm::PointerType>(ptrTy_)), "env_null");

        if (e.args.size() == 1) {
            return wrapPtrAsOption(result, "env");
        } else {
            llvm::Value *def = emitExpr(*e.args[1]);
            if (def->getType() != ptrTy_)
                codegenError("env() default must be str");
            return builder_.CreateSelect(isNull, def, result, "env_val");
        }
    }

    if (e.callee == "join" && e.args.size() == 1) {
        llvm::Value *taskVal = emitExpr(*e.args[0]);
        llvm::Type *resultTy = getTaskResultType(taskVal);
        if (!resultTy)
            codegenError("join() requires Task<T>");

        llvm::FunctionType *joinTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*ctx_), {ptrTy_, ptrTy_}, false);
        llvm::FunctionCallee joinFn = mod_->getOrInsertFunction("__ry_task_join", joinTy);
        if (resultTy->isVoidTy())
            return builder_.CreateCall(joinFn, {taskVal, llvm::ConstantPointerNull::get(
                llvm::cast<llvm::PointerType>(ptrTy_))});
        llvm::AllocaInst *resultSlot = builder_.CreateAlloca(resultTy, nullptr, "join_result");
        builder_.CreateCall(joinFn, {taskVal, resultSlot});
        return builder_.CreateLoad(resultTy, resultSlot, "joined");
    }

    if (e.callee == "spawn_in") {
        if (e.args.size() != 2)
            codegenError("spawn_in() requires exactly 2 arguments: spawn_in(group, fn(args...))");

        llvm::Value *groupVal = emitExpr(*e.args[0]);

        auto *innerCallPtr = std::get_if<std::unique_ptr<CallExpr>>(&e.args[1]->data);
        if (!innerCallPtr)
            codegenError("spawn_in() second argument must be a function call, e.g. spawn_in(g, compute(1))");

        task_group_stack_.push_back(groupVal);
        const CallExpr &innerCall = **innerCallPtr;

        llvm::Function *directFunc = nullptr;
        llvm::Value *calleeVal = nullptr;
        FnTypeInfo calleeInfo;
        std::vector<llvm::Value*> argVals;

        auto namedIt = functions_.find(innerCall.callee);
        if (namedIt != functions_.end()) {
            directFunc = resolveOverload(innerCall.callee, innerCall.args, argVals);
        } else if (llvm::AllocaInst *varPtr = findVar(innerCall.callee)) {
            auto fnIt = fn_type_info_.find(varPtr);
            if (fnIt == fn_type_info_.end())
                codegenError("spawn_in requires a function or lambda call");
            calleeInfo = fnIt->second;
            for (auto &arg : innerCall.args)
                argVals.push_back(emitExpr(*arg));
            calleeVal = builder_.CreateLoad(ptrTy_, varPtr, innerCall.callee + ".spawn_fn");
        } else {
            codegenError("spawn_in requires a user-defined function or lambda call");
        }

        llvm::Type *resultTy = directFunc ? directFunc->getReturnType() : calleeInfo.returnType;
        if (resultTy->isVoidTy())
            codegenError("spawn_in does not support Unit-returning calls");

        std::vector<llvm::Type*> envFields;
        if (!directFunc) envFields.push_back(ptrTy_);
        for (auto *argVal : argVals) envFields.push_back(argVal->getType());
        if (envFields.empty()) envFields.push_back(i8Ty_);
        llvm::StructType *envTy = llvm::StructType::get(*ctx_, envFields);

        llvm::FunctionType *mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
        llvm::FunctionCallee mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
        const llvm::DataLayout &dl = mod_->getDataLayout();
        llvm::Value *envPtr = builder_.CreateCall(
            mallocFn,
            {llvm::ConstantInt::get(i64Ty_, std::max<uint64_t>(1, dl.getTypeAllocSize(envTy)))},
            "spawni_env");

        unsigned fieldIndex = 0;
        if (directFunc && argVals.empty()) {
            llvm::Value *df = builder_.CreateStructGEP(envTy, envPtr, 0, "spawni_env_dummy");
            builder_.CreateStore(llvm::ConstantInt::get(i8Ty_, 0), df);
        } else if (!directFunc) {
            llvm::Value *cf = builder_.CreateStructGEP(envTy, envPtr, fieldIndex++, "spawni_env_fn");
            builder_.CreateStore(calleeVal, cf);
        }
        for (size_t i = 0; i < argVals.size(); ++i) {
            llvm::Value *af = builder_.CreateStructGEP(envTy, envPtr, fieldIndex++, "spawni_env_arg." + std::to_string(i));
            builder_.CreateStore(argVals[i], af);
        }

        llvm::FunctionType *thunkTy = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), {ptrTy_, ptrTy_}, false);
        llvm::Function *thunk = llvm::Function::Create(
            thunkTy, llvm::Function::InternalLinkage,
            "__ry_spawn_in." + std::to_string(lambda_counter_++), *mod_);
        {
            FnScope guard(*this);
            fn_ = thunk;
            pushScope();
            llvm::BasicBlock *entry = llvm::BasicBlock::Create(*ctx_, "entry", thunk);
            builder_.SetInsertPoint(entry);
            auto argIt = thunk->arg_begin();
            llvm::Value *envRaw = &*argIt++; envRaw->setName("env_raw");
            llvm::Value *outRaw = &*argIt; outRaw->setName("out_raw");

            std::vector<llvm::Value*> thunkArgs;
            fieldIndex = 0;
            llvm::Value *thunkCallee = nullptr;
            if (!directFunc) {
                llvm::Value *cf = builder_.CreateStructGEP(envTy, envRaw, fieldIndex++, "spawni_fn_field");
                thunkCallee = builder_.CreateLoad(ptrTy_, cf, "spawni_fn");
            }
            for (size_t i = 0; i < argVals.size(); ++i) {
                llvm::Type *argTy = argVals[i]->getType();
                llvm::Value *af = builder_.CreateStructGEP(envTy, envRaw, fieldIndex++, "spawni_arg." + std::to_string(i));
                thunkArgs.push_back(builder_.CreateLoad(argTy, af, "spawni_arg_val." + std::to_string(i)));
            }

            llvm::Value *result = directFunc
                ? builder_.CreateCall(directFunc, thunkArgs, "spawni_call")
                : emitLambdaCall(thunkCallee, calleeInfo, thunkArgs, "spawni_call");
            builder_.CreateStore(result, outRaw);
            builder_.CreateRetVoid();
        }

        llvm::FunctionType *groupSpawnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, ptrTy_, i64Ty_}, false);
        llvm::FunctionCallee groupSpawnFn = mod_->getOrInsertFunction("__ry_task_group_spawn", groupSpawnTy);
        llvm::Value *task = builder_.CreateCall(
            groupSpawnFn,
            {groupVal, builder_.CreateBitCast(thunk, ptrTy_), envPtr,
             llvm::ConstantInt::get(i64Ty_, dl.getTypeAllocSize(resultTy))},
            "task");
        task_result_types_[task] = resultTy;
        task_group_stack_.pop_back();
        return task;
    }

    if (e.callee == "cancel") {
        requireArgs(e, 1);
        llvm::Value *taskVal = emitExpr(*e.args[0]);
        llvm::FunctionType *cancelTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
        llvm::FunctionCallee cancelFn = mod_->getOrInsertFunction("__ry_task_cancel", cancelTy);
        return builder_.CreateCall(cancelFn, {taskVal});
    }

    if (e.callee == "is_cancelled") {
        if (!e.args.empty())
            codegenError("is_cancelled() takes no arguments");
        llvm::FunctionType *isCancelledTy = llvm::FunctionType::get(i1Ty_, {}, false);
        llvm::FunctionCallee isCancelledFn = mod_->getOrInsertFunction("__ry_current_task_is_cancelled", isCancelledTy);
        return builder_.CreateCall(isCancelledFn, {}, "is_cancelled");
    }

    if (e.callee == "send") {
        requireArgs(e, 2);
        llvm::Value *firstArg = emitExpr(*e.args[0]);
        if (isTcpStream(firstArg) || isTlsStream(firstArg)) {
            llvm::Value *data = emitExpr(*e.args[1]);
            if (!getListElementType(data) || getListElementType(data) != i8Ty_)
                codegenError("send() with TcpStream/TlsStream requires List<byte> as second argument");
            auto fnTy = fnTy_ptr_ptr_to_i64_;
            std::string rtFn = isTlsStream(firstArg) ? "__ry_tls_send" : "__ry_tcp_send";
            auto fn = mod_->getOrInsertFunction(rtFn, fnTy);
            llvm::Value *sent = builder_.CreateCall(fn, {firstArg, data}, "tcp_send");
            // Wrap in Result<int, Error>
            llvm::Value *isErr = builder_.CreateICmpSLT(sent,
                llvm::ConstantInt::get(i64Ty_, 0), "send_err");
            llvm::StructType *resTy = getResultType(i64Ty_, errorTy_);
            llvm::Value *okVal = buildOkValue(sent, resTy);
            llvm::Value *errVal = buildErrValue(buildStaticError("send failed", ".send_err_msg"), resTy);
            return builder_.CreateSelect(isErr, errVal, okVal, "send_result");
        }
        llvm::Value *channelVal = firstArg;
        llvm::Type *elemTy = getChannelElementType(channelVal);
        if (!elemTy)
            codegenError("send() requires Channel<T> or TcpStream as first argument");
        llvm::Value *valueVal = emitExpr(*e.args[1]);
        if (valueVal->getType() != elemTy)
            codegenError("send() value type does not match channel element type");

        llvm::Value *valuePtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));
        if (!elemTy->isVoidTy()) {
            llvm::AllocaInst *valueSlot = builder_.CreateAlloca(elemTy, nullptr, "send_value");
            builder_.CreateStore(valueVal, valueSlot);
            valuePtr = valueSlot;
        }

        llvm::FunctionType *fnTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*ctx_), {ptrTy_, ptrTy_}, false);
        llvm::FunctionCallee fn = mod_->getOrInsertFunction("__ry_channel_send", fnTy);
        return builder_.CreateCall(fn, {channelVal, valuePtr});
    }

    if (e.callee == "try_send") {
        requireArgs(e, 2);
        llvm::Value *channelVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getChannelElementType(channelVal);
        if (!elemTy)
            codegenError("try_send() requires Channel<T> as first argument");
        llvm::Value *valueVal = emitExpr(*e.args[1]);
        if (valueVal->getType() != elemTy)
            codegenError("try_send() value type does not match channel element type");

        llvm::Value *valuePtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));
        if (!elemTy->isVoidTy()) {
            llvm::AllocaInst *valueSlot = builder_.CreateAlloca(elemTy, nullptr, "try_send_value");
            builder_.CreateStore(valueVal, valueSlot);
            valuePtr = valueSlot;
        }

        llvm::FunctionType *fnTy = llvm::FunctionType::get(i1Ty_, {ptrTy_, ptrTy_}, false);
        llvm::FunctionCallee fn = mod_->getOrInsertFunction("__ry_channel_try_send", fnTy);
        return builder_.CreateCall(fn, {channelVal, valuePtr}, "try_send_ok");
    }

    if (e.callee == "recv") {
        if (e.args.size() == 2) {
            // TCP/TLS recv(stream, max_bytes) -> Result<List<byte>, Error>
            llvm::Value *streamVal = emitExpr(*e.args[0]);
            if (!isTcpStream(streamVal) && !isTlsStream(streamVal))
                codegenError("recv() with 2 arguments requires TcpStream or TlsStream as first argument");
            llvm::Value *maxBytes = emitExpr(*e.args[1]);
            auto fnTy = fnTy_ptr_i64_to_ptr_;
            std::string rtFn = isTlsStream(streamVal) ? "__ry_tls_recv" : "__ry_tcp_recv";
            auto fn = mod_->getOrInsertFunction(rtFn, fnTy);
            llvm::Value *ptr = builder_.CreateCall(fn, {streamVal, maxBytes}, "tcp_recv");
            // Wrap in Result<List<byte>, Error>: nullptr = Err, non-null = Ok
            llvm::Value *isNull = builder_.CreateICmpEQ(ptr,
                llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)), "recv_null");
            llvm::StructType *resTy = getResultType(ptrTy_, errorTy_);
            llvm::Value *okVal = buildOkValue(ptr, resTy);
            llvm::Value *errVal = buildErrValue(buildStaticError("recv failed", ".recv_err_msg"), resTy);
            llvm::Value *result = builder_.CreateSelect(isNull, errVal, okVal, "recv_result");
            list_element_types_[result] = i8Ty_;
            return result;
        }
        if (e.args.size() != 1)
            codegenError("recv() takes 1 or 2 arguments");
        llvm::Value *channelVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getChannelElementType(channelVal);
        if (!elemTy)
            codegenError("recv() requires Channel<T> argument");

        llvm::FunctionType *fnTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*ctx_), {ptrTy_, ptrTy_}, false);
        llvm::FunctionCallee fn = mod_->getOrInsertFunction("__ry_channel_recv", fnTy);
        if (elemTy->isVoidTy()) {
            return builder_.CreateCall(fn, {
                channelVal,
                llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_))
            });
        }

        llvm::AllocaInst *resultSlot = builder_.CreateAlloca(elemTy, nullptr, "recv_result");
        builder_.CreateCall(fn, {channelVal, resultSlot});
        return builder_.CreateLoad(elemTy, resultSlot, "received");
    }

    if (e.callee == "recv_opt") {
        requireArgs(e, 1);
        llvm::Value *channelVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getChannelElementType(channelVal);
        if (!elemTy)
            codegenError("recv_opt() requires Channel<T> argument");

        llvm::FunctionType *fnTy = llvm::FunctionType::get(i1Ty_, {ptrTy_, ptrTy_}, false);
        llvm::FunctionCallee fn = mod_->getOrInsertFunction("__ry_channel_recv_opt", fnTy);
        llvm::Value *outPtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));
        llvm::AllocaInst *resultSlot = nullptr;
        if (!elemTy->isVoidTy()) {
            resultSlot = builder_.CreateAlloca(elemTy, nullptr, "recv_opt_result");
            builder_.CreateStore(llvm::Constant::getNullValue(elemTy), resultSlot);
            outPtr = resultSlot;
        }

        llvm::Value *hasValue = builder_.CreateCall(fn, {channelVal, outPtr}, "recv_opt_has_value");
        if (elemTy->isVoidTy())
            return hasValue;

        llvm::StructType *optTy = getOptionType(elemTy);
        llvm::Value *inner = builder_.CreateLoad(elemTy, resultSlot, "recv_opt_loaded");
        llvm::Value *optInner = builder_.CreateSelect(hasValue, inner, llvm::UndefValue::get(elemTy), "recv_opt_inner");
        llvm::Value *opt = llvm::UndefValue::get(optTy);
        opt = builder_.CreateInsertValue(opt, hasValue, 0, "recv_opt_has");
        opt = builder_.CreateInsertValue(opt, optInner, 1, "recv_opt_value");
        return opt;
    }

    if (e.callee == "try_recv") {
        requireArgs(e, 1);
        llvm::Value *channelVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getChannelElementType(channelVal);
        if (!elemTy)
            codegenError("try_recv() requires Channel<T> argument");

        llvm::FunctionType *fnTy = llvm::FunctionType::get(i1Ty_, {ptrTy_, ptrTy_}, false);
        llvm::FunctionCallee fn = mod_->getOrInsertFunction("__ry_channel_try_recv", fnTy);
        llvm::Value *outPtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));
        llvm::AllocaInst *resultSlot = nullptr;
        if (!elemTy->isVoidTy()) {
            resultSlot = builder_.CreateAlloca(elemTy, nullptr, "try_recv_result");
            builder_.CreateStore(llvm::Constant::getNullValue(elemTy), resultSlot);
            outPtr = resultSlot;
        }

        llvm::Value *hasValue = builder_.CreateCall(fn, {channelVal, outPtr}, "try_recv_has_value");
        if (elemTy->isVoidTy())
            return hasValue;

        llvm::StructType *optTy = getOptionType(elemTy);
        llvm::Value *inner = builder_.CreateLoad(elemTy, resultSlot, "try_recv_loaded");
        llvm::Value *optInner = builder_.CreateSelect(hasValue, inner, llvm::UndefValue::get(elemTy), "try_recv_inner");
        llvm::Value *opt = llvm::UndefValue::get(optTy);
        opt = builder_.CreateInsertValue(opt, hasValue, 0, "try_recv_has");
        opt = builder_.CreateInsertValue(opt, optInner, 1, "try_recv_value");
        return opt;
    }

    if (e.callee == "close") {
        requireArgs(e, 1);
        llvm::Value *val = emitExpr(*e.args[0]);
        auto *voidPtrFnTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
        if (isTcpListener(val)) {
            auto fn = mod_->getOrInsertFunction("__ry_tcp_listener_close", voidPtrFnTy);
            return builder_.CreateCall(fn, {val});
        }
        if (isTcpStream(val)) {
            auto fn = mod_->getOrInsertFunction("__ry_tcp_close", voidPtrFnTy);
            return builder_.CreateCall(fn, {val});
        }
        if (isTlsStream(val)) {
            auto fn = mod_->getOrInsertFunction("__ry_tls_close", voidPtrFnTy);
            return builder_.CreateCall(fn, {val});
        }
        if (!getChannelElementType(val))
            codegenError("close() requires Channel<T>, TcpStream, TlsStream, or TcpListener argument");
        auto fn = mod_->getOrInsertFunction("__ry_channel_close", voidPtrFnTy);
        return builder_.CreateCall(fn, {val});
    }

    // range(n), range(start, end), or range(start, end, step) → List<int>
    if (e.callee == "range") {
        if (e.args.size() < 1 || e.args.size() > 3)
            codegenError("range() takes 1, 2, or 3 arguments");

        llvm::Value *start, *end, *step;
        llvm::Value *zero = llvm::ConstantInt::get(i64Ty_, 0);
        llvm::Value *one = llvm::ConstantInt::get(i64Ty_, 1);

        if (e.args.size() == 1) {
            start = zero;
            end = emitExpr(*e.args[0]);
            step = one;
        } else if (e.args.size() == 2) {
            start = emitExpr(*e.args[0]);
            end = emitExpr(*e.args[1]);
            step = one;
        } else {
            start = emitExpr(*e.args[0]);
            end = emitExpr(*e.args[1]);
            step = emitExpr(*e.args[2]);
        }

        // Runtime check: step == 0 → error
        if (e.args.size() == 3) {
            llvm::Value *stepZero = builder_.CreateICmpEQ(step, zero, "step_zero");
            llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, "range.step_err", fn_);
            llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "range.step_ok", fn_);
            builder_.CreateCondBr(stepZero, errBB, okBB);
            builder_.SetInsertPoint(errBB);
            emitRuntimeError("runtime error: range() step must not be zero\n", ".range_step_err");
            builder_.SetInsertPoint(okBB);
        }

        // Compute count based on step sign
        // step > 0: count = max(0, (end - start + step - 1) / step)
        // step < 0: count = max(0, (start - end + (-step) - 1) / (-step))
        llvm::Value *stepPos = builder_.CreateICmpSGT(step, zero, "step_pos");

        // Positive step case
        llvm::Value *diffPos = builder_.CreateSub(end, start, "diff_pos");
        llvm::Value *numPos = builder_.CreateAdd(diffPos, builder_.CreateSub(step, one, "step_m1"), "num_pos");
        llvm::Value *countPos = builder_.CreateSDiv(numPos, step, "count_pos");
        llvm::Value *countPosClamped = builder_.CreateSelect(
            builder_.CreateICmpSGT(countPos, zero, "pos_gt0"), countPos, zero, "count_pos_c");

        // Negative step case
        llvm::Value *negStep = builder_.CreateNeg(step, "neg_step");
        llvm::Value *diffNeg = builder_.CreateSub(start, end, "diff_neg");
        llvm::Value *numNeg = builder_.CreateAdd(diffNeg, builder_.CreateSub(negStep, one, "negstep_m1"), "num_neg");
        llvm::Value *countNeg = builder_.CreateSDiv(numNeg, negStep, "count_neg");
        llvm::Value *countNegClamped = builder_.CreateSelect(
            builder_.CreateICmpSGT(countNeg, zero, "neg_gt0"), countNeg, zero, "count_neg_c");

        llvm::Value *count = builder_.CreateSelect(stepPos, countPosClamped, countNegClamped, "range_count");

        // Allocate list header
        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        llvm::Value *headerPtr = builder_.CreateCall(
            mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "range_header");

        // Allocate data array
        uint64_t elemSize = dl.getTypeAllocSize(i64Ty_);
        llvm::Value *dataSize = builder_.CreateMul(count, llvm::ConstantInt::get(i64Ty_, elemSize), "range_data_size");
        llvm::Value *dataPtr = builder_.CreateCall(mallocFn, {dataSize}, "range_data");

        // Fill data with start, start+step, start+2*step, ... using a loop
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "range_i");
        builder_.CreateStore(zero, iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "range.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "range.body", fn_);
        llvm::BasicBlock *endBB  = llvm::BasicBlock::Create(*ctx_, "range.end", fn_);

        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(condBB);
        llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "ri");
        llvm::Value *cond = builder_.CreateICmpSLT(iVal, count, "range_cond");
        builder_.CreateCondBr(cond, bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "ri_cur");
        llvm::Value *offset = builder_.CreateMul(iCur, step, "range_offset");
        llvm::Value *val = builder_.CreateAdd(start, offset, "range_val");
        llvm::Value *elemPtr = builder_.CreateGEP(i64Ty_, dataPtr, {iCur}, "range_elem_ptr");
        builder_.CreateStore(val, elemPtr);
        llvm::Value *iNext = builder_.CreateAdd(iCur, one, "ri_next");
        builder_.CreateStore(iNext, iVar);
        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(endBB);

        // Store header fields
        llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 0, "range_len_ptr");
        builder_.CreateStore(count, lenPtr);
        llvm::Value *capPtr = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 1, "range_cap_ptr");
        builder_.CreateStore(count, capPtr);
        llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 2, "range_data_field");
        builder_.CreateStore(dataPtr, dataPtrField);

        list_element_types_[headerPtr] = i64Ty_;
        return headerPtr;
    }

    // length(xs) → list/map length
    if (e.callee == "length") {
        requireArgs(e, 1);
        llvm::Value *ptr = emitExpr(*e.args[0]);
        if (ptr->getType() != ptrTy_)
            codegenError("length() requires list, map, or str argument");
        // Check if it's a set
        if (getSetElementType(ptr))
            return loadSetHeader(ptr, "set").len;
        // Check if it's a map
        llvm::Type *mapKeyTy = getMapKeyType(ptr);
        if (mapKeyTy)
            return loadMapHeader(ptr, "map").len;
        // Check if it's a list
        if (getListElementType(ptr))
            return loadListHeader(ptr, "list").len;
        // String: call __ry_utf8_len (character count)
        auto utf8LenTy = fnTy_ptr_to_i64_;
        auto utf8LenFn = mod_->getOrInsertFunction("__ry_utf8_len", utf8LenTy);
        return builder_.CreateCall(utf8LenFn, {ptr}, "str_len");
    }

    // byte_len(str) → int (byte length)
    if (e.callee == "byte_len") {
        requireArgs(e, 1);
        llvm::Value *ptr = emitExpr(*e.args[0]);
        if (ptr->getType() != ptrTy_)
            codegenError("byte_len() requires str argument");
        auto strlenFn = getStdlibStrlen();
        return builder_.CreateCall(strlenFn, {ptr}, "byte_len");
    }

    // Some(x) → Option<T> constructor
    if (e.callee == "Some") {
        requireArgs(e, 1);
        llvm::Value *inner = emitExpr(*e.args[0]);
        llvm::StructType *optTy = getOptionType(inner->getType());
        return buildSomeValue(inner, optTy);
    }

    // Ok(value) → Result<V, Error> constructor
    if (e.callee == "Ok") {
        requireArgs(e, 1);
        llvm::Value *inner = emitExpr(*e.args[0]);
        // Determine the error type from the enclosing function's return type
        llvm::Type *errTy = errorTy_;
        if (fn_) {
            llvm::Type *retTy = fn_->getReturnType();
            if (isResultType(retTy)) {
                auto *retStructTy = llvm::cast<llvm::StructType>(retTy);
                errTy = retStructTy->getElementType(2);
            }
        }
        llvm::StructType *resTy = getResultType(inner->getType(), errTy);
        return buildOkValue(inner, resTy);
    }

    // Err(error) → Result<V, E> constructor
    if (e.callee == "Err") {
        requireArgs(e, 1);
        llvm::Value *inner = emitExpr(*e.args[0]);
        // Determine the ok type from the enclosing function's return type
        llvm::Type *okTy = i8Ty_; // default: Unit (i8 dummy)
        if (fn_) {
            llvm::Type *retTy = fn_->getReturnType();
            if (isResultType(retTy)) {
                auto *retStructTy = llvm::cast<llvm::StructType>(retTy);
                okTy = retStructTy->getElementType(1);
            }
        }
        llvm::StructType *resTy = getResultType(okTy, inner->getType());
        return buildErrValue(inner, resTy);
    }

    // Error("msg") / Error("msg", code) → Error struct constructor
    if (e.callee == "Error") {
        if (e.args.empty() || e.args.size() > 2)
            codegenError("Error() takes 1 or 2 arguments");
        llvm::Value *msg = emitExpr(*e.args[0]);
        if (msg->getType() != ptrTy_)
            codegenError("Error() first argument must be a string");
        llvm::Value *code;
        if (e.args.size() == 2) {
            code = emitExpr(*e.args[1]);
            if (code->getType() != i64Ty_)
                codegenError("Error() second argument must be an integer");
        } else {
            code = llvm::ConstantInt::get(i64Ty_, 0);
        }
        llvm::Value *result = llvm::UndefValue::get(errorTy_);
        result = builder_.CreateInsertValue(result, msg, 0, "err.msg");
        result = builder_.CreateInsertValue(result, code, 1, "err.code");
        return result;
    }

    // unwrap() has been removed — use match or ?? instead
    if (e.callee == "unwrap") {
        codegenError("unwrap() has been removed. Use match or ?? instead");
    }

    // has_key(map, key) → bool
    if (e.callee == "has_key") {
        requireArgs(e, 2);
        llvm::Value *mapPtr = emitExpr(*e.args[0]);
        if (mapPtr->getType() != ptrTy_)
            codegenError("has_key() requires map as first argument");
        llvm::Type *keyTy = getMapKeyType(mapPtr);
        if (!keyTy)
            codegenError("has_key() requires map as first argument");
        llvm::Value *key = emitExpr(*e.args[1]);
        if (key->getType() != keyTy)
            codegenError("has_key() key type mismatch");
        llvm::Value *idx = emitMapKeyLookup(mapPtr, key, keyTy);
        return builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "has_key");
    }


    return nullptr;
}


// ===== CallExpr Dispatcher =====

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<CallExpr> &e) {
    // ADT constructor: Enum::Variant(args...)
    {
        auto colonPos = e->callee.find("::");
        if (colonPos != std::string::npos) {
            std::string enumName = e->callee.substr(0, colonPos);
            std::string variantName = e->callee.substr(colonPos + 2);
            // Try to instantiate generic enum if not found
            if (!enum_types_.count(enumName)) {
                auto ltPos = enumName.find('<');
                if (ltPos != std::string::npos && enumName.back() == '>') {
                    std::string baseName = enumName.substr(0, ltPos);
                    std::string argsStr = enumName.substr(ltPos + 1, enumName.size() - ltPos - 2);
                    std::vector<std::string> typeArgs;
                    std::string curr;
                    int depth = 0;
                    for (char c : argsStr) {
                        if (c == '<') depth++;
                        else if (c == '>') depth--;
                        else if (c == ',' && depth == 0) {
                            typeArgs.push_back(curr);
                            curr.clear();
                            continue;
                        }
                        curr += c;
                    }
                    if (!curr.empty()) typeArgs.push_back(curr);
                    instantiateGenericEnum(enumName, baseName, typeArgs);
                }
            }
            auto eit = enum_types_.find(enumName);
            if (eit != enum_types_.end() && eit->second.isADT) {
                auto &info = eit->second;
                auto vit = info.variants.find(variantName);
                if (vit == info.variants.end())
                    codegenError("unknown variant '" + variantName + "' in enum '" + enumName + "'");
                int64_t tag = vit->second;

                auto fit = info.variantFields.find(variantName);
                if (fit == info.variantFields.end())
                    codegenError("variant '" + variantName + "' has no associated data");
                auto &fieldInfo = fit->second;
                if (e->args.size() != fieldInfo.fieldTypes.size())
                    codegenError("variant '" + variantName + "' expects " +
                        std::to_string(fieldInfo.fieldTypes.size()) + " arguments");

                llvm::Value *adtVal = llvm::UndefValue::get(info.adtType);
                adtVal = builder_.CreateInsertValue(adtVal, llvm::ConstantInt::get(i64Ty_, tag), 0, "adt.tag");

                const llvm::DataLayout &dl = mod_->getDataLayout();
                llvm::AllocaInst *tmpAlloca = builder_.CreateAlloca(info.adtType, nullptr, "adt.tmp");
                builder_.CreateStore(adtVal, tmpAlloca);
                llvm::Value *payloadPtr = builder_.CreateStructGEP(info.adtType, tmpAlloca, 1, "adt.payload");

                size_t offset = 0;
                for (size_t i = 0; i < e->args.size(); ++i) {
                    llvm::Value *argVal = emitExpr(*e->args[i]);
                    uint64_t align = dl.getABITypeAlign(fieldInfo.fieldTypes[i]).value();
                    offset = (offset + align - 1) / align * align;
                    llvm::Value *fieldPtr = builder_.CreateGEP(
                        llvm::Type::getInt8Ty(*ctx_), payloadPtr,
                        {llvm::ConstantInt::get(i64Ty_, offset)}, "adt.field." + std::to_string(i));
                    builder_.CreateStore(argVal, fieldPtr);
                    offset += dl.getTypeAllocSize(fieldInfo.fieldTypes[i]);
                }

                llvm::Value *result = builder_.CreateLoad(info.adtType, tmpAlloca, "adt.val");
                enum_value_types_[result] = enumName;
                return result;
            }
        }
    }

    // verify(fn_name) → call count
    if (e->callee == "verify") {
        if (!test_mode_)
            codegenError("'verify' is only allowed in test mode (use 'ry test')");
        if (e->args.size() != 1)
            codegenError("verify() requires exactly 1 argument");
        auto *strExpr = std::get_if<StringExpr>(&e->args[0]->data);
        if (!strExpr)
            codegenError("verify() argument must be a function name");
        auto vit = functions_.find(strExpr->value);
        if (vit == functions_.end())
            codegenError("verify(): unknown function '" + strExpr->value + "'");
        if (vit->second.size() != 1)
            codegenError("verify(): overloaded functions are not supported");
        auto *getCountTy = fnTy_ptr_to_i64_;
        llvm::FunctionCallee getCountFn = mod_->getOrInsertFunction("__ry_mock_get_call_count", getCountTy);
        llvm::Value *nameStr = cachedGlobalString(strExpr->value, ".verify_name");
        return builder_.CreateCall(getCountFn, {nameStr}, "call_count");
    }

    // Dispatch to language-builtin helpers (Pattern B: no @native registry)
    if (auto *v = emitBuiltinIterator(*e))    return v;
    if (auto *v = emitBuiltinString(*e))      return v;
    if (auto *v = emitBuiltinConversion(*e))  return v;
    if (auto *v = emitBuiltinQuery(*e))       return v;
    if (auto *v = emitBuiltinCore(*e))        return v;
    if (auto *v = emitBuiltinHigherOrder(*e)) return v;
    if (auto *v = emitBuiltinCollection(*e))  return v;
    if (auto *v = emitBuiltinSetOps(*e))      return v;
    if (auto *v = emitBuiltinRegex(*e))       return v;

    // Dispatch to stdlib package helpers (Pattern A: @native registry guard)
    // To add a new stdlib package, add its emitBuiltin method here.
    using StdlibDispatcher = llvm::Value *(CodeGen::*)(const CallExpr &);
    static const StdlibDispatcher stdlib_dispatchers[] = {
        &CodeGen::emitBuiltinMath,
        &CodeGen::emitBuiltinIO,
        &CodeGen::emitBuiltinNet,
        &CodeGen::emitBuiltinHttp,
        &CodeGen::emitBuiltinJson,
        &CodeGen::emitBuiltinBase64,
    };
    for (auto dispatcher : stdlib_dispatchers) {
        if (auto *v = (this->*dispatcher)(*e)) return v;
    }

    // Struct constructor
    auto sit = struct_types_.find(e->callee);
    if (sit != struct_types_.end()) {
        if (deprecated_types_.count(e->callee))
            emitDeprecationWarning(e->callee);
        return emitStructConstructor(sit->second, e->callee, e->args);
    }

    // Try indirect call via variable (function pointer / lambda)
    if (llvm::AllocaInst *varPtr = findVar(e->callee)) {
        auto fnIt = fn_type_info_.find(varPtr);
        if (fnIt != fn_type_info_.end()) {
            auto &info = fnIt->second;

            std::vector<llvm::Value*> argVals;
            for (auto &arg : e->args)
                argVals.push_back(emitExpr(*arg));
            llvm::Value *loaded = builder_.CreateLoad(ptrTy_, varPtr, e->callee + ".fn");
            return emitLambdaCall(loaded, info, argVals, "indirect_call");
        }
    }

    // Generic function dispatch (explicit type args or type inference)
    {
        std::string baseName = e->callee;
        std::vector<std::string> typeArgs;

        auto ltPos = baseName.find('<');
        if (ltPos != std::string::npos && baseName.back() == '>') {
            std::string argsStr = baseName.substr(ltPos + 1, baseName.size() - ltPos - 2);
            baseName = baseName.substr(0, ltPos);
            std::string curr;
            int depth = 0;
            for (char c : argsStr) {
                if (c == '<') depth++;
                else if (c == '>') depth--;
                else if (c == ',' && depth == 0) {
                    typeArgs.push_back(curr);
                    curr.clear();
                    continue;
                }
                curr += c;
            }
            if (!curr.empty()) typeArgs.push_back(curr);
        }

        if (generic_fn_templates_.count(baseName)) {
            if (typeArgs.empty() && !functions_.count(baseName))
                typeArgs = inferTypeArgs(baseName, e->args);
            if (!typeArgs.empty()) {
                instantiateGenericFn(baseName, typeArgs);
                // Build fullName matching instantiateGenericFn's key format
                std::string fullName = baseName + "<";
                for (size_t i = 0; i < typeArgs.size(); ++i) {
                    if (i > 0) fullName += ",";
                    fullName += typeArgs[i];
                }
                fullName += ">";
                return emitUserFnCall(fullName, e->args);
            }
        }
    }

    return emitUserFnCall(e->callee, e->args);
}

// ===== Lambda call helper =====

std::vector<llvm::Value*> CodeGen::coerceCallArgs(const FnTypeInfo &info,
                                                  std::vector<llvm::Value*> args,
                                                  const std::string &context) {
    if (args.size() != info.paramTypes.size()) {
        codegenError(
            context + ": expected " + std::to_string(info.paramTypes.size()) +
            " arguments, got " + std::to_string(args.size()));
    }

    for (size_t i = 0; i < args.size(); ++i) {
        if (args[i]->getType() == info.paramTypes[i])
            continue;

        if (isAnyType(info.paramTypes[i])) {
            args[i] = wrapInAny(args[i]);
            continue;
        }

        if (isAnyType(args[i]->getType()) && canAnyHoldType(info.paramTypes[i])) {
            args[i] = unwrapFromAny(args[i], info.paramTypes[i]);
            continue;
        }

        if (i < info.paramTypeNames.size() && isUnionType(info.paramTypeNames[i])) {
            args[i] = wrapInUnion(args[i], info.paramTypeNames[i]);
            continue;
        }

        codegenError(context + ": argument " + std::to_string(i) + " type mismatch");
    }

    return args;
}

llvm::Value *CodeGen::emitLambdaCall(llvm::Value *lambdaVal, const FnTypeInfo &info,
                                      std::vector<llvm::Value*> args, const std::string &name) {
    args = coerceCallArgs(info, std::move(args), "lambda call");

    if (info.capturedVars.empty()) {
        llvm::FunctionType *ft = llvm::FunctionType::get(
            info.returnType, info.paramTypes, false);
        if (info.returnType->isVoidTy())
            return builder_.CreateCall(ft, lambdaVal, args);
        return builder_.CreateCall(ft, lambdaVal, args, name);
    } else {
        std::vector<llvm::Type*> closureFields;
        closureFields.push_back(ptrTy_);
        for (auto *ct : info.capturedTypes)
            closureFields.push_back(ct);
        llvm::StructType *closureTy = llvm::StructType::get(*ctx_, closureFields);

        llvm::Value *fnPtrField = builder_.CreateStructGEP(
            closureTy, lambdaVal, 0, "lcall.fn_ptr");
        llvm::Value *fnPtr = builder_.CreateLoad(ptrTy_, fnPtrField, "lcall.fn");

        std::vector<llvm::Value*> fullArgs = args;
        std::vector<llvm::Type*> allParamTypes = info.paramTypes;
        for (size_t i = 0; i < info.capturedTypes.size(); ++i) {
            llvm::Value *capField = builder_.CreateStructGEP(
                closureTy, lambdaVal, i + 1, "lcall.cap." + std::to_string(i));
            llvm::Value *capVal = builder_.CreateLoad(
                info.capturedTypes[i], capField, "lcall.cap_val." + std::to_string(i));
            fullArgs.push_back(capVal);
            allParamTypes.push_back(info.capturedTypes[i]);
        }

        llvm::FunctionType *ft = llvm::FunctionType::get(
            info.returnType, allParamTypes, false);
        if (info.returnType->isVoidTy())
            return builder_.CreateCall(ft, fnPtr, fullArgs);
        return builder_.CreateCall(ft, fnPtr, fullArgs, name);
    }
}


// ===== Shared Result-wrapping helpers =====

llvm::Value *CodeGen::emitResultBranch(llvm::Value *isErr, llvm::StructType *resTy,
                                        std::function<llvm::Value*()> buildOk,
                                        std::function<llvm::Value*()> buildErr) {
    llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "res.ok", fn_);
    llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, "res.err", fn_);
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "res.merge", fn_);
    builder_.CreateCondBr(isErr, errBB, okBB);

    builder_.SetInsertPoint(okBB);
    llvm::Value *okVal = buildOk();
    builder_.CreateBr(mergeBB);
    okBB = builder_.GetInsertBlock();

    builder_.SetInsertPoint(errBB);
    llvm::Value *errVal = buildErr();
    builder_.CreateBr(mergeBB);
    errBB = builder_.GetInsertBlock();

    builder_.SetInsertPoint(mergeBB);
    llvm::PHINode *phi = builder_.CreatePHI(resTy, 2, "result");
    phi->addIncoming(okVal, okBB);
    phi->addIncoming(errVal, errBB);
    return phi;
}

llvm::Value *CodeGen::buildErrorFromRuntime(const char *errFnName) {
    auto errFnTy = llvm::FunctionType::get(ptrTy_, {}, false);
    auto errFn = mod_->getOrInsertFunction(errFnName, errFnTy);
    llvm::Value *errMsg = builder_.CreateCall(errFn, {}, "err_msg");
    llvm::Value *errStruct = llvm::UndefValue::get(errorTy_);
    errStruct = builder_.CreateInsertValue(errStruct, errMsg, 0, "err.msg");
    errStruct = builder_.CreateInsertValue(errStruct, llvm::ConstantInt::get(i64Ty_, 0), 1, "err.code");
    return errStruct;
}

llvm::Value *CodeGen::wrapPtrAsResult(llvm::Value *ptr, const char *errFnName) {
    llvm::StructType *resTy = getResultType(ptrTy_, errorTy_);
    llvm::Value *isNull = builder_.CreateICmpEQ(ptr,
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)), "is_null");
    return emitResultBranch(isNull, resTy,
        [&]() { return buildOkValue(ptr, resTy); },
        [&]() { return buildErrValue(buildErrorFromRuntime(errFnName), resTy); });
}

llvm::Value *CodeGen::wrapStatusAsResult(llvm::Value *status, const char *errFnName) {
    llvm::StructType *resTy = getResultType(i8Ty_, errorTy_);
    llvm::Value *isErr = builder_.CreateICmpNE(status,
        llvm::ConstantInt::get(i64Ty_, 0), "is_err");
    return emitResultBranch(isErr, resTy,
        [&]() { return buildOkValue(llvm::ConstantInt::get(i8Ty_, 0), resTy); },
        [&]() { return buildErrValue(buildErrorFromRuntime(errFnName), resTy); });
}

// ===== Codegen helpers =====

void CodeGen::requireArgs(const CallExpr &e, size_t expected) {
    requireArgs(e.callee, e.args.size(), expected);
}

void CodeGen::requireArgs(const std::string &callee, size_t actual, size_t expected) {
    if (actual != expected)
        codegenError(callee + "() takes exactly " + std::to_string(expected) +
                     " argument" + (expected == 1 ? "" : "s"));
}

CodeGen::ListFields CodeGen::loadListHeader(llvm::Value *listVal, const std::string &prefix) {
    ListFields f;
    llvm::Twine p(prefix);
    f.lenPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 0, p + "_len_ptr");
    f.len = builder_.CreateLoad(i64Ty_, f.lenPtr, p + "_len");
    f.capPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 1, p + "_cap_ptr");
    f.cap = builder_.CreateLoad(i64Ty_, f.capPtr, p + "_cap");
    f.dataPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 2, p + "_data_ptr");
    f.data = builder_.CreateLoad(ptrTy_, f.dataPtr, p + "_data");
    return f;
}

CodeGen::SetFields CodeGen::loadSetHeader(llvm::Value *setVal, const std::string &prefix) {
    SetFields f;
    llvm::Twine p(prefix);
    f.lenPtr = builder_.CreateStructGEP(setHeaderTy_, setVal, 0, p + "_len_ptr");
    f.len = builder_.CreateLoad(i64Ty_, f.lenPtr, p + "_len");
    f.capPtr = builder_.CreateStructGEP(setHeaderTy_, setVal, 1, p + "_cap_ptr");
    f.cap = builder_.CreateLoad(i64Ty_, f.capPtr, p + "_cap");
    f.elemsPtr = builder_.CreateStructGEP(setHeaderTy_, setVal, 2, p + "_elems_ptr");
    f.elems = builder_.CreateLoad(ptrTy_, f.elemsPtr, p + "_elems");
    return f;
}

CodeGen::MapFields CodeGen::loadMapHeader(llvm::Value *mapVal, const std::string &prefix) {
    MapFields f;
    llvm::Twine p(prefix);
    f.lenPtr = builder_.CreateStructGEP(mapHeaderTy_, mapVal, 0, p + "_len_ptr");
    f.len = builder_.CreateLoad(i64Ty_, f.lenPtr, p + "_len");
    f.capPtr = builder_.CreateStructGEP(mapHeaderTy_, mapVal, 1, p + "_cap_ptr");
    f.cap = builder_.CreateLoad(i64Ty_, f.capPtr, p + "_cap");
    f.keysPtr = builder_.CreateStructGEP(mapHeaderTy_, mapVal, 2, p + "_keys_ptr");
    f.keys = builder_.CreateLoad(ptrTy_, f.keysPtr, p + "_keys");
    f.valsPtr = builder_.CreateStructGEP(mapHeaderTy_, mapVal, 3, p + "_vals_ptr");
    f.vals = builder_.CreateLoad(ptrTy_, f.valsPtr, p + "_vals");
    return f;
}

llvm::Value *CodeGen::wrapPtrAsOption(llvm::Value *ptr, const std::string &hint) {
    llvm::Twine h(hint);
    llvm::Value *isNull = builder_.CreateICmpEQ(ptr,
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)),
        h + "_null");
    llvm::StructType *optTy = getOptionType(ptrTy_);
    llvm::Value *someVal = buildSomeValue(ptr, optTy);
    llvm::Value *noneVal = buildNoneValue(optTy);
    return builder_.CreateSelect(isNull, noneVal, someVal, h + "_opt");
}

// ===== Native constant registry & emission =====

enum class NativeConstantKind { Value, Infinity, NaN };

struct NativeConstantEntry {
    NativeConstantKind kind;
    double value;  // used only when kind == Value
};

static const std::unordered_map<std::string, NativeConstantEntry> native_constant_registry = {
    {"PI",  {NativeConstantKind::Value,    3.141592653589793}},
    {"E",   {NativeConstantKind::Value,    2.718281828459045}},
    {"Inf", {NativeConstantKind::Infinity, 0.0}},
    {"NaN", {NativeConstantKind::NaN,      0.0}},
};

bool CodeGen::isNativeConstant(const std::string &name) {
    return native_constant_registry.count(name);
}

llvm::Value *CodeGen::emitNativeConstant(const std::string &name) {
    auto it = native_constant_registry.find(name);
    if (it == native_constant_registry.end())
        codegenError("unknown native constant: " + name);
    switch (it->second.kind) {
    case NativeConstantKind::Value:    return llvm::ConstantFP::get(f64Ty_, it->second.value);
    case NativeConstantKind::Infinity: return llvm::ConstantFP::getInfinity(f64Ty_);
    case NativeConstantKind::NaN:      return llvm::ConstantFP::getNaN(f64Ty_);
    }
    llvm_unreachable("unhandled NativeConstantKind");
}

// ===== Builtin Math =====

llvm::Value *CodeGen::emitBuiltinMath(const CallExpr &e) {
    // Only dispatch if the callee was declared via @native (i.e., explicitly imported)
    if (!native_fn_arg_counts_.count(e.callee))
        return nullptr;

    // Helper: get fabs C function
    auto getFabs = [&]() {
        auto ty = llvm::FunctionType::get(f64Ty_, {f64Ty_}, false);
        return mod_->getOrInsertFunction("fabs", ty);
    };

    // abs(int) -> int, abs(float) -> float
    if (e.callee == "abs") {
        requireArgs(e, 1);
        llvm::Value *x = emitExpr(*e.args[0]);
        if (x->getType() == f64Ty_)
            return builder_.CreateCall(getFabs(), {x}, "abs");
        if (x->getType()->isIntegerTy(64)) {
            llvm::Value *neg = builder_.CreateNeg(x, "neg");
            llvm::Value *isNeg = builder_.CreateICmpSLT(x, llvm::ConstantInt::get(i64Ty_, 0), "is_neg");
            return builder_.CreateSelect(isNeg, neg, x, "abs");
        }
        codegenError("abs() requires int or float argument");
    }

    // floor/ceil/round(float) -> int
    if (e.callee == "floor" || e.callee == "ceil" || e.callee == "round") {
        requireArgs(e, 1);
        llvm::Value *x = emitExpr(*e.args[0]);
        if (x->getType() != f64Ty_)
            codegenError(e.callee + "() requires float argument");

        // Runtime check: reject NaN and values outside i64 range
        llvm::Value *isNan = builder_.CreateFCmpUNO(x, x, "is_nan_chk");
        llvm::Value *absVal = builder_.CreateCall(getFabs(), {x}, "abs_chk");
        // 2^63 = 9.223372036854776e+18 — values >= this overflow i64
        llvm::Value *limit = llvm::ConstantFP::get(f64Ty_, 9.223372036854776e+18);
        llvm::Value *tooBig = builder_.CreateFCmpOGE(absVal, limit, "too_big_chk");
        llvm::Value *invalid = builder_.CreateOr(isNan, tooBig, "invalid_chk");

        llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, e.callee + ".fail", fn_);
        llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, e.callee + ".ok", fn_);
        builder_.CreateCondBr(invalid, failBB, okBB);

        builder_.SetInsertPoint(failBB);
        static int mathErrCounter = 0;
        emitRuntimeError("runtime error: " + e.callee + "() argument out of int range\n",
                          ".math_err_" + std::to_string(mathErrCounter++));

        builder_.SetInsertPoint(okBB);
        auto fnTy = llvm::FunctionType::get(f64Ty_, {f64Ty_}, false);
        auto fn = mod_->getOrInsertFunction(e.callee, fnTy);
        llvm::Value *result = builder_.CreateCall(fn, {x}, e.callee);
        return builder_.CreateFPToSI(result, i64Ty_, e.callee + "_i");
    }

    // 1-arg float -> float: sqrt, log, log2, log10, exp, sin, cos, tan, asin, acos, atan
    {
        static const std::unordered_set<std::string> oneArgFloat = {
            "sqrt", "log", "log2", "log10", "exp",
            "sin", "cos", "tan", "asin", "acos", "atan"
        };
        if (oneArgFloat.count(e.callee)) {
            requireArgs(e, 1);
            llvm::Value *x = emitExpr(*e.args[0]);
            if (x->getType() != f64Ty_)
                codegenError(e.callee + "() requires float argument");
            auto fnTy = llvm::FunctionType::get(f64Ty_, {f64Ty_}, false);
            auto fn = mod_->getOrInsertFunction(e.callee, fnTy);
            return builder_.CreateCall(fn, {x}, e.callee);
        }
    }

    // 2-arg float -> float: pow, atan2, hypot
    {
        static const std::unordered_set<std::string> twoArgFloat = {
            "pow", "atan2", "hypot"
        };
        if (twoArgFloat.count(e.callee)) {
            requireArgs(e, 2);
            llvm::Value *x = emitExpr(*e.args[0]);
            llvm::Value *y = emitExpr(*e.args[1]);
            if (x->getType() != f64Ty_ || y->getType() != f64Ty_)
                codegenError(e.callee + "() requires float arguments");
            auto fnTy = llvm::FunctionType::get(f64Ty_, {f64Ty_, f64Ty_}, false);
            auto fn = mod_->getOrInsertFunction(e.callee, fnTy);
            return builder_.CreateCall(fn, {x, y}, e.callee);
        }
    }

    // is_nan(float) -> bool
    if (e.callee == "is_nan") {
        requireArgs(e, 1);
        llvm::Value *x = emitExpr(*e.args[0]);
        if (x->getType() != f64Ty_)
            codegenError("is_nan() requires float argument");
        return builder_.CreateFCmpUNO(x, x, "is_nan");
    }

    // is_inf(float) -> bool
    if (e.callee == "is_inf") {
        requireArgs(e, 1);
        llvm::Value *x = emitExpr(*e.args[0]);
        if (x->getType() != f64Ty_)
            codegenError("is_inf() requires float argument");
        llvm::Value *absVal = builder_.CreateCall(getFabs(), {x}, "abs_for_inf");
        llvm::Value *posInf = llvm::ConstantFP::getInfinity(f64Ty_);
        return builder_.CreateFCmpOEQ(absVal, posInf, "is_inf");
    }

    return nullptr;
}
