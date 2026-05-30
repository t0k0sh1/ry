#include "ry/codegen.hpp"
#include "ry/diagnostic/diagnostic.hpp"



namespace ry {

// ===== Builtin Higher-Order =====

llvm::Value *CodeGen::emitBuiltinHigherOrder(const CallExpr &e, llvm::Value *preEmittedArg0) {
    // filter(list, predicate) → new list with elements matching predicate
    if (e.callee == "filter") {
        requireArgs(e, 2);

        llvm::Value *listVal = preEmittedArg0 ? preEmittedArg0 : emitExpr(*e.args[0]);
        llvm::Value *lambdaVal = emitExpr(*e.args[1]);

        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy)
            codegenError("filter() requires a list as first argument");

        auto *fnInfo = lookupFnTypeInfo(lambdaVal);
        if (!fnInfo)
            codegenError("filter() requires a function as second argument");
        auto info = *fnInfo;

        if (info.paramTypes.size() != 1 || info.returnType != i1Ty_)
            codegenError("filter() predicate must take 1 argument and return bool");

        // Read source list
        auto lf = loadListHeader(listVal, "filter_src");

        // Allocate new list header + data (capacity = source length)
        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        llvm::Value *newHeader = emitArcAllocCollectionHeader(listHeaderTy_);

        uint64_t elemSize = dl.getTypeAllocSize(elemTy);
        llvm::Value *dataSize = emitIntOverflowCheck(
            llvm::Intrinsic::umul_with_overflow,
            lf.len,
            llvm::ConstantInt::get(i64Ty_, elemSize),
            "filter_data_size");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "filter_data");

        // Set up data pointer in header
        llvm::Value *newDataField = builder_.CreateStructGEP(listHeaderTy_, newHeader, 2, "filter_data_field");
        builder_.CreateStore(newData, newDataField);
        llvm::Value *newCapPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 1, "filter_cap_ptr");
        builder_.CreateStore(lf.len, newCapPtr);

        // Loop counter and output counter
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "filter_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
        llvm::AllocaInst *outVar = builder_.CreateAlloca(i64Ty_, nullptr, "filter_out");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), outVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "filter.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "filter.body", fn_);
        llvm::BasicBlock *storeBB = llvm::BasicBlock::Create(*ctx_, "filter.store", fn_);
        llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "filter.next", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "filter.end", fn_);

        builder_.CreateBr(condBB);

        // Condition: i < srcLen
        builder_.SetInsertPoint(condBB);
        llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "fi");
        llvm::Value *cond = builder_.CreateICmpSLT(iVal, lf.len, "filter_cond");
        builder_.CreateCondBr(cond, bodyBB, endBB);

        // Body: load element, call predicate
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "fi_cur");
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, lf.data, {iCur}, "filter_elem_ptr");
        llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "filter_elem");
        llvm::Value *pred = emitLambdaCall(lambdaVal, info, {elem}, "filter_pred");
        builder_.CreateCondBr(pred, storeBB, nextBB);

        // Store: add element to output
        builder_.SetInsertPoint(storeBB);
        llvm::Value *outIdx = builder_.CreateLoad(i64Ty_, outVar, "filter_out_idx");
        llvm::Value *dstPtr = builder_.CreateGEP(elemTy, newData, {outIdx}, "filter_dst_ptr");
        builder_.CreateStore(elem, dstPtr);
        llvm::Value *outNext = builder_.CreateAdd(outIdx, llvm::ConstantInt::get(i64Ty_, 1), "filter_out_next");
        builder_.CreateStore(outNext, outVar);
        builder_.CreateBr(nextBB);

        // Next: increment i
        builder_.SetInsertPoint(nextBB);
        llvm::Value *iCur2 = builder_.CreateLoad(i64Ty_, iVar, "fi_cur2");
        llvm::Value *iNext = builder_.CreateAdd(iCur2, llvm::ConstantInt::get(i64Ty_, 1), "fi_next");
        builder_.CreateStore(iNext, iVar);
        builder_.CreateBr(condBB);

        // End: set final length
        builder_.SetInsertPoint(endBB);
        llvm::Value *finalLen = builder_.CreateLoad(i64Ty_, outVar, "filter_final_len");
        llvm::Value *newLenPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 0, "filter_len_ptr");
        builder_.CreateStore(finalLen, newLenPtr);

        // Source and result share element pointers (loop copied them
        // verbatim). propagateMeta below makes the result inherit
        // list_elem_type_name, so resolveCollectionDestructor picks the same
        // inner-arc-release destructor for both — without per-element
        // retain, both destructors release the same elements at scope/exit
        // (glibc malloc detects this as `invalid next->prev_inuse`; macOS
        // libmalloc / ASan-on-JIT-code are blind to it). Mirror the
        // emitCollOp_slice retain block (#1204 / #1667).
        {
            const ValueMetadata *srcMeta = getMeta(listVal);
            const std::string elemSigSnap =
                srcMeta ? resolveTypeAlias(srcMeta->list_elem_type_name)
                         : std::string{};
            if (elemSigSnap.size() >= 2 && elemSigSnap.front() == '(' &&
                elemSigSnap.back() == ')') {
                if (auto *tupleTy = llvm::dyn_cast<llvm::StructType>(elemTy)) {
                    emitTupleElemRetainLoop(newData, finalLen, "filter_telem",
                                             elemSigSnap, tupleTy);
                }
            } else {
                CollectionKind elemArcKind = CollectionKind::List;
                if (elementTypeIsArcManaged(listVal, CollectionKind::List,
                                             &elemArcKind)) {
                    emitCowRetainArcElements(newData, finalLen, "filter_elem",
                                              elemArcKind);
                }
            }
        }

        setTypeMeta(TypeMeta::ListElem, newHeader, elemTy);
        propagateMeta(listVal, newHeader);
        return newHeader;
    }

    // map(list, transform) → new list with transformed elements
    if (e.callee == "map") {
        requireArgs(e, 2);

        llvm::Value *listVal = preEmittedArg0 ? preEmittedArg0 : emitExpr(*e.args[0]);
        llvm::Value *lambdaVal = emitExpr(*e.args[1]);

        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy)
            codegenError("map() requires a list as first argument");

        auto *fnInfo = lookupFnTypeInfo(lambdaVal);
        if (!fnInfo)
            codegenError("map() requires a function as second argument");
        auto info = *fnInfo;

        if (info.paramTypes.size() != 1)
            codegenError("map() transform must take exactly 1 argument");

        llvm::Type *outElemTy = info.returnType;

        // Read source list
        auto lf = loadListHeader(listVal, "map_src");

        // Allocate new list
        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        llvm::Value *newHeader = emitArcAllocCollectionHeader(listHeaderTy_);

        uint64_t outElemSize = dl.getTypeAllocSize(outElemTy);
        llvm::Value *dataSize = emitIntOverflowCheck(
            llvm::Intrinsic::umul_with_overflow,
            lf.len,
            llvm::ConstantInt::get(i64Ty_, outElemSize),
            "map_data_size");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "map_data");

        // Set header fields
        llvm::Value *newLenPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 0, "map_len_ptr");
        builder_.CreateStore(lf.len, newLenPtr);
        llvm::Value *newCapPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 1, "map_cap_ptr");
        builder_.CreateStore(lf.len, newCapPtr);
        llvm::Value *newDataField = builder_.CreateStructGEP(listHeaderTy_, newHeader, 2, "map_data_field");
        builder_.CreateStore(newData, newDataField);

        // Loop: transform each element
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "map_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "map.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "map.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "map.end", fn_);

        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(condBB);
        llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "mi");
        llvm::Value *cond = builder_.CreateICmpSLT(iVal, lf.len, "map_cond");
        builder_.CreateCondBr(cond, bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "mi_cur");
        llvm::Value *srcElemPtr = builder_.CreateGEP(elemTy, lf.data, {iCur}, "map_src_elem_ptr");
        llvm::Value *srcElem = builder_.CreateLoad(elemTy, srcElemPtr, "map_src_elem");
        llvm::Value *mapped = emitLambdaCall(lambdaVal, info, {srcElem}, "map_result");
        llvm::Value *dstElemPtr = builder_.CreateGEP(outElemTy, newData, {iCur}, "map_dst_elem_ptr");
        builder_.CreateStore(mapped, dstElemPtr);
        llvm::Value *iNext = builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1), "mi_next");
        builder_.CreateStore(iNext, iVar);
        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(endBB);
        setTypeMeta(TypeMeta::ListElem, newHeader, outElemTy);
        // Stamp source-level element name from the lambda's declared return
        // type so downstream higher-order combinators (sequence, traverse, …)
        // can reconstruct payload metadata after `xs.map(f)` chains. Without
        // this, the output's list_elem_type_name stays empty and ptr-backed
        // payloads collapse to "str" via reverseResolveTypeName at the next
        // metadata-recovery site.
        if (!info.returnTypeName.empty())
            getOrCreateMeta(newHeader).list_elem_type_name = info.returnTypeName;
        return newHeader;
    }

    // sort(list) or sort(list, comparator) → new sorted list
    if (e.callee == "sort") {
        if (e.args.size() < 1 || e.args.size() > 2)
            codegenError("sort() takes 1 or 2 arguments");

        llvm::Value *listVal = emitExpr(*e.args[0]);
        return emitSortCore(listVal, e.args, "sort");
    }

    // sort!(list) / sort!(list, comparator) → in-place sort
    if (e.callee == "sort!") {
        if (e.args.size() < 1 || e.args.size() > 2)
            codegenError("sort!() takes 1 or 2 arguments");

        llvm::AllocaInst *receiverAlloca = tryGetReceiverAlloca(*e.args[0]);
        llvm::Value *listPtr = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listPtr);
        if (!elemTy) codegenError("sort!() requires a list");
        listPtr = emitCowCheck(listPtr, receiverAlloca, CollectionKind::List);

        llvm::Value *sorted = emitSortCore(listPtr, e.args, "sort!");
        if (!sorted)
            codegenError("sort!() internal error");

        // Copy sorted data back into original list
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t elemSize = dl.getTypeAllocSize(elemTy);
        auto memcpyFn = getStdlibMemcpy();

        auto lf = loadListHeader(listPtr, "sortm");
        auto sf = loadListHeader(sorted, "sortm_sorted");
        llvm::Value *copySize = emitIntOverflowCheck(
            llvm::Intrinsic::umul_with_overflow,
            lf.len,
            llvm::ConstantInt::get(i64Ty_, elemSize),
            "sortm_sz");
        builder_.CreateCall(memcpyFn, {lf.data, sf.data, copySize});

        // Release the temporary sorted list through ARC (destructor frees data buffer)
        auto *sortedHdr = emitArcGetHeaderFromData(sorted);
        emitArcRelease(sortedHdr, false, getOrCreateCollectionDestructor(CollectionKind::List));

        return llvm::ConstantInt::get(i64Ty_, 0);
    }

    // reverse!(list) → in-place reverse
    if (e.callee == "reverse!") {
        requireArgs(e, 1);
        llvm::AllocaInst *receiverAlloca = tryGetReceiverAlloca(*e.args[0]);
        llvm::Value *listPtr = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listPtr);
        if (!elemTy)
            codegenError("reverse!() requires a list; strings are immutable, use reverse() instead");
        listPtr = emitCowCheck(listPtr, receiverAlloca, CollectionKind::List);

        auto lf = loadListHeader(listPtr, "revm");
        llvm::Value *len = lf.len;
        llvm::Value *dataPtr = lf.data;

        llvm::Value *half = builder_.CreateSDiv(len, llvm::ConstantInt::get(i64Ty_, 2), "revm_half");
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "revm_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "revm.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "revm.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "revm.end", fn_);
        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "revm_idx");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, half, "revm_cond"), bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "revm_cur");
        llvm::Value *j = builder_.CreateSub(builder_.CreateSub(len, llvm::ConstantInt::get(i64Ty_, 1)), iCur, "revm_j");
        llvm::Value *ptrI = builder_.CreateGEP(elemTy, dataPtr, iCur, "revm_pi");
        llvm::Value *ptrJ = builder_.CreateGEP(elemTy, dataPtr, j, "revm_pj");
        llvm::Value *vi = builder_.CreateLoad(elemTy, ptrI, "revm_vi");
        llvm::Value *vj = builder_.CreateLoad(elemTy, ptrJ, "revm_vj");
        builder_.CreateStore(vj, ptrI);
        builder_.CreateStore(vi, ptrJ);
        builder_.CreateStore(builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(endBB);
        return llvm::ConstantInt::get(i64Ty_, 0);
    }

    // ===== reduce(list, fn(a, b) -> a op b) -> Option<T> =====
    // fold() is panic-free without Option because its explicit seed makes the
    // empty-list case well-defined; reduce has no seed so must signal empty.
    if (e.callee == "reduce") {
        if (e.args.size() == 3)
            codegenError("reduce() takes 2 arguments, not 3; "
                         "to use an initial value, call fold(list, init, fn) instead");
        requireArgs(e, 2);
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Value *lambdaVal = emitExpr(*e.args[1]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy) codegenError("reduce() requires a list");
        auto *fnInfo = lookupFnTypeInfo(lambdaVal);
        if (!fnInfo)
            codegenError("reduce() requires a function");
        auto info = *fnInfo;

        llvm::StructType *optTy = getOptionType(info.returnType);
        auto lf = loadListHeader(listVal, "reduce");
        llvm::Value *srcLen = lf.len;
        llvm::Value *srcData = lf.data;

        llvm::Value *isEmpty = builder_.CreateICmpEQ(
            srcLen, llvm::ConstantInt::get(i64Ty_, 0), "reduce_empty");
        llvm::BasicBlock *emptyBB = llvm::BasicBlock::Create(*ctx_, "reduce.empty", fn_);
        llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "reduce.ok", fn_);
        llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "reduce.merge", fn_);
        builder_.CreateCondBr(isEmpty, emptyBB, okBB);

        builder_.SetInsertPoint(emptyBB);
        llvm::Value *noneVal = buildNoneValue(optTy);
        builder_.CreateBr(mergeBB);
        llvm::BasicBlock *emptyEndBB = builder_.GetInsertBlock();

        // Seed accumulator with element[0]; the loop below starts at i=1.
        builder_.SetInsertPoint(okBB);
        llvm::Value *firstElem = builder_.CreateLoad(elemTy, srcData, "reduce_first");
        // Untyped lambda makes info.returnType = anyTy_ (16B) while first is a
        // raw primitive (e.g. i64, 8B). Coerce via wrapInAny so the full 16B
        // Any slot is initialised before the loop and before wrapping as Some.
        if (isAnyType(info.returnType) && !isAnyType(firstElem->getType()))
            firstElem = wrapInAny(firstElem);
        llvm::AllocaInst *accVar = builder_.CreateAlloca(info.returnType, nullptr, "reduce_acc");
        builder_.CreateStore(firstElem, accVar);
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "reduce_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 1), iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "reduce.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "reduce.body", fn_);
        llvm::BasicBlock *loopEndBB = llvm::BasicBlock::Create(*ctx_, "reduce.loop_end", fn_);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "ri");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, srcLen), bodyBB, loopEndBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, srcData, {i}, "reduce_ep");
        llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "reduce_elem");
        llvm::Value *acc = builder_.CreateLoad(info.returnType, accVar, "reduce_acc_val");
        llvm::Value *result = emitLambdaCall(lambdaVal, info, {acc, elem}, "reduce_call");
        builder_.CreateStore(result, accVar);
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(loopEndBB);
        llvm::Value *finalAcc = builder_.CreateLoad(info.returnType, accVar, "reduce_final");
        llvm::Value *someVal = buildSomeValue(finalAcc, optTy);
        builder_.CreateBr(mergeBB);
        llvm::BasicBlock *okEndBB = builder_.GetInsertBlock();

        builder_.SetInsertPoint(mergeBB);
        llvm::PHINode *phi = builder_.CreatePHI(optTy, 2, "reduce_result");
        phi->addIncoming(noneVal, emptyEndBB);
        phi->addIncoming(someVal, okEndBB);
        return phi;
    }

    // ===== fold(list, init, fn(a, b) -> a op b) =====
    if (e.callee == "fold") {
        requireArgs(e, 3);
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Value *initVal = emitExpr(*e.args[1]);
        llvm::Value *lambdaVal = emitExpr(*e.args[2]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy) codegenError("fold() requires a list");
        auto *fnInfo = lookupFnTypeInfo(lambdaVal);
        if (!fnInfo)
            codegenError("fold() requires a function");
        auto info = *fnInfo;
        if (info.paramTypes.size() != 2)
            codegenError("fold() function must take 2 parameters (accumulator, element)");
        // Untyped lambda makes info.returnType = anyTy_ (16B) while initVal is a
        // raw primitive (e.g. i64, 8B). A narrow CreateStore would leave the Any
        // data field uninitialized; coerce initVal via wrapInAny so the full 16B
        // slot is always initialised. Mirrors the same fix applied to reduce (#1020).
        if (isAnyType(info.returnType) && !isAnyType(initVal->getType()))
            initVal = wrapInAny(initVal);
        if (info.returnType != initVal->getType())
            codegenError("fold() initial value type must match function return type");

        auto lf = loadListHeader(listVal, "fold");
        llvm::Value *srcLen = lf.len;
        llvm::Value *srcData = lf.data;

        llvm::AllocaInst *accVar = builder_.CreateAlloca(info.returnType, nullptr, "fold_acc");
        builder_.CreateStore(initVal, accVar);
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "fold_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "fold.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "fold.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "fold.end", fn_);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "fi");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, srcLen), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, srcData, {i}, "fold_ep");
        llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "fold_elem");
        llvm::Value *acc = builder_.CreateLoad(info.returnType, accVar, "fold_acc_val");
        llvm::Value *result = emitLambdaCall(lambdaVal, info, {acc, elem}, "fold_call");
        builder_.CreateStore(result, accVar);
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(endBB);
        return builder_.CreateLoad(info.returnType, accVar, "fold_result");
    }

    // ===== any(list, pred) =====
    if (e.callee == "any") {
        requireArgs(e, 2);
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Value *lambdaVal = emitExpr(*e.args[1]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy) codegenError("any() requires a list");
        auto *fnInfo = lookupFnTypeInfo(lambdaVal);
        if (!fnInfo)
            codegenError("any() requires a function");
        auto info = *fnInfo;
        if (info.paramTypes.size() != 1)
            codegenError("any() predicate must take 1 parameter");
        if (info.returnType != i1Ty_)
            codegenError("any() predicate must return bool");

        auto lf = loadListHeader(listVal, "any");
        llvm::Value *srcLen = lf.len;
        llvm::Value *srcData = lf.data;

        llvm::AllocaInst *resultVar = builder_.CreateAlloca(i1Ty_, nullptr, "any_result");
        builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 0), resultVar);
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "any_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "any.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "any.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "any.end", fn_);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "ai");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, srcLen), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, srcData, {i}, "any_ep");
        llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "any_elem");
        llvm::Value *pred = emitLambdaCall(lambdaVal, info, {elem}, "any_pred");
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        llvm::BasicBlock *foundBB = llvm::BasicBlock::Create(*ctx_, "any.found", fn_);
        builder_.CreateCondBr(pred, foundBB, condBB);
        builder_.SetInsertPoint(foundBB);
        builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 1), resultVar);
        builder_.CreateBr(endBB);
        builder_.SetInsertPoint(endBB);
        return builder_.CreateLoad(i1Ty_, resultVar, "any_final");
    }

    // ===== all(list, pred) =====
    if (e.callee == "all") {
        requireArgs(e, 2);
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Value *lambdaVal = emitExpr(*e.args[1]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy) codegenError("all() requires a list");
        auto *fnInfo = lookupFnTypeInfo(lambdaVal);
        if (!fnInfo)
            codegenError("all() requires a function");
        auto info = *fnInfo;
        if (info.paramTypes.size() != 1)
            codegenError("all() predicate must take 1 parameter");
        if (info.returnType != i1Ty_)
            codegenError("all() predicate must return bool");

        auto lf = loadListHeader(listVal, "all");
        llvm::Value *srcLen = lf.len;
        llvm::Value *srcData = lf.data;

        llvm::AllocaInst *resultVar = builder_.CreateAlloca(i1Ty_, nullptr, "all_result");
        builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 1), resultVar);
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "all_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "all.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "all.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "all.end", fn_);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "ali");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, srcLen), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, srcData, {i}, "all_ep");
        llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "all_elem");
        llvm::Value *pred = emitLambdaCall(lambdaVal, info, {elem}, "all_pred");
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, "all.fail", fn_);
        builder_.CreateCondBr(pred, condBB, failBB);
        builder_.SetInsertPoint(failBB);
        builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 0), resultVar);
        builder_.CreateBr(endBB);
        builder_.SetInsertPoint(endBB);
        return builder_.CreateLoad(i1Ty_, resultVar, "all_final");
    }

    // ===== sum(list) =====
    if (e.callee == "sum") {
        requireArgs(e, 1);
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy) codegenError("sum() requires a list");
        if (elemTy != i64Ty_ && elemTy != f64Ty_ && elemTy != i8Ty_)
            codegenError("sum() requires a numeric list (int, float, or u8)");

        auto lf = loadListHeader(listVal, "sum");
        llvm::Value *srcLen = lf.len;
        llvm::Value *srcData = lf.data;

        llvm::AllocaInst *accVar = builder_.CreateAlloca(elemTy, nullptr, "sum_acc");
        if (elemTy == f64Ty_)
            builder_.CreateStore(llvm::ConstantFP::get(f64Ty_, 0.0), accVar);
        else
            builder_.CreateStore(llvm::ConstantInt::get(elemTy, 0), accVar);
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "sum_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "sum.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "sum.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "sum.end", fn_);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "si");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, srcLen), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, srcData, {i}, "sum_ep");
        llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "sum_elem");
        llvm::Value *acc = builder_.CreateLoad(elemTy, accVar, "sum_acc_val");
        llvm::Value *newAcc;
        if (elemTy == f64Ty_)
            newAcc = builder_.CreateFAdd(acc, elem, "sum_add");
        else
            newAcc = builder_.CreateAdd(acc, elem, "sum_add");
        builder_.CreateStore(newAcc, accVar);
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(endBB);
        return builder_.CreateLoad(elemTy, accVar, "sum_result");
    }

    // ===== min(list) / max(list) =====
    if (e.callee == "min" || e.callee == "max") {
        requireArgs(e, 1);
        bool isMax = (e.callee == "max");
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy) codegenError(e.callee + "() requires a list");
        if (elemTy != i64Ty_ && elemTy != f64Ty_)
            codegenError(e.callee + "() requires a numeric list (int or float)");

        auto lf = loadListHeader(listVal, "mm");
        llvm::Value *srcLen = lf.len;
        llvm::Value *srcData = lf.data;

        // Check empty list
        llvm::Value *isEmptyMM = builder_.CreateICmpEQ(srcLen, llvm::ConstantInt::get(i64Ty_, 0), "mm_empty");
        llvm::BasicBlock *errBBMM = llvm::BasicBlock::Create(*ctx_, "mm.err", fn_);
        llvm::BasicBlock *okBBMM = llvm::BasicBlock::Create(*ctx_, "mm.ok", fn_);
        builder_.CreateCondBr(isEmptyMM, errBBMM, okBBMM);
        builder_.SetInsertPoint(errBBMM);
        emitRuntimeError("runtime error: " + e.callee + "() on empty list\n", ".mm_empty_err");
        builder_.SetInsertPoint(okBBMM);

        llvm::Value *first = builder_.CreateLoad(elemTy, srcData, "mm_first");
        llvm::AllocaInst *bestVar = builder_.CreateAlloca(elemTy, nullptr, "mm_best");
        builder_.CreateStore(first, bestVar);
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "mm_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 1), iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "mm.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "mm.body", fn_);
        llvm::BasicBlock *updateBB = llvm::BasicBlock::Create(*ctx_, "mm.update", fn_);
        llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "mm.next", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "mm.end", fn_);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "mi");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, srcLen), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, srcData, {i}, "mm_ep");
        llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "mm_elem");
        llvm::Value *best = builder_.CreateLoad(elemTy, bestVar, "mm_best_val");
        llvm::Value *cmp;
        if (elemTy == f64Ty_)
            cmp = isMax ? builder_.CreateFCmpOGT(elem, best, "mm_cmp")
                        : builder_.CreateFCmpOLT(elem, best, "mm_cmp");
        else
            cmp = isMax ? builder_.CreateICmpSGT(elem, best, "mm_cmp")
                        : builder_.CreateICmpSLT(elem, best, "mm_cmp");
        builder_.CreateCondBr(cmp, updateBB, nextBB);
        builder_.SetInsertPoint(updateBB);
        builder_.CreateStore(elem, bestVar);
        builder_.CreateBr(nextBB);
        builder_.SetInsertPoint(nextBB);
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(endBB);
        return builder_.CreateLoad(elemTy, bestVar, "mm_result");
    }


    // tap(list, fn) → call fn on each element, return original list
    if (e.callee == "tap") {
        requireArgs(e, 2);

        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Value *lambdaVal = emitExpr(*e.args[1]);

        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy)
            codegenError("tap() requires a list as first argument");

        auto *fnInfo = lookupFnTypeInfo(lambdaVal);
        if (!fnInfo)
            codegenError("tap() requires a function as second argument");
        auto info = *fnInfo;

        if (info.paramTypes.size() != 1)
            codegenError("tap() function must take exactly 1 argument");

        // Read source list
        auto lf = loadListHeader(listVal, "tap_src");

        // Loop: call fn on each element
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "tap_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "tap.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "tap.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "tap.end", fn_);

        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(condBB);
        llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "tap_iv");
        llvm::Value *cond = builder_.CreateICmpSLT(iVal, lf.len, "tap_cond");
        builder_.CreateCondBr(cond, bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "tap_ic");
        llvm::Value *srcElemPtr = builder_.CreateGEP(elemTy, lf.data, {iCur}, "tap_elem_ptr");
        llvm::Value *srcElem = builder_.CreateLoad(elemTy, srcElemPtr, "tap_elem");
        emitLambdaCall(lambdaVal, info, {srcElem}, "tap_call");
        llvm::Value *iNext = builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1), "tap_next");
        builder_.CreateStore(iNext, iVar);
        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(endBB);
        return listVal;
    }

    if (e.callee == "sequence") {
        requireArgs(e, 1);

        llvm::Value *listVal = preEmittedArg0 ? preEmittedArg0 : emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy)
            codegenError("sequence() requires a list of Result or Option");

        bool isResult = isResultType(elemTy);
        bool isOption = !isResult && isOptionType(elemTy);
        if (!isResult && !isOption)
            codegenError("sequence() requires a list of Result or Option");

        auto *elemStructTy = llvm::cast<llvm::StructType>(elemTy);
        llvm::Type *innerTy = elemStructTy->getElementType(1);
        llvm::Type *errTy = isResult ? elemStructTy->getElementType(2) : nullptr;

        const ValueMetadata *srcMeta = getMeta(listVal);
        std::string sourceElemName = srcMeta ? srcMeta->list_elem_type_name : std::string{};
        std::string innerTypeName;
        std::string errTypeName;
        if (sourceElemName.size() > 8 &&
            sourceElemName.compare(0, 7, "Result<") == 0 &&
            sourceElemName.back() == '>') {
            std::string inside = sourceElemName.substr(7, sourceElemName.size() - 8);
            auto parts = splitTypeArgs(inside);
            if (!parts.empty())
                innerTypeName = ry::util::trimTypeNameSpaces(parts[0]);
            if (parts.size() >= 2)
                errTypeName = ry::util::trimTypeNameSpaces(parts[1]);
        } else if (sourceElemName.size() > 8 &&
                   sourceElemName.compare(0, 7, "Option<") == 0 &&
                   sourceElemName.back() == '>') {
            std::string inside = sourceElemName.substr(7, sourceElemName.size() - 8);
            auto parts = splitTypeArgs(inside);
            if (!parts.empty())
                innerTypeName = ry::util::trimTypeNameSpaces(parts[0]);
        } else if (sourceElemName.size() > 1 && sourceElemName.back() == '?') {
            innerTypeName = sourceElemName.substr(0, sourceElemName.size() - 1);
        }

        llvm::StructType *outResultTy = isResult ? getResultType(ptrTy_, errTy) : nullptr;
        llvm::StructType *outOptionTy = isOption ? getOptionType(ptrTy_) : nullptr;

        auto lf = loadListHeader(listVal, "seq_src");

        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        llvm::Value *outHeader = emitArcAllocCollectionHeader(listHeaderTy_);

        uint64_t innerSize = dl.getTypeAllocSize(innerTy);
        llvm::Value *dataSize = emitIntOverflowCheck(
            llvm::Intrinsic::umul_with_overflow,
            lf.len,
            llvm::ConstantInt::get(i64Ty_, innerSize),
            "seq_data_size");
        llvm::Value *outData = builder_.CreateCall(mallocFn, {dataSize}, "seq_data");

        storeListHeaderFields(outHeader, llvm::ConstantInt::get(i64Ty_, 0), lf.len, outData);
        llvm::Value *outLenPtr = builder_.CreateStructGEP(listHeaderTy_, outHeader, 0, "seq_len_ptr");

        setTypeMeta(TypeMeta::ListElem, outHeader, innerTy);
        if (!innerTypeName.empty())
            propagateTypeMeta("List<" + innerTypeName + ">", outHeader);

        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "seq_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "seq.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "seq.body", fn_);
        llvm::BasicBlock *contBB = llvm::BasicBlock::Create(*ctx_, "seq.cont", fn_);
        llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, "seq.fail", fn_);
        llvm::BasicBlock *finBB = llvm::BasicBlock::Create(*ctx_, "seq.fin", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "seq.end", fn_);

        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(condBB);
        llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "seq_iv");
        llvm::Value *cond = builder_.CreateICmpSLT(iVal, lf.len, "seq_cond");
        builder_.CreateCondBr(cond, bodyBB, finBB);

        builder_.SetInsertPoint(bodyBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "seq_ic");
        llvm::Value *srcElemPtr = builder_.CreateGEP(elemTy, lf.data, {iCur}, "seq_elem_ptr");
        llvm::Value *srcElem = builder_.CreateLoad(elemTy, srcElemPtr, "seq_elem");
        llvm::Value *tag = builder_.CreateExtractValue(srcElem, {0u}, "seq_tag");
        builder_.CreateCondBr(tag, contBB, failBB);

        builder_.SetInsertPoint(contBB);
        llvm::Value *payload = builder_.CreateExtractValue(srcElem, {1u}, "seq_payload");
        if (innerTy == ptrTy_) {
            if (!innerTypeName.empty())
                propagateTypeMeta(innerTypeName, payload);
            retainArcValue(payload);
        }
        llvm::Value *dstPtr = builder_.CreateGEP(innerTy, outData, {iCur}, "seq_dst_ptr");
        builder_.CreateStore(payload, dstPtr);
        llvm::Value *iNext = builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1), "seq_inext");
        builder_.CreateStore(iNext, iVar);
        builder_.CreateStore(iNext, outLenPtr);
        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(failBB);
        llvm::Value *failResult;
        if (isResult) {
            llvm::Value *errVal = builder_.CreateExtractValue(srcElem, {2u}, "seq_err");
            failResult = buildErrValue(errVal, outResultTy);
        } else {
            failResult = buildNoneValue(outOptionTy);
        }
        {
            llvm::FunctionCallee dtor = getOrCreateCollectionDestructor(
                CollectionKind::List, innerTypeName, "");
            llvm::Value *outArcHdrPtr = emitArcGetHeaderFromData(outHeader);
            emitArcRelease(outArcHdrPtr, false, dtor);
        }
        builder_.CreateBr(endBB);
        llvm::BasicBlock *failEndBB = builder_.GetInsertBlock();

        builder_.SetInsertPoint(finBB);
        llvm::Value *successResult = isResult
            ? buildOkValue(outHeader, outResultTy)
            : buildSomeValue(outHeader, outOptionTy);
        builder_.CreateBr(endBB);
        llvm::BasicBlock *finEndBB = builder_.GetInsertBlock();

        builder_.SetInsertPoint(endBB);
        llvm::Type *resultPhiTy = isResult ? outResultTy : outOptionTy;
        llvm::PHINode *phi = builder_.CreatePHI(resultPhiTy, 2, "seq_result");
        phi->addIncoming(failResult, failEndBB);
        phi->addIncoming(successResult, finEndBB);

        if (!innerTypeName.empty()) {
            std::string outputTypeName;
            if (isResult) {
                std::string err = errTypeName.empty() ? std::string("Error") : errTypeName;
                outputTypeName = "Result<List<" + innerTypeName + ">, " + err + ">";
            } else {
                outputTypeName = "Option<List<" + innerTypeName + ">>";
            }
            propagateTypeMeta(outputTypeName, phi);
        }
        return phi;
    }

    return nullptr;
}

llvm::Value *CodeGen::emitSortCore(llvm::Value *listVal, const std::vector<ExprPtr> &args, const std::string &callee) {
    llvm::Type *elemTy = getListElementType(listVal);
    if (!elemTy)
        codegenError(callee + "() requires a list as first argument");

    bool hasComparator = (args.size() >= 2);
    llvm::Value *compVal = nullptr;
    FnTypeInfo compInfo;
    if (hasComparator) {
        compVal = emitExpr(*args[1]);
        auto *fnInfo = lookupFnTypeInfo(compVal);
        if (!fnInfo)
            codegenError(callee + "() comparator must be a function");
        compInfo = *fnInfo;
        if (compInfo.paramTypes.size() != 2 || compInfo.returnType != i1Ty_)
            codegenError(callee + "() comparator must take 2 arguments and return bool");
    }

    // Read source list
    auto lf = loadListHeader(listVal, "sort_src");
    llvm::Value *srcLen = lf.len;
    llvm::Value *srcData = lf.data;

    // Allocate new list and copy data
    auto mallocFn = getStdlibMalloc();
    const llvm::DataLayout &dl = mod_->getDataLayout();
    llvm::Value *newHeader = emitArcAllocCollectionHeader(listHeaderTy_);

    uint64_t elemSz = dl.getTypeAllocSize(elemTy);
    llvm::Value *dataSize = emitIntOverflowCheck(
        llvm::Intrinsic::umul_with_overflow,
        srcLen,
        llvm::ConstantInt::get(i64Ty_, elemSz),
        "sort_data_size");
    llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "sort_data");

    // memcpy source data to new data
    auto memcpyFn = getStdlibMemcpy();
    builder_.CreateCall(memcpyFn, {newData, srcData, dataSize});

    // Set header
    llvm::Value *newLenPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 0, "sort_len_ptr");
    builder_.CreateStore(srcLen, newLenPtr);
    llvm::Value *newCapPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 1, "sort_cap_ptr");
    builder_.CreateStore(srcLen, newCapPtr);
    llvm::Value *newDataField = builder_.CreateStructGEP(listHeaderTy_, newHeader, 2, "sort_data_field");
    builder_.CreateStore(newData, newDataField);

    // Generate trampoline function for TimSort comparator
    std::string trampName = "__sort_trampoline_" + std::to_string(lambda_counter_++);
    llvm::FunctionType *trampTy = llvm::FunctionType::get(
        i1Ty_, {ptrTy_, ptrTy_, ptrTy_}, false);
    llvm::Function *trampFn = llvm::Function::Create(
        trampTy, llvm::Function::ExternalLinkage, trampName, mod_.get());
    trampFn->setCallingConv(llvm::CallingConv::C);

    auto trampArgs = trampFn->arg_begin();
    llvm::Argument *argA = &*trampArgs++;
    llvm::Argument *argB = &*trampArgs++;
    llvm::Argument *argCtx = &*trampArgs++;
    argA->setName("a_ptr");
    argB->setName("b_ptr");
    argCtx->setName("ctx");

    {
        FnScope guard(*this);
        fn_ = trampFn;
        llvm::BasicBlock *trampBB = llvm::BasicBlock::Create(*ctx_, "entry", trampFn);
        builder_.SetInsertPoint(trampBB);

        llvm::Value *valA = builder_.CreateLoad(elemTy, argA, "val_a");
        llvm::Value *valB = builder_.CreateLoad(elemTy, argB, "val_b");

        llvm::Value *result;
        if (hasComparator) {
            result = emitLambdaCall(argCtx, compInfo, {valA, valB}, "sort_comp");
        } else if (elemTy == i64Ty_) {
            result = builder_.CreateICmpSLT(valA, valB, "sort_lt");
        } else if (elemTy == f64Ty_) {
            result = builder_.CreateFCmpOLT(valA, valB, "sort_lt");
        } else if (elemTy == ptrTy_) {
            auto strcmpFn = getStdlibStrcmp();
            llvm::Value *cmpResult = builder_.CreateCall(strcmpFn, {valA, valB}, "sort_strcmp");
            result = builder_.CreateICmpSLT(cmpResult, llvm::ConstantInt::get(i32Ty_, 0), "sort_lt");
        } else {
            codegenError(callee + "() does not support this element type");
        }

        builder_.CreateRet(result);
    }

    // Call __ry_timsort(newData, srcLen, elemSize, trampoline, cmpCtx)
    llvm::Value *elemSizeConst = llvm::ConstantInt::get(i64Ty_, elemSz);
    llvm::Value *cmpCtx = hasComparator
        ? compVal
        : llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));

    auto timsortFn = getRuntimeFn("__ry_timsort",
        llvm::Type::getVoidTy(*ctx_), {ptrTy_, i64Ty_, i64Ty_, ptrTy_, ptrTy_});
    builder_.CreateCall(timsortFn, {newData, srcLen, elemSizeConst, trampFn, cmpCtx});

    // memcpy duplicates ARC element pointers without bumping refcounts;
    // propagateMeta below makes src and result share the same destructor,
    // which would double-release each element. Mirror the
    // emitCollOp_slice retain block (#1204 / #1667).
    {
        const ValueMetadata *srcMeta = getMeta(listVal);
        const std::string elemSigSnap =
            srcMeta ? resolveTypeAlias(srcMeta->list_elem_type_name)
                     : std::string{};
        if (elemSigSnap.size() >= 2 && elemSigSnap.front() == '(' &&
            elemSigSnap.back() == ')') {
            if (auto *tupleTy = llvm::dyn_cast<llvm::StructType>(elemTy)) {
                emitTupleElemRetainLoop(newData, srcLen, "sort_telem",
                                         elemSigSnap, tupleTy);
            }
        } else {
            CollectionKind elemArcKind = CollectionKind::List;
            if (elementTypeIsArcManaged(listVal, CollectionKind::List,
                                         &elemArcKind)) {
                emitCowRetainArcElements(newData, srcLen, "sort_elem",
                                          elemArcKind);
            }
        }
    }

    // Return sorted list
    setTypeMeta(TypeMeta::ListElem, newHeader, elemTy);
    propagateMeta(listVal, newHeader);
    return newHeader;
}

} // namespace ry
