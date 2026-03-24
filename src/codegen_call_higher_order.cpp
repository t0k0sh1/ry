#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"


// ===== Builtin Higher-Order =====

llvm::Value *CodeGen::emitBuiltinHigherOrder(const CallExpr &e) {
    // filter(list, predicate) → new list with elements matching predicate
    if (e.callee == "filter") {
        if (e.args.size() != 2)
            codegenError("filter() takes exactly 2 arguments");

        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Value *lambdaVal = emitExpr(*e.args[1]);

        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy)
            codegenError("filter() requires a list as first argument");

        // Get lambda type info (handle LoadInst for variable-passed functions)
        auto fnIt = fn_type_info_.find(lambdaVal);
        if (fnIt == fn_type_info_.end()) {
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(lambdaVal))
                fnIt = fn_type_info_.find(load->getPointerOperand());
        }
        if (fnIt == fn_type_info_.end())
            codegenError("filter() requires a function as second argument");
        auto &info = fnIt->second;

        if (info.paramTypes.size() != 1 || info.returnType != i1Ty_)
            codegenError("filter() predicate must take 1 argument and return bool");

        // Read source list
        llvm::Value *srcLenPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 0, "filter_src_len_ptr");
        llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, srcLenPtr, "filter_src_len");
        llvm::Value *srcDataPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 2, "filter_src_data_field");
        llvm::Value *srcData = builder_.CreateLoad(ptrTy_, srcDataPtr, "filter_src_data");

        // Allocate new list header + data (capacity = source length)
        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        llvm::Value *newHeader = builder_.CreateCall(
            mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "filter_header");

        uint64_t elemSize = dl.getTypeAllocSize(elemTy);
        llvm::Value *dataSize = builder_.CreateMul(srcLen, llvm::ConstantInt::get(i64Ty_, elemSize), "filter_data_size");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "filter_data");

        // Set up data pointer in header
        llvm::Value *newDataField = builder_.CreateStructGEP(listHeaderTy_, newHeader, 2, "filter_data_field");
        builder_.CreateStore(newData, newDataField);
        llvm::Value *newCapPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 1, "filter_cap_ptr");
        builder_.CreateStore(srcLen, newCapPtr);

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
        llvm::Value *cond = builder_.CreateICmpSLT(iVal, srcLen, "filter_cond");
        builder_.CreateCondBr(cond, bodyBB, endBB);

        // Body: load element, call predicate
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "fi_cur");
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, srcData, {iCur}, "filter_elem_ptr");
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

        list_element_types_[newHeader] = elemTy;
        return newHeader;
    }

    // map(list, transform) → new list with transformed elements
    if (e.callee == "map") {
        if (e.args.size() != 2)
            codegenError("map() takes exactly 2 arguments");

        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Value *lambdaVal = emitExpr(*e.args[1]);

        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy)
            codegenError("map() requires a list as first argument");

        auto fnIt = fn_type_info_.find(lambdaVal);
        if (fnIt == fn_type_info_.end()) {
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(lambdaVal))
                fnIt = fn_type_info_.find(load->getPointerOperand());
        }
        if (fnIt == fn_type_info_.end())
            codegenError("map() requires a function as second argument");
        auto &info = fnIt->second;

        if (info.paramTypes.size() != 1)
            codegenError("map() transform must take exactly 1 argument");

        llvm::Type *outElemTy = info.returnType;

        // Read source list
        llvm::Value *srcLenPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 0, "map_src_len_ptr");
        llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, srcLenPtr, "map_src_len");
        llvm::Value *srcDataPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 2, "map_src_data_field");
        llvm::Value *srcData = builder_.CreateLoad(ptrTy_, srcDataPtr, "map_src_data");

        // Allocate new list
        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        llvm::Value *newHeader = builder_.CreateCall(
            mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "map_header");

        uint64_t outElemSize = dl.getTypeAllocSize(outElemTy);
        llvm::Value *dataSize = builder_.CreateMul(srcLen, llvm::ConstantInt::get(i64Ty_, outElemSize), "map_data_size");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "map_data");

        // Set header fields
        llvm::Value *newLenPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 0, "map_len_ptr");
        builder_.CreateStore(srcLen, newLenPtr);
        llvm::Value *newCapPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 1, "map_cap_ptr");
        builder_.CreateStore(srcLen, newCapPtr);
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
        llvm::Value *cond = builder_.CreateICmpSLT(iVal, srcLen, "map_cond");
        builder_.CreateCondBr(cond, bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "mi_cur");
        llvm::Value *srcElemPtr = builder_.CreateGEP(elemTy, srcData, {iCur}, "map_src_elem_ptr");
        llvm::Value *srcElem = builder_.CreateLoad(elemTy, srcElemPtr, "map_src_elem");
        llvm::Value *mapped = emitLambdaCall(lambdaVal, info, {srcElem}, "map_result");
        llvm::Value *dstElemPtr = builder_.CreateGEP(outElemTy, newData, {iCur}, "map_dst_elem_ptr");
        builder_.CreateStore(mapped, dstElemPtr);
        llvm::Value *iNext = builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1), "mi_next");
        builder_.CreateStore(iNext, iVar);
        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(endBB);
        list_element_types_[newHeader] = outElemTy;
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

        llvm::Value *listPtr = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listPtr);
        if (!elemTy) codegenError("sort!() requires a list");

        llvm::Value *sorted = emitSortCore(listPtr, e.args, "sort!");
        if (!sorted)
            codegenError("sort!() internal error");

        // Copy sorted data back into original list
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t elemSize = dl.getTypeAllocSize(elemTy);
        auto memcpyFn = getStdlibMemcpy();

        llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(listHeaderTy_, listPtr, 0), "sortm_len");
        llvm::Value *srcData = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(listHeaderTy_, listPtr, 2), "sortm_data");
        llvm::Value *sortedData = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(listHeaderTy_, sorted, 2), "sortm_sorted");
        llvm::Value *copySize = builder_.CreateMul(srcLen, llvm::ConstantInt::get(i64Ty_, elemSize), "sortm_sz");
        builder_.CreateCall(memcpyFn, {srcData, sortedData, copySize});

        // Free the temporary sorted list
        auto freeFn = getStdlibFree();
        builder_.CreateCall(freeFn, {sortedData});
        builder_.CreateCall(freeFn, {sorted});

        return llvm::ConstantInt::get(i64Ty_, 0);
    }

    // ===== reduce(list, fn(a, b) -> a op b) =====
    if (e.callee == "reduce") {
        if (e.args.size() != 2)
            codegenError("reduce() takes exactly 2 arguments");
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Value *lambdaVal = emitExpr(*e.args[1]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy) codegenError("reduce() requires a list");
        auto fnIt = fn_type_info_.find(lambdaVal);
        if (fnIt == fn_type_info_.end())
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(lambdaVal))
                fnIt = fn_type_info_.find(load->getPointerOperand());
        if (fnIt == fn_type_info_.end())
            codegenError("reduce() requires a function");
        auto &info = fnIt->second;

        llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(listHeaderTy_, listVal, 0), "reduce_len");
        llvm::Value *srcData = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(listHeaderTy_, listVal, 2), "reduce_data");

        // Check empty list
        llvm::Value *isEmptyR = builder_.CreateICmpEQ(srcLen, llvm::ConstantInt::get(i64Ty_, 0), "reduce_empty");
        llvm::BasicBlock *errBBR = llvm::BasicBlock::Create(*ctx_, "reduce.err", fn_);
        llvm::BasicBlock *okBBR = llvm::BasicBlock::Create(*ctx_, "reduce.ok", fn_);
        builder_.CreateCondBr(isEmptyR, errBBR, okBBR);
        builder_.SetInsertPoint(errBBR);
        emitRuntimeError("runtime error: reduce() on empty list\n", ".reduce_empty_err");
        builder_.SetInsertPoint(okBBR);

        // acc = list[0]
        llvm::Value *first = builder_.CreateLoad(elemTy, srcData, "reduce_first");
        llvm::AllocaInst *accVar = builder_.CreateAlloca(info.returnType, nullptr, "reduce_acc");
        builder_.CreateStore(first, accVar);
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "reduce_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 1), iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "reduce.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "reduce.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "reduce.end", fn_);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "ri");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, srcLen), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, srcData, {i}, "reduce_ep");
        llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "reduce_elem");
        llvm::Value *acc = builder_.CreateLoad(info.returnType, accVar, "reduce_acc_val");
        llvm::Value *result = emitLambdaCall(lambdaVal, info, {acc, elem}, "reduce_call");
        builder_.CreateStore(result, accVar);
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(endBB);
        return builder_.CreateLoad(info.returnType, accVar, "reduce_result");
    }

    // ===== fold(list, init, fn(a, b) -> a op b) =====
    if (e.callee == "fold") {
        if (e.args.size() != 3)
            codegenError("fold() takes exactly 3 arguments");
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Value *initVal = emitExpr(*e.args[1]);
        llvm::Value *lambdaVal = emitExpr(*e.args[2]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy) codegenError("fold() requires a list");
        auto fnIt = fn_type_info_.find(lambdaVal);
        if (fnIt == fn_type_info_.end())
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(lambdaVal))
                fnIt = fn_type_info_.find(load->getPointerOperand());
        if (fnIt == fn_type_info_.end())
            codegenError("fold() requires a function");
        auto &info = fnIt->second;
        if (info.paramTypes.size() != 2)
            codegenError("fold() function must take 2 parameters (accumulator, element)");
        if (info.returnType != initVal->getType())
            codegenError("fold() initial value type must match function return type");

        llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(listHeaderTy_, listVal, 0), "fold_len");
        llvm::Value *srcData = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(listHeaderTy_, listVal, 2), "fold_data");

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
        if (e.args.size() != 2)
            codegenError("any() takes exactly 2 arguments");
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Value *lambdaVal = emitExpr(*e.args[1]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy) codegenError("any() requires a list");
        auto fnIt = fn_type_info_.find(lambdaVal);
        if (fnIt == fn_type_info_.end())
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(lambdaVal))
                fnIt = fn_type_info_.find(load->getPointerOperand());
        if (fnIt == fn_type_info_.end())
            codegenError("any() requires a function");
        auto &info = fnIt->second;
        if (info.paramTypes.size() != 1)
            codegenError("any() predicate must take 1 parameter");
        if (info.returnType != i1Ty_)
            codegenError("any() predicate must return bool");

        llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(listHeaderTy_, listVal, 0), "any_len");
        llvm::Value *srcData = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(listHeaderTy_, listVal, 2), "any_data");

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
        if (e.args.size() != 2)
            codegenError("all() takes exactly 2 arguments");
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Value *lambdaVal = emitExpr(*e.args[1]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy) codegenError("all() requires a list");
        auto fnIt = fn_type_info_.find(lambdaVal);
        if (fnIt == fn_type_info_.end())
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(lambdaVal))
                fnIt = fn_type_info_.find(load->getPointerOperand());
        if (fnIt == fn_type_info_.end())
            codegenError("all() requires a function");
        auto &info = fnIt->second;
        if (info.paramTypes.size() != 1)
            codegenError("all() predicate must take 1 parameter");
        if (info.returnType != i1Ty_)
            codegenError("all() predicate must return bool");

        llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(listHeaderTy_, listVal, 0), "all_len");
        llvm::Value *srcData = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(listHeaderTy_, listVal, 2), "all_data");

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
        if (e.args.size() != 1)
            codegenError("sum() takes exactly 1 argument");
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy) codegenError("sum() requires a list");
        if (elemTy != i64Ty_ && elemTy != f64Ty_ && elemTy != i8Ty_)
            codegenError("sum() requires a numeric list (int, float, or byte)");

        llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(listHeaderTy_, listVal, 0), "sum_len");
        llvm::Value *srcData = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(listHeaderTy_, listVal, 2), "sum_data");

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
        if (e.args.size() != 1)
            codegenError(e.callee + "() takes exactly 1 argument");
        bool isMax = (e.callee == "max");
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy) codegenError(e.callee + "() requires a list");
        if (elemTy != i64Ty_ && elemTy != f64Ty_)
            codegenError(e.callee + "() requires a numeric list (int or float)");

        llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(listHeaderTy_, listVal, 0), "mm_len");
        llvm::Value *srcData = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(listHeaderTy_, listVal, 2), "mm_data");

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
        if (e.args.size() != 2)
            codegenError("tap() takes exactly 2 arguments");

        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Value *lambdaVal = emitExpr(*e.args[1]);

        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy)
            codegenError("tap() requires a list as first argument");

        auto fnIt = fn_type_info_.find(lambdaVal);
        if (fnIt == fn_type_info_.end()) {
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(lambdaVal))
                fnIt = fn_type_info_.find(load->getPointerOperand());
        }
        if (fnIt == fn_type_info_.end())
            codegenError("tap() requires a function as second argument");
        auto &info = fnIt->second;

        if (info.paramTypes.size() != 1)
            codegenError("tap() function must take exactly 1 argument");

        // Read source list
        llvm::Value *srcLenPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 0, "tap_src_len_ptr");
        llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, srcLenPtr, "tap_src_len");
        llvm::Value *srcDataPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 2, "tap_src_data_field");
        llvm::Value *srcData = builder_.CreateLoad(ptrTy_, srcDataPtr, "tap_src_data");

        // Loop: call fn on each element
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "tap_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "tap.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "tap.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "tap.end", fn_);

        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(condBB);
        llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "tap_iv");
        llvm::Value *cond = builder_.CreateICmpSLT(iVal, srcLen, "tap_cond");
        builder_.CreateCondBr(cond, bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "tap_ic");
        llvm::Value *srcElemPtr = builder_.CreateGEP(elemTy, srcData, {iCur}, "tap_elem_ptr");
        llvm::Value *srcElem = builder_.CreateLoad(elemTy, srcElemPtr, "tap_elem");
        emitLambdaCall(lambdaVal, info, {srcElem}, "tap_call");
        llvm::Value *iNext = builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1), "tap_next");
        builder_.CreateStore(iNext, iVar);
        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(endBB);
        return listVal;
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
        auto fnIt = fn_type_info_.find(compVal);
        if (fnIt == fn_type_info_.end()) {
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(compVal))
                fnIt = fn_type_info_.find(load->getPointerOperand());
        }
        if (fnIt == fn_type_info_.end())
            codegenError(callee + "() comparator must be a function");
        compInfo = fnIt->second;
        if (compInfo.paramTypes.size() != 2 || compInfo.returnType != i1Ty_)
            codegenError(callee + "() comparator must take 2 arguments and return bool");
    }

    // Read source list
    llvm::Value *srcLenPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 0, "sort_src_len_ptr");
    llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, srcLenPtr, "sort_src_len");
    llvm::Value *srcDataPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 2, "sort_src_data_field");
    llvm::Value *srcData = builder_.CreateLoad(ptrTy_, srcDataPtr, "sort_src_data");

    // Allocate new list and copy data
    auto mallocFn = getStdlibMalloc();
    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
    llvm::Value *newHeader = builder_.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "sort_header");

    uint64_t elemSz = dl.getTypeAllocSize(elemTy);
    llvm::Value *dataSize = builder_.CreateMul(srcLen, llvm::ConstantInt::get(i64Ty_, elemSz), "sort_data_size");
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

    auto timsortTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {ptrTy_, i64Ty_, i64Ty_, ptrTy_, ptrTy_}, false);
    auto timsortFn = mod_->getOrInsertFunction("__ry_timsort", timsortTy);
    builder_.CreateCall(timsortFn, {newData, srcLen, elemSizeConst, trampFn, cmpCtx});

    // Return sorted list
    list_element_types_[newHeader] = elemTy;
    return newHeader;
}
