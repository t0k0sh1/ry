#include "ry/codegen.hpp"

llvm::Value *CodeGen::emitArcAlloc(llvm::Value *dataSize) {
    auto *headerSize = llvm::ConstantInt::get(i64Ty_, ARC_HEADER_SIZE);
    auto *totalSize = builder_.CreateAdd(dataSize, headerSize, "arc_total");
    auto *headerPtr = builder_.CreateCall(getStdlibMalloc(), {totalSize}, "arc_hdr");

    auto *strongPtr = builder_.CreateStructGEP(arcHeaderTy_, headerPtr, 0, "arc_strong_ptr");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 1), strongPtr);

    auto *weakPtr = builder_.CreateStructGEP(arcHeaderTy_, headerPtr, 1, "arc_weak_ptr");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), weakPtr);

    return headerPtr;
}

llvm::Value *CodeGen::emitArcGetDataPtr(llvm::Value *headerPtr) {
    return builder_.CreateGEP(i8Ty_, headerPtr,
                              llvm::ConstantInt::get(i64Ty_, ARC_HEADER_SIZE),
                              "arc_data");
}

void CodeGen::emitArcRetain(llvm::Value *headerPtr, bool atomic) {
    auto *strongPtr = builder_.CreateStructGEP(arcHeaderTy_, headerPtr, 0, "arc_retain_ptr");

    if (atomic) {
        builder_.CreateAtomicRMW(llvm::AtomicRMWInst::Add, strongPtr,
                                 llvm::ConstantInt::get(i64Ty_, 1),
                                 llvm::MaybeAlign(),
                                 llvm::AtomicOrdering::SequentiallyConsistent);
    } else {
        auto *cur = builder_.CreateLoad(i64Ty_, strongPtr, "arc_strong");
        auto *inc = builder_.CreateAdd(cur, llvm::ConstantInt::get(i64Ty_, 1), "arc_inc");
        builder_.CreateStore(inc, strongPtr);
    }
}

void CodeGen::emitArcRelease(llvm::Value *headerPtr, bool atomic,
                              llvm::FunctionCallee destructor) {
    auto *strongPtr = builder_.CreateStructGEP(arcHeaderTy_, headerPtr, 0, "arc_rel_ptr");

    llvm::Value *isZero;
    if (atomic) {
        // atomicrmw returns the OLD value; object is dead when old == 1
        auto *old = builder_.CreateAtomicRMW(
            llvm::AtomicRMWInst::Sub, strongPtr,
            llvm::ConstantInt::get(i64Ty_, 1),
            llvm::MaybeAlign(),
            llvm::AtomicOrdering::SequentiallyConsistent);
        isZero = builder_.CreateICmpEQ(old, llvm::ConstantInt::get(i64Ty_, 1), "arc_dead");
    } else {
        auto *cur = builder_.CreateLoad(i64Ty_, strongPtr, "arc_strong");
        auto *dec = builder_.CreateSub(cur, llvm::ConstantInt::get(i64Ty_, 1), "arc_dec");
        builder_.CreateStore(dec, strongPtr);
        isZero = builder_.CreateICmpEQ(dec, llvm::ConstantInt::get(i64Ty_, 0), "arc_dead");
    }

    auto *fn = builder_.GetInsertBlock()->getParent();
    auto *freeBB = llvm::BasicBlock::Create(*ctx_, "arc.release", fn);
    auto *doneBB = llvm::BasicBlock::Create(*ctx_, "arc.done", fn);

    builder_.CreateCondBr(isZero, freeBB, doneBB);

    builder_.SetInsertPoint(freeBB);
    if (destructor) {
        auto *dataPtr = emitArcGetDataPtr(headerPtr);
        builder_.CreateCall(destructor, {dataPtr});
    }
    // Only free the entire block when no weak references remain.
    // When weak_count > 0, the header must stay alive for weak ref resolution.
    auto *weakPtr = builder_.CreateStructGEP(arcHeaderTy_, headerPtr, 1, "arc_weak_ptr");
    auto *weakCount = builder_.CreateLoad(i64Ty_, weakPtr, "arc_weak");
    auto *noWeak = builder_.CreateICmpEQ(weakCount, llvm::ConstantInt::get(i64Ty_, 0), "arc_no_weak");

    auto *realFreeBB = llvm::BasicBlock::Create(*ctx_, "arc.free", fn);
    auto *skipFreeBB = llvm::BasicBlock::Create(*ctx_, "arc.skip_free", fn);
    builder_.CreateCondBr(noWeak, realFreeBB, skipFreeBB);

    builder_.SetInsertPoint(realFreeBB);
    builder_.CreateCall(getStdlibFree(), {headerPtr});
    builder_.CreateBr(doneBB);

    builder_.SetInsertPoint(skipFreeBB);
    builder_.CreateBr(doneBB);

    builder_.SetInsertPoint(doneBB);
}

bool CodeGen::isArcAtomic(llvm::Value *val) const {
    if (arc_atomic_values_.count(val))
        return true;
    if (auto *stripped = val->stripPointerCasts())
        if (arc_atomic_values_.count(stripped))
            return true;
    if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
        auto *ptr = load->getPointerOperand();
        if (arc_atomic_values_.count(ptr))
            return true;
        if (auto *strippedPtr = ptr->stripPointerCasts())
            return arc_atomic_values_.count(strippedPtr) > 0;
    }
    return false;
}

void CodeGen::markArcAtomic(llvm::Value *val) {
    arc_atomic_values_.insert(val);
}
