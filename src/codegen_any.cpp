#include "ry/codegen.hpp"

bool CodeGen::isAnyType(llvm::Type *ty) const {
    return ty == anyTy_;
}

int64_t CodeGen::getAnyTypeTag(llvm::Type *ty) {
    if (ty == i64Ty_)  return TAG_INT;
    if (ty == f64Ty_)  return TAG_FLOAT;
    if (ty == i1Ty_)   return TAG_BOOL;
    if (ty == ptrTy_)  return TAG_STR;
    codegenError("type error: 'any' can only hold int/float/bool/str");
}

llvm::Value *CodeGen::wrapInAny(llvm::Value *val) {
    int64_t tag = getAnyTypeTag(val->getType());

    // alloca required: data field is [8 x i8], val type differs (type punning)
    llvm::AllocaInst *tmp = builder_.CreateAlloca(anyTy_, nullptr, "any.tmp");
    auto *tagPtr = builder_.CreateStructGEP(anyTy_, tmp, 0, "any.tag");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, tag), tagPtr);
    auto *dataPtr = builder_.CreateStructGEP(anyTy_, tmp, 1, "any.data");
    builder_.CreateStore(val, dataPtr);
    return builder_.CreateLoad(anyTy_, tmp, "any.val");
}

llvm::Value *CodeGen::unwrapFromAny(llvm::Value *anyVal, llvm::Type *targetTy) {
    int64_t expectedTag = getAnyTypeTag(targetTy);

    llvm::Value *tag = builder_.CreateExtractValue(anyVal, 0, "any.tag.val");
    llvm::Value *cmp = builder_.CreateICmpEQ(
        tag, llvm::ConstantInt::get(i64Ty_, expectedTag), "any.tag.check");

    llvm::Function *fn = builder_.GetInsertBlock()->getParent();
    auto *matchBB = llvm::BasicBlock::Create(*ctx_, "any.match", fn);
    auto *mismatchBB = llvm::BasicBlock::Create(*ctx_, "any.mismatch", fn);

    builder_.CreateCondBr(cmp, matchBB, mismatchBB);

    builder_.SetInsertPoint(mismatchBB);
    emitRuntimeError("runtime error: any type mismatch\n", ".any_type_err");

    // Use anyTy_ alloca for proper alignment when type-punning the data field
    builder_.SetInsertPoint(matchBB);
    llvm::AllocaInst *tmp = builder_.CreateAlloca(anyTy_, nullptr, "any.tmp");
    builder_.CreateStore(anyVal, tmp);
    auto *dataPtr = builder_.CreateStructGEP(anyTy_, tmp, 1, "any.data.ptr");
    return builder_.CreateLoad(targetTy, dataPtr, "any.unwrap.val");
}
