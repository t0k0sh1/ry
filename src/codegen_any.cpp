#include "ry/codegen.hpp"


namespace ry {

bool CodeGen::isAnyType(llvm::Type *ty) const {
    return ty == anyTy_;
}

bool CodeGen::canAnyHoldType(llvm::Type *ty) const {
    return ty == i64Ty_ || ty == f64Ty_ || ty == i1Ty_ || ty == ptrTy_;
}

int64_t CodeGen::getAnyTypeTag(llvm::Type *ty) {
    if (ty == i64Ty_)  return static_cast<int64_t>(RyAnyTag::Int);
    if (ty == f64Ty_)  return static_cast<int64_t>(RyAnyTag::Float);
    if (ty == i1Ty_)   return static_cast<int64_t>(RyAnyTag::Bool);
    if (ty == ptrTy_)  return static_cast<int64_t>(RyAnyTag::Str);
    codegenError("type error: 'any' can only hold int/float/bool/str");
}

bool CodeGen::isNonStrPointer(llvm::Value *val) {
    if (val->getType() != ptrTy_) return false;
    auto *meta = getMeta(val);
    return meta && meta->hasAnyMeta();
}

bool CodeGen::isStringValue(llvm::Value *val) {
    return val->getType() == ptrTy_ && !isNonStrPointer(val);
}

llvm::Value *CodeGen::wrapInAny(llvm::Value *val) {
    if (isNonStrPointer(val))
        codegenError("type error: 'any' can only hold int/float/bool/str; "
                     "non-str pointer types (collections, resources, function "
                     "pointers, etc.) are not supported");

    int64_t tag = getAnyTypeTag(val->getType());

    // Bool (i1) must be zero-extended to i64 so that the runtime's 8-byte
    // memcpy reads a well-defined 0/1 value instead of uninitialized bytes.
    if (val->getType()->isIntegerTy(1))
        val = builder_.CreateZExt(val, i64Ty_, "any.bool.zext");

    // alloca required: data field is [8 x i8], val type differs (type punning)
    llvm::AllocaInst *tmp = builder_.CreateAlloca(anyTy_, nullptr, "any.tmp");
    auto *tagPtr = builder_.CreateStructGEP(anyTy_, tmp, 0, "any.tag");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(tag)), tagPtr);
    auto *dataPtr = builder_.CreateStructGEP(anyTy_, tmp, 1, "any.data");
    builder_.CreateStore(val, dataPtr);
    return builder_.CreateLoad(anyTy_, tmp, "any.val");
}

llvm::Value *CodeGen::buildUnitAny() {
    llvm::AllocaInst *tmp = builder_.CreateAlloca(anyTy_, nullptr, "any.unit.tmp");
    auto *tagPtr = builder_.CreateStructGEP(anyTy_, tmp, 0, "any.unit.tag");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, static_cast<int64_t>(RyAnyTag::Unit)), tagPtr);
    auto *dataPtr = builder_.CreateStructGEP(anyTy_, tmp, 1, "any.unit.data");
    builder_.CreateStore(
        llvm::Constant::getNullValue(anyTy_->getElementType(1)),
        dataPtr);
    return builder_.CreateLoad(anyTy_, tmp, "any.unit.val");
}

llvm::Value *CodeGen::unwrapFromAny(llvm::Value *anyVal, llvm::Type *targetTy) {
    llvm::Value *tag = builder_.CreateExtractValue(anyVal, 0, "any.tag.val");
    llvm::Function *fn = builder_.GetInsertBlock()->getParent();

    // int→float auto-promotion: accept both static_cast<int64_t>(RyAnyTag::Float) and static_cast<int64_t>(RyAnyTag::Int)
    if (targetTy == f64Ty_) {
        auto *floatBB = llvm::BasicBlock::Create(*ctx_, "any.float", fn);
        auto *checkIntBB = llvm::BasicBlock::Create(*ctx_, "any.check_int", fn);
        auto *intPromoteBB = llvm::BasicBlock::Create(*ctx_, "any.int2float", fn);
        auto *mismatchBB = llvm::BasicBlock::Create(*ctx_, "any.mismatch", fn);
        auto *mergeBB = llvm::BasicBlock::Create(*ctx_, "any.merge", fn);

        // Shared alloca for type-punning the data field (used by both branches)
        llvm::AllocaInst *tmp = builder_.CreateAlloca(anyTy_, nullptr, "any.tmp.fp");
        builder_.CreateStore(anyVal, tmp);
        auto *dataPtr = builder_.CreateStructGEP(anyTy_, tmp, 1, "any.data.fp");

        llvm::Value *isFloat = builder_.CreateICmpEQ(
            tag, llvm::ConstantInt::get(i64Ty_, static_cast<int64_t>(RyAnyTag::Float)), "is.float");
        builder_.CreateCondBr(isFloat, floatBB, checkIntBB);

        builder_.SetInsertPoint(checkIntBB);
        llvm::Value *isInt = builder_.CreateICmpEQ(
            tag, llvm::ConstantInt::get(i64Ty_, static_cast<int64_t>(RyAnyTag::Int)), "is.int");
        builder_.CreateCondBr(isInt, intPromoteBB, mismatchBB);

        builder_.SetInsertPoint(mismatchBB);
        emitRuntimeError("runtime error: any type mismatch (expected float or int)\n", ".any_type_err");

        builder_.SetInsertPoint(floatBB);
        llvm::Value *floatVal = builder_.CreateLoad(f64Ty_, dataPtr, "any.f64");
        builder_.CreateBr(mergeBB);

        builder_.SetInsertPoint(intPromoteBB);
        llvm::Value *intVal = builder_.CreateLoad(i64Ty_, dataPtr, "any.i64");
        llvm::Value *promoted = builder_.CreateSIToFP(intVal, f64Ty_, "any.i2f");
        builder_.CreateBr(mergeBB);

        builder_.SetInsertPoint(mergeBB);
        llvm::PHINode *phi = builder_.CreatePHI(f64Ty_, 2, "any.unwrap.f64");
        phi->addIncoming(floatVal, floatBB);
        phi->addIncoming(promoted, intPromoteBB);
        return phi;
    }

    // Standard 2-way: exact tag match or error
    int64_t expectedTag = getAnyTypeTag(targetTy);
    llvm::Value *cmp = builder_.CreateICmpEQ(
        tag, llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(expectedTag)), "any.tag.check");

    auto *matchBB = llvm::BasicBlock::Create(*ctx_, "any.match", fn);
    auto *mismatchBB = llvm::BasicBlock::Create(*ctx_, "any.mismatch", fn);

    builder_.CreateCondBr(cmp, matchBB, mismatchBB);

    builder_.SetInsertPoint(mismatchBB);
    std::string typeName = (targetTy == i64Ty_) ? "int"
                         : (targetTy == i1Ty_)  ? "bool"
                         : (targetTy == ptrTy_) ? "str"
                                                : "unknown";
    emitRuntimeError("runtime error: any type mismatch (expected " + typeName + ")\n",
                     ".any_type_err");

    // Use anyTy_ alloca for proper alignment when type-punning the data field
    builder_.SetInsertPoint(matchBB);
    llvm::AllocaInst *tmp = builder_.CreateAlloca(anyTy_, nullptr, "any.tmp");
    builder_.CreateStore(anyVal, tmp);
    auto *dataPtr = builder_.CreateStructGEP(anyTy_, tmp, 1, "any.data.ptr");
    return builder_.CreateLoad(targetTy, dataPtr, "any.unwrap.val");
}

llvm::Value *CodeGen::emitAnyToString(llvm::Value *anyVal, bool inCollection) {
    llvm::AllocaInst *tmp = builder_.CreateAlloca(anyTy_, nullptr, "any.ts");
    builder_.CreateStore(anyVal, tmp);
    llvm::FunctionType *fnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_}, false);
    const char *fnName = inCollection ? "__ry_any_to_string_in_collection"
                                      : "__ry_any_to_string";
    llvm::FunctionCallee fn = mod_->getOrInsertFunction(fnName, fnTy);
    return builder_.CreateCall(fn, {tmp}, "any.ts.str");
}

llvm::Value *CodeGen::emitAnyBinaryOp(const std::string &op,
                                       llvm::Value *lhs, llvm::Value *rhs) {
    llvm::AllocaInst *lhsPtr = builder_.CreateAlloca(anyTy_, nullptr, "any.lhs");
    builder_.CreateStore(lhs, lhsPtr);
    llvm::AllocaInst *rhsPtr = builder_.CreateAlloca(anyTy_, nullptr, "any.rhs");
    builder_.CreateStore(rhs, rhsPtr);

    static const std::unordered_map<std::string, std::string> arithOps = {
        {"+", "__ry_any_add"}, {"-", "__ry_any_sub"},
        {"*", "__ry_any_mul"}, {"/", "__ry_any_div"},
        {"%", "__ry_any_mod"}, {"//", "__ry_any_floordiv"},
        {"**", "__ry_any_pow"},
    };
    static const std::unordered_map<std::string, std::string> cmpOps = {
        {"==", "__ry_any_eq"}, {"!=", "__ry_any_ne"},
        {"<", "__ry_any_lt"},  {"<=", "__ry_any_le"},
        {">", "__ry_any_gt"},  {">=", "__ry_any_ge"},
    };

    auto ait = arithOps.find(op);
    if (ait != arithOps.end()) {
        llvm::AllocaInst *resultPtr = builder_.CreateAlloca(anyTy_, nullptr, "any.result");
        llvm::FunctionType *fnTy = llvm::FunctionType::get(
            builder_.getVoidTy(), {ptrTy_, ptrTy_, ptrTy_}, false);
        llvm::FunctionCallee fn = mod_->getOrInsertFunction(ait->second, fnTy);
        builder_.CreateCall(fn, {resultPtr, lhsPtr, rhsPtr});
        return builder_.CreateLoad(anyTy_, resultPtr, "any.binop");
    }

    auto cit = cmpOps.find(op);
    if (cit != cmpOps.end()) {
        llvm::FunctionType *fnTy = llvm::FunctionType::get(
            i64Ty_, {ptrTy_, ptrTy_}, false);
        llvm::FunctionCallee fn = mod_->getOrInsertFunction(cit->second, fnTy);
        llvm::Value *result = builder_.CreateCall(fn, {lhsPtr, rhsPtr}, "any.cmp");
        llvm::Value *zero = builder_.getInt64(0);
        return builder_.CreateICmpNE(result, zero, "any.cmp.bool");
    }

    codegenError("operator '" + op + "' not supported for any type");
}

llvm::Value *CodeGen::emitAnyUnaryNeg(llvm::Value *operand) {
    llvm::AllocaInst *opPtr = builder_.CreateAlloca(anyTy_, nullptr, "any.neg.op");
    builder_.CreateStore(operand, opPtr);
    llvm::AllocaInst *resultPtr = builder_.CreateAlloca(anyTy_, nullptr, "any.neg.result");

    llvm::FunctionType *fnTy = llvm::FunctionType::get(
        builder_.getVoidTy(), {ptrTy_, ptrTy_}, false);
    llvm::FunctionCallee fn = mod_->getOrInsertFunction("__ry_any_neg", fnTy);
    builder_.CreateCall(fn, {resultPtr, opPtr});
    return builder_.CreateLoad(anyTy_, resultPtr, "any.neg");
}

} // namespace ry
