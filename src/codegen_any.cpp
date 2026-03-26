#include "ry/codegen.hpp"

#include <unordered_map>

bool CodeGen::isAnyType(llvm::Type *ty) const {
    return ty == anyTy_;
}

bool CodeGen::canAnyHoldType(llvm::Type *ty) const {
    return ty == i64Ty_ || ty == f64Ty_ || ty == i1Ty_ || ty == ptrTy_;
}

int64_t CodeGen::getAnyTypeTag(llvm::Type *ty) {
    if (ty == i64Ty_)  return TAG_INT;
    if (ty == f64Ty_)  return TAG_FLOAT;
    if (ty == i1Ty_)   return TAG_BOOL;
    if (ty == ptrTy_)  return TAG_STR;
    codegenError("type error: 'any' can only hold int/float/bool/str");
}

bool CodeGen::isNonStrPointer(llvm::Value *val) {
    if (val->getType() != ptrTy_) return false;

    // Collection types
    if (lookupCollectionType(list_element_types_, val)) return true;
    if (lookupCollectionType(map_key_types_, val)) return true;
    if (lookupCollectionType(map_value_types_, val)) return true;
    if (lookupCollectionType(set_element_types_, val)) return true;
    if (lookupCollectionType(nested_list_element_types_, val)) return true;
    if (lookupCollectionType(channel_element_types_, val)) return true;
    if (lookupCollectionType(iterator_element_types_, val)) return true;
    if (lookupCollectionType(task_result_types_, val)) return true;

    // Resource types
    if (isTcpListener(val)) return true;
    if (isTcpStream(val)) return true;
    if (isTlsStream(val)) return true;
    if (isHttpRequest(val)) return true;
    if (isHttpResponse(val)) return true;
    if (isHttpClientResponse(val)) return true;
    if (isJsonValue(val)) return true;

    // Function pointers
    if (fn_type_info_.count(val)) return true;
    if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val))
        if (fn_type_info_.count(load->getPointerOperand())) return true;

    return false;
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
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, tag), tagPtr);
    auto *dataPtr = builder_.CreateStructGEP(anyTy_, tmp, 1, "any.data");
    builder_.CreateStore(val, dataPtr);
    return builder_.CreateLoad(anyTy_, tmp, "any.val");
}

llvm::Value *CodeGen::buildUnitAny() {
    llvm::AllocaInst *tmp = builder_.CreateAlloca(anyTy_, nullptr, "any.unit.tmp");
    auto *tagPtr = builder_.CreateStructGEP(anyTy_, tmp, 0, "any.unit.tag");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, TAG_UNIT), tagPtr);
    auto *dataPtr = builder_.CreateStructGEP(anyTy_, tmp, 1, "any.unit.data");
    builder_.CreateStore(
        llvm::Constant::getNullValue(anyTy_->getElementType(1)),
        dataPtr);
    return builder_.CreateLoad(anyTy_, tmp, "any.unit.val");
}

llvm::Value *CodeGen::unwrapFromAny(llvm::Value *anyVal, llvm::Type *targetTy) {
    llvm::Value *tag = builder_.CreateExtractValue(anyVal, 0, "any.tag.val");
    llvm::Function *fn = builder_.GetInsertBlock()->getParent();

    // int→float auto-promotion: accept both TAG_FLOAT and TAG_INT
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
            tag, llvm::ConstantInt::get(i64Ty_, TAG_FLOAT), "is.float");
        builder_.CreateCondBr(isFloat, floatBB, checkIntBB);

        builder_.SetInsertPoint(checkIntBB);
        llvm::Value *isInt = builder_.CreateICmpEQ(
            tag, llvm::ConstantInt::get(i64Ty_, TAG_INT), "is.int");
        builder_.CreateCondBr(isInt, intPromoteBB, mismatchBB);

        builder_.SetInsertPoint(mismatchBB);
        emitRuntimeError("runtime error: any type mismatch\n", ".any_type_err");

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
        tag, llvm::ConstantInt::get(i64Ty_, expectedTag), "any.tag.check");

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

llvm::Value *CodeGen::emitAnyToString(llvm::Value *anyVal) {
    llvm::AllocaInst *tmp = builder_.CreateAlloca(anyTy_, nullptr, "any.ts");
    builder_.CreateStore(anyVal, tmp);
    llvm::FunctionType *fnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_}, false);
    llvm::FunctionCallee fn = mod_->getOrInsertFunction("__ry_any_to_string", fnTy);
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
