#include "ry/codegen.hpp"
#include <llvm/IR/Intrinsics.h>


namespace ry {

// ===== Checked/Saturating/Wrapping Arithmetic =====
//
// These builtins provide explicit overflow control for integer types (int, i8..i64, u8..u64).
// - checked_*  : returns Result<T, Error> (Err on overflow)
// - saturating_*: returns T (clamped to operand type's min/max on overflow)
// - wrapping_* : returns T (wraps, same as default +/-/*)

// Shared validation: both args must be the same integer type
void CodeGen::validateCheckedArithArgs(llvm::Value *lhs, llvm::Value *rhs,
                                        const std::string &callee) {
    if (lhs->getType() != rhs->getType())
        codegenError(callee + "() requires both arguments to be the same type");

    llvm::Type *ty = lhs->getType();
    if (ty->isFloatingPointTy())
        codegenError(callee + "() does not support floating-point types");

    if (!ty->isIntegerTy())
        codegenError(callee + "() requires integer type arguments (int, i8..i64, u8..u64)");

    // Treat bare int (empty metadata) as "int" for display and mix checking.
    // Without this, int+i64 would pass because only one side is empty.
    const std::string &lhsLL = getLowLevelTypeName(lhs);
    const std::string &rhsLL = getLowLevelTypeName(rhs);
    const std::string lhsName = lhsLL.empty() ? "int" : lhsLL;
    const std::string rhsName = rhsLL.empty() ? "int" : rhsLL;

    // Check type consistency: int+int is fine, i32+i32 is fine, int+i32 is not.
    if (lhsName != rhsName)
        codegenError(callee + "() cannot mix " + lhsName + " and " + rhsName);
}

// ===== checked_add / checked_sub / checked_mul =====

llvm::Value *CodeGen::emitCheckedArithmetic(const std::string &callee,
                                             llvm::Value *lhs, llvm::Value *rhs) {
    validateCheckedArithArgs(lhs, rhs, callee);
    bool isUnsigned = isUnsignedLowLevel(lhs) || isUnsignedLowLevel(rhs);

    // Select the appropriate overflow intrinsic
    llvm::Intrinsic::ID id;
    std::string op = callee.substr(8); // "add", "sub", "mul"
    if (op == "add") id = isUnsigned ? llvm::Intrinsic::uadd_with_overflow : llvm::Intrinsic::sadd_with_overflow;
    else if (op == "sub") id = isUnsigned ? llvm::Intrinsic::usub_with_overflow : llvm::Intrinsic::ssub_with_overflow;
    else id = isUnsigned ? llvm::Intrinsic::umul_with_overflow : llvm::Intrinsic::smul_with_overflow;

    llvm::Function *intrinsic = llvm::Intrinsic::getOrInsertDeclaration(mod_.get(), id, {lhs->getType()});
    llvm::Value *result = builder_.CreateCall(intrinsic, {lhs, rhs}, "checked");
    llvm::Value *value = builder_.CreateExtractValue(result, 0, "checked_val");
    llvm::Value *overflow = builder_.CreateExtractValue(result, 1, "overflow");

    // Build Result<T, Error>
    llvm::StructType *resTy = getResultType(lhs->getType(), errorTy_);
    return emitResultBranch(overflow, resTy,
        [&]() { return buildOkValue(value, resTy); },
        [&]() { return buildErrValue(buildStaticError("arithmetic overflow", ".err_checked_overflow"), resTy); });
}

// ===== saturating_add / saturating_sub / saturating_mul =====

llvm::Value *CodeGen::emitSaturatingArithmetic(const std::string &callee,
                                                llvm::Value *lhs, llvm::Value *rhs) {
    validateCheckedArithArgs(lhs, rhs, callee);
    bool isUnsigned = isUnsignedLowLevel(lhs) || isUnsignedLowLevel(rhs);
    std::string typeName = getLowLevelTypeName(lhs);
    if (typeName.empty()) typeName = getLowLevelTypeName(rhs);
    std::string op = callee.substr(11); // "add", "sub", "mul"

    llvm::Value *result = nullptr;

    if (op == "add" || op == "sub") {
        // LLVM has native saturating intrinsics for add/sub
        llvm::Intrinsic::ID id;
        if (op == "add") id = isUnsigned ? llvm::Intrinsic::uadd_sat : llvm::Intrinsic::sadd_sat;
        else id = isUnsigned ? llvm::Intrinsic::usub_sat : llvm::Intrinsic::ssub_sat;

        llvm::Function *intrinsic = llvm::Intrinsic::getOrInsertDeclaration(mod_.get(), id, {lhs->getType()});
        result = builder_.CreateCall(intrinsic, {lhs, rhs}, "sat");
    } else {
        // No LLVM intrinsic for saturating mul — use overflow detection + clamp
        llvm::Intrinsic::ID ovId = isUnsigned ? llvm::Intrinsic::umul_with_overflow
                                               : llvm::Intrinsic::smul_with_overflow;
        llvm::Function *intrinsic = llvm::Intrinsic::getOrInsertDeclaration(mod_.get(), ovId, {lhs->getType()});
        llvm::Value *mulResult = builder_.CreateCall(intrinsic, {lhs, rhs}, "satmul");
        llvm::Value *value = builder_.CreateExtractValue(mulResult, 0, "satmul_val");
        llvm::Value *overflow = builder_.CreateExtractValue(mulResult, 1, "satmul_ov");

        llvm::Type *ty = lhs->getType();
        unsigned bits = ty->getIntegerBitWidth();

        if (isUnsigned) {
            // Unsigned: clamp to UINT_MAX
            llvm::Value *maxVal = llvm::ConstantInt::get(ty, llvm::APInt::getMaxValue(bits));
            result = builder_.CreateSelect(overflow, maxVal, value, "satmul_res");
        } else {
            // Signed: clamp to INT_MAX or INT_MIN based on sign of operands.
            // XOR of operands has MSB=1 iff signs differ → product would be negative.
            llvm::Value *xorVal = builder_.CreateXor(lhs, rhs, "sign_xor");
            llvm::Value *isNeg = builder_.CreateICmpSLT(xorVal,
                llvm::ConstantInt::get(ty, 0), "sign_neg");
            llvm::Value *minVal = llvm::ConstantInt::get(ty, llvm::APInt::getSignedMinValue(bits));
            llvm::Value *maxVal = llvm::ConstantInt::get(ty, llvm::APInt::getSignedMaxValue(bits));
            llvm::Value *clampVal = builder_.CreateSelect(isNeg, minVal, maxVal, "satmul_clamp");
            result = builder_.CreateSelect(overflow, clampVal, value, "satmul_res");
        }
    }

    if (!typeName.empty())
        getOrCreateMeta(result).low_level_type_name = typeName;
    return result;
}

// ===== int overflow check (panic on overflow) =====

llvm::Value *CodeGen::emitIntOverflowCheck(llvm::Intrinsic::ID intrinsicId,
                                            llvm::Value *lhs, llvm::Value *rhs,
                                            const std::string &opName) {
    // Constant folding: if both operands are constants, compute at compile time
    if (auto *cl = llvm::dyn_cast<llvm::ConstantInt>(lhs)) {
        if (auto *cr = llvm::dyn_cast<llvm::ConstantInt>(rhs)) {
            llvm::APInt a = cl->getValue();
            llvm::APInt b = cr->getValue();
            bool overflow = false;
            llvm::APInt result;
            if (intrinsicId == llvm::Intrinsic::sadd_with_overflow)
                result = a.sadd_ov(b, overflow);
            else if (intrinsicId == llvm::Intrinsic::ssub_with_overflow)
                result = a.ssub_ov(b, overflow);
            else if (intrinsicId == llvm::Intrinsic::smul_with_overflow)
                result = a.smul_ov(b, overflow);
            else
                codegenError("internal: unsupported overflow intrinsic in emitIntOverflowCheck");
            if (overflow)
                codegenError("integer overflow");
            return llvm::ConstantInt::get(lhs->getType(), result);
        }
    }

    llvm::Function *intrinsic = llvm::Intrinsic::getOrInsertDeclaration(
        mod_.get(), intrinsicId, {lhs->getType()});
    llvm::Value *result = builder_.CreateCall(intrinsic, {lhs, rhs}, opName + "_ov");
    llvm::Value *value = builder_.CreateExtractValue(result, 0, opName + "_val");
    llvm::Value *overflow = builder_.CreateExtractValue(result, 1, opName + "_flag");

    llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, opName + ".overflow_err", fn_);
    llvm::BasicBlock *okBB  = llvm::BasicBlock::Create(*ctx_, opName + ".ok", fn_);
    builder_.CreateCondBr(overflow, errBB, okBB);

    builder_.SetInsertPoint(errBB);
    emitRuntimeError("runtime error: integer overflow\n",
                      ".int_overflow_err_" + std::to_string(overflow_err_counter_++));

    builder_.SetInsertPoint(okBB);
    return value;
}

// ===== wrapping_add / wrapping_sub / wrapping_mul =====

llvm::Value *CodeGen::emitWrappingArithmetic(const std::string &callee,
                                              llvm::Value *lhs, llvm::Value *rhs) {
    validateCheckedArithArgs(lhs, rhs, callee);
    std::string typeName = getLowLevelTypeName(lhs);
    if (typeName.empty()) typeName = getLowLevelTypeName(rhs);
    std::string op = callee.substr(9); // "add", "sub", "mul"

    llvm::Value *result;
    if (op == "add") result = builder_.CreateAdd(lhs, rhs, "wrap_add");
    else if (op == "sub") result = builder_.CreateSub(lhs, rhs, "wrap_sub");
    else result = builder_.CreateMul(lhs, rhs, "wrap_mul");

    if (!typeName.empty())
        getOrCreateMeta(result).low_level_type_name = typeName;
    return result;
}

} // namespace ry
