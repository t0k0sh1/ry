#include "ry/codegen.hpp"
#include <llvm/IR/Intrinsics.h>

// ===== Checked/Saturating/Wrapping Arithmetic =====
//
// These builtins provide explicit overflow control for low-level integer types.
// - checked_*  : returns Result<T, Error> (Err on overflow)
// - saturating_*: returns T (clamped to min/max on overflow)
// - wrapping_* : returns T (wraps, same as default +/-/*)

// Shared validation: both args must be the same low-level integer type
void CodeGen::validateCheckedArithArgs(llvm::Value *lhs, llvm::Value *rhs,
                                        const std::string &callee) {
    if (lhs->getType() != rhs->getType())
        codegenError(callee + "() requires both arguments to be the same type");

    llvm::Type *ty = lhs->getType();
    if (ty->isFloatingPointTy())
        codegenError(callee + "() does not support floating-point types");

    if (!ty->isIntegerTy())
        codegenError(callee + "() requires low-level integer type arguments");

    const std::string &lhsName = getLowLevelTypeName(lhs);
    const std::string &rhsName = getLowLevelTypeName(rhs);

    // For i64 type, require metadata to distinguish i64/u64 from high-level int
    if (ty->isIntegerTy(64) && lhsName.empty() && rhsName.empty())
        codegenError(callee + "() requires low-level integer type arguments, not int");

    // Check signed/unsigned consistency
    if (!lhsName.empty() && !rhsName.empty() && lhsName != rhsName)
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
        low_level_type_names_[result] = typeName;
    return result;
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
        low_level_type_names_[result] = typeName;
    return result;
}
