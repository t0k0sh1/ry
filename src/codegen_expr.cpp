#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"
#include <climits>

// Range check for suffixed integer literals.
// Since `-128i8` is parsed as UnaryExpr("-", NumberExpr{128, "i8"}),
// we allow positive values up to 2^(N-1) for signed types (= |MIN|).
template<typename ErrorFn>
static void validateIntRange(int64_t value, const std::string &suffix,
                             ErrorFn error) {
    if (suffix == "i8") {
        if (value < INT8_MIN || value > (int64_t)(-((int64_t)INT8_MIN)))
            error("i8 literal out of range: " + std::to_string(value));
    } else if (suffix == "i16") {
        if (value < INT16_MIN || value > (int64_t)(-((int64_t)INT16_MIN)))
            error("i16 literal out of range: " + std::to_string(value));
    } else if (suffix == "i32") {
        if (value < INT32_MIN || value > (int64_t)(-((int64_t)INT32_MIN)))
            error("i32 literal out of range: " + std::to_string(value));
    } else if (suffix == "u8") {
        if (value < 0 || value > UINT8_MAX)
            error("u8 literal out of range: " + std::to_string(value));
    } else if (suffix == "u16") {
        if (value < 0 || value > UINT16_MAX)
            error("u16 literal out of range: " + std::to_string(value));
    } else if (suffix == "u32") {
        if (value < 0 || value > (int64_t)UINT32_MAX)
            error("u32 literal out of range: " + std::to_string(value));
    } else if (suffix == "u64") {
        if (value < 0)
            error("u64 literal out of range: " + std::to_string(value));
    }
}

llvm::Value *CodeGen::emitExpr(const ExprNode &node) {
    if (node.loc.isValid()) current_loc_ = node.loc;
    return std::visit([this](const auto &e) -> llvm::Value* { return emitExprVariant(e); },
                      node.data);
}

llvm::Value *CodeGen::emitExprVariant(const NumberExpr &e) {
    if (e.suffix.empty())
        return llvm::ConstantInt::get(i64Ty_, e.value, true);

    validateIntRange(e.value, e.suffix,
        [this](const std::string &msg) { codegenError(msg); });

    llvm::Type *ty = resolveType(e.suffix);
    bool isSigned = !isUnsignedLowLevelName(e.suffix);
    auto *result = llvm::ConstantInt::get(ty, static_cast<uint64_t>(e.value), isSigned);
    // Do NOT set low_level_type_names_ on ConstantInt: LLVM's constant uniquing
    // shares the same pointer for identical (type, value) pairs, causing metadata
    // corruption when different suffixes map to the same constant (#311).
    // Suffix info is propagated via AST (getExprLowLevelSuffix) instead.
    return result;
}

llvm::Value *CodeGen::emitExprVariant(const FloatExpr &e) {
    if (e.suffix.empty() || e.suffix == "f64")
        return llvm::ConstantFP::get(f64Ty_, e.value);
    // suffix == "f32"
    auto *result = llvm::ConstantFP::get(f32Ty_, e.value);
    // Do NOT set low_level_type_names_ on ConstantFP (same reason as NumberExpr, #311).
    return result;
}

llvm::Value *CodeGen::emitExprVariant(const BoolExpr &e) {
    return llvm::ConstantInt::get(i1Ty_, e.value ? 1 : 0, false);
}

llvm::Value *CodeGen::emitExprVariant(const StringExpr &e) {
    return builder_.CreateGlobalString(e.value, ".str");
}

llvm::Value *CodeGen::emitExprVariant(const VariableExpr &e) {
    llvm::AllocaInst *alloca = findVar(e.name);
    if (alloca) {
        if (deprecated_variables_.count(e.name))
            emitDeprecationWarning(e.name);
        llvm::Type *ty = alloca->getAllocatedType();
        return builder_.CreateLoad(ty, alloca, e.name);
    }
    // Check native constants (PI, E, Inf, NaN) after local scope
    if (!native_constants_.empty() && native_constants_.count(e.name))
        return emitNativeConstant(e.name);
    // Try named function reference
    auto fit = functions_.find(e.name);
    if (fit != functions_.end() && fit->second.size() == 1) {
        if (deprecated_functions_.count(e.name))
            emitDeprecationWarning(e.name);
        llvm::Function *func = fit->second[0].func;
        FnTypeInfo info;
        info.paramTypes = fit->second[0].paramTypes;
        info.paramTypeNames = fit->second[0].paramTypeNames;
        info.returnType = func->getReturnType();
        fn_type_info_[func] = info;
        return func;
    }
    codegenError("undefined variable: " + e.name);
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<UnaryExpr> &e) {
    llvm::Value *val = emitExpr(*e->operand);

    // Try user-defined unary operator first
    std::string opFnName = "operator" + e->op;
    if (auto *result = tryUnaryOperatorCall(opFnName, val))
        return result;

    // any-type unary dispatch (#223)
    if (isAnyType(val->getType())) {
        if (e->op == "-") return emitAnyUnaryNeg(val);
        if (e->op == "+") return val;
        codegenError("operator '" + e->op + "' not supported for any type");
    }

    if (e->op == "+") {
        return val;
    }
    if (e->op == "-") {
        if (val->getType()->isDoubleTy() || val->getType()->isFloatTy())
            return builder_.CreateFNeg(val, "fneg");
        // Check for unsigned suffix on the operand AST node
        if (auto *ne = std::get_if<NumberExpr>(&e->operand->data)) {
            if (isUnsignedLowLevelName(ne->suffix))
                codegenError("cannot negate unsigned type '" + ne->suffix + "'");
        }
        if (isUnsignedLowLevel(val))
            codegenError("cannot negate unsigned type '" + getLowLevelTypeName(val) + "'");
        val = promoteToInt(val);
        return builder_.CreateNeg(val, "neg");
    }
    if (e->op == "not") {
        llvm::Value *boolVal = toBool(val);
        return builder_.CreateNot(boolVal, "not");
    }
    if (e->op == "~") {
        if (val->getType()->isDoubleTy())
            codegenError("bitwise NOT (~) requires integer, got float");
        val = promoteToInt(val);
        return builder_.CreateNot(val, "bnot");
    }
    codegenError("unknown unary operator: " + e->op);
}

// ===== Operator overload helpers =====

llvm::Value *CodeGen::tryOperatorCall(const std::string &opFnName,
                                       llvm::Value *lhs, llvm::Value *rhs) {
    auto fit = functions_.find(opFnName);
    if (fit == functions_.end())
        return nullptr;

    llvm::Type *lhsTy = lhs->getType();
    llvm::Type *rhsTy = rhs->getType();

    for (auto &entry : fit->second) {
        if (entry.paramTypes.size() == 2 &&
            entry.paramTypes[0] == lhsTy &&
            entry.paramTypes[1] == rhsTy) {
            if (entry.func->getReturnType()->isVoidTy())
                return builder_.CreateCall(entry.func, {lhs, rhs});
            return builder_.CreateCall(entry.func, {lhs, rhs}, "opcall");
        }
    }
    return nullptr;
}

llvm::Value *CodeGen::tryUnaryOperatorCall(const std::string &opFnName,
                                            llvm::Value *operand) {
    auto fit = functions_.find(opFnName);
    if (fit == functions_.end())
        return nullptr;

    llvm::Type *opTy = operand->getType();

    for (auto &entry : fit->second) {
        if (entry.paramTypes.size() == 1 &&
            entry.paramTypes[0] == opTy) {
            if (entry.func->getReturnType()->isVoidTy())
                return builder_.CreateCall(entry.func, {operand});
            return builder_.CreateCall(entry.func, {operand}, "opcall");
        }
    }
    return nullptr;
}

// ===== B2: BinaryExpr sub-dispatchers =====

llvm::Value *CodeGen::emitComparisonOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs,
                                        const std::string &llNameHint) {
    // Option type comparison with none: check has_value flag only
    // Only allowed when at least one side is Option and both sides are Option
    // (none is also an Option value with has_value=false)
    bool lhsIsOpt = isOptionType(lhs->getType());
    bool rhsIsOpt = isOptionType(rhs->getType());
    if (lhsIsOpt && rhsIsOpt && (op == "==" || op == "!=")) {
        // Only support comparison with none (has_value == false on one side)
        // Extract has_value flags from both
        llvm::Value *lhsFlag = builder_.CreateExtractValue(lhs, 0, "lhs_has");
        llvm::Value *rhsFlag = builder_.CreateExtractValue(rhs, 0, "rhs_has");
        if (op == "==") return builder_.CreateICmpEQ(lhsFlag, rhsFlag, "opt_eq");
        return builder_.CreateICmpNE(lhsFlag, rhsFlag, "opt_ne");
    }

    // Record (struct) type comparison: field-by-field (only == and != supported)
    if (op == "==" || op == "!=") {
        auto *lhsST = llvm::dyn_cast<llvm::StructType>(lhs->getType());
        auto *rhsST = llvm::dyn_cast<llvm::StructType>(rhs->getType());
        if (lhsST && rhsST && lhsST == rhsST) {
            std::string typeName = lhsST->getName().str();
            auto it = struct_types_.find(typeName);
            if (it != struct_types_.end())
                return emitStructComparison(op, lhs, rhs, it->second);
        }
    }

    // String comparison via strcmp
    if (lhs->getType() == ptrTy_ && rhs->getType() == ptrTy_) {
        auto strcmpFn = getStdlibStrcmp();
        llvm::Value *cmp = builder_.CreateCall(strcmpFn, {lhs, rhs}, "strcmp");
        llvm::Value *zero = llvm::ConstantInt::get(i32Ty_, 0);
        if (op == "==") return builder_.CreateICmpEQ(cmp, zero, "str_eq");
        if (op == "!=") return builder_.CreateICmpNE(cmp, zero, "str_ne");
        if (op == "<")  return builder_.CreateICmpSLT(cmp, zero, "str_lt");
        if (op == "<=") return builder_.CreateICmpSLE(cmp, zero, "str_le");
        if (op == ">")  return builder_.CreateICmpSGT(cmp, zero, "str_gt");
        if (op == ">=") return builder_.CreateICmpSGE(cmp, zero, "str_ge");
        codegenError("unsupported string comparison: " + op);
    }

    // Low-level type mix check
    checkLowLevelTypeMix(lhs, rhs, op);

    // Low-level type native-width comparison
    if (isLowLevelTy(lhs) && lhs->getType() == rhs->getType()) {
        if (isLowLevelFloatTy(lhs->getType())) {
            llvm::CmpInst::Predicate pred;
            if      (op == "==") pred = llvm::CmpInst::FCMP_OEQ;
            else if (op == "!=") pred = llvm::CmpInst::FCMP_ONE;
            else if (op == "<")  pred = llvm::CmpInst::FCMP_OLT;
            else if (op == "<=") pred = llvm::CmpInst::FCMP_OLE;
            else if (op == ">")  pred = llvm::CmpInst::FCMP_OGT;
            else                 pred = llvm::CmpInst::FCMP_OGE;
            return builder_.CreateFCmp(pred, lhs, rhs, "fcmp_ll");
        }
        bool isUnsigned = isUnsignedLowLevel(lhs) || isUnsignedLowLevelName(llNameHint);
        llvm::CmpInst::Predicate pred;
        if      (op == "==") pred = llvm::CmpInst::ICMP_EQ;
        else if (op == "!=") pred = llvm::CmpInst::ICMP_NE;
        else if (op == "<")  pred = isUnsigned ? llvm::CmpInst::ICMP_ULT : llvm::CmpInst::ICMP_SLT;
        else if (op == "<=") pred = isUnsigned ? llvm::CmpInst::ICMP_ULE : llvm::CmpInst::ICMP_SLE;
        else if (op == ">")  pred = isUnsigned ? llvm::CmpInst::ICMP_UGT : llvm::CmpInst::ICMP_SGT;
        else                 pred = isUnsigned ? llvm::CmpInst::ICMP_UGE : llvm::CmpInst::ICMP_SGE;
        return builder_.CreateICmp(pred, lhs, rhs, "icmp_ll");
    }

    lhs = promoteToInt(lhs);
    rhs = promoteToInt(rhs);

    bool lf = lhs->getType()->isDoubleTy();
    bool rf = rhs->getType()->isDoubleTy();
    if (lf || rf) {
        std::tie(lhs, rhs) = promoteToFloat(lhs, rhs);
        llvm::CmpInst::Predicate pred;
        if      (op == "==") pred = llvm::CmpInst::FCMP_OEQ;
        else if (op == "!=") pred = llvm::CmpInst::FCMP_ONE;
        else if (op == "<")  pred = llvm::CmpInst::FCMP_OLT;
        else if (op == "<=") pred = llvm::CmpInst::FCMP_OLE;
        else if (op == ">")  pred = llvm::CmpInst::FCMP_OGT;
        else                 pred = llvm::CmpInst::FCMP_OGE;
        return builder_.CreateFCmp(pred, lhs, rhs, "fcmp");
    }
    llvm::CmpInst::Predicate pred;
    if      (op == "==") pred = llvm::CmpInst::ICMP_EQ;
    else if (op == "!=") pred = llvm::CmpInst::ICMP_NE;
    else if (op == "<")  pred = llvm::CmpInst::ICMP_SLT;
    else if (op == "<=") pred = llvm::CmpInst::ICMP_SLE;
    else if (op == ">")  pred = llvm::CmpInst::ICMP_SGT;
    else                 pred = llvm::CmpInst::ICMP_SGE;
    return builder_.CreateICmp(pred, lhs, rhs, "icmp");
}

llvm::Value *CodeGen::emitStructComparison(const std::string &op, llvm::Value *lhs,
                                            llvm::Value *rhs, const StructInfo &info) {
    llvm::Value *result = llvm::ConstantInt::getTrue(*ctx_);
    for (unsigned i = 0; i < info.fields.size(); ++i) {
        llvm::Value *fieldL = builder_.CreateExtractValue(lhs, i, "l." + info.fields[i].name);
        llvm::Value *fieldR = builder_.CreateExtractValue(rhs, i, "r." + info.fields[i].name);
        llvm::Value *fieldEq = emitComparisonOp("==", fieldL, fieldR, "");
        result = builder_.CreateAnd(result, fieldEq, "and.eq");
    }
    if (op == "!=")
        return builder_.CreateNot(result, "struct_ne");
    return result;
}

llvm::Value *CodeGen::emitBitwiseOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs,
                                     const std::string &llNameHint) {
    if (lhs->getType()->isDoubleTy() || rhs->getType()->isDoubleTy() ||
        isLowLevelFloatTy(lhs->getType()) || isLowLevelFloatTy(rhs->getType()))
        codegenError(
            "bitwise operator '" + op + "' requires integer operands, got float");
    checkLowLevelTypeMix(lhs, rhs, op);
    // Low-level integer bitwise at native width
    if (isLowLevelIntTy(lhs) && lhs->getType() == rhs->getType()) {
        std::string llName = getLowLevelTypeName(lhs);
        if (llName.empty()) llName = getLowLevelTypeName(rhs);
        if (llName.empty()) llName = llNameHint;
        auto propagate = [&](llvm::Value *result) -> llvm::Value* {
            if (!llName.empty()) low_level_type_names_[result] = llName;
            return result;
        };
        if (op == "&")  return propagate(builder_.CreateAnd(lhs, rhs,  "band_ll"));
        if (op == "|")  return propagate(builder_.CreateOr(lhs,  rhs,  "bor_ll"));
        if (op == "^")  return propagate(builder_.CreateXor(lhs, rhs,  "bxor_ll"));
        if (op == "<<") return propagate(builder_.CreateShl(lhs,  rhs, "shl_ll"));
        if (op == ">>") {
            if (isUnsignedLowLevel(lhs) || isUnsignedLowLevelName(llNameHint))
                return propagate(builder_.CreateLShr(lhs, rhs, "lshr_ll"));
            return propagate(builder_.CreateAShr(lhs, rhs, "ashr_ll"));
        }
        if (op == ">>>") return propagate(builder_.CreateLShr(lhs, rhs, "lshr_ll"));
        codegenError("unknown bitwise operator: " + op);
    }
    lhs = promoteToInt(lhs);
    rhs = promoteToInt(rhs);
    if (op == "&")  return builder_.CreateAnd(lhs, rhs,  "band");
    if (op == "|")  return builder_.CreateOr(lhs,  rhs,  "bor");
    if (op == "^")  return builder_.CreateXor(lhs, rhs,  "bxor");
    if (op == "<<") return builder_.CreateShl(lhs,  rhs, "shl");
    if (op == ">>") return builder_.CreateAShr(lhs, rhs, "ashr");
    if (op == ">>>") return builder_.CreateLShr(lhs, rhs, "lshr");
    codegenError("unknown bitwise operator: " + op);
}

llvm::Value *CodeGen::emitArithmeticOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs,
                                        const std::string &llNameHint) {
    // Low-level type mix check (must come first)
    checkLowLevelTypeMix(lhs, rhs, op);

    // Low-level type native-width arithmetic
    if (isLowLevelTy(lhs) && lhs->getType() == rhs->getType()) {
        llvm::Type *ty = lhs->getType();
        if (op == "**")
            codegenError("operator '**' is not supported for low-level numeric types");
        // Propagate low-level type metadata to result
        std::string llName = getLowLevelTypeName(lhs);
        if (llName.empty()) llName = getLowLevelTypeName(rhs);
        if (llName.empty()) llName = llNameHint;
        auto propagate = [&](llvm::Value *result) -> llvm::Value* {
            if (!llName.empty()) low_level_type_names_[result] = llName;
            return result;
        };
        if (isLowLevelFloatTy(ty)) {
            if (op == "//")
                codegenError("operator '//' is not supported for f32");
            if (op == "%")  return propagate(builder_.CreateFRem(lhs, rhs, "frem32"));
            if (op == "/")  return propagate(builder_.CreateFDiv(lhs, rhs, "fdiv32"));
            if (op == "+")  return propagate(builder_.CreateFAdd(lhs, rhs, "fadd32"));
            if (op == "-")  return propagate(builder_.CreateFSub(lhs, rhs, "fsub32"));
            if (op == "*")  return propagate(builder_.CreateFMul(lhs, rhs, "fmul32"));
            codegenError("unknown operator: " + op);
        }
        // Low-level integer
        bool isUnsigned = isUnsignedLowLevel(lhs) || isUnsignedLowLevelName(llNameHint);
        if (op == "/" || op == "//") {
            if (isUnsigned) return propagate(builder_.CreateUDiv(lhs, rhs, "udiv_ll"));
            return propagate(builder_.CreateSDiv(lhs, rhs, "sdiv_ll"));
        }
        if (op == "%") {
            if (isUnsigned) return propagate(builder_.CreateURem(lhs, rhs, "urem_ll"));
            return propagate(builder_.CreateSRem(lhs, rhs, "srem_ll"));
        }
        if (op == "+")  return propagate(builder_.CreateAdd(lhs, rhs, "add_ll"));
        if (op == "-")  return propagate(builder_.CreateSub(lhs, rhs, "sub_ll"));
        if (op == "*")  return propagate(builder_.CreateMul(lhs, rhs, "mul_ll"));
        codegenError("unknown operator: " + op);
    }

    // ** 累乗: 常にf64、libmのpow()を呼ぶ
    if (op == "**") {
        if (lhs->getType() == i8Ty_)
            lhs = builder_.CreateUIToFP(lhs, f64Ty_, "lhs_f");
        else if (lhs->getType()->isIntegerTy())
            lhs = builder_.CreateSIToFP(lhs, f64Ty_, "lhs_f");
        if (rhs->getType() == i8Ty_)
            rhs = builder_.CreateUIToFP(rhs, f64Ty_, "rhs_f");
        else if (rhs->getType()->isIntegerTy())
            rhs = builder_.CreateSIToFP(rhs, f64Ty_, "rhs_f");
        llvm::FunctionType *powTy = llvm::FunctionType::get(f64Ty_, {f64Ty_, f64Ty_}, false);
        llvm::FunctionCallee powFn = mod_->getOrInsertFunction("pow", powTy);
        return builder_.CreateCall(powFn, {lhs, rhs}, "pow");
    }

    // String concatenation
    if (op == "+" && lhs->getType() == ptrTy_ && rhs->getType() == ptrTy_) {
        auto strlenFn = getStdlibStrlen();
        auto mallocFn = getStdlibMalloc();
        auto strcpyFn = getStdlibStrcpy();
        auto strcatFn = getStdlibStrcat();

        llvm::Value *lenL = builder_.CreateCall(strlenFn, {lhs}, "len_l");
        llvm::Value *lenR = builder_.CreateCall(strlenFn, {rhs}, "len_r");
        llvm::Value *total = builder_.CreateAdd(lenL, lenR, "total_len");
        total = builder_.CreateAdd(total, llvm::ConstantInt::get(i64Ty_, 1), "total_plus_null");
        llvm::Value *buf = builder_.CreateCall(mallocFn, {total}, "concat_buf");
        builder_.CreateCall(strcpyFn, {buf, lhs});
        builder_.CreateCall(strcatFn, {buf, rhs});
        return buf;
    }

    // String repetition: "ab" * 3 or 3 * "ab"
    if (op == "*") {
        llvm::Value *strVal = nullptr;
        llvm::Value *intVal = nullptr;
        if (lhs->getType() == ptrTy_ && rhs->getType()->isIntegerTy()) {
            strVal = lhs; intVal = rhs;
        } else if (rhs->getType() == ptrTy_ && lhs->getType()->isIntegerTy()) {
            strVal = rhs; intVal = lhs;
        }
        if (strVal) {
            if (intVal->getType() == i1Ty_)
                intVal = builder_.CreateZExt(intVal, i64Ty_, "n_ext");
            else if (intVal->getType() == i8Ty_)
                intVal = builder_.CreateZExt(intVal, i64Ty_, "n_ext");
            return emitStringRepeat(strVal, intVal);
        }
    }

    // // floor division (toward -∞)
    if (op == "//") {
        lhs = promoteToInt(lhs);
        rhs = promoteToInt(rhs);
        bool lf = lhs->getType()->isDoubleTy();
        bool rf = rhs->getType()->isDoubleTy();
        if (lf || rf) {
            // float: floor(fdiv)
            std::tie(lhs, rhs) = promoteToFloat(lhs, rhs);
            llvm::Value *div = builder_.CreateFDiv(lhs, rhs, "fdiv");
            llvm::Function *floorFn = llvm::Intrinsic::getDeclaration(
                mod_.get(), llvm::Intrinsic::floor, {f64Ty_});
            return builder_.CreateCall(floorFn, {div}, "floordiv");
        }
        // int: zero-division guard
        {
            llvm::Value *isZero = builder_.CreateICmpEQ(
                rhs, llvm::ConstantInt::get(i64Ty_, 0), "div_zero");
            llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, "floordiv.zero_err", fn_);
            llvm::BasicBlock *okBB  = llvm::BasicBlock::Create(*ctx_, "floordiv.ok", fn_);
            builder_.CreateCondBr(isZero, errBB, okBB);
            builder_.SetInsertPoint(errBB);
            emitRuntimeError("runtime error: division by zero\n",
                              ".floordiv_zero_err_" + std::to_string(arith_zero_err_counter_++));
            builder_.SetInsertPoint(okBB);
        }
        // int: sdiv + floor adjustment
        llvm::Value *q   = builder_.CreateSDiv(lhs, rhs, "q");
        llvm::Value *rem  = builder_.CreateSRem(lhs, rhs, "rem");
        llvm::Value *xorV = builder_.CreateXor(lhs, rhs, "xor");
        llvm::Value *signsDiffer = builder_.CreateICmpSLT(
            xorV, llvm::ConstantInt::get(i64Ty_, 0), "signs_differ");
        llvm::Value *hasRem = builder_.CreateICmpNE(
            rem, llvm::ConstantInt::get(i64Ty_, 0), "has_rem");
        llvm::Value *needsAdj = builder_.CreateAnd(signsDiffer, hasRem, "needs_adj");
        llvm::Value *adjusted = builder_.CreateSub(
            q, llvm::ConstantInt::get(i64Ty_, 1), "adjusted");
        return builder_.CreateSelect(needsAdj, adjusted, q, "floordiv");
    }

    // / 除算: 常にf64
    if (op == "/") {
        std::tie(lhs, rhs) = promoteToFloat(lhs, rhs);
        return builder_.CreateFDiv(lhs, rhs, "div");
    }

    // % 剰余: 片方f64ならfrem、両方i64ならsrem
    if (op == "%") {
        lhs = promoteToInt(lhs);
        rhs = promoteToInt(rhs);
        bool lf = lhs->getType()->isDoubleTy();
        bool rf = rhs->getType()->isDoubleTy();
        if (lf || rf) {
            std::tie(lhs, rhs) = promoteToFloat(lhs, rhs);
            return builder_.CreateFRem(lhs, rhs, "frem");
        }
        // int: zero-division guard
        {
            llvm::Value *isZero = builder_.CreateICmpEQ(
                rhs, llvm::ConstantInt::get(i64Ty_, 0), "mod_zero");
            llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, "mod.zero_err", fn_);
            llvm::BasicBlock *okBB  = llvm::BasicBlock::Create(*ctx_, "mod.ok", fn_);
            builder_.CreateCondBr(isZero, errBB, okBB);
            builder_.SetInsertPoint(errBB);
            emitRuntimeError("runtime error: modulo by zero\n",
                              ".mod_zero_err_" + std::to_string(arith_zero_err_counter_++));
            builder_.SetInsertPoint(okBB);
        }
        return builder_.CreateSRem(lhs, rhs, "srem");
    }

    // +/-/*: 片方f64なら浮動小数点命令
    lhs = promoteToInt(lhs);
    rhs = promoteToInt(rhs);
    bool lf = lhs->getType()->isDoubleTy();
    bool rf = rhs->getType()->isDoubleTy();
    if (lf || rf) {
        std::tie(lhs, rhs) = promoteToFloat(lhs, rhs);
        if (op == "+") return builder_.CreateFAdd(lhs, rhs, "fadd");
        if (op == "-") return builder_.CreateFSub(lhs, rhs, "fsub");
        if (op == "*") return builder_.CreateFMul(lhs, rhs, "fmul");
        codegenError("unknown operator: " + op);
    }
    if (op == "+") return builder_.CreateAdd(lhs, rhs, "add");
    if (op == "-") return builder_.CreateSub(lhs, rhs, "sub");
    if (op == "*") return builder_.CreateMul(lhs, rhs, "mul");
    codegenError("unknown operator: " + op);
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<BinaryExpr> &e) {
    // Handle 'in' / 'not in' operator: lhs in rhs (set, list, or map)
    if (e->op == "in" || e->op == "not in") {
        llvm::Value *elem = emitExpr(*e->lhs);
        llvm::Value *container = emitExpr(*e->rhs);

        // Try set
        llvm::Type *setElemTy = getSetElementType(container);
        if (setElemTy) {
            if (elem->getType() != setElemTy)
                codegenError("'" + e->op + "' operator: element type mismatch");
            llvm::Value *idx = emitSetElementLookup(container, elem, setElemTy);
            llvm::Value *result = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "set_in");
            if (e->op == "not in")
                result = builder_.CreateNot(result, "set_not_in");
            return result;
        }

        // Try map (key lookup)
        llvm::Type *mapKeyTy = getMapKeyType(container);
        if (mapKeyTy) {
            if (elem->getType() != mapKeyTy)
                codegenError("'" + e->op + "' operator: key type mismatch");
            llvm::Value *idx = emitMapKeyLookup(container, elem, mapKeyTy);
            llvm::Value *result = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "map_in");
            if (e->op == "not in")
                result = builder_.CreateNot(result, "map_not_in");
            return result;
        }

        // Try list (linear search)
        llvm::Type *listElemTy = getListElementType(container);
        if (listElemTy) {
            if (elem->getType() != listElemTy)
                codegenError("'" + e->op + "' operator: element type mismatch");

            // Linear search loop
            llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, container, 0, "in_len_ptr");
            llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "in_length");
            llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, container, 2, "in_data_ptr");
            llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataPtrField, "in_data");

            llvm::AllocaInst *foundVar = builder_.CreateAlloca(i1Ty_, nullptr, "in_found");
            builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 0), foundVar);
            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "in_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

            llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "in.cond", fn_);
            llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "in.body", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "in.end", fn_);

            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(condBB);
            llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "in_iv");
            llvm::Value *cond = builder_.CreateICmpSLT(iVal, length, "in_cond");
            builder_.CreateCondBr(cond, bodyBB, endBB);

            builder_.SetInsertPoint(bodyBB);
            llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "in_ic");
            llvm::Value *elemPtr = builder_.CreateGEP(listElemTy, dataPtr, {iCur}, "in_elem_ptr");
            llvm::Value *listElem = builder_.CreateLoad(listElemTy, elemPtr, "in_elem");

            llvm::Value *match;
            if (listElemTy == ptrTy_) {
                // String comparison
                auto strcmpFn = getStdlibStrcmp();
                llvm::Value *cmpResult = builder_.CreateCall(strcmpFn, {elem, listElem}, "in_strcmp");
                match = builder_.CreateICmpEQ(cmpResult, llvm::ConstantInt::get(i32Ty_, 0), "in_match");
            } else if (listElemTy->isDoubleTy()) {
                match = builder_.CreateFCmpOEQ(elem, listElem, "in_match");
            } else {
                match = builder_.CreateICmpEQ(elem, listElem, "in_match");
            }

            llvm::BasicBlock *foundBB = llvm::BasicBlock::Create(*ctx_, "in.found", fn_);
            llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "in.next", fn_);
            builder_.CreateCondBr(match, foundBB, nextBB);

            builder_.SetInsertPoint(foundBB);
            builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 1), foundVar);
            builder_.CreateBr(endBB);

            builder_.SetInsertPoint(nextBB);
            llvm::Value *iNext = builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1), "in_next");
            builder_.CreateStore(iNext, iVar);
            builder_.CreateBr(condBB);

            builder_.SetInsertPoint(endBB);
            llvm::Value *result = builder_.CreateLoad(i1Ty_, foundVar, "in_result");
            if (e->op == "not in")
                result = builder_.CreateNot(result, "list_not_in");
            return result;
        }

        codegenError("'" + e->op + "' operator requires a set, list, or map on the right side");
    }

    // Short-circuit evaluation for 'and' / 'or'
    if (e->op == "and" || e->op == "or") {
        llvm::Value *lhs = emitExpr(*e->lhs);
        llvm::Value *lhsBool = toBool(lhs);

        llvm::BasicBlock *rhsBB = llvm::BasicBlock::Create(*ctx_, "sc.rhs", fn_);
        llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "sc.merge", fn_);
        llvm::BasicBlock *lhsBB = builder_.GetInsertBlock();

        if (e->op == "and")
            builder_.CreateCondBr(lhsBool, rhsBB, mergeBB);
        else
            builder_.CreateCondBr(lhsBool, mergeBB, rhsBB);

        builder_.SetInsertPoint(rhsBB);
        llvm::Value *rhs = emitExpr(*e->rhs);
        llvm::Value *rhsBool = toBool(rhs);
        llvm::BasicBlock *rhsEndBB = builder_.GetInsertBlock();
        builder_.CreateBr(mergeBB);

        builder_.SetInsertPoint(mergeBB);
        llvm::PHINode *phi = builder_.CreatePHI(i1Ty_, 2, e->op);
        phi->addIncoming(lhsBool, lhsBB);
        phi->addIncoming(rhsBool, rhsEndBB);
        return phi;
    }

    // Null coalescing operator: lhs ?? rhs
    if (e->op == "??") {
        llvm::Value *lhs = emitExpr(*e->lhs);
        if (!isOptionType(lhs->getType()))
            codegenError("'" "??" "' operator requires Option type on the left side");
        llvm::Value *hasVal = builder_.CreateExtractValue(lhs, {0}, "has_val");
        llvm::Value *innerVal = builder_.CreateExtractValue(lhs, {1}, "inner_val");
        llvm::Value *rhs = emitExpr(*e->rhs);
        if (rhs->getType() != innerVal->getType())
            codegenError("'" "??" "' operator: right-hand side type must match Option's inner type");
        return builder_.CreateSelect(hasVal, innerVal, rhs, "coalesce");
    }

    llvm::Value *lhs = emitExpr(*e->lhs);
    llvm::Value *rhs = emitExpr(*e->rhs);
    const std::string &op = e->op;

    // Try user-defined binary operator first
    std::string opFnName = "operator" + op;
    if (auto *result = tryOperatorCall(opFnName, lhs, rhs))
        return result;

    // any-type dynamic dispatch (#223)
    if (isAnyType(lhs->getType()) || isAnyType(rhs->getType())) {
        if (!isAnyType(lhs->getType())) lhs = wrapInAny(lhs);
        if (!isAnyType(rhs->getType())) rhs = wrapInAny(rhs);
        return emitAnyBinaryOp(op, lhs, rhs);
    }

    // Compute low-level type name hint from AST suffixes for constant operands (#311).
    std::string llHint = getExprLowLevelSuffix(*e->lhs);
    if (llHint.empty()) llHint = getExprLowLevelSuffix(*e->rhs);

    if (op == "==" || op == "!=" || op == "<" ||
        op == "<=" || op == ">"  || op == ">=")
        return emitComparisonOp(op, lhs, rhs, llHint);

    if (op == "&" || op == "|" || op == "^" ||
        op == "<<" || op == ">>" || op == ">>>")
        return emitBitwiseOp(op, lhs, rhs, llHint);

    return emitArithmeticOp(op, lhs, rhs, llHint);
}

void CodeGen::emitStmt(RecordStmt &s) {
    if (struct_types_.count(s.name))
        codegenError("redefined type: " + s.name);

    std::vector<llvm::Type*> fieldTypes;
    for (auto &f : s.fields)
        fieldTypes.push_back(resolveType(f.type));

    llvm::StructType *structTy = llvm::StructType::create(*ctx_, fieldTypes, s.name);
    if (hasDirective(s.directives, "deprecated"))
        deprecated_types_.insert(s.name);
    for (auto &f : s.fields) {
        if (hasDirective(f.directives, "deprecated"))
            deprecated_fields_.insert(s.name + "." + f.name);
    }

    struct_types_[s.name] = {structTy, std::move(s.fields), std::move(s.invariants)};
}

llvm::Value *CodeGen::emitStructConstructor(const StructInfo &info,
                                             const std::string &name,
                                             const std::vector<ExprPtr> &args) {
    if (args.size() != info.fields.size())
        codegenError("type '" + name + "': expected " +
                                 std::to_string(info.fields.size()) + " arguments, got " +
                                 std::to_string(args.size()));

    llvm::Value *result = llvm::UndefValue::get(info.llvmType);

    for (unsigned i = 0; i < info.fields.size(); ++i) {
        llvm::Value *val = emitExpr(*args[i]);
        llvm::Type *expectedTy = info.llvmType->getElementType(i);
        if (val->getType() != expectedTy)
            codegenError("type '" + name + "': field '" + info.fields[i].name +
                                     "' type mismatch");
        result = builder_.CreateInsertValue(result, val, i);
    }

    // Check invariants after construction
    if (!info.invariants.empty())
        emitInvariantCheck(name, info, result);

    return result;
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<FieldAccessExpr> &e) {
    llvm::Value *obj = emitExpr(*e->object);
    llvm::Type *objTy = obj->getType();

    llvm::StructType *structTy = llvm::dyn_cast<llvm::StructType>(objTy);
    if (!structTy)
        codegenError("field access on non-struct type");

    // Error type field access: .message (idx 0), .code (idx 1)
    if (structTy == errorTy_) {
        if (e->field == "message")
            return builder_.CreateExtractValue(obj, 0, "err.message");
        if (e->field == "code")
            return builder_.CreateExtractValue(obj, 1, "err.code");
        codegenError("Error type has no field '" + e->field + "'");
    }

    // Numeric index access for tuples (.0, .1, ...)
    if (!e->field.empty() && std::isdigit(static_cast<unsigned char>(e->field[0]))) {
        unsigned idx = std::stoul(e->field);
        if (idx >= structTy->getNumElements())
            codegenError("tuple index " + e->field + " out of range");
        return builder_.CreateExtractValue(obj, idx, "tuple." + e->field);
    }

    std::string typeName = structTy->getName().str();
    auto it = struct_types_.find(typeName);
    if (it == struct_types_.end())
        codegenError("unknown struct type: " + typeName);

    const auto &info = it->second;
    for (unsigned i = 0; i < info.fields.size(); ++i) {
        if (info.fields[i].name == e->field) {
            std::string qualifiedField = typeName + "." + e->field;
            if (deprecated_fields_.count(qualifiedField))
                emitDeprecationWarning(qualifiedField);
            return builder_.CreateExtractValue(obj, i, e->field);
        }
    }

    codegenError("type '" + typeName + "' has no field '" + e->field + "'");
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<TupleExpr> &e) {
    std::vector<llvm::Type*> types;
    std::vector<llvm::Value*> vals;
    for (auto &el : e->elements) {
        llvm::Value *v = emitExpr(*el);
        types.push_back(v->getType());
        vals.push_back(v);
    }
    llvm::StructType *tupleType = llvm::StructType::get(*ctx_, types);
    llvm::Value *result = llvm::UndefValue::get(tupleType);
    for (unsigned i = 0; i < vals.size(); ++i)
        result = builder_.CreateInsertValue(result, vals[i], i);
    return result;
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<ListExpr> &e) {
    if (e->elements.empty())
        codegenError("empty list literal requires type annotation (not yet supported)");

    // Evaluate all elements
    std::vector<llvm::Value*> vals;
    for (auto &el : e->elements)
        vals.push_back(emitExpr(*el));

    // Check all elements have the same type
    llvm::Type *elemTy = vals[0]->getType();
    for (size_t i = 1; i < vals.size(); ++i) {
        if (vals[i]->getType() != elemTy)
            codegenError("list elements must all have the same type");
    }

    int64_t count = static_cast<int64_t>(vals.size());

    // Allocate list header: { i64 length, i64 capacity, ptr data }
    auto mallocFn = getStdlibMalloc();

    // Allocate header
    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
    llvm::Value *headerPtr = builder_.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "list_header");

    // Allocate data
    uint64_t elemSize = dl.getTypeAllocSize(elemTy);
    llvm::Value *dataSize = llvm::ConstantInt::get(i64Ty_, elemSize * count);
    llvm::Value *dataPtr = builder_.CreateCall(mallocFn, {dataSize}, "list_data");

    // Store elements into data
    for (int64_t i = 0; i < count; ++i) {
        llvm::Value *elemPtr = builder_.CreateGEP(
            elemTy, dataPtr, {llvm::ConstantInt::get(i64Ty_, i)}, "elem_ptr");
        builder_.CreateStore(vals[i], elemPtr);
    }

    // Store length, capacity, data pointer into header
    llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 0, "len_ptr");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, count), lenPtr);

    llvm::Value *capPtr = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 1, "cap_ptr");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, count), capPtr);

    llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 2, "data_ptr");
    builder_.CreateStore(dataPtr, dataPtrField);

    // Track element type
    list_element_types_[headerPtr] = elemTy;

    // Track nested list element type (for flatten support)
    // Only set if ALL elements are lists with the same inner element type
    if (elemTy == ptrTy_) {
        llvm::Type *innerElemTy = getListElementType(vals[0]);
        if (innerElemTy) {
            bool allMatch = true;
            for (size_t i = 1; i < vals.size(); ++i) {
                llvm::Type *otherInner = getListElementType(vals[i]);
                if (!otherInner || otherInner != innerElemTy) {
                    allMatch = false;
                    break;
                }
            }
            if (allMatch)
                nested_list_element_types_[headerPtr] = innerElemTy;
        }
    }

    return headerPtr;
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<MapExpr> &e) {
    if (e->keys.empty())
        codegenError("empty map literal requires type annotation");

    // Evaluate all keys and values
    std::vector<llvm::Value*> keyVals, valVals;
    for (auto &k : e->keys) keyVals.push_back(emitExpr(*k));
    for (auto &v : e->values) valVals.push_back(emitExpr(*v));

    // Check all keys have the same type
    llvm::Type *keyTy = keyVals[0]->getType();
    for (size_t i = 1; i < keyVals.size(); ++i) {
        if (keyVals[i]->getType() != keyTy)
            codegenError("map keys must all have the same type");
    }

    // Check all values have the same type
    llvm::Type *valTy = valVals[0]->getType();
    for (size_t i = 1; i < valVals.size(); ++i) {
        if (valVals[i]->getType() != valTy)
            codegenError("map values must all have the same type");
    }

    int64_t count = static_cast<int64_t>(keyVals.size());

    auto mallocFn = getStdlibMalloc();
    const llvm::DataLayout &dl = mod_->getDataLayout();

    // Allocate MapHeader
    uint64_t headerSize = dl.getTypeAllocSize(mapHeaderTy_);
    llvm::Value *headerPtr = builder_.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "map_header");

    // Allocate keys array
    uint64_t keySize = dl.getTypeAllocSize(keyTy);
    llvm::Value *keysPtr = builder_.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty_, keySize * count)}, "map_keys");

    // Allocate values array
    uint64_t valSize = dl.getTypeAllocSize(valTy);
    llvm::Value *valsPtr = builder_.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty_, valSize * count)}, "map_vals");

    // Store keys and values
    for (int64_t i = 0; i < count; ++i) {
        llvm::Value *kp = builder_.CreateGEP(keyTy, keysPtr,
            {llvm::ConstantInt::get(i64Ty_, i)}, "key_ptr");
        builder_.CreateStore(keyVals[i], kp);
        llvm::Value *vp = builder_.CreateGEP(valTy, valsPtr,
            {llvm::ConstantInt::get(i64Ty_, i)}, "val_ptr");
        builder_.CreateStore(valVals[i], vp);
    }

    // Store header fields: length, capacity, keys_ptr, values_ptr
    llvm::Value *lenPtr = builder_.CreateStructGEP(mapHeaderTy_, headerPtr, 0, "map_len_ptr");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, count), lenPtr);

    llvm::Value *capPtr = builder_.CreateStructGEP(mapHeaderTy_, headerPtr, 1, "map_cap_ptr");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, count), capPtr);

    llvm::Value *keysPtrField = builder_.CreateStructGEP(mapHeaderTy_, headerPtr, 2, "map_keys_field");
    builder_.CreateStore(keysPtr, keysPtrField);

    llvm::Value *valsPtrField = builder_.CreateStructGEP(mapHeaderTy_, headerPtr, 3, "map_vals_field");
    builder_.CreateStore(valsPtr, valsPtrField);

    // Initialize hash table buckets via rehash
    int64_t initBucketCount = 8;
    while (initBucketCount * 3 < count * 4) initBucketCount *= 2;
    {
        std::string rehashName;
        llvm::Type *rehashKeyTy;
        if (keyTy == ptrTy_) {
            rehashName = "__ry_ht_rehash_str";
            rehashKeyTy = ptrTy_;
        } else if (keyTy->isDoubleTy()) {
            rehashName = "__ry_ht_rehash_f64";
            rehashKeyTy = f64Ty_;
        } else {
            rehashName = "__ry_ht_rehash_i64";
            rehashKeyTy = i64Ty_;
        }
        llvm::FunctionType *rehashTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, i64Ty_, i64Ty_}, false);
        llvm::FunctionCallee rehashFn = mod_->getOrInsertFunction(rehashName, rehashTy);
        llvm::Value *buckets = builder_.CreateCall(rehashFn,
            {keysPtr, llvm::ConstantInt::get(i64Ty_, count),
             llvm::ConstantInt::get(i64Ty_, initBucketCount)}, "map_buckets");
        llvm::Value *bcPtr = builder_.CreateStructGEP(mapHeaderTy_, headerPtr, 4, "map_bc_ptr");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, initBucketCount), bcPtr);
        llvm::Value *bpPtr = builder_.CreateStructGEP(mapHeaderTy_, headerPtr, 5, "map_bp_ptr");
        builder_.CreateStore(buckets, bpPtr);
    }

    // Track types
    map_key_types_[headerPtr] = keyTy;
    map_value_types_[headerPtr] = valTy;

    return headerPtr;
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<SetExpr> &e) {
    if (e->elements.empty()) {
        // Empty set — requires type annotation (handled in emitVarDecl)
        // If reached here directly, error
        codegenError("empty set literal requires type annotation");
    }

    // Evaluate all elements
    std::vector<llvm::Value*> vals;
    for (auto &el : e->elements)
        vals.push_back(emitExpr(*el));

    // Check all elements have the same type
    llvm::Type *elemTy = vals[0]->getType();
    for (size_t i = 1; i < vals.size(); ++i) {
        if (vals[i]->getType() != elemTy)
            codegenError("set elements must all have the same type");
    }

    int64_t count = static_cast<int64_t>(vals.size());

    auto mallocFn = getStdlibMalloc();
    const llvm::DataLayout &dl = mod_->getDataLayout();

    // Allocate SetHeader
    uint64_t headerSize = dl.getTypeAllocSize(setHeaderTy_);
    llvm::Value *headerPtr = builder_.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "set_header");

    // Allocate elements array
    uint64_t elemSize = dl.getTypeAllocSize(elemTy);
    llvm::Value *elemsPtr = builder_.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty_, elemSize * count)}, "set_elems");

    // Store elements
    for (int64_t i = 0; i < count; ++i) {
        llvm::Value *ep = builder_.CreateGEP(elemTy, elemsPtr,
            {llvm::ConstantInt::get(i64Ty_, i)}, "set_elem_ptr");
        builder_.CreateStore(vals[i], ep);
    }

    // Store header fields: length, capacity, elements_ptr
    llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, headerPtr, 0, "set_len_ptr");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, count), lenPtr);

    llvm::Value *capPtr = builder_.CreateStructGEP(setHeaderTy_, headerPtr, 1, "set_cap_ptr");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, count), capPtr);

    llvm::Value *elemsPtrField = builder_.CreateStructGEP(setHeaderTy_, headerPtr, 2, "set_elems_field");
    builder_.CreateStore(elemsPtr, elemsPtrField);

    // Initialize hash table buckets via rehash
    int64_t initBucketCount = 8;
    while (initBucketCount * 3 < count * 4) initBucketCount *= 2;
    {
        std::string rehashName;
        if (elemTy == ptrTy_) {
            rehashName = "__ry_ht_rehash_str";
        } else if (elemTy->isDoubleTy()) {
            rehashName = "__ry_ht_rehash_f64";
        } else {
            rehashName = "__ry_ht_rehash_i64";
        }
        llvm::FunctionType *rehashTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, i64Ty_, i64Ty_}, false);
        llvm::FunctionCallee rehashFn = mod_->getOrInsertFunction(rehashName, rehashTy);
        llvm::Value *buckets = builder_.CreateCall(rehashFn,
            {elemsPtr, llvm::ConstantInt::get(i64Ty_, count),
             llvm::ConstantInt::get(i64Ty_, initBucketCount)}, "set_buckets");
        llvm::Value *bcPtr = builder_.CreateStructGEP(setHeaderTy_, headerPtr, 3, "set_bc_ptr");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, initBucketCount), bcPtr);
        llvm::Value *bpPtr = builder_.CreateStructGEP(setHeaderTy_, headerPtr, 4, "set_bp_ptr");
        builder_.CreateStore(buckets, bpPtr);
    }

    // Track element type
    set_element_types_[headerPtr] = elemTy;

    return headerPtr;
}

llvm::Value *CodeGen::emitExprVariant(const EnumAccessExpr &e) {
    // Try to instantiate generic enum if not found
    if (!enum_types_.count(e.enum_name)) {
        auto ltPos = e.enum_name.find('<');
        if (ltPos != std::string::npos && e.enum_name.back() == '>') {
            std::string baseName = e.enum_name.substr(0, ltPos);
            std::string argsStr = e.enum_name.substr(ltPos + 1, e.enum_name.size() - ltPos - 2);
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
            instantiateGenericEnum(e.enum_name, baseName, typeArgs);
        }
    }
    auto it = enum_types_.find(e.enum_name);
    if (it == enum_types_.end())
        codegenError("undefined enum: " + e.enum_name);
    auto vit = it->second.variants.find(e.variant_name);
    if (vit == it->second.variants.end())
        codegenError("enum '" + e.enum_name + "' has no variant '" + e.variant_name + "'");

    if (it->second.isADT) {
        // Reject access to payload-carrying variants without arguments
        auto fit = it->second.variantFields.find(e.variant_name);
        if (fit != it->second.variantFields.end() && !fit->second.fieldTypes.empty())
            codegenError("variant '" + e.enum_name + "::" + e.variant_name +
                "' requires " + std::to_string(fit->second.fieldTypes.size()) +
                " argument(s); use '" + e.enum_name + "::" + e.variant_name + "(...)' instead");
        // ADT enum: create struct { tag, zero-payload } for data-less variants
        llvm::Value *adtVal = llvm::UndefValue::get(it->second.adtType);
        adtVal = builder_.CreateInsertValue(adtVal, llvm::ConstantInt::get(i64Ty_, vit->second), 0, "adt.tag");
        enum_value_types_[adtVal] = e.enum_name;
        return adtVal;
    }

    llvm::Value *val = llvm::ConstantInt::get(i64Ty_, vit->second);
    enum_value_types_[val] = e.enum_name;
    return val;
}


llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<IndexExpr> &e) {
    llvm::Value *objPtr = emitExpr(*e->object);
    llvm::Value *index = emitExpr(*e->index);

    if (objPtr->getType() != ptrTy_)
        codegenError("index operator requires list or map");

    // Check if this is a map
    llvm::Type *mapKeyTy = getMapKeyType(objPtr);
    if (mapKeyTy) {
        llvm::Type *mapValTy = getMapValueType(objPtr);
        if (!mapValTy)
            codegenError("cannot determine map value type");

        // Check key type matches
        if (index->getType() != mapKeyTy)
            codegenError("map key type mismatch");

        // Lookup key
        llvm::Value *idx = emitMapKeyLookup(objPtr, index, mapKeyTy);

        // Check if found
        llvm::Value *notFound = builder_.CreateICmpSLT(idx, llvm::ConstantInt::get(i64Ty_, 0), "not_found");

        llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, "map.notfound", fn_);
        llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "map.found", fn_);

        builder_.CreateCondBr(notFound, failBB, okBB);

        // Not found: print error and exit
        builder_.SetInsertPoint(failBB);
        emitRuntimeError("runtime error: map key not found\n", ".map_key_err");

        // Found: get value
        builder_.SetInsertPoint(okBB);
        llvm::Value *valsPtrField = builder_.CreateStructGEP(mapHeaderTy_, objPtr, 3, "map_vals_ptr");
        llvm::Value *valsPtr = builder_.CreateLoad(ptrTy_, valsPtrField, "map_vals");
        llvm::Value *valElemPtr = builder_.CreateGEP(mapValTy, valsPtr, {idx}, "val_elem_ptr");
        return builder_.CreateLoad(mapValTy, valElemPtr, "map_val");
    }

    // List index access
    llvm::Type *elemTy = getListElementType(objPtr);
    if (!elemTy)
        codegenError("cannot determine list element type for index access");

    if (index->getType() == i1Ty_)
        index = builder_.CreateZExt(index, i64Ty_, "idx_ext");

    llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, objPtr, 0, "len_ptr");
    llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "length");

    llvm::Value *negCheck = builder_.CreateICmpSLT(index, llvm::ConstantInt::get(i64Ty_, 0), "neg_check");
    llvm::Value *overCheck = builder_.CreateICmpSGE(index, length, "over_check");
    llvm::Value *outOfBounds = builder_.CreateOr(negCheck, overCheck, "oob");

    llvm::BasicBlock *oobBB = llvm::BasicBlock::Create(*ctx_, "index.oob", fn_);
    llvm::BasicBlock *okBB2 = llvm::BasicBlock::Create(*ctx_, "index.ok", fn_);

    builder_.CreateCondBr(outOfBounds, oobBB, okBB2);

    builder_.SetInsertPoint(oobBB);
    emitRuntimeError("runtime error: list index out of range\n", ".idx_err");

    builder_.SetInsertPoint(okBB2);
    llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, objPtr, 2, "data_ptr");
    llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataPtrField, "data");
    llvm::Value *elemPtr = builder_.CreateGEP(elemTy, dataPtr, {index}, "elem_ptr");
    return builder_.CreateLoad(elemTy, elemPtr, "elem");
}

llvm::Value *CodeGen::valueToString(llvm::Value *val) {
    llvm::Type *ty = val->getType();

    if (ty == anyTy_)
        return emitAnyToString(val);

    // Enum value → variant name string
    {
        auto evIt = enum_value_types_.find(val);
        if (evIt == enum_value_types_.end()) {
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val))
                evIt = enum_value_types_.find(load->getPointerOperand());
        }
        if (evIt != enum_value_types_.end()) {
            auto &einfo = enum_types_[evIt->second];
            if (!einfo.isADT) {
                if (einfo.hasExplicitValues) {
                    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "vts.enum.merge", fn_);
                    llvm::BasicBlock *defaultBB = llvm::BasicBlock::Create(*ctx_, "vts.enum.default", fn_);
                    auto *sw = builder_.CreateSwitch(val, defaultBB, einfo.variantCount);
                    builder_.SetInsertPoint(mergeBB);
                    auto *namePhi = builder_.CreatePHI(ptrTy_, einfo.variantCount + 1, "vts.enum.name");
                    for (size_t i = 0; i < einfo.variantOrder.size(); ++i) {
                        const auto &vname = einfo.variantOrder[i];
                        int64_t vval = einfo.variants.at(vname);
                        llvm::BasicBlock *caseBB = llvm::BasicBlock::Create(*ctx_, "vts.enum." + vname, fn_);
                        sw->addCase(llvm::cast<llvm::ConstantInt>(llvm::ConstantInt::get(i64Ty_, vval, true)), caseBB);
                        builder_.SetInsertPoint(caseBB);
                        llvm::Value *namePtr = builder_.CreateGEP(
                            llvm::ArrayType::get(ptrTy_, einfo.variantCount),
                            einfo.nameArray,
                            {llvm::ConstantInt::get(i64Ty_, 0), llvm::ConstantInt::get(i64Ty_, i)},
                            "enum_name_ptr");
                        llvm::Value *nameStr = builder_.CreateLoad(ptrTy_, namePtr, "enum_name");
                        namePhi->addIncoming(nameStr, caseBB);
                        builder_.CreateBr(mergeBB);
                    }
                    builder_.SetInsertPoint(defaultBB);
                    llvm::Value *unknownStr = builder_.CreateGlobalString("?", ".enum_unknown");
                    namePhi->addIncoming(unknownStr, defaultBB);
                    builder_.CreateBr(mergeBB);
                    builder_.SetInsertPoint(mergeBB);
                    return namePhi;
                }
                llvm::Value *namePtr = builder_.CreateGEP(
                    llvm::ArrayType::get(ptrTy_, einfo.variantCount),
                    einfo.nameArray,
                    {llvm::ConstantInt::get(i64Ty_, 0), val},
                    "enum_name_ptr");
                return builder_.CreateLoad(ptrTy_, namePtr, "enum_name");
            }
        }
    }

    if (auto *structTy = llvm::dyn_cast<llvm::StructType>(ty)) {
        std::string name = structTy->getName().str();
        if (struct_types_.count(name))
            return structToString(val);
    }

    if (ty->isPointerTy()) {
        // Reject non-string pointer types (collections, function pointers)
        if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
            llvm::Value *src = load->getPointerOperand();
            if (list_element_types_.count(src) || map_key_types_.count(src) ||
                set_element_types_.count(src))
                codegenError("cannot convert collection to string");
        }
        if (fn_type_info_.count(val))
            codegenError("cannot convert function to string");
        return val; // string pointer
    }
    auto mallocFn = getStdlibMalloc();
    auto snprintfFn = getStdlibSnprintf();

    if (ty == i1Ty_) {
        llvm::Constant *trueStr = builder_.CreateGlobalString("true", ".vts_true");
        llvm::Constant *falseStr = builder_.CreateGlobalString("false", ".vts_false");
        return builder_.CreateSelect(val, trueStr, falseStr, "vts_bool");
    }
    if (ty->isDoubleTy()) {
        llvm::Value *buf = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, 64)}, "vts_buf");
        llvm::Constant *fmt = builder_.CreateGlobalString("%g", ".vts_float_fmt");
        builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 64), fmt, val});
        return buf;
    }
    // Check low-level type metadata for ambiguous LLVM types
    std::string llName = getLowLevelTypeName(val);

    if (ty == i8Ty_) {
        llvm::Value *buf = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, 32)}, "vts_buf");
        if (llName == "i8") {
            llvm::Constant *fmt = builder_.CreateGlobalString("%d", ".vts_i8_fmt");
            llvm::Value *ext = builder_.CreateSExt(val, i32Ty_, "i8_ext");
            builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 32), fmt, ext});
        } else if (llName == "u8") {
            llvm::Constant *fmt = builder_.CreateGlobalString("%u", ".vts_u8_fmt");
            llvm::Value *ext = builder_.CreateZExt(val, i32Ty_, "u8_ext");
            builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 32), fmt, ext});
        } else {
            // byte (default)
            llvm::Constant *fmt = builder_.CreateGlobalString("%d", ".vts_byte_fmt");
            llvm::Value *ext = builder_.CreateZExt(val, i32Ty_, "byte_ext");
            builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 32), fmt, ext});
        }
        return buf;
    }
    if (ty == i16Ty_) {
        llvm::Value *buf = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, 32)}, "vts_buf");
        if (llName == "u16") {
            llvm::Constant *fmt = builder_.CreateGlobalString("%u", ".vts_u16_fmt");
            llvm::Value *ext = builder_.CreateZExt(val, i32Ty_, "u16_ext");
            builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 32), fmt, ext});
        } else {
            llvm::Constant *fmt = builder_.CreateGlobalString("%d", ".vts_i16_fmt");
            llvm::Value *ext = builder_.CreateSExt(val, i32Ty_, "i16_ext");
            builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 32), fmt, ext});
        }
        return buf;
    }
    if (ty == i32Ty_) {
        llvm::Value *buf = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, 32)}, "vts_buf");
        if (llName == "u32") {
            llvm::Constant *fmt = builder_.CreateGlobalString("%u", ".vts_u32_fmt");
            builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 32), fmt, val});
        } else {
            llvm::Constant *fmt = builder_.CreateGlobalString("%d", ".vts_i32_fmt");
            builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 32), fmt, val});
        }
        return buf;
    }
    if (ty == f32Ty_) {
        llvm::Value *buf = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, 64)}, "vts_buf");
        llvm::Constant *fmt = builder_.CreateGlobalString("%g", ".vts_f32_fmt");
        llvm::Value *ext = builder_.CreateFPExt(val, f64Ty_, "f32_ext");
        builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 64), fmt, ext});
        return buf;
    }
    // default: int (i64) or i64/u64
    llvm::Value *buf = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, 32)}, "vts_buf");
    if (llName == "u64") {
        llvm::Constant *fmt = builder_.CreateGlobalString("%lu", ".vts_u64_fmt");
        builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 32), fmt, val});
    } else {
        llvm::Constant *fmt = builder_.CreateGlobalString("%ld", ".vts_int_fmt");
        builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 32), fmt, val});
    }
    return buf;
}

llvm::Value *CodeGen::structToString(llvm::Value *val) {
    auto *structTy = llvm::cast<llvm::StructType>(val->getType());
    std::string typeName = structTy->getName().str();
    auto it = struct_types_.find(typeName);
    if (it == struct_types_.end())
        codegenError("structToString: unknown record type: " + typeName);

    const auto &info = it->second;

    // Check for user-defined to_str overload
    auto fit = functions_.find("to_str");
    if (fit != functions_.end()) {
        for (auto &entry : fit->second) {
            if (entry.paramTypes.size() == 1 && entry.paramTypes[0] == structTy) {
                return builder_.CreateCall(entry.func, {val}, "user_to_str");
            }
        }
    }

    // Auto-generate: "TypeName(field1: val1, field2: val2)"
    auto strlenFn = getStdlibStrlen();
    auto mallocFn = getStdlibMalloc();
    auto memcpyFn = getStdlibMemcpy();

    // Build string parts with lengths (use constants for literals, strlen for dynamic values)
    struct StringPart { llvm::Value *str; llvm::Value *len; };
    std::vector<StringPart> parts;

    auto addLiteral = [&](const std::string &s, const char *label) {
        parts.push_back({builder_.CreateGlobalString(s, label),
                         llvm::ConstantInt::get(i64Ty_, s.size())});
    };

    addLiteral(typeName + "(", ".sts_prefix");

    for (unsigned i = 0; i < info.fields.size(); ++i) {
        if (i > 0)
            addLiteral(", ", ".sts_sep");
        addLiteral(info.fields[i].name + ": ", ".sts_fname");
        llvm::Value *field = builder_.CreateExtractValue(val, i, info.fields[i].name);
        llvm::Value *fieldStr = valueToString(field);
        parts.push_back({fieldStr, builder_.CreateCall(strlenFn, {fieldStr}, "sts_len")});
    }

    addLiteral(")", ".sts_suffix");

    // Compute total length
    llvm::Value *totalLen = llvm::ConstantInt::get(i64Ty_, 0);
    for (auto &p : parts)
        totalLen = builder_.CreateAdd(totalLen, p.len, "sts_total");

    // Allocate and concatenate
    llvm::Value *bufSize = builder_.CreateAdd(totalLen, llvm::ConstantInt::get(i64Ty_, 1), "sts_bufsize");
    llvm::Value *buf = builder_.CreateCall(mallocFn, {bufSize}, "sts_buf");
    llvm::Value *offset = llvm::ConstantInt::get(i64Ty_, 0);
    for (auto &p : parts) {
        llvm::Value *dst = builder_.CreateGEP(builder_.getInt8Ty(), buf, {offset}, "sts_dst");
        builder_.CreateCall(memcpyFn, {dst, p.str, p.len});
        offset = builder_.CreateAdd(offset, p.len, "sts_off");
    }

    // Null-terminate
    llvm::Value *endPtr = builder_.CreateGEP(builder_.getInt8Ty(), buf, {offset}, "sts_end");
    builder_.CreateStore(llvm::ConstantInt::get(builder_.getInt8Ty(), 0), endPtr);

    return buf;
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<CastExpr> &e) {
    llvm::Value *val = emitExpr(*e->value);
    llvm::Type *srcTy = val->getType();
    const std::string &target = e->target_type;

    if (target == "float") {
        if (srcTy->isDoubleTy()) return val;
        if (srcTy == f32Ty_) return builder_.CreateFPExt(val, f64Ty_, "cast_f");
        if (srcTy == i1Ty_) val = builder_.CreateZExt(val, i64Ty_, "bool_ext");
        else if (srcTy == i8Ty_) {
            if (isUnsignedLowLevel(val))
                return builder_.CreateUIToFP(val, f64Ty_, "cast_f");
            std::string name = getLowLevelTypeName(val);
            if (name == "i8") {
                val = builder_.CreateSExt(val, i64Ty_, "i8_ext");
            } else {
                val = builder_.CreateZExt(val, i64Ty_, "byte_ext");
            }
        }
        else if (srcTy == i16Ty_) {
            if (isUnsignedLowLevel(val))
                return builder_.CreateUIToFP(val, f64Ty_, "cast_f");
            val = builder_.CreateSExt(val, i64Ty_, "i16_ext");
        }
        else if (srcTy == i32Ty_) {
            if (isUnsignedLowLevel(val))
                return builder_.CreateUIToFP(val, f64Ty_, "cast_f");
            val = builder_.CreateSExt(val, i64Ty_, "i32_ext");
        }
        else if (srcTy == i64Ty_ && isUnsignedLowLevel(val))
            return builder_.CreateUIToFP(val, f64Ty_, "cast_f");
        if (val->getType()->isIntegerTy())
            return builder_.CreateSIToFP(val, f64Ty_, "cast_f");
        codegenError("cannot cast to float");
    }
    if (target == "int") {
        if (srcTy == i64Ty_) return val;
        if (srcTy->isDoubleTy()) return builder_.CreateFPToSI(val, i64Ty_, "cast_i");
        if (srcTy == i1Ty_) return builder_.CreateZExt(val, i64Ty_, "cast_i");
        if (srcTy == i8Ty_) {
            std::string name = getLowLevelTypeName(val);
            if (name == "i8") return builder_.CreateSExt(val, i64Ty_, "cast_i");
            return builder_.CreateZExt(val, i64Ty_, "cast_i");
        }
        if (srcTy == i32Ty_) {
            if (isUnsignedLowLevel(val)) return builder_.CreateZExt(val, i64Ty_, "cast_i");
            return builder_.CreateSExt(val, i64Ty_, "cast_i");
        }
        if (srcTy == i16Ty_) {
            if (isUnsignedLowLevel(val)) return builder_.CreateZExt(val, i64Ty_, "cast_i");
            return builder_.CreateSExt(val, i64Ty_, "cast_i");
        }
        if (srcTy == f32Ty_) return builder_.CreateFPToSI(val, i64Ty_, "cast_i");
        codegenError("cannot cast to int");
    }
    if (target == "bool") {
        if (srcTy == i1Ty_) return val;
        if (srcTy == i64Ty_) return builder_.CreateICmpNE(val, llvm::ConstantInt::get(i64Ty_, 0), "cast_b");
        if (srcTy == i8Ty_) return builder_.CreateICmpNE(val, llvm::ConstantInt::get(i8Ty_, 0), "cast_b");
        if (srcTy->isDoubleTy()) return builder_.CreateFCmpONE(val, llvm::ConstantFP::get(f64Ty_, 0.0), "cast_b");
        codegenError("cannot cast to bool");
    }
    if (target == "str") {
        return valueToString(val);
    }
    if (target == "byte") {
        if (srcTy == i8Ty_) return val;
        if (srcTy == i64Ty_) return builder_.CreateTrunc(val, i8Ty_, "cast_byte");
        if (srcTy == i1Ty_) return builder_.CreateZExt(val, i8Ty_, "cast_byte");
        codegenError("cannot cast to byte");
    }
    // Low-level type casts — helper lambda for metadata
    // Skip metadata for Constant values to avoid ConstantInt sharing corruption (#311).
    auto withMeta = [&](llvm::Value *result, const std::string &name) -> llvm::Value* {
        if (!llvm::isa<llvm::Constant>(result))
            low_level_type_names_[result] = name;
        return result;
    };

    if (target == "i32") {
        llvm::Value *r;
        if (srcTy == i32Ty_) r = val;
        else if (srcTy == i64Ty_) r = builder_.CreateTrunc(val, i32Ty_, "cast_i32");
        else if (srcTy == i16Ty_) r = builder_.CreateSExt(val, i32Ty_, "cast_i32");
        else if (srcTy == i8Ty_) {
            r = isUnsignedLowLevel(val) ? builder_.CreateZExt(val, i32Ty_, "cast_i32")
                                        : builder_.CreateSExt(val, i32Ty_, "cast_i32");
        }
        else if (srcTy == i1Ty_) r = builder_.CreateZExt(val, i32Ty_, "cast_i32");
        else if (srcTy->isDoubleTy()) r = builder_.CreateFPToSI(val, i32Ty_, "cast_i32");
        else if (srcTy == f32Ty_) r = builder_.CreateFPToSI(val, i32Ty_, "cast_i32");
        else codegenError("cannot cast to i32");
        return withMeta(r, "i32");
    }
    if (target == "i16") {
        llvm::Value *r;
        if (srcTy == i16Ty_) r = val;
        else if (srcTy == i64Ty_) r = builder_.CreateTrunc(val, i16Ty_, "cast_i16");
        else if (srcTy == i32Ty_) r = builder_.CreateTrunc(val, i16Ty_, "cast_i16");
        else if (srcTy == i8Ty_) {
            r = isUnsignedLowLevel(val) ? builder_.CreateZExt(val, i16Ty_, "cast_i16")
                                        : builder_.CreateSExt(val, i16Ty_, "cast_i16");
        }
        else if (srcTy == i1Ty_) r = builder_.CreateZExt(val, i16Ty_, "cast_i16");
        else if (srcTy->isDoubleTy()) r = builder_.CreateFPToSI(val, i16Ty_, "cast_i16");
        else if (srcTy == f32Ty_) r = builder_.CreateFPToSI(val, i16Ty_, "cast_i16");
        else codegenError("cannot cast to i16");
        return withMeta(r, "i16");
    }
    if (target == "i8") {
        llvm::Value *r;
        if (srcTy == i8Ty_) r = val;
        else if (srcTy == i64Ty_) r = builder_.CreateTrunc(val, i8Ty_, "cast_i8");
        else if (srcTy == i32Ty_) r = builder_.CreateTrunc(val, i8Ty_, "cast_i8");
        else if (srcTy == i16Ty_) r = builder_.CreateTrunc(val, i8Ty_, "cast_i8");
        else if (srcTy == i1Ty_) r = builder_.CreateZExt(val, i8Ty_, "cast_i8");
        else if (srcTy->isDoubleTy()) r = builder_.CreateFPToSI(val, i8Ty_, "cast_i8");
        else if (srcTy == f32Ty_) r = builder_.CreateFPToSI(val, i8Ty_, "cast_i8");
        else codegenError("cannot cast to i8");
        return withMeta(r, "i8");
    }
    if (target == "i64") {
        llvm::Value *r;
        if (srcTy == i64Ty_) r = val;
        else if (srcTy == i32Ty_) {
            r = isUnsignedLowLevel(val) ? builder_.CreateZExt(val, i64Ty_, "cast_i64")
                                        : builder_.CreateSExt(val, i64Ty_, "cast_i64");
        }
        else if (srcTy == i16Ty_) {
            r = isUnsignedLowLevel(val) ? builder_.CreateZExt(val, i64Ty_, "cast_i64")
                                        : builder_.CreateSExt(val, i64Ty_, "cast_i64");
        }
        else if (srcTy == i8Ty_) {
            r = isUnsignedLowLevel(val) ? builder_.CreateZExt(val, i64Ty_, "cast_i64")
                                        : builder_.CreateSExt(val, i64Ty_, "cast_i64");
        }
        else if (srcTy == i1Ty_) r = builder_.CreateZExt(val, i64Ty_, "cast_i64");
        else if (srcTy->isDoubleTy()) r = builder_.CreateFPToSI(val, i64Ty_, "cast_i64");
        else if (srcTy == f32Ty_) r = builder_.CreateFPToSI(val, i64Ty_, "cast_i64");
        else codegenError("cannot cast to i64");
        return withMeta(r, "i64");
    }
    if (target == "u8") {
        llvm::Value *r;
        if (srcTy == i8Ty_) r = val;
        else if (srcTy == i64Ty_) r = builder_.CreateTrunc(val, i8Ty_, "cast_u8");
        else if (srcTy == i32Ty_) r = builder_.CreateTrunc(val, i8Ty_, "cast_u8");
        else if (srcTy == i16Ty_) r = builder_.CreateTrunc(val, i8Ty_, "cast_u8");
        else if (srcTy == i1Ty_) r = builder_.CreateZExt(val, i8Ty_, "cast_u8");
        else if (srcTy->isDoubleTy()) r = builder_.CreateFPToUI(val, i8Ty_, "cast_u8");
        else if (srcTy == f32Ty_) r = builder_.CreateFPToUI(val, i8Ty_, "cast_u8");
        else codegenError("cannot cast to u8");
        return withMeta(r, "u8");
    }
    if (target == "u16") {
        llvm::Value *r;
        if (srcTy == i16Ty_) r = val;
        else if (srcTy == i64Ty_) r = builder_.CreateTrunc(val, i16Ty_, "cast_u16");
        else if (srcTy == i32Ty_) r = builder_.CreateTrunc(val, i16Ty_, "cast_u16");
        else if (srcTy == i8Ty_) r = builder_.CreateZExt(val, i16Ty_, "cast_u16");
        else if (srcTy == i1Ty_) r = builder_.CreateZExt(val, i16Ty_, "cast_u16");
        else if (srcTy->isDoubleTy()) r = builder_.CreateFPToUI(val, i16Ty_, "cast_u16");
        else if (srcTy == f32Ty_) r = builder_.CreateFPToUI(val, i16Ty_, "cast_u16");
        else codegenError("cannot cast to u16");
        return withMeta(r, "u16");
    }
    if (target == "u32") {
        llvm::Value *r;
        if (srcTy == i32Ty_) r = val;
        else if (srcTy == i64Ty_) r = builder_.CreateTrunc(val, i32Ty_, "cast_u32");
        else if (srcTy == i16Ty_) r = builder_.CreateZExt(val, i32Ty_, "cast_u32");
        else if (srcTy == i8Ty_) r = builder_.CreateZExt(val, i32Ty_, "cast_u32");
        else if (srcTy == i1Ty_) r = builder_.CreateZExt(val, i32Ty_, "cast_u32");
        else if (srcTy->isDoubleTy()) r = builder_.CreateFPToUI(val, i32Ty_, "cast_u32");
        else if (srcTy == f32Ty_) r = builder_.CreateFPToUI(val, i32Ty_, "cast_u32");
        else codegenError("cannot cast to u32");
        return withMeta(r, "u32");
    }
    if (target == "u64") {
        llvm::Value *r;
        if (srcTy == i64Ty_) r = val;
        else if (srcTy == i32Ty_) r = builder_.CreateZExt(val, i64Ty_, "cast_u64");
        else if (srcTy == i16Ty_) r = builder_.CreateZExt(val, i64Ty_, "cast_u64");
        else if (srcTy == i8Ty_) r = builder_.CreateZExt(val, i64Ty_, "cast_u64");
        else if (srcTy == i1Ty_) r = builder_.CreateZExt(val, i64Ty_, "cast_u64");
        else if (srcTy->isDoubleTy()) r = builder_.CreateFPToUI(val, i64Ty_, "cast_u64");
        else if (srcTy == f32Ty_) r = builder_.CreateFPToUI(val, i64Ty_, "cast_u64");
        else codegenError("cannot cast to u64");
        return withMeta(r, "u64");
    }
    if (target == "f32") {
        if (srcTy == f32Ty_) return val;
        if (srcTy->isDoubleTy()) return builder_.CreateFPTrunc(val, f32Ty_, "cast_f32");
        if (srcTy == i64Ty_) {
            if (isUnsignedLowLevel(val)) return builder_.CreateUIToFP(val, f32Ty_, "cast_f32");
            return builder_.CreateSIToFP(val, f32Ty_, "cast_f32");
        }
        if (srcTy == i32Ty_) {
            if (isUnsignedLowLevel(val)) return builder_.CreateUIToFP(val, f32Ty_, "cast_f32");
            return builder_.CreateSIToFP(val, f32Ty_, "cast_f32");
        }
        if (srcTy == i16Ty_) {
            if (isUnsignedLowLevel(val)) return builder_.CreateUIToFP(val, f32Ty_, "cast_f32");
            return builder_.CreateSIToFP(val, f32Ty_, "cast_f32");
        }
        if (srcTy == i8Ty_)  return builder_.CreateUIToFP(val, f32Ty_, "cast_f32");
        if (srcTy == i1Ty_)  return builder_.CreateUIToFP(val, f32Ty_, "cast_f32");
        codegenError("cannot cast to f32");
    }
    codegenError("unsupported cast target type: " + target);
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<InterpolatedStringExpr> &e) {
    // Convert each expression to string
    std::vector<llvm::Value*> strParts;
    for (size_t i = 0; i < e->parts.size(); ++i) {
        if (!e->parts[i].empty())
            strParts.push_back(builder_.CreateGlobalString(e->parts[i], ".fstr_lit"));
        else
            strParts.push_back(nullptr); // empty literal segment
        if (i < e->exprs.size()) {
            llvm::Value *exprVal = emitExpr(*e->exprs[i]);
            strParts.push_back(valueToString(exprVal));
        }
    }

    // Compute total length
    auto strlenFn = getStdlibStrlen();
    auto mallocFn = getStdlibMalloc();
    auto memcpyFn = getStdlibMemcpy();

    llvm::Value *totalLen = llvm::ConstantInt::get(i64Ty_, 0);
    std::vector<llvm::Value*> lengths;
    for (auto *sp : strParts) {
        if (sp) {
            llvm::Value *len = builder_.CreateCall(strlenFn, {sp}, "fstr_len");
            lengths.push_back(len);
            totalLen = builder_.CreateAdd(totalLen, len, "fstr_total");
        } else {
            lengths.push_back(llvm::ConstantInt::get(i64Ty_, 0));
        }
    }

    // Allocate result buffer
    llvm::Value *bufSize = builder_.CreateAdd(totalLen, llvm::ConstantInt::get(i64Ty_, 1), "fstr_bufsize");
    llvm::Value *buf = builder_.CreateCall(mallocFn, {bufSize}, "fstr_buf");

    // Copy segments
    llvm::Value *offset = llvm::ConstantInt::get(i64Ty_, 0);
    for (size_t i = 0; i < strParts.size(); ++i) {
        if (strParts[i]) {
            llvm::Value *dst = builder_.CreateGEP(builder_.getInt8Ty(), buf, {offset}, "fstr_dst");
            builder_.CreateCall(memcpyFn, {dst, strParts[i], lengths[i]});
            offset = builder_.CreateAdd(offset, lengths[i], "fstr_off");
        }
    }

    // Null-terminate
    llvm::Value *endPtr = builder_.CreateGEP(builder_.getInt8Ty(), buf, {offset}, "fstr_end");
    builder_.CreateStore(llvm::ConstantInt::get(builder_.getInt8Ty(), 0), endPtr);

    return buf;
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<TernaryExpr> &e) {
    llvm::Value *cond = emitExpr(*e->condition);
    cond = toBool(cond);

    llvm::BasicBlock *trueBB = llvm::BasicBlock::Create(*ctx_, "ternary.true", fn_);
    llvm::BasicBlock *falseBB = llvm::BasicBlock::Create(*ctx_, "ternary.false", fn_);
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "ternary.merge", fn_);

    builder_.CreateCondBr(cond, trueBB, falseBB);

    builder_.SetInsertPoint(trueBB);
    llvm::Value *trueVal = emitExpr(*e->true_expr);
    llvm::BasicBlock *trueEndBB = builder_.GetInsertBlock();
    builder_.CreateBr(mergeBB);

    builder_.SetInsertPoint(falseBB);
    llvm::Value *falseVal = emitExpr(*e->false_expr);
    llvm::BasicBlock *falseEndBB = builder_.GetInsertBlock();
    builder_.CreateBr(mergeBB);

    if (trueVal->getType() != falseVal->getType())
        codegenError("ternary expression: both branches must have the same type");

    // Semantic type check for pointer types (str, List, Map, Set are all ptrTy_)
    if (trueVal->getType() == ptrTy_) {
        enum class SemanticKind { Str, List, Map, Set, Other };
        auto classify = [&](llvm::Value *v) -> SemanticKind {
            if (list_element_types_.count(v)) return SemanticKind::List;
            if (map_key_types_.count(v)) return SemanticKind::Map;
            if (set_element_types_.count(v)) return SemanticKind::Set;
            if (channel_element_types_.count(v)) return SemanticKind::Other;
            if (task_result_types_.count(v)) return SemanticKind::Other;
            return SemanticKind::Str;
        };
        SemanticKind trueKind = classify(trueVal);
        SemanticKind falseKind = classify(falseVal);
        if (trueKind != falseKind)
            codegenError("ternary expression: both branches must have the same type");

        // For List, check element types match
        if (trueKind == SemanticKind::List) {
            llvm::Type *trueElem = list_element_types_[trueVal];
            llvm::Type *falseElem = list_element_types_[falseVal];
            if (trueElem != falseElem)
                codegenError("ternary expression: both branches must have the same type");
        }
    }

    builder_.SetInsertPoint(mergeBB);
    llvm::PHINode *phi = builder_.CreatePHI(trueVal->getType(), 2, "ternary");
    phi->addIncoming(trueVal, trueEndBB);
    phi->addIncoming(falseVal, falseEndBB);

    propagateAllMetadata(trueVal, phi);

    return phi;
}

// ===== TypeAliasStmt =====

void CodeGen::emitStmt(TypeAliasStmt &s) {
    // Type aliases are resolved at compile time via resolveType()
    // Store the alias mapping for later lookup
    type_aliases_[s.name] = s.target_type;
}

// ===== RangeExpr =====

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<RangeExpr> &e) {
    llvm::Value *startVal = emitExpr(*e->start);
    llvm::Value *endVal = emitExpr(*e->end);

    if (startVal->getType() != i64Ty_ || endVal->getType() != i64Ty_)
        codegenError("range (..) operator requires int operands");

    // Calculate length: end - start + 1 (inclusive range)
    llvm::Value *diff = builder_.CreateSub(endVal, startVal, "range_diff");
    llvm::Value *length = builder_.CreateAdd(diff, llvm::ConstantInt::get(i64Ty_, 1), "range_len");

    // Clamp negative length to 0
    llvm::Value *isNeg = builder_.CreateICmpSLT(length, llvm::ConstantInt::get(i64Ty_, 0), "len_neg");
    length = builder_.CreateSelect(isNeg, llvm::ConstantInt::get(i64Ty_, 0), length, "range_len_clamped");

    // Allocate list: header + data
    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
    uint64_t elemSize = dl.getTypeAllocSize(i64Ty_);

    auto mallocFn = getStdlibMalloc();

    llvm::Value *headerPtr = builder_.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "range_hdr");
    llvm::Value *dataSize = builder_.CreateMul(length, llvm::ConstantInt::get(i64Ty_, elemSize), "data_size");
    llvm::Value *dataPtr = builder_.CreateCall(mallocFn, {dataSize}, "range_data");

    // Store header: len, cap, data
    builder_.CreateStore(length, builder_.CreateStructGEP(listHeaderTy_, headerPtr, 0));
    builder_.CreateStore(length, builder_.CreateStructGEP(listHeaderTy_, headerPtr, 1));
    builder_.CreateStore(dataPtr, builder_.CreateStructGEP(listHeaderTy_, headerPtr, 2));

    // Fill data with start..end
    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "range.cond", fn_);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "range.body", fn_);
    llvm::BasicBlock *endBB  = llvm::BasicBlock::Create(*ctx_, "range.end", fn_);

    llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "range_i");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
    builder_.CreateBr(condBB);

    builder_.SetInsertPoint(condBB);
    llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "i");
    llvm::Value *cond = builder_.CreateICmpSLT(iVal, length, "range_cond");
    builder_.CreateCondBr(cond, bodyBB, endBB);

    builder_.SetInsertPoint(bodyBB);
    llvm::Value *curI = builder_.CreateLoad(i64Ty_, iVar, "cur_i");
    llvm::Value *val = builder_.CreateAdd(startVal, curI, "range_val");
    llvm::Value *elemPtr = builder_.CreateGEP(i64Ty_, dataPtr, {curI}, "range_elem_ptr");
    builder_.CreateStore(val, elemPtr);
    llvm::Value *nextI = builder_.CreateAdd(curI, llvm::ConstantInt::get(i64Ty_, 1), "next_i");
    builder_.CreateStore(nextI, iVar);
    builder_.CreateBr(condBB);

    builder_.SetInsertPoint(endBB);
    list_element_types_[headerPtr] = i64Ty_;
    return headerPtr;
}

// ===== NoneExpr =====

llvm::Value *CodeGen::emitExprVariant(const NoneExpr &) {
    // Build a None value for the expected Option type
    // The type will be inferred from context (assignment, comparison, etc.)
    // Default to Option<int> if no context is available
    llvm::StructType *optTy = getOptionType(i64Ty_);
    return buildNoneValue(optTy);
}

// ===== ErrorPropagateExpr (?) =====

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<ErrorPropagateExpr> &e) {
    llvm::Value *operandVal = emitExpr(*e->operand);
    llvm::Type *operandTy = operandVal->getType();
    if (!isResultType(operandTy))
        codegenError("'?' operator requires a Result type operand");

    llvm::Type *fnRetTy = fn_->getReturnType();
    if (!isResultType(fnRetTy))
        codegenError("'?' operator can only be used in a function that returns Result");

    llvm::StructType *operandResultTy = llvm::cast<llvm::StructType>(operandTy);
    llvm::StructType *retResultTy = llvm::cast<llvm::StructType>(fnRetTy);
    if (operandResultTy->getElementType(2) != retResultTy->getElementType(2))
        codegenError("'?' operator error type mismatch: operand and function return different error types");

    llvm::Value *isOk = builder_.CreateExtractValue(operandVal, 0, "is_ok");
    llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "try.ok", fn_);
    llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, "try.err", fn_);
    builder_.CreateCondBr(isOk, okBB, errBB);

    // Err path: extract error, wrap in function return type, return
    builder_.SetInsertPoint(errBB);
    llvm::Value *errVal = builder_.CreateExtractValue(operandVal, 2, "err_val");
    llvm::Value *retErr = buildErrValue(errVal, retResultTy);
    emitEnsureChecks(retErr);
    builder_.CreateRet(retErr);

    // Ok path: extract ok value, continue
    builder_.SetInsertPoint(okBB);
    llvm::Value *okVal = builder_.CreateExtractValue(operandVal, 1, "ok_val");
    propagateAllMetadata(operandVal, okVal);
    return okVal;
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<SpawnExpr> &e) {
    auto *callPtr = std::get_if<std::unique_ptr<CallExpr>>(&e->operand->data);
    if (!callPtr)
        codegenError("spawn requires a function call expression");

    const CallExpr &callExpr = *(*callPtr);
    llvm::Function *directFunc = nullptr;
    llvm::Value *calleeVal = nullptr;
    FnTypeInfo calleeInfo;
    std::vector<llvm::Value*> argVals;

    auto namedIt = functions_.find(callExpr.callee);
    if (namedIt != functions_.end()) {
        directFunc = resolveOverload(callExpr.callee, callExpr.args, argVals);
        if (!directFunc)
            codegenError("spawn requires a resolvable user function call");
    } else if (llvm::AllocaInst *varPtr = findVar(callExpr.callee)) {
        auto fnIt = fn_type_info_.find(varPtr);
        if (fnIt == fn_type_info_.end())
            codegenError("spawn requires a function or lambda call");
        calleeInfo = fnIt->second;
        for (auto &arg : callExpr.args)
            argVals.push_back(emitExpr(*arg));
        calleeVal = builder_.CreateLoad(ptrTy_, varPtr, callExpr.callee + ".spawn_fn");
    } else {
        codegenError("spawn requires a user-defined function or lambda call");
    }

    llvm::Type *resultTy = directFunc ? directFunc->getReturnType() : calleeInfo.returnType;
    if (resultTy->isVoidTy())
        codegenError("spawn does not support Unit-returning calls");

    std::vector<llvm::Type*> envFields;
    if (!directFunc)
        envFields.push_back(ptrTy_);
    for (auto *argVal : argVals)
        envFields.push_back(argVal->getType());
    if (envFields.empty())
        envFields.push_back(i8Ty_);
    llvm::StructType *envTy = llvm::StructType::get(*ctx_, envFields);

    llvm::FunctionType *mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
    llvm::FunctionCallee mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
    const llvm::DataLayout &dl = mod_->getDataLayout();
    llvm::Value *envPtr = builder_.CreateCall(
        mallocFn,
        {llvm::ConstantInt::get(i64Ty_, std::max<uint64_t>(1, dl.getTypeAllocSize(envTy)))},
        "spawn_env");

    unsigned fieldIndex = 0;
    if (directFunc && argVals.empty()) {
        llvm::Value *dummyField = builder_.CreateStructGEP(envTy, envPtr, 0, "spawn_env_dummy");
        builder_.CreateStore(llvm::ConstantInt::get(i8Ty_, 0), dummyField);
    } else if (!directFunc) {
        llvm::Value *calleeField = builder_.CreateStructGEP(envTy, envPtr, fieldIndex++, "spawn_env_fn");
        builder_.CreateStore(calleeVal, calleeField);
    }
    for (size_t i = 0; i < argVals.size(); ++i) {
        llvm::Value *argField = builder_.CreateStructGEP(
            envTy, envPtr, fieldIndex++, "spawn_env_arg." + std::to_string(i));
        builder_.CreateStore(argVals[i], argField);
    }

    llvm::FunctionType *thunkTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {ptrTy_, ptrTy_}, false);
    llvm::Function *thunk = llvm::Function::Create(
        thunkTy, llvm::Function::InternalLinkage,
        "__ry_spawn." + std::to_string(lambda_counter_++), *mod_);

    {
        FnScope guard(*this);
        fn_ = thunk;
        pushScope();

        llvm::BasicBlock *entry = llvm::BasicBlock::Create(*ctx_, "entry", thunk);
        builder_.SetInsertPoint(entry);

        auto argIt = thunk->arg_begin();
        llvm::Value *envRaw = &*argIt++;
        envRaw->setName("env_raw");
        llvm::Value *outRaw = &*argIt;
        outRaw->setName("out_raw");

        llvm::Value *typedEnv = builder_.CreateBitCast(envRaw, ptrTy_, "spawn_env_typed");

        std::vector<llvm::Value*> thunkArgs;
        fieldIndex = 0;
        llvm::Value *thunkCallee = nullptr;
        if (!directFunc) {
            llvm::Value *calleeField = builder_.CreateStructGEP(envTy, typedEnv, fieldIndex++, "spawn_fn_field");
            thunkCallee = builder_.CreateLoad(ptrTy_, calleeField, "spawn_fn");
        }
        for (size_t i = 0; i < argVals.size(); ++i) {
            llvm::Type *argTy = argVals[i]->getType();
            llvm::Value *argField = builder_.CreateStructGEP(
                envTy, typedEnv, fieldIndex++, "spawn_arg_field." + std::to_string(i));
            thunkArgs.push_back(builder_.CreateLoad(argTy, argField, "spawn_arg." + std::to_string(i)));
        }

        llvm::Value *result = nullptr;
        if (directFunc) {
            result = builder_.CreateCall(directFunc, thunkArgs, "spawn_call");
        } else {
            result = emitLambdaCall(thunkCallee, calleeInfo, thunkArgs, "spawn_call");
        }

        llvm::Value *outTyped = builder_.CreateBitCast(outRaw, ptrTy_, "spawn_out_typed");
        builder_.CreateStore(result, outTyped);
        builder_.CreateRetVoid();
    }

    llvm::Value *task = nullptr;
    if (!task_group_stack_.empty()) {
        llvm::FunctionType *groupSpawnTy = llvm::FunctionType::get(
            ptrTy_, {ptrTy_, ptrTy_, ptrTy_, i64Ty_}, false);
        llvm::FunctionCallee groupSpawnFn = mod_->getOrInsertFunction("__ry_task_group_spawn", groupSpawnTy);
        task = builder_.CreateCall(
            groupSpawnFn,
            {
                task_group_stack_.back(),
                builder_.CreateBitCast(thunk, ptrTy_),
                envPtr,
                llvm::ConstantInt::get(i64Ty_, dl.getTypeAllocSize(resultTy))
            },
            "task");
    } else {
        llvm::FunctionType *spawnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, i64Ty_}, false);
        llvm::FunctionCallee spawnFn = mod_->getOrInsertFunction("__ry_task_spawn", spawnTy);
        task = builder_.CreateCall(
            spawnFn,
            {
                builder_.CreateBitCast(thunk, ptrTy_),
                envPtr,
                llvm::ConstantInt::get(i64Ty_, dl.getTypeAllocSize(resultTy))
            },
            "task");
    }
    task_result_types_[task] = resultTy;
    return task;
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<AwaitExpr> &e) {
    llvm::Value *taskVal = emitExpr(*e->operand);
    llvm::Type *resultTy = getTaskResultType(taskVal);
    if (!resultTy)
        codegenError("await requires a Task value");

    llvm::FunctionType *joinTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {ptrTy_, ptrTy_}, false);
    llvm::FunctionCallee joinFn = mod_->getOrInsertFunction("__ry_task_join", joinTy);

    if (resultTy->isVoidTy()) {
        return builder_.CreateCall(joinFn, {taskVal, llvm::ConstantPointerNull::get(
            llvm::cast<llvm::PointerType>(ptrTy_))});
    }

    llvm::AllocaInst *resultSlot = builder_.CreateAlloca(resultTy, nullptr, "await_result");
    builder_.CreateCall(joinFn, {taskVal, resultSlot});
    return builder_.CreateLoad(resultTy, resultSlot, "awaited");
}

// Common helper: emit IR for string repetition.
// strVal must be ptr (i8*), n must be i64.
llvm::Value *CodeGen::emitStringRepeat(llvm::Value *strVal, llvm::Value *n) {
    auto strlenFn = getStdlibStrlen();
    auto mallocFn = getStdlibMalloc();
    auto memcpyFn = getStdlibMemcpy();

    llvm::Value *strLen = builder_.CreateCall(strlenFn, {strVal}, "str_len");

    // If n <= 0, return empty string
    llvm::Value *nPos = builder_.CreateICmpSGT(n, llvm::ConstantInt::get(i64Ty_, 0), "n_pos");

    llvm::BasicBlock *emptyBB = llvm::BasicBlock::Create(*ctx_, "str_rep.empty", fn_);
    llvm::BasicBlock *repeatBB = llvm::BasicBlock::Create(*ctx_, "str_rep.repeat", fn_);
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "str_rep.merge", fn_);

    builder_.CreateCondBr(nPos, repeatBB, emptyBB);

    // Empty case: return ""
    builder_.SetInsertPoint(emptyBB);
    llvm::Value *emptyStr = builder_.CreateGlobalString("", ".empty_str");
    builder_.CreateBr(mergeBB);

    // Repeat case
    builder_.SetInsertPoint(repeatBB);
    llvm::Value *totalLen = builder_.CreateMul(strLen, n, "total_len");
    llvm::Value *bufSize = builder_.CreateAdd(totalLen, llvm::ConstantInt::get(i64Ty_, 1), "buf_size");
    llvm::Value *buf = builder_.CreateCall(mallocFn, {bufSize}, "rep_buf");

    // Loop: copy strVal into buf n times
    llvm::BasicBlock *loopBB = llvm::BasicBlock::Create(*ctx_, "str_rep.loop", fn_);
    llvm::BasicBlock *doneBB = llvm::BasicBlock::Create(*ctx_, "str_rep.done", fn_);

    builder_.CreateBr(loopBB);
    builder_.SetInsertPoint(loopBB);

    llvm::PHINode *i = builder_.CreatePHI(i64Ty_, 2, "i");
    i->addIncoming(llvm::ConstantInt::get(i64Ty_, 0), repeatBB);

    llvm::Value *offset = builder_.CreateMul(i, strLen, "offset");
    llvm::Value *dst = builder_.CreateGEP(builder_.getInt8Ty(), buf, {offset}, "dst");
    builder_.CreateCall(memcpyFn, {dst, strVal, strLen});

    llvm::Value *iNext = builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1), "i_next");
    i->addIncoming(iNext, loopBB);
    llvm::Value *cond = builder_.CreateICmpSLT(iNext, n, "loop_cond");
    builder_.CreateCondBr(cond, loopBB, doneBB);

    builder_.SetInsertPoint(doneBB);
    // Null-terminate
    llvm::Value *endPtr = builder_.CreateGEP(builder_.getInt8Ty(), buf, {totalLen}, "end_ptr");
    builder_.CreateStore(llvm::ConstantInt::get(builder_.getInt8Ty(), 0), endPtr);
    builder_.CreateBr(mergeBB);

    // Merge
    builder_.SetInsertPoint(mergeBB);
    llvm::PHINode *result = builder_.CreatePHI(ptrTy_, 2, "str_rep_result");
    result->addIncoming(emptyStr, emptyBB);
    result->addIncoming(buf, doneBB);
    return result;
}
