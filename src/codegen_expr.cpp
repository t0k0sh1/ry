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
    return cachedGlobalString(e.value, ".str");
}

llvm::Value *CodeGen::emitExprVariant(const VariableExpr &e) {
    llvm::AllocaInst *alloca = findVar(e.name);
    if (alloca) {
        if (deprecated_variables_.count(e.name))
            emitDeprecationWarning(e.name);
        llvm::Type *ty = alloca->getAllocatedType();
        // Fixed-length arrays: return alloca pointer for GEP-based indexing
        if (llvm::isa<llvm::ArrayType>(ty))
            return alloca;
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

llvm::Value *CodeGen::findAndCallOverload(const std::string &opFnName,
                                           llvm::ArrayRef<llvm::Value*> args,
                                           const char *callName) {
    auto fit = functions_.find(opFnName);
    if (fit == functions_.end()) return nullptr;

    for (auto &entry : fit->second) {
        if (entry.paramTypes.size() != args.size()) continue;
        bool match = true;
        for (size_t i = 0; i < args.size(); ++i) {
            if (entry.paramTypes[i] != args[i]->getType()) {
                match = false; break;
            }
        }
        if (!match) continue;
        if (entry.func->getReturnType()->isVoidTy())
            return builder_.CreateCall(entry.func, args);
        return builder_.CreateCall(entry.func, args, callName);
    }
    return nullptr;
}

llvm::Value *CodeGen::tryOperatorCall(const std::string &opFnName,
                                       llvm::Value *lhs, llvm::Value *rhs) {
    return findAndCallOverload(opFnName, {lhs, rhs}, "opcall");
}

llvm::Value *CodeGen::tryUnaryOperatorCall(const std::string &opFnName,
                                            llvm::Value *operand) {
    return findAndCallOverload(opFnName, {operand}, "opcall");
}

llvm::Value *CodeGen::trySubscriptOperatorCall(
    llvm::Value *object, llvm::ArrayRef<llvm::Value*> indices) {
    llvm::SmallVector<llvm::Value*, 4> args;
    args.push_back(object);
    args.append(indices.begin(), indices.end());
    return findAndCallOverload("operator[]", args, "subscript");
}

bool CodeGen::trySubscriptAssignOperatorCall(
    llvm::Value *object, llvm::ArrayRef<llvm::Value*> indices, llvm::Value *value) {
    llvm::SmallVector<llvm::Value*, 4> args;
    args.push_back(object);
    args.append(indices.begin(), indices.end());
    args.push_back(value);
    return findAndCallOverload("operator[]=", args, "subscr_assign") != nullptr;
}

llvm::Value *CodeGen::tryCallOperator(const std::string &callee,
                                       const std::vector<ExprPtr> &args) {
    llvm::AllocaInst *varPtr = findVar(callee);
    if (!varPtr) return nullptr;
    llvm::Type *allocTy = varPtr->getAllocatedType();
    if (!allocTy->isStructTy()) return nullptr;
    llvm::Value *obj = builder_.CreateLoad(allocTy, varPtr, callee + ".val");
    llvm::SmallVector<llvm::Value*, 4> callArgs;
    callArgs.push_back(obj);
    for (auto &arg : args)
        callArgs.push_back(emitExpr(*arg));
    return findAndCallOverload("operator()", callArgs, "call_op");
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

    // Reject str with non-str operands
    if (lhs->getType() == ptrTy_ || rhs->getType() == ptrTy_)
        codegenError("type error: operator '" + op + "' not supported between str and non-str types");

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
    // Reject str operands
    if (lhs->getType() == ptrTy_ || rhs->getType() == ptrTy_)
        codegenError("type error: bitwise operator '" + op + "' not supported for str type");
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
        if (lhs->getType() == ptrTy_ || rhs->getType() == ptrTy_)
            codegenError("type error: operator '**' not supported between str and non-str types");
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

    // Auto-convert non-str to str for + operator (#393)
    auto isScalarTy = [&](llvm::Value *v) {
        return v->getType()->isIntegerTy() || v->getType()->isDoubleTy();
    };
    if (op == "+" && lhs->getType() == ptrTy_ && isScalarTy(rhs))
        rhs = valueToString(rhs);
    else if (op == "+" && rhs->getType() == ptrTy_ && isScalarTy(lhs))
        lhs = valueToString(lhs);

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

    // Reject str with non-str operands (must come after string concat/repeat checks)
    if (lhs->getType() == ptrTy_ || rhs->getType() == ptrTy_)
        codegenError("type error: operator '" + op + "' not supported between str and non-str types");

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

        if (auto *result = tryOperatorCall("operatorin", elem, container)) {
            if (e->op == "not in")
                result = builder_.CreateNot(result, "user_not_in");
            return result;
        }

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

    // Compute low-level type name hint from AST suffixes for constant operands (#311).
    std::string llHint = getExprLowLevelSuffix(*e->lhs);
    if (llHint.empty()) llHint = getExprLowLevelSuffix(*e->rhs);

    return emitBinaryOp(op, lhs, rhs, llHint);
}

llvm::Value *CodeGen::emitBinaryOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs,
                                    const std::string &llNameHint) {
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

    if (op == "==" || op == "!=" || op == "<" ||
        op == "<=" || op == ">"  || op == ">=")
        return emitComparisonOp(op, lhs, rhs, llNameHint);

    if (op == "&" || op == "|" || op == "^" ||
        op == "<<" || op == ">>" || op == ">>>")
        return emitBitwiseOp(op, lhs, rhs, llNameHint);

    return emitArithmeticOp(op, lhs, rhs, llNameHint);
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
            if (type_meta_[TM_ListElem].count(v)) return SemanticKind::List;
            if (type_meta_[TM_MapKey].count(v)) return SemanticKind::Map;
            if (type_meta_[TM_SetElem].count(v)) return SemanticKind::Set;
            if (type_meta_[TM_TaskResult].count(v)) return SemanticKind::Other;
            return SemanticKind::Str;
        };
        SemanticKind trueKind = classify(trueVal);
        SemanticKind falseKind = classify(falseVal);
        if (trueKind != falseKind)
            codegenError("ternary expression: both branches must have the same type");

        // For List, check element types match
        if (trueKind == SemanticKind::List) {
            llvm::Type *trueElem = type_meta_[TM_ListElem][trueVal];
            llvm::Type *falseElem = type_meta_[TM_ListElem][falseVal];
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
    llvm::Type *operandErrTy = operandResultTy->getElementType(2);
    llvm::Type *retErrTy = retResultTy->getElementType(2);

    llvm::Value *isOk = builder_.CreateExtractValue(operandVal, 0, "is_ok");
    llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "try.ok", fn_);
    llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, "try.err", fn_);
    builder_.CreateCondBr(isOk, okBB, errBB);

    // Err path: extract error, wrap in function return type, return
    builder_.SetInsertPoint(errBB);
    llvm::Value *errVal = builder_.CreateExtractValue(operandVal, 2, "err_val");
    if (operandErrTy != retErrTy) {
        if (auto *sliced = tryEmitSubtypeCoerce(errVal, retErrTy))
            errVal = sliced;
        else
            codegenError("'?' operator error type mismatch: operand and function return different error types");
    }
    llvm::Value *retErr = buildErrValue(errVal, retResultTy);
    emitEnsureChecks(retErr);
    emitScopeCleanupToDepth(0);
    builder_.CreateRet(retErr);

    // Ok path: extract ok value, continue
    builder_.SetInsertPoint(okBB);
    llvm::Value *okVal = builder_.CreateExtractValue(operandVal, 1, "ok_val");
    propagateAllMetadata(operandVal, okVal);
    return okVal;
}

llvm::Value *CodeGen::emitTaskWait(llvm::Value *taskVal, const char *runtimeFn, const char *label) {
    llvm::Type *resultTy = getTaskResultType(taskVal);
    if (!resultTy)
        codegenError(std::string(label) + "() requires a Task value");

    llvm::FunctionType *fnTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {ptrTy_, ptrTy_}, false);
    llvm::FunctionCallee fn = mod_->getOrInsertFunction(runtimeFn, fnTy);

    if (resultTy->isVoidTy()) {
        return builder_.CreateCall(fn, {taskVal, llvm::ConstantPointerNull::get(
            llvm::cast<llvm::PointerType>(ptrTy_))});
    }

    llvm::AllocaInst *resultSlot = builder_.CreateAlloca(resultTy, nullptr, std::string(label) + "_result");
    builder_.CreateCall(fn, {taskVal, resultSlot});
    return builder_.CreateLoad(resultTy, resultSlot, std::string(label) + "_val");
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<AwaitExpr> &e) {
    return emitTaskWait(emitExpr(*e->operand), "__ry_task_join", "await");
}
