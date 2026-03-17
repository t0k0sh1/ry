#include "ry/codegen.hpp"
#include <stdexcept>

llvm::Value *CodeGen::emitExpr(const ExprNode &node) {
    return std::visit([this](const auto &e) -> llvm::Value* { return emitExprVariant(e); },
                      node.data);
}

llvm::Value *CodeGen::emitExprVariant(const NumberExpr &e) {
    return llvm::ConstantInt::get(i64Ty_, e.value, true);
}

llvm::Value *CodeGen::emitExprVariant(const FloatExpr &e) {
    return llvm::ConstantFP::get(f64Ty_, e.value);
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
    // Try named function reference
    auto fit = functions_.find(e.name);
    if (fit != functions_.end() && fit->second.size() == 1) {
        if (deprecated_functions_.count(e.name))
            emitDeprecationWarning(e.name);
        llvm::Function *func = fit->second[0].func;
        FnTypeInfo info;
        info.paramTypes = fit->second[0].paramTypes;
        info.returnType = func->getReturnType();
        fn_type_info_[func] = info;
        return func;
    }
    throw std::runtime_error("undefined variable: " + e.name);
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<UnaryExpr> &e) {
    llvm::Value *val = emitExpr(*e->operand);

    // Try user-defined unary operator first
    std::string opFnName = "operator" + e->op;
    if (auto *result = tryUnaryOperatorCall(opFnName, val))
        return result;

    if (e->op == "+") {
        return val;
    }
    if (e->op == "-") {
        if (val->getType()->isDoubleTy())
            return builder_.CreateFNeg(val, "fneg");
        val = promoteToInt(val);
        return builder_.CreateNeg(val, "neg");
    }
    if (e->op == "not") {
        llvm::Value *boolVal = toBool(val);
        return builder_.CreateNot(boolVal, "not");
    }
    if (e->op == "~") {
        if (val->getType()->isDoubleTy())
            throw std::runtime_error("bitwise NOT (~) requires integer, got float");
        val = promoteToInt(val);
        return builder_.CreateNot(val, "bnot");
    }
    throw std::runtime_error("unknown unary operator: " + e->op);
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

llvm::Value *CodeGen::emitComparisonOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs) {
    // String comparison via strcmp
    if (lhs->getType() == ptrTy_ && rhs->getType() == ptrTy_) {
        auto strcmpTy = llvm::FunctionType::get(i32Ty_, {ptrTy_, ptrTy_}, false);
        auto strcmpFn = mod_->getOrInsertFunction("strcmp", strcmpTy);
        llvm::Value *cmp = builder_.CreateCall(strcmpFn, {lhs, rhs}, "strcmp");
        llvm::Value *zero = llvm::ConstantInt::get(i32Ty_, 0);
        if (op == "==") return builder_.CreateICmpEQ(cmp, zero, "str_eq");
        if (op == "!=") return builder_.CreateICmpNE(cmp, zero, "str_ne");
        if (op == "<")  return builder_.CreateICmpSLT(cmp, zero, "str_lt");
        if (op == "<=") return builder_.CreateICmpSLE(cmp, zero, "str_le");
        if (op == ">")  return builder_.CreateICmpSGT(cmp, zero, "str_gt");
        if (op == ">=") return builder_.CreateICmpSGE(cmp, zero, "str_ge");
        throw std::runtime_error("unsupported string comparison: " + op);
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

llvm::Value *CodeGen::emitLogicalOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs) {
    llvm::Value *lhsBool = toBool(lhs);
    llvm::Value *rhsBool = toBool(rhs);
    if (op == "and")
        return builder_.CreateAnd(lhsBool, rhsBool, "and");
    return builder_.CreateOr(lhsBool, rhsBool, "or");
}

llvm::Value *CodeGen::emitBitwiseOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs) {
    if (lhs->getType()->isDoubleTy() || rhs->getType()->isDoubleTy())
        throw std::runtime_error(
            "bitwise operator '" + op + "' requires integer operands, got float");
    lhs = promoteToInt(lhs);
    rhs = promoteToInt(rhs);
    if (op == "&")  return builder_.CreateAnd(lhs, rhs,  "band");
    if (op == "|")  return builder_.CreateOr(lhs,  rhs,  "bor");
    if (op == "^")  return builder_.CreateXor(lhs, rhs,  "bxor");
    if (op == "<<") return builder_.CreateShl(lhs,  rhs, "shl");
    if (op == ">>") return builder_.CreateAShr(lhs, rhs, "ashr");
    if (op == ">>>") return builder_.CreateLShr(lhs, rhs, "lshr");
    throw std::runtime_error("unknown bitwise operator: " + op);
}

llvm::Value *CodeGen::emitArithmeticOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs) {
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
        auto strlenTy = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
        auto strlenFn = mod_->getOrInsertFunction("strlen", strlenTy);
        auto mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
        auto mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
        auto strcpyTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_}, false);
        auto strcpyFn = mod_->getOrInsertFunction("strcpy", strcpyTy);
        auto strcatTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_}, false);
        auto strcatFn = mod_->getOrInsertFunction("strcat", strcatTy);

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

            auto strlenTy = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
            auto strlenFn = mod_->getOrInsertFunction("strlen", strlenTy);
            auto mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
            auto mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
            auto memcpyTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, i64Ty_}, false);
            auto memcpyFn = mod_->getOrInsertFunction("memcpy", memcpyTy);

            llvm::Value *strLen = builder_.CreateCall(strlenFn, {strVal}, "str_len");

            // If n <= 0, return empty string
            llvm::Value *nPos = builder_.CreateICmpSGT(intVal, llvm::ConstantInt::get(i64Ty_, 0), "n_pos");

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
            llvm::Value *totalLen = builder_.CreateMul(strLen, intVal, "total_len");
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
            llvm::Value *cond = builder_.CreateICmpSLT(iNext, intVal, "loop_cond");
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
    }

    // // 整数除算: f64入力はi64に変換してからsdiv
    if (op == "//") {
        lhs = promoteToInt(lhs);
        rhs = promoteToInt(rhs);
        if (lhs->getType()->isDoubleTy()) lhs = builder_.CreateFPToSI(lhs, i64Ty_, "lhs_i");
        if (rhs->getType()->isDoubleTy()) rhs = builder_.CreateFPToSI(rhs, i64Ty_, "rhs_i");
        return builder_.CreateSDiv(lhs, rhs, "idiv");
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
        throw std::runtime_error("unknown operator: " + op);
    }
    if (op == "+") return builder_.CreateAdd(lhs, rhs, "add");
    if (op == "-") return builder_.CreateSub(lhs, rhs, "sub");
    if (op == "*") return builder_.CreateMul(lhs, rhs, "mul");
    throw std::runtime_error("unknown operator: " + op);
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
                throw std::runtime_error("'" + e->op + "' operator: element type mismatch");
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
                throw std::runtime_error("'" + e->op + "' operator: key type mismatch");
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
                throw std::runtime_error("'" + e->op + "' operator: element type mismatch");

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
                auto strcmpTy = llvm::FunctionType::get(i32Ty_, {ptrTy_, ptrTy_}, false);
                auto strcmpFn = mod_->getOrInsertFunction("strcmp", strcmpTy);
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

        throw std::runtime_error("'" + e->op + "' operator requires a set, list, or map on the right side");
    }

    llvm::Value *lhs = emitExpr(*e->lhs);
    llvm::Value *rhs = emitExpr(*e->rhs);
    const std::string &op = e->op;

    // Try user-defined binary operator first
    std::string opFnName = "operator" + op;
    if (auto *result = tryOperatorCall(opFnName, lhs, rhs))
        return result;

    if (op == "==" || op == "!=" || op == "<" ||
        op == "<=" || op == ">"  || op == ">=")
        return emitComparisonOp(op, lhs, rhs);

    if (op == "and" || op == "or")
        return emitLogicalOp(op, lhs, rhs);

    if (op == "&" || op == "|" || op == "^" ||
        op == "<<" || op == ">>" || op == ">>>")
        return emitBitwiseOp(op, lhs, rhs);

    return emitArithmeticOp(op, lhs, rhs);
}

void CodeGen::emitStmt(TypeStmt &s) {
    if (struct_types_.count(s.name))
        throw std::runtime_error("redefined type: " + s.name);

    std::vector<llvm::Type*> fieldTypes;
    for (auto &f : s.fields)
        fieldTypes.push_back(resolveType(f.type));

    llvm::StructType *structTy = llvm::StructType::create(*ctx_, fieldTypes, s.name);
    struct_types_[s.name] = {structTy, s.fields, std::move(s.invariants)};

    if (hasDirective(s.directives, "deprecated"))
        deprecated_types_.insert(s.name);
    for (auto &f : s.fields) {
        if (hasDirective(f.directives, "deprecated"))
            deprecated_fields_.insert(s.name + "." + f.name);
    }
}

llvm::Value *CodeGen::emitStructConstructor(const StructInfo &info,
                                             const std::string &name,
                                             const std::vector<ExprPtr> &args) {
    if (args.size() != info.fields.size())
        throw std::runtime_error("type '" + name + "': expected " +
                                 std::to_string(info.fields.size()) + " arguments, got " +
                                 std::to_string(args.size()));

    llvm::Value *result = llvm::UndefValue::get(info.llvmType);

    for (unsigned i = 0; i < info.fields.size(); ++i) {
        llvm::Value *val = emitExpr(*args[i]);
        llvm::Type *expectedTy = info.llvmType->getElementType(i);
        if (val->getType() != expectedTy)
            throw std::runtime_error("type '" + name + "': field '" + info.fields[i].name +
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
        throw std::runtime_error("field access on non-struct type");

    // Numeric index access for tuples (.0, .1, ...)
    if (!e->field.empty() && std::isdigit(static_cast<unsigned char>(e->field[0]))) {
        unsigned idx = std::stoul(e->field);
        if (idx >= structTy->getNumElements())
            throw std::runtime_error("tuple index " + e->field + " out of range");
        return builder_.CreateExtractValue(obj, idx, "tuple." + e->field);
    }

    std::string typeName = structTy->getName().str();
    auto it = struct_types_.find(typeName);
    if (it == struct_types_.end())
        throw std::runtime_error("unknown struct type: " + typeName);

    const auto &info = it->second;
    for (unsigned i = 0; i < info.fields.size(); ++i) {
        if (info.fields[i].name == e->field) {
            std::string qualifiedField = typeName + "." + e->field;
            if (deprecated_fields_.count(qualifiedField))
                emitDeprecationWarning(qualifiedField);
            return builder_.CreateExtractValue(obj, i, e->field);
        }
    }

    throw std::runtime_error("type '" + typeName + "' has no field '" + e->field + "'");
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
        throw std::runtime_error("empty list literal requires type annotation (not yet supported)");

    // Evaluate all elements
    std::vector<llvm::Value*> vals;
    for (auto &el : e->elements)
        vals.push_back(emitExpr(*el));

    // Check all elements have the same type
    llvm::Type *elemTy = vals[0]->getType();
    for (size_t i = 1; i < vals.size(); ++i) {
        if (vals[i]->getType() != elemTy)
            throw std::runtime_error("list elements must all have the same type");
    }

    int64_t count = static_cast<int64_t>(vals.size());

    // Allocate list header: { i64 length, i64 capacity, ptr data }
    llvm::FunctionType *mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
    llvm::FunctionCallee mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);

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

    return headerPtr;
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<MapExpr> &e) {
    if (e->keys.empty())
        throw std::runtime_error("empty map literal requires type annotation");

    // Evaluate all keys and values
    std::vector<llvm::Value*> keyVals, valVals;
    for (auto &k : e->keys) keyVals.push_back(emitExpr(*k));
    for (auto &v : e->values) valVals.push_back(emitExpr(*v));

    // Check all keys have the same type
    llvm::Type *keyTy = keyVals[0]->getType();
    for (size_t i = 1; i < keyVals.size(); ++i) {
        if (keyVals[i]->getType() != keyTy)
            throw std::runtime_error("map keys must all have the same type");
    }

    // Check all values have the same type
    llvm::Type *valTy = valVals[0]->getType();
    for (size_t i = 1; i < valVals.size(); ++i) {
        if (valVals[i]->getType() != valTy)
            throw std::runtime_error("map values must all have the same type");
    }

    int64_t count = static_cast<int64_t>(keyVals.size());

    llvm::FunctionType *mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
    llvm::FunctionCallee mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
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
        throw std::runtime_error("empty set literal requires type annotation");
    }

    // Evaluate all elements
    std::vector<llvm::Value*> vals;
    for (auto &el : e->elements)
        vals.push_back(emitExpr(*el));

    // Check all elements have the same type
    llvm::Type *elemTy = vals[0]->getType();
    for (size_t i = 1; i < vals.size(); ++i) {
        if (vals[i]->getType() != elemTy)
            throw std::runtime_error("set elements must all have the same type");
    }

    int64_t count = static_cast<int64_t>(vals.size());

    llvm::FunctionType *mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
    llvm::FunctionCallee mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
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
        throw std::runtime_error("undefined enum: " + e.enum_name);
    auto vit = it->second.variants.find(e.variant_name);
    if (vit == it->second.variants.end())
        throw std::runtime_error("enum '" + e.enum_name + "' has no variant '" + e.variant_name + "'");

    if (it->second.isADT) {
        // ADT enum: create struct { tag, zero-payload }
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
        throw std::runtime_error("index operator requires list or map");

    // Check if this is a map
    llvm::Type *mapKeyTy = getMapKeyType(objPtr);
    if (mapKeyTy) {
        llvm::Type *mapValTy = getMapValueType(objPtr);
        if (!mapValTy)
            throw std::runtime_error("cannot determine map value type");

        // Check key type matches
        if (index->getType() != mapKeyTy)
            throw std::runtime_error("map key type mismatch");

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
        throw std::runtime_error("cannot determine list element type for index access");

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

// ===== Contract expression variants =====

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<OldExpr> &e) {
    if (!in_ensure_context_)
        throw std::runtime_error("old() can only be used in ensure clause");
    auto it = old_value_map_.find(e.get());
    if (it == old_value_map_.end())
        throw std::runtime_error("old() value not found (internal error)");
    llvm::AllocaInst *alloca = it->second;
    return builder_.CreateLoad(alloca->getAllocatedType(), alloca, "old_load");
}

llvm::Value *CodeGen::emitExprVariant(const ResultExpr &) {
    if (!in_ensure_context_)
        throw std::runtime_error("result can only be used in ensure clause");
    if (!result_alloca_)
        throw std::runtime_error("result used in void function");
    llvm::Type *ty = result_alloca_->getAllocatedType();
    return builder_.CreateLoad(ty, result_alloca_, "result_load");
}

llvm::Value *CodeGen::valueToString(llvm::Value *val) {
    llvm::Type *ty = val->getType();

    if (ty->isPointerTy()) {
        // Reject non-string pointer types (collections, function pointers)
        if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
            llvm::Value *src = load->getPointerOperand();
            if (list_element_types_.count(src) || map_key_types_.count(src) ||
                set_element_types_.count(src))
                throw std::runtime_error("cannot convert collection to string");
        }
        if (fn_type_info_.count(val))
            throw std::runtime_error("cannot convert function to string");
        return val; // string pointer
    }

    auto mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
    auto mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
    auto snprintfTy = llvm::FunctionType::get(i32Ty_, {ptrTy_, i64Ty_, ptrTy_}, true);
    auto snprintfFn = mod_->getOrInsertFunction("snprintf", snprintfTy);

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
    if (ty == i8Ty_) {
        llvm::Value *buf = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, 32)}, "vts_buf");
        llvm::Constant *fmt = builder_.CreateGlobalString("%d", ".vts_byte_fmt");
        llvm::Value *ext = builder_.CreateZExt(val, i32Ty_, "byte_ext");
        builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 32), fmt, ext});
        return buf;
    }
    // default: int (i64)
    llvm::Value *buf = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, 32)}, "vts_buf");
    llvm::Constant *fmt = builder_.CreateGlobalString("%ld", ".vts_int_fmt");
    builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 32), fmt, val});
    return buf;
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<CastExpr> &e) {
    llvm::Value *val = emitExpr(*e->value);
    llvm::Type *srcTy = val->getType();
    const std::string &target = e->target_type;

    if (target == "float") {
        if (srcTy->isDoubleTy()) return val;
        if (srcTy == i1Ty_) val = builder_.CreateZExt(val, i64Ty_, "bool_ext");
        else if (srcTy == i8Ty_) val = builder_.CreateZExt(val, i64Ty_, "byte_ext");
        if (val->getType()->isIntegerTy())
            return builder_.CreateSIToFP(val, f64Ty_, "cast_f");
        throw std::runtime_error("cannot cast to float");
    }
    if (target == "int") {
        if (srcTy == i64Ty_) return val;
        if (srcTy->isDoubleTy()) return builder_.CreateFPToSI(val, i64Ty_, "cast_i");
        if (srcTy == i1Ty_) return builder_.CreateZExt(val, i64Ty_, "cast_i");
        if (srcTy == i8Ty_) return builder_.CreateZExt(val, i64Ty_, "cast_i");
        throw std::runtime_error("cannot cast to int");
    }
    if (target == "bool") {
        if (srcTy == i1Ty_) return val;
        if (srcTy == i64Ty_) return builder_.CreateICmpNE(val, llvm::ConstantInt::get(i64Ty_, 0), "cast_b");
        if (srcTy == i8Ty_) return builder_.CreateICmpNE(val, llvm::ConstantInt::get(i8Ty_, 0), "cast_b");
        if (srcTy->isDoubleTy()) return builder_.CreateFCmpONE(val, llvm::ConstantFP::get(f64Ty_, 0.0), "cast_b");
        throw std::runtime_error("cannot cast to bool");
    }
    if (target == "str") {
        return valueToString(val);
    }
    if (target == "byte") {
        if (srcTy == i8Ty_) return val;
        if (srcTy == i64Ty_) return builder_.CreateTrunc(val, i8Ty_, "cast_byte");
        if (srcTy == i1Ty_) return builder_.CreateZExt(val, i8Ty_, "cast_byte");
        throw std::runtime_error("cannot cast to byte");
    }
    throw std::runtime_error("unsupported cast target type: " + target);
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
    auto strlenTy = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
    auto strlenFn = mod_->getOrInsertFunction("strlen", strlenTy);
    auto mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
    auto mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
    auto memcpyTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, i64Ty_}, false);
    auto memcpyFn = mod_->getOrInsertFunction("memcpy", memcpyTy);

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
        throw std::runtime_error("ternary expression: both branches must have the same type");

    // Semantic type check for pointer types (str, List, Map, Set are all ptrTy_)
    if (trueVal->getType() == ptrTy_) {
        enum class SemanticKind { Str, List, Map, Set, Other };
        auto classify = [&](llvm::Value *v) -> SemanticKind {
            if (list_element_types_.count(v)) return SemanticKind::List;
            if (map_key_types_.count(v)) return SemanticKind::Map;
            if (set_element_types_.count(v)) return SemanticKind::Set;
            return SemanticKind::Str;
        };
        SemanticKind trueKind = classify(trueVal);
        SemanticKind falseKind = classify(falseVal);
        if (trueKind != falseKind)
            throw std::runtime_error("ternary expression: both branches must have the same type");

        // For List, check element types match
        if (trueKind == SemanticKind::List) {
            llvm::Type *trueElem = list_element_types_[trueVal];
            llvm::Type *falseElem = list_element_types_[falseVal];
            if (trueElem != falseElem)
                throw std::runtime_error("ternary expression: both branches must have the same type");
        }
    }

    builder_.SetInsertPoint(mergeBB);
    llvm::PHINode *phi = builder_.CreatePHI(trueVal->getType(), 2, "ternary");
    phi->addIncoming(trueVal, trueEndBB);
    phi->addIncoming(falseVal, falseEndBB);

    // Propagate semantic type metadata to the PHI result
    if (list_element_types_.count(trueVal))
        list_element_types_[phi] = list_element_types_[trueVal];
    if (map_key_types_.count(trueVal)) {
        map_key_types_[phi] = map_key_types_[trueVal];
        map_value_types_[phi] = map_value_types_[trueVal];
    }
    if (set_element_types_.count(trueVal))
        set_element_types_[phi] = set_element_types_[trueVal];

    return phi;
}

