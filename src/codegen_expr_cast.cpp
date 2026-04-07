#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"


namespace ry {

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<CastExpr> &e) {
    llvm::Value *val = emitExpr(*e->value);
    llvm::Type *srcTy = val->getType();
    const std::string target = e->target_type->toString();

    // Try user-defined operator as (matches by source type + semantic return type name)
    auto *fit = findFunction("operatoras");
    if (fit) {
        std::string resolvedTarget = resolveTypeAlias(target);
        for (auto &entry : *fit) {
            if (entry.paramTypes.size() == 1 &&
                entry.paramTypes[0] == srcTy &&
                resolveTypeAlias(entry.returnTypeName) == resolvedTarget) {
                llvm::Value *result = builder_.CreateCall(entry.func, {val}, "cast_op");
                propagateReturnTypeMeta(&entry, result);
                propagateReturnFnTypeMeta(&entry, entry.func, result);
                return result;
            }
        }
    }

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
                val = builder_.CreateZExt(val, i64Ty_, "u8_ext");
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
    // Low-level type casts — helper lambda for metadata
    // Skip metadata for Constant values to avoid ConstantInt sharing corruption (#311).
    auto withMeta = [&](llvm::Value *result, const std::string &name) -> llvm::Value* {
        if (!llvm::isa<llvm::Constant>(result))
            getOrCreateMeta(result).low_level_type_name = name;
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
    strParts.reserve(e->parts.size() + e->exprs.size());
    for (size_t i = 0; i < e->parts.size(); ++i) {
        if (!e->parts[i].empty())
            strParts.push_back(cachedGlobalString(e->parts[i], ".fstr_lit"));
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

    // Allocate result buffer with ARC header
    llvm::Value *bufSize = builder_.CreateAdd(totalLen, llvm::ConstantInt::get(i64Ty_, 1), "fstr_bufsize");
    auto *arcHdr = emitArcAlloc(bufSize);
    llvm::Value *buf = emitArcGetDataPtr(arcHdr);

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

// ===== TypeAliasStmt =====

void CodeGen::emitStmt(TypeAliasStmt &s) {
    emitTraceSymbolDefine("type_alias", s.name, s.loc);
    // Type aliases are resolved at compile time via resolveType()
    // Store the alias mapping for later lookup
    type_aliases_[s.name] = s.target_type->toString();
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
    uint64_t elemSize = dl.getTypeAllocSize(i64Ty_);

    auto mallocFn = getStdlibMalloc();

    llvm::Value *headerPtr = emitArcAllocCollectionHeader(listHeaderTy_);
    llvm::Value *dataSize = builder_.CreateMul(length, llvm::ConstantInt::get(i64Ty_, elemSize), "data_size");
    llvm::Value *dataPtr = builder_.CreateCall(mallocFn, {dataSize}, "range_data");

    // Store header: len, cap, data
    storeListHeaderFields(headerPtr, length, length, dataPtr);

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
    setTypeMeta(TypeMeta::ListElem, headerPtr, i64Ty_);
    return headerPtr;
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
    llvm::Value *emptyStr = cachedGlobalString("", ".empty_str");
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

} // namespace ry
