#include "ry/codegen.hpp"
#include "ry/diagnostic/diagnostic.hpp"


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
        if (srcTy->isDoubleTy() || srcTy == f32Ty_) return emitCheckedFPToInt(val, i64Ty_, "int", "cast_i");
        if (srcTy == i1Ty_) return builder_.CreateZExt(val, i64Ty_, "cast_i");
        if (srcTy == i8Ty_) {
            std::string name = getLowLevelTypeName(val);
            if (name == "i8") return builder_.CreateSExt(val, i64Ty_, "cast_i");
            return builder_.CreateZExt(val, i64Ty_, "cast_i");
        }
        if (srcTy == i32Ty_ || srcTy == i16Ty_) {
            if (isUnsignedLowLevel(val)) return builder_.CreateZExt(val, i64Ty_, "cast_i");
            return builder_.CreateSExt(val, i64Ty_, "cast_i");
        }
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
        else if (srcTy->isDoubleTy() || srcTy == f32Ty_) r = emitCheckedFPToInt(val, i32Ty_, "i32", "cast_i32");
        else codegenError("cannot cast to i32");
        return withMeta(r, "i32");
    }
    if (target == "i16") {
        llvm::Value *r;
        if (srcTy == i16Ty_) r = val;
        else if (srcTy == i64Ty_ || srcTy == i32Ty_) r = builder_.CreateTrunc(val, i16Ty_, "cast_i16");
        else if (srcTy == i8Ty_) {
            r = isUnsignedLowLevel(val) ? builder_.CreateZExt(val, i16Ty_, "cast_i16")
                                        : builder_.CreateSExt(val, i16Ty_, "cast_i16");
        }
        else if (srcTy == i1Ty_) r = builder_.CreateZExt(val, i16Ty_, "cast_i16");
        else if (srcTy->isDoubleTy() || srcTy == f32Ty_) r = emitCheckedFPToInt(val, i16Ty_, "i16", "cast_i16");
        else codegenError("cannot cast to i16");
        return withMeta(r, "i16");
    }
    if (target == "i8") {
        llvm::Value *r;
        if (srcTy == i8Ty_) r = val;
        else if (srcTy == i64Ty_ || srcTy == i32Ty_ || srcTy == i16Ty_) r = builder_.CreateTrunc(val, i8Ty_, "cast_i8");
        else if (srcTy == i1Ty_) r = builder_.CreateZExt(val, i8Ty_, "cast_i8");
        else if (srcTy->isDoubleTy() || srcTy == f32Ty_) r = emitCheckedFPToInt(val, i8Ty_, "i8", "cast_i8");
        else codegenError("cannot cast to i8");
        return withMeta(r, "i8");
    }
    if (target == "i64") {
        llvm::Value *r;
        if (srcTy == i64Ty_) r = val;
        else if (srcTy == i32Ty_ || srcTy == i16Ty_ || srcTy == i8Ty_) {
            r = isUnsignedLowLevel(val) ? builder_.CreateZExt(val, i64Ty_, "cast_i64")
                                        : builder_.CreateSExt(val, i64Ty_, "cast_i64");
        }
        else if (srcTy == i1Ty_) r = builder_.CreateZExt(val, i64Ty_, "cast_i64");
        else if (srcTy->isDoubleTy() || srcTy == f32Ty_) r = emitCheckedFPToInt(val, i64Ty_, "i64", "cast_i64");
        else codegenError("cannot cast to i64");
        return withMeta(r, "i64");
    }
    if (target == "u8") {
        llvm::Value *r;
        if (srcTy == i8Ty_) r = val;
        else if (srcTy == i64Ty_ || srcTy == i32Ty_ || srcTy == i16Ty_) r = builder_.CreateTrunc(val, i8Ty_, "cast_u8");
        else if (srcTy == i1Ty_) r = builder_.CreateZExt(val, i8Ty_, "cast_u8");
        else if (srcTy->isDoubleTy() || srcTy == f32Ty_) r = emitCheckedFPToInt(val, i8Ty_, "u8", "cast_u8");
        else codegenError("cannot cast to u8");
        return withMeta(r, "u8");
    }
    if (target == "u16") {
        llvm::Value *r;
        if (srcTy == i16Ty_) r = val;
        else if (srcTy == i64Ty_ || srcTy == i32Ty_) r = builder_.CreateTrunc(val, i16Ty_, "cast_u16");
        else if (srcTy == i8Ty_ || srcTy == i1Ty_) r = builder_.CreateZExt(val, i16Ty_, "cast_u16");
        else if (srcTy->isDoubleTy() || srcTy == f32Ty_) r = emitCheckedFPToInt(val, i16Ty_, "u16", "cast_u16");
        else codegenError("cannot cast to u16");
        return withMeta(r, "u16");
    }
    if (target == "u32") {
        llvm::Value *r;
        if (srcTy == i32Ty_) r = val;
        else if (srcTy == i64Ty_) r = builder_.CreateTrunc(val, i32Ty_, "cast_u32");
        else if (srcTy == i16Ty_ || srcTy == i8Ty_ || srcTy == i1Ty_) r = builder_.CreateZExt(val, i32Ty_, "cast_u32");
        else if (srcTy->isDoubleTy() || srcTy == f32Ty_) r = emitCheckedFPToInt(val, i32Ty_, "u32", "cast_u32");
        else codegenError("cannot cast to u32");
        return withMeta(r, "u32");
    }
    if (target == "u64") {
        llvm::Value *r;
        if (srcTy == i64Ty_) r = val;
        else if (srcTy == i32Ty_ || srcTy == i16Ty_ || srcTy == i8Ty_ || srcTy == i1Ty_) r = builder_.CreateZExt(val, i64Ty_, "cast_u64");
        else if (srcTy->isDoubleTy() || srcTy == f32Ty_) r = emitCheckedFPToInt(val, i64Ty_, "u64", "cast_u64");
        else codegenError("cannot cast to u64");
        return withMeta(r, "u64");
    }
    if (target == "f32") {
        if (srcTy == f32Ty_) return val;
        if (srcTy->isDoubleTy()) return builder_.CreateFPTrunc(val, f32Ty_, "cast_f32");
        if (srcTy == i64Ty_ || srcTy == i32Ty_ || srcTy == i16Ty_) {
            if (isUnsignedLowLevel(val)) return builder_.CreateUIToFP(val, f32Ty_, "cast_f32");
            return builder_.CreateSIToFP(val, f32Ty_, "cast_f32");
        }
        if (srcTy == i8Ty_) {
            if (isUnsignedLowLevel(val)) return builder_.CreateUIToFP(val, f32Ty_, "cast_f32");
            return builder_.CreateSIToFP(val, f32Ty_, "cast_f32");
        }
        if (srcTy == i1Ty_) return builder_.CreateUIToFP(val, f32Ty_, "cast_f32");
        codegenError("cannot cast to f32");
    }
    codegenError("unsupported cast target type: " + target);
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<InterpolatedStringExpr> &e) {
    // Build (str_ptr, byte_len) pairs for each non-empty segment.  All parts
    // must be StringHeader-managed handles so emitStringByteLen works correctly.
    std::vector<std::pair<llvm::Value *, llvm::Value *>> parts;
    parts.reserve(e->parts.size() + e->exprs.size());
    for (size_t i = 0; i < e->parts.size(); ++i) {
        if (!e->parts[i].empty()) {
            llvm::Value *litPtr = cachedGlobalString(e->parts[i], ".fstr_lit");
            llvm::Value *litLen = emitStringByteLen(litPtr);
            parts.emplace_back(litPtr, litLen);
        }
        if (i < e->exprs.size()) {
            llvm::Value *exprStr = valueToString(emitExpr(*e->exprs[i]));
            llvm::Value *exprLen = emitStringByteLen(exprStr);
            parts.emplace_back(exprStr, exprLen);
        }
    }
    return concatStringParts(parts, "fstr");
}

// ===== TypeAliasStmt =====

void CodeGen::emitStmt(TypeAliasStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;
    emitTraceSymbolDefine("type_alias", s.name, s.loc);
    validateDirectives(s.directives, DirectiveTarget::TypeAlias);
    if (current_namespace_target_) {
        codegenError("type alias declarations in qualified-imported user-defined modules are not yet supported; use 'from <mod> import <type>' instead");
    }
    if (type_aliases_.count(s.name))
        codegenError("type alias '" + s.name + "' is already defined");
    rejectIfTypeNameTakenByOtherKind(s.name);
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
    llvm::BasicBlock *condBB = createBB("range.cond");
    llvm::BasicBlock *bodyBB = createBB("range.body");
    llvm::BasicBlock *endBB  = createBB("range.end");

    llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "range_i");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
    emitBranchUncond(condBB);

    builder_.SetInsertPoint(condBB);
    llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "i");
    llvm::Value *cond = builder_.CreateICmpSLT(iVal, length, "range_cond");
    emitBranchCond(cond, bodyBB, endBB);

    builder_.SetInsertPoint(bodyBB);
    llvm::Value *curI = builder_.CreateLoad(i64Ty_, iVar, "cur_i");
    llvm::Value *val = builder_.CreateAdd(startVal, curI, "range_val");
    llvm::Value *elemPtr = builder_.CreateGEP(i64Ty_, dataPtr, {curI}, "range_elem_ptr");
    builder_.CreateStore(val, elemPtr);
    llvm::Value *nextI = builder_.CreateAdd(curI, llvm::ConstantInt::get(i64Ty_, 1), "next_i");
    builder_.CreateStore(nextI, iVar);
    emitBranchUncond(condBB);

    builder_.SetInsertPoint(endBB);
    setTypeMeta(TypeMeta::ListElem, headerPtr, i64Ty_);
    return headerPtr;
}

// Common helper: emit IR for string repetition.
// strVal must be a StringHeader handle (ptr), n must be i64.
// #2096: all builder_.Create* routed through ry_emit_* primitives so this
// function carries no inline IR generation (ARC side-effect tables stay C++).
llvm::Value *CodeGen::emitStringRepeat(llvm::Value *strVal, llvm::Value *n) {
    // Pre-register memcpy so its `declare` line lands BEFORE
    // `__ry_string_make_uninit`'s declare — matches the pre-#2096 order (the
    // old code called `getStdlibMemcpy()` before `getRuntimeFn(...)` at the
    // top of the function). emitRuntimeCallDirect uses `getOrInsertFunction`
    // internally so the redundant call only adds the declare; the IR call
    // sites still go through emitRuntimeCallDirect below.
    (void)getStdlibMemcpy();

    // NUL-safe: read byte_len from StringHeader
    llvm::Value *strLen = emitStringByteLen(strVal);

    // If n <= 0, return empty string (a StringHeader global)
    llvm::Value *zero = emitConstInt(i64Ty_, 0);
    llvm::Value *nPos = emitICmpSGT(n, zero, "n_pos");

    llvm::BasicBlock *emptyBB = createBB("str_rep.empty");
    llvm::BasicBlock *repeatBB = createBB("str_rep.repeat");
    llvm::BasicBlock *mergeBB = createBB("str_rep.merge");

    emitBranchCond(nPos, repeatBB, emptyBB);

    // Empty case: heap-allocate so the PHI result is uniformly ARC-managed;
    // cachedGlobalString returns an immortal global that must not be released.
    builder_.SetInsertPoint(emptyBB);
    llvm::Value *emptyStr = emitRuntimeCallDirect("__ry_string_make_uninit", ptrTy_,
                                                  {i64Ty_}, {zero}, "empty_buf");
    emitBranchUncond(mergeBB);

    // Repeat case: guard against strLen * n overflowing int64, then allocate.
    builder_.SetInsertPoint(repeatBB);
    // When strLen == 0, "" * n == "" regardless of n — short-circuit to emptyBB (O(1)).
    // Overflow is only possible when strLen > 0.
    llvm::Value *strLenPos = emitICmpSGT(strLen, zero, "slen_pos");
    llvm::BasicBlock *ovfCheckBB = createBB("str_rep.ovf_check");
    llvm::BasicBlock *allocBB    = createBB("str_rep.alloc");
    emitBranchCond(strLenPos, ovfCheckBB, emptyBB);

    builder_.SetInsertPoint(ovfCheckBB);
    llvm::Value *maxN = emitSDiv(
        emitConstInt(i64Ty_, INT64_MAX), strLen, "max_n");
    llvm::Value *wouldOverflow = emitICmpSGT(n, maxN, "would_overflow");
    llvm::BasicBlock *ovfErrBB = createBB("str_rep.ovf_err");
    emitBranchCond(wouldOverflow, ovfErrBB, allocBB);

    builder_.SetInsertPoint(ovfErrBB);
    emitRuntimeError("runtime error: str * count overflows\n", ".str_rep_overflow");
    // emitRuntimeError ends with CreateUnreachable(); no fall-through.

    // Alloc case: compute totalLen = strLen * n (overflow-free), then allocate.
    builder_.SetInsertPoint(allocBB);
    llvm::Value *totalLen = emitMul(strLen, n, "total_len");
    llvm::Value *buf = emitRuntimeCallDirect("__ry_string_make_uninit", ptrTy_,
                                             {i64Ty_}, {totalLen}, "rep_buf");

    // Loop: copy strVal into buf n times
    llvm::BasicBlock *loopBB = createBB("str_rep.loop");
    llvm::BasicBlock *doneBB = createBB("str_rep.done");

    emitBranchUncond(loopBB);
    builder_.SetInsertPoint(loopBB);

    llvm::PHINode *i = createPhi(i64Ty_, {}, "i");
    i->addIncoming(zero, allocBB);

    llvm::Value *offset = emitMul(i, strLen, "offset");
    llvm::Value *dst = emitGEP(i8Ty_, buf, offset, "dst");
    emitRuntimeCallDirect("memcpy", ptrTy_, {ptrTy_, ptrTy_, i64Ty_},
                          {dst, strVal, strLen}, "");

    llvm::Value *iNext = emitAdd(i, emitConstInt(i64Ty_, 1), "i_next");
    i->addIncoming(iNext, loopBB);
    llvm::Value *cond = emitICmpSLT(iNext, n, "loop_cond");
    emitBranchCond(cond, loopBB, doneBB);

    builder_.SetInsertPoint(doneBB);
    // NUL at buf[totalLen] already written by __ry_string_make_uninit
    emitBranchUncond(mergeBB);

    // Merge
    builder_.SetInsertPoint(mergeBB);
    llvm::PHINode *result = createPhi(ptrTy_, {}, "str_rep_result");
    result->addIncoming(emptyStr, emptyBB);
    result->addIncoming(buf, doneBB);
    arc_str_owned_values_.insert(result);
    return result;
}

} // namespace ry
