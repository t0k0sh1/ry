#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"

// ===== Whitespace helper =====

llvm::Value *CodeGen::emitIsWhitespace(llvm::Value *ch) {
    llvm::Value *isSp  = builder_.CreateICmpEQ(ch, llvm::ConstantInt::get(i8Ty_, ' '));
    llvm::Value *isTab = builder_.CreateICmpEQ(ch, llvm::ConstantInt::get(i8Ty_, '\t'));
    llvm::Value *isNl  = builder_.CreateICmpEQ(ch, llvm::ConstantInt::get(i8Ty_, '\n'));
    llvm::Value *isCr  = builder_.CreateICmpEQ(ch, llvm::ConstantInt::get(i8Ty_, '\r'));
    return builder_.CreateOr(builder_.CreateOr(isSp, isTab),
                             builder_.CreateOr(isNl, isCr));
}

// ===== Builtin String (dispatch table) =====

llvm::Value *CodeGen::emitBuiltinString(const CallExpr &e) {
    using Handler = llvm::Value *(CodeGen::*)(const CallExpr &);
    static const std::unordered_map<std::string, Handler> dispatch = {
        {"contains",     &CodeGen::emitStrOp_contains},
        {"_contains",    &CodeGen::emitStrOp_contains},
        {"starts_with",  &CodeGen::emitStrOp_starts_with},
        {"_starts_with", &CodeGen::emitStrOp_starts_with},
        {"ends_with",    &CodeGen::emitStrOp_ends_with},
        {"_ends_with",   &CodeGen::emitStrOp_ends_with},
        {"find",        &CodeGen::emitStrOp_find},
        {"substring",   &CodeGen::emitStrOp_substring},
        {"char_at",     &CodeGen::emitStrOp_char_at},
        {"replace",     &CodeGen::emitStrOp_replace},
        {"to_upper",    &CodeGen::emitStrOp_to_upper},
        {"to_lower",    &CodeGen::emitStrOp_to_lower},
        {"trim",        &CodeGen::emitStrOp_trim},
        {"trim_start",  &CodeGen::emitStrOp_trim_start},
        {"trim_end",    &CodeGen::emitStrOp_trim_end},
        {"repeat",      &CodeGen::emitStrOp_repeat},
        {"reverse",     &CodeGen::emitStrOp_reverse},
        {"reverse!",    &CodeGen::emitStrOp_reverse_mut},
        {"split",       &CodeGen::emitStrOp_split},
        {"join",        &CodeGen::emitStrOp_join},
    };
    auto it = dispatch.find(e.callee);
    if (it == dispatch.end()) return nullptr;
    return (this->*it->second)(e);
}

// ===== String operation handlers =====

// contains(s, sub[, ignore_case]) → bool
llvm::Value *CodeGen::emitStrOp_contains(const CallExpr &e) {
    if (e.args.size() < 2 || e.args.size() > 3)
        codegenError("contains() takes 2 or 3 arguments");
    llvm::Value *s = emitExpr(*e.args[0]);
    llvm::Value *sub = emitExpr(*e.args[1]);
    llvm::Value *ignoreCase = (e.args.size() == 3)
        ? emitExpr(*e.args[2])
        : llvm::ConstantInt::get(i1Ty_, 0);
    if (s->getType() != ptrTy_ || sub->getType() != ptrTy_)
        codegenError("contains() requires str arguments");

    auto strstrFn = getStdlibStrstr();
    auto strcasestrFn = getStdlibStrcasestr();
    llvm::Value *null = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));

    llvm::BasicBlock *icTrueBB = llvm::BasicBlock::Create(*ctx_, "ct.ic_true", fn_);
    llvm::BasicBlock *icFalseBB = llvm::BasicBlock::Create(*ctx_, "ct.ic_false", fn_);
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "ct.merge", fn_);

    builder_.CreateCondBr(ignoreCase, icTrueBB, icFalseBB);

    builder_.SetInsertPoint(icTrueBB);
    llvm::Value *resIC = builder_.CreateCall(strcasestrFn, {s, sub}, "strcasestr");
    llvm::Value *containsIC = builder_.CreateICmpNE(resIC, null, "contains_ic");
    builder_.CreateBr(mergeBB);

    builder_.SetInsertPoint(icFalseBB);
    llvm::Value *resCS = builder_.CreateCall(strstrFn, {s, sub}, "strstr");
    llvm::Value *containsCS = builder_.CreateICmpNE(resCS, null, "contains_cs");
    builder_.CreateBr(mergeBB);

    builder_.SetInsertPoint(mergeBB);
    llvm::PHINode *phi = builder_.CreatePHI(i1Ty_, 2, "contains");
    phi->addIncoming(containsIC, icTrueBB);
    phi->addIncoming(containsCS, icFalseBB);
    return phi;
}

// starts_with(s, prefix[, ignore_case]) → bool
llvm::Value *CodeGen::emitStrOp_starts_with(const CallExpr &e) {
    if (e.args.size() < 2 || e.args.size() > 3)
        codegenError("starts_with() takes 2 or 3 arguments");
    llvm::Value *s = emitExpr(*e.args[0]);
    llvm::Value *prefix = emitExpr(*e.args[1]);
    llvm::Value *ignoreCase = (e.args.size() == 3)
        ? emitExpr(*e.args[2])
        : llvm::ConstantInt::get(i1Ty_, 0);
    if (s->getType() != ptrTy_ || prefix->getType() != ptrTy_)
        codegenError("starts_with() requires str arguments");
    auto strlenFn = getStdlibStrlen();
    auto strncmpFn = getStdlibStrncmp();
    auto strncasecmpFn = getStdlibStrncasecmp();
    llvm::Value *prefixLen = builder_.CreateCall(strlenFn, {prefix}, "prefix_len");

    llvm::BasicBlock *icTrueBB = llvm::BasicBlock::Create(*ctx_, "sw.ic_true", fn_);
    llvm::BasicBlock *icFalseBB = llvm::BasicBlock::Create(*ctx_, "sw.ic_false", fn_);
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "sw.merge", fn_);

    builder_.CreateCondBr(ignoreCase, icTrueBB, icFalseBB);

    builder_.SetInsertPoint(icTrueBB);
    llvm::Value *cmpIC = builder_.CreateCall(strncasecmpFn, {s, prefix, prefixLen}, "strncasecmp");
    llvm::Value *matchIC = builder_.CreateICmpEQ(cmpIC, llvm::ConstantInt::get(i32Ty_, 0), "sw_ic");
    builder_.CreateBr(mergeBB);

    builder_.SetInsertPoint(icFalseBB);
    llvm::Value *cmpCS = builder_.CreateCall(strncmpFn, {s, prefix, prefixLen}, "strncmp");
    llvm::Value *matchCS = builder_.CreateICmpEQ(cmpCS, llvm::ConstantInt::get(i32Ty_, 0), "sw_cs");
    builder_.CreateBr(mergeBB);

    builder_.SetInsertPoint(mergeBB);
    llvm::PHINode *phi = builder_.CreatePHI(i1Ty_, 2, "starts_with");
    phi->addIncoming(matchIC, icTrueBB);
    phi->addIncoming(matchCS, icFalseBB);
    return phi;
}

// ends_with(s, suffix[, ignore_case]) → bool
llvm::Value *CodeGen::emitStrOp_ends_with(const CallExpr &e) {
    if (e.args.size() < 2 || e.args.size() > 3)
        codegenError("ends_with() takes 2 or 3 arguments");
    llvm::Value *s = emitExpr(*e.args[0]);
    llvm::Value *suffix = emitExpr(*e.args[1]);
    llvm::Value *ignoreCase = (e.args.size() == 3)
        ? emitExpr(*e.args[2])
        : llvm::ConstantInt::get(i1Ty_, 0);
    if (s->getType() != ptrTy_ || suffix->getType() != ptrTy_)
        codegenError("ends_with() requires str arguments");
    auto strlenFn = getStdlibStrlen();
    auto strncmpFn = getStdlibStrncmp();
    auto strncasecmpFn = getStdlibStrncasecmp();
    llvm::Value *sLen = builder_.CreateCall(strlenFn, {s}, "s_len");
    llvm::Value *suffixLen = builder_.CreateCall(strlenFn, {suffix}, "suffix_len");

    llvm::Value *tooLong = builder_.CreateICmpUGT(suffixLen, sLen, "too_long");

    llvm::BasicBlock *checkBB = llvm::BasicBlock::Create(*ctx_, "ew.check", fn_);
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "ew.merge", fn_);
    llvm::BasicBlock *curBB = builder_.GetInsertBlock();

    builder_.CreateCondBr(tooLong, mergeBB, checkBB);

    builder_.SetInsertPoint(checkBB);
    llvm::Value *offset = builder_.CreateSub(sLen, suffixLen, "offset");
    llvm::Value *tailPtr = builder_.CreateGEP(builder_.getInt8Ty(), s, offset, "tail_ptr");

    // Branch on ignore_case
    llvm::BasicBlock *icTrueBB = llvm::BasicBlock::Create(*ctx_, "ew.ic_true", fn_);
    llvm::BasicBlock *icFalseBB = llvm::BasicBlock::Create(*ctx_, "ew.ic_false", fn_);
    llvm::BasicBlock *cmpMergeBB = llvm::BasicBlock::Create(*ctx_, "ew.cmp_merge", fn_);

    builder_.CreateCondBr(ignoreCase, icTrueBB, icFalseBB);

    builder_.SetInsertPoint(icTrueBB);
    llvm::Value *cmpIC = builder_.CreateCall(strncasecmpFn, {tailPtr, suffix, suffixLen}, "strncasecmp");
    llvm::Value *matchIC = builder_.CreateICmpEQ(cmpIC, llvm::ConstantInt::get(i32Ty_, 0), "ew_ic");
    builder_.CreateBr(cmpMergeBB);

    builder_.SetInsertPoint(icFalseBB);
    llvm::Value *cmpCS = builder_.CreateCall(strncmpFn, {tailPtr, suffix, suffixLen}, "strncmp");
    llvm::Value *matchCS = builder_.CreateICmpEQ(cmpCS, llvm::ConstantInt::get(i32Ty_, 0), "ew_cs");
    builder_.CreateBr(cmpMergeBB);

    builder_.SetInsertPoint(cmpMergeBB);
    llvm::PHINode *matchPhi = builder_.CreatePHI(i1Ty_, 2, "ew_match");
    matchPhi->addIncoming(matchIC, icTrueBB);
    matchPhi->addIncoming(matchCS, icFalseBB);
    builder_.CreateBr(mergeBB);

    builder_.SetInsertPoint(mergeBB);
    llvm::PHINode *phi = builder_.CreatePHI(i1Ty_, 2, "ends_with");
    phi->addIncoming(llvm::ConstantInt::get(i1Ty_, 0), curBB);
    phi->addIncoming(matchPhi, cmpMergeBB);
    return phi;
}

// find(s, sub) → Option<int>
llvm::Value *CodeGen::emitStrOp_find(const CallExpr &e) {
    requireArgs(e, 2);
    llvm::Value *s = emitExpr(*e.args[0]);
    llvm::Value *sub = emitExpr(*e.args[1]);
    if (s->getType() != ptrTy_ || sub->getType() != ptrTy_)
        codegenError("find() requires str arguments");

    llvm::StructType *optTy = getOptionType(i64Ty_);
    auto strstrFn = getStdlibStrstr();
    llvm::Value *result = builder_.CreateCall(strstrFn, {s, sub}, "find_ptr");
    llvm::Value *null = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));
    llvm::Value *isNull = builder_.CreateICmpEQ(result, null, "find_null");

    llvm::BasicBlock *foundBB = llvm::BasicBlock::Create(*ctx_, "find.found", fn_);
    llvm::BasicBlock *notFoundBB = llvm::BasicBlock::Create(*ctx_, "find.notfound", fn_);
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "find.merge", fn_);
    builder_.CreateCondBr(isNull, notFoundBB, foundBB);

    builder_.SetInsertPoint(foundBB);
    llvm::Value *sInt = builder_.CreatePtrToInt(s, i64Ty_, "s_int");
    llvm::Value *rInt = builder_.CreatePtrToInt(result, i64Ty_, "r_int");
    llvm::Value *byteOffset = builder_.CreateSub(rInt, sInt, "find_byte_offset");
    // Convert byte offset to character index
    auto charIdxFn = getRuntimeFn("__ry_utf8_char_index", i64Ty_, {ptrTy_, i64Ty_});
    llvm::Value *charIdx = builder_.CreateCall(charIdxFn, {s, byteOffset}, "find_char_idx");
    llvm::Value *someVal = buildSomeValue(charIdx, optTy);
    builder_.CreateBr(mergeBB);
    llvm::BasicBlock *foundEndBB = builder_.GetInsertBlock();

    builder_.SetInsertPoint(notFoundBB);
    llvm::Value *noneVal = buildNoneValue(optTy);
    builder_.CreateBr(mergeBB);
    llvm::BasicBlock *notFoundEndBB = builder_.GetInsertBlock();

    builder_.SetInsertPoint(mergeBB);
    llvm::PHINode *phi = builder_.CreatePHI(optTy, 2, "find_result");
    phi->addIncoming(someVal, foundEndBB);
    phi->addIncoming(noneVal, notFoundEndBB);
    return phi;
}

// substring(s, start, end) → str (UTF-8 character indices, clamped)
llvm::Value *CodeGen::emitStrOp_substring(const CallExpr &e) {
    requireArgs(e, 3);
    llvm::Value *s = emitExpr(*e.args[0]);
    llvm::Value *start = emitExpr(*e.args[1]);
    llvm::Value *end = emitExpr(*e.args[2]);
    if (s->getType() != ptrTy_)
        codegenError("substring() requires str as first argument");

    // Fast path: both indices are compile-time constants satisfying sv >= 0, ev >= 0, ev >= sv.
    // All three clamping selects are provably no-ops, so skip them and call the runtime directly.
    if (auto *ciStart = llvm::dyn_cast<llvm::ConstantInt>(start)) {
        if (auto *ciEnd = llvm::dyn_cast<llvm::ConstantInt>(end)) {
            int64_t sv = ciStart->getSExtValue();
            int64_t ev = ciEnd->getSExtValue();
            if (sv >= 0 && ev >= 0 && ev >= sv) {
                auto substrFn = getRuntimeFn("__ry_utf8_substring", ptrTy_,
                                             {ptrTy_, i64Ty_, i64Ty_});
                return builder_.CreateCall(substrFn, {s, start, end}, "substring");
            }
        }
    }

    // Clamp start and end to be non-negative; let the runtime clamp to string length.
    llvm::Value *zero = llvm::ConstantInt::get(i64Ty_, 0);

    llvm::Value *clampedStart = builder_.CreateSelect(
        builder_.CreateICmpSLT(start, zero), zero, start, "substr_cstart");

    llvm::Value *clampedEnd = builder_.CreateSelect(
        builder_.CreateICmpSLT(end, zero), zero, end, "substr_cend");

    // Ensure end >= start
    clampedEnd = builder_.CreateSelect(
        builder_.CreateICmpSLT(clampedEnd, clampedStart), clampedStart, clampedEnd, "substr_cend2");

    auto substrFn = getRuntimeFn("__ry_utf8_substring", ptrTy_, {ptrTy_, i64Ty_, i64Ty_});
    return builder_.CreateCall(substrFn, {s, clampedStart, clampedEnd}, "substring");
}

// char_at(s, i) → str (single UTF-8 character as string)
llvm::Value *CodeGen::emitStrOp_char_at(const CallExpr &e) {
    requireArgs(e, 2);
    llvm::Value *s = emitExpr(*e.args[0]);
    llvm::Value *idx = emitExpr(*e.args[1]);
    if (s->getType() != ptrTy_)
        codegenError("char_at() requires str as first argument");

    if (idx->getType()->isIntegerTy(1))
        idx = builder_.CreateZExt(idx, i64Ty_, "char_at_idx");

    auto fn = getRuntimeFn("__ry_utf8_char_at_checked", ptrTy_, {ptrTy_, i64Ty_});
    return builder_.CreateCall(fn, {s, idx}, "char_at");
}

// replace(s, old, new) → str
llvm::Value *CodeGen::emitStrOp_replace(const CallExpr &e) {
    requireArgs(e, 3);
    llvm::Value *s = emitExpr(*e.args[0]);
    llvm::Value *oldStr = emitExpr(*e.args[1]);
    llvm::Value *newStr = emitExpr(*e.args[2]);
    // Regex overload: replace(text, /pattern/, replacement) → delegate to regex runtime
    if (isRegex(oldStr) && isStringValue(s)) {
        auto fn = mod_->getOrInsertFunction("__ry_regex_replace", fnTy_ptr_ptr_ptr_to_ptr_);
        return builder_.CreateCall(fn, {oldStr, s, newStr}, "regex_replace");
    }
    if (s->getType() != ptrTy_ || oldStr->getType() != ptrTy_ || newStr->getType() != ptrTy_)
        codegenError("replace() requires str arguments");
    auto strlenFn = getStdlibStrlen();
    auto strstrFn = getStdlibStrstr();
    auto mallocFn = getStdlibMalloc();
    auto memcpyFn = getStdlibMemcpy();

    llvm::Value *sLen = builder_.CreateCall(strlenFn, {s}, "repl_s_len");
    llvm::Value *oldLen = builder_.CreateCall(strlenFn, {oldStr}, "repl_old_len");
    llvm::Value *newLen = builder_.CreateCall(strlenFn, {newStr}, "repl_new_len");

    // Pass 1: count occurrences
    llvm::AllocaInst *countVar = builder_.CreateAlloca(i64Ty_, nullptr, "repl_count");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), countVar);
    llvm::AllocaInst *searchVar = builder_.CreateAlloca(ptrTy_, nullptr, "repl_search");
    builder_.CreateStore(s, searchVar);

    llvm::BasicBlock *countCondBB = llvm::BasicBlock::Create(*ctx_, "repl.count_cond", fn_);
    llvm::BasicBlock *countBodyBB = llvm::BasicBlock::Create(*ctx_, "repl.count_body", fn_);
    llvm::BasicBlock *countEndBB = llvm::BasicBlock::Create(*ctx_, "repl.count_end", fn_);

    builder_.CreateBr(countCondBB);
    builder_.SetInsertPoint(countCondBB);
    llvm::Value *searchPtr = builder_.CreateLoad(ptrTy_, searchVar, "search_ptr");
    llvm::Value *found = builder_.CreateCall(strstrFn, {searchPtr, oldStr}, "found_ptr");
    llvm::Value *null = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));
    llvm::Value *notNull = builder_.CreateICmpNE(found, null, "found_not_null");
    builder_.CreateCondBr(notNull, countBodyBB, countEndBB);

    builder_.SetInsertPoint(countBodyBB);
    llvm::Value *cnt = builder_.CreateLoad(i64Ty_, countVar, "cnt");
    builder_.CreateStore(builder_.CreateAdd(cnt, llvm::ConstantInt::get(i64Ty_, 1), "cnt_inc"), countVar);
    llvm::Value *nextSearch = builder_.CreateGEP(builder_.getInt8Ty(), found, oldLen, "next_search");
    builder_.CreateStore(nextSearch, searchVar);
    builder_.CreateBr(countCondBB);

    builder_.SetInsertPoint(countEndBB);
    llvm::Value *count = builder_.CreateLoad(i64Ty_, countVar, "final_count");

    // Calculate new length: sLen + count * (newLen - oldLen) + 1
    llvm::Value *diff = builder_.CreateSub(newLen, oldLen, "len_diff");
    llvm::Value *totalDiff = builder_.CreateMul(count, diff, "total_diff");
    llvm::Value *resultLen = builder_.CreateAdd(sLen, totalDiff, "result_len");
    llvm::Value *bufSize = builder_.CreateAdd(resultLen, llvm::ConstantInt::get(i64Ty_, 1), "buf_size");
    llvm::Value *buf = builder_.CreateCall(mallocFn, {bufSize}, "repl_buf");

    // Pass 2: build result
    llvm::AllocaInst *srcVar = builder_.CreateAlloca(ptrTy_, nullptr, "repl_src");
    builder_.CreateStore(s, srcVar);
    llvm::AllocaInst *dstVar = builder_.CreateAlloca(ptrTy_, nullptr, "repl_dst");
    builder_.CreateStore(buf, dstVar);

    llvm::BasicBlock *buildCondBB = llvm::BasicBlock::Create(*ctx_, "repl.build_cond", fn_);
    llvm::BasicBlock *buildBodyBB = llvm::BasicBlock::Create(*ctx_, "repl.build_body", fn_);
    llvm::BasicBlock *buildEndBB = llvm::BasicBlock::Create(*ctx_, "repl.build_end", fn_);

    builder_.CreateBr(buildCondBB);
    builder_.SetInsertPoint(buildCondBB);
    llvm::Value *curSrc = builder_.CreateLoad(ptrTy_, srcVar, "cur_src");
    llvm::Value *foundBuild = builder_.CreateCall(strstrFn, {curSrc, oldStr}, "found_build");
    llvm::Value *notNullBuild = builder_.CreateICmpNE(foundBuild, null, "found_build_nn");
    builder_.CreateCondBr(notNullBuild, buildBodyBB, buildEndBB);

    builder_.SetInsertPoint(buildBodyBB);
    llvm::Value *curDst = builder_.CreateLoad(ptrTy_, dstVar, "cur_dst");
    llvm::Value *srcInt = builder_.CreatePtrToInt(curSrc, i64Ty_, "src_int");
    llvm::Value *foundInt = builder_.CreatePtrToInt(foundBuild, i64Ty_, "found_int");
    llvm::Value *prefixLen = builder_.CreateSub(foundInt, srcInt, "prefix_len");
    builder_.CreateCall(memcpyFn, {curDst, curSrc, prefixLen});
    llvm::Value *dstAfterPrefix = builder_.CreateGEP(builder_.getInt8Ty(), curDst, prefixLen, "dst_after_prefix");
    builder_.CreateCall(memcpyFn, {dstAfterPrefix, newStr, newLen});
    llvm::Value *dstAfterNew = builder_.CreateGEP(builder_.getInt8Ty(), dstAfterPrefix, newLen, "dst_after_new");
    builder_.CreateStore(dstAfterNew, dstVar);
    llvm::Value *srcAfterOld = builder_.CreateGEP(builder_.getInt8Ty(), foundBuild, oldLen, "src_after_old");
    builder_.CreateStore(srcAfterOld, srcVar);
    builder_.CreateBr(buildCondBB);

    builder_.SetInsertPoint(buildEndBB);
    llvm::Value *finalSrc = builder_.CreateLoad(ptrTy_, srcVar, "final_src");
    llvm::Value *finalDst = builder_.CreateLoad(ptrTy_, dstVar, "final_dst");
    llvm::Value *remainLen = builder_.CreateCall(strlenFn, {finalSrc}, "remain_len");
    llvm::Value *remainPlusNull = builder_.CreateAdd(remainLen, llvm::ConstantInt::get(i64Ty_, 1), "remain_plus_null");
    builder_.CreateCall(memcpyFn, {finalDst, finalSrc, remainPlusNull});

    return buf;
}

// to_upper(s) → str
llvm::Value *CodeGen::emitStrOp_to_upper(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *s = emitExpr(*e.args[0]);
    if (s->getType() != ptrTy_)
        codegenError("to_upper() requires str argument");
    auto strlenFn = getStdlibStrlen();
    auto mallocFn = getStdlibMalloc();

    llvm::Value *len = builder_.CreateCall(strlenFn, {s}, "upper_len");
    llvm::Value *bufSize = builder_.CreateAdd(len, llvm::ConstantInt::get(i64Ty_, 1), "upper_buf_size");
    llvm::Value *buf = builder_.CreateCall(mallocFn, {bufSize}, "upper_buf");

    llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "upper_i");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "upper.cond", fn_);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "upper.body", fn_);
    llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "upper.end", fn_);

    builder_.CreateBr(condBB);
    builder_.SetInsertPoint(condBB);
    llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "upper_idx");
    builder_.CreateCondBr(builder_.CreateICmpSLT(i, len, "upper_cond"), bodyBB, endBB);

    builder_.SetInsertPoint(bodyBB);
    llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "upper_i_cur");
    llvm::Value *srcPtr = builder_.CreateGEP(builder_.getInt8Ty(), s, iCur, "upper_src");
    llvm::Value *ch = builder_.CreateLoad(i8Ty_, srcPtr, "upper_ch");
    llvm::Value *isLowerA = builder_.CreateICmpUGE(ch, llvm::ConstantInt::get(i8Ty_, 'a'), "is_lower_a");
    llvm::Value *isLowerZ = builder_.CreateICmpULE(ch, llvm::ConstantInt::get(i8Ty_, 'z'), "is_lower_z");
    llvm::Value *isLower = builder_.CreateAnd(isLowerA, isLowerZ, "is_lower");
    llvm::Value *upper = builder_.CreateSub(ch, llvm::ConstantInt::get(i8Ty_, 32), "upper_ch_val");
    llvm::Value *result = builder_.CreateSelect(isLower, upper, ch, "upper_result");
    llvm::Value *dstPtr = builder_.CreateGEP(builder_.getInt8Ty(), buf, iCur, "upper_dst");
    builder_.CreateStore(result, dstPtr);
    builder_.CreateStore(builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1), "upper_next"), iVar);
    builder_.CreateBr(condBB);

    builder_.SetInsertPoint(endBB);
    llvm::Value *nullPtr = builder_.CreateGEP(builder_.getInt8Ty(), buf, len, "upper_null");
    builder_.CreateStore(llvm::ConstantInt::get(i8Ty_, 0), nullPtr);
    return buf;
}

// to_lower(s) → str
llvm::Value *CodeGen::emitStrOp_to_lower(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *s = emitExpr(*e.args[0]);
    if (s->getType() != ptrTy_)
        codegenError("to_lower() requires str argument");
    auto strlenFn = getStdlibStrlen();
    auto mallocFn = getStdlibMalloc();

    llvm::Value *len = builder_.CreateCall(strlenFn, {s}, "lower_len");
    llvm::Value *bufSize = builder_.CreateAdd(len, llvm::ConstantInt::get(i64Ty_, 1), "lower_buf_size");
    llvm::Value *buf = builder_.CreateCall(mallocFn, {bufSize}, "lower_buf");

    llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "lower_i");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "lower.cond", fn_);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "lower.body", fn_);
    llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "lower.end", fn_);

    builder_.CreateBr(condBB);
    builder_.SetInsertPoint(condBB);
    llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "lower_idx");
    builder_.CreateCondBr(builder_.CreateICmpSLT(i, len, "lower_cond"), bodyBB, endBB);

    builder_.SetInsertPoint(bodyBB);
    llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "lower_i_cur");
    llvm::Value *srcPtr = builder_.CreateGEP(builder_.getInt8Ty(), s, iCur, "lower_src");
    llvm::Value *ch = builder_.CreateLoad(i8Ty_, srcPtr, "lower_ch");
    llvm::Value *isUpperA = builder_.CreateICmpUGE(ch, llvm::ConstantInt::get(i8Ty_, 'A'), "is_upper_a");
    llvm::Value *isUpperZ = builder_.CreateICmpULE(ch, llvm::ConstantInt::get(i8Ty_, 'Z'), "is_upper_z");
    llvm::Value *isUpper = builder_.CreateAnd(isUpperA, isUpperZ, "is_upper");
    llvm::Value *lower = builder_.CreateAdd(ch, llvm::ConstantInt::get(i8Ty_, 32), "lower_ch_val");
    llvm::Value *result = builder_.CreateSelect(isUpper, lower, ch, "lower_result");
    llvm::Value *dstPtr = builder_.CreateGEP(builder_.getInt8Ty(), buf, iCur, "lower_dst");
    builder_.CreateStore(result, dstPtr);
    builder_.CreateStore(builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1), "lower_next"), iVar);
    builder_.CreateBr(condBB);

    builder_.SetInsertPoint(endBB);
    llvm::Value *nullPtr = builder_.CreateGEP(builder_.getInt8Ty(), buf, len, "lower_null");
    builder_.CreateStore(llvm::ConstantInt::get(i8Ty_, 0), nullPtr);
    return buf;
}

// trim(s) → str
llvm::Value *CodeGen::emitStrOp_trim(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *s = emitExpr(*e.args[0]);
    if (s->getType() != ptrTy_)
        codegenError("trim() requires str argument");
    auto strlenFn = getStdlibStrlen();
    auto mallocFn = getStdlibMalloc();
    auto memcpyFn = getStdlibMemcpy();

    llvm::Value *len = builder_.CreateCall(strlenFn, {s}, "trim_len");

    llvm::AllocaInst *startVar = builder_.CreateAlloca(i64Ty_, nullptr, "trim_start");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), startVar);

    llvm::BasicBlock *startCondBB = llvm::BasicBlock::Create(*ctx_, "trim.start_cond", fn_);
    llvm::BasicBlock *startBodyBB = llvm::BasicBlock::Create(*ctx_, "trim.start_body", fn_);
    llvm::BasicBlock *startEndBB = llvm::BasicBlock::Create(*ctx_, "trim.start_end", fn_);

    builder_.CreateBr(startCondBB);
    builder_.SetInsertPoint(startCondBB);
    llvm::Value *startIdx = builder_.CreateLoad(i64Ty_, startVar, "start_idx");
    llvm::Value *startInBounds = builder_.CreateICmpSLT(startIdx, len, "start_in_bounds");

    llvm::BasicBlock *startCheckBB = llvm::BasicBlock::Create(*ctx_, "trim.start_check", fn_);
    builder_.CreateCondBr(startInBounds, startCheckBB, startEndBB);

    builder_.SetInsertPoint(startCheckBB);
    llvm::Value *startPtr = builder_.CreateGEP(builder_.getInt8Ty(), s, startIdx, "start_ptr");
    llvm::Value *startCh = builder_.CreateLoad(i8Ty_, startPtr, "start_ch");
    llvm::Value *isWs = emitIsWhitespace(startCh);
    builder_.CreateCondBr(isWs, startBodyBB, startEndBB);

    builder_.SetInsertPoint(startBodyBB);
    llvm::Value *startNext = builder_.CreateAdd(startIdx, llvm::ConstantInt::get(i64Ty_, 1), "start_next");
    builder_.CreateStore(startNext, startVar);
    builder_.CreateBr(startCondBB);

    builder_.SetInsertPoint(startEndBB);
    llvm::Value *finalStart = builder_.CreateLoad(i64Ty_, startVar, "final_start");

    llvm::AllocaInst *endVar = builder_.CreateAlloca(i64Ty_, nullptr, "trim_end");
    builder_.CreateStore(len, endVar);

    llvm::BasicBlock *endCondBB = llvm::BasicBlock::Create(*ctx_, "trim.end_cond", fn_);
    llvm::BasicBlock *endBodyBB = llvm::BasicBlock::Create(*ctx_, "trim.end_body", fn_);
    llvm::BasicBlock *endEndBB = llvm::BasicBlock::Create(*ctx_, "trim.end_end", fn_);

    builder_.CreateBr(endCondBB);
    builder_.SetInsertPoint(endCondBB);
    llvm::Value *endIdx = builder_.CreateLoad(i64Ty_, endVar, "end_idx");
    llvm::Value *endGtStart = builder_.CreateICmpSGT(endIdx, finalStart, "end_gt_start");

    llvm::BasicBlock *endCheckBB = llvm::BasicBlock::Create(*ctx_, "trim.end_check", fn_);
    builder_.CreateCondBr(endGtStart, endCheckBB, endEndBB);

    builder_.SetInsertPoint(endCheckBB);
    llvm::Value *endPrev = builder_.CreateSub(endIdx, llvm::ConstantInt::get(i64Ty_, 1), "end_prev");
    llvm::Value *endPtr = builder_.CreateGEP(builder_.getInt8Ty(), s, endPrev, "end_ptr");
    llvm::Value *endCh = builder_.CreateLoad(i8Ty_, endPtr, "end_ch");
    llvm::Value *isWs2 = emitIsWhitespace(endCh);
    builder_.CreateCondBr(isWs2, endBodyBB, endEndBB);

    builder_.SetInsertPoint(endBodyBB);
    builder_.CreateStore(endPrev, endVar);
    builder_.CreateBr(endCondBB);

    builder_.SetInsertPoint(endEndBB);
    llvm::Value *finalEnd = builder_.CreateLoad(i64Ty_, endVar, "final_end");

    llvm::Value *resultLen = builder_.CreateSub(finalEnd, finalStart, "trim_result_len");
    llvm::Value *zero = llvm::ConstantInt::get(i64Ty_, 0);
    llvm::Value *isNeg = builder_.CreateICmpSLT(resultLen, zero, "trim_neg");
    llvm::Value *safeLen = builder_.CreateSelect(isNeg, zero, resultLen, "trim_safe_len");
    llvm::Value *bufSize = builder_.CreateAdd(safeLen, llvm::ConstantInt::get(i64Ty_, 1), "trim_buf_size");
    llvm::Value *buf = builder_.CreateCall(mallocFn, {bufSize}, "trim_buf");
    llvm::Value *srcPtr = builder_.CreateGEP(builder_.getInt8Ty(), s, finalStart, "trim_src");
    builder_.CreateCall(memcpyFn, {buf, srcPtr, safeLen});
    llvm::Value *nullEnd = builder_.CreateGEP(builder_.getInt8Ty(), buf, safeLen, "trim_null");
    builder_.CreateStore(llvm::ConstantInt::get(i8Ty_, 0), nullEnd);
    return buf;
}

// trim_start(s) → str
llvm::Value *CodeGen::emitStrOp_trim_start(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *s = emitExpr(*e.args[0]);
    if (s->getType() != ptrTy_)
        codegenError("trim_start() requires str argument");
    auto strlenFn = getStdlibStrlen();
    auto mallocFn = getStdlibMalloc();
    auto memcpyFn = getStdlibMemcpy();

    llvm::Value *len = builder_.CreateCall(strlenFn, {s}, "tstart_len");

    llvm::AllocaInst *startVar = builder_.CreateAlloca(i64Ty_, nullptr, "tstart_start");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), startVar);

    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "tstart.cond", fn_);
    llvm::BasicBlock *checkBB = llvm::BasicBlock::Create(*ctx_, "tstart.check", fn_);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "tstart.body", fn_);
    llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "tstart.end", fn_);

    builder_.CreateBr(condBB);
    builder_.SetInsertPoint(condBB);
    llvm::Value *idx = builder_.CreateLoad(i64Ty_, startVar, "tstart_idx");
    builder_.CreateCondBr(builder_.CreateICmpSLT(idx, len, "tstart_bound"), checkBB, endBB);

    builder_.SetInsertPoint(checkBB);
    llvm::Value *ptr = builder_.CreateGEP(builder_.getInt8Ty(), s, idx, "tstart_ptr");
    llvm::Value *ch = builder_.CreateLoad(i8Ty_, ptr, "tstart_ch");
    llvm::Value *isWs = emitIsWhitespace(ch);
    builder_.CreateCondBr(isWs, bodyBB, endBB);

    builder_.SetInsertPoint(bodyBB);
    builder_.CreateStore(builder_.CreateAdd(idx, llvm::ConstantInt::get(i64Ty_, 1), "tstart_next"), startVar);
    builder_.CreateBr(condBB);

    builder_.SetInsertPoint(endBB);
    llvm::Value *finalStart = builder_.CreateLoad(i64Ty_, startVar, "tstart_final");
    llvm::Value *resultLen = builder_.CreateSub(len, finalStart, "tstart_rlen");
    llvm::Value *bufSize = builder_.CreateAdd(resultLen, llvm::ConstantInt::get(i64Ty_, 1), "tstart_bsize");
    llvm::Value *buf = builder_.CreateCall(mallocFn, {bufSize}, "tstart_buf");
    llvm::Value *srcPtr = builder_.CreateGEP(builder_.getInt8Ty(), s, finalStart, "tstart_src");
    builder_.CreateCall(memcpyFn, {buf, srcPtr, resultLen});
    llvm::Value *nullEnd = builder_.CreateGEP(builder_.getInt8Ty(), buf, resultLen, "tstart_null");
    builder_.CreateStore(llvm::ConstantInt::get(i8Ty_, 0), nullEnd);
    return buf;
}

// trim_end(s) → str
llvm::Value *CodeGen::emitStrOp_trim_end(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *s = emitExpr(*e.args[0]);
    if (s->getType() != ptrTy_)
        codegenError("trim_end() requires str argument");
    auto strlenFn = getStdlibStrlen();
    auto mallocFn = getStdlibMalloc();
    auto memcpyFn = getStdlibMemcpy();

    llvm::Value *len = builder_.CreateCall(strlenFn, {s}, "tend_len");

    llvm::AllocaInst *endVar = builder_.CreateAlloca(i64Ty_, nullptr, "tend_end");
    builder_.CreateStore(len, endVar);

    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "tend.cond", fn_);
    llvm::BasicBlock *checkBB = llvm::BasicBlock::Create(*ctx_, "tend.check", fn_);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "tend.body", fn_);
    llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "tend.end", fn_);

    builder_.CreateBr(condBB);
    builder_.SetInsertPoint(condBB);
    llvm::Value *endIdx = builder_.CreateLoad(i64Ty_, endVar, "tend_idx");
    llvm::Value *gtZero = builder_.CreateICmpSGT(endIdx, llvm::ConstantInt::get(i64Ty_, 0), "tend_gt0");
    builder_.CreateCondBr(gtZero, checkBB, endBB);

    builder_.SetInsertPoint(checkBB);
    llvm::Value *prevIdx = builder_.CreateSub(endIdx, llvm::ConstantInt::get(i64Ty_, 1), "tend_prev");
    llvm::Value *ptr = builder_.CreateGEP(builder_.getInt8Ty(), s, prevIdx, "tend_ptr");
    llvm::Value *ch = builder_.CreateLoad(i8Ty_, ptr, "tend_ch");
    llvm::Value *isWs = emitIsWhitespace(ch);
    builder_.CreateCondBr(isWs, bodyBB, endBB);

    builder_.SetInsertPoint(bodyBB);
    builder_.CreateStore(prevIdx, endVar);
    builder_.CreateBr(condBB);

    builder_.SetInsertPoint(endBB);
    llvm::Value *finalEnd = builder_.CreateLoad(i64Ty_, endVar, "tend_final");
    llvm::Value *bufSize = builder_.CreateAdd(finalEnd, llvm::ConstantInt::get(i64Ty_, 1), "tend_bsize");
    llvm::Value *buf = builder_.CreateCall(mallocFn, {bufSize}, "tend_buf");
    builder_.CreateCall(memcpyFn, {buf, s, finalEnd});
    llvm::Value *nullEnd2 = builder_.CreateGEP(builder_.getInt8Ty(), buf, finalEnd, "tend_null");
    builder_.CreateStore(llvm::ConstantInt::get(i8Ty_, 0), nullEnd2);
    return buf;
}

// repeat(s, n) → str
llvm::Value *CodeGen::emitStrOp_repeat(const CallExpr &e) {
    requireArgs(e, 2);
    llvm::Value *s = emitExpr(*e.args[0]);
    llvm::Value *n = emitExpr(*e.args[1]);
    if (s->getType() != ptrTy_)
        codegenError("repeat() requires str as first argument");
    if (n->getType() != i64Ty_)
        codegenError("repeat() requires int as second argument");

    return emitStringRepeat(s, n);
}

// reverse(list) → new reversed list, or reverse(str) → str
llvm::Value *CodeGen::emitStrOp_reverse(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *arg = emitExpr(*e.args[0]);

    // List reverse
    llvm::Type *elemTy = getListElementType(arg);
    if (elemTy) {
        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        uint64_t elemSize = dl.getTypeAllocSize(elemTy);

        auto lf = loadListHeader(arg, "rev");
        llvm::Value *len = lf.len;
        llvm::Value *srcData = lf.data;

        llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "rev_header");
        llvm::Value *dataSize = builder_.CreateMul(len, llvm::ConstantInt::get(i64Ty_, elemSize), "rev_dsize");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "rev_data");

        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "rev_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "lrev.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "lrev.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "lrev.end", fn_);
        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "rev_idx");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, len, "rev_cond"), bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "rev_i_cur");
        llvm::Value *srcIdx = builder_.CreateSub(builder_.CreateSub(len, llvm::ConstantInt::get(i64Ty_, 1)), iCur, "rev_src_idx");
        llvm::Value *srcPtr = builder_.CreateGEP(elemTy, srcData, srcIdx, "rev_src");
        llvm::Value *val = builder_.CreateLoad(elemTy, srcPtr, "rev_val");
        llvm::Value *dstPtr = builder_.CreateGEP(elemTy, newData, iCur, "rev_dst");
        builder_.CreateStore(val, dstPtr);
        builder_.CreateStore(builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(endBB);
        llvm::Value *newLenPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 0, "rev_new_len");
        builder_.CreateStore(len, newLenPtr);
        llvm::Value *newCapPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 1, "rev_new_cap");
        builder_.CreateStore(len, newCapPtr);
        llvm::Value *newDataField = builder_.CreateStructGEP(listHeaderTy_, newHeader, 2, "rev_new_data");
        builder_.CreateStore(newData, newDataField);

        type_meta_[TM_ListElem][newHeader] = elemTy;
        return newHeader;
    }

    // String reverse (UTF-8 aware)
    llvm::Value *s = arg;
    if (s->getType() != ptrTy_)
        codegenError("reverse() requires list or str argument");

    auto revFn = getRuntimeFn("__ry_utf8_reverse", ptrTy_, {ptrTy_});
    return builder_.CreateCall(revFn, {s}, "str_rev");
}

// reverse!(list) → in-place reverse
llvm::Value *CodeGen::emitStrOp_reverse_mut(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::AllocaInst *receiverAlloca = tryGetReceiverAlloca(*e.args[0]);
    llvm::Value *listPtr = emitExpr(*e.args[0]);
    llvm::Type *elemTy = getListElementType(listPtr);
    if (!elemTy) codegenError("reverse!() requires a list");
    listPtr = emitCowCheck(listPtr, receiverAlloca, CollectionKind::List);

    auto lf = loadListHeader(listPtr, "revm");
    llvm::Value *len = lf.len;
    llvm::Value *dataPtr = lf.data;

    // Swap elements: i from 0 to len/2
    llvm::Value *half = builder_.CreateSDiv(len, llvm::ConstantInt::get(i64Ty_, 2), "revm_half");
    llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "revm_i");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "revm.cond", fn_);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "revm.body", fn_);
    llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "revm.end", fn_);
    builder_.CreateBr(condBB);

    builder_.SetInsertPoint(condBB);
    llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "revm_idx");
    builder_.CreateCondBr(builder_.CreateICmpSLT(i, half, "revm_cond"), bodyBB, endBB);

    builder_.SetInsertPoint(bodyBB);
    llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "revm_cur");
    llvm::Value *j = builder_.CreateSub(builder_.CreateSub(len, llvm::ConstantInt::get(i64Ty_, 1)), iCur, "revm_j");
    llvm::Value *ptrI = builder_.CreateGEP(elemTy, dataPtr, iCur, "revm_pi");
    llvm::Value *ptrJ = builder_.CreateGEP(elemTy, dataPtr, j, "revm_pj");
    llvm::Value *vi = builder_.CreateLoad(elemTy, ptrI, "revm_vi");
    llvm::Value *vj = builder_.CreateLoad(elemTy, ptrJ, "revm_vj");
    builder_.CreateStore(vj, ptrI);
    builder_.CreateStore(vi, ptrJ);
    builder_.CreateStore(builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
    builder_.CreateBr(condBB);

    builder_.SetInsertPoint(endBB);
    return llvm::ConstantInt::get(i64Ty_, 0);
}

// split(s, delim) → List<str>
llvm::Value *CodeGen::emitStrOp_split(const CallExpr &e) {
    requireArgs(e, 2);
    llvm::Value *s = emitExpr(*e.args[0]);
    llvm::Value *delim = emitExpr(*e.args[1]);
    // Regex overload: split(text, /pattern/) → delegate to regex runtime
    if (isRegex(delim) && isStringValue(s)) {
        auto fn = mod_->getOrInsertFunction("__ry_regex_split", fnTy_ptr_ptr_to_ptr_);
        llvm::Value *r = builder_.CreateCall(fn, {delim, s}, "regex_split");
        type_meta_[TM_ListElem][r] = ptrTy_;
        return r;
    }
    if (s->getType() != ptrTy_ || delim->getType() != ptrTy_)
        codegenError("split() requires str arguments");
    auto strlenFn = getStdlibStrlen();
    auto strstrFn = getStdlibStrstr();
    auto mallocFn = getStdlibMalloc();
    auto memcpyFn = getStdlibMemcpy();

    llvm::Value *delimLen = builder_.CreateCall(strlenFn, {delim}, "split_dlen");
    llvm::Value *null = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));

    llvm::AllocaInst *countVar = builder_.CreateAlloca(i64Ty_, nullptr, "split_count");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), countVar);
    llvm::AllocaInst *searchVar = builder_.CreateAlloca(ptrTy_, nullptr, "split_search");
    builder_.CreateStore(s, searchVar);

    llvm::BasicBlock *countCondBB = llvm::BasicBlock::Create(*ctx_, "split.count_cond", fn_);
    llvm::BasicBlock *countBodyBB = llvm::BasicBlock::Create(*ctx_, "split.count_body", fn_);
    llvm::BasicBlock *countEndBB = llvm::BasicBlock::Create(*ctx_, "split.count_end", fn_);

    builder_.CreateBr(countCondBB);
    builder_.SetInsertPoint(countCondBB);
    llvm::Value *sp = builder_.CreateLoad(ptrTy_, searchVar, "split_sp");
    llvm::Value *found = builder_.CreateCall(strstrFn, {sp, delim}, "split_found");
    builder_.CreateCondBr(builder_.CreateICmpNE(found, null, "split_nn"), countBodyBB, countEndBB);

    builder_.SetInsertPoint(countBodyBB);
    llvm::Value *cnt = builder_.CreateLoad(i64Ty_, countVar, "split_cnt");
    builder_.CreateStore(builder_.CreateAdd(cnt, llvm::ConstantInt::get(i64Ty_, 1)), countVar);
    builder_.CreateStore(builder_.CreateGEP(builder_.getInt8Ty(), found, delimLen, "split_adv"), searchVar);
    builder_.CreateBr(countCondBB);

    builder_.SetInsertPoint(countEndBB);
    llvm::Value *delimCount = builder_.CreateLoad(i64Ty_, countVar, "split_delim_count");
    llvm::Value *elemCount = builder_.CreateAdd(delimCount, llvm::ConstantInt::get(i64Ty_, 1), "split_elem_count");

    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
    llvm::Value *headerPtr = builder_.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "split_header");

    uint64_t ptrSize = dl.getTypeAllocSize(ptrTy_);
    llvm::Value *dataSize = builder_.CreateMul(elemCount, llvm::ConstantInt::get(i64Ty_, ptrSize), "split_data_size");
    llvm::Value *dataPtr = builder_.CreateCall(mallocFn, {dataSize}, "split_data");

    llvm::AllocaInst *srcVar = builder_.CreateAlloca(ptrTy_, nullptr, "split_src");
    builder_.CreateStore(s, srcVar);
    llvm::AllocaInst *idxVar = builder_.CreateAlloca(i64Ty_, nullptr, "split_idx");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), idxVar);

    llvm::BasicBlock *buildCondBB = llvm::BasicBlock::Create(*ctx_, "split.build_cond", fn_);
    llvm::BasicBlock *buildBodyBB = llvm::BasicBlock::Create(*ctx_, "split.build_body", fn_);
    llvm::BasicBlock *buildEndBB = llvm::BasicBlock::Create(*ctx_, "split.build_end", fn_);

    builder_.CreateBr(buildCondBB);
    builder_.SetInsertPoint(buildCondBB);
    llvm::Value *curSrc = builder_.CreateLoad(ptrTy_, srcVar, "split_cur_src");
    llvm::Value *foundBuild = builder_.CreateCall(strstrFn, {curSrc, delim}, "split_found_build");
    builder_.CreateCondBr(builder_.CreateICmpNE(foundBuild, null, "split_build_nn"), buildBodyBB, buildEndBB);

    builder_.SetInsertPoint(buildBodyBB);
    llvm::Value *curSrcInt = builder_.CreatePtrToInt(curSrc, i64Ty_, "split_src_int");
    llvm::Value *foundInt = builder_.CreatePtrToInt(foundBuild, i64Ty_, "split_found_int");
    llvm::Value *segLen = builder_.CreateSub(foundInt, curSrcInt, "split_seg_len");
    llvm::Value *segBufSize = builder_.CreateAdd(segLen, llvm::ConstantInt::get(i64Ty_, 1), "split_seg_bsize");
    llvm::Value *segBuf = builder_.CreateCall(mallocFn, {segBufSize}, "split_seg_buf");
    builder_.CreateCall(memcpyFn, {segBuf, curSrc, segLen});
    llvm::Value *segNull = builder_.CreateGEP(builder_.getInt8Ty(), segBuf, segLen, "split_seg_null");
    builder_.CreateStore(llvm::ConstantInt::get(i8Ty_, 0), segNull);
    llvm::Value *curIdx = builder_.CreateLoad(i64Ty_, idxVar, "split_cur_idx");
    llvm::Value *elemPtr = builder_.CreateGEP(ptrTy_, dataPtr, {curIdx}, "split_elem_ptr");
    builder_.CreateStore(segBuf, elemPtr);
    builder_.CreateStore(builder_.CreateAdd(curIdx, llvm::ConstantInt::get(i64Ty_, 1)), idxVar);
    builder_.CreateStore(builder_.CreateGEP(builder_.getInt8Ty(), foundBuild, delimLen, "split_adv2"), srcVar);
    builder_.CreateBr(buildCondBB);

    builder_.SetInsertPoint(buildEndBB);
    llvm::Value *lastSrc = builder_.CreateLoad(ptrTy_, srcVar, "split_last_src");
    llvm::Value *lastLen = builder_.CreateCall(strlenFn, {lastSrc}, "split_last_len");
    llvm::Value *lastBufSize = builder_.CreateAdd(lastLen, llvm::ConstantInt::get(i64Ty_, 1), "split_last_bsize");
    llvm::Value *lastBuf = builder_.CreateCall(mallocFn, {lastBufSize}, "split_last_buf");
    builder_.CreateCall(memcpyFn, {lastBuf, lastSrc, lastLen});
    llvm::Value *lastNull = builder_.CreateGEP(builder_.getInt8Ty(), lastBuf, lastLen, "split_last_null");
    builder_.CreateStore(llvm::ConstantInt::get(i8Ty_, 0), lastNull);
    llvm::Value *lastIdx = builder_.CreateLoad(i64Ty_, idxVar, "split_last_idx");
    llvm::Value *lastElemPtr = builder_.CreateGEP(ptrTy_, dataPtr, {lastIdx}, "split_last_elem_ptr");
    builder_.CreateStore(lastBuf, lastElemPtr);

    llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 0, "split_len_ptr");
    builder_.CreateStore(elemCount, lenPtr);
    llvm::Value *capPtr = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 1, "split_cap_ptr");
    builder_.CreateStore(elemCount, capPtr);
    llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 2, "split_data_field");
    builder_.CreateStore(dataPtr, dataPtrField);

    type_meta_[TM_ListElem][headerPtr] = ptrTy_;
    return headerPtr;
}

// join(list, sep) → str
llvm::Value *CodeGen::emitStrOp_join(const CallExpr &e) {
    if (e.args.size() != 2)
        return nullptr; // Not the builtin join(List<str>, str); fall through
    llvm::Value *listPtr = emitExpr(*e.args[0]);
    llvm::Value *sep = emitExpr(*e.args[1]);
    if (listPtr->getType() != ptrTy_ || sep->getType() != ptrTy_)
        codegenError("join() requires List<str> and str arguments");
    llvm::Type *elemTy = getListElementType(listPtr);
    if (!elemTy)
        return nullptr; // First arg is not a list; fall through to stdlib
    if (elemTy != ptrTy_)
        codegenError("join() requires List<str> as first argument");
    auto strlenFn = getStdlibStrlen();
    auto mallocFn = getStdlibMalloc();
    auto memcpyFn = getStdlibMemcpy();

    auto lf = loadListHeader(listPtr, "join");
    llvm::Value *listLen = lf.len;
    llvm::Value *listData = lf.data;
    llvm::Value *sepLen = builder_.CreateCall(strlenFn, {sep}, "join_sep_len");

    llvm::AllocaInst *totalVar = builder_.CreateAlloca(i64Ty_, nullptr, "join_total");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), totalVar);
    llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "join_i");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

    llvm::BasicBlock *len1CondBB = llvm::BasicBlock::Create(*ctx_, "join.len_cond", fn_);
    llvm::BasicBlock *len1BodyBB = llvm::BasicBlock::Create(*ctx_, "join.len_body", fn_);
    llvm::BasicBlock *len1EndBB = llvm::BasicBlock::Create(*ctx_, "join.len_end", fn_);

    builder_.CreateBr(len1CondBB);
    builder_.SetInsertPoint(len1CondBB);
    llvm::Value *i1 = builder_.CreateLoad(i64Ty_, iVar, "join_i1");
    builder_.CreateCondBr(builder_.CreateICmpSLT(i1, listLen, "join_len_cond"), len1BodyBB, len1EndBB);

    builder_.SetInsertPoint(len1BodyBB);
    llvm::Value *i1Cur = builder_.CreateLoad(i64Ty_, iVar, "join_i1_cur");
    llvm::Value *elemPtr = builder_.CreateGEP(ptrTy_, listData, {i1Cur}, "join_elem_ptr");
    llvm::Value *elem = builder_.CreateLoad(ptrTy_, elemPtr, "join_elem");
    llvm::Value *elemLen = builder_.CreateCall(strlenFn, {elem}, "join_elem_len");
    llvm::Value *total = builder_.CreateLoad(i64Ty_, totalVar, "join_total_cur");
    llvm::Value *newTotal = builder_.CreateAdd(total, elemLen, "join_total_add");
    builder_.CreateStore(newTotal, totalVar);
    builder_.CreateStore(builder_.CreateAdd(i1Cur, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
    builder_.CreateBr(len1CondBB);

    builder_.SetInsertPoint(len1EndBB);
    llvm::Value *elemTotal = builder_.CreateLoad(i64Ty_, totalVar, "join_elem_total");
    llvm::Value *sepCount = builder_.CreateSub(listLen, llvm::ConstantInt::get(i64Ty_, 1), "join_sep_count");
    llvm::Value *isPositive = builder_.CreateICmpSGT(listLen, llvm::ConstantInt::get(i64Ty_, 0), "join_has_elems");
    llvm::Value *safeSepCount = builder_.CreateSelect(isPositive, sepCount, llvm::ConstantInt::get(i64Ty_, 0), "safe_sep_count");
    llvm::Value *sepTotal = builder_.CreateMul(safeSepCount, sepLen, "join_sep_total");
    llvm::Value *grandTotal = builder_.CreateAdd(elemTotal, sepTotal, "join_grand_total");
    llvm::Value *bufSize = builder_.CreateAdd(grandTotal, llvm::ConstantInt::get(i64Ty_, 1), "join_bsize");
    llvm::Value *buf = builder_.CreateCall(mallocFn, {bufSize}, "join_buf");

    llvm::AllocaInst *dstVar = builder_.CreateAlloca(ptrTy_, nullptr, "join_dst");
    builder_.CreateStore(buf, dstVar);
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

    llvm::BasicBlock *buildCondBB = llvm::BasicBlock::Create(*ctx_, "join.build_cond", fn_);
    llvm::BasicBlock *buildBodyBB = llvm::BasicBlock::Create(*ctx_, "join.build_body", fn_);
    llvm::BasicBlock *buildEndBB = llvm::BasicBlock::Create(*ctx_, "join.build_end", fn_);

    builder_.CreateBr(buildCondBB);
    builder_.SetInsertPoint(buildCondBB);
    llvm::Value *i2 = builder_.CreateLoad(i64Ty_, iVar, "join_i2");
    builder_.CreateCondBr(builder_.CreateICmpSLT(i2, listLen, "join_build_cond"), buildBodyBB, buildEndBB);

    builder_.SetInsertPoint(buildBodyBB);
    llvm::Value *i2Cur = builder_.CreateLoad(i64Ty_, iVar, "join_i2_cur");
    llvm::Value *curDst = builder_.CreateLoad(ptrTy_, dstVar, "join_cur_dst");

    llvm::Value *notFirst = builder_.CreateICmpSGT(i2Cur, llvm::ConstantInt::get(i64Ty_, 0), "join_not_first");
    llvm::BasicBlock *sepBB = llvm::BasicBlock::Create(*ctx_, "join.sep", fn_);
    llvm::BasicBlock *elemBB = llvm::BasicBlock::Create(*ctx_, "join.elem", fn_);
    builder_.CreateCondBr(notFirst, sepBB, elemBB);

    builder_.SetInsertPoint(sepBB);
    llvm::Value *dstBeforeSep = builder_.CreateLoad(ptrTy_, dstVar, "dst_before_sep");
    builder_.CreateCall(memcpyFn, {dstBeforeSep, sep, sepLen});
    llvm::Value *dstAfterSep = builder_.CreateGEP(builder_.getInt8Ty(), dstBeforeSep, sepLen, "dst_after_sep");
    builder_.CreateStore(dstAfterSep, dstVar);
    builder_.CreateBr(elemBB);

    builder_.SetInsertPoint(elemBB);
    llvm::Value *dstForElem = builder_.CreateLoad(ptrTy_, dstVar, "dst_for_elem");
    llvm::Value *elemPtr2 = builder_.CreateGEP(ptrTy_, listData, {i2Cur}, "join_elem_ptr2");
    llvm::Value *elem2 = builder_.CreateLoad(ptrTy_, elemPtr2, "join_elem2");
    llvm::Value *elem2Len = builder_.CreateCall(strlenFn, {elem2}, "join_elem2_len");
    builder_.CreateCall(memcpyFn, {dstForElem, elem2, elem2Len});
    llvm::Value *dstAfterElem = builder_.CreateGEP(builder_.getInt8Ty(), dstForElem, elem2Len, "dst_after_elem");
    builder_.CreateStore(dstAfterElem, dstVar);
    builder_.CreateStore(builder_.CreateAdd(i2Cur, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
    builder_.CreateBr(buildCondBB);

    builder_.SetInsertPoint(buildEndBB);
    llvm::Value *finalDst = builder_.CreateLoad(ptrTy_, dstVar, "join_final_dst");
    builder_.CreateStore(llvm::ConstantInt::get(i8Ty_, 0), finalDst);
    return buf;
}

