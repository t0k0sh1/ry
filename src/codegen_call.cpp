#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"

namespace {
// RAII guard to restore moved-out args after proxy CallExpr delegation
struct ArgsRestoreGuard {
    std::vector<ExprPtr> &dst;
    std::vector<ExprPtr> &src;
    ~ArgsRestoreGuard() { dst = std::move(src); }
};
} // namespace

// ===== Whitespace helper =====

llvm::Value *CodeGen::emitIsWhitespace(llvm::Value *ch) {
    llvm::Value *isSp  = builder_.CreateICmpEQ(ch, llvm::ConstantInt::get(i8Ty_, ' '));
    llvm::Value *isTab = builder_.CreateICmpEQ(ch, llvm::ConstantInt::get(i8Ty_, '\t'));
    llvm::Value *isNl  = builder_.CreateICmpEQ(ch, llvm::ConstantInt::get(i8Ty_, '\n'));
    llvm::Value *isCr  = builder_.CreateICmpEQ(ch, llvm::ConstantInt::get(i8Ty_, '\r'));
    return builder_.CreateOr(builder_.CreateOr(isSp, isTab),
                             builder_.CreateOr(isNl, isCr));
}

// ===== Builtin String =====

llvm::Value *CodeGen::emitBuiltinString(const CallExpr &e) {
    // contains(s, sub) → bool
    if (e.callee == "contains") {
        if (e.args.size() != 2)
            codegenError("contains() takes exactly 2 arguments");
        llvm::Value *s = emitExpr(*e.args[0]);
        llvm::Value *sub = emitExpr(*e.args[1]);
        if (s->getType() != ptrTy_ || sub->getType() != ptrTy_)
            codegenError("contains() requires str arguments");
        auto strstrFn = getStdlibStrstr();
        llvm::Value *result = builder_.CreateCall(strstrFn, {s, sub}, "strstr");
        llvm::Value *null = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));
        return builder_.CreateICmpNE(result, null, "contains");
    }

    // starts_with(s, prefix) → bool
    if (e.callee == "starts_with") {
        if (e.args.size() != 2)
            codegenError("starts_with() takes exactly 2 arguments");
        llvm::Value *s = emitExpr(*e.args[0]);
        llvm::Value *prefix = emitExpr(*e.args[1]);
        if (s->getType() != ptrTy_ || prefix->getType() != ptrTy_)
            codegenError("starts_with() requires str arguments");
        auto strlenFn = getStdlibStrlen();
        auto strncmpFn = getStdlibStrncmp();
        llvm::Value *prefixLen = builder_.CreateCall(strlenFn, {prefix}, "prefix_len");
        llvm::Value *cmp = builder_.CreateCall(strncmpFn, {s, prefix, prefixLen}, "strncmp");
        return builder_.CreateICmpEQ(cmp, llvm::ConstantInt::get(i32Ty_, 0), "starts_with");
    }

    // ends_with(s, suffix) → bool
    if (e.callee == "ends_with") {
        if (e.args.size() != 2)
            codegenError("ends_with() takes exactly 2 arguments");
        llvm::Value *s = emitExpr(*e.args[0]);
        llvm::Value *suffix = emitExpr(*e.args[1]);
        if (s->getType() != ptrTy_ || suffix->getType() != ptrTy_)
            codegenError("ends_with() requires str arguments");
        auto strlenFn = getStdlibStrlen();
        auto strncmpFn = getStdlibStrncmp();
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
        llvm::Value *cmp = builder_.CreateCall(strncmpFn, {tailPtr, suffix, suffixLen}, "strncmp");
        llvm::Value *match = builder_.CreateICmpEQ(cmp, llvm::ConstantInt::get(i32Ty_, 0), "match");
        builder_.CreateBr(mergeBB);

        builder_.SetInsertPoint(mergeBB);
        llvm::PHINode *phi = builder_.CreatePHI(i1Ty_, 2, "ends_with");
        phi->addIncoming(llvm::ConstantInt::get(i1Ty_, 0), curBB);
        phi->addIncoming(match, checkBB);
        return phi;
    }

    // find(s, sub) → Option<int>
    if (e.callee == "find") {
        if (e.args.size() != 2)
            codegenError("find() takes exactly 2 arguments");
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
        auto charIdxTy = llvm::FunctionType::get(i64Ty_, {ptrTy_, i64Ty_}, false);
        auto charIdxFn = mod_->getOrInsertFunction("__ry_utf8_char_index", charIdxTy);
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

    // substring(s, start, end) → str (UTF-8 character indices)
    if (e.callee == "substring") {
        if (e.args.size() != 3)
            codegenError("substring() takes exactly 3 arguments");
        llvm::Value *s = emitExpr(*e.args[0]);
        llvm::Value *start = emitExpr(*e.args[1]);
        llvm::Value *end = emitExpr(*e.args[2]);
        if (s->getType() != ptrTy_)
            codegenError("substring() requires str as first argument");

        auto substrTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, i64Ty_, i64Ty_}, false);
        auto substrFn = mod_->getOrInsertFunction("__ry_utf8_substring", substrTy);
        return builder_.CreateCall(substrFn, {s, start, end}, "substring");
    }

    // char_at(s, i) → str (single UTF-8 character as string)
    if (e.callee == "char_at") {
        if (e.args.size() != 2)
            codegenError("char_at() takes exactly 2 arguments");
        llvm::Value *s = emitExpr(*e.args[0]);
        llvm::Value *idx = emitExpr(*e.args[1]);
        if (s->getType() != ptrTy_)
            codegenError("char_at() requires str as first argument");

        auto charAtTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, i64Ty_}, false);
        auto charAtFn = mod_->getOrInsertFunction("__ry_utf8_char_at", charAtTy);
        return builder_.CreateCall(charAtFn, {s, idx}, "char_at");
    }

    // replace(s, old, new) → str
    if (e.callee == "replace") {
        if (e.args.size() != 3)
            codegenError("replace() takes exactly 3 arguments");
        llvm::Value *s = emitExpr(*e.args[0]);
        llvm::Value *oldStr = emitExpr(*e.args[1]);
        llvm::Value *newStr = emitExpr(*e.args[2]);
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
    if (e.callee == "to_upper") {
        if (e.args.size() != 1)
            codegenError("to_upper() takes exactly 1 argument");
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
    if (e.callee == "to_lower") {
        if (e.args.size() != 1)
            codegenError("to_lower() takes exactly 1 argument");
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
    if (e.callee == "trim") {
        if (e.args.size() != 1)
            codegenError("trim() takes exactly 1 argument");
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
    if (e.callee == "trim_start") {
        if (e.args.size() != 1)
            codegenError("trim_start() takes exactly 1 argument");
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
    if (e.callee == "trim_end") {
        if (e.args.size() != 1)
            codegenError("trim_end() takes exactly 1 argument");
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
    if (e.callee == "repeat") {
        if (e.args.size() != 2)
            codegenError("repeat() takes exactly 2 arguments");
        llvm::Value *s = emitExpr(*e.args[0]);
        llvm::Value *n = emitExpr(*e.args[1]);
        if (s->getType() != ptrTy_)
            codegenError("repeat() requires str as first argument");
        auto strlenFn = getStdlibStrlen();
        auto mallocFn = getStdlibMalloc();
        auto memcpyFn = getStdlibMemcpy();

        llvm::Value *sLen = builder_.CreateCall(strlenFn, {s}, "repeat_slen");
        llvm::Value *totalLen = builder_.CreateMul(sLen, n, "repeat_total");
        llvm::Value *bufSize = builder_.CreateAdd(totalLen, llvm::ConstantInt::get(i64Ty_, 1), "repeat_bsize");
        llvm::Value *buf = builder_.CreateCall(mallocFn, {bufSize}, "repeat_buf");

        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "repeat_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "repeat.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "repeat.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "repeat.end", fn_);

        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "repeat_idx");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, n, "repeat_cond"), bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "repeat_i_cur");
        llvm::Value *offset = builder_.CreateMul(iCur, sLen, "repeat_offset");
        llvm::Value *dstPtr = builder_.CreateGEP(builder_.getInt8Ty(), buf, offset, "repeat_dst");
        builder_.CreateCall(memcpyFn, {dstPtr, s, sLen});
        builder_.CreateStore(builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1), "repeat_next"), iVar);
        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(endBB);
        llvm::Value *nullPtr = builder_.CreateGEP(builder_.getInt8Ty(), buf, totalLen, "repeat_null");
        builder_.CreateStore(llvm::ConstantInt::get(i8Ty_, 0), nullPtr);
        return buf;
    }

    // reverse(list) → new reversed list, or reverse(str) → str
    if (e.callee == "reverse") {
        if (e.args.size() != 1)
            codegenError("reverse() takes exactly 1 argument");
        llvm::Value *arg = emitExpr(*e.args[0]);

        // List reverse
        llvm::Type *elemTy = getListElementType(arg);
        if (elemTy) {
            auto mallocFn = getStdlibMalloc();
            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
            uint64_t elemSize = dl.getTypeAllocSize(elemTy);

            llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, arg, 0, "rev_len_ptr");
            llvm::Value *len = builder_.CreateLoad(i64Ty_, lenPtr, "rev_len");
            llvm::Value *dataField = builder_.CreateStructGEP(listHeaderTy_, arg, 2, "rev_data_field");
            llvm::Value *srcData = builder_.CreateLoad(ptrTy_, dataField, "rev_src_data");

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

            list_element_types_[newHeader] = elemTy;
            return newHeader;
        }

        // String reverse (UTF-8 aware)
        llvm::Value *s = arg;
        if (s->getType() != ptrTy_)
            codegenError("reverse() requires list or str argument");

        auto revTy = llvm::FunctionType::get(ptrTy_, {ptrTy_}, false);
        auto revFn = mod_->getOrInsertFunction("__ry_utf8_reverse", revTy);
        return builder_.CreateCall(revFn, {s}, "str_rev");
    }

    // reverse!(list) → in-place reverse
    if (e.callee == "reverse!") {
        if (e.args.size() != 1)
            codegenError("reverse!() takes exactly 1 argument");
        llvm::Value *listPtr = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listPtr);
        if (!elemTy) codegenError("reverse!() requires a list");

        llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, listPtr, 0, "revm_len_ptr");
        llvm::Value *len = builder_.CreateLoad(i64Ty_, lenPtr, "revm_len");
        llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(listHeaderTy_, listPtr, 2), "revm_data");

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
    if (e.callee == "split") {
        if (e.args.size() != 2)
            codegenError("split() takes exactly 2 arguments");
        llvm::Value *s = emitExpr(*e.args[0]);
        llvm::Value *delim = emitExpr(*e.args[1]);
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

        list_element_types_[headerPtr] = ptrTy_;
        return headerPtr;
    }

    // join(list, sep) → str
    if (e.callee == "join") {
        if (e.args.size() != 2) {
            if (e.args.size() != 1)
                codegenError("join() expects 1 argument (Task<T>) or 2 arguments (List<str>, str)");
            return nullptr;
        }
        llvm::Value *listPtr = emitExpr(*e.args[0]);
        llvm::Value *sep = emitExpr(*e.args[1]);
        if (listPtr->getType() != ptrTy_ || sep->getType() != ptrTy_)
            codegenError("join() requires List<str> and str arguments");
        if (getListElementType(listPtr) != ptrTy_)
            codegenError("join() requires List<str> as first argument");
        auto strlenFn = getStdlibStrlen();
        auto mallocFn = getStdlibMalloc();
        auto memcpyFn = getStdlibMemcpy();

        llvm::Value *listLen = builder_.CreateLoad(i64Ty_,
            builder_.CreateStructGEP(listHeaderTy_, listPtr, 0, "join_len_ptr"), "join_len");
        llvm::Value *listData = builder_.CreateLoad(ptrTy_,
            builder_.CreateStructGEP(listHeaderTy_, listPtr, 2, "join_data_ptr"), "join_data");
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

    return nullptr;
}

// ===== Builtin Conversion =====

llvm::Value *CodeGen::emitBuiltinConversion(const CallExpr &e) {
    // to_int(s) → int
    if (e.callee == "to_int") {
        if (e.args.size() != 1)
            codegenError("to_int() takes exactly 1 argument");
        llvm::Value *s = emitExpr(*e.args[0]);
        if (s->getType() != ptrTy_)
            codegenError("to_int() requires str argument");
        auto atolTy = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
        auto atolFn = mod_->getOrInsertFunction("atol", atolTy);
        return builder_.CreateCall(atolFn, {s}, "to_int");
    }

    // to_float(s) → float
    if (e.callee == "to_float") {
        if (e.args.size() != 1)
            codegenError("to_float() takes exactly 1 argument");
        llvm::Value *s = emitExpr(*e.args[0]);
        if (s->getType() != ptrTy_)
            codegenError("to_float() requires str argument");
        auto atofTy = llvm::FunctionType::get(f64Ty_, {ptrTy_}, false);
        auto atofFn = mod_->getOrInsertFunction("atof", atofTy);
        return builder_.CreateCall(atofFn, {s}, "to_float");
    }

    // to_str(v) → str (int/float/bool/str → str)
    if (e.callee == "to_str") {
        if (e.args.size() != 1)
            codegenError("to_str() takes exactly 1 argument");
        return valueToString(emitExpr(*e.args[0]));
    }

    return nullptr;
}

// ===== Builtin Query =====

llvm::Value *CodeGen::emitBuiltinQuery(const CallExpr &e) {
    // ===== keys(map) =====
    if (e.callee == "keys") {
        if (e.args.size() != 1)
            codegenError("keys() takes exactly 1 argument");
        llvm::Value *mapVal = emitExpr(*e.args[0]);
        llvm::Type *keyTy = getMapKeyType(mapVal);
        if (!keyTy) codegenError("keys() requires a map");

        llvm::Value *mapLen = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(mapHeaderTy_, mapVal, 0), "keys_len");
        llvm::Value *keysData = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(mapHeaderTy_, mapVal, 2), "keys_data");

        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "keys_header");
        uint64_t elemSize = dl.getTypeAllocSize(keyTy);
        llvm::Value *dataSize = builder_.CreateMul(mapLen, llvm::ConstantInt::get(i64Ty_, elemSize), "keys_ds");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "keys_nd");
        auto memcpyFn = getStdlibMemcpy();
        builder_.CreateCall(memcpyFn, {newData, keysData, dataSize});

        builder_.CreateStore(mapLen, builder_.CreateStructGEP(listHeaderTy_, newHeader, 0));
        builder_.CreateStore(mapLen, builder_.CreateStructGEP(listHeaderTy_, newHeader, 1));
        builder_.CreateStore(newData, builder_.CreateStructGEP(listHeaderTy_, newHeader, 2));
        list_element_types_[newHeader] = keyTy;
        return newHeader;
    }

    // ===== values(map) =====
    if (e.callee == "values") {
        if (e.args.size() != 1)
            codegenError("values() takes exactly 1 argument");
        llvm::Value *mapVal = emitExpr(*e.args[0]);
        llvm::Type *valTy = getMapValueType(mapVal);
        if (!valTy) codegenError("values() requires a map");

        llvm::Value *mapLen = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(mapHeaderTy_, mapVal, 0), "vals_len");
        llvm::Value *valsData = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(mapHeaderTy_, mapVal, 3), "vals_data");

        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "vals_header");
        uint64_t elemSize = dl.getTypeAllocSize(valTy);
        llvm::Value *dataSize = builder_.CreateMul(mapLen, llvm::ConstantInt::get(i64Ty_, elemSize), "vals_ds");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "vals_nd");
        auto memcpyFn = getStdlibMemcpy();
        builder_.CreateCall(memcpyFn, {newData, valsData, dataSize});

        builder_.CreateStore(mapLen, builder_.CreateStructGEP(listHeaderTy_, newHeader, 0));
        builder_.CreateStore(mapLen, builder_.CreateStructGEP(listHeaderTy_, newHeader, 1));
        builder_.CreateStore(newData, builder_.CreateStructGEP(listHeaderTy_, newHeader, 2));
        list_element_types_[newHeader] = valTy;
        return newHeader;
    }

    // ===== first(list) → Option<T> =====
    if (e.callee == "first") {
        if (e.args.size() != 1)
            codegenError("first() takes exactly 1 argument");
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy) codegenError("first() requires a list");
        llvm::StructType *optTy = getOptionType(elemTy);
        llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(listHeaderTy_, listVal, 0), "first_len");
        llvm::Value *srcData = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(listHeaderTy_, listVal, 2), "first_data");

        llvm::Value *isEmptyF = builder_.CreateICmpEQ(srcLen, llvm::ConstantInt::get(i64Ty_, 0), "first_empty");
        llvm::BasicBlock *emptyBB = llvm::BasicBlock::Create(*ctx_, "first.empty", fn_);
        llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "first.ok", fn_);
        llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "first.merge", fn_);
        builder_.CreateCondBr(isEmptyF, emptyBB, okBB);

        builder_.SetInsertPoint(emptyBB);
        llvm::Value *noneVal = buildNoneValue(optTy);
        builder_.CreateBr(mergeBB);
        llvm::BasicBlock *emptyEndBB = builder_.GetInsertBlock();

        builder_.SetInsertPoint(okBB);
        llvm::Value *firstVal = builder_.CreateLoad(elemTy, srcData, "first_val");
        llvm::Value *someVal = buildSomeValue(firstVal, optTy);
        builder_.CreateBr(mergeBB);
        llvm::BasicBlock *okEndBB = builder_.GetInsertBlock();

        builder_.SetInsertPoint(mergeBB);
        llvm::PHINode *phi = builder_.CreatePHI(optTy, 2, "first_result");
        phi->addIncoming(noneVal, emptyEndBB);
        phi->addIncoming(someVal, okEndBB);
        return phi;
    }

    // ===== last(list) → Option<T> =====
    if (e.callee == "last") {
        if (e.args.size() != 1)
            codegenError("last() takes exactly 1 argument");
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy) codegenError("last() requires a list");
        llvm::StructType *optTy = getOptionType(elemTy);
        llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(listHeaderTy_, listVal, 0), "last_len");
        llvm::Value *srcData = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(listHeaderTy_, listVal, 2), "last_data");

        llvm::Value *isEmptyL = builder_.CreateICmpEQ(srcLen, llvm::ConstantInt::get(i64Ty_, 0), "last_empty");
        llvm::BasicBlock *emptyBBL = llvm::BasicBlock::Create(*ctx_, "last.empty", fn_);
        llvm::BasicBlock *okBBL = llvm::BasicBlock::Create(*ctx_, "last.ok", fn_);
        llvm::BasicBlock *mergeBBL = llvm::BasicBlock::Create(*ctx_, "last.merge", fn_);
        builder_.CreateCondBr(isEmptyL, emptyBBL, okBBL);

        builder_.SetInsertPoint(emptyBBL);
        llvm::Value *noneValL = buildNoneValue(optTy);
        builder_.CreateBr(mergeBBL);
        llvm::BasicBlock *emptyEndBBL = builder_.GetInsertBlock();

        builder_.SetInsertPoint(okBBL);
        llvm::Value *lastIdx = builder_.CreateSub(srcLen, llvm::ConstantInt::get(i64Ty_, 1), "last_idx");
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, srcData, {lastIdx}, "last_ep");
        llvm::Value *lastVal = builder_.CreateLoad(elemTy, elemPtr, "last_val");
        llvm::Value *someValL = buildSomeValue(lastVal, optTy);
        builder_.CreateBr(mergeBBL);
        llvm::BasicBlock *okEndBBL = builder_.GetInsertBlock();

        builder_.SetInsertPoint(mergeBBL);
        llvm::PHINode *phiL = builder_.CreatePHI(optTy, 2, "last_result");
        phiL->addIncoming(noneValL, emptyEndBBL);
        phiL->addIncoming(someValL, okEndBBL);
        return phiL;
    }

    // ===== is_empty(list/map/set) =====
    if (e.callee == "is_empty") {
        if (e.args.size() != 1)
            codegenError("is_empty() takes exactly 1 argument");
        llvm::Value *val = emitExpr(*e.args[0]);
        llvm::Type *headerTy = nullptr;
        if (getListElementType(val)) headerTy = listHeaderTy_;
        else if (getMapKeyType(val)) headerTy = mapHeaderTy_;
        else if (getSetElementType(val)) headerTy = setHeaderTy_;
        if (!headerTy)
            codegenError("is_empty() requires a collection (list, map, or set)");
        llvm::Value *len = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(headerTy, val, 0), "ie_len");
        return builder_.CreateICmpEQ(len, llvm::ConstantInt::get(i64Ty_, 0), "is_empty");
    }

    // ===== enumerate(list) =====
    if (e.callee == "enumerate") {
        if (e.args.size() != 1)
            codegenError("enumerate() takes exactly 1 argument");
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy) codegenError("enumerate() requires a list");

        llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(listHeaderTy_, listVal, 0), "enum_len");
        llvm::Value *srcData = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(listHeaderTy_, listVal, 2), "enum_data");

        llvm::StructType *tupleTy = llvm::StructType::get(*ctx_, {i64Ty_, elemTy});
        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "enum_header");
        uint64_t tupleSize = dl.getTypeAllocSize(tupleTy);
        llvm::Value *dataSize = builder_.CreateMul(srcLen, llvm::ConstantInt::get(i64Ty_, tupleSize), "enum_ds");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "enum_nd");

        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "enum_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "enum.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "enum.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "enum.end", fn_);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "ei");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, srcLen), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, srcData, {i}, "enum_ep");
        llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "enum_elem");
        llvm::Value *tupleVal = llvm::UndefValue::get(tupleTy);
        tupleVal = builder_.CreateInsertValue(tupleVal, i, 0);
        tupleVal = builder_.CreateInsertValue(tupleVal, elem, 1);
        llvm::Value *dstPtr = builder_.CreateGEP(tupleTy, newData, {i}, "enum_dp");
        builder_.CreateStore(tupleVal, dstPtr);
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(endBB);

        builder_.CreateStore(srcLen, builder_.CreateStructGEP(listHeaderTy_, newHeader, 0));
        builder_.CreateStore(srcLen, builder_.CreateStructGEP(listHeaderTy_, newHeader, 1));
        builder_.CreateStore(newData, builder_.CreateStructGEP(listHeaderTy_, newHeader, 2));
        list_element_types_[newHeader] = tupleTy;
        return newHeader;
    }

    // ===== zip(list1, list2) =====
    if (e.callee == "zip") {
        if (e.args.size() != 2)
            codegenError("zip() takes exactly 2 arguments");
        llvm::Value *list1 = emitExpr(*e.args[0]);
        llvm::Value *list2 = emitExpr(*e.args[1]);
        llvm::Type *elemTy1 = getListElementType(list1);
        llvm::Type *elemTy2 = getListElementType(list2);
        if (!elemTy1 || !elemTy2) codegenError("zip() requires two lists");

        llvm::Value *len1 = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(listHeaderTy_, list1, 0), "zip_len1");
        llvm::Value *len2 = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(listHeaderTy_, list2, 0), "zip_len2");
        llvm::Value *data1 = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(listHeaderTy_, list1, 2), "zip_data1");
        llvm::Value *data2 = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(listHeaderTy_, list2, 2), "zip_data2");

        llvm::Value *minLen = builder_.CreateSelect(builder_.CreateICmpSLT(len1, len2), len1, len2, "zip_minlen");

        llvm::StructType *tupleTy = llvm::StructType::get(*ctx_, {elemTy1, elemTy2});
        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "zip_header");
        uint64_t tupleSize = dl.getTypeAllocSize(tupleTy);
        llvm::Value *dataSize = builder_.CreateMul(minLen, llvm::ConstantInt::get(i64Ty_, tupleSize), "zip_ds");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "zip_nd");

        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "zip_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "zip.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "zip.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "zip.end", fn_);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "zi");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, minLen), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *ep1 = builder_.CreateGEP(elemTy1, data1, {i}, "zip_ep1");
        llvm::Value *ep2 = builder_.CreateGEP(elemTy2, data2, {i}, "zip_ep2");
        llvm::Value *e1 = builder_.CreateLoad(elemTy1, ep1, "zip_e1");
        llvm::Value *e2 = builder_.CreateLoad(elemTy2, ep2, "zip_e2");
        llvm::Value *tupleVal = llvm::UndefValue::get(tupleTy);
        tupleVal = builder_.CreateInsertValue(tupleVal, e1, 0);
        tupleVal = builder_.CreateInsertValue(tupleVal, e2, 1);
        llvm::Value *dstPtr = builder_.CreateGEP(tupleTy, newData, {i}, "zip_dp");
        builder_.CreateStore(tupleVal, dstPtr);
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(endBB);

        builder_.CreateStore(minLen, builder_.CreateStructGEP(listHeaderTy_, newHeader, 0));
        builder_.CreateStore(minLen, builder_.CreateStructGEP(listHeaderTy_, newHeader, 1));
        builder_.CreateStore(newData, builder_.CreateStructGEP(listHeaderTy_, newHeader, 2));
        list_element_types_[newHeader] = tupleTy;
        return newHeader;
    }

    return nullptr;
}

// ===== Builtin Core =====

llvm::Value *CodeGen::emitBuiltinCore(const CallExpr &e) {
    if (e.callee.size() > 8 && e.callee.substr(0, 8) == "channel<" && e.callee.back() == '>') {
        if (e.args.size() > 1)
            codegenError("channel[T]() takes 0 or 1 arguments");

        std::string inner = e.callee.substr(8, e.callee.size() - 9);
        llvm::Type *elemTy = resolveType(inner);
        llvm::Value *capacity = llvm::ConstantInt::get(i64Ty_, 0);
        if (e.args.size() == 1) {
            capacity = emitExpr(*e.args[0]);
            if (capacity->getType() != i64Ty_)
                codegenError("channel[T](capacity) requires int capacity");
        }

        const llvm::DataLayout &dl = mod_->getDataLayout();
        int64_t elemSize = elemTy->isVoidTy() ? 0 : static_cast<int64_t>(dl.getTypeAllocSize(elemTy));
        llvm::FunctionType *fnTy = llvm::FunctionType::get(ptrTy_, {i64Ty_, i64Ty_}, false);
        llvm::FunctionCallee fn = mod_->getOrInsertFunction("__ry_channel_new", fnTy);
        llvm::Value *result = builder_.CreateCall(
            fn,
            {llvm::ConstantInt::get(i64Ty_, elemSize), capacity},
            "channel");
        channel_element_types_[result] = elemTy;
        return result;
    }

    // exit(code) as expression — emit exit, then create dead block for subsequent IR
    if (e.callee == "exit") {
        emitExit(e.args);
        llvm::BasicBlock *deadBB = llvm::BasicBlock::Create(*ctx_, "exit.dead", fn_);
        builder_.SetInsertPoint(deadBB);
        return llvm::UndefValue::get(i64Ty_);
    }

    // args() → List<str>
    if (e.callee == "args") {
        if (!e.args.empty())
            codegenError("args() takes no arguments");

        // Call __ry_args_count()
        llvm::FunctionType *countTy = llvm::FunctionType::get(i32Ty_, false);
        llvm::FunctionCallee countFn = mod_->getOrInsertFunction("__ry_args_count", countTy);
        llvm::Value *count32 = builder_.CreateCall(countFn, {}, "argc");
        llvm::Value *count = builder_.CreateSExt(count32, i64Ty_, "argc64");

        // Allocate list header
        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        llvm::Value *headerPtr = builder_.CreateCall(
            mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "args_header");

        // Allocate data array (ptr per element)
        uint64_t elemSize = dl.getTypeAllocSize(ptrTy_);
        llvm::Value *dataSize = builder_.CreateMul(count, llvm::ConstantInt::get(i64Ty_, elemSize), "args_data_size");
        llvm::Value *dataPtr = builder_.CreateCall(mallocFn, {dataSize}, "args_data");

        // Loop: for i in 0..count, get arg pointer
        llvm::Value *zero = llvm::ConstantInt::get(i64Ty_, 0);
        llvm::Value *one = llvm::ConstantInt::get(i64Ty_, 1);
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "args_i");
        builder_.CreateStore(zero, iVar);

        // __ry_args_get function type
        llvm::FunctionType *getTy = llvm::FunctionType::get(ptrTy_, {i32Ty_}, false);
        llvm::FunctionCallee getFn = mod_->getOrInsertFunction("__ry_args_get", getTy);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "args.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "args.body", fn_);
        llvm::BasicBlock *endBB  = llvm::BasicBlock::Create(*ctx_, "args.end", fn_);

        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(condBB);
        llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "ai");
        llvm::Value *cond = builder_.CreateICmpSLT(iVal, count, "args_cond");
        builder_.CreateCondBr(cond, bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "ai_cur");
        llvm::Value *iCur32 = builder_.CreateTrunc(iCur, i32Ty_, "ai_cur32");
        llvm::Value *argStr = builder_.CreateCall(getFn, {iCur32}, "arg_str");
        llvm::Value *elemPtr = builder_.CreateGEP(ptrTy_, dataPtr, {iCur}, "args_elem_ptr");
        builder_.CreateStore(argStr, elemPtr);
        llvm::Value *iNext = builder_.CreateAdd(iCur, one, "ai_next");
        builder_.CreateStore(iNext, iVar);
        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(endBB);

        // Store header fields
        llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 0, "args_len_ptr");
        builder_.CreateStore(count, lenPtr);
        llvm::Value *capPtr = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 1, "args_cap_ptr");
        builder_.CreateStore(count, capPtr);
        llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 2, "args_data_field");
        builder_.CreateStore(dataPtr, dataPtrField);

        list_element_types_[headerPtr] = ptrTy_;
        return headerPtr;
    }

    // available_parallelism() -> int
    if (e.callee == "available_parallelism") {
        if (!e.args.empty())
            codegenError("available_parallelism() takes no arguments");

        llvm::FunctionType *fnTy = llvm::FunctionType::get(i64Ty_, false);
        llvm::FunctionCallee fn = mod_->getOrInsertFunction("__ry_available_parallelism", fnTy);
        return builder_.CreateCall(fn, {}, "available_parallelism");
    }

    if (e.callee == "join" && e.args.size() == 1) {
        llvm::Value *taskVal = emitExpr(*e.args[0]);
        llvm::Type *resultTy = getTaskResultType(taskVal);
        if (!resultTy)
            codegenError("join() requires Task<T>");

        llvm::FunctionType *joinTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*ctx_), {ptrTy_, ptrTy_}, false);
        llvm::FunctionCallee joinFn = mod_->getOrInsertFunction("__ry_task_join", joinTy);
        if (resultTy->isVoidTy())
            return builder_.CreateCall(joinFn, {taskVal, llvm::ConstantPointerNull::get(
                llvm::cast<llvm::PointerType>(ptrTy_))});
        llvm::AllocaInst *resultSlot = builder_.CreateAlloca(resultTy, nullptr, "join_result");
        builder_.CreateCall(joinFn, {taskVal, resultSlot});
        return builder_.CreateLoad(resultTy, resultSlot, "joined");
    }

    if (e.callee == "send") {
        if (e.args.size() != 2)
            codegenError("send() takes exactly 2 arguments");
        llvm::Value *channelVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getChannelElementType(channelVal);
        if (!elemTy)
            codegenError("send() requires Channel<T> as first argument");
        llvm::Value *valueVal = emitExpr(*e.args[1]);
        if (valueVal->getType() != elemTy)
            codegenError("send() value type does not match channel element type");

        llvm::Value *valuePtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));
        if (!elemTy->isVoidTy()) {
            llvm::AllocaInst *valueSlot = builder_.CreateAlloca(elemTy, nullptr, "send_value");
            builder_.CreateStore(valueVal, valueSlot);
            valuePtr = valueSlot;
        }

        llvm::FunctionType *fnTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*ctx_), {ptrTy_, ptrTy_}, false);
        llvm::FunctionCallee fn = mod_->getOrInsertFunction("__ry_channel_send", fnTy);
        return builder_.CreateCall(fn, {channelVal, valuePtr});
    }

    if (e.callee == "try_send") {
        if (e.args.size() != 2)
            codegenError("try_send() takes exactly 2 arguments");
        llvm::Value *channelVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getChannelElementType(channelVal);
        if (!elemTy)
            codegenError("try_send() requires Channel<T> as first argument");
        llvm::Value *valueVal = emitExpr(*e.args[1]);
        if (valueVal->getType() != elemTy)
            codegenError("try_send() value type does not match channel element type");

        llvm::Value *valuePtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));
        if (!elemTy->isVoidTy()) {
            llvm::AllocaInst *valueSlot = builder_.CreateAlloca(elemTy, nullptr, "try_send_value");
            builder_.CreateStore(valueVal, valueSlot);
            valuePtr = valueSlot;
        }

        llvm::FunctionType *fnTy = llvm::FunctionType::get(i1Ty_, {ptrTy_, ptrTy_}, false);
        llvm::FunctionCallee fn = mod_->getOrInsertFunction("__ry_channel_try_send", fnTy);
        return builder_.CreateCall(fn, {channelVal, valuePtr}, "try_send_ok");
    }

    if (e.callee == "recv") {
        if (e.args.size() != 1)
            codegenError("recv() takes exactly 1 argument");
        llvm::Value *channelVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getChannelElementType(channelVal);
        if (!elemTy)
            codegenError("recv() requires Channel<T> argument");

        llvm::FunctionType *fnTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*ctx_), {ptrTy_, ptrTy_}, false);
        llvm::FunctionCallee fn = mod_->getOrInsertFunction("__ry_channel_recv", fnTy);
        if (elemTy->isVoidTy()) {
            return builder_.CreateCall(fn, {
                channelVal,
                llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_))
            });
        }

        llvm::AllocaInst *resultSlot = builder_.CreateAlloca(elemTy, nullptr, "recv_result");
        builder_.CreateCall(fn, {channelVal, resultSlot});
        return builder_.CreateLoad(elemTy, resultSlot, "received");
    }

    if (e.callee == "recv_opt") {
        if (e.args.size() != 1)
            codegenError("recv_opt() takes exactly 1 argument");
        llvm::Value *channelVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getChannelElementType(channelVal);
        if (!elemTy)
            codegenError("recv_opt() requires Channel<T> argument");

        llvm::FunctionType *fnTy = llvm::FunctionType::get(i1Ty_, {ptrTy_, ptrTy_}, false);
        llvm::FunctionCallee fn = mod_->getOrInsertFunction("__ry_channel_recv_opt", fnTy);
        llvm::Value *outPtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));
        llvm::AllocaInst *resultSlot = nullptr;
        if (!elemTy->isVoidTy()) {
            resultSlot = builder_.CreateAlloca(elemTy, nullptr, "recv_opt_result");
            builder_.CreateStore(llvm::Constant::getNullValue(elemTy), resultSlot);
            outPtr = resultSlot;
        }

        llvm::Value *hasValue = builder_.CreateCall(fn, {channelVal, outPtr}, "recv_opt_has_value");
        if (elemTy->isVoidTy())
            return hasValue;

        llvm::StructType *optTy = getOptionType(elemTy);
        llvm::Value *inner = builder_.CreateLoad(elemTy, resultSlot, "recv_opt_loaded");
        llvm::Value *optInner = builder_.CreateSelect(hasValue, inner, llvm::UndefValue::get(elemTy), "recv_opt_inner");
        llvm::Value *opt = llvm::UndefValue::get(optTy);
        opt = builder_.CreateInsertValue(opt, hasValue, 0, "recv_opt_has");
        opt = builder_.CreateInsertValue(opt, optInner, 1, "recv_opt_value");
        return opt;
    }

    if (e.callee == "try_recv") {
        if (e.args.size() != 1)
            codegenError("try_recv() takes exactly 1 argument");
        llvm::Value *channelVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getChannelElementType(channelVal);
        if (!elemTy)
            codegenError("try_recv() requires Channel<T> argument");

        llvm::FunctionType *fnTy = llvm::FunctionType::get(i1Ty_, {ptrTy_, ptrTy_}, false);
        llvm::FunctionCallee fn = mod_->getOrInsertFunction("__ry_channel_try_recv", fnTy);
        llvm::Value *outPtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));
        llvm::AllocaInst *resultSlot = nullptr;
        if (!elemTy->isVoidTy()) {
            resultSlot = builder_.CreateAlloca(elemTy, nullptr, "try_recv_result");
            builder_.CreateStore(llvm::Constant::getNullValue(elemTy), resultSlot);
            outPtr = resultSlot;
        }

        llvm::Value *hasValue = builder_.CreateCall(fn, {channelVal, outPtr}, "try_recv_has_value");
        if (elemTy->isVoidTy())
            return hasValue;

        llvm::StructType *optTy = getOptionType(elemTy);
        llvm::Value *inner = builder_.CreateLoad(elemTy, resultSlot, "try_recv_loaded");
        llvm::Value *optInner = builder_.CreateSelect(hasValue, inner, llvm::UndefValue::get(elemTy), "try_recv_inner");
        llvm::Value *opt = llvm::UndefValue::get(optTy);
        opt = builder_.CreateInsertValue(opt, hasValue, 0, "try_recv_has");
        opt = builder_.CreateInsertValue(opt, optInner, 1, "try_recv_value");
        return opt;
    }

    if (e.callee == "close") {
        if (e.args.size() != 1)
            codegenError("close() takes exactly 1 argument");
        llvm::Value *channelVal = emitExpr(*e.args[0]);
        if (!getChannelElementType(channelVal))
            codegenError("close() requires Channel<T> argument");

        llvm::FunctionType *fnTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
        llvm::FunctionCallee fn = mod_->getOrInsertFunction("__ry_channel_close", fnTy);
        return builder_.CreateCall(fn, {channelVal});
    }

    // range(n), range(start, end), or range(start, end, step) → List<int>
    if (e.callee == "range") {
        if (e.args.size() < 1 || e.args.size() > 3)
            codegenError("range() takes 1, 2, or 3 arguments");

        llvm::Value *start, *end, *step;
        llvm::Value *zero = llvm::ConstantInt::get(i64Ty_, 0);
        llvm::Value *one = llvm::ConstantInt::get(i64Ty_, 1);

        if (e.args.size() == 1) {
            start = zero;
            end = emitExpr(*e.args[0]);
            step = one;
        } else if (e.args.size() == 2) {
            start = emitExpr(*e.args[0]);
            end = emitExpr(*e.args[1]);
            step = one;
        } else {
            start = emitExpr(*e.args[0]);
            end = emitExpr(*e.args[1]);
            step = emitExpr(*e.args[2]);
        }

        // Runtime check: step == 0 → error
        if (e.args.size() == 3) {
            llvm::Value *stepZero = builder_.CreateICmpEQ(step, zero, "step_zero");
            llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, "range.step_err", fn_);
            llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "range.step_ok", fn_);
            builder_.CreateCondBr(stepZero, errBB, okBB);
            builder_.SetInsertPoint(errBB);
            emitRuntimeError("runtime error: range() step must not be zero\n", ".range_step_err");
            builder_.SetInsertPoint(okBB);
        }

        // Compute count based on step sign
        // step > 0: count = max(0, (end - start + step - 1) / step)
        // step < 0: count = max(0, (start - end + (-step) - 1) / (-step))
        llvm::Value *stepPos = builder_.CreateICmpSGT(step, zero, "step_pos");

        // Positive step case
        llvm::Value *diffPos = builder_.CreateSub(end, start, "diff_pos");
        llvm::Value *numPos = builder_.CreateAdd(diffPos, builder_.CreateSub(step, one, "step_m1"), "num_pos");
        llvm::Value *countPos = builder_.CreateSDiv(numPos, step, "count_pos");
        llvm::Value *countPosClamped = builder_.CreateSelect(
            builder_.CreateICmpSGT(countPos, zero, "pos_gt0"), countPos, zero, "count_pos_c");

        // Negative step case
        llvm::Value *negStep = builder_.CreateNeg(step, "neg_step");
        llvm::Value *diffNeg = builder_.CreateSub(start, end, "diff_neg");
        llvm::Value *numNeg = builder_.CreateAdd(diffNeg, builder_.CreateSub(negStep, one, "negstep_m1"), "num_neg");
        llvm::Value *countNeg = builder_.CreateSDiv(numNeg, negStep, "count_neg");
        llvm::Value *countNegClamped = builder_.CreateSelect(
            builder_.CreateICmpSGT(countNeg, zero, "neg_gt0"), countNeg, zero, "count_neg_c");

        llvm::Value *count = builder_.CreateSelect(stepPos, countPosClamped, countNegClamped, "range_count");

        // Allocate list header
        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        llvm::Value *headerPtr = builder_.CreateCall(
            mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "range_header");

        // Allocate data array
        uint64_t elemSize = dl.getTypeAllocSize(i64Ty_);
        llvm::Value *dataSize = builder_.CreateMul(count, llvm::ConstantInt::get(i64Ty_, elemSize), "range_data_size");
        llvm::Value *dataPtr = builder_.CreateCall(mallocFn, {dataSize}, "range_data");

        // Fill data with start, start+step, start+2*step, ... using a loop
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "range_i");
        builder_.CreateStore(zero, iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "range.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "range.body", fn_);
        llvm::BasicBlock *endBB  = llvm::BasicBlock::Create(*ctx_, "range.end", fn_);

        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(condBB);
        llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "ri");
        llvm::Value *cond = builder_.CreateICmpSLT(iVal, count, "range_cond");
        builder_.CreateCondBr(cond, bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "ri_cur");
        llvm::Value *offset = builder_.CreateMul(iCur, step, "range_offset");
        llvm::Value *val = builder_.CreateAdd(start, offset, "range_val");
        llvm::Value *elemPtr = builder_.CreateGEP(i64Ty_, dataPtr, {iCur}, "range_elem_ptr");
        builder_.CreateStore(val, elemPtr);
        llvm::Value *iNext = builder_.CreateAdd(iCur, one, "ri_next");
        builder_.CreateStore(iNext, iVar);
        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(endBB);

        // Store header fields
        llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 0, "range_len_ptr");
        builder_.CreateStore(count, lenPtr);
        llvm::Value *capPtr = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 1, "range_cap_ptr");
        builder_.CreateStore(count, capPtr);
        llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 2, "range_data_field");
        builder_.CreateStore(dataPtr, dataPtrField);

        list_element_types_[headerPtr] = i64Ty_;
        return headerPtr;
    }

    // len(xs) → list/map length
    if (e.callee == "len") {
        if (e.args.size() != 1)
            codegenError("len() takes exactly 1 argument");
        llvm::Value *ptr = emitExpr(*e.args[0]);
        if (ptr->getType() != ptrTy_)
            codegenError("len() requires list, map, or str argument");
        // Check if it's a set
        if (getSetElementType(ptr)) {
            llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, ptr, 0, "set_len_ptr");
            return builder_.CreateLoad(i64Ty_, lenPtr, "set_len");
        }
        // Check if it's a map
        llvm::Type *mapKeyTy = getMapKeyType(ptr);
        if (mapKeyTy) {
            llvm::Value *lenPtr = builder_.CreateStructGEP(mapHeaderTy_, ptr, 0, "map_len_ptr");
            return builder_.CreateLoad(i64Ty_, lenPtr, "map_len");
        }
        // Check if it's a list
        if (getListElementType(ptr)) {
            llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, ptr, 0, "len_ptr");
            return builder_.CreateLoad(i64Ty_, lenPtr, "len");
        }
        // String: call __ry_utf8_len (character count)
        auto utf8LenTy = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
        auto utf8LenFn = mod_->getOrInsertFunction("__ry_utf8_len", utf8LenTy);
        return builder_.CreateCall(utf8LenFn, {ptr}, "str_len");
    }

    // byte_len(str) → int (byte length)
    if (e.callee == "byte_len") {
        if (e.args.size() != 1)
            codegenError("byte_len() takes exactly 1 argument");
        llvm::Value *ptr = emitExpr(*e.args[0]);
        if (ptr->getType() != ptrTy_)
            codegenError("byte_len() requires str argument");
        auto strlenFn = getStdlibStrlen();
        return builder_.CreateCall(strlenFn, {ptr}, "byte_len");
    }

    // Some(x) → Option<T> constructor
    if (e.callee == "Some") {
        if (e.args.size() != 1)
            codegenError("Some() takes exactly 1 argument");
        llvm::Value *inner = emitExpr(*e.args[0]);
        llvm::StructType *optTy = getOptionType(inner->getType());
        return buildSomeValue(inner, optTy);
    }

    // Error("msg") / Error("msg", code) → Error struct constructor
    if (e.callee == "Error") {
        if (e.args.empty() || e.args.size() > 2)
            codegenError("Error() takes 1 or 2 arguments");
        llvm::Value *msg = emitExpr(*e.args[0]);
        if (msg->getType() != ptrTy_)
            codegenError("Error() first argument must be a string");
        llvm::Value *code;
        if (e.args.size() == 2) {
            code = emitExpr(*e.args[1]);
            if (code->getType() != i64Ty_)
                codegenError("Error() second argument must be an integer");
        } else {
            code = llvm::ConstantInt::get(i64Ty_, 0);
        }
        llvm::Value *result = llvm::UndefValue::get(errorTy_);
        result = builder_.CreateInsertValue(result, msg, 0, "err.msg");
        result = builder_.CreateInsertValue(result, code, 1, "err.code");
        return result;
    }

    // unwrap() has been removed — use match or ?? instead
    if (e.callee == "unwrap") {
        codegenError("unwrap() has been removed. Use match or ?? instead");
    }

    // has_key(map, key) → bool
    if (e.callee == "has_key") {
        if (e.args.size() != 2)
            codegenError("has_key() takes exactly 2 arguments");
        llvm::Value *mapPtr = emitExpr(*e.args[0]);
        if (mapPtr->getType() != ptrTy_)
            codegenError("has_key() requires map as first argument");
        llvm::Type *keyTy = getMapKeyType(mapPtr);
        if (!keyTy)
            codegenError("has_key() requires map as first argument");
        llvm::Value *key = emitExpr(*e.args[1]);
        if (key->getType() != keyTy)
            codegenError("has_key() key type mismatch");
        llvm::Value *idx = emitMapKeyLookup(mapPtr, key, keyTy);
        return builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "has_key");
    }


    return nullptr;
}

// ===== Builtin Higher-Order =====

llvm::Value *CodeGen::emitBuiltinHigherOrder(const CallExpr &e) {
    // filter(list, predicate) → new list with elements matching predicate
    if (e.callee == "filter") {
        if (e.args.size() != 2)
            codegenError("filter() takes exactly 2 arguments");

        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Value *lambdaVal = emitExpr(*e.args[1]);

        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy)
            codegenError("filter() requires a list as first argument");

        // Get lambda type info (handle LoadInst for variable-passed functions)
        auto fnIt = fn_type_info_.find(lambdaVal);
        if (fnIt == fn_type_info_.end()) {
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(lambdaVal))
                fnIt = fn_type_info_.find(load->getPointerOperand());
        }
        if (fnIt == fn_type_info_.end())
            codegenError("filter() requires a function as second argument");
        auto &info = fnIt->second;

        if (info.paramTypes.size() != 1 || info.returnType != i1Ty_)
            codegenError("filter() predicate must take 1 argument and return bool");

        // Read source list
        llvm::Value *srcLenPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 0, "filter_src_len_ptr");
        llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, srcLenPtr, "filter_src_len");
        llvm::Value *srcDataPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 2, "filter_src_data_field");
        llvm::Value *srcData = builder_.CreateLoad(ptrTy_, srcDataPtr, "filter_src_data");

        // Allocate new list header + data (capacity = source length)
        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        llvm::Value *newHeader = builder_.CreateCall(
            mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "filter_header");

        uint64_t elemSize = dl.getTypeAllocSize(elemTy);
        llvm::Value *dataSize = builder_.CreateMul(srcLen, llvm::ConstantInt::get(i64Ty_, elemSize), "filter_data_size");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "filter_data");

        // Set up data pointer in header
        llvm::Value *newDataField = builder_.CreateStructGEP(listHeaderTy_, newHeader, 2, "filter_data_field");
        builder_.CreateStore(newData, newDataField);
        llvm::Value *newCapPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 1, "filter_cap_ptr");
        builder_.CreateStore(srcLen, newCapPtr);

        // Loop counter and output counter
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "filter_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
        llvm::AllocaInst *outVar = builder_.CreateAlloca(i64Ty_, nullptr, "filter_out");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), outVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "filter.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "filter.body", fn_);
        llvm::BasicBlock *storeBB = llvm::BasicBlock::Create(*ctx_, "filter.store", fn_);
        llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "filter.next", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "filter.end", fn_);

        builder_.CreateBr(condBB);

        // Condition: i < srcLen
        builder_.SetInsertPoint(condBB);
        llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "fi");
        llvm::Value *cond = builder_.CreateICmpSLT(iVal, srcLen, "filter_cond");
        builder_.CreateCondBr(cond, bodyBB, endBB);

        // Body: load element, call predicate
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "fi_cur");
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, srcData, {iCur}, "filter_elem_ptr");
        llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "filter_elem");
        llvm::Value *pred = emitLambdaCall(lambdaVal, info, {elem}, "filter_pred");
        builder_.CreateCondBr(pred, storeBB, nextBB);

        // Store: add element to output
        builder_.SetInsertPoint(storeBB);
        llvm::Value *outIdx = builder_.CreateLoad(i64Ty_, outVar, "filter_out_idx");
        llvm::Value *dstPtr = builder_.CreateGEP(elemTy, newData, {outIdx}, "filter_dst_ptr");
        builder_.CreateStore(elem, dstPtr);
        llvm::Value *outNext = builder_.CreateAdd(outIdx, llvm::ConstantInt::get(i64Ty_, 1), "filter_out_next");
        builder_.CreateStore(outNext, outVar);
        builder_.CreateBr(nextBB);

        // Next: increment i
        builder_.SetInsertPoint(nextBB);
        llvm::Value *iCur2 = builder_.CreateLoad(i64Ty_, iVar, "fi_cur2");
        llvm::Value *iNext = builder_.CreateAdd(iCur2, llvm::ConstantInt::get(i64Ty_, 1), "fi_next");
        builder_.CreateStore(iNext, iVar);
        builder_.CreateBr(condBB);

        // End: set final length
        builder_.SetInsertPoint(endBB);
        llvm::Value *finalLen = builder_.CreateLoad(i64Ty_, outVar, "filter_final_len");
        llvm::Value *newLenPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 0, "filter_len_ptr");
        builder_.CreateStore(finalLen, newLenPtr);

        list_element_types_[newHeader] = elemTy;
        return newHeader;
    }

    // map(list, transform) → new list with transformed elements
    if (e.callee == "map") {
        if (e.args.size() != 2)
            codegenError("map() takes exactly 2 arguments");

        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Value *lambdaVal = emitExpr(*e.args[1]);

        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy)
            codegenError("map() requires a list as first argument");

        auto fnIt = fn_type_info_.find(lambdaVal);
        if (fnIt == fn_type_info_.end()) {
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(lambdaVal))
                fnIt = fn_type_info_.find(load->getPointerOperand());
        }
        if (fnIt == fn_type_info_.end())
            codegenError("map() requires a function as second argument");
        auto &info = fnIt->second;

        if (info.paramTypes.size() != 1)
            codegenError("map() transform must take exactly 1 argument");

        llvm::Type *outElemTy = info.returnType;

        // Read source list
        llvm::Value *srcLenPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 0, "map_src_len_ptr");
        llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, srcLenPtr, "map_src_len");
        llvm::Value *srcDataPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 2, "map_src_data_field");
        llvm::Value *srcData = builder_.CreateLoad(ptrTy_, srcDataPtr, "map_src_data");

        // Allocate new list
        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        llvm::Value *newHeader = builder_.CreateCall(
            mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "map_header");

        uint64_t outElemSize = dl.getTypeAllocSize(outElemTy);
        llvm::Value *dataSize = builder_.CreateMul(srcLen, llvm::ConstantInt::get(i64Ty_, outElemSize), "map_data_size");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "map_data");

        // Set header fields
        llvm::Value *newLenPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 0, "map_len_ptr");
        builder_.CreateStore(srcLen, newLenPtr);
        llvm::Value *newCapPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 1, "map_cap_ptr");
        builder_.CreateStore(srcLen, newCapPtr);
        llvm::Value *newDataField = builder_.CreateStructGEP(listHeaderTy_, newHeader, 2, "map_data_field");
        builder_.CreateStore(newData, newDataField);

        // Loop: transform each element
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "map_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "map.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "map.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "map.end", fn_);

        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(condBB);
        llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "mi");
        llvm::Value *cond = builder_.CreateICmpSLT(iVal, srcLen, "map_cond");
        builder_.CreateCondBr(cond, bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "mi_cur");
        llvm::Value *srcElemPtr = builder_.CreateGEP(elemTy, srcData, {iCur}, "map_src_elem_ptr");
        llvm::Value *srcElem = builder_.CreateLoad(elemTy, srcElemPtr, "map_src_elem");
        llvm::Value *mapped = emitLambdaCall(lambdaVal, info, {srcElem}, "map_result");
        llvm::Value *dstElemPtr = builder_.CreateGEP(outElemTy, newData, {iCur}, "map_dst_elem_ptr");
        builder_.CreateStore(mapped, dstElemPtr);
        llvm::Value *iNext = builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1), "mi_next");
        builder_.CreateStore(iNext, iVar);
        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(endBB);
        list_element_types_[newHeader] = outElemTy;
        return newHeader;
    }

    // sort(list) or sort(list, comparator) → new sorted list
    if (e.callee == "sort") {
        if (e.args.size() < 1 || e.args.size() > 2)
            codegenError("sort() takes 1 or 2 arguments");

        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy)
            codegenError("sort() requires a list as first argument");

        bool hasComparator = (e.args.size() == 2);
        llvm::Value *compVal = nullptr;
        FnTypeInfo compInfo;
        if (hasComparator) {
            compVal = emitExpr(*e.args[1]);
            auto fnIt = fn_type_info_.find(compVal);
            if (fnIt == fn_type_info_.end()) {
                if (auto *load = llvm::dyn_cast<llvm::LoadInst>(compVal))
                    fnIt = fn_type_info_.find(load->getPointerOperand());
            }
            if (fnIt == fn_type_info_.end())
                codegenError("sort() comparator must be a function");
            compInfo = fnIt->second;
            if (compInfo.paramTypes.size() != 2 || compInfo.returnType != i1Ty_)
                codegenError("sort() comparator must take 2 arguments and return bool");
        }

        // Read source list
        llvm::Value *srcLenPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 0, "sort_src_len_ptr");
        llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, srcLenPtr, "sort_src_len");
        llvm::Value *srcDataPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 2, "sort_src_data_field");
        llvm::Value *srcData = builder_.CreateLoad(ptrTy_, srcDataPtr, "sort_src_data");

        // Allocate new list and copy data
        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        llvm::Value *newHeader = builder_.CreateCall(
            mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "sort_header");

        uint64_t elemSz = dl.getTypeAllocSize(elemTy);
        llvm::Value *dataSize = builder_.CreateMul(srcLen, llvm::ConstantInt::get(i64Ty_, elemSz), "sort_data_size");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "sort_data");

        // memcpy source data to new data
        auto memcpyFn = getStdlibMemcpy();
        builder_.CreateCall(memcpyFn, {newData, srcData, dataSize});

        // Set header
        llvm::Value *newLenPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 0, "sort_len_ptr");
        builder_.CreateStore(srcLen, newLenPtr);
        llvm::Value *newCapPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 1, "sort_cap_ptr");
        builder_.CreateStore(srcLen, newCapPtr);
        llvm::Value *newDataField = builder_.CreateStructGEP(listHeaderTy_, newHeader, 2, "sort_data_field");
        builder_.CreateStore(newData, newDataField);

        // Generate trampoline function for TimSort comparator
        std::string trampName = "__sort_trampoline_" + std::to_string(lambda_counter_++);
        llvm::FunctionType *trampTy = llvm::FunctionType::get(
            i1Ty_, {ptrTy_, ptrTy_, ptrTy_}, false);
        llvm::Function *trampFn = llvm::Function::Create(
            trampTy, llvm::Function::ExternalLinkage, trampName, mod_.get());
        trampFn->setCallingConv(llvm::CallingConv::C);

        auto trampArgs = trampFn->arg_begin();
        llvm::Argument *argA = &*trampArgs++;
        llvm::Argument *argB = &*trampArgs++;
        llvm::Argument *argCtx = &*trampArgs++;
        argA->setName("a_ptr");
        argB->setName("b_ptr");
        argCtx->setName("ctx");

        {
            FnScope guard(*this);
            fn_ = trampFn;
            llvm::BasicBlock *trampBB = llvm::BasicBlock::Create(*ctx_, "entry", trampFn);
            builder_.SetInsertPoint(trampBB);

            llvm::Value *valA = builder_.CreateLoad(elemTy, argA, "val_a");
            llvm::Value *valB = builder_.CreateLoad(elemTy, argB, "val_b");

            llvm::Value *result;
            if (hasComparator) {
                // ctx is the closure struct pointer (or raw fn ptr for non-closures)
                result = emitLambdaCall(argCtx, compInfo, {valA, valB}, "sort_comp");
            } else if (elemTy == i64Ty_) {
                result = builder_.CreateICmpSLT(valA, valB, "sort_lt");
            } else if (elemTy == f64Ty_) {
                result = builder_.CreateFCmpOLT(valA, valB, "sort_lt");
            } else if (elemTy == ptrTy_) {
                auto strcmpFn = getStdlibStrcmp();
                llvm::Value *cmpResult = builder_.CreateCall(strcmpFn, {valA, valB}, "sort_strcmp");
                result = builder_.CreateICmpSLT(cmpResult, llvm::ConstantInt::get(i32Ty_, 0), "sort_lt");
            } else {
                codegenError("sort() does not support this element type");
            }

            builder_.CreateRet(result);
        }

        // Call __ry_timsort(newData, srcLen, elemSize, trampoline, cmpCtx)
        llvm::Value *elemSizeConst = llvm::ConstantInt::get(i64Ty_, elemSz);
        llvm::Value *cmpCtx = hasComparator
            ? compVal
            : llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));

        auto timsortTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*ctx_), {ptrTy_, i64Ty_, i64Ty_, ptrTy_, ptrTy_}, false);
        auto timsortFn = mod_->getOrInsertFunction("__ry_timsort", timsortTy);
        builder_.CreateCall(timsortFn, {newData, srcLen, elemSizeConst, trampFn, cmpCtx});

        // Return sorted list
        list_element_types_[newHeader] = elemTy;
        return newHeader;
    }

    // sort!(list) / sort!(list, comparator) → in-place sort
    if (e.callee == "sort!") {
        if (e.args.size() < 1 || e.args.size() > 2)
            codegenError("sort!() takes 1 or 2 arguments");

        // Evaluate list arg once before sort() re-evaluates it
        llvm::Value *listPtr = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listPtr);
        if (!elemTy) codegenError("sort!() requires a list");

        // Reuse sort() via a proxy CallExpr with scope guard for exception safety
        auto &mutArgs = const_cast<CallExpr &>(e).args;
        CallExpr sortProxy;
        sortProxy.callee = "sort";
        sortProxy.args = std::move(mutArgs);
        ArgsRestoreGuard guard{mutArgs, sortProxy.args};
        llvm::Value *sorted = emitBuiltinHigherOrder(sortProxy);

        if (!sorted)
            codegenError("sort!() internal error");

        // Copy sorted data back into original list
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t elemSize = dl.getTypeAllocSize(elemTy);
        auto memcpyFn = getStdlibMemcpy();

        llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(listHeaderTy_, listPtr, 0), "sortm_len");
        llvm::Value *srcData = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(listHeaderTy_, listPtr, 2), "sortm_data");
        llvm::Value *sortedData = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(listHeaderTy_, sorted, 2), "sortm_sorted");
        llvm::Value *copySize = builder_.CreateMul(srcLen, llvm::ConstantInt::get(i64Ty_, elemSize), "sortm_sz");
        builder_.CreateCall(memcpyFn, {srcData, sortedData, copySize});

        // Free the temporary sorted list
        auto freeFn = getStdlibFree();
        builder_.CreateCall(freeFn, {sortedData});
        builder_.CreateCall(freeFn, {sorted});

        return llvm::ConstantInt::get(i64Ty_, 0);
    }

    // ===== reduce(list, fn(a, b) -> a op b) =====
    if (e.callee == "reduce") {
        if (e.args.size() != 2)
            codegenError("reduce() takes exactly 2 arguments");
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Value *lambdaVal = emitExpr(*e.args[1]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy) codegenError("reduce() requires a list");
        auto fnIt = fn_type_info_.find(lambdaVal);
        if (fnIt == fn_type_info_.end())
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(lambdaVal))
                fnIt = fn_type_info_.find(load->getPointerOperand());
        if (fnIt == fn_type_info_.end())
            codegenError("reduce() requires a function");
        auto &info = fnIt->second;

        llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(listHeaderTy_, listVal, 0), "reduce_len");
        llvm::Value *srcData = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(listHeaderTy_, listVal, 2), "reduce_data");

        // Check empty list
        llvm::Value *isEmptyR = builder_.CreateICmpEQ(srcLen, llvm::ConstantInt::get(i64Ty_, 0), "reduce_empty");
        llvm::BasicBlock *errBBR = llvm::BasicBlock::Create(*ctx_, "reduce.err", fn_);
        llvm::BasicBlock *okBBR = llvm::BasicBlock::Create(*ctx_, "reduce.ok", fn_);
        builder_.CreateCondBr(isEmptyR, errBBR, okBBR);
        builder_.SetInsertPoint(errBBR);
        emitRuntimeError("runtime error: reduce() on empty list\n", ".reduce_empty_err");
        builder_.SetInsertPoint(okBBR);

        // acc = list[0]
        llvm::Value *first = builder_.CreateLoad(elemTy, srcData, "reduce_first");
        llvm::AllocaInst *accVar = builder_.CreateAlloca(info.returnType, nullptr, "reduce_acc");
        builder_.CreateStore(first, accVar);
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "reduce_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 1), iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "reduce.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "reduce.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "reduce.end", fn_);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "ri");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, srcLen), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, srcData, {i}, "reduce_ep");
        llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "reduce_elem");
        llvm::Value *acc = builder_.CreateLoad(info.returnType, accVar, "reduce_acc_val");
        llvm::Value *result = emitLambdaCall(lambdaVal, info, {acc, elem}, "reduce_call");
        builder_.CreateStore(result, accVar);
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(endBB);
        return builder_.CreateLoad(info.returnType, accVar, "reduce_result");
    }

    // ===== fold(list, init, fn(a, b) -> a op b) =====
    if (e.callee == "fold") {
        if (e.args.size() != 3)
            codegenError("fold() takes exactly 3 arguments");
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Value *initVal = emitExpr(*e.args[1]);
        llvm::Value *lambdaVal = emitExpr(*e.args[2]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy) codegenError("fold() requires a list");
        auto fnIt = fn_type_info_.find(lambdaVal);
        if (fnIt == fn_type_info_.end())
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(lambdaVal))
                fnIt = fn_type_info_.find(load->getPointerOperand());
        if (fnIt == fn_type_info_.end())
            codegenError("fold() requires a function");
        auto &info = fnIt->second;
        if (info.paramTypes.size() != 2)
            codegenError("fold() function must take 2 parameters (accumulator, element)");
        if (info.returnType != initVal->getType())
            codegenError("fold() initial value type must match function return type");

        llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(listHeaderTy_, listVal, 0), "fold_len");
        llvm::Value *srcData = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(listHeaderTy_, listVal, 2), "fold_data");

        llvm::AllocaInst *accVar = builder_.CreateAlloca(info.returnType, nullptr, "fold_acc");
        builder_.CreateStore(initVal, accVar);
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "fold_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "fold.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "fold.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "fold.end", fn_);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "fi");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, srcLen), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, srcData, {i}, "fold_ep");
        llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "fold_elem");
        llvm::Value *acc = builder_.CreateLoad(info.returnType, accVar, "fold_acc_val");
        llvm::Value *result = emitLambdaCall(lambdaVal, info, {acc, elem}, "fold_call");
        builder_.CreateStore(result, accVar);
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(endBB);
        return builder_.CreateLoad(info.returnType, accVar, "fold_result");
    }

    // ===== any(list, pred) =====
    if (e.callee == "any") {
        if (e.args.size() != 2)
            codegenError("any() takes exactly 2 arguments");
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Value *lambdaVal = emitExpr(*e.args[1]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy) codegenError("any() requires a list");
        auto fnIt = fn_type_info_.find(lambdaVal);
        if (fnIt == fn_type_info_.end())
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(lambdaVal))
                fnIt = fn_type_info_.find(load->getPointerOperand());
        if (fnIt == fn_type_info_.end())
            codegenError("any() requires a function");
        auto &info = fnIt->second;
        if (info.paramTypes.size() != 1)
            codegenError("any() predicate must take 1 parameter");
        if (info.returnType != i1Ty_)
            codegenError("any() predicate must return bool");

        llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(listHeaderTy_, listVal, 0), "any_len");
        llvm::Value *srcData = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(listHeaderTy_, listVal, 2), "any_data");

        llvm::AllocaInst *resultVar = builder_.CreateAlloca(i1Ty_, nullptr, "any_result");
        builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 0), resultVar);
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "any_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "any.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "any.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "any.end", fn_);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "ai");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, srcLen), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, srcData, {i}, "any_ep");
        llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "any_elem");
        llvm::Value *pred = emitLambdaCall(lambdaVal, info, {elem}, "any_pred");
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        llvm::BasicBlock *foundBB = llvm::BasicBlock::Create(*ctx_, "any.found", fn_);
        builder_.CreateCondBr(pred, foundBB, condBB);
        builder_.SetInsertPoint(foundBB);
        builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 1), resultVar);
        builder_.CreateBr(endBB);
        builder_.SetInsertPoint(endBB);
        return builder_.CreateLoad(i1Ty_, resultVar, "any_final");
    }

    // ===== all(list, pred) =====
    if (e.callee == "all") {
        if (e.args.size() != 2)
            codegenError("all() takes exactly 2 arguments");
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Value *lambdaVal = emitExpr(*e.args[1]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy) codegenError("all() requires a list");
        auto fnIt = fn_type_info_.find(lambdaVal);
        if (fnIt == fn_type_info_.end())
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(lambdaVal))
                fnIt = fn_type_info_.find(load->getPointerOperand());
        if (fnIt == fn_type_info_.end())
            codegenError("all() requires a function");
        auto &info = fnIt->second;
        if (info.paramTypes.size() != 1)
            codegenError("all() predicate must take 1 parameter");
        if (info.returnType != i1Ty_)
            codegenError("all() predicate must return bool");

        llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(listHeaderTy_, listVal, 0), "all_len");
        llvm::Value *srcData = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(listHeaderTy_, listVal, 2), "all_data");

        llvm::AllocaInst *resultVar = builder_.CreateAlloca(i1Ty_, nullptr, "all_result");
        builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 1), resultVar);
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "all_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "all.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "all.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "all.end", fn_);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "ali");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, srcLen), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, srcData, {i}, "all_ep");
        llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "all_elem");
        llvm::Value *pred = emitLambdaCall(lambdaVal, info, {elem}, "all_pred");
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, "all.fail", fn_);
        builder_.CreateCondBr(pred, condBB, failBB);
        builder_.SetInsertPoint(failBB);
        builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 0), resultVar);
        builder_.CreateBr(endBB);
        builder_.SetInsertPoint(endBB);
        return builder_.CreateLoad(i1Ty_, resultVar, "all_final");
    }

    // ===== sum(list) =====
    if (e.callee == "sum") {
        if (e.args.size() != 1)
            codegenError("sum() takes exactly 1 argument");
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy) codegenError("sum() requires a list");
        if (elemTy != i64Ty_ && elemTy != f64Ty_ && elemTy != i8Ty_)
            codegenError("sum() requires a numeric list (int, float, or byte)");

        llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(listHeaderTy_, listVal, 0), "sum_len");
        llvm::Value *srcData = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(listHeaderTy_, listVal, 2), "sum_data");

        llvm::AllocaInst *accVar = builder_.CreateAlloca(elemTy, nullptr, "sum_acc");
        if (elemTy == f64Ty_)
            builder_.CreateStore(llvm::ConstantFP::get(f64Ty_, 0.0), accVar);
        else
            builder_.CreateStore(llvm::ConstantInt::get(elemTy, 0), accVar);
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "sum_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "sum.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "sum.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "sum.end", fn_);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "si");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, srcLen), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, srcData, {i}, "sum_ep");
        llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "sum_elem");
        llvm::Value *acc = builder_.CreateLoad(elemTy, accVar, "sum_acc_val");
        llvm::Value *newAcc;
        if (elemTy == f64Ty_)
            newAcc = builder_.CreateFAdd(acc, elem, "sum_add");
        else
            newAcc = builder_.CreateAdd(acc, elem, "sum_add");
        builder_.CreateStore(newAcc, accVar);
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(endBB);
        return builder_.CreateLoad(elemTy, accVar, "sum_result");
    }

    // ===== min(list) / max(list) =====
    if (e.callee == "min" || e.callee == "max") {
        if (e.args.size() != 1)
            codegenError(e.callee + "() takes exactly 1 argument");
        bool isMax = (e.callee == "max");
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy) codegenError(e.callee + "() requires a list");
        if (elemTy != i64Ty_ && elemTy != f64Ty_)
            codegenError(e.callee + "() requires a numeric list (int or float)");

        llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(listHeaderTy_, listVal, 0), "mm_len");
        llvm::Value *srcData = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(listHeaderTy_, listVal, 2), "mm_data");

        // Check empty list
        llvm::Value *isEmptyMM = builder_.CreateICmpEQ(srcLen, llvm::ConstantInt::get(i64Ty_, 0), "mm_empty");
        llvm::BasicBlock *errBBMM = llvm::BasicBlock::Create(*ctx_, "mm.err", fn_);
        llvm::BasicBlock *okBBMM = llvm::BasicBlock::Create(*ctx_, "mm.ok", fn_);
        builder_.CreateCondBr(isEmptyMM, errBBMM, okBBMM);
        builder_.SetInsertPoint(errBBMM);
        emitRuntimeError("runtime error: " + e.callee + "() on empty list\n", ".mm_empty_err");
        builder_.SetInsertPoint(okBBMM);

        llvm::Value *first = builder_.CreateLoad(elemTy, srcData, "mm_first");
        llvm::AllocaInst *bestVar = builder_.CreateAlloca(elemTy, nullptr, "mm_best");
        builder_.CreateStore(first, bestVar);
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "mm_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 1), iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "mm.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "mm.body", fn_);
        llvm::BasicBlock *updateBB = llvm::BasicBlock::Create(*ctx_, "mm.update", fn_);
        llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "mm.next", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "mm.end", fn_);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "mi");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, srcLen), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, srcData, {i}, "mm_ep");
        llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "mm_elem");
        llvm::Value *best = builder_.CreateLoad(elemTy, bestVar, "mm_best_val");
        llvm::Value *cmp;
        if (elemTy == f64Ty_)
            cmp = isMax ? builder_.CreateFCmpOGT(elem, best, "mm_cmp")
                        : builder_.CreateFCmpOLT(elem, best, "mm_cmp");
        else
            cmp = isMax ? builder_.CreateICmpSGT(elem, best, "mm_cmp")
                        : builder_.CreateICmpSLT(elem, best, "mm_cmp");
        builder_.CreateCondBr(cmp, updateBB, nextBB);
        builder_.SetInsertPoint(updateBB);
        builder_.CreateStore(elem, bestVar);
        builder_.CreateBr(nextBB);
        builder_.SetInsertPoint(nextBB);
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(endBB);
        return builder_.CreateLoad(elemTy, bestVar, "mm_result");
    }


    // tap(list, fn) → call fn on each element, return original list
    if (e.callee == "tap") {
        if (e.args.size() != 2)
            codegenError("tap() takes exactly 2 arguments");

        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Value *lambdaVal = emitExpr(*e.args[1]);

        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy)
            codegenError("tap() requires a list as first argument");

        auto fnIt = fn_type_info_.find(lambdaVal);
        if (fnIt == fn_type_info_.end()) {
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(lambdaVal))
                fnIt = fn_type_info_.find(load->getPointerOperand());
        }
        if (fnIt == fn_type_info_.end())
            codegenError("tap() requires a function as second argument");
        auto &info = fnIt->second;

        if (info.paramTypes.size() != 1)
            codegenError("tap() function must take exactly 1 argument");

        // Read source list
        llvm::Value *srcLenPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 0, "tap_src_len_ptr");
        llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, srcLenPtr, "tap_src_len");
        llvm::Value *srcDataPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 2, "tap_src_data_field");
        llvm::Value *srcData = builder_.CreateLoad(ptrTy_, srcDataPtr, "tap_src_data");

        // Loop: call fn on each element
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "tap_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "tap.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "tap.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "tap.end", fn_);

        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(condBB);
        llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "tap_iv");
        llvm::Value *cond = builder_.CreateICmpSLT(iVal, srcLen, "tap_cond");
        builder_.CreateCondBr(cond, bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "tap_ic");
        llvm::Value *srcElemPtr = builder_.CreateGEP(elemTy, srcData, {iCur}, "tap_elem_ptr");
        llvm::Value *srcElem = builder_.CreateLoad(elemTy, srcElemPtr, "tap_elem");
        emitLambdaCall(lambdaVal, info, {srcElem}, "tap_call");
        llvm::Value *iNext = builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1), "tap_next");
        builder_.CreateStore(iNext, iVar);
        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(endBB);
        return listVal;
    }

    return nullptr;
}

// ===== Builtin Collection =====

llvm::Value *CodeGen::emitBuiltinCollection(const CallExpr &e) {
    // add(set, val) → add element to set (no-op if already present)
    // Only intercept if first arg is a set (fall through to user function otherwise)
    if (e.callee == "add" && e.args.size() == 2) {
        llvm::Value *setPtr = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getSetElementType(setPtr);
        if (elemTy) {
            llvm::Value *elem = emitExpr(*e.args[1]);
            if (elem->getType() != elemTy)
                codegenError("add() element type mismatch");

            llvm::Value *idx = emitSetElementLookup(setPtr, elem, elemTy);
            llvm::Value *found = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "found");

            llvm::BasicBlock *insertBB = llvm::BasicBlock::Create(*ctx_, "set.insert", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "set.add_end", fn_);
            builder_.CreateCondBr(found, endBB, insertBB);

            builder_.SetInsertPoint(insertBB);
            llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, setPtr, 0, "set_len_ptr");
            llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "set_len");
            llvm::Value *capPtr = builder_.CreateStructGEP(setHeaderTy_, setPtr, 1, "set_cap_ptr");
            llvm::Value *cap = builder_.CreateLoad(i64Ty_, capPtr, "set_cap");

            llvm::Value *needGrow = builder_.CreateICmpEQ(length, cap, "need_grow");
            llvm::BasicBlock *growBB = llvm::BasicBlock::Create(*ctx_, "set.grow", fn_);
            llvm::BasicBlock *storeBB = llvm::BasicBlock::Create(*ctx_, "set.store", fn_);
            builder_.CreateCondBr(needGrow, growBB, storeBB);

            builder_.SetInsertPoint(growBB);
            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t elemSize = dl.getTypeAllocSize(elemTy);
            llvm::Value *newCap = builder_.CreateMul(cap, llvm::ConstantInt::get(i64Ty_, 2), "new_cap");
            auto mallocFn = getStdlibMalloc();
            llvm::Value *newSize = builder_.CreateMul(newCap, llvm::ConstantInt::get(i64Ty_, elemSize), "new_size");
            llvm::Value *newElemsPtr = builder_.CreateCall(mallocFn, {newSize}, "new_elems");

            auto memcpyFn = getStdlibMemcpy();
            llvm::Value *elemsPtrField = builder_.CreateStructGEP(setHeaderTy_, setPtr, 2, "elems_field");
            llvm::Value *oldElemsPtr = builder_.CreateLoad(ptrTy_, elemsPtrField, "old_elems");
            llvm::Value *oldSize = builder_.CreateMul(length, llvm::ConstantInt::get(i64Ty_, elemSize), "old_size");
            builder_.CreateCall(memcpyFn, {newElemsPtr, oldElemsPtr, oldSize});

            auto freeFn = getStdlibFree();
            builder_.CreateCall(freeFn, {oldElemsPtr});

            builder_.CreateStore(newElemsPtr, elemsPtrField);
            builder_.CreateStore(newCap, capPtr);
            builder_.CreateBr(storeBB);

            builder_.SetInsertPoint(storeBB);
            llvm::Value *curLen = builder_.CreateLoad(i64Ty_, lenPtr, "cur_len");
            llvm::Value *elemsPtrField2 = builder_.CreateStructGEP(setHeaderTy_, setPtr, 2, "elems_field2");
            llvm::Value *curElemsPtr = builder_.CreateLoad(ptrTy_, elemsPtrField2, "cur_elems");
            llvm::Value *newElemPtr = builder_.CreateGEP(elemTy, curElemsPtr, {curLen}, "new_elem_ptr");
            builder_.CreateStore(elem, newElemPtr);

            llvm::Value *newLen = builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1), "new_len");
            builder_.CreateStore(newLen, lenPtr);

            // Insert into hash table buckets and check rehash
            emitBucketInsertAndRehashCheck(setPtr, setHeaderTy_, 0, 3, 4, elem, elemTy, curLen);

            builder_.CreateBr(endBB);

            builder_.SetInsertPoint(endBB);
            return llvm::ConstantInt::get(i64Ty_, 0);
        }
        // Not a set — fall through to user function resolution
    }

    // remove(set, val) → remove element from set
    if (e.callee == "remove" && e.args.size() == 2) {
        llvm::Value *containerPtr = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getSetElementType(containerPtr);
        if (elemTy) {
            llvm::Value *elem = emitExpr(*e.args[1]);
            if (elem->getType() != elemTy)
                codegenError("remove() element type mismatch");

            llvm::Value *idx = emitSetElementLookup(containerPtr, elem, elemTy);
            llvm::Value *found = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "found");

            llvm::BasicBlock *removeBB = llvm::BasicBlock::Create(*ctx_, "set.remove", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "set.remove_end", fn_);
            builder_.CreateCondBr(found, removeBB, endBB);

            builder_.SetInsertPoint(removeBB);
            llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, containerPtr, 0, "set_len_ptr");
            llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "set_len");
            llvm::Value *elemsPtrField = builder_.CreateStructGEP(setHeaderTy_, containerPtr, 2, "set_elems_ptr");
            llvm::Value *elemsPtr = builder_.CreateLoad(ptrTy_, elemsPtrField, "set_elems");

            // Remove from bucket: set tombstone for this element
            {
                auto hfi = resolveHashFn(elemTy);
                llvm::Value *hashElem = elem;
                if (elemTy != hfi.hashArgTy && elemTy->isIntegerTy() && hfi.hashArgTy->isIntegerTy())
                    hashElem = builder_.CreateZExt(elem, hfi.hashArgTy, "rm_hash_zext");
                llvm::FunctionType *hashTy = llvm::FunctionType::get(i64Ty_, {hfi.hashArgTy}, false);
                llvm::FunctionCallee hashFn = mod_->getOrInsertFunction(hfi.hashFnName, hashTy);
                llvm::Value *hashVal = builder_.CreateCall(hashFn, {hashElem}, "rm_hash");

                llvm::Value *bucketsField = builder_.CreateStructGEP(setHeaderTy_, containerPtr, 4, "rm_bp");
                llvm::Value *bucketsPtr = builder_.CreateLoad(ptrTy_, bucketsField, "rm_buckets");
                llvm::Value *bcField = builder_.CreateStructGEP(setHeaderTy_, containerPtr, 3, "rm_bc_field");
                llvm::Value *bucketCount = builder_.CreateLoad(i64Ty_, bcField, "rm_bc");
                llvm::Value *bucketMask = builder_.CreateSub(bucketCount, llvm::ConstantInt::get(i64Ty_, 1), "rm_bmask");

                llvm::FunctionType *removeTy = llvm::FunctionType::get(
                    llvm::Type::getVoidTy(*ctx_), {ptrTy_, i64Ty_, i64Ty_, i64Ty_}, false);
                llvm::FunctionCallee removeFn = mod_->getOrInsertFunction("__ry_ht_remove", removeTy);
                builder_.CreateCall(removeFn, {bucketsPtr, bucketMask, hashVal, idx});
            }

            // Swap-remove: move last element to idx position
            llvm::Value *lastIdx = builder_.CreateSub(length, llvm::ConstantInt::get(i64Ty_, 1), "last_idx");
            llvm::Value *isNotLast = builder_.CreateICmpNE(idx, lastIdx, "is_not_last");

            llvm::BasicBlock *swapBB = llvm::BasicBlock::Create(*ctx_, "set.swap", fn_);
            llvm::BasicBlock *decBB = llvm::BasicBlock::Create(*ctx_, "set.dec", fn_);
            builder_.CreateCondBr(isNotLast, swapBB, decBB);

            builder_.SetInsertPoint(swapBB);
            // Load last element
            llvm::Value *lastPtr = builder_.CreateGEP(elemTy, elemsPtr, {lastIdx}, "last_ptr");
            llvm::Value *lastVal = builder_.CreateLoad(elemTy, lastPtr, "last_val");
            // Store at idx position
            llvm::Value *dstPtr = builder_.CreateGEP(elemTy, elemsPtr, {idx}, "swap_dst");
            builder_.CreateStore(lastVal, dstPtr);

            // Update bucket for the moved element: change lastIdx -> idx
            {
                auto hfi2 = resolveHashFn(elemTy);
                llvm::Value *hashLastVal = lastVal;
                if (elemTy != hfi2.hashArgTy && elemTy->isIntegerTy() && hfi2.hashArgTy->isIntegerTy())
                    hashLastVal = builder_.CreateZExt(lastVal, hfi2.hashArgTy, "swap_hash_zext");
                llvm::FunctionType *hashTy = llvm::FunctionType::get(i64Ty_, {hfi2.hashArgTy}, false);
                llvm::FunctionCallee hashFn = mod_->getOrInsertFunction(hfi2.hashFnName, hashTy);
                llvm::Value *lastHash = builder_.CreateCall(hashFn, {hashLastVal}, "last_hash");

                llvm::Value *bucketsField = builder_.CreateStructGEP(setHeaderTy_, containerPtr, 4, "swap_bp");
                llvm::Value *bucketsPtr = builder_.CreateLoad(ptrTy_, bucketsField, "swap_buckets");
                llvm::Value *bcField = builder_.CreateStructGEP(setHeaderTy_, containerPtr, 3, "swap_bc_field");
                llvm::Value *bucketCount = builder_.CreateLoad(i64Ty_, bcField, "swap_bc");
                llvm::Value *bucketMask = builder_.CreateSub(bucketCount, llvm::ConstantInt::get(i64Ty_, 1), "swap_bmask");

                llvm::FunctionType *updateTy = llvm::FunctionType::get(
                    llvm::Type::getVoidTy(*ctx_), {ptrTy_, i64Ty_, i64Ty_, i64Ty_, i64Ty_}, false);
                llvm::FunctionCallee updateFn = mod_->getOrInsertFunction("__ry_ht_update_index", updateTy);
                builder_.CreateCall(updateFn, {bucketsPtr, bucketMask, lastHash, lastIdx, idx});
            }
            builder_.CreateBr(decBB);

            builder_.SetInsertPoint(decBB);
            llvm::Value *newLen = builder_.CreateSub(length, llvm::ConstantInt::get(i64Ty_, 1), "new_len");
            builder_.CreateStore(newLen, lenPtr);
            builder_.CreateBr(endBB);

            builder_.SetInsertPoint(endBB);
            return llvm::ConstantInt::get(i64Ty_, 0);
        }
        // Not a set — try list remove
        llvm::Type *listElemTy = getListElementType(containerPtr);
        if (listElemTy) {
            llvm::Value *val = emitExpr(*e.args[1]);
            if (val->getType() != listElemTy)
                codegenError("remove() value type mismatch with list element type");

            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t elemSize = dl.getTypeAllocSize(listElemTy);

            llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, containerPtr, 0, "lrem_len_ptr");
            llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "lrem_len");
            llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, containerPtr, 2, "lrem_data_field");
            llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataPtrField, "lrem_data");

            // Linear search for the value
            llvm::AllocaInst *foundIdx = builder_.CreateAlloca(i64Ty_, nullptr, "lrem_found_idx");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, -1), foundIdx);
            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "lrem_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

            llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "lrem.cond", fn_);
            llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "lrem.body", fn_);
            llvm::BasicBlock *endSearchBB = llvm::BasicBlock::Create(*ctx_, "lrem.end_search", fn_);

            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(condBB);
            llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "lrem_iv");
            llvm::Value *notYetFound = builder_.CreateICmpSLT(
                builder_.CreateLoad(i64Ty_, foundIdx, "lrem_fi"), llvm::ConstantInt::get(i64Ty_, 0), "lrem_not_found");
            llvm::Value *inBounds = builder_.CreateICmpSLT(iVal, length, "lrem_in_bounds");
            llvm::Value *cont = builder_.CreateAnd(notYetFound, inBounds, "lrem_cont");
            builder_.CreateCondBr(cont, bodyBB, endSearchBB);

            builder_.SetInsertPoint(bodyBB);
            llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "lrem_ic");
            llvm::Value *elemPtr = builder_.CreateGEP(listElemTy, dataPtr, {iCur}, "lrem_elem_ptr");
            llvm::Value *listElem = builder_.CreateLoad(listElemTy, elemPtr, "lrem_elem");

            llvm::Value *match;
            if (listElemTy == ptrTy_) {
                if (getNestedListElementType(containerPtr))
                    codegenError("remove() is not supported for lists of non-string pointer elements");
                auto strcmpFn = getStdlibStrcmp();
                llvm::Value *cmpResult = builder_.CreateCall(strcmpFn, {val, listElem}, "lrem_strcmp");
                match = builder_.CreateICmpEQ(cmpResult, llvm::ConstantInt::get(i32Ty_, 0), "lrem_match");
            } else if (listElemTy->isDoubleTy()) {
                match = builder_.CreateFCmpOEQ(val, listElem, "lrem_match");
            } else {
                match = builder_.CreateICmpEQ(val, listElem, "lrem_match");
            }

            llvm::BasicBlock *foundBB = llvm::BasicBlock::Create(*ctx_, "lrem.found", fn_);
            llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "lrem.next", fn_);
            builder_.CreateCondBr(match, foundBB, nextBB);

            builder_.SetInsertPoint(foundBB);
            builder_.CreateStore(iCur, foundIdx);
            builder_.CreateBr(condBB);

            builder_.SetInsertPoint(nextBB);
            llvm::Value *iNext = builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1), "lrem_inext");
            builder_.CreateStore(iNext, iVar);
            builder_.CreateBr(condBB);

            // After search: if found, memmove to close the gap
            builder_.SetInsertPoint(endSearchBB);
            llvm::Value *idx = builder_.CreateLoad(i64Ty_, foundIdx, "lrem_idx");
            llvm::Value *wasFound = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "lrem_was_found");

            llvm::BasicBlock *removeBB = llvm::BasicBlock::Create(*ctx_, "lrem.remove", fn_);
            llvm::BasicBlock *doneBB = llvm::BasicBlock::Create(*ctx_, "lrem.done", fn_);
            builder_.CreateCondBr(wasFound, removeBB, doneBB);

            builder_.SetInsertPoint(removeBB);
            auto memmoveFn = getStdlibMemmove();
            llvm::Value *dstPtr = builder_.CreateGEP(listElemTy, dataPtr, {idx}, "lrem_dst");
            llvm::Value *srcPtr = builder_.CreateGEP(listElemTy, dataPtr,
                {builder_.CreateAdd(idx, llvm::ConstantInt::get(i64Ty_, 1))}, "lrem_src");
            llvm::Value *moveCount = builder_.CreateSub(
                builder_.CreateSub(length, idx), llvm::ConstantInt::get(i64Ty_, 1), "lrem_move_count");
            llvm::Value *moveBytes = builder_.CreateMul(moveCount, llvm::ConstantInt::get(i64Ty_, elemSize), "lrem_move_bytes");
            builder_.CreateCall(memmoveFn, {dstPtr, srcPtr, moveBytes});
            llvm::Value *newLen = builder_.CreateSub(length, llvm::ConstantInt::get(i64Ty_, 1), "lrem_new_len");
            builder_.CreateStore(newLen, lenPtr);
            builder_.CreateBr(doneBB);

            builder_.SetInsertPoint(doneBB);
            return llvm::ConstantInt::get(i64Ty_, 0);
        }
        // Not a set or list — try map remove
        llvm::Type *keyTy = getMapKeyType(containerPtr);
        llvm::Type *valTy = getMapValueType(containerPtr);
        if (keyTy && valTy) {
            llvm::Value *key = emitExpr(*e.args[1]);
            if (key->getType() != keyTy)
                codegenError("remove() key type mismatch");
            llvm::Value *idx = emitMapKeyLookup(containerPtr, key, keyTy);
            llvm::Value *found = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "mrem_found");

            llvm::BasicBlock *removeBB = llvm::BasicBlock::Create(*ctx_, "mrem.do", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "mrem.end", fn_);
            builder_.CreateCondBr(found, removeBB, endBB);

            builder_.SetInsertPoint(removeBB);
            llvm::Value *lenPtr = builder_.CreateStructGEP(mapHeaderTy_, containerPtr, 0, "mrem_len_ptr");
            llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "mrem_len");
            llvm::Value *keysPtrField = builder_.CreateStructGEP(mapHeaderTy_, containerPtr, 2, "mrem_keys_field");
            llvm::Value *keysPtr = builder_.CreateLoad(ptrTy_, keysPtrField, "mrem_keys");
            llvm::Value *valsPtrField = builder_.CreateStructGEP(mapHeaderTy_, containerPtr, 3, "mrem_vals_field");
            llvm::Value *valsPtr = builder_.CreateLoad(ptrTy_, valsPtrField, "mrem_vals");

            // Remove from hash table
            auto hfi = resolveHashFn(keyTy);
            llvm::Value *hashKey = key;
            if (keyTy != hfi.hashArgTy && keyTy->isIntegerTy() && hfi.hashArgTy->isIntegerTy())
                hashKey = builder_.CreateZExt(key, hfi.hashArgTy, "mrem_hash_zext");
            llvm::FunctionType *hashTy = llvm::FunctionType::get(i64Ty_, {hfi.hashArgTy}, false);
            llvm::FunctionCallee hashFn = mod_->getOrInsertFunction(hfi.hashFnName, hashTy);
            llvm::Value *hashVal = builder_.CreateCall(hashFn, {hashKey}, "mrem_hash");

            llvm::Value *bucketsField = builder_.CreateStructGEP(mapHeaderTy_, containerPtr, 5, "mrem_bp");
            llvm::Value *bucketsPtr = builder_.CreateLoad(ptrTy_, bucketsField, "mrem_buckets");
            llvm::Value *bcField = builder_.CreateStructGEP(mapHeaderTy_, containerPtr, 4, "mrem_bc_field");
            llvm::Value *bucketCount = builder_.CreateLoad(i64Ty_, bcField, "mrem_bc");
            llvm::Value *bucketMask = builder_.CreateSub(bucketCount, llvm::ConstantInt::get(i64Ty_, 1), "mrem_bmask");

            llvm::FunctionType *htRemoveTy = llvm::FunctionType::get(
                llvm::Type::getVoidTy(*ctx_), {ptrTy_, i64Ty_, i64Ty_, i64Ty_}, false);
            llvm::FunctionCallee htRemoveFn = mod_->getOrInsertFunction("__ry_ht_remove", htRemoveTy);
            builder_.CreateCall(htRemoveFn, {bucketsPtr, bucketMask, hashVal, idx});

            // Swap-remove from keys and values arrays
            llvm::Value *lastIdx = builder_.CreateSub(length, llvm::ConstantInt::get(i64Ty_, 1), "mrem_last_idx");
            llvm::Value *isNotLast = builder_.CreateICmpNE(idx, lastIdx, "mrem_not_last");

            llvm::BasicBlock *swapBB = llvm::BasicBlock::Create(*ctx_, "mrem.swap", fn_);
            llvm::BasicBlock *decBB = llvm::BasicBlock::Create(*ctx_, "mrem.dec", fn_);
            builder_.CreateCondBr(isNotLast, swapBB, decBB);

            builder_.SetInsertPoint(swapBB);
            llvm::Value *lastKeyPtr = builder_.CreateGEP(keyTy, keysPtr, {lastIdx}, "mrem_last_kp");
            llvm::Value *lastKey = builder_.CreateLoad(keyTy, lastKeyPtr, "mrem_last_key");
            llvm::Value *dstKeyPtr = builder_.CreateGEP(keyTy, keysPtr, {idx}, "mrem_dst_kp");
            builder_.CreateStore(lastKey, dstKeyPtr);
            llvm::Value *lastValPtr = builder_.CreateGEP(valTy, valsPtr, {lastIdx}, "mrem_last_vp");
            llvm::Value *lastVal = builder_.CreateLoad(valTy, lastValPtr, "mrem_last_val");
            llvm::Value *dstValPtr = builder_.CreateGEP(valTy, valsPtr, {idx}, "mrem_dst_vp");
            builder_.CreateStore(lastVal, dstValPtr);

            // Update bucket for moved element
            llvm::Value *hashLastKey = lastKey;
            if (keyTy != hfi.hashArgTy && keyTy->isIntegerTy() && hfi.hashArgTy->isIntegerTy())
                hashLastKey = builder_.CreateZExt(lastKey, hfi.hashArgTy, "mrem_lk_hash_zext");
            llvm::Value *lastKeyHash = builder_.CreateCall(hashFn, {hashLastKey}, "mrem_lk_hash");
            llvm::FunctionType *updateTy = llvm::FunctionType::get(
                llvm::Type::getVoidTy(*ctx_), {ptrTy_, i64Ty_, i64Ty_, i64Ty_, i64Ty_}, false);
            llvm::FunctionCallee updateFn = mod_->getOrInsertFunction("__ry_ht_update_index", updateTy);
            builder_.CreateCall(updateFn, {bucketsPtr, bucketMask, lastKeyHash, lastIdx, idx});
            builder_.CreateBr(decBB);

            builder_.SetInsertPoint(decBB);
            llvm::Value *newLen = builder_.CreateSub(length, llvm::ConstantInt::get(i64Ty_, 1), "mrem_new_len");
            builder_.CreateStore(newLen, lenPtr);
            builder_.CreateBr(endBB);

            builder_.SetInsertPoint(endBB);
            return llvm::ConstantInt::get(i64Ty_, 0);
        }
        // Not a set, list, or map — fall through to user function resolution
    }

    // append(list, val) → mutating append
    if (e.callee == "append" && e.args.size() == 2) {
        llvm::Value *listPtr = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listPtr);
        if (elemTy) {
            llvm::Value *val = emitExpr(*e.args[1]);
            if (val->getType() != elemTy)
                codegenError("append() element type mismatch");

            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t elemSize = dl.getTypeAllocSize(elemTy);
            auto mallocFn = getStdlibMalloc();
            auto memcpyFn = getStdlibMemcpy();
            auto freeFn = getStdlibFree();

            llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, listPtr, 0, "app_len_ptr");
            llvm::Value *capPtr = builder_.CreateStructGEP(listHeaderTy_, listPtr, 1, "app_cap_ptr");
            llvm::Value *dataField = builder_.CreateStructGEP(listHeaderTy_, listPtr, 2, "app_data_field");
            llvm::Value *len = builder_.CreateLoad(i64Ty_, lenPtr, "app_len");
            llvm::Value *cap = builder_.CreateLoad(i64Ty_, capPtr, "app_cap");
            llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataField, "app_data");

            // Check if realloc needed
            llvm::Value *needGrow = builder_.CreateICmpEQ(len, cap, "app_need_grow");
            llvm::BasicBlock *growBB = llvm::BasicBlock::Create(*ctx_, "app.grow", fn_);
            llvm::BasicBlock *storeBB = llvm::BasicBlock::Create(*ctx_, "app.store", fn_);

            builder_.CreateCondBr(needGrow, growBB, storeBB);

            // Grow: new_cap = cap * 2 (min 4)
            builder_.SetInsertPoint(growBB);
            llvm::Value *four = llvm::ConstantInt::get(i64Ty_, 4);
            llvm::Value *doubled = builder_.CreateMul(cap, llvm::ConstantInt::get(i64Ty_, 2), "app_doubled");
            llvm::Value *newCap = builder_.CreateSelect(
                builder_.CreateICmpSGT(doubled, four, "cap_gt4"), doubled, four, "app_new_cap");
            llvm::Value *newSize = builder_.CreateMul(newCap, llvm::ConstantInt::get(i64Ty_, elemSize), "app_new_size");
            llvm::Value *newData = builder_.CreateCall(mallocFn, {newSize}, "app_new_data");
            llvm::Value *oldSize = builder_.CreateMul(len, llvm::ConstantInt::get(i64Ty_, elemSize), "app_old_size");
            builder_.CreateCall(memcpyFn, {newData, dataPtr, oldSize});
            builder_.CreateCall(freeFn, {dataPtr});
            builder_.CreateStore(newData, dataField);
            builder_.CreateStore(newCap, capPtr);
            builder_.CreateBr(storeBB);

            // Store the new element
            builder_.SetInsertPoint(storeBB);
            llvm::Value *curData = builder_.CreateLoad(ptrTy_, dataField, "app_cur_data");
            llvm::Value *curLen = builder_.CreateLoad(i64Ty_, lenPtr, "app_cur_len");
            llvm::Value *elemPtr = builder_.CreateGEP(elemTy, curData, curLen, "app_elem_ptr");
            builder_.CreateStore(val, elemPtr);
            llvm::Value *newLen = builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1), "app_new_len");
            builder_.CreateStore(newLen, lenPtr);

            return llvm::ConstantInt::get(i64Ty_, 0);
        }
    }

    // appended(list, elem) → new list with element added
    if (e.callee == "appended" && e.args.size() == 2) {
        llvm::Value *listPtr = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listPtr);
        if (elemTy) {
            llvm::Value *val = emitExpr(*e.args[1]);
            if (val->getType() != elemTy)
                codegenError("appended() element type mismatch");

            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
            uint64_t elemSize = dl.getTypeAllocSize(elemTy);
            auto mallocFn = getStdlibMalloc();
            auto memcpyFn = getStdlibMemcpy();

            llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(listHeaderTy_, listPtr, 0), "apd_len");
            llvm::Value *srcData = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(listHeaderTy_, listPtr, 2), "apd_data");
            llvm::Value *newLen = builder_.CreateAdd(srcLen, llvm::ConstantInt::get(i64Ty_, 1), "apd_new_len");

            llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "apd_header");
            llvm::Value *newDataSize = builder_.CreateMul(newLen, llvm::ConstantInt::get(i64Ty_, elemSize), "apd_ds");
            llvm::Value *newData = builder_.CreateCall(mallocFn, {newDataSize}, "apd_nd");

            llvm::Value *oldDataSize = builder_.CreateMul(srcLen, llvm::ConstantInt::get(i64Ty_, elemSize), "apd_ods");
            builder_.CreateCall(memcpyFn, {newData, srcData, oldDataSize});

            llvm::Value *newElemPtr = builder_.CreateGEP(elemTy, newData, srcLen, "apd_new_ep");
            builder_.CreateStore(val, newElemPtr);

            builder_.CreateStore(newLen, builder_.CreateStructGEP(listHeaderTy_, newHeader, 0));
            builder_.CreateStore(newLen, builder_.CreateStructGEP(listHeaderTy_, newHeader, 1));
            builder_.CreateStore(newData, builder_.CreateStructGEP(listHeaderTy_, newHeader, 2));

            list_element_types_[newHeader] = elemTy;
            return newHeader;
        }
    }

    // append!(list, elem) → alias for append
    if (e.callee == "append!" && e.args.size() == 2) {
        auto &mutArgs = const_cast<CallExpr &>(e).args;
        CallExpr appendProxy;
        appendProxy.callee = "append";
        appendProxy.args = std::move(mutArgs);
        ArgsRestoreGuard guard{mutArgs, appendProxy.args};
        return emitBuiltinCollection(appendProxy);
    }

    // pop(list) → Option<T>: remove and return last element
    if (e.callee == "pop" && e.args.size() == 1) {
        llvm::Value *listPtr = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listPtr);
        if (elemTy) {
            llvm::StructType *optTy = getOptionType(elemTy);
            llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, listPtr, 0, "pop_len_ptr");
            llvm::Value *len = builder_.CreateLoad(i64Ty_, lenPtr, "pop_len");
            llvm::Value *dataField = builder_.CreateStructGEP(listHeaderTy_, listPtr, 2, "pop_data_field");
            llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataField, "pop_data");

            llvm::Value *isEmpty = builder_.CreateICmpEQ(len, llvm::ConstantInt::get(i64Ty_, 0), "pop_empty");
            llvm::BasicBlock *emptyBB = llvm::BasicBlock::Create(*ctx_, "pop.empty", fn_);
            llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "pop.ok", fn_);
            llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "pop.merge", fn_);
            builder_.CreateCondBr(isEmpty, emptyBB, okBB);

            builder_.SetInsertPoint(emptyBB);
            llvm::Value *noneVal = buildNoneValue(optTy);
            builder_.CreateBr(mergeBB);
            llvm::BasicBlock *emptyEndBB = builder_.GetInsertBlock();

            builder_.SetInsertPoint(okBB);
            llvm::Value *lastIdx = builder_.CreateSub(len, llvm::ConstantInt::get(i64Ty_, 1), "pop_last_idx");
            llvm::Value *elemPtr = builder_.CreateGEP(elemTy, dataPtr, lastIdx, "pop_elem_ptr");
            llvm::Value *val = builder_.CreateLoad(elemTy, elemPtr, "pop_val");
            builder_.CreateStore(lastIdx, lenPtr);
            llvm::Value *someVal = buildSomeValue(val, optTy);
            builder_.CreateBr(mergeBB);
            llvm::BasicBlock *okEndBB = builder_.GetInsertBlock();

            builder_.SetInsertPoint(mergeBB);
            llvm::PHINode *phi = builder_.CreatePHI(optTy, 2, "pop_result");
            phi->addIncoming(noneVal, emptyEndBB);
            phi->addIncoming(someVal, okEndBB);
            return phi;
        }
    }

    // slice(list, start, end) → new sub-list
    if (e.callee == "slice" && e.args.size() == 3) {
        llvm::Value *listPtr = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listPtr);
        if (elemTy) {
            llvm::Value *startVal = emitExpr(*e.args[1]);
            llvm::Value *endVal = emitExpr(*e.args[2]);

            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
            uint64_t elemSize = dl.getTypeAllocSize(elemTy);
            auto mallocFn = getStdlibMalloc();
            auto memcpyFn = getStdlibMemcpy();

            llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, listPtr, 0, "sl_len_ptr");
            llvm::Value *len = builder_.CreateLoad(i64Ty_, lenPtr, "sl_len");
            llvm::Value *dataField = builder_.CreateStructGEP(listHeaderTy_, listPtr, 2, "sl_data_field");
            llvm::Value *srcData = builder_.CreateLoad(ptrTy_, dataField, "sl_src_data");

            // Clamp start and end
            llvm::Value *zero = llvm::ConstantInt::get(i64Ty_, 0);
            llvm::Value *clampedStart = builder_.CreateSelect(
                builder_.CreateICmpSLT(startVal, zero), zero, startVal, "sl_cstart");
            clampedStart = builder_.CreateSelect(
                builder_.CreateICmpSGT(clampedStart, len), len, clampedStart, "sl_cstart2");
            llvm::Value *clampedEnd = builder_.CreateSelect(
                builder_.CreateICmpSLT(endVal, zero), zero, endVal, "sl_cend");
            clampedEnd = builder_.CreateSelect(
                builder_.CreateICmpSGT(clampedEnd, len), len, clampedEnd, "sl_cend2");

            // Compute count = max(0, end - start)
            llvm::Value *diff = builder_.CreateSub(clampedEnd, clampedStart, "sl_diff");
            llvm::Value *count = builder_.CreateSelect(
                builder_.CreateICmpSGT(diff, zero), diff, zero, "sl_count");

            // Allocate new list
            llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "sl_header");
            llvm::Value *dataSize = builder_.CreateMul(count, llvm::ConstantInt::get(i64Ty_, elemSize), "sl_dsize");
            llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "sl_data");

            // Copy elements
            llvm::Value *srcOffset = builder_.CreateGEP(elemTy, srcData, clampedStart, "sl_src_off");
            builder_.CreateCall(memcpyFn, {newData, srcOffset, dataSize});

            // Set header fields
            llvm::Value *newLenPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 0, "sl_new_len");
            builder_.CreateStore(count, newLenPtr);
            llvm::Value *newCapPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 1, "sl_new_cap");
            builder_.CreateStore(count, newCapPtr);
            llvm::Value *newDataField = builder_.CreateStructGEP(listHeaderTy_, newHeader, 2, "sl_new_data");
            builder_.CreateStore(newData, newDataField);

            list_element_types_[newHeader] = elemTy;
            return newHeader;
        }
    }

    // take(list, n) → new list with first n elements
    if (e.callee == "take" && e.args.size() == 2) {
        llvm::Value *listPtr = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listPtr);
        if (elemTy) {
            llvm::Value *nVal = emitExpr(*e.args[1]);

            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
            uint64_t elemSize = dl.getTypeAllocSize(elemTy);
            auto mallocFn = getStdlibMalloc();
            auto memcpyFn = getStdlibMemcpy();

            llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, listPtr, 0, "tk_len_ptr");
            llvm::Value *len = builder_.CreateLoad(i64Ty_, lenPtr, "tk_len");
            llvm::Value *dataField = builder_.CreateStructGEP(listHeaderTy_, listPtr, 2, "tk_data_field");
            llvm::Value *srcData = builder_.CreateLoad(ptrTy_, dataField, "tk_src_data");

            // Clamp n: max(0, min(n, len))
            llvm::Value *zero = llvm::ConstantInt::get(i64Ty_, 0);
            llvm::Value *clampedN = builder_.CreateSelect(
                builder_.CreateICmpSLT(nVal, zero), zero, nVal, "tk_cn");
            clampedN = builder_.CreateSelect(
                builder_.CreateICmpSGT(clampedN, len), len, clampedN, "tk_cn2");

            // Allocate new list
            llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "tk_header");
            llvm::Value *dataSize = builder_.CreateMul(clampedN, llvm::ConstantInt::get(i64Ty_, elemSize), "tk_dsize");
            llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "tk_data");

            // Copy elements
            builder_.CreateCall(memcpyFn, {newData, srcData, dataSize});

            // Set header fields
            llvm::Value *newLenPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 0, "tk_new_len");
            builder_.CreateStore(clampedN, newLenPtr);
            llvm::Value *newCapPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 1, "tk_new_cap");
            builder_.CreateStore(clampedN, newCapPtr);
            llvm::Value *newDataField = builder_.CreateStructGEP(listHeaderTy_, newHeader, 2, "tk_new_data");
            builder_.CreateStore(newData, newDataField);

            list_element_types_[newHeader] = elemTy;
            return newHeader;
        }
    }
    // ===== insert(list, index, value) =====
    if (e.callee == "insert" && e.args.size() == 3) {
        llvm::Value *listPtr = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listPtr);
        if (elemTy) {
            llvm::Value *idx = emitExpr(*e.args[1]);
            if (idx->getType() != i64Ty_)
                codegenError("insert() index must be int");
            llvm::Value *val = emitExpr(*e.args[2]);
            if (val->getType() != elemTy)
                codegenError("insert() element type mismatch");

            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t elemSize = dl.getTypeAllocSize(elemTy);

            auto mallocFn = getStdlibMalloc();
            auto memcpyFn = getStdlibMemcpy();
            auto freeFn = getStdlibFree();
            auto memmoveFn = getStdlibMemmove();

            llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, listPtr, 0, "ins_len_ptr");
            llvm::Value *capPtr = builder_.CreateStructGEP(listHeaderTy_, listPtr, 1, "ins_cap_ptr");
            llvm::Value *dataField = builder_.CreateStructGEP(listHeaderTy_, listPtr, 2, "ins_data_field");
            llvm::Value *len = builder_.CreateLoad(i64Ty_, lenPtr, "ins_len");
            llvm::Value *cap = builder_.CreateLoad(i64Ty_, capPtr, "ins_cap");
            llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataField, "ins_data");

            // Bounds check
            llvm::Value *outOfBounds = builder_.CreateOr(
                builder_.CreateICmpSLT(idx, llvm::ConstantInt::get(i64Ty_, 0)),
                builder_.CreateICmpSGT(idx, len), "ins_oob");
            llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, "ins.err", fn_);
            llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "ins.ok", fn_);
            builder_.CreateCondBr(outOfBounds, errBB, okBB);
            builder_.SetInsertPoint(errBB);
            emitRuntimeError("runtime error: insert() index out of bounds\n", ".ins_oob_err");

            builder_.SetInsertPoint(okBB);
            // Check if realloc needed
            llvm::Value *needGrow = builder_.CreateICmpEQ(len, cap, "ins_need_grow");
            llvm::BasicBlock *growBB = llvm::BasicBlock::Create(*ctx_, "ins.grow", fn_);
            llvm::BasicBlock *moveBB = llvm::BasicBlock::Create(*ctx_, "ins.move", fn_);
            builder_.CreateCondBr(needGrow, growBB, moveBB);

            builder_.SetInsertPoint(growBB);
            llvm::Value *four = llvm::ConstantInt::get(i64Ty_, 4);
            llvm::Value *doubled = builder_.CreateMul(cap, llvm::ConstantInt::get(i64Ty_, 2), "ins_doubled");
            llvm::Value *newCap = builder_.CreateSelect(
                builder_.CreateICmpSGT(doubled, four), doubled, four, "ins_new_cap");
            llvm::Value *newSize = builder_.CreateMul(newCap, llvm::ConstantInt::get(i64Ty_, elemSize), "ins_new_size");
            llvm::Value *newData = builder_.CreateCall(mallocFn, {newSize}, "ins_new_data");
            llvm::Value *oldSize = builder_.CreateMul(len, llvm::ConstantInt::get(i64Ty_, elemSize), "ins_old_size");
            builder_.CreateCall(memcpyFn, {newData, dataPtr, oldSize});
            builder_.CreateCall(freeFn, {dataPtr});
            builder_.CreateStore(newData, dataField);
            builder_.CreateStore(newCap, capPtr);
            builder_.CreateBr(moveBB);

            builder_.SetInsertPoint(moveBB);
            llvm::Value *curData = builder_.CreateLoad(ptrTy_, dataField, "ins_cur_data");
            // memmove elements from idx to idx+1
            llvm::Value *srcPtr = builder_.CreateGEP(elemTy, curData, {idx}, "ins_src");
            llvm::Value *dstPtr = builder_.CreateGEP(elemTy, curData,
                {builder_.CreateAdd(idx, llvm::ConstantInt::get(i64Ty_, 1))}, "ins_dst");
            llvm::Value *moveCount = builder_.CreateSub(len, idx, "ins_move_count");
            llvm::Value *moveBytes = builder_.CreateMul(moveCount, llvm::ConstantInt::get(i64Ty_, elemSize), "ins_move_bytes");
            builder_.CreateCall(memmoveFn, {dstPtr, srcPtr, moveBytes});
            // Store new element at idx
            llvm::Value *insertPtr = builder_.CreateGEP(elemTy, curData, {idx}, "ins_ptr");
            builder_.CreateStore(val, insertPtr);
            // len++
            llvm::Value *newLen = builder_.CreateAdd(len, llvm::ConstantInt::get(i64Ty_, 1), "ins_new_len");
            builder_.CreateStore(newLen, lenPtr);

            return llvm::ConstantInt::get(i64Ty_, 0);
        }
    }

    // ===== remove_at(list, index) =====
    if (e.callee == "remove_at" && e.args.size() == 2) {
        llvm::Value *listPtr = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listPtr);
        if (elemTy) {
            llvm::Value *idx = emitExpr(*e.args[1]);
            if (idx->getType() != i64Ty_)
                codegenError("remove_at() index must be int");

            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t elemSize = dl.getTypeAllocSize(elemTy);

            auto memmoveFn = getStdlibMemmove();

            llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, listPtr, 0, "rmat_len_ptr");
            llvm::Value *dataField = builder_.CreateStructGEP(listHeaderTy_, listPtr, 2, "rmat_data_field");
            llvm::Value *len = builder_.CreateLoad(i64Ty_, lenPtr, "rmat_len");
            llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataField, "rmat_data");

            // Bounds check
            llvm::Value *outOfBounds = builder_.CreateOr(
                builder_.CreateICmpSLT(idx, llvm::ConstantInt::get(i64Ty_, 0)),
                builder_.CreateICmpSGE(idx, len), "rmat_oob");
            llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, "rmat.err", fn_);
            llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "rmat.ok", fn_);
            builder_.CreateCondBr(outOfBounds, errBB, okBB);
            builder_.SetInsertPoint(errBB);
            emitRuntimeError("runtime error: remove_at() index out of bounds\n", ".rmat_oob_err");

            builder_.SetInsertPoint(okBB);
            // Save element to return
            llvm::Value *elemPtr = builder_.CreateGEP(elemTy, dataPtr, {idx}, "rmat_elem_ptr");
            llvm::Value *removedVal = builder_.CreateLoad(elemTy, elemPtr, "rmat_val");
            // memmove elements from idx+1 to idx
            llvm::Value *srcPtr = builder_.CreateGEP(elemTy, dataPtr,
                {builder_.CreateAdd(idx, llvm::ConstantInt::get(i64Ty_, 1))}, "rmat_src");
            llvm::Value *moveCount = builder_.CreateSub(
                builder_.CreateSub(len, idx), llvm::ConstantInt::get(i64Ty_, 1), "rmat_move_count");
            llvm::Value *moveBytes = builder_.CreateMul(moveCount, llvm::ConstantInt::get(i64Ty_, elemSize), "rmat_move_bytes");
            builder_.CreateCall(memmoveFn, {elemPtr, srcPtr, moveBytes});
            // len--
            llvm::Value *newLen = builder_.CreateSub(len, llvm::ConstantInt::get(i64Ty_, 1), "rmat_new_len");
            builder_.CreateStore(newLen, lenPtr);

            return removedVal;
        }
    }

    // ===== distinct(list) → new list with duplicates removed =====
    if (e.callee == "distinct" && e.args.size() == 1) {
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy)
            codegenError("distinct() requires a list as argument");

        // Reject non-string pointer elements (e.g. list-of-lists) — strcmp would be UB
        if (elemTy == ptrTy_ && getNestedListElementType(listVal))
            codegenError("distinct() is not supported for lists of non-string pointer elements");

        llvm::Value *srcLenPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 0, "dist_src_len_ptr");
        llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, srcLenPtr, "dist_src_len");
        llvm::Value *srcDataPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 2, "dist_src_data_field");
        llvm::Value *srcData = builder_.CreateLoad(ptrTy_, srcDataPtr, "dist_src_data");

        // Allocate new list (capacity = source length)
        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        uint64_t elemSize = dl.getTypeAllocSize(elemTy);

        llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "dist_header");
        llvm::Value *dataSize = builder_.CreateMul(srcLen, llvm::ConstantInt::get(i64Ty_, elemSize), "dist_data_size");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "dist_data");

        builder_.CreateStore(newData, builder_.CreateStructGEP(listHeaderTy_, newHeader, 2, "dist_data_field"));
        builder_.CreateStore(srcLen, builder_.CreateStructGEP(listHeaderTy_, newHeader, 1, "dist_cap_ptr"));

        // Output length counter
        llvm::AllocaInst *outLen = builder_.CreateAlloca(i64Ty_, nullptr, "dist_out_len");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), outLen);

        // Outer loop: for each source element
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "dist_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

        llvm::BasicBlock *outerCondBB = llvm::BasicBlock::Create(*ctx_, "dist.ocond", fn_);
        llvm::BasicBlock *outerBodyBB = llvm::BasicBlock::Create(*ctx_, "dist.obody", fn_);
        llvm::BasicBlock *outerEndBB = llvm::BasicBlock::Create(*ctx_, "dist.oend", fn_);

        builder_.CreateBr(outerCondBB);
        builder_.SetInsertPoint(outerCondBB);
        llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "dist_iv");
        builder_.CreateCondBr(builder_.CreateICmpSLT(iVal, srcLen), outerBodyBB, outerEndBB);

        builder_.SetInsertPoint(outerBodyBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "dist_ic");
        llvm::Value *srcElemPtr = builder_.CreateGEP(elemTy, srcData, {iCur}, "dist_src_ep");
        llvm::Value *srcElem = builder_.CreateLoad(elemTy, srcElemPtr, "dist_src_elem");

        // Inner loop: check if srcElem already exists in output
        llvm::AllocaInst *dupFound = builder_.CreateAlloca(i1Ty_, nullptr, "dist_dup");
        builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 0), dupFound);
        llvm::AllocaInst *jVar = builder_.CreateAlloca(i64Ty_, nullptr, "dist_j");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), jVar);

        llvm::BasicBlock *innerCondBB = llvm::BasicBlock::Create(*ctx_, "dist.icond", fn_);
        llvm::BasicBlock *innerBodyBB = llvm::BasicBlock::Create(*ctx_, "dist.ibody", fn_);
        llvm::BasicBlock *innerEndBB = llvm::BasicBlock::Create(*ctx_, "dist.iend", fn_);

        llvm::Value *curOutLen = builder_.CreateLoad(i64Ty_, outLen, "dist_cur_out");
        builder_.CreateBr(innerCondBB);

        builder_.SetInsertPoint(innerCondBB);
        llvm::Value *jVal = builder_.CreateLoad(i64Ty_, jVar, "dist_jv");
        llvm::Value *notDup = builder_.CreateICmpEQ(builder_.CreateLoad(i1Ty_, dupFound), llvm::ConstantInt::get(i1Ty_, 0), "dist_not_dup");
        llvm::Value *jInBounds = builder_.CreateICmpSLT(jVal, curOutLen, "dist_j_inb");
        llvm::Value *innerCont = builder_.CreateAnd(notDup, jInBounds, "dist_icont");
        builder_.CreateCondBr(innerCont, innerBodyBB, innerEndBB);

        builder_.SetInsertPoint(innerBodyBB);
        llvm::Value *jCur = builder_.CreateLoad(i64Ty_, jVar, "dist_jc");
        llvm::Value *outElemPtr = builder_.CreateGEP(elemTy, newData, {jCur}, "dist_out_ep");
        llvm::Value *outElem = builder_.CreateLoad(elemTy, outElemPtr, "dist_out_elem");

        llvm::Value *match;
        if (elemTy == ptrTy_) {
            auto strcmpFn = getStdlibStrcmp();
            llvm::Value *cmpResult = builder_.CreateCall(strcmpFn, {srcElem, outElem}, "dist_strcmp");
            match = builder_.CreateICmpEQ(cmpResult, llvm::ConstantInt::get(i32Ty_, 0), "dist_match");
        } else if (elemTy->isDoubleTy()) {
            match = builder_.CreateFCmpOEQ(srcElem, outElem, "dist_match");
        } else {
            match = builder_.CreateICmpEQ(srcElem, outElem, "dist_match");
        }

        llvm::BasicBlock *dupBB = llvm::BasicBlock::Create(*ctx_, "dist.dup", fn_);
        llvm::BasicBlock *innerNextBB = llvm::BasicBlock::Create(*ctx_, "dist.inext", fn_);
        builder_.CreateCondBr(match, dupBB, innerNextBB);

        builder_.SetInsertPoint(dupBB);
        builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 1), dupFound);
        builder_.CreateBr(innerCondBB);

        builder_.SetInsertPoint(innerNextBB);
        builder_.CreateStore(builder_.CreateAdd(jCur, llvm::ConstantInt::get(i64Ty_, 1)), jVar);
        builder_.CreateBr(innerCondBB);

        // After inner loop: if not duplicate, add to output
        builder_.SetInsertPoint(innerEndBB);
        llvm::Value *isDup = builder_.CreateLoad(i1Ty_, dupFound, "dist_is_dup");

        llvm::BasicBlock *addBB = llvm::BasicBlock::Create(*ctx_, "dist.add", fn_);
        llvm::BasicBlock *outerNextBB = llvm::BasicBlock::Create(*ctx_, "dist.onext", fn_);
        builder_.CreateCondBr(isDup, outerNextBB, addBB);

        builder_.SetInsertPoint(addBB);
        llvm::Value *outIdx = builder_.CreateLoad(i64Ty_, outLen, "dist_out_idx");
        llvm::Value *dstPtr = builder_.CreateGEP(elemTy, newData, {outIdx}, "dist_dst");
        builder_.CreateStore(srcElem, dstPtr);
        builder_.CreateStore(builder_.CreateAdd(outIdx, llvm::ConstantInt::get(i64Ty_, 1)), outLen);
        builder_.CreateBr(outerNextBB);

        builder_.SetInsertPoint(outerNextBB);
        builder_.CreateStore(builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        builder_.CreateBr(outerCondBB);

        // End: set final length
        builder_.SetInsertPoint(outerEndBB);
        llvm::Value *finalLen = builder_.CreateLoad(i64Ty_, outLen, "dist_final_len");
        builder_.CreateStore(finalLen, builder_.CreateStructGEP(listHeaderTy_, newHeader, 0, "dist_len_ptr"));

        list_element_types_[newHeader] = elemTy;
        return newHeader;
    }

    // ===== flatten(list) → flatten nested list one level =====
    if (e.callee == "flatten" && e.args.size() == 1) {
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Type *outerElemTy = getListElementType(listVal);
        if (!outerElemTy || outerElemTy != ptrTy_)
            codegenError("flatten() requires a list of lists");

        // Look up the inner element type
        llvm::Type *innerElemTy = getNestedListElementType(listVal);
        if (!innerElemTy)
            codegenError("flatten() cannot determine inner list element type; use a list literal (e.g. [[1, 2], [3, 4]])");

        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        uint64_t innerElemSize = dl.getTypeAllocSize(innerElemTy);

        auto mallocFn = getStdlibMalloc();
        auto memcpyFn = getStdlibMemcpy();

        llvm::Value *outerLenPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 0, "flat_olen_ptr");
        llvm::Value *outerLen = builder_.CreateLoad(i64Ty_, outerLenPtr, "flat_olen");
        llvm::Value *outerDataField = builder_.CreateStructGEP(listHeaderTy_, listVal, 2, "flat_odata_field");
        llvm::Value *outerData = builder_.CreateLoad(ptrTy_, outerDataField, "flat_odata");

        // Pass 1: sum all inner lengths
        llvm::AllocaInst *totalLen = builder_.CreateAlloca(i64Ty_, nullptr, "flat_total");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), totalLen);
        {
            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "flat_s_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "flat.s.cond", fn_);
            llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "flat.s.body", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "flat.s.end", fn_);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(condBB);
            llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "flat_si");
            builder_.CreateCondBr(builder_.CreateICmpSLT(i, outerLen), bodyBB, endBB);
            builder_.SetInsertPoint(bodyBB);
            llvm::Value *innerPtr = builder_.CreateGEP(ptrTy_, outerData, {i}, "flat_inner_ptr");
            llvm::Value *innerList = builder_.CreateLoad(ptrTy_, innerPtr, "flat_inner");
            llvm::Value *innerLenPtr = builder_.CreateStructGEP(listHeaderTy_, innerList, 0, "flat_ilen_ptr");
            llvm::Value *innerLen = builder_.CreateLoad(i64Ty_, innerLenPtr, "flat_ilen");
            llvm::Value *curTotal = builder_.CreateLoad(i64Ty_, totalLen, "flat_cur_total");
            builder_.CreateStore(builder_.CreateAdd(curTotal, innerLen), totalLen);
            builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(endBB);
        }

        // Allocate new list
        llvm::Value *total = builder_.CreateLoad(i64Ty_, totalLen, "flat_total_len");
        llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "flat_hdr");
        llvm::Value *dataSize = builder_.CreateMul(total, llvm::ConstantInt::get(i64Ty_, innerElemSize), "flat_ds");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "flat_data");

        // Set header
        builder_.CreateStore(total, builder_.CreateStructGEP(listHeaderTy_, newHeader, 0));
        builder_.CreateStore(total, builder_.CreateStructGEP(listHeaderTy_, newHeader, 1));
        builder_.CreateStore(newData, builder_.CreateStructGEP(listHeaderTy_, newHeader, 2));

        // Pass 2: copy each inner list's data
        llvm::AllocaInst *offset = builder_.CreateAlloca(i64Ty_, nullptr, "flat_off");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), offset);
        {
            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "flat_c_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "flat.c.cond", fn_);
            llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "flat.c.body", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "flat.c.end", fn_);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(condBB);
            llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "flat_ci");
            builder_.CreateCondBr(builder_.CreateICmpSLT(i, outerLen), bodyBB, endBB);
            builder_.SetInsertPoint(bodyBB);
            llvm::Value *innerPtr = builder_.CreateGEP(ptrTy_, outerData, {i}, "flat_c_inner_ptr");
            llvm::Value *innerList = builder_.CreateLoad(ptrTy_, innerPtr, "flat_c_inner");
            llvm::Value *innerLenPtr = builder_.CreateStructGEP(listHeaderTy_, innerList, 0, "flat_c_ilen_ptr");
            llvm::Value *innerLen = builder_.CreateLoad(i64Ty_, innerLenPtr, "flat_c_ilen");
            llvm::Value *innerDataField = builder_.CreateStructGEP(listHeaderTy_, innerList, 2, "flat_c_idata_field");
            llvm::Value *innerData = builder_.CreateLoad(ptrTy_, innerDataField, "flat_c_idata");

            llvm::Value *curOff = builder_.CreateLoad(i64Ty_, offset, "flat_cur_off");
            llvm::Value *dstPtr = builder_.CreateGEP(innerElemTy, newData, {curOff}, "flat_dst");
            llvm::Value *copyBytes = builder_.CreateMul(innerLen, llvm::ConstantInt::get(i64Ty_, innerElemSize), "flat_cb");
            builder_.CreateCall(memcpyFn, {dstPtr, innerData, copyBytes});
            builder_.CreateStore(builder_.CreateAdd(curOff, innerLen), offset);
            builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(endBB);
        }

        list_element_types_[newHeader] = innerElemTy;
        return newHeader;
    }

    // ===== items(map) → List<(K, V)> =====
    if (e.callee == "items" && e.args.size() == 1) {
        llvm::Value *mapPtr = emitExpr(*e.args[0]);
        llvm::Type *keyTy = getMapKeyType(mapPtr);
        llvm::Type *valTy = getMapValueType(mapPtr);
        if (keyTy && valTy) {
            llvm::Value *lenPtr = builder_.CreateStructGEP(mapHeaderTy_, mapPtr, 0, "items_len_ptr");
            llvm::Value *len = builder_.CreateLoad(i64Ty_, lenPtr, "items_len");
            llvm::Value *keysPtrField = builder_.CreateStructGEP(mapHeaderTy_, mapPtr, 2, "items_keys_field");
            llvm::Value *keysPtr = builder_.CreateLoad(ptrTy_, keysPtrField, "items_keys");
            llvm::Value *valsPtrField = builder_.CreateStructGEP(mapHeaderTy_, mapPtr, 3, "items_vals_field");
            llvm::Value *valsPtr = builder_.CreateLoad(ptrTy_, valsPtrField, "items_vals");

            llvm::StructType *tupleTy = llvm::StructType::get(*ctx_, {keyTy, valTy});
            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
            uint64_t tupleSize = dl.getTypeAllocSize(tupleTy);

            auto mallocFn = getStdlibMalloc();

            llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "items_hdr");
            llvm::Value *dataSize = builder_.CreateMul(len, llvm::ConstantInt::get(i64Ty_, tupleSize), "items_ds");
            llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "items_data");

            // Fill tuples
            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "items_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "items.cond", fn_);
            llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "items.body", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "items.end", fn_);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(condBB);
            llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "items_ci");
            builder_.CreateCondBr(builder_.CreateICmpSLT(i, len), bodyBB, endBB);
            builder_.SetInsertPoint(bodyBB);
            llvm::Value *kp = builder_.CreateGEP(keyTy, keysPtr, {i}, "items_kp");
            llvm::Value *vp = builder_.CreateGEP(valTy, valsPtr, {i}, "items_vp");
            llvm::Value *k = builder_.CreateLoad(keyTy, kp, "items_k");
            llvm::Value *v = builder_.CreateLoad(valTy, vp, "items_v");
            llvm::Value *tuple = llvm::UndefValue::get(tupleTy);
            tuple = builder_.CreateInsertValue(tuple, k, 0);
            tuple = builder_.CreateInsertValue(tuple, v, 1);
            llvm::Value *dp = builder_.CreateGEP(tupleTy, newData, {i}, "items_dp");
            builder_.CreateStore(tuple, dp);
            builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(endBB);

            builder_.CreateStore(len, builder_.CreateStructGEP(listHeaderTy_, newHeader, 0));
            builder_.CreateStore(len, builder_.CreateStructGEP(listHeaderTy_, newHeader, 1));
            builder_.CreateStore(newData, builder_.CreateStructGEP(listHeaderTy_, newHeader, 2));
            list_element_types_[newHeader] = tupleTy;
            return newHeader;
        }
    }

    // ===== get(map, key) — 2-arg → Option<V> =====
    if (e.callee == "get" && e.args.size() == 2) {
        llvm::Value *mapPtr = emitExpr(*e.args[0]);
        llvm::Type *keyTy = getMapKeyType(mapPtr);
        llvm::Type *valTy = getMapValueType(mapPtr);
        if (keyTy && valTy) {
            llvm::Value *key = emitExpr(*e.args[1]);
            if (key->getType() != keyTy)
                codegenError("get() key type mismatch");
            llvm::StructType *optTy = getOptionType(valTy);
            llvm::Value *idx = emitMapKeyLookup(mapPtr, key, keyTy);
            llvm::Value *found = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "get2_found");

            llvm::BasicBlock *foundBB = llvm::BasicBlock::Create(*ctx_, "get2.found", fn_);
            llvm::BasicBlock *notFoundBB = llvm::BasicBlock::Create(*ctx_, "get2.notfound", fn_);
            llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "get2.merge", fn_);
            builder_.CreateCondBr(found, foundBB, notFoundBB);

            builder_.SetInsertPoint(foundBB);
            llvm::Value *valsPtrField = builder_.CreateStructGEP(mapHeaderTy_, mapPtr, 3, "get2_vals_field");
            llvm::Value *valsPtr = builder_.CreateLoad(ptrTy_, valsPtrField, "get2_vals");
            llvm::Value *valPtr = builder_.CreateGEP(valTy, valsPtr, {idx}, "get2_val_ptr");
            llvm::Value *foundVal = builder_.CreateLoad(valTy, valPtr, "get2_val");
            llvm::Value *someVal = buildSomeValue(foundVal, optTy);
            builder_.CreateBr(mergeBB);
            llvm::BasicBlock *foundEndBB = builder_.GetInsertBlock();

            builder_.SetInsertPoint(notFoundBB);
            llvm::Value *noneVal = buildNoneValue(optTy);
            builder_.CreateBr(mergeBB);
            llvm::BasicBlock *notFoundEndBB = builder_.GetInsertBlock();

            builder_.SetInsertPoint(mergeBB);
            llvm::PHINode *phi = builder_.CreatePHI(optTy, 2, "get2_result");
            phi->addIncoming(someVal, foundEndBB);
            phi->addIncoming(noneVal, notFoundEndBB);
            return phi;
        }
    }

    // ===== get(map, key, default) — 3-arg =====
    if (e.callee == "get" && e.args.size() == 3) {
        llvm::Value *mapPtr = emitExpr(*e.args[0]);
        llvm::Type *keyTy = getMapKeyType(mapPtr);
        llvm::Type *valTy = getMapValueType(mapPtr);
        if (keyTy && valTy) {
            llvm::Value *key = emitExpr(*e.args[1]);
            if (key->getType() != keyTy)
                codegenError("get() key type mismatch");
            llvm::Value *defaultVal = emitExpr(*e.args[2]);
            if (defaultVal->getType() != valTy)
                codegenError("get() default value type must match map's value type");
            llvm::Value *idx = emitMapKeyLookup(mapPtr, key, keyTy);
            llvm::Value *found = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "get_found");

            llvm::BasicBlock *foundBB = llvm::BasicBlock::Create(*ctx_, "get.found", fn_);
            llvm::BasicBlock *notFoundBB = llvm::BasicBlock::Create(*ctx_, "get.notfound", fn_);
            llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "get.merge", fn_);
            builder_.CreateCondBr(found, foundBB, notFoundBB);

            builder_.SetInsertPoint(foundBB);
            llvm::Value *valsPtrField = builder_.CreateStructGEP(mapHeaderTy_, mapPtr, 3, "get_vals_field");
            llvm::Value *valsPtr = builder_.CreateLoad(ptrTy_, valsPtrField, "get_vals");
            llvm::Value *valPtr = builder_.CreateGEP(valTy, valsPtr, {idx}, "get_val_ptr");
            llvm::Value *foundVal = builder_.CreateLoad(valTy, valPtr, "get_val");
            llvm::BasicBlock *foundEndBB = builder_.GetInsertBlock();
            builder_.CreateBr(mergeBB);

            builder_.SetInsertPoint(notFoundBB);
            llvm::BasicBlock *notFoundEndBB = builder_.GetInsertBlock();
            builder_.CreateBr(mergeBB);

            builder_.SetInsertPoint(mergeBB);
            llvm::PHINode *phi = builder_.CreatePHI(valTy, 2, "get_result");
            phi->addIncoming(foundVal, foundEndBB);
            phi->addIncoming(defaultVal, notFoundEndBB);
            return phi;
        }
    }

    // ===== merge(map1, map2) → new map =====
    if (e.callee == "merge" && e.args.size() == 2) {
        llvm::Value *map1 = emitExpr(*e.args[0]);
        llvm::Value *map2 = emitExpr(*e.args[1]);
        llvm::Type *keyTy = getMapKeyType(map1);
        llvm::Type *valTy = getMapValueType(map1);
        if (!keyTy || !valTy)
            codegenError("merge() requires maps as arguments");
        {
            llvm::Type *keyTy2 = getMapKeyType(map2);
            llvm::Type *valTy2 = getMapValueType(map2);
            if (!keyTy2 || keyTy2 != keyTy || !valTy2 || valTy2 != valTy)
                codegenError("merge() requires two maps with the same key and value types");

            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t headerSize = dl.getTypeAllocSize(mapHeaderTy_);
            uint64_t keySize = dl.getTypeAllocSize(keyTy);
            uint64_t valSize = dl.getTypeAllocSize(valTy);

            auto mallocFn = getStdlibMalloc();
            auto memcpyFn = getStdlibMemcpy();

            llvm::Value *len1 = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(mapHeaderTy_, map1, 0), "mg_len1");
            llvm::Value *len2 = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(mapHeaderTy_, map2, 0), "mg_len2");
            llvm::Value *keys1 = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(mapHeaderTy_, map1, 2), "mg_keys1");
            llvm::Value *vals1 = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(mapHeaderTy_, map1, 3), "mg_vals1");
            llvm::Value *keys2 = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(mapHeaderTy_, map2, 2), "mg_keys2");
            llvm::Value *vals2 = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(mapHeaderTy_, map2, 3), "mg_vals2");

            // Allocate new map with capacity = len1 + len2
            llvm::Value *maxCap = builder_.CreateAdd(len1, len2, "mg_max_cap");
            llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "mg_hdr");
            llvm::Value *newKeysSize = builder_.CreateMul(maxCap, llvm::ConstantInt::get(i64Ty_, keySize), "mg_ks");
            llvm::Value *newKeys = builder_.CreateCall(mallocFn, {newKeysSize}, "mg_keys");
            llvm::Value *newValsSize = builder_.CreateMul(maxCap, llvm::ConstantInt::get(i64Ty_, valSize), "mg_vs");
            llvm::Value *newVals = builder_.CreateCall(mallocFn, {newValsSize}, "mg_vals");

            // Copy all of map1
            llvm::Value *copy1KeySize = builder_.CreateMul(len1, llvm::ConstantInt::get(i64Ty_, keySize), "mg_ck1");
            builder_.CreateCall(memcpyFn, {newKeys, keys1, copy1KeySize});
            llvm::Value *copy1ValSize = builder_.CreateMul(len1, llvm::ConstantInt::get(i64Ty_, valSize), "mg_cv1");
            builder_.CreateCall(memcpyFn, {newVals, vals1, copy1ValSize});

            // Set up header
            llvm::Value *lenPtr = builder_.CreateStructGEP(mapHeaderTy_, newHeader, 0, "mg_len_ptr");
            builder_.CreateStore(len1, lenPtr);
            builder_.CreateStore(maxCap, builder_.CreateStructGEP(mapHeaderTy_, newHeader, 1));
            builder_.CreateStore(newKeys, builder_.CreateStructGEP(mapHeaderTy_, newHeader, 2));
            builder_.CreateStore(newVals, builder_.CreateStructGEP(mapHeaderTy_, newHeader, 3));

            // Init hash buckets
            emitBucketInit(newHeader, mapHeaderTy_, 4, 5, 16);

            // Re-hash map1 keys into new map's buckets
            {
                llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "mg_rh_i");
                builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
                llvm::BasicBlock *rCondBB = llvm::BasicBlock::Create(*ctx_, "mg.rh.cond", fn_);
                llvm::BasicBlock *rBodyBB = llvm::BasicBlock::Create(*ctx_, "mg.rh.body", fn_);
                llvm::BasicBlock *rEndBB = llvm::BasicBlock::Create(*ctx_, "mg.rh.end", fn_);
                builder_.CreateBr(rCondBB);
                builder_.SetInsertPoint(rCondBB);
                llvm::Value *ri = builder_.CreateLoad(i64Ty_, iVar, "mg_ri");
                builder_.CreateCondBr(builder_.CreateICmpSLT(ri, len1), rBodyBB, rEndBB);
                builder_.SetInsertPoint(rBodyBB);
                llvm::Value *kp = builder_.CreateGEP(keyTy, newKeys, {ri}, "mg_rh_kp");
                llvm::Value *kv = builder_.CreateLoad(keyTy, kp, "mg_rh_kv");
                emitBucketInsertAndRehashCheck(newHeader, mapHeaderTy_, 0, 4, 5, kv, keyTy, ri);
                builder_.CreateStore(builder_.CreateAdd(ri, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
                builder_.CreateBr(rCondBB);
                builder_.SetInsertPoint(rEndBB);
            }

            // Add/update entries from map2
            {
                llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "mg_i2");
                builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
                llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "mg.add.cond", fn_);
                llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "mg.add.body", fn_);
                llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "mg.add.end", fn_);
                builder_.CreateBr(condBB);
                builder_.SetInsertPoint(condBB);
                llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "mg_ci");
                builder_.CreateCondBr(builder_.CreateICmpSLT(i, len2), bodyBB, endBB);

                builder_.SetInsertPoint(bodyBB);
                llvm::Value *kp = builder_.CreateGEP(keyTy, keys2, {i}, "mg_kp2");
                llvm::Value *kv = builder_.CreateLoad(keyTy, kp, "mg_kv2");
                llvm::Value *vp = builder_.CreateGEP(valTy, vals2, {i}, "mg_vp2");
                llvm::Value *vv = builder_.CreateLoad(valTy, vp, "mg_vv2");

                // Check if key exists in new map
                llvm::Value *lookupIdx = emitMapKeyLookup(newHeader, kv, keyTy);
                llvm::Value *exists = builder_.CreateICmpSGE(lookupIdx, llvm::ConstantInt::get(i64Ty_, 0), "mg_exists");

                llvm::BasicBlock *updateBB = llvm::BasicBlock::Create(*ctx_, "mg.update", fn_);
                llvm::BasicBlock *insertBB = llvm::BasicBlock::Create(*ctx_, "mg.insert", fn_);
                llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "mg.next", fn_);
                builder_.CreateCondBr(exists, updateBB, insertBB);

                // Update existing key's value
                builder_.SetInsertPoint(updateBB);
                llvm::Value *curVals = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(mapHeaderTy_, newHeader, 3), "mg_cur_vals");
                llvm::Value *updPtr = builder_.CreateGEP(valTy, curVals, {lookupIdx}, "mg_upd_ptr");
                builder_.CreateStore(vv, updPtr);
                builder_.CreateBr(nextBB);

                // Insert new key-value pair
                builder_.SetInsertPoint(insertBB);
                llvm::Value *curLen = builder_.CreateLoad(i64Ty_, lenPtr, "mg_cur_len");
                llvm::Value *curKeys = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(mapHeaderTy_, newHeader, 2), "mg_cur_keys");
                llvm::Value *newKeyPtr = builder_.CreateGEP(keyTy, curKeys, {curLen}, "mg_new_kp");
                builder_.CreateStore(kv, newKeyPtr);
                llvm::Value *curVals2 = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(mapHeaderTy_, newHeader, 3), "mg_cur_vals2");
                llvm::Value *newValPtr = builder_.CreateGEP(valTy, curVals2, {curLen}, "mg_new_vp");
                builder_.CreateStore(vv, newValPtr);
                builder_.CreateStore(builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1)), lenPtr);
                emitBucketInsertAndRehashCheck(newHeader, mapHeaderTy_, 0, 4, 5, kv, keyTy, curLen);
                builder_.CreateBr(nextBB);

                builder_.SetInsertPoint(nextBB);
                builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
                builder_.CreateBr(condBB);
                builder_.SetInsertPoint(endBB);
            }

            map_key_types_[newHeader] = keyTy;
            map_value_types_[newHeader] = valTy;
            return newHeader;
        }
    }


    return nullptr;
}

// ===== Builtin Set Ops =====

llvm::Value *CodeGen::emitBuiltinSetOps(const CallExpr &e) {
    // ===== Set operations =====

    // Helper lambda to create a new set from iteration
    // union(set1, set2)
    if (e.callee == "union" && e.args.size() == 2) {
        llvm::Value *set1 = emitExpr(*e.args[0]);
        llvm::Value *set2 = emitExpr(*e.args[1]);
        llvm::Type *elemTy = getSetElementType(set1);
        if (elemTy) {
            llvm::Type *elemTy2 = getSetElementType(set2);
            if (!elemTy2 || elemTy2 != elemTy)
                codegenError("union() requires two sets with the same element type");
            // Create new set with all elements from set1, then add elements from set2
            llvm::Value *len1 = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(setHeaderTy_, set1, 0), "u_len1");
            llvm::Value *len2 = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(setHeaderTy_, set2, 0), "u_len2");
            llvm::Value *data1 = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(setHeaderTy_, set1, 2), "u_data1");
            llvm::Value *data2 = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(setHeaderTy_, set2, 2), "u_data2");

            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t headerSize = dl.getTypeAllocSize(setHeaderTy_);
            uint64_t elemSize = dl.getTypeAllocSize(elemTy);
            auto mallocFn = getStdlibMalloc();

            // Allocate max possible size (len1 + len2)
            llvm::Value *maxLen = builder_.CreateAdd(len1, len2, "u_max_len");
            llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "u_hdr");
            llvm::Value *dataSize = builder_.CreateMul(maxLen, llvm::ConstantInt::get(i64Ty_, elemSize), "u_ds");
            llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "u_data");

            // Copy all of set1
            auto memcpyFn = getStdlibMemcpy();
            llvm::Value *copy1Size = builder_.CreateMul(len1, llvm::ConstantInt::get(i64Ty_, elemSize), "u_copy1_size");
            builder_.CreateCall(memcpyFn, {newData, data1, copy1Size});

            // Init header with len1
            llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, newHeader, 0, "u_len_ptr");
            builder_.CreateStore(len1, lenPtr);
            builder_.CreateStore(maxLen, builder_.CreateStructGEP(setHeaderTy_, newHeader, 1));
            builder_.CreateStore(newData, builder_.CreateStructGEP(setHeaderTy_, newHeader, 2));

            // Init buckets for the new set
            emitBucketInit(newHeader, setHeaderTy_, 3, 4, 16);

            // Re-hash all elements from set1 into new set's buckets
            {
                llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "u_rehash_i");
                builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
                llvm::BasicBlock *rCondBB = llvm::BasicBlock::Create(*ctx_, "u.rehash.cond", fn_);
                llvm::BasicBlock *rBodyBB = llvm::BasicBlock::Create(*ctx_, "u.rehash.body", fn_);
                llvm::BasicBlock *rEndBB = llvm::BasicBlock::Create(*ctx_, "u.rehash.end", fn_);
                builder_.CreateBr(rCondBB);
                builder_.SetInsertPoint(rCondBB);
                llvm::Value *ri = builder_.CreateLoad(i64Ty_, iVar, "u_ri");
                builder_.CreateCondBr(builder_.CreateICmpSLT(ri, len1), rBodyBB, rEndBB);
                builder_.SetInsertPoint(rBodyBB);
                llvm::Value *ep = builder_.CreateGEP(elemTy, newData, {ri}, "u_rehash_ep");
                llvm::Value *ev = builder_.CreateLoad(elemTy, ep, "u_rehash_ev");
                emitBucketInsertAndRehashCheck(newHeader, setHeaderTy_, 0, 3, 4, ev, elemTy, ri);
                builder_.CreateStore(builder_.CreateAdd(ri, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
                builder_.CreateBr(rCondBB);
                builder_.SetInsertPoint(rEndBB);
            }

            // Add elements from set2 that are not in set1
            {
                llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "u_i2");
                builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
                llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "u.add.cond", fn_);
                llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "u.add.body", fn_);
                llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "u.add.end", fn_);
                builder_.CreateBr(condBB);
                builder_.SetInsertPoint(condBB);
                llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "u_ci");
                builder_.CreateCondBr(builder_.CreateICmpSLT(i, len2), bodyBB, endBB);
                builder_.SetInsertPoint(bodyBB);
                llvm::Value *ep = builder_.CreateGEP(elemTy, data2, {i}, "u_ep2");
                llvm::Value *ev = builder_.CreateLoad(elemTy, ep, "u_ev2");

                // Check if already in new set
                llvm::Value *lookupIdx = emitSetElementLookup(newHeader, ev, elemTy);
                llvm::Value *notFound = builder_.CreateICmpSLT(lookupIdx, llvm::ConstantInt::get(i64Ty_, 0), "u_not_found");
                llvm::BasicBlock *addBB = llvm::BasicBlock::Create(*ctx_, "u.add.do", fn_);
                llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "u.add.next", fn_);
                builder_.CreateCondBr(notFound, addBB, nextBB);

                builder_.SetInsertPoint(addBB);
                llvm::Value *curLen = builder_.CreateLoad(i64Ty_, lenPtr, "u_cur_len");
                llvm::Value *storePtr = builder_.CreateGEP(elemTy, newData, {curLen}, "u_store_ptr");
                builder_.CreateStore(ev, storePtr);
                emitBucketInsertAndRehashCheck(newHeader, setHeaderTy_, 0, 3, 4, ev, elemTy, curLen);
                builder_.CreateStore(builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1)), lenPtr);
                builder_.CreateBr(nextBB);

                builder_.SetInsertPoint(nextBB);
                builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
                builder_.CreateBr(condBB);
                builder_.SetInsertPoint(endBB);
            }

            set_element_types_[newHeader] = elemTy;
            return newHeader;
        }
    }

    // intersection(set1, set2)
    if (e.callee == "intersection" && e.args.size() == 2) {
        llvm::Value *set1 = emitExpr(*e.args[0]);
        llvm::Value *set2 = emitExpr(*e.args[1]);
        llvm::Type *elemTy = getSetElementType(set1);
        if (elemTy) {
            llvm::Type *elemTy2 = getSetElementType(set2);
            if (!elemTy2 || elemTy2 != elemTy)
                codegenError("intersection() requires two sets with the same element type");
            llvm::Value *len1 = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(setHeaderTy_, set1, 0), "is_len1");
            llvm::Value *data1 = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(setHeaderTy_, set1, 2), "is_data1");

            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t headerSize = dl.getTypeAllocSize(setHeaderTy_);
            uint64_t elemSize = dl.getTypeAllocSize(elemTy);
            auto mallocFn = getStdlibMalloc();

            llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "is_hdr");
            llvm::Value *dataSize = builder_.CreateMul(len1, llvm::ConstantInt::get(i64Ty_, elemSize), "is_ds");
            llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "is_data");

            llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, newHeader, 0, "is_len_ptr");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), lenPtr);
            builder_.CreateStore(len1, builder_.CreateStructGEP(setHeaderTy_, newHeader, 1));
            builder_.CreateStore(newData, builder_.CreateStructGEP(setHeaderTy_, newHeader, 2));
            emitBucketInit(newHeader, setHeaderTy_, 3, 4, 16);

            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "is_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "is.cond", fn_);
            llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "is.body", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "is.end", fn_);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(condBB);
            llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "is_ci");
            builder_.CreateCondBr(builder_.CreateICmpSLT(i, len1), bodyBB, endBB);
            builder_.SetInsertPoint(bodyBB);
            llvm::Value *ep = builder_.CreateGEP(elemTy, data1, {i}, "is_ep");
            llvm::Value *ev = builder_.CreateLoad(elemTy, ep, "is_ev");

            llvm::Value *inSet2 = emitSetElementLookup(set2, ev, elemTy);
            llvm::Value *found = builder_.CreateICmpSGE(inSet2, llvm::ConstantInt::get(i64Ty_, 0), "is_found");
            llvm::BasicBlock *addBB = llvm::BasicBlock::Create(*ctx_, "is.add", fn_);
            llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "is.next", fn_);
            builder_.CreateCondBr(found, addBB, nextBB);

            builder_.SetInsertPoint(addBB);
            llvm::Value *curLen = builder_.CreateLoad(i64Ty_, lenPtr, "is_cur_len");
            llvm::Value *storePtr = builder_.CreateGEP(elemTy, newData, {curLen}, "is_store_ptr");
            builder_.CreateStore(ev, storePtr);
            emitBucketInsertAndRehashCheck(newHeader, setHeaderTy_, 0, 3, 4, ev, elemTy, curLen);
            builder_.CreateStore(builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1)), lenPtr);
            builder_.CreateBr(nextBB);

            builder_.SetInsertPoint(nextBB);
            builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(endBB);

            set_element_types_[newHeader] = elemTy;
            return newHeader;
        }
    }

    // difference(set1, set2) — elements in set1 not in set2
    if (e.callee == "difference" && e.args.size() == 2) {
        llvm::Value *set1 = emitExpr(*e.args[0]);
        llvm::Value *set2 = emitExpr(*e.args[1]);
        llvm::Type *elemTy = getSetElementType(set1);
        if (elemTy) {
            llvm::Type *elemTy2 = getSetElementType(set2);
            if (!elemTy2 || elemTy2 != elemTy)
                codegenError("difference() requires two sets with the same element type");
            llvm::Value *len1 = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(setHeaderTy_, set1, 0), "df_len1");
            llvm::Value *data1 = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(setHeaderTy_, set1, 2), "df_data1");

            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t headerSize = dl.getTypeAllocSize(setHeaderTy_);
            uint64_t elemSize = dl.getTypeAllocSize(elemTy);
            auto mallocFn = getStdlibMalloc();

            llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "df_hdr");
            llvm::Value *dataSize = builder_.CreateMul(len1, llvm::ConstantInt::get(i64Ty_, elemSize), "df_ds");
            llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "df_data");

            llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, newHeader, 0, "df_len_ptr");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), lenPtr);
            builder_.CreateStore(len1, builder_.CreateStructGEP(setHeaderTy_, newHeader, 1));
            builder_.CreateStore(newData, builder_.CreateStructGEP(setHeaderTy_, newHeader, 2));
            emitBucketInit(newHeader, setHeaderTy_, 3, 4, 16);

            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "df_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "df.cond", fn_);
            llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "df.body", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "df.end", fn_);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(condBB);
            llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "df_ci");
            builder_.CreateCondBr(builder_.CreateICmpSLT(i, len1), bodyBB, endBB);
            builder_.SetInsertPoint(bodyBB);
            llvm::Value *ep = builder_.CreateGEP(elemTy, data1, {i}, "df_ep");
            llvm::Value *ev = builder_.CreateLoad(elemTy, ep, "df_ev");

            llvm::Value *inSet2 = emitSetElementLookup(set2, ev, elemTy);
            llvm::Value *notFound = builder_.CreateICmpSLT(inSet2, llvm::ConstantInt::get(i64Ty_, 0), "df_not_found");
            llvm::BasicBlock *addBB = llvm::BasicBlock::Create(*ctx_, "df.add", fn_);
            llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "df.next", fn_);
            builder_.CreateCondBr(notFound, addBB, nextBB);

            builder_.SetInsertPoint(addBB);
            llvm::Value *curLen = builder_.CreateLoad(i64Ty_, lenPtr, "df_cur_len");
            llvm::Value *storePtr = builder_.CreateGEP(elemTy, newData, {curLen}, "df_store_ptr");
            builder_.CreateStore(ev, storePtr);
            emitBucketInsertAndRehashCheck(newHeader, setHeaderTy_, 0, 3, 4, ev, elemTy, curLen);
            builder_.CreateStore(builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1)), lenPtr);
            builder_.CreateBr(nextBB);

            builder_.SetInsertPoint(nextBB);
            builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(endBB);

            set_element_types_[newHeader] = elemTy;
            return newHeader;
        }
    }

    // symmetric_difference(set1, set2)
    if (e.callee == "symmetric_difference" && e.args.size() == 2) {
        llvm::Value *set1 = emitExpr(*e.args[0]);
        llvm::Value *set2 = emitExpr(*e.args[1]);
        llvm::Type *elemTy = getSetElementType(set1);
        if (elemTy) {
            llvm::Type *elemTy2 = getSetElementType(set2);
            if (!elemTy2 || elemTy2 != elemTy)
                codegenError("symmetric_difference() requires two sets with the same element type");
            llvm::Value *len1 = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(setHeaderTy_, set1, 0), "sd_len1");
            llvm::Value *len2 = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(setHeaderTy_, set2, 0), "sd_len2");
            llvm::Value *data1 = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(setHeaderTy_, set1, 2), "sd_data1");
            llvm::Value *data2 = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(setHeaderTy_, set2, 2), "sd_data2");

            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t headerSize = dl.getTypeAllocSize(setHeaderTy_);
            uint64_t elemSize = dl.getTypeAllocSize(elemTy);
            auto mallocFn = getStdlibMalloc();

            llvm::Value *maxLen = builder_.CreateAdd(len1, len2, "sd_max_len");
            llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "sd_hdr");
            llvm::Value *dataSize = builder_.CreateMul(maxLen, llvm::ConstantInt::get(i64Ty_, elemSize), "sd_ds");
            llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "sd_data");

            llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, newHeader, 0, "sd_len_ptr");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), lenPtr);
            builder_.CreateStore(maxLen, builder_.CreateStructGEP(setHeaderTy_, newHeader, 1));
            builder_.CreateStore(newData, builder_.CreateStructGEP(setHeaderTy_, newHeader, 2));
            emitBucketInit(newHeader, setHeaderTy_, 3, 4, 16);

            // Add elements from set1 not in set2
            auto emitSetDiffLoop = [&](llvm::Value *srcData, llvm::Value *srcLen, llvm::Value *otherSet, const std::string &prefix) {
                llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, prefix + "_i");
                builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
                llvm::BasicBlock *cBB = llvm::BasicBlock::Create(*ctx_, prefix + ".cond", fn_);
                llvm::BasicBlock *bBB = llvm::BasicBlock::Create(*ctx_, prefix + ".body", fn_);
                llvm::BasicBlock *eBB = llvm::BasicBlock::Create(*ctx_, prefix + ".end", fn_);
                builder_.CreateBr(cBB);
                builder_.SetInsertPoint(cBB);
                llvm::Value *ci = builder_.CreateLoad(i64Ty_, iVar, prefix + "_ci");
                builder_.CreateCondBr(builder_.CreateICmpSLT(ci, srcLen), bBB, eBB);
                builder_.SetInsertPoint(bBB);
                llvm::Value *ePtr = builder_.CreateGEP(elemTy, srcData, {ci}, prefix + "_ep");
                llvm::Value *eVal = builder_.CreateLoad(elemTy, ePtr, prefix + "_ev");
                llvm::Value *inOther = emitSetElementLookup(otherSet, eVal, elemTy);
                llvm::Value *notInOther = builder_.CreateICmpSLT(inOther, llvm::ConstantInt::get(i64Ty_, 0), prefix + "_nf");
                llvm::BasicBlock *aBB = llvm::BasicBlock::Create(*ctx_, prefix + ".add", fn_);
                llvm::BasicBlock *nBB = llvm::BasicBlock::Create(*ctx_, prefix + ".next", fn_);
                builder_.CreateCondBr(notInOther, aBB, nBB);
                builder_.SetInsertPoint(aBB);
                llvm::Value *curLen = builder_.CreateLoad(i64Ty_, lenPtr, prefix + "_cl");
                llvm::Value *sp = builder_.CreateGEP(elemTy, newData, {curLen}, prefix + "_sp");
                builder_.CreateStore(eVal, sp);
                emitBucketInsertAndRehashCheck(newHeader, setHeaderTy_, 0, 3, 4, eVal, elemTy, curLen);
                builder_.CreateStore(builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1)), lenPtr);
                builder_.CreateBr(nBB);
                builder_.SetInsertPoint(nBB);
                builder_.CreateStore(builder_.CreateAdd(ci, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
                builder_.CreateBr(cBB);
                builder_.SetInsertPoint(eBB);
            };

            emitSetDiffLoop(data1, len1, set2, "sd1");
            emitSetDiffLoop(data2, len2, set1, "sd2");

            set_element_types_[newHeader] = elemTy;
            return newHeader;
        }
    }

    // is_subset(set1, set2)
    if (e.callee == "is_subset" && e.args.size() == 2) {
        llvm::Value *set1 = emitExpr(*e.args[0]);
        llvm::Value *set2 = emitExpr(*e.args[1]);
        llvm::Type *elemTy = getSetElementType(set1);
        if (elemTy) {
            llvm::Type *elemTy2 = getSetElementType(set2);
            if (!elemTy2 || elemTy2 != elemTy)
                codegenError("is_subset() requires two sets with the same element type");
            llvm::Value *len1 = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(setHeaderTy_, set1, 0), "sub_len1");
            llvm::Value *data1 = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(setHeaderTy_, set1, 2), "sub_data1");

            llvm::AllocaInst *resultVar = builder_.CreateAlloca(i1Ty_, nullptr, "sub_result");
            builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 1), resultVar);

            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "sub_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "sub.cond", fn_);
            llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "sub.body", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "sub.end", fn_);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(condBB);
            llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "sub_ci");
            builder_.CreateCondBr(builder_.CreateICmpSLT(i, len1), bodyBB, endBB);
            builder_.SetInsertPoint(bodyBB);
            llvm::Value *ep = builder_.CreateGEP(elemTy, data1, {i}, "sub_ep");
            llvm::Value *ev = builder_.CreateLoad(elemTy, ep, "sub_ev");
            llvm::Value *inSet2 = emitSetElementLookup(set2, ev, elemTy);
            llvm::Value *notFound = builder_.CreateICmpSLT(inSet2, llvm::ConstantInt::get(i64Ty_, 0), "sub_nf");
            llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, "sub.fail", fn_);
            llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "sub.next", fn_);
            builder_.CreateCondBr(notFound, failBB, nextBB);
            builder_.SetInsertPoint(failBB);
            builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 0), resultVar);
            builder_.CreateBr(endBB);  // early exit
            builder_.SetInsertPoint(nextBB);
            builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(endBB);

            return builder_.CreateLoad(i1Ty_, resultVar, "is_subset_result");
        }
    }

    // is_superset(set1, set2) = is_subset(set2, set1)
    if (e.callee == "is_superset" && e.args.size() == 2) {
        llvm::Value *set1 = emitExpr(*e.args[0]);
        llvm::Value *set2 = emitExpr(*e.args[1]);
        llvm::Type *elemTy = getSetElementType(set1);
        if (elemTy) {
            llvm::Type *elemTy2 = getSetElementType(set2);
            if (!elemTy2 || elemTy2 != elemTy)
                codegenError("is_superset() requires two sets with the same element type");
            // Check all elements of set2 are in set1
            llvm::Value *len2 = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(setHeaderTy_, set2, 0), "sup_len2");
            llvm::Value *data2 = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(setHeaderTy_, set2, 2), "sup_data2");

            llvm::AllocaInst *resultVar = builder_.CreateAlloca(i1Ty_, nullptr, "sup_result");
            builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 1), resultVar);

            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "sup_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "sup.cond", fn_);
            llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "sup.body", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "sup.end", fn_);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(condBB);
            llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "sup_ci");
            builder_.CreateCondBr(builder_.CreateICmpSLT(i, len2), bodyBB, endBB);
            builder_.SetInsertPoint(bodyBB);
            llvm::Value *ep = builder_.CreateGEP(elemTy, data2, {i}, "sup_ep");
            llvm::Value *ev = builder_.CreateLoad(elemTy, ep, "sup_ev");
            llvm::Value *inSet1 = emitSetElementLookup(set1, ev, elemTy);
            llvm::Value *notFound = builder_.CreateICmpSLT(inSet1, llvm::ConstantInt::get(i64Ty_, 0), "sup_nf");
            llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, "sup.fail", fn_);
            llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "sup.next", fn_);
            builder_.CreateCondBr(notFound, failBB, nextBB);
            builder_.SetInsertPoint(failBB);
            builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 0), resultVar);
            builder_.CreateBr(endBB);
            builder_.SetInsertPoint(nextBB);
            builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(endBB);

            return builder_.CreateLoad(i1Ty_, resultVar, "is_superset_result");
        }
    }

    return nullptr;
}

// ===== CallExpr Dispatcher =====

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<CallExpr> &e) {
    // ADT constructor: Enum::Variant(args...)
    {
        auto colonPos = e->callee.find("::");
        if (colonPos != std::string::npos) {
            std::string enumName = e->callee.substr(0, colonPos);
            std::string variantName = e->callee.substr(colonPos + 2);
            // Try to instantiate generic enum if not found
            if (!enum_types_.count(enumName)) {
                auto ltPos = enumName.find('<');
                if (ltPos != std::string::npos && enumName.back() == '>') {
                    std::string baseName = enumName.substr(0, ltPos);
                    std::string argsStr = enumName.substr(ltPos + 1, enumName.size() - ltPos - 2);
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
                    instantiateGenericEnum(enumName, baseName, typeArgs);
                }
            }
            auto eit = enum_types_.find(enumName);
            if (eit != enum_types_.end() && eit->second.isADT) {
                auto &info = eit->second;
                auto vit = info.variants.find(variantName);
                if (vit == info.variants.end())
                    codegenError("unknown variant '" + variantName + "' in enum '" + enumName + "'");
                int64_t tag = vit->second;

                auto fit = info.variantFields.find(variantName);
                if (fit == info.variantFields.end())
                    codegenError("variant '" + variantName + "' has no associated data");
                auto &fieldInfo = fit->second;
                if (e->args.size() != fieldInfo.fieldTypes.size())
                    codegenError("variant '" + variantName + "' expects " +
                        std::to_string(fieldInfo.fieldTypes.size()) + " arguments");

                llvm::Value *adtVal = llvm::UndefValue::get(info.adtType);
                adtVal = builder_.CreateInsertValue(adtVal, llvm::ConstantInt::get(i64Ty_, tag), 0, "adt.tag");

                const llvm::DataLayout &dl = mod_->getDataLayout();
                llvm::AllocaInst *tmpAlloca = builder_.CreateAlloca(info.adtType, nullptr, "adt.tmp");
                builder_.CreateStore(adtVal, tmpAlloca);
                llvm::Value *payloadPtr = builder_.CreateStructGEP(info.adtType, tmpAlloca, 1, "adt.payload");

                size_t offset = 0;
                for (size_t i = 0; i < e->args.size(); ++i) {
                    llvm::Value *argVal = emitExpr(*e->args[i]);
                    uint64_t align = dl.getABITypeAlign(fieldInfo.fieldTypes[i]).value();
                    offset = (offset + align - 1) / align * align;
                    llvm::Value *fieldPtr = builder_.CreateGEP(
                        llvm::Type::getInt8Ty(*ctx_), payloadPtr,
                        {llvm::ConstantInt::get(i64Ty_, offset)}, "adt.field." + std::to_string(i));
                    builder_.CreateStore(argVal, fieldPtr);
                    offset += dl.getTypeAllocSize(fieldInfo.fieldTypes[i]);
                }

                llvm::Value *result = builder_.CreateLoad(info.adtType, tmpAlloca, "adt.val");
                enum_value_types_[result] = enumName;
                return result;
            }
        }
    }

    // verify(fn_name) → call count
    if (e->callee == "verify") {
        if (!test_mode_)
            codegenError("'verify' is only allowed in test mode (use 'ry test')");
        if (e->args.size() != 1)
            codegenError("verify() requires exactly 1 argument");
        auto *strExpr = std::get_if<StringExpr>(&e->args[0]->data);
        if (!strExpr)
            codegenError("verify() argument must be a function name");
        auto vit = functions_.find(strExpr->value);
        if (vit == functions_.end())
            codegenError("verify(): unknown function '" + strExpr->value + "'");
        if (vit->second.size() != 1)
            codegenError("verify(): overloaded functions are not supported");
        llvm::FunctionType *getCountTy = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
        llvm::FunctionCallee getCountFn = mod_->getOrInsertFunction("__ry_mock_get_call_count", getCountTy);
        llvm::Value *nameStr = builder_.CreateGlobalString(strExpr->value, ".verify_name");
        return builder_.CreateCall(getCountFn, {nameStr}, "call_count");
    }

    // Validate @native fn type signatures before dispatch
    validateNativeCallArgs(e->callee, e->args);

    // Dispatch to category helpers
    if (auto *v = emitBuiltinIterator(*e))   return v;
    if (auto *v = emitBuiltinString(*e))     return v;
    if (auto *v = emitBuiltinConversion(*e)) return v;
    if (auto *v = emitBuiltinQuery(*e))      return v;
    if (auto *v = emitBuiltinCore(*e))       return v;
    if (auto *v = emitBuiltinHigherOrder(*e)) return v;
    if (auto *v = emitBuiltinCollection(*e)) return v;
    if (auto *v = emitBuiltinSetOps(*e))     return v;
    if (auto *v = emitBuiltinRegex(*e))     return v;
    if (auto *v = emitBuiltinMath(*e))      return v;

    // Struct constructor
    auto sit = struct_types_.find(e->callee);
    if (sit != struct_types_.end()) {
        if (deprecated_types_.count(e->callee))
            emitDeprecationWarning(e->callee);
        return emitStructConstructor(sit->second, e->callee, e->args);
    }

    // Try indirect call via variable (function pointer / lambda)
    if (llvm::AllocaInst *varPtr = findVar(e->callee)) {
        auto fnIt = fn_type_info_.find(varPtr);
        if (fnIt != fn_type_info_.end()) {
            auto &info = fnIt->second;

            std::vector<llvm::Value*> argVals;
            for (auto &arg : e->args)
                argVals.push_back(emitExpr(*arg));

            if (argVals.size() != info.paramTypes.size())
                codegenError(
                    "lambda call: expected " + std::to_string(info.paramTypes.size()) +
                    " arguments, got " + std::to_string(argVals.size()));

            for (size_t i = 0; i < argVals.size(); ++i) {
                if (argVals[i]->getType() != info.paramTypes[i])
                    codegenError(
                        "lambda call: argument " + std::to_string(i) + " type mismatch");
            }

            llvm::Value *loaded = builder_.CreateLoad(ptrTy_, varPtr, e->callee + ".fn");
            return emitLambdaCall(loaded, info, argVals, "indirect_call");
        }
    }

    return emitUserFnCall(e->callee, e->args);
}

// ===== Lambda call helper =====

llvm::Value *CodeGen::emitLambdaCall(llvm::Value *lambdaVal, const FnTypeInfo &info,
                                      std::vector<llvm::Value*> args, const std::string &name) {
    if (info.capturedVars.empty()) {
        llvm::FunctionType *ft = llvm::FunctionType::get(
            info.returnType, info.paramTypes, false);
        if (info.returnType->isVoidTy())
            return builder_.CreateCall(ft, lambdaVal, args);
        return builder_.CreateCall(ft, lambdaVal, args, name);
    } else {
        std::vector<llvm::Type*> closureFields;
        closureFields.push_back(ptrTy_);
        for (auto *ct : info.capturedTypes)
            closureFields.push_back(ct);
        llvm::StructType *closureTy = llvm::StructType::get(*ctx_, closureFields);

        llvm::Value *fnPtrField = builder_.CreateStructGEP(
            closureTy, lambdaVal, 0, "lcall.fn_ptr");
        llvm::Value *fnPtr = builder_.CreateLoad(ptrTy_, fnPtrField, "lcall.fn");

        std::vector<llvm::Value*> fullArgs = args;
        std::vector<llvm::Type*> allParamTypes = info.paramTypes;
        for (size_t i = 0; i < info.capturedTypes.size(); ++i) {
            llvm::Value *capField = builder_.CreateStructGEP(
                closureTy, lambdaVal, i + 1, "lcall.cap." + std::to_string(i));
            llvm::Value *capVal = builder_.CreateLoad(
                info.capturedTypes[i], capField, "lcall.cap_val." + std::to_string(i));
            fullArgs.push_back(capVal);
            allParamTypes.push_back(info.capturedTypes[i]);
        }

        llvm::FunctionType *ft = llvm::FunctionType::get(
            info.returnType, allParamTypes, false);
        if (info.returnType->isVoidTy())
            return builder_.CreateCall(ft, fnPtr, fullArgs);
        return builder_.CreateCall(ft, fnPtr, fullArgs, name);
    }
}

// ===== Builtin Regex =====

llvm::Value *CodeGen::emitBuiltinRegex(const CallExpr &e) {
    // regex_match(pattern, text) -> bool
    if (e.callee == "regex_match") {
        if (e.args.size() != 2)
            codegenError("regex_match() takes exactly 2 arguments");
        llvm::Value *pattern = emitExpr(*e.args[0]);
        llvm::Value *text = emitExpr(*e.args[1]);
        if (pattern->getType() != ptrTy_ || text->getType() != ptrTy_)
            codegenError("regex_match() requires str arguments");
        auto fnTy = llvm::FunctionType::get(i64Ty_, {ptrTy_, ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_regex_match", fnTy);
        llvm::Value *result = builder_.CreateCall(fn, {pattern, text}, "regex_match");
        return builder_.CreateTrunc(result, i1Ty_, "regex_match_bool");
    }

    // regex_search(pattern, text) -> int
    if (e.callee == "regex_search") {
        if (e.args.size() != 2)
            codegenError("regex_search() takes exactly 2 arguments");
        llvm::Value *pattern = emitExpr(*e.args[0]);
        llvm::Value *text = emitExpr(*e.args[1]);
        if (pattern->getType() != ptrTy_ || text->getType() != ptrTy_)
            codegenError("regex_search() requires str arguments");
        auto fnTy = llvm::FunctionType::get(i64Ty_, {ptrTy_, ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_regex_search", fnTy);
        return builder_.CreateCall(fn, {pattern, text}, "regex_search");
    }

    // regex_replace(pattern, text, replacement) -> str
    if (e.callee == "regex_replace") {
        if (e.args.size() != 3)
            codegenError("regex_replace() takes exactly 3 arguments");
        llvm::Value *pattern = emitExpr(*e.args[0]);
        llvm::Value *text = emitExpr(*e.args[1]);
        llvm::Value *replacement = emitExpr(*e.args[2]);
        if (pattern->getType() != ptrTy_ || text->getType() != ptrTy_ ||
            replacement->getType() != ptrTy_)
            codegenError("regex_replace() requires str arguments");
        auto fnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_regex_replace", fnTy);
        return builder_.CreateCall(fn, {pattern, text, replacement}, "regex_replace");
    }

    // regex_split(pattern, text) -> List<str>
    if (e.callee == "regex_split") {
        if (e.args.size() != 2)
            codegenError("regex_split() takes exactly 2 arguments");
        llvm::Value *pattern = emitExpr(*e.args[0]);
        llvm::Value *text = emitExpr(*e.args[1]);
        if (pattern->getType() != ptrTy_ || text->getType() != ptrTy_)
            codegenError("regex_split() requires str arguments");
        auto fnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_regex_split", fnTy);
        llvm::Value *result = builder_.CreateCall(fn, {pattern, text}, "regex_split");
        list_element_types_[result] = ptrTy_;
        return result;
    }

    // regex_find_all(pattern, text) -> List<str>
    if (e.callee == "regex_find_all") {
        if (e.args.size() != 2)
            codegenError("regex_find_all() takes exactly 2 arguments");
        llvm::Value *pattern = emitExpr(*e.args[0]);
        llvm::Value *text = emitExpr(*e.args[1]);
        if (pattern->getType() != ptrTy_ || text->getType() != ptrTy_)
            codegenError("regex_find_all() requires str arguments");
        auto fnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_regex_find_all", fnTy);
        llvm::Value *result = builder_.CreateCall(fn, {pattern, text}, "regex_find_all");
        list_element_types_[result] = ptrTy_;
        return result;
    }

    return nullptr;
}

// ===== Builtin Math =====

llvm::Value *CodeGen::emitBuiltinMath(const CallExpr &e) {
    // Only dispatch if the callee was declared via @native (i.e., explicitly imported)
    if (!native_fn_arg_counts_.count(e.callee))
        return nullptr;

    // PI() -> float
    if (e.callee == "PI") {
        if (!e.args.empty())
            codegenError("PI() takes no arguments");
        return llvm::ConstantFP::get(f64Ty_, 3.141592653589793);
    }

    // E() -> float
    if (e.callee == "E") {
        if (!e.args.empty())
            codegenError("E() takes no arguments");
        return llvm::ConstantFP::get(f64Ty_, 2.718281828459045);
    }

    // inf() -> float
    if (e.callee == "inf") {
        if (!e.args.empty())
            codegenError("inf() takes no arguments");
        return llvm::ConstantFP::getInfinity(f64Ty_);
    }

    // nan() -> float
    if (e.callee == "nan") {
        if (!e.args.empty())
            codegenError("nan() takes no arguments");
        return llvm::ConstantFP::getNaN(f64Ty_);
    }

    // Helper: get fabs C function
    auto getFabs = [&]() {
        auto ty = llvm::FunctionType::get(f64Ty_, {f64Ty_}, false);
        return mod_->getOrInsertFunction("fabs", ty);
    };

    // abs(int) -> int, abs(float) -> float
    if (e.callee == "abs") {
        if (e.args.size() != 1)
            codegenError("abs() takes exactly 1 argument");
        llvm::Value *x = emitExpr(*e.args[0]);
        if (x->getType() == f64Ty_)
            return builder_.CreateCall(getFabs(), {x}, "abs");
        if (x->getType()->isIntegerTy(64)) {
            llvm::Value *neg = builder_.CreateNeg(x, "neg");
            llvm::Value *isNeg = builder_.CreateICmpSLT(x, llvm::ConstantInt::get(i64Ty_, 0), "is_neg");
            return builder_.CreateSelect(isNeg, neg, x, "abs");
        }
        codegenError("abs() requires int or float argument");
    }

    // floor/ceil/round(float) -> int
    if (e.callee == "floor" || e.callee == "ceil" || e.callee == "round") {
        if (e.args.size() != 1)
            codegenError(e.callee + "() takes exactly 1 argument");
        llvm::Value *x = emitExpr(*e.args[0]);
        if (x->getType() != f64Ty_)
            codegenError(e.callee + "() requires float argument");

        // Runtime check: reject NaN and values outside i64 range
        llvm::Value *isNan = builder_.CreateFCmpUNO(x, x, "is_nan_chk");
        llvm::Value *absVal = builder_.CreateCall(getFabs(), {x}, "abs_chk");
        // 2^63 = 9.223372036854776e+18 — values >= this overflow i64
        llvm::Value *limit = llvm::ConstantFP::get(f64Ty_, 9.223372036854776e+18);
        llvm::Value *tooBig = builder_.CreateFCmpOGE(absVal, limit, "too_big_chk");
        llvm::Value *invalid = builder_.CreateOr(isNan, tooBig, "invalid_chk");

        llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, e.callee + ".fail", fn_);
        llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, e.callee + ".ok", fn_);
        builder_.CreateCondBr(invalid, failBB, okBB);

        builder_.SetInsertPoint(failBB);
        static int mathErrCounter = 0;
        emitRuntimeError("runtime error: " + e.callee + "() argument out of int range\n",
                          ".math_err_" + std::to_string(mathErrCounter++));

        builder_.SetInsertPoint(okBB);
        auto fnTy = llvm::FunctionType::get(f64Ty_, {f64Ty_}, false);
        auto fn = mod_->getOrInsertFunction(e.callee, fnTy);
        llvm::Value *result = builder_.CreateCall(fn, {x}, e.callee);
        return builder_.CreateFPToSI(result, i64Ty_, e.callee + "_i");
    }

    // 1-arg float -> float: sqrt, log, log2, log10, exp, sin, cos, tan, asin, acos, atan
    {
        static const std::unordered_set<std::string> oneArgFloat = {
            "sqrt", "log", "log2", "log10", "exp",
            "sin", "cos", "tan", "asin", "acos", "atan"
        };
        if (oneArgFloat.count(e.callee)) {
            if (e.args.size() != 1)
                codegenError(e.callee + "() takes exactly 1 argument");
            llvm::Value *x = emitExpr(*e.args[0]);
            if (x->getType() != f64Ty_)
                codegenError(e.callee + "() requires float argument");
            auto fnTy = llvm::FunctionType::get(f64Ty_, {f64Ty_}, false);
            auto fn = mod_->getOrInsertFunction(e.callee, fnTy);
            return builder_.CreateCall(fn, {x}, e.callee);
        }
    }

    // 2-arg float -> float: pow, atan2, hypot
    {
        static const std::unordered_set<std::string> twoArgFloat = {
            "pow", "atan2", "hypot"
        };
        if (twoArgFloat.count(e.callee)) {
            if (e.args.size() != 2)
                codegenError(e.callee + "() takes exactly 2 arguments");
            llvm::Value *x = emitExpr(*e.args[0]);
            llvm::Value *y = emitExpr(*e.args[1]);
            if (x->getType() != f64Ty_ || y->getType() != f64Ty_)
                codegenError(e.callee + "() requires float arguments");
            auto fnTy = llvm::FunctionType::get(f64Ty_, {f64Ty_, f64Ty_}, false);
            auto fn = mod_->getOrInsertFunction(e.callee, fnTy);
            return builder_.CreateCall(fn, {x, y}, e.callee);
        }
    }

    // is_nan(float) -> bool
    if (e.callee == "is_nan") {
        if (e.args.size() != 1)
            codegenError("is_nan() takes exactly 1 argument");
        llvm::Value *x = emitExpr(*e.args[0]);
        if (x->getType() != f64Ty_)
            codegenError("is_nan() requires float argument");
        return builder_.CreateFCmpUNO(x, x, "is_nan");
    }

    // is_inf(float) -> bool
    if (e.callee == "is_inf") {
        if (e.args.size() != 1)
            codegenError("is_inf() takes exactly 1 argument");
        llvm::Value *x = emitExpr(*e.args[0]);
        if (x->getType() != f64Ty_)
            codegenError("is_inf() requires float argument");
        llvm::Value *absVal = builder_.CreateCall(getFabs(), {x}, "abs_for_inf");
        llvm::Value *posInf = llvm::ConstantFP::getInfinity(f64Ty_);
        return builder_.CreateFCmpOEQ(absVal, posInf, "is_inf");
    }

    return nullptr;
}

void CodeGen::validateNativeCallArgs(const std::string &callee,
                                      const std::vector<ExprPtr> &args) {
    auto it = native_fn_arg_counts_.find(callee);
    if (it == native_fn_arg_counts_.end()) return;

    const auto &counts = it->second;

    for (size_t count : counts) {
        if (count == args.size())
            return;
    }

    // Deduplicate and sort counts for clear error messages
    std::vector<size_t> unique_counts(counts.begin(), counts.end());
    std::sort(unique_counts.begin(), unique_counts.end());
    unique_counts.erase(std::unique(unique_counts.begin(), unique_counts.end()), unique_counts.end());

    std::string expected;
    for (size_t i = 0; i < unique_counts.size(); ++i) {
        if (i > 0) expected += " or ";
        expected += std::to_string(unique_counts[i]);
    }
    codegenError(callee + "() expects " + expected +
        " argument(s), but got " + std::to_string(args.size()));
}

// ===== Builtin Iterator =====

// Helper: allocate IteratorHeader {next_fn, state} and track element type
static llvm::Value *emitIteratorHeaderAlloc(
    CodeGen &cg, llvm::IRBuilder<> &builder, llvm::Module &mod,
    llvm::StructType *iterHeaderTy, llvm::Type *i64Ty, llvm::Type *ptrTy,
    llvm::Function *nextFn, llvm::Value *stateAlloc, llvm::Type *elemTy,
    std::unordered_map<llvm::Value*, llvm::Type*> &elemTypes,
    const std::string &name) {
    llvm::FunctionType *mallocTy = llvm::FunctionType::get(ptrTy, {i64Ty}, false);
    llvm::FunctionCallee mallocFn = mod.getOrInsertFunction("malloc", mallocTy);
    uint64_t headerSize = mod.getDataLayout().getTypeAllocSize(iterHeaderTy);
    llvm::Value *header = builder.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty, headerSize)}, name);
    builder.CreateStore(nextFn, builder.CreateStructGEP(iterHeaderTy, header, 0));
    builder.CreateStore(stateAlloc, builder.CreateStructGEP(iterHeaderTy, header, 1));
    elemTypes[header] = elemTy;
    return header;
}

// Helper: load {next_fn, state} from an IteratorHeader
static std::pair<llvm::Value*, llvm::Value*> loadIteratorFields(
    llvm::IRBuilder<> &builder, llvm::StructType *iterHeaderTy,
    llvm::Type *ptrTy, llvm::Value *iterVal, const std::string &prefix) {
    llvm::Value *nfField = builder.CreateStructGEP(iterHeaderTy, iterVal, 0, prefix + "_nf");
    llvm::Value *nf = builder.CreateLoad(ptrTy, nfField, prefix + "_next_fn");
    llvm::Value *stField = builder.CreateStructGEP(iterHeaderTy, iterVal, 1, prefix + "_st");
    llvm::Value *st = builder.CreateLoad(ptrTy, stField, prefix + "_state");
    return {nf, st};
}

llvm::Value *CodeGen::emitBuiltinIterator(const CallExpr &e) {
    // iter(collection) → Iterator
    if (e.callee == "iter" && e.args.size() == 1) {
        llvm::Value *collVal = emitExpr(*e.args[0]);
        if (collVal->getType() != ptrTy_)
            return nullptr;

        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();

        // Helper lambda: generate a dense-array next function for List/Set
        // State: { ptr data, i64 length, i64 index }
        auto emitDenseIterator = [&](llvm::Type *elemTy, llvm::StructType *collHeaderTy,
                                     unsigned dataPtrIdx, unsigned lenIdx,
                                     const std::string &kind) -> llvm::Value* {
            llvm::StructType *stateTy = llvm::StructType::get(*ctx_, {ptrTy_, i64Ty_, i64Ty_});
            uint64_t stateSize = dl.getTypeAllocSize(stateTy);

            std::string fnName = "__iter_" + kind + "_next." + std::to_string(iterator_fn_counter_++);
            llvm::StructType *optTy = getOptionType(elemTy);
            llvm::FunctionType *nextFnTy = llvm::FunctionType::get(optTy, {ptrTy_}, false);
            llvm::Function *nextFn = llvm::Function::Create(
                nextFnTy, llvm::Function::ExternalLinkage, fnName, *mod_);

            {
                FnScope guard(*this);
                fn_ = nextFn;
                pushScope();
                llvm::BasicBlock *entry = llvm::BasicBlock::Create(*ctx_, "entry", nextFn);
                builder_.SetInsertPoint(entry);

                llvm::Value *statePtr = nextFn->getArg(0);
                llvm::Value *data = builder_.CreateLoad(ptrTy_,
                    builder_.CreateStructGEP(stateTy, statePtr, 0), "data");
                llvm::Value *len = builder_.CreateLoad(i64Ty_,
                    builder_.CreateStructGEP(stateTy, statePtr, 1), "len");
                llvm::Value *idxField = builder_.CreateStructGEP(stateTy, statePtr, 2, "idx_field");
                llvm::Value *idx = builder_.CreateLoad(i64Ty_, idxField, "idx");

                llvm::BasicBlock *someBB = llvm::BasicBlock::Create(*ctx_, "some", nextFn);
                llvm::BasicBlock *noneBB = llvm::BasicBlock::Create(*ctx_, "none", nextFn);
                builder_.CreateCondBr(builder_.CreateICmpSLT(idx, len, "in_bounds"), someBB, noneBB);

                builder_.SetInsertPoint(someBB);
                llvm::Value *elem = builder_.CreateLoad(elemTy,
                    builder_.CreateGEP(elemTy, data, {idx}, "elem_ptr"), "elem");
                builder_.CreateStore(
                    builder_.CreateAdd(idx, llvm::ConstantInt::get(i64Ty_, 1), "next_idx"), idxField);
                builder_.CreateRet(buildSomeValue(elem, optTy));

                builder_.SetInsertPoint(noneBB);
                builder_.CreateRet(buildNoneValue(optTy));
                popScope();
            }

            // Allocate and fill state
            llvm::Value *stateAlloc = builder_.CreateCall(
                mallocFn, {llvm::ConstantInt::get(i64Ty_, stateSize)}, "iter_state");
            llvm::Value *srcData = builder_.CreateLoad(ptrTy_,
                builder_.CreateStructGEP(collHeaderTy, collVal, dataPtrIdx), "src_data");
            llvm::Value *srcLen = builder_.CreateLoad(i64Ty_,
                builder_.CreateStructGEP(collHeaderTy, collVal, lenIdx), "src_len");
            builder_.CreateStore(srcData, builder_.CreateStructGEP(stateTy, stateAlloc, 0));
            builder_.CreateStore(srcLen, builder_.CreateStructGEP(stateTy, stateAlloc, 1));
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0),
                builder_.CreateStructGEP(stateTy, stateAlloc, 2));

            return emitIteratorHeaderAlloc(*this, builder_, *mod_, iteratorHeaderTy_,
                i64Ty_, ptrTy_, nextFn, stateAlloc, elemTy, iterator_element_types_, "iter_header");
        };

        // Try List (data at index 2, len at index 0)
        if (llvm::Type *elemTy = getListElementType(collVal))
            return emitDenseIterator(elemTy, listHeaderTy_, 2, 0, "list");

        // Try Set (data at index 2, len at index 0)
        if (llvm::Type *setElemTy = getSetElementType(collVal))
            return emitDenseIterator(setElemTy, setHeaderTy_, 2, 0, "set");

        // Try Map → Iterator over (K, V) tuples
        llvm::Type *keyTy = getMapKeyType(collVal);
        llvm::Type *valTy = getMapValueType(collVal);
        if (keyTy && valTy) {
            llvm::StructType *tupleTy = llvm::StructType::get(*ctx_, {keyTy, valTy});
            llvm::StructType *stateTy = llvm::StructType::get(*ctx_, {ptrTy_, ptrTy_, i64Ty_, i64Ty_});
            uint64_t stateSize = dl.getTypeAllocSize(stateTy);

            std::string fnName = "__iter_map_next." + std::to_string(iterator_fn_counter_++);
            llvm::StructType *optTy = getOptionType(tupleTy);
            llvm::FunctionType *nextFnTy = llvm::FunctionType::get(optTy, {ptrTy_}, false);
            llvm::Function *nextFn = llvm::Function::Create(
                nextFnTy, llvm::Function::ExternalLinkage, fnName, *mod_);

            {
                FnScope guard(*this);
                fn_ = nextFn;
                pushScope();
                llvm::BasicBlock *entry = llvm::BasicBlock::Create(*ctx_, "entry", nextFn);
                builder_.SetInsertPoint(entry);

                llvm::Value *statePtr = nextFn->getArg(0);
                llvm::Value *keys = builder_.CreateLoad(ptrTy_,
                    builder_.CreateStructGEP(stateTy, statePtr, 0), "keys");
                llvm::Value *vals = builder_.CreateLoad(ptrTy_,
                    builder_.CreateStructGEP(stateTy, statePtr, 1), "vals");
                llvm::Value *len = builder_.CreateLoad(i64Ty_,
                    builder_.CreateStructGEP(stateTy, statePtr, 2), "len");
                llvm::Value *idxField = builder_.CreateStructGEP(stateTy, statePtr, 3, "idx_field");
                llvm::Value *idx = builder_.CreateLoad(i64Ty_, idxField, "idx");

                llvm::BasicBlock *someBB = llvm::BasicBlock::Create(*ctx_, "some", nextFn);
                llvm::BasicBlock *noneBB = llvm::BasicBlock::Create(*ctx_, "none", nextFn);
                builder_.CreateCondBr(builder_.CreateICmpSLT(idx, len, "in_bounds"), someBB, noneBB);

                builder_.SetInsertPoint(someBB);
                llvm::Value *key = builder_.CreateLoad(keyTy,
                    builder_.CreateGEP(keyTy, keys, {idx}, "key_ptr"), "key");
                llvm::Value *val = builder_.CreateLoad(valTy,
                    builder_.CreateGEP(valTy, vals, {idx}, "val_ptr"), "val");
                llvm::Value *tuple = llvm::UndefValue::get(tupleTy);
                tuple = builder_.CreateInsertValue(tuple, key, 0, "tuple_k");
                tuple = builder_.CreateInsertValue(tuple, val, 1, "tuple_kv");
                builder_.CreateStore(
                    builder_.CreateAdd(idx, llvm::ConstantInt::get(i64Ty_, 1), "next_idx"), idxField);
                builder_.CreateRet(buildSomeValue(tuple, optTy));

                builder_.SetInsertPoint(noneBB);
                builder_.CreateRet(buildNoneValue(optTy));
                popScope();
            }

            llvm::Value *stateAlloc = builder_.CreateCall(
                mallocFn, {llvm::ConstantInt::get(i64Ty_, stateSize)}, "iter_state");
            builder_.CreateStore(
                builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(mapHeaderTy_, collVal, 2)),
                builder_.CreateStructGEP(stateTy, stateAlloc, 0));
            builder_.CreateStore(
                builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(mapHeaderTy_, collVal, 3)),
                builder_.CreateStructGEP(stateTy, stateAlloc, 1));
            builder_.CreateStore(
                builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(mapHeaderTy_, collVal, 0)),
                builder_.CreateStructGEP(stateTy, stateAlloc, 2));
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0),
                builder_.CreateStructGEP(stateTy, stateAlloc, 3));

            return emitIteratorHeaderAlloc(*this, builder_, *mod_, iteratorHeaderTy_,
                i64Ty_, ptrTy_, nextFn, stateAlloc, tupleTy, iterator_element_types_, "iter_header");
        }

        codegenError("iter() argument must be a List, Set, or Map");
    }

    // to_list() → collect Iterator into List
    if (e.callee == "to_list" && e.args.size() == 1) {
        llvm::Value *iterVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getIteratorElementType(iterVal);
        if (!elemTy) return nullptr;

        auto mallocFn = getStdlibMalloc();
        auto reallocFn = getStdlibRealloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t elemSize = dl.getTypeAllocSize(elemTy);

        auto [nextFnPtr, statePtr] = loadIteratorFields(
            builder_, iteratorHeaderTy_, ptrTy_, iterVal, "tl");

        // Allocate list header
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        llvm::Value *headerPtr = builder_.CreateCall(
            mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "tl_header");

        // Initial capacity = 8
        llvm::AllocaInst *capVar = builder_.CreateAlloca(i64Ty_, nullptr, "tl_cap");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 8), capVar);
        llvm::AllocaInst *lenVar = builder_.CreateAlloca(i64Ty_, nullptr, "tl_len");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), lenVar);
        llvm::AllocaInst *dataVar = builder_.CreateAlloca(ptrTy_, nullptr, "tl_data_var");
        llvm::Value *initData = builder_.CreateCall(
            mallocFn, {llvm::ConstantInt::get(i64Ty_, elemSize * 8)}, "tl_init_data");
        builder_.CreateStore(initData, dataVar);

        llvm::StructType *optTy = getOptionType(elemTy);
        llvm::FunctionType *nextCallTy = llvm::FunctionType::get(optTy, {ptrTy_}, false);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "tl.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "tl.body", fn_);
        llvm::BasicBlock *growBB = llvm::BasicBlock::Create(*ctx_, "tl.grow", fn_);
        llvm::BasicBlock *storeBB = llvm::BasicBlock::Create(*ctx_, "tl.store", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "tl.end", fn_);

        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(condBB);
        llvm::Value *opt = builder_.CreateCall(nextCallTy, nextFnPtr, {statePtr}, "tl_opt");
        llvm::Value *hasVal = builder_.CreateExtractValue(opt, 0, "tl_has");
        builder_.CreateCondBr(hasVal, bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        llvm::Value *elem = builder_.CreateExtractValue(opt, 1, "tl_elem");
        llvm::Value *curLen = builder_.CreateLoad(i64Ty_, lenVar, "tl_cur_len");
        llvm::Value *curCap = builder_.CreateLoad(i64Ty_, capVar, "tl_cur_cap");
        builder_.CreateCondBr(builder_.CreateICmpEQ(curLen, curCap, "tl_need_grow"), growBB, storeBB);

        builder_.SetInsertPoint(growBB);
        llvm::Value *newCap = builder_.CreateMul(curCap, llvm::ConstantInt::get(i64Ty_, 2), "tl_new_cap");
        builder_.CreateStore(newCap, capVar);
        llvm::Value *newData = builder_.CreateCall(reallocFn, {
            builder_.CreateLoad(ptrTy_, dataVar, "tl_old_data"),
            builder_.CreateMul(newCap, llvm::ConstantInt::get(i64Ty_, elemSize), "tl_new_size")
        }, "tl_new_data");
        builder_.CreateStore(newData, dataVar);
        builder_.CreateBr(storeBB);

        builder_.SetInsertPoint(storeBB);
        llvm::Value *storeLen = builder_.CreateLoad(i64Ty_, lenVar, "tl_store_len");
        llvm::Value *storeData = builder_.CreateLoad(ptrTy_, dataVar, "tl_store_data");
        builder_.CreateStore(elem, builder_.CreateGEP(elemTy, storeData, {storeLen}, "tl_dst_ptr"));
        builder_.CreateStore(
            builder_.CreateAdd(storeLen, llvm::ConstantInt::get(i64Ty_, 1), "tl_new_len"), lenVar);
        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(endBB);
        builder_.CreateStore(builder_.CreateLoad(i64Ty_, lenVar, "tl_final_len"),
            builder_.CreateStructGEP(listHeaderTy_, headerPtr, 0));
        builder_.CreateStore(builder_.CreateLoad(i64Ty_, capVar, "tl_final_cap"),
            builder_.CreateStructGEP(listHeaderTy_, headerPtr, 1));
        builder_.CreateStore(builder_.CreateLoad(ptrTy_, dataVar, "tl_final_data"),
            builder_.CreateStructGEP(listHeaderTy_, headerPtr, 2));

        list_element_types_[headerPtr] = elemTy;

        // Propagate nested list metadata for flatten() support
        {
            llvm::Type *nestedTy = getNestedListElementType(iterVal);
            if (nestedTy)
                nested_list_element_types_[headerPtr] = nestedTy;
        }

        return headerPtr;
    }

    // next() → call next_fn(state) on Iterator
    if (e.callee == "next" && e.args.size() == 1) {
        llvm::Value *iterVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getIteratorElementType(iterVal);
        if (!elemTy) return nullptr;

        auto [nextFnPtr, statePtr] = loadIteratorFields(
            builder_, iteratorHeaderTy_, ptrTy_, iterVal, "next");
        llvm::StructType *optTy = getOptionType(elemTy);
        llvm::FunctionType *nextCallTy = llvm::FunctionType::get(optTy, {ptrTy_}, false);
        return builder_.CreateCall(nextCallTy, nextFnPtr, {statePtr}, "next_result");
    }

    // filter(iter, predicate) → new Iterator
    if (e.callee == "filter" && e.args.size() == 2) {
        llvm::Value *iterVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getIteratorElementType(iterVal);
        if (!elemTy) return nullptr;

        llvm::Value *lambdaVal = emitExpr(*e.args[1]);
        auto fnIt = fn_type_info_.find(lambdaVal);
        if (fnIt == fn_type_info_.end()) {
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(lambdaVal))
                fnIt = fn_type_info_.find(load->getPointerOperand());
        }
        if (fnIt == fn_type_info_.end())
            codegenError("filter() on iterator requires a predicate function");
        auto &info = fnIt->second;

        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();

        // State: { ptr src_next_fn, ptr src_state, ptr predicate }
        llvm::StructType *stateTy = llvm::StructType::get(*ctx_, {ptrTy_, ptrTy_, ptrTy_});

        std::string fnName = "__iter_filter_next." + std::to_string(iterator_fn_counter_++);
        llvm::StructType *optTy = getOptionType(elemTy);
        llvm::FunctionType *nextFnTy = llvm::FunctionType::get(optTy, {ptrTy_}, false);
        llvm::Function *filterNextFn = llvm::Function::Create(
            nextFnTy, llvm::Function::ExternalLinkage, fnName, *mod_);

        {
            FnScope guard(*this);
            fn_ = filterNextFn;
            pushScope();
            llvm::BasicBlock *entry = llvm::BasicBlock::Create(*ctx_, "entry", filterNextFn);
            builder_.SetInsertPoint(entry);

            llvm::Value *statePtr = filterNextFn->getArg(0);
            llvm::Value *srcNextFn = builder_.CreateLoad(ptrTy_,
                builder_.CreateStructGEP(stateTy, statePtr, 0), "src_next_fn");
            llvm::Value *srcState = builder_.CreateLoad(ptrTy_,
                builder_.CreateStructGEP(stateTy, statePtr, 1), "src_state");
            llvm::Value *predPtr = builder_.CreateLoad(ptrTy_,
                builder_.CreateStructGEP(stateTy, statePtr, 2), "pred_ptr");

            llvm::FunctionType *srcNextCallTy = llvm::FunctionType::get(optTy, {ptrTy_}, false);
            llvm::BasicBlock *loopBB = llvm::BasicBlock::Create(*ctx_, "loop", filterNextFn);
            builder_.CreateBr(loopBB);

            builder_.SetInsertPoint(loopBB);
            llvm::Value *opt = builder_.CreateCall(srcNextCallTy, srcNextFn, {srcState}, "src_opt");
            llvm::Value *hasVal = builder_.CreateExtractValue(opt, 0, "has_val");
            llvm::BasicBlock *checkBB = llvm::BasicBlock::Create(*ctx_, "check", filterNextFn);
            llvm::BasicBlock *noneBB = llvm::BasicBlock::Create(*ctx_, "none", filterNextFn);
            builder_.CreateCondBr(hasVal, checkBB, noneBB);

            builder_.SetInsertPoint(checkBB);
            llvm::Value *elem = builder_.CreateExtractValue(opt, 1, "elem");
            llvm::Value *predResult = emitLambdaCall(predPtr, info, {elem}, "pred_result");
            llvm::BasicBlock *matchBB = llvm::BasicBlock::Create(*ctx_, "match", filterNextFn);
            builder_.CreateCondBr(predResult, matchBB, loopBB);

            builder_.SetInsertPoint(matchBB);
            builder_.CreateRet(buildSomeValue(elem, optTy));

            builder_.SetInsertPoint(noneBB);
            builder_.CreateRet(buildNoneValue(optTy));
            popScope();
        }

        auto [srcNf, srcSt] = loadIteratorFields(
            builder_, iteratorHeaderTy_, ptrTy_, iterVal, "filter");
        uint64_t stateSize = dl.getTypeAllocSize(stateTy);
        llvm::Value *stateAlloc = builder_.CreateCall(
            mallocFn, {llvm::ConstantInt::get(i64Ty_, stateSize)}, "filter_state");
        builder_.CreateStore(srcNf, builder_.CreateStructGEP(stateTy, stateAlloc, 0));
        builder_.CreateStore(srcSt, builder_.CreateStructGEP(stateTy, stateAlloc, 1));
        builder_.CreateStore(lambdaVal, builder_.CreateStructGEP(stateTy, stateAlloc, 2));

        return emitIteratorHeaderAlloc(*this, builder_, *mod_, iteratorHeaderTy_,
            i64Ty_, ptrTy_, filterNextFn, stateAlloc, elemTy, iterator_element_types_, "filter_iter");
    }

    // map(iter, transform) → new Iterator with transformed element type
    if (e.callee == "map" && e.args.size() == 2) {
        llvm::Value *iterVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getIteratorElementType(iterVal);
        if (!elemTy) return nullptr;

        llvm::Value *lambdaVal = emitExpr(*e.args[1]);
        auto fnIt = fn_type_info_.find(lambdaVal);
        if (fnIt == fn_type_info_.end()) {
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(lambdaVal))
                fnIt = fn_type_info_.find(load->getPointerOperand());
        }
        if (fnIt == fn_type_info_.end())
            codegenError("map() on iterator requires a transform function");
        auto &info = fnIt->second;
        llvm::Type *outElemTy = info.returnType;

        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();

        llvm::StructType *stateTy = llvm::StructType::get(*ctx_, {ptrTy_, ptrTy_, ptrTy_});

        std::string fnName = "__iter_map_next." + std::to_string(iterator_fn_counter_++);
        llvm::StructType *srcOptTy = getOptionType(elemTy);
        llvm::StructType *outOptTy = getOptionType(outElemTy);
        llvm::FunctionType *nextFnTy = llvm::FunctionType::get(outOptTy, {ptrTy_}, false);
        llvm::Function *mapNextFn = llvm::Function::Create(
            nextFnTy, llvm::Function::ExternalLinkage, fnName, *mod_);

        {
            FnScope guard(*this);
            fn_ = mapNextFn;
            pushScope();
            llvm::BasicBlock *entry = llvm::BasicBlock::Create(*ctx_, "entry", mapNextFn);
            builder_.SetInsertPoint(entry);

            llvm::Value *statePtr = mapNextFn->getArg(0);
            llvm::Value *srcNextFn = builder_.CreateLoad(ptrTy_,
                builder_.CreateStructGEP(stateTy, statePtr, 0), "src_next_fn");
            llvm::Value *srcState = builder_.CreateLoad(ptrTy_,
                builder_.CreateStructGEP(stateTy, statePtr, 1), "src_state");
            llvm::Value *transPtr = builder_.CreateLoad(ptrTy_,
                builder_.CreateStructGEP(stateTy, statePtr, 2), "trans_ptr");

            llvm::FunctionType *srcNextCallTy = llvm::FunctionType::get(srcOptTy, {ptrTy_}, false);
            llvm::Value *opt = builder_.CreateCall(srcNextCallTy, srcNextFn, {srcState}, "src_opt");
            llvm::Value *hasVal = builder_.CreateExtractValue(opt, 0, "has_val");

            llvm::BasicBlock *someBB = llvm::BasicBlock::Create(*ctx_, "some", mapNextFn);
            llvm::BasicBlock *noneBB = llvm::BasicBlock::Create(*ctx_, "none", mapNextFn);
            builder_.CreateCondBr(hasVal, someBB, noneBB);

            builder_.SetInsertPoint(someBB);
            llvm::Value *elem = builder_.CreateExtractValue(opt, 1, "elem");
            builder_.CreateRet(buildSomeValue(emitLambdaCall(transPtr, info, {elem}, "mapped"), outOptTy));

            builder_.SetInsertPoint(noneBB);
            builder_.CreateRet(buildNoneValue(outOptTy));
            popScope();
        }

        auto [srcNf, srcSt] = loadIteratorFields(
            builder_, iteratorHeaderTy_, ptrTy_, iterVal, "map");
        uint64_t stateSize = dl.getTypeAllocSize(stateTy);
        llvm::Value *stateAlloc = builder_.CreateCall(
            mallocFn, {llvm::ConstantInt::get(i64Ty_, stateSize)}, "map_state");
        builder_.CreateStore(srcNf, builder_.CreateStructGEP(stateTy, stateAlloc, 0));
        builder_.CreateStore(srcSt, builder_.CreateStructGEP(stateTy, stateAlloc, 1));
        builder_.CreateStore(lambdaVal, builder_.CreateStructGEP(stateTy, stateAlloc, 2));

        return emitIteratorHeaderAlloc(*this, builder_, *mod_, iteratorHeaderTy_,
            i64Ty_, ptrTy_, mapNextFn, stateAlloc, outElemTy, iterator_element_types_, "map_iter");
    }

    // take(iter, n) → new Iterator that yields at most n elements
    if (e.callee == "take" && e.args.size() == 2) {
        llvm::Value *iterVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getIteratorElementType(iterVal);
        if (!elemTy) return nullptr;

        llvm::Value *n = emitExpr(*e.args[1]);

        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();

        llvm::StructType *stateTy = llvm::StructType::get(*ctx_, {ptrTy_, ptrTy_, i64Ty_});

        std::string fnName = "__iter_take_next." + std::to_string(iterator_fn_counter_++);
        llvm::StructType *optTy = getOptionType(elemTy);
        llvm::FunctionType *nextFnTy = llvm::FunctionType::get(optTy, {ptrTy_}, false);
        llvm::Function *takeNextFn = llvm::Function::Create(
            nextFnTy, llvm::Function::ExternalLinkage, fnName, *mod_);

        {
            FnScope guard(*this);
            fn_ = takeNextFn;
            pushScope();
            llvm::BasicBlock *entry = llvm::BasicBlock::Create(*ctx_, "entry", takeNextFn);
            builder_.SetInsertPoint(entry);

            llvm::Value *statePtr = takeNextFn->getArg(0);
            llvm::Value *srcNextFn = builder_.CreateLoad(ptrTy_,
                builder_.CreateStructGEP(stateTy, statePtr, 0), "src_next_fn");
            llvm::Value *srcState = builder_.CreateLoad(ptrTy_,
                builder_.CreateStructGEP(stateTy, statePtr, 1), "src_state");
            llvm::Value *remField = builder_.CreateStructGEP(stateTy, statePtr, 2, "rem_f");
            llvm::Value *remaining = builder_.CreateLoad(i64Ty_, remField, "remaining");

            llvm::BasicBlock *callBB = llvm::BasicBlock::Create(*ctx_, "call", takeNextFn);
            llvm::BasicBlock *noneBB = llvm::BasicBlock::Create(*ctx_, "none", takeNextFn);
            builder_.CreateCondBr(
                builder_.CreateICmpSGT(remaining, llvm::ConstantInt::get(i64Ty_, 0), "has_rem"),
                callBB, noneBB);

            builder_.SetInsertPoint(callBB);
            builder_.CreateStore(
                builder_.CreateSub(remaining, llvm::ConstantInt::get(i64Ty_, 1), "new_rem"), remField);
            llvm::FunctionType *srcNextCallTy = llvm::FunctionType::get(optTy, {ptrTy_}, false);
            builder_.CreateRet(builder_.CreateCall(srcNextCallTy, srcNextFn, {srcState}, "src_opt"));

            builder_.SetInsertPoint(noneBB);
            builder_.CreateRet(buildNoneValue(optTy));
            popScope();
        }

        auto [srcNf, srcSt] = loadIteratorFields(
            builder_, iteratorHeaderTy_, ptrTy_, iterVal, "take");
        uint64_t stateSize = dl.getTypeAllocSize(stateTy);
        llvm::Value *stateAlloc = builder_.CreateCall(
            mallocFn, {llvm::ConstantInt::get(i64Ty_, stateSize)}, "take_state");
        builder_.CreateStore(srcNf, builder_.CreateStructGEP(stateTy, stateAlloc, 0));
        builder_.CreateStore(srcSt, builder_.CreateStructGEP(stateTy, stateAlloc, 1));
        builder_.CreateStore(n, builder_.CreateStructGEP(stateTy, stateAlloc, 2));

        return emitIteratorHeaderAlloc(*this, builder_, *mod_, iteratorHeaderTy_,
            i64Ty_, ptrTy_, takeNextFn, stateAlloc, elemTy, iterator_element_types_, "take_iter");
    }

    return nullptr;
}
