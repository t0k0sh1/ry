#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"


namespace ry {

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
        {"contains",   &CodeGen::emitStrOp_contains},
        {"count",      &CodeGen::emitStrOp_count},
        {"startsWith", &CodeGen::emitStrOp_starts_with},
        {"endsWith",   &CodeGen::emitStrOp_ends_with},
        {"find",       &CodeGen::emitStrOp_find},
        {"substr",     &CodeGen::emitStrOp_substring},
        {"charAt",     &CodeGen::emitStrOp_char_at},
        {"replace",    &CodeGen::emitStrOp_replace},
        {"toUpper",    &CodeGen::emitStrOp_to_upper},
        {"toLower",    &CodeGen::emitStrOp_to_lower},
        {"trim",       &CodeGen::emitStrOp_trim},
        {"trimStart",  &CodeGen::emitStrOp_trim_start},
        {"trimEnd",    &CodeGen::emitStrOp_trim_end},
        {"repeat",     &CodeGen::emitStrOp_repeat},
        {"reverse",    &CodeGen::emitStrOp_reverse},
        {"split",      &CodeGen::emitStrOp_split},
        {"join",       &CodeGen::emitStrOp_join},
    };
    auto it = dispatch.find(e.callee);
    if (it == dispatch.end()) return nullptr;
    return (this->*it->second)(e);
}

// ===== String operation handlers =====

// contains(s, sub[, ignore_case]) → bool
// Set / Map membership are intercepted here because the dispatch chain routes
// "contains" through the string handler before emitBuiltinCollection.
llvm::Value *CodeGen::emitStrOp_contains(const CallExpr &e) {
    if (e.args.size() < 2 || e.args.size() > 3)
        codegenError("contains() takes 2 or 3 arguments");
    llvm::Value *s = emitExpr(*e.args[0]);

    if (llvm::Type *setElemTy = getSetElementType(s)) {
        if (e.args.size() != 2)
            codegenError("Set.contains() takes exactly 1 argument");
        llvm::Value *elem = emitExpr(*e.args[1]);
        if (elem->getType() != setElemTy)
            codegenError("contains() element type mismatch");
        std::string cElemName = getSetElemName(s);
        validateSetElemType(cElemName, elem, "contains()");
        llvm::Value *idx = emitSetElementLookup(s, elem, setElemTy, cElemName);
        return builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "set_contains");
    }

    if (llvm::Type *mapKeyTy = getMapKeyType(s)) {
        if (e.args.size() != 2)
            codegenError("Map.contains() takes exactly 1 argument");
        llvm::Value *key = emitExpr(*e.args[1]);
        if (key->getType() != mapKeyTy)
            codegenError("contains() key type mismatch");
        llvm::Value *idx = emitMapKeyLookup(s, key, mapKeyTy);
        return builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "map_contains");
    }

    llvm::Value *sub = emitExpr(*e.args[1]);
    llvm::Value *ignoreCase = (e.args.size() == 3)
        ? emitExpr(*e.args[2])
        : llvm::ConstantInt::get(i1Ty_, 0);
    if (s->getType() != ptrTy_ || sub->getType() != ptrTy_)
        codegenError("contains() requires str arguments");

    // NUL-safe: read byte lengths from StringHeader and call __ry_str_find_byte.
    // Returns byte offset >= 0 when found, -1 when not found.
    llvm::Value *sl   = emitStringByteLen(s);
    llvm::Value *subl = emitStringByteLen(sub);
    llvm::Value *icI32 = builder_.CreateZExt(ignoreCase, i32Ty_, "ic_i32");
    auto findByteFn = getRuntimeFn("__ry_str_find_byte", i64Ty_,
                                   {ptrTy_, i64Ty_, ptrTy_, i64Ty_, i32Ty_});
    llvm::Value *byteOff = builder_.CreateCall(findByteFn, {s, sl, sub, subl, icI32},
                                               "find_byte_off");
    return builder_.CreateICmpNE(byteOff,
                                 llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(-1LL)),
                                 "contains");
}

// count(s, sub[, ignore_case]) → int
llvm::Value *CodeGen::emitStrOp_count(const CallExpr &e) {
    if (e.args.size() < 2 || e.args.size() > 3)
        codegenError("count() takes 2 or 3 arguments");
    llvm::Value *s = emitExpr(*e.args[0]);
    llvm::Value *sub = emitExpr(*e.args[1]);
    llvm::Value *ignoreCase = (e.args.size() == 3)
        ? emitExpr(*e.args[2])
        : llvm::ConstantInt::get(i1Ty_, 0);
    if (s->getType() != ptrTy_ || sub->getType() != ptrTy_)
        codegenError("count() requires str arguments");

    llvm::Value *sl   = emitStringByteLen(s);
    llvm::Value *subl = emitStringByteLen(sub);
    llvm::Value *icI32 = builder_.CreateZExt(ignoreCase, i32Ty_, "ic_i32");
    auto countByteFn = getRuntimeFn("__ry_str_count_byte", i64Ty_,
                                     {ptrTy_, i64Ty_, ptrTy_, i64Ty_, i32Ty_});
    return builder_.CreateCall(countByteFn, {s, sl, sub, subl, icI32}, "str_count");
}

// startsWith(s, prefix[, ignore_case]) → bool
llvm::Value *CodeGen::emitStrOp_starts_with(const CallExpr &e) {
    if (e.args.size() < 2 || e.args.size() > 3)
        codegenError("startsWith() takes 2 or 3 arguments");
    llvm::Value *s = emitExpr(*e.args[0]);
    llvm::Value *prefix = emitExpr(*e.args[1]);
    llvm::Value *ignoreCase = (e.args.size() == 3)
        ? emitExpr(*e.args[2])
        : llvm::ConstantInt::get(i1Ty_, 0);
    if (s->getType() != ptrTy_ || prefix->getType() != ptrTy_)
        codegenError("startsWith() requires str arguments");

    // NUL-safe: read byte lengths from StringHeader and call __ry_str_starts_with.
    llvm::Value *sl = emitStringByteLen(s);
    llvm::Value *pl = emitStringByteLen(prefix);
    llvm::Value *icI32 = builder_.CreateZExt(ignoreCase, i32Ty_, "ic_i32");
    auto swFn = getRuntimeFn("__ry_str_starts_with", i32Ty_,
                             {ptrTy_, i64Ty_, ptrTy_, i64Ty_, i32Ty_});
    llvm::Value *result = builder_.CreateCall(swFn, {s, sl, prefix, pl, icI32}, "sw_result");
    return builder_.CreateICmpNE(result, llvm::ConstantInt::get(i32Ty_, 0), "starts_with");
}

// endsWith(s, suffix[, ignore_case]) → bool
llvm::Value *CodeGen::emitStrOp_ends_with(const CallExpr &e) {
    if (e.args.size() < 2 || e.args.size() > 3)
        codegenError("endsWith() takes 2 or 3 arguments");
    llvm::Value *s = emitExpr(*e.args[0]);
    llvm::Value *suffix = emitExpr(*e.args[1]);
    llvm::Value *ignoreCase = (e.args.size() == 3)
        ? emitExpr(*e.args[2])
        : llvm::ConstantInt::get(i1Ty_, 0);
    if (s->getType() != ptrTy_ || suffix->getType() != ptrTy_)
        codegenError("endsWith() requires str arguments");

    // NUL-safe: read byte lengths from StringHeader and call __ry_str_ends_with.
    llvm::Value *sl  = emitStringByteLen(s);
    llvm::Value *sfl = emitStringByteLen(suffix);
    llvm::Value *icI32 = builder_.CreateZExt(ignoreCase, i32Ty_, "ic_i32");
    auto ewFn = getRuntimeFn("__ry_str_ends_with", i32Ty_,
                             {ptrTy_, i64Ty_, ptrTy_, i64Ty_, i32Ty_});
    llvm::Value *result = builder_.CreateCall(ewFn, {s, sl, suffix, sfl, icI32}, "ew_result");
    return builder_.CreateICmpNE(result, llvm::ConstantInt::get(i32Ty_, 0), "ends_with");
}

// find(s, sub) → Option<int>
llvm::Value *CodeGen::emitStrOp_find(const CallExpr &e) {
    requireArgs(e, 2);
    llvm::Value *s = emitExpr(*e.args[0]);
    llvm::Value *sub = emitExpr(*e.args[1]);
    if (s->getType() != ptrTy_ || sub->getType() != ptrTy_)
        codegenError("find() requires str arguments");

    llvm::StructType *optTy = getOptionType(i64Ty_);

    // NUL-safe: read byte lengths from StringHeader, call __ry_str_find_byte which
    // returns the byte offset (>= 0) or -1 if not found.
    llvm::Value *sl   = emitStringByteLen(s);
    llvm::Value *subl = emitStringByteLen(sub);
    auto findByteFn = getRuntimeFn("__ry_str_find_byte", i64Ty_,
                                   {ptrTy_, i64Ty_, ptrTy_, i64Ty_, i32Ty_});
    llvm::Value *byteOff = builder_.CreateCall(
        findByteFn, {s, sl, sub, subl, llvm::ConstantInt::get(i32Ty_, 0)}, "find_byte_off");
    llvm::Value *notFound = builder_.CreateICmpEQ(
        byteOff, llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(-1LL)), "find_notfound");

    llvm::BasicBlock *foundBB    = llvm::BasicBlock::Create(*ctx_, "find.found", fn_);
    llvm::BasicBlock *notFoundBB = llvm::BasicBlock::Create(*ctx_, "find.notfound", fn_);
    llvm::BasicBlock *mergeBB    = llvm::BasicBlock::Create(*ctx_, "find.merge", fn_);
    builder_.CreateCondBr(notFound, notFoundBB, foundBB);

    builder_.SetInsertPoint(foundBB);
    // NUL-safe byte-offset → char-index conversion (replaces __ry_utf8_char_index
    // which stopped at the first '\0').
    auto charIdxFn = getRuntimeFn("__ry_utf8_char_index_n", i64Ty_, {ptrTy_, i64Ty_, i64Ty_});
    llvm::Value *charIdx = builder_.CreateCall(charIdxFn, {s, sl, byteOff}, "find_char_idx");
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

// substr(s, start, end) → str (UTF-8 character indices, clamped)
llvm::Value *CodeGen::emitStrOp_substring(const CallExpr &e) {
    requireArgs(e, 3);
    llvm::Value *s = emitExpr(*e.args[0]);
    llvm::Value *start = emitExpr(*e.args[1]);
    llvm::Value *end = emitExpr(*e.args[2]);
    if (s->getType() != ptrTy_)
        codegenError("substr() requires str as first argument");

    // Fast path: both indices are compile-time constants satisfying sv >= 0, ev >= 0, ev >= sv.
    // All wrap and clamp operations are provably no-ops, so skip them and call the runtime directly.
    if (auto *ciStart = llvm::dyn_cast<llvm::ConstantInt>(start)) {
        if (auto *ciEnd = llvm::dyn_cast<llvm::ConstantInt>(end)) {
            int64_t sv = ciStart->getSExtValue();
            int64_t ev = ciEnd->getSExtValue();
            if (sv >= 0 && ev >= 0 && ev >= sv) {
                auto substrFn = getRuntimeFn("__ry_utf8_substring", ptrTy_,
                                             {ptrTy_, i64Ty_, i64Ty_, i64Ty_});
                auto *r = builder_.CreateCall(substrFn, {s, emitStringByteLen(s), start, end},
                                              "substring");
                arc_str_owned_values_.insert(r);
                return r;
            }
        }
    }

    // Wrap negative indices Python-style (length + idx), then clamp the lower bound to 0.
    // The upper bound clamp (end <= length) is performed by __ry_utf8_substring at runtime.
    // wrapBase must be the UTF-8 codepoint count, not the byte length — multi-byte
    // strings like "あいうえお" (5 chars / 15 bytes) otherwise wrap against the wrong base.
    llvm::Value *byteLen = emitStringByteLen(s);
    auto utf8LenFn = getRuntimeFn("__ry_utf8_len_n", i64Ty_, {ptrTy_, i64Ty_});
    llvm::Value *charLen = builder_.CreateCall(utf8LenFn, {s, byteLen}, "substr_charlen");

    llvm::Value *startWrapped = emitNegativeIndexWrap(start, charLen, "substr_start");
    llvm::Value *endWrapped = emitNegativeIndexWrap(end, charLen, "substr_end");

    llvm::Value *zero = llvm::ConstantInt::get(i64Ty_, 0);

    llvm::Value *clampedStart = builder_.CreateSelect(
        builder_.CreateICmpSLT(startWrapped, zero), zero, startWrapped, "substr_cstart");

    llvm::Value *clampedEnd = builder_.CreateSelect(
        builder_.CreateICmpSLT(endWrapped, zero), zero, endWrapped, "substr_cend");

    // Ensure end >= start
    clampedEnd = builder_.CreateSelect(
        builder_.CreateICmpSLT(clampedEnd, clampedStart), clampedStart, clampedEnd, "substr_cend2");

    auto substrFn = getRuntimeFn("__ry_utf8_substring", ptrTy_, {ptrTy_, i64Ty_, i64Ty_, i64Ty_});
    auto *r = builder_.CreateCall(substrFn, {s, byteLen, clampedStart, clampedEnd},
                                  "substring");
    arc_str_owned_values_.insert(r);
    return r;
}

// charAt(s, i) → str (single UTF-8 character as string)
llvm::Value *CodeGen::emitStrOp_char_at(const CallExpr &e) {
    requireArgs(e, 2);
    llvm::Value *s = emitExpr(*e.args[0]);
    llvm::Value *idx = emitExpr(*e.args[1]);
    if (s->getType() != ptrTy_)
        codegenError("charAt() requires str as first argument");

    if (idx->getType()->isIntegerTy(1))
        idx = builder_.CreateZExt(idx, i64Ty_, "char_at_idx");

    auto fn = getRuntimeFn("__ry_utf8_char_at_checked", ptrTy_, {ptrTy_, i64Ty_, i64Ty_});
    auto *r = builder_.CreateCall(fn, {s, emitStringByteLen(s), idx}, "char_at");
    arc_str_owned_values_.insert(r);
    return r;
}

// replace(s, old, new) → str
llvm::Value *CodeGen::emitStrOp_replace(const CallExpr &e) {
    requireArgs(e, 3);
    llvm::Value *s = emitExpr(*e.args[0]);
    llvm::Value *oldStr = emitExpr(*e.args[1]);
    llvm::Value *newStr = emitExpr(*e.args[2]);
    // Regex overload: replace(text, /pattern/, replacement) → delegate to regex runtime
    // Regex values are StringHeader-backed so emitStringByteLen is safe (#1052).
    if (isRegex(oldStr) && isStringValue(s)) {
        if (!isStringValue(newStr))
            codegenError("replace() requires str arguments");
        auto fn = mod_->getOrInsertFunction("__ry_regex_replace",
                                            fnTy_ptr_i64_ptr_i64_ptr_i64_to_ptr_);
        auto *r = builder_.CreateCall(fn,
            {oldStr, emitStringByteLen(oldStr),
             s,      emitStringByteLen(s),
             newStr, emitStringByteLen(newStr)},
            "regex_replace");
        llvm::Value *isNull = builder_.CreateICmpEQ(
            r, llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)),
            "regex_replace_is_null");
        llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, "regex_replace.err", fn_);
        llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "regex_replace.ok", fn_);
        builder_.CreateCondBr(isNull, errBB, okBB);
        builder_.SetInsertPoint(errBB);
        auto errFnTy = llvm::FunctionType::get(ptrTy_, {}, false);
        auto errFn = mod_->getOrInsertFunction("__ry_regex_get_last_error", errFnTy);
        llvm::Value *msgPtr = builder_.CreateCall(errFn, {}, "regex_replace_err_msg");
        emitRuntimeError("error: %s\n", ".regex_replace_runtime_err", {msgPtr});
        builder_.SetInsertPoint(okBB);
        arc_str_owned_values_.insert(r);
        return r;
    }
    if (s->getType() != ptrTy_ || oldStr->getType() != ptrTy_ || newStr->getType() != ptrTy_)
        codegenError("replace() requires str arguments");

    llvm::Value *sLen   = emitStringByteLen(s);
    llvm::Value *oldLen = emitStringByteLen(oldStr);
    llvm::Value *newLen = emitStringByteLen(newStr);
    auto replaceFn = getRuntimeFn("__ry_str_replace", ptrTy_,
                                  {ptrTy_, i64Ty_, ptrTy_, i64Ty_, ptrTy_, i64Ty_});
    auto *r = builder_.CreateCall(replaceFn, {s, sLen, oldStr, oldLen, newStr, newLen},
                                  "replace_result");
    arc_str_owned_values_.insert(r);
    return r;
}

// toUpper(s) → str
llvm::Value *CodeGen::emitStrOp_to_upper(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *s = emitExpr(*e.args[0]);
    if (s->getType() != ptrTy_)
        codegenError("toUpper() requires str argument");

    llvm::Value *len = emitStringByteLen(s);
    auto makeUninitTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
    auto makeUninitFn = mod_->getOrInsertFunction("__ry_string_make_uninit", makeUninitTy);
    llvm::Value *buf = builder_.CreateCall(makeUninitFn, {len}, "upper_buf");

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
    arc_str_owned_values_.insert(buf);
    return buf;
}

// toLower(s) → str
llvm::Value *CodeGen::emitStrOp_to_lower(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *s = emitExpr(*e.args[0]);
    if (s->getType() != ptrTy_)
        codegenError("toLower() requires str argument");

    llvm::Value *len = emitStringByteLen(s);
    auto makeUninitTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
    auto makeUninitFn = mod_->getOrInsertFunction("__ry_string_make_uninit", makeUninitTy);
    llvm::Value *buf = builder_.CreateCall(makeUninitFn, {len}, "lower_buf");

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
    arc_str_owned_values_.insert(buf);
    return buf;
}

// trim(s) → str
llvm::Value *CodeGen::emitStrOp_trim(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *s = emitExpr(*e.args[0]);
    if (s->getType() != ptrTy_)
        codegenError("trim() requires str argument");
    auto memcpyFn = getStdlibMemcpy();

    llvm::Value *len = emitStringByteLen(s);

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
    auto makeUninitTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
    auto makeUninitFn = mod_->getOrInsertFunction("__ry_string_make_uninit", makeUninitTy);
    llvm::Value *buf = builder_.CreateCall(makeUninitFn, {safeLen}, "trim_buf");
    llvm::Value *srcPtr = builder_.CreateGEP(builder_.getInt8Ty(), s, finalStart, "trim_src");
    builder_.CreateCall(memcpyFn, {buf, srcPtr, safeLen});
    arc_str_owned_values_.insert(buf);
    return buf;
}

// trimStart(s) → str
llvm::Value *CodeGen::emitStrOp_trim_start(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *s = emitExpr(*e.args[0]);
    if (s->getType() != ptrTy_)
        codegenError("trimStart() requires str argument");
    auto memcpyFn = getStdlibMemcpy();

    llvm::Value *len = emitStringByteLen(s);

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
    auto makeUninitTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
    auto makeUninitFn = mod_->getOrInsertFunction("__ry_string_make_uninit", makeUninitTy);
    llvm::Value *buf = builder_.CreateCall(makeUninitFn, {resultLen}, "tstart_buf");
    llvm::Value *srcPtr = builder_.CreateGEP(builder_.getInt8Ty(), s, finalStart, "tstart_src");
    builder_.CreateCall(memcpyFn, {buf, srcPtr, resultLen});
    arc_str_owned_values_.insert(buf);
    return buf;
}

// trimEnd(s) → str
llvm::Value *CodeGen::emitStrOp_trim_end(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *s = emitExpr(*e.args[0]);
    if (s->getType() != ptrTy_)
        codegenError("trimEnd() requires str argument");
    auto memcpyFn = getStdlibMemcpy();

    llvm::Value *len = emitStringByteLen(s);

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
    auto makeUninitTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
    auto makeUninitFn = mod_->getOrInsertFunction("__ry_string_make_uninit", makeUninitTy);
    llvm::Value *buf = builder_.CreateCall(makeUninitFn, {finalEnd}, "tend_buf");
    builder_.CreateCall(memcpyFn, {buf, s, finalEnd});
    arc_str_owned_values_.insert(buf);
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
        uint64_t elemSize = dl.getTypeAllocSize(elemTy);

        auto lf = loadListHeader(arg, "rev");
        llvm::Value *len = lf.len;
        llvm::Value *srcData = lf.data;

        llvm::Value *newHeader = emitArcAllocCollectionHeader(listHeaderTy_);
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

        setTypeMeta(TypeMeta::ListElem, newHeader, elemTy);
        propagateMeta(arg, newHeader);
        return newHeader;
    }

    // String reverse (UTF-8 aware)
    llvm::Value *s = arg;
    if (s->getType() != ptrTy_)
        codegenError("reverse() requires list or str argument");

    auto revFn = getRuntimeFn("__ry_utf8_reverse", ptrTy_, {ptrTy_, i64Ty_});
    auto *r = builder_.CreateCall(revFn, {s, emitStringByteLen(s)}, "str_rev");
    arc_str_owned_values_.insert(r);
    return r;
}

// split(s[, delim]) → List<str>
llvm::Value *CodeGen::emitStrOp_split(const CallExpr &e) {
    if (e.callee == "_split") {
        if (e.args.size() != 2)
            codegenError("_split() takes exactly 2 arguments");
    } else {
        if (e.args.size() < 1 || e.args.size() > 2)
            codegenError("split() takes 1 or 2 arguments");
    }
    llvm::Value *s = emitExpr(*e.args[0]);
    llvm::Value *delim = (e.args.size() == 2)
        ? emitExpr(*e.args[1])
        : cachedGlobalString(" ", ".split_default_delim");
    // Regex overload: split(text, /pattern/) → delegate to regex runtime
    // Regex values are StringHeader-backed so emitStringByteLen is safe (#1052).
    if (isRegex(delim) && isStringValue(s)) {
        auto fn = mod_->getOrInsertFunction("__ry_regex_split",
                                            fnTy_ptr_i64_ptr_i64_to_ptr_);
        llvm::Value *r = builder_.CreateCall(fn,
            {delim, emitStringByteLen(delim), s, emitStringByteLen(s)},
            "regex_split");
        llvm::Value *isNull = builder_.CreateICmpEQ(
            r, llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)),
            "regex_split_is_null");
        llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, "regex_split.err", fn_);
        llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "regex_split.ok", fn_);
        builder_.CreateCondBr(isNull, errBB, okBB);
        builder_.SetInsertPoint(errBB);
        auto errFnTy = llvm::FunctionType::get(ptrTy_, {}, false);
        auto errFn = mod_->getOrInsertFunction("__ry_regex_get_last_error", errFnTy);
        llvm::Value *msgPtr = builder_.CreateCall(errFn, {}, "regex_split_err_msg");
        emitRuntimeError("error: %s\n", ".regex_split_runtime_err", {msgPtr});
        builder_.SetInsertPoint(okBB);
        setTypeMeta(TypeMeta::ListElem, r, ptrTy_);
        // Mirror the non-regex path: stamp str-element metadata so the
        // str-aware destructor runs on overwrite and tryRetainArcSource
        // Case 4 emits the missing element retain on untyped destructure
        // (`a, b = split(text, /re/)`).  Symmetric counter (#1576) makes
        // this safe.
        getOrCreateMeta(r).list_elem_type_name = "str";
        getOrCreateMeta(r).list_elem_is_str = true;
        return r;
    }
    if (s->getType() != ptrTy_ || delim->getType() != ptrTy_)
        codegenError("split() requires str arguments");

    // Empty delimiter: split into individual characters (UTF-8 aware)
    llvm::Value *delimLen = emitStringByteLen(delim);
    llvm::Value *isEmptyDelim = builder_.CreateICmpEQ(
        delimLen, llvm::ConstantInt::get(i64Ty_, 0), "split_empty_delim");

    llvm::BasicBlock *emptyDelimBB = llvm::BasicBlock::Create(*ctx_, "split.empty_delim", fn_);
    llvm::BasicBlock *normalBB = llvm::BasicBlock::Create(*ctx_, "split.normal", fn_);
    llvm::BasicBlock *doneBB = llvm::BasicBlock::Create(*ctx_, "split.done", fn_);

    builder_.CreateCondBr(isEmptyDelim, emptyDelimBB, normalBB);

    // --- Empty delimiter path: call __ry_split_chars runtime ---
    builder_.SetInsertPoint(emptyDelimBB);
    auto splitCharsFn = mod_->getOrInsertFunction("__ry_split_chars", fnTy_ptr_i64_to_ptr_);
    llvm::Value *charsResult = builder_.CreateCall(splitCharsFn, {s, emitStringByteLen(s)},
                                                   "split_chars");
    builder_.CreateBr(doneBB);

    // --- Normal delimiter path: NUL-safe runtime helper (#1051) ---
    builder_.SetInsertPoint(normalBB);
    auto splitFnTy = llvm::FunctionType::get(
        ptrTy_, {ptrTy_, i64Ty_, ptrTy_, i64Ty_}, false);
    auto splitFn = mod_->getOrInsertFunction("__ry_str_split", splitFnTy);
    llvm::Value *normalResult = builder_.CreateCall(
        splitFn, {s, emitStringByteLen(s), delim, delimLen}, "split_normal");
    builder_.CreateBr(doneBB);

    // --- Merge point ---
    builder_.SetInsertPoint(doneBB);
    llvm::PHINode *result = builder_.CreatePHI(ptrTy_, 2, "split_result");
    result->addIncoming(charsResult, emptyDelimBB);
    result->addIncoming(normalResult, normalBB);

    setTypeMeta(TypeMeta::ListElem, result, ptrTy_);
    // Declare element type so resolveCollectionDestructor dispatches to the
    // str-aware variant which releases each str element before freeing the
    // buffer.  Safe after #1576: makeString/freeStringSlot are now symmetric
    // with __ry_arc_alloc_counted, so per-element release no longer
    // underflows arcLiveCount().  Also enables tryRetainArcSource Case 4 to
    // emit retain on `a, b = parts` after IndexExpr propagates it onto the
    // loaded element via list_elem_is_str (#1266 side-channel — preserved
    // because the read-side propagation path still relies on it).
    getOrCreateMeta(result).list_elem_type_name = "str";
    getOrCreateMeta(result).list_elem_is_str = true;
    return result;
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
    if (!elemTy) {
        // Support sep.join(list) — UFCS places the receiver first
        elemTy = getListElementType(sep);
        if (!elemTy)
            return nullptr;
        std::swap(listPtr, sep);
    }
    if (elemTy != ptrTy_)
        codegenError("join() requires List<str> as first argument");
    auto memcpyFn = getStdlibMemcpy();
    auto makeUninitTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
    auto makeUninitFn = mod_->getOrInsertFunction("__ry_string_make_uninit", makeUninitTy);

    auto lf = loadListHeader(listPtr, "join");
    llvm::Value *listLen = lf.len;
    llvm::Value *listData = lf.data;
    llvm::Value *sepLen = emitStringByteLen(sep);

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
    llvm::Value *elemLen = emitStringByteLen(elem);
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
    llvm::Value *buf = builder_.CreateCall(makeUninitFn, {grandTotal}, "join_buf");

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
    llvm::Value *elem2Len = emitStringByteLen(elem2);
    builder_.CreateCall(memcpyFn, {dstForElem, elem2, elem2Len});
    llvm::Value *dstAfterElem = builder_.CreateGEP(builder_.getInt8Ty(), dstForElem, elem2Len, "dst_after_elem");
    builder_.CreateStore(dstAfterElem, dstVar);
    builder_.CreateStore(builder_.CreateAdd(i2Cur, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
    builder_.CreateBr(buildCondBB);

    builder_.SetInsertPoint(buildEndBB);
    arc_str_owned_values_.insert(buf);
    return buf;
}

} // namespace ry
