#include "ry/codegen.hpp"
#include "ry/diagnostic/diagnostic.hpp"
#include "ry/llvm_emit/api.h" // ry_emit_list_reverse (#2095)
#include "ry/llvm_emit/cast_helpers.hpp" // asRyValue / asRyType (#2095)


namespace ry {

// ===== Whitespace helper =====

// #2072 [C]=(ii): emitted via ry_emit_* primitives. The 4 ICmpEQ and 3 Or are
// unnamed (no name arg in the pre-migration CreateICmpEQ/CreateOr), so each
// wrapper is called with name "" to keep the auto-numbered SSA values identical.
// The nested Or structure is preserved verbatim so the compiler's
// argument-evaluation order (and thus the emission order) matches the baseline.
llvm::Value *CodeGen::emitIsWhitespace(llvm::Value *ch) {
    llvm::Value *isSp  = emitICmpEQ(ch, emitConstInt(i8Ty_, ' '), "");
    llvm::Value *isTab = emitICmpEQ(ch, emitConstInt(i8Ty_, '\t'), "");
    llvm::Value *isNl  = emitICmpEQ(ch, emitConstInt(i8Ty_, '\n'), "");
    llvm::Value *isCr  = emitICmpEQ(ch, emitConstInt(i8Ty_, '\r'), "");
    return emitOr(emitOr(isSp, isTab, ""), emitOr(isNl, isCr, ""), "");
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
    // #2094 ([C] = (ii) boundary move): both runtime calls (__ry_str_find_byte
    // 5-arg + __ry_utf8_char_index_n 3-arg, both non-variadic) cross via
    // emitRuntimeCallDirect (ry_emit_runtime_call); the found/not-found
    // condition compute crosses via emitICmpEQ. emitStringByteLen stays
    // C++-side (StringHeader-load capability, follow-on).
    llvm::Value *sl   = emitStringByteLen(s);
    llvm::Value *subl = emitStringByteLen(sub);
    llvm::Value *byteOff = emitRuntimeCallDirect(
        "__ry_str_find_byte", i64Ty_,
        {ptrTy_, i64Ty_, ptrTy_, i64Ty_, i32Ty_},
        {s, sl, sub, subl, emitConstInt(i32Ty_, 0)}, "find_byte_off");
    llvm::Value *notFound = emitICmpEQ(
        byteOff, emitConstInt(i64Ty_, static_cast<uint64_t>(-1LL)), "find_notfound");

    llvm::BasicBlock *foundBB    = createBB("find.found");
    llvm::BasicBlock *notFoundBB = createBB("find.notfound");
    llvm::BasicBlock *mergeBB    = createBB("find.merge");
    emitBranchCond(notFound, notFoundBB, foundBB);

    builder_.SetInsertPoint(foundBB);
    // NUL-safe byte-offset → char-index conversion (replaces __ry_utf8_char_index
    // which stopped at the first '\0').
    llvm::Value *charIdx = emitRuntimeCallDirect(
        "__ry_utf8_char_index_n", i64Ty_,
        {ptrTy_, i64Ty_, i64Ty_}, {s, sl, byteOff}, "find_char_idx");
    llvm::Value *someVal = buildSomeValue(charIdx, optTy);
    emitBranchUncond(mergeBB);
    llvm::BasicBlock *foundEndBB = builder_.GetInsertBlock();

    builder_.SetInsertPoint(notFoundBB);
    llvm::Value *noneVal = buildNoneValue(optTy);
    emitBranchUncond(mergeBB);
    llvm::BasicBlock *notFoundEndBB = builder_.GetInsertBlock();

    builder_.SetInsertPoint(mergeBB);
    llvm::PHINode *phi = createPhi(optTy, {}, "find_result");
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
                llvm::Value *r = emitRuntimeCallDirect("__ry_utf8_substring", ptrTy_,
                                                       {ptrTy_, i64Ty_, i64Ty_, i64Ty_},
                                                       {s, emitStringByteLen(s), start, end},
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
    llvm::Value *charLen = emitRuntimeCallDirect("__ry_utf8_len_n", i64Ty_,
                                                 {ptrTy_, i64Ty_}, {s, byteLen},
                                                 "substr_charlen");

    llvm::Value *startWrapped = emitNegativeIndexWrap(start, charLen, "substr_start");
    llvm::Value *endWrapped = emitNegativeIndexWrap(end, charLen, "substr_end");

    llvm::Value *zero = emitConstInt(i64Ty_, 0);

    llvm::Value *clampedStart = emitSelect(
        emitICmpSLT(startWrapped, zero, ""), zero, startWrapped, "substr_cstart");

    llvm::Value *clampedEnd = emitSelect(
        emitICmpSLT(endWrapped, zero, ""), zero, endWrapped, "substr_cend");

    // Ensure end >= start
    clampedEnd = emitSelect(
        emitICmpSLT(clampedEnd, clampedStart, ""), clampedStart, clampedEnd, "substr_cend2");

    llvm::Value *r = emitRuntimeCallDirect("__ry_utf8_substring", ptrTy_,
                                           {ptrTy_, i64Ty_, i64Ty_, i64Ty_},
                                           {s, byteLen, clampedStart, clampedEnd},
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
        auto fn = getRuntimeFn("__ry_regex_replace",
                               ptrTy_, {ptrTy_, i64Ty_, ptrTy_, i64Ty_, ptrTy_, i64Ty_});
        auto *r = builder_.CreateCall(fn,
            {oldStr, emitStringByteLen(oldStr),
             s,      emitStringByteLen(s),
             newStr, emitStringByteLen(newStr)},
            "regex_replace");
        llvm::Value *isNull = builder_.CreateICmpEQ(
            r, llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)),
            "regex_replace_is_null");
        llvm::BasicBlock *errBB = createBB("regex_replace.err");
        llvm::BasicBlock *okBB = createBB("regex_replace.ok");
        emitBranchCond(isNull, errBB, okBB);
        builder_.SetInsertPoint(errBB);
        auto errFn = getRuntimeFn("__ry_regex_get_last_error", ptrTy_, {});
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
    llvm::Value *r = emitRuntimeCallDirect("__ry_str_replace", ptrTy_,
                                           {ptrTy_, i64Ty_, ptrTy_, i64Ty_, ptrTy_, i64Ty_},
                                           {s, sLen, oldStr, oldLen, newStr, newLen},
                                           "replace_result");
    arc_str_owned_values_.insert(r);
    return r;
}

// toUpper(s) → str
// #2072 [C]=(ii): emitted via the ry_emit_* scalar primitives (no
// builder_.Create*). emitStringByteLen / SetInsertPoint stay direct (see
// codegen.hpp). IR is byte-for-byte identical to the pre-migration form.
llvm::Value *CodeGen::emitStrOp_to_upper(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *s = emitExpr(*e.args[0]);
    if (s->getType() != ptrTy_)
        codegenError("toUpper() requires str argument");

    llvm::Value *len = emitStringByteLen(s);
    llvm::Value *buf = emitRuntimeCallDirect("__ry_string_make_uninit", ptrTy_,
                                             {i64Ty_}, {len}, "upper_buf");

    llvm::Value *iVar = emitAlloca(i64Ty_, "upper_i");
    emitStore(emitConstInt(i64Ty_, 0), iVar);

    llvm::BasicBlock *condBB = createBB("upper.cond");
    llvm::BasicBlock *bodyBB = createBB("upper.body");
    llvm::BasicBlock *endBB = createBB("upper.end");

    emitBranchUncond(condBB);
    builder_.SetInsertPoint(condBB);
    llvm::Value *i = emitLoad(i64Ty_, iVar, "upper_idx");
    emitBranchCond(emitICmpSLT(i, len, "upper_cond"), bodyBB, endBB);

    builder_.SetInsertPoint(bodyBB);
    llvm::Value *iCur = emitLoad(i64Ty_, iVar, "upper_i_cur");
    llvm::Value *srcPtr = emitGEP(i8Ty_, s, iCur, "upper_src");
    llvm::Value *ch = emitLoad(i8Ty_, srcPtr, "upper_ch");
    llvm::Value *isLowerA = emitICmpUGE(ch, emitConstInt(i8Ty_, 'a'), "is_lower_a");
    llvm::Value *isLowerZ = emitICmpULE(ch, emitConstInt(i8Ty_, 'z'), "is_lower_z");
    llvm::Value *isLower = emitAnd(isLowerA, isLowerZ, "is_lower");
    llvm::Value *upper = emitSub(ch, emitConstInt(i8Ty_, 32), "upper_ch_val");
    llvm::Value *result = emitSelect(isLower, upper, ch, "upper_result");
    llvm::Value *dstPtr = emitGEP(i8Ty_, buf, iCur, "upper_dst");
    emitStore(result, dstPtr);
    emitStore(emitAdd(iCur, emitConstInt(i64Ty_, 1), "upper_next"), iVar);
    emitBranchUncond(condBB);

    builder_.SetInsertPoint(endBB);
    arc_str_owned_values_.insert(buf);
    return buf;
}

// toLower(s) → str
// #2072 [C]=(ii): same shape as toUpper via ry_emit_* primitives. NB the SSA
// names invert semantically (is_upper_a / lower_ch_val) and the case arithmetic
// is Add (+32), not Sub — threaded through emitConstInt / emitAdd verbatim.
llvm::Value *CodeGen::emitStrOp_to_lower(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *s = emitExpr(*e.args[0]);
    if (s->getType() != ptrTy_)
        codegenError("toLower() requires str argument");

    llvm::Value *len = emitStringByteLen(s);
    llvm::Value *buf = emitRuntimeCallDirect("__ry_string_make_uninit", ptrTy_,
                                             {i64Ty_}, {len}, "lower_buf");

    llvm::Value *iVar = emitAlloca(i64Ty_, "lower_i");
    emitStore(emitConstInt(i64Ty_, 0), iVar);

    llvm::BasicBlock *condBB = createBB("lower.cond");
    llvm::BasicBlock *bodyBB = createBB("lower.body");
    llvm::BasicBlock *endBB = createBB("lower.end");

    emitBranchUncond(condBB);
    builder_.SetInsertPoint(condBB);
    llvm::Value *i = emitLoad(i64Ty_, iVar, "lower_idx");
    emitBranchCond(emitICmpSLT(i, len, "lower_cond"), bodyBB, endBB);

    builder_.SetInsertPoint(bodyBB);
    llvm::Value *iCur = emitLoad(i64Ty_, iVar, "lower_i_cur");
    llvm::Value *srcPtr = emitGEP(i8Ty_, s, iCur, "lower_src");
    llvm::Value *ch = emitLoad(i8Ty_, srcPtr, "lower_ch");
    llvm::Value *isUpperA = emitICmpUGE(ch, emitConstInt(i8Ty_, 'A'), "is_upper_a");
    llvm::Value *isUpperZ = emitICmpULE(ch, emitConstInt(i8Ty_, 'Z'), "is_upper_z");
    llvm::Value *isUpper = emitAnd(isUpperA, isUpperZ, "is_upper");
    llvm::Value *lower = emitAdd(ch, emitConstInt(i8Ty_, 32), "lower_ch_val");
    llvm::Value *result = emitSelect(isUpper, lower, ch, "lower_result");
    llvm::Value *dstPtr = emitGEP(i8Ty_, buf, iCur, "lower_dst");
    emitStore(result, dstPtr);
    emitStore(emitAdd(iCur, emitConstInt(i64Ty_, 1), "lower_next"), iVar);
    emitBranchUncond(condBB);

    builder_.SetInsertPoint(endBB);
    arc_str_owned_values_.insert(buf);
    return buf;
}

// trim(s) → str
// #2072 [C]=(ii): forward + backward whitespace scan then memcpy, emitted via
// ry_emit_* primitives. memcpy / __ry_string_make_uninit go through
// emitRuntimeCallDirect (ry_emit_runtime_call) instead of builder_.CreateCall.
llvm::Value *CodeGen::emitStrOp_trim(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *s = emitExpr(*e.args[0]);
    if (s->getType() != ptrTy_)
        codegenError("trim() requires str argument");

    llvm::Value *len = emitStringByteLen(s);

    llvm::Value *startVar = emitAlloca(i64Ty_, "trim_start");
    emitStore(emitConstInt(i64Ty_, 0), startVar);

    llvm::BasicBlock *startCondBB = createBB("trim.start_cond");
    llvm::BasicBlock *startBodyBB = createBB("trim.start_body");
    llvm::BasicBlock *startEndBB = createBB("trim.start_end");

    emitBranchUncond(startCondBB);
    builder_.SetInsertPoint(startCondBB);
    llvm::Value *startIdx = emitLoad(i64Ty_, startVar, "start_idx");
    llvm::Value *startInBounds = emitICmpSLT(startIdx, len, "start_in_bounds");

    llvm::BasicBlock *startCheckBB = createBB("trim.start_check");
    emitBranchCond(startInBounds, startCheckBB, startEndBB);

    builder_.SetInsertPoint(startCheckBB);
    llvm::Value *startPtr = emitGEP(i8Ty_, s, startIdx, "start_ptr");
    llvm::Value *startCh = emitLoad(i8Ty_, startPtr, "start_ch");
    llvm::Value *isWs = emitIsWhitespace(startCh);
    emitBranchCond(isWs, startBodyBB, startEndBB);

    builder_.SetInsertPoint(startBodyBB);
    llvm::Value *startNext = emitAdd(startIdx, emitConstInt(i64Ty_, 1), "start_next");
    emitStore(startNext, startVar);
    emitBranchUncond(startCondBB);

    builder_.SetInsertPoint(startEndBB);
    llvm::Value *finalStart = emitLoad(i64Ty_, startVar, "final_start");

    llvm::Value *endVar = emitAlloca(i64Ty_, "trim_end");
    emitStore(len, endVar);

    llvm::BasicBlock *endCondBB = createBB("trim.end_cond");
    llvm::BasicBlock *endBodyBB = createBB("trim.end_body");
    llvm::BasicBlock *endEndBB = createBB("trim.end_end");

    emitBranchUncond(endCondBB);
    builder_.SetInsertPoint(endCondBB);
    llvm::Value *endIdx = emitLoad(i64Ty_, endVar, "end_idx");
    llvm::Value *endGtStart = emitICmpSGT(endIdx, finalStart, "end_gt_start");

    llvm::BasicBlock *endCheckBB = createBB("trim.end_check");
    emitBranchCond(endGtStart, endCheckBB, endEndBB);

    builder_.SetInsertPoint(endCheckBB);
    llvm::Value *endPrev = emitSub(endIdx, emitConstInt(i64Ty_, 1), "end_prev");
    llvm::Value *endPtr = emitGEP(i8Ty_, s, endPrev, "end_ptr");
    llvm::Value *endCh = emitLoad(i8Ty_, endPtr, "end_ch");
    llvm::Value *isWs2 = emitIsWhitespace(endCh);
    emitBranchCond(isWs2, endBodyBB, endEndBB);

    builder_.SetInsertPoint(endBodyBB);
    emitStore(endPrev, endVar);
    emitBranchUncond(endCondBB);

    builder_.SetInsertPoint(endEndBB);
    llvm::Value *finalEnd = emitLoad(i64Ty_, endVar, "final_end");

    llvm::Value *resultLen = emitSub(finalEnd, finalStart, "trim_result_len");
    llvm::Value *zero = emitConstInt(i64Ty_, 0);
    llvm::Value *isNeg = emitICmpSLT(resultLen, zero, "trim_neg");
    llvm::Value *safeLen = emitSelect(isNeg, zero, resultLen, "trim_safe_len");
    llvm::Value *buf = emitRuntimeCallDirect("__ry_string_make_uninit", ptrTy_,
                                             {i64Ty_}, {safeLen}, "trim_buf");
    llvm::Value *srcPtr = emitGEP(i8Ty_, s, finalStart, "trim_src");
    emitRuntimeCallDirect("memcpy", ptrTy_, {ptrTy_, ptrTy_, i64Ty_},
                          {buf, srcPtr, safeLen}, "");
    arc_str_owned_values_.insert(buf);
    return buf;
}

// trimStart(s) → str
// #2072 [C]=(ii): forward whitespace scan then memcpy, via ry_emit_* primitives.
llvm::Value *CodeGen::emitStrOp_trim_start(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *s = emitExpr(*e.args[0]);
    if (s->getType() != ptrTy_)
        codegenError("trimStart() requires str argument");

    llvm::Value *len = emitStringByteLen(s);

    llvm::Value *startVar = emitAlloca(i64Ty_, "tstart_start");
    emitStore(emitConstInt(i64Ty_, 0), startVar);

    llvm::BasicBlock *condBB = createBB("tstart.cond");
    llvm::BasicBlock *checkBB = createBB("tstart.check");
    llvm::BasicBlock *bodyBB = createBB("tstart.body");
    llvm::BasicBlock *endBB = createBB("tstart.end");

    emitBranchUncond(condBB);
    builder_.SetInsertPoint(condBB);
    llvm::Value *idx = emitLoad(i64Ty_, startVar, "tstart_idx");
    emitBranchCond(emitICmpSLT(idx, len, "tstart_bound"), checkBB, endBB);

    builder_.SetInsertPoint(checkBB);
    llvm::Value *ptr = emitGEP(i8Ty_, s, idx, "tstart_ptr");
    llvm::Value *ch = emitLoad(i8Ty_, ptr, "tstart_ch");
    llvm::Value *isWs = emitIsWhitespace(ch);
    emitBranchCond(isWs, bodyBB, endBB);

    builder_.SetInsertPoint(bodyBB);
    emitStore(emitAdd(idx, emitConstInt(i64Ty_, 1), "tstart_next"), startVar);
    emitBranchUncond(condBB);

    builder_.SetInsertPoint(endBB);
    llvm::Value *finalStart = emitLoad(i64Ty_, startVar, "tstart_final");
    llvm::Value *resultLen = emitSub(len, finalStart, "tstart_rlen");
    llvm::Value *buf = emitRuntimeCallDirect("__ry_string_make_uninit", ptrTy_,
                                             {i64Ty_}, {resultLen}, "tstart_buf");
    llvm::Value *srcPtr = emitGEP(i8Ty_, s, finalStart, "tstart_src");
    emitRuntimeCallDirect("memcpy", ptrTy_, {ptrTy_, ptrTy_, i64Ty_},
                          {buf, srcPtr, resultLen}, "");
    arc_str_owned_values_.insert(buf);
    return buf;
}

// trimEnd(s) → str
// #2072 [C]=(ii): backward whitespace scan then memcpy, via ry_emit_*
// primitives. The memcpy source is `s` directly (prefix copy), unlike trim /
// trimStart which offset by the trimmed start.
llvm::Value *CodeGen::emitStrOp_trim_end(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *s = emitExpr(*e.args[0]);
    if (s->getType() != ptrTy_)
        codegenError("trimEnd() requires str argument");

    llvm::Value *len = emitStringByteLen(s);

    llvm::Value *endVar = emitAlloca(i64Ty_, "tend_end");
    emitStore(len, endVar);

    llvm::BasicBlock *condBB = createBB("tend.cond");
    llvm::BasicBlock *checkBB = createBB("tend.check");
    llvm::BasicBlock *bodyBB = createBB("tend.body");
    llvm::BasicBlock *endBB = createBB("tend.end");

    emitBranchUncond(condBB);
    builder_.SetInsertPoint(condBB);
    llvm::Value *endIdx = emitLoad(i64Ty_, endVar, "tend_idx");
    llvm::Value *gtZero = emitICmpSGT(endIdx, emitConstInt(i64Ty_, 0), "tend_gt0");
    emitBranchCond(gtZero, checkBB, endBB);

    builder_.SetInsertPoint(checkBB);
    llvm::Value *prevIdx = emitSub(endIdx, emitConstInt(i64Ty_, 1), "tend_prev");
    llvm::Value *ptr = emitGEP(i8Ty_, s, prevIdx, "tend_ptr");
    llvm::Value *ch = emitLoad(i8Ty_, ptr, "tend_ch");
    llvm::Value *isWs = emitIsWhitespace(ch);
    emitBranchCond(isWs, bodyBB, endBB);

    builder_.SetInsertPoint(bodyBB);
    emitStore(prevIdx, endVar);
    emitBranchUncond(condBB);

    builder_.SetInsertPoint(endBB);
    llvm::Value *finalEnd = emitLoad(i64Ty_, endVar, "tend_final");
    llvm::Value *buf = emitRuntimeCallDirect("__ry_string_make_uninit", ptrTy_,
                                             {i64Ty_}, {finalEnd}, "tend_buf");
    emitRuntimeCallDirect("memcpy", ptrTy_, {ptrTy_, ptrTy_, i64Ty_},
                          {buf, s, finalEnd}, "");
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

        // #2095 — header load, ARC alloc, and the rev_data data-buffer
        // allocation stay C++-side to preserve the baseline's pre-loop
        // instruction order (the data-buffer call uses the LLVM IR builder to
        // emit a call to libc's allocator; the linter ban targets raw
        // C-runtime use in the codegen process, not emitted IR). The boundary
        // owns the reverse loop body + named StructGEP stores for the new
        // header fields (rev_new_len/cap/data via LIST_FIELD_LEN/CAP/DATA).
        // Post-loop ARC retain dispatch (below) stays C++-side because it
        // touches ValueMetadata.
        auto lf = loadListHeader(arg, "rev");
        llvm::Value *len = lf.len;
        llvm::Value *srcData = lf.data;

        llvm::Value *newHeader = emitArcAllocCollectionHeader(listHeaderTy_);
        llvm::Value *dataSize = builder_.CreateMul(len, llvm::ConstantInt::get(i64Ty_, elemSize), "rev_dsize");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "rev_data");

        ry_emit_list_reverse(emit_ctx_,
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(len)),
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(srcData)),
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(newData)),
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(newHeader)),
            ry::llvm_emit::asRyType(listHeaderTy_),
            ry::llvm_emit::asRyType(elemTy));

        // Per-element copy loop above duplicates ARC pointers without
        // retaining them; propagateMeta inherits the destructor, so source
        // and result would double-release. Mirror emitCollOp_slice
        // (#1204 / #1667).
        {
            const ValueMetadata *srcMeta = getMeta(arg);
            const std::string elemSigSnap =
                srcMeta ? resolveTypeAlias(srcMeta->list_elem_type_name)
                         : std::string{};
            if (elemSigSnap.size() >= 2 && elemSigSnap.front() == '(' &&
                elemSigSnap.back() == ')') {
                if (auto *tupleTy = llvm::dyn_cast<llvm::StructType>(elemTy)) {
                    emitTupleElemRetainLoop(newData, len, "rev_telem",
                                             elemSigSnap, tupleTy);
                }
            } else {
                CollectionKind elemArcKind = CollectionKind::List;
                if (elementTypeIsArcManaged(arg, CollectionKind::List,
                                             &elemArcKind)) {
                    emitCowRetainArcElements(newData, len, "rev_elem",
                                              elemArcKind);
                }
            }
        }

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
        auto fn = getRuntimeFn("__ry_regex_split",
                               ptrTy_, {ptrTy_, i64Ty_, ptrTy_, i64Ty_});
        llvm::Value *r = builder_.CreateCall(fn,
            {delim, emitStringByteLen(delim), s, emitStringByteLen(s)},
            "regex_split");
        llvm::Value *isNull = builder_.CreateICmpEQ(
            r, llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)),
            "regex_split_is_null");
        llvm::BasicBlock *errBB = createBB("regex_split.err");
        llvm::BasicBlock *okBB = createBB("regex_split.ok");
        emitBranchCond(isNull, errBB, okBB);
        builder_.SetInsertPoint(errBB);
        auto errFn = getRuntimeFn("__ry_regex_get_last_error", ptrTy_, {});
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
    llvm::Value *isEmptyDelim = emitICmpEQ(
        delimLen, emitConstInt(i64Ty_, 0), "split_empty_delim");

    llvm::BasicBlock *emptyDelimBB = createBB("split.empty_delim");
    llvm::BasicBlock *normalBB = createBB("split.normal");
    llvm::BasicBlock *doneBB = createBB("split.done");

    emitBranchCond(isEmptyDelim, emptyDelimBB, normalBB);

    // --- Empty delimiter path: call __ry_split_chars runtime ---
    builder_.SetInsertPoint(emptyDelimBB);
    llvm::Value *charsResult = emitRuntimeCallDirect("__ry_split_chars", ptrTy_,
                                                     {ptrTy_, i64Ty_}, {s, emitStringByteLen(s)},
                                                     "split_chars");
    emitBranchUncond(doneBB);

    // --- Normal delimiter path: NUL-safe runtime helper (#1051) ---
    builder_.SetInsertPoint(normalBB);
    llvm::Value *normalResult = emitRuntimeCallDirect("__ry_str_split", ptrTy_,
                                                      {ptrTy_, i64Ty_, ptrTy_, i64Ty_},
                                                      {s, emitStringByteLen(s), delim, delimLen},
                                                      "split_normal");
    emitBranchUncond(doneBB);

    // --- Merge point ---
    builder_.SetInsertPoint(doneBB);
    llvm::PHINode *result = createPhi(ptrTy_, {}, "split_result");
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
// #2096: all builder_.Create* routed through ry_emit_* primitives so this
// function carries no inline IR generation (loadListHeader is a C++-only
// header-load helper; arc_str_owned_values_ stays a C++ side-effect table).
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

    // Pre-register memcpy so its `declare` lands BEFORE __ry_string_make_uninit's
    // (matches the pre-#2096 order where `auto memcpyFn = getStdlibMemcpy()` ran
    // before `getRuntimeFn("__ry_string_make_uninit", ...)`; emitRuntimeCallDirect
    // would otherwise register make_uninit first and reorder the declares).
    (void)getStdlibMemcpy();

    auto lf = loadListHeader(listPtr, "join");
    llvm::Value *listLen = lf.len;
    llvm::Value *listData = lf.data;
    llvm::Value *sepLen = emitStringByteLen(sep);
    llvm::Value *zero = emitConstInt(i64Ty_, 0);
    llvm::Value *one = emitConstInt(i64Ty_, 1);

    llvm::Value *totalVar = emitAlloca(i64Ty_, "join_total");
    emitStore(zero, totalVar);
    llvm::Value *iVar = emitAlloca(i64Ty_, "join_i");
    emitStore(zero, iVar);

    llvm::BasicBlock *len1CondBB = createBB("join.len_cond");
    llvm::BasicBlock *len1BodyBB = createBB("join.len_body");
    llvm::BasicBlock *len1EndBB = createBB("join.len_end");

    emitBranchUncond(len1CondBB);
    builder_.SetInsertPoint(len1CondBB);
    llvm::Value *i1 = emitLoad(i64Ty_, iVar, "join_i1");
    emitBranchCond(emitICmpSLT(i1, listLen, "join_len_cond"), len1BodyBB, len1EndBB);

    builder_.SetInsertPoint(len1BodyBB);
    llvm::Value *i1Cur = emitLoad(i64Ty_, iVar, "join_i1_cur");
    llvm::Value *elemPtr = emitGEP(ptrTy_, listData, i1Cur, "join_elem_ptr");
    llvm::Value *elem = emitLoad(ptrTy_, elemPtr, "join_elem");
    llvm::Value *elemLen = emitStringByteLen(elem);
    llvm::Value *total = emitLoad(i64Ty_, totalVar, "join_total_cur");
    llvm::Value *newTotal = emitAdd(total, elemLen, "join_total_add");
    emitStore(newTotal, totalVar);
    emitStore(emitAdd(i1Cur, one, ""), iVar);
    emitBranchUncond(len1CondBB);

    builder_.SetInsertPoint(len1EndBB);
    llvm::Value *elemTotal = emitLoad(i64Ty_, totalVar, "join_elem_total");
    llvm::Value *sepCount = emitSub(listLen, one, "join_sep_count");
    llvm::Value *isPositive = emitICmpSGT(listLen, zero, "join_has_elems");
    llvm::Value *safeSepCount = emitSelect(isPositive, sepCount, zero, "safe_sep_count");
    llvm::Value *sepTotal = emitMul(safeSepCount, sepLen, "join_sep_total");
    llvm::Value *grandTotal = emitAdd(elemTotal, sepTotal, "join_grand_total");
    llvm::Value *buf = emitRuntimeCallDirect("__ry_string_make_uninit", ptrTy_,
                                             {i64Ty_}, {grandTotal}, "join_buf");

    llvm::Value *dstVar = emitAlloca(ptrTy_, "join_dst");
    emitStore(buf, dstVar);
    emitStore(zero, iVar);

    llvm::BasicBlock *buildCondBB = createBB("join.build_cond");
    llvm::BasicBlock *buildBodyBB = createBB("join.build_body");
    llvm::BasicBlock *buildEndBB = createBB("join.build_end");

    emitBranchUncond(buildCondBB);
    builder_.SetInsertPoint(buildCondBB);
    llvm::Value *i2 = emitLoad(i64Ty_, iVar, "join_i2");
    emitBranchCond(emitICmpSLT(i2, listLen, "join_build_cond"), buildBodyBB, buildEndBB);

    builder_.SetInsertPoint(buildBodyBB);
    llvm::Value *i2Cur = emitLoad(i64Ty_, iVar, "join_i2_cur");

    llvm::Value *notFirst = emitICmpSGT(i2Cur, zero, "join_not_first");
    llvm::BasicBlock *sepBB = createBB("join.sep");
    llvm::BasicBlock *elemBB = createBB("join.elem");
    emitBranchCond(notFirst, sepBB, elemBB);

    builder_.SetInsertPoint(sepBB);
    llvm::Value *dstBeforeSep = emitLoad(ptrTy_, dstVar, "dst_before_sep");
    emitRuntimeCallDirect("memcpy", ptrTy_, {ptrTy_, ptrTy_, i64Ty_},
                          {dstBeforeSep, sep, sepLen}, "");
    llvm::Value *dstAfterSep = emitGEP(i8Ty_, dstBeforeSep, sepLen, "dst_after_sep");
    emitStore(dstAfterSep, dstVar);
    emitBranchUncond(elemBB);

    builder_.SetInsertPoint(elemBB);
    llvm::Value *dstForElem = emitLoad(ptrTy_, dstVar, "dst_for_elem");
    llvm::Value *elemPtr2 = emitGEP(ptrTy_, listData, i2Cur, "join_elem_ptr2");
    llvm::Value *elem2 = emitLoad(ptrTy_, elemPtr2, "join_elem2");
    llvm::Value *elem2Len = emitStringByteLen(elem2);
    emitRuntimeCallDirect("memcpy", ptrTy_, {ptrTy_, ptrTy_, i64Ty_},
                          {dstForElem, elem2, elem2Len}, "");
    llvm::Value *dstAfterElem = emitGEP(i8Ty_, dstForElem, elem2Len, "dst_after_elem");
    emitStore(dstAfterElem, dstVar);
    emitStore(emitAdd(i2Cur, one, ""), iVar);
    emitBranchUncond(buildCondBB);

    builder_.SetInsertPoint(buildEndBB);
    arc_str_owned_values_.insert(buf);
    return buf;
}

} // namespace ry
