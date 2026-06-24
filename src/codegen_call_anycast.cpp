#include "ry/codegen.hpp"
#include "ry/diagnostic/diagnostic.hpp"
#include "ry/ry_layout.hpp"
#include "ry/util/type_name.hpp"

namespace ry {

// Explicit `any` cast / type-test builtins (#2315).
//
// Two surfaces are exposed:
//   - `asType[T](v: any) -> Result<T, Error>` — checked cast that reuses
//     `tryUnwrapFromAny` with `callerLabel = "asType"` so the error
//     prefix reads "asType[T]: ..." instead of "load[T]: ...".
//   - `isType[T](v: any) -> bool` — runtime tag comparison; for records
//     it walks the descriptor chain via `__ry_record_is_subtype_desc`.
//
// Both go through the `name<T>` callee form that `parser_expr` already
// produces for any `ident[T](...)` call site, so no parser change is
// required. Dispatched from `codegen_call_dispatch.cpp` via
// `emitBuiltinAnyCast`.

static llvm::Value *emitAsType(CodeGen &cg, const CallExpr &e,
                                const std::string &typeArg) {
    cg.requireArgs(e, 1);
    llvm::Value *val = cg.emitExpr(*e.args[0]);

    // Auto-wrap concrete values for ergonomics (matches json5.stringify).
    // For an `any`-typed source this is a no-op.
    if (val->getType() != cg.anyTy_)
        val = cg.wrapInAny(val);

    llvm::Type *targetTy = cg.resolveType(typeArg);
    if (!targetTy)
        cg.codegenError("asType[" + typeArg +
                        "]: unknown target type '" + typeArg + "'");

    llvm::Value *result =
        cg.tryUnwrapFromAny(val, targetTy, typeArg, "asType");
    cg.propagateTypeMeta("Result<" + typeArg + ", Error>", result);
    return result;
}

static llvm::Value *emitIsType(CodeGen &cg, const CallExpr &e,
                                const std::string &typeArg) {
    cg.requireArgs(e, 1);
    llvm::Value *val = cg.emitExpr(*e.args[0]);

    // `isType[any](_)` is true regardless of source — useful as a catch-all
    // and consistent with `asType[any]` (which would Ok-wrap any value).
    if (typeArg == "any")
        return llvm::ConstantInt::get(cg.i1Ty_, 1);

    // Auto-wrap concrete sources so callers can write `isType[T](someInt)`
    // without manual `: any` annotation. For an `any` source this is a no-op.
    if (val->getType() != cg.anyTy_)
        val = cg.wrapInAny(val);

    llvm::Type *targetTy = cg.resolveType(typeArg);
    if (!targetTy)
        cg.codegenError("isType[" + typeArg +
                        "]: unknown target type '" + typeArg + "'");

    // For Option<T> / Result<T,E> / organic enums the runtime stores all of
    // them under the same RyAnyTag::Enum tag, so a bare tag-compare cannot
    // distinguish "is Option<int>" from "is Result<int, str>". Until per-
    // enum descriptor inspection is added (separate issue), reject these
    // targets so users do not get surprising false positives.
    if (auto *st = llvm::dyn_cast<llvm::StructType>(targetTy)) {
        if (cg.isOptionType(st) || cg.isResultType(st) ||
            !cg.findAdtEnumName(st).empty())
            cg.codegenError("isType[" + typeArg +
                            "]: enum / Option / Result targets are not yet "
                            "supported; use case-based unwrap instead");
    }
    if (targetTy == cg.i64Ty_ && !typeArg.empty() &&
        cg.isSimpleEnumTypeName(typeArg))
        cg.codegenError("isType[" + typeArg +
                        "]: simple-enum target is not yet supported; use "
                        "case-based unwrap instead");

    llvm::Value *tag = cg.builder_.CreateExtractValue(val, {0}, "any.tag");

    // Records: target is a StructType with a registered record info. Tag
    // match alone is insufficient — the descriptor must be a (sub)type of
    // the requested record. The runtime helper walks `parent_desc` and
    // returns 1 / 0. Branch on tag-eq first so a non-record source skips
    // the descriptor load (which would alias non-record data otherwise).
    if (auto *recordStructTy = llvm::dyn_cast<llvm::StructType>(targetTy)) {
        if (cg.findRecordInfoForType(recordStructTy)) {
            std::string typeName = cg.findRecordTypeName(recordStructTy);
            if (typeName.empty())
                cg.codegenError("isType[" + typeArg +
                                "]: could not resolve record type name");
            auto *expectedDesc =
                cg.getOrCreateRecordDescriptor(typeName, recordStructTy);

            llvm::Value *isRecord = cg.builder_.CreateICmpEQ(
                tag,
                llvm::ConstantInt::get(
                    cg.i64Ty_, static_cast<int64_t>(RyAnyTag::Record)),
                "isType.is_rec");

            llvm::Function *fn = cg.builder_.GetInsertBlock()->getParent();
            auto *checkBB = cg.createBBInFn("isType.rec.check", fn);
            auto *doneBB = cg.createBBInFn("isType.rec.done", fn);
            llvm::BasicBlock *tagFalseBB = cg.builder_.GetInsertBlock();
            cg.emitBranchCond(isRecord, checkBB, doneBB);

            cg.builder_.SetInsertPoint(checkBB);
            // any.data[8] is the data-region pointer for the record box
            // ([ArcHeader 16B][descriptor ptr 8B][payload]); descriptor
            // lives at offset 0 of that region.
            llvm::Value *dataPtr = cg.loadAnyDataPtr(val, "any.rec");
            llvm::Value *actualDesc = cg.builder_.CreateLoad(
                cg.ptrTy_, dataPtr, "any.rec.actual_desc");
            auto subtypeFn = cg.getRuntimeFn(
                "__ry_record_is_subtype_desc", cg.i64Ty_,
                {cg.ptrTy_, cg.ptrTy_});
            llvm::Value *subtypeRes = cg.builder_.CreateCall(
                subtypeFn, {actualDesc, expectedDesc},
                "any.rec.is_subtype");
            llvm::Value *descMatch = cg.builder_.CreateICmpNE(
                subtypeRes, llvm::ConstantInt::get(cg.i64Ty_, 0),
                "any.rec.desc_match");
            llvm::BasicBlock *checkEndBB = cg.builder_.GetInsertBlock();
            cg.emitBranchUncond(doneBB);

            cg.builder_.SetInsertPoint(doneBB);
            llvm::PHINode *phi =
                cg.builder_.CreatePHI(cg.i1Ty_, 2, "isType.result");
            phi->addIncoming(llvm::ConstantInt::get(cg.i1Ty_, 0), tagFalseBB);
            phi->addIncoming(descMatch, checkEndBB);
            return phi;
        }
    }

    // Resolve the expected RyAnyTag for primitives / collections. Collections
    // need the type-name to disambiguate List vs Map vs Set since `targetTy`
    // is `ptrTy_` for all three. Element types are erased at runtime, so
    // `isType[List<int>]` and `isType[List<any>]` are equivalent.
    int64_t expectedTag;
    if (targetTy == cg.ptrTy_ && !typeArg.empty()) {
        std::string resolved = cg.resolveTypeAlias(typeArg);
        if (ry::util::isListTypeName(resolved))
            expectedTag = static_cast<int64_t>(RyAnyTag::List);
        else if (ry::util::isMapTypeName(resolved))
            expectedTag = static_cast<int64_t>(RyAnyTag::Map);
        else if (ry::util::isSetTypeName(resolved))
            expectedTag = static_cast<int64_t>(RyAnyTag::Set);
        else
            expectedTag = cg.getAnyTypeTag(targetTy);
    } else {
        expectedTag = cg.getAnyTypeTag(targetTy);
    }

    return cg.builder_.CreateICmpEQ(
        tag,
        llvm::ConstantInt::get(cg.i64Ty_,
                               static_cast<uint64_t>(expectedTag)),
        "isType.tag_eq");
}

// Strip a `prefix` (e.g. "asType<") and trailing '>' from a parser-produced
// callee like "asType<int>" to recover the type-name. Returns true on match
// and writes the substring into `out`. Mirrors the same idiom in
// codegen_call_json{,5}.cpp's `load<T>` interception.
static bool stripGenericCallee(const std::string &callee,
                                std::string_view prefix,
                                std::string &out) {
    if (callee.size() > prefix.size() + 1 &&
        callee.compare(0, prefix.size(), prefix) == 0 &&
        callee.back() == '>') {
        out = callee.substr(prefix.size(),
                            callee.size() - prefix.size() - 1);
        return true;
    }
    return false;
}

llvm::Value *CodeGen::emitBuiltinAnyCast(const CallExpr &e) {
    std::string typeArg;
    if (stripGenericCallee(e.callee, "asType<", typeArg))
        return emitAsType(*this, e, typeArg);
    if (e.callee == "asType")
        codegenError(
            "asType() requires an explicit type argument: asType[T](value). "
            "Pick a concrete T such as asType[int], asType[str], or "
            "asType[Map<str, any>].");

    if (stripGenericCallee(e.callee, "isType<", typeArg))
        return emitIsType(*this, e, typeArg);
    if (e.callee == "isType")
        codegenError(
            "isType() requires an explicit type argument: isType[T](value). "
            "Pick a concrete T such as isType[int] or isType[List<any>].");

    return nullptr;
}

} // namespace ry
