#include "ry/codegen.hpp"
#include "ry/llvm_emit/api.h"
#include "ry/llvm_emit/cast_helpers.hpp"
#include <llvm/IR/Operator.h>


namespace ry {

bool CodeGen::isAnyType(llvm::Type *ty) const {
    return ty == anyTy_;
}

bool CodeGen::canAnyHoldType(llvm::Type *ty) const {
    return ty == i64Ty_ || ty == f64Ty_ || ty == i1Ty_ || ty == ptrTy_;
}

bool CodeGen::isAnyArithOp(const std::string &op) {
    // Binary operators rejected by the `[strict-any/any-arithmetic]` rule:
    // the seven arithmetic ops plus the four ordering comparisons. The rule
    // fires at the call site in emitBinaryOp before any operand is wrapped
    // in any, so emitAnyBinaryOp never sees them. Equality (`==`, `!=`) is
    // intentionally absent — `__ry_any_eq` returns 0 on type mismatch
    // (safe), and is the only operator emitAnyBinaryOp still handles.
    return op == "+" || op == "-" || op == "*" || op == "/" ||
           op == "%" || op == "//" || op == "**" ||
           op == "<" || op == "<=" || op == ">" || op == ">=";
}

int64_t CodeGen::getAnyTypeTag(llvm::Type *ty) {
    if (ty == i64Ty_)  return static_cast<int64_t>(RyAnyTag::Int);
    if (ty == f64Ty_)  return static_cast<int64_t>(RyAnyTag::Float);
    if (ty == i1Ty_)   return static_cast<int64_t>(RyAnyTag::Bool);
    if (ty == ptrTy_)  return static_cast<int64_t>(RyAnyTag::Str);
    codegenError("type error: 'any' can only hold int/float/bool/str");
}

int64_t CodeGen::getAnyTypeTagForValue(llvm::Value *val) {
    if (val->getType() == ptrTy_) {
        if (auto *meta = getMeta(val)) {
            if (meta->list_elem) return static_cast<int64_t>(RyAnyTag::List);
            if (meta->map_key || meta->map_value)
                return static_cast<int64_t>(RyAnyTag::Map);
            if (meta->set_elem) return static_cast<int64_t>(RyAnyTag::Set);
        }
    }
    return getAnyTypeTag(val->getType());
}

bool CodeGen::isNonStrPointer(llvm::Value *val) {
    if (val->getType() != ptrTy_) return false;
    auto *meta = getMeta(val);
    return meta && meta->hasAnyMeta();
}

bool CodeGen::isStringValue(llvm::Value *val) {
    // Positive-evidence predicate for `-24` retain dispatch (#2248).
    // Type-discrimination callers want `isStrLike`. See codegen-arc-cow.md.
    if (val->getType() != ptrTy_) return false;
    if (arc_str_owned_values_.count(val) > 0) return true;
    if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
        if (auto *alloca = llvm::dyn_cast<llvm::AllocaInst>(
                load->getPointerOperand())) {
            if (arc_str_managed_vars_.count(alloca) > 0) return true;
        }
    }
    if (llvm::isa<llvm::GlobalVariable>(val)) return true;
    if (auto *gep = llvm::dyn_cast<llvm::GEPOperator>(val)) {
        if (llvm::isa<llvm::GlobalVariable>(
                gep->getPointerOperand()->stripPointerCasts())) {
            return true;
        }
    }
    auto *meta = getMeta(val);
    return meta && meta->str_elem;
}

bool CodeGen::isStrLike(llvm::Value *val) {
    return val->getType() == ptrTy_ && !isNonStrPointer(val);
}

llvm::Value *CodeGen::wrapInAny(llvm::Value *val) {
    // Enum types (simple, ADT, `Option<T>`, `Result<V,E>`) share the
    // record-style heap-box layout (#1798): `[ ArcHeader | descriptor ptr |
    // payload ]`. Detect first so an organic-enum / Option / Result struct
    // does not fall into the record path's `findRecordInfoForType` rejection.
    // Simple enums (LLVM i64 with `enum_value_type` metadata) are boxed too —
    // tagging them as Int would lose enum identity and break equality / unwrap.
    if (std::string enumName = findEnumLikeTypeNameForBoxing(val); !enumName.empty()) {
        llvm::Type *payloadTy = val->getType();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        auto *layoutTy = enumBoxLayoutType(payloadTy);
        uint64_t boxDataSize = dl.getTypeAllocSize(layoutTy);
        auto *desc = getOrCreateEnumDescriptor(enumName, payloadTy);

        // Field-wise retain on existing aliases. Fresh constructions (Call /
        // Invoke) are sole owners and need no retain — mirror the record
        // ARC reassignment guard. Done on the CodeGen side because the helper
        // depends on enum-name + payload-type metadata that does not cross the
        // boundary surface.
        if (!llvm::isa<llvm::CallInst>(val) && !llvm::isa<llvm::InvokeInst>(val)) {
            emitEnumBoxArcFieldsRetain(val, enumName, payloadTy);
        }

        RyAnyWrapDesc wrapDesc{};
        wrapDesc.kind = static_cast<int>(AnyWrapKind::EnumBox);
        wrapDesc.target_tag = static_cast<int64_t>(RyAnyTag::Enum);
        wrapDesc.val_id =
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(val));
        wrapDesc.descriptor_id =
            desc ? ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(desc))
                 : 0;
        wrapDesc.box_layout_ty = ry::llvm_emit::asRyType(layoutTy);
        wrapDesc.box_data_size = boxDataSize;
        wrapDesc.any_ty = ry::llvm_emit::asRyType(anyTy_);
        return ry::llvm_emit::asLlvmValue(ry_emit_resolve(
            emit_ctx_, ry_emit_any_wrap(emit_ctx_, &wrapDesc)));
    }

    // Record types are stored on the heap as `[ ArcHeader (16B) | descriptor ptr
    // (8B) | record struct ]` because the inner struct is generally larger than
    // the 8-byte `data[8]` slot. The slot holds the box's data-region pointer
    // (i.e. headerPtr + 16). Cross-function-boundary type info is preserved by
    // the per-type descriptor at offset 0 of the data region, so `any`-typed
    // function returns / aliases survive even when the static type name is lost.
    if (auto *recordStructTy = llvm::dyn_cast<llvm::StructType>(val->getType())) {
        const RecordInfo *info = findRecordInfoForType(recordStructTy);
        if (!info)
            codegenError("type error: 'any' cannot hold this struct type — "
                         "only record types declared with `record` are supported");
        std::string typeName = findRecordTypeName(recordStructTy);
        if (typeName.empty())
            codegenError("type error: 'any' record wrap could not resolve "
                         "source-level type name");

        const llvm::DataLayout &dl = mod_->getDataLayout();
        auto *layoutTy = recordBoxLayoutType(recordStructTy);
        uint64_t boxDataSize = dl.getTypeAllocSize(layoutTy);
        auto *desc = getOrCreateRecordDescriptor(typeName, recordStructTy);

        // Per [[codegen-arc-cow]] "Record ARC reassignment guard": every value
        // that is not a fresh `CallInst` / `InvokeInst` is treated as a view —
        // including `InsertValueInst` chains from record literals. The helper
        // internally skips inline-owned values via `arc_owned_values_` /
        // `arc_str_owned_values_`, so plain literal field values (newly-built
        // collections, fresh strings) are not over-retained.
        if (!llvm::isa<llvm::CallInst>(val) && !llvm::isa<llvm::InvokeInst>(val)) {
            emitRecordArcFieldsRetain(val, recordStructTy);
        }

        RyAnyWrapDesc wrapDesc{};
        wrapDesc.kind = static_cast<int>(AnyWrapKind::RecordBox);
        wrapDesc.target_tag = static_cast<int64_t>(RyAnyTag::Record);
        wrapDesc.val_id =
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(val));
        wrapDesc.descriptor_id =
            desc ? ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(desc))
                 : 0;
        wrapDesc.box_layout_ty = ry::llvm_emit::asRyType(layoutTy);
        wrapDesc.box_data_size = boxDataSize;
        wrapDesc.any_ty = ry::llvm_emit::asRyType(anyTy_);
        return ry::llvm_emit::asLlvmValue(ry_emit_resolve(
            emit_ctx_, ry_emit_any_wrap(emit_ctx_, &wrapDesc)));
    }

    // Collections (List / Map / Set) are wrap-eligible from #1697; resources,
    // function pointers, enums and other non-collection pointer-shaped
    // values remain rejected. Discriminate via metadata.
    bool isCollection = false;
    auto *meta = (val->getType() == ptrTy_) ? getMeta(val) : nullptr;
    if (meta) {
        isCollection = meta->list_elem || meta->map_key || meta->map_value ||
                       meta->set_elem;
    }
    if (isNonStrPointer(val) && !isCollection)
        codegenError("type error: 'any' can only hold int/float/bool/str, "
                     "List/Map/Set, record, and enum types; non-collection "
                     "pointer types (resources, function pointers, etc.) are "
                     "not supported");

    int64_t tag = isCollection ? getAnyTypeTagForValue(val)
                               : getAnyTypeTag(val->getType());

    // typed_coll registration must happen BEFORE the boundary emits its internal
    // ARC retain — `__ry_any_register_typed_coll` is keyed by the header
    // pointer and must be associated before the retain bumps the strong
    // count. Element-type metadata that drives this lookup is CodeGen-private
    // (lives on `ValueMetadata`), so the call stays here.
    if (isCollection) {
        // Element-type-erasure (#1697) means the stringify_any path cannot
        // see whether the inner buffer uses 16-byte RyAny stride or a
        // narrower native stride. For typed-non-any collections, record
        // the source-level type name in a side-table keyed by the inner
        // header pointer so `__ry_json_stringify_any` can panic with an
        // actionable message instead of reading OOB (#1811).
        bool typedNonAnyList = meta->list_elem && meta->list_elem != anyTy_;
        bool typedNonAnyMapVal = meta->map_value && meta->map_value != anyTy_;
        // Map keys collapse to `ptrTy_` for both `Map<str, V>` and
        // `Map<List<_>, V>` / `Map<Map<_,_>, V>` / `Map<Set<_>, V>` under
        // opaque pointers, so LLVM-type equality cannot pick `str` keys out.
        // `stringify_any`'s Map arm reads `hdr->keys[i]` as `char**` and
        // calls `stringByteLen(keys[i])` on it, which is OOB for `int` keys
        // (8-byte int read as pointer) and for any non-`str` pointer key.
        // Use the stamped source-level type name as the discriminator;
        // require a non-empty name so unknown / unstamped cases keep their
        // existing behavior (no false-positive register on legitimate
        // `Map<str, any>` whose name happened to be lost).
        bool typedNonStrMapKey = meta->map_key &&
                                 !meta->map_key_type_name.empty() &&
                                 meta->map_key_type_name != "str";
        bool typedNonAnySet = meta->set_elem && meta->set_elem != anyTy_;
        if (typedNonAnyList || typedNonAnyMapVal || typedNonStrMapKey ||
            typedNonAnySet) {
            std::string typeName = buildTypeNameFromMeta(val);
            if (!typeName.empty()) {
                auto *nameGlobal = cachedGlobalString(typeName, ".any.typed_coll.name");
                llvm::FunctionType *regTy = llvm::FunctionType::get(
                    builder_.getVoidTy(), {ptrTy_, ptrTy_}, false);
                llvm::FunctionCallee regFn = mod_->getOrInsertFunction(
                    "__ry_any_register_typed_coll", regTy);
                builder_.CreateCall(regFn, {val, nameGlobal});
            }
        }
    }

    // isStringValue is positive-evidence (#2248); no inline re-check needed.
    bool doStrRetain = !isCollection && isStringValue(val);

    RyAnyWrapDesc wrapDesc{};
    wrapDesc.kind = static_cast<int>(AnyWrapKind::NonBox);
    wrapDesc.target_tag = tag;
    wrapDesc.val_id =
        ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(val));
    wrapDesc.do_collection_retain = isCollection ? 1 : 0;
    wrapDesc.do_str_retain = doStrRetain ? 1 : 0;
    wrapDesc.any_ty = ry::llvm_emit::asRyType(anyTy_);
    return ry::llvm_emit::asLlvmValue(ry_emit_resolve(
        emit_ctx_, ry_emit_any_wrap(emit_ctx_, &wrapDesc)));
}

llvm::Value *CodeGen::buildUnitAny() {
    llvm::AllocaInst *tmp = builder_.CreateAlloca(anyTy_, nullptr, "any.unit.tmp");
    auto *tagPtr = builder_.CreateStructGEP(anyTy_, tmp, 0, "any.unit.tag");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, static_cast<int64_t>(RyAnyTag::Unit)), tagPtr);
    auto *dataPtr = builder_.CreateStructGEP(anyTy_, tmp, 1, "any.unit.data");
    builder_.CreateStore(
        llvm::Constant::getNullValue(anyTy_->getElementType(1)),
        dataPtr);
    return builder_.CreateLoad(anyTy_, tmp, "any.unit.val");
}

llvm::Value *CodeGen::loadAnyDataPtr(llvm::Value *anyVal,
                                       const llvm::Twine &nameStem) {
    llvm::AllocaInst *tmp =
        builder_.CreateAlloca(anyTy_, nullptr, nameStem + ".tmp");
    builder_.CreateStore(anyVal, tmp);
    llvm::Value *slot =
        builder_.CreateStructGEP(anyTy_, tmp, 1, nameStem + ".data.slot");
    return builder_.CreateLoad(ptrTy_, slot, nameStem + ".data");
}

llvm::Value *CodeGen::unwrapFromAny(llvm::Value *anyVal, llvm::Type *targetTy,
                                     const std::string &rawTargetTypeName) {
    // Substitute generic type-parameter names at the helper's entry so every
    // downstream dispatch (collection-arm via `ry::util::isListTypeName` /
    // `ry::util::isMapTypeName` / `ry::util::isSetTypeName`, enum-descriptor cache key in
    // `unwrapEnumFromAny`, error message, record `findRecordTypeName`) sees
    // the concrete type rather than the raw "T". No-op when `type_param_scope_`
    // is empty, so non-generic call sites stay byte-identical.
    const std::string targetTypeName = substituteTypeParamsInName(rawTargetTypeName);

    // Collection element-type guard (#1698): generic monomorphization of
    // `load[T]` (and `fn f<T>(a: any) -> T` analogs) can produce strides that
    // walk off the 16-byte RyAny slot. Only fire when substitution actually
    // rewrote a type parameter; literal annotations stay covered by the
    // var-decl side gate (#1883).
    const bool fromGenericSubstitution =
        targetTypeName != rawTargetTypeName;
    if (fromGenericSubstitution && targetTy == ptrTy_ && !targetTypeName.empty()) {
        std::string resolved = resolveTypeAlias(targetTypeName);
        if (ry::util::isListTypeName(resolved)) {
            std::string inner = ry::util::trimTypeNameSpaces(
                resolved.substr(5, resolved.size() - 6));
            if (inner != "any") {
                codegenError(
                    "unwrapping 'any' to '" + targetTypeName +
                    "' is not supported in this release: element type must "
                    "be 'any' (use 'List<any>' and unwrap elements "
                    "individually)");
            }
        } else if (ry::util::isSetTypeName(resolved)) {
            std::string inner = ry::util::trimTypeNameSpaces(
                resolved.substr(4, resolved.size() - 5));
            if (inner != "any") {
                codegenError(
                    "unwrapping 'any' to '" + targetTypeName +
                    "' is not supported in this release: element type must "
                    "be 'any' (use 'Set<any>' and unwrap elements "
                    "individually)");
            }
        } else if (ry::util::isMapTypeName(resolved)) {
            std::string innerArgs = resolved.substr(4, resolved.size() - 5);
            auto parts = splitTypeArgs(innerArgs);
            if (parts.size() == 2) {
                std::string k = ry::util::trimTypeNameSpaces(parts[0]);
                std::string v = ry::util::trimTypeNameSpaces(parts[1]);
                if (k != "str" || v != "any") {
                    codegenError(
                        "unwrapping 'any' to '" + targetTypeName +
                        "' is not supported in this release: must be "
                        "'Map<str, any>' (unwrap values individually)");
                }
            }
        }
    }

    // Enum (organic / Option / Result) unwrap. Detect BEFORE the record
    // StructType branch so an enum-struct target does not error out via
    // `findRecordInfoForType` rejection. Symmetric to wrapInAny.
    if (auto *st = llvm::dyn_cast<llvm::StructType>(targetTy)) {
        bool isEnumStruct = !findAdtEnumName(st).empty() ||
                            isOptionType(st) || isResultType(st);
        if (isEnumStruct) {
            return unwrapEnumFromAny(anyVal, st, targetTypeName);
        }
    }
    if (targetTy == i64Ty_ && !targetTypeName.empty() &&
        isSimpleEnumTypeName(targetTypeName)) {
        return unwrapEnumFromAny(anyVal, targetTy, targetTypeName);
    }

    // Record unwrap dispatches at entry: `getAnyTypeTag(targetTy)` below
    // emits `codegenError` when `targetTy` is a `StructType`, so the standard
    // 2-way path never sees record targets. Descriptor-chain walk admits
    // exact-type and subtype unwrap (#1802); unrelated records trap at
    // runtime via the descriptor-mismatch branch.
    if (auto *recordStructTy = llvm::dyn_cast<llvm::StructType>(targetTy)) {
        const RecordInfo *info = findRecordInfoForType(recordStructTy);
        if (!info)
            codegenError("type error: unwrapping 'any' to non-record struct "
                         "type is not supported");
        std::string typeName = findRecordTypeName(recordStructTy);
        if (typeName.empty())
            codegenError("type error: 'any' record unwrap could not resolve "
                         "source-level type name");

        auto *expectedDesc = getOrCreateRecordDescriptor(typeName, recordStructTy);
        auto *layoutTy = recordBoxLayoutType(recordStructTy);

        std::string mismatchMsg = "runtime error: any type mismatch (expected " +
                                  typeName + ", got non-record)\n";
        std::string descMismatchMsg =
            "runtime error: any record type mismatch (expected " + typeName +
            ", got a different record type)\n";

        RyAnyUnwrapDesc unwrapDesc{};
        unwrapDesc.kind = static_cast<int>(AnyUnwrapKind::Record);
        unwrapDesc.any_val_id =
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(anyVal));
        unwrapDesc.any_ty = ry::llvm_emit::asRyType(anyTy_);
        unwrapDesc.target_ty = ry::llvm_emit::asRyType(targetTy);
        unwrapDesc.expected_tag = static_cast<int64_t>(RyAnyTag::Record);
        unwrapDesc.mismatch_msg = mismatchMsg.c_str();
        unwrapDesc.mismatch_global_name = ".any_type_err";
        unwrapDesc.expected_desc_id =
            expectedDesc
                ? ry_emit_intern(emit_ctx_,
                                 ry::llvm_emit::asRyValue(expectedDesc))
                : 0;
        unwrapDesc.box_layout_ty = ry::llvm_emit::asRyType(layoutTy);
        unwrapDesc.record_struct_ty =
            ry::llvm_emit::asRyType(recordStructTy);
        unwrapDesc.desc_mismatch_msg = descMismatchMsg.c_str();
        unwrapDesc.desc_mismatch_global_name = ".any_rec_err";
        llvm::Value *recordVal = ry::llvm_emit::asLlvmValue(ry_emit_resolve(
            emit_ctx_, ry_emit_any_unwrap(emit_ctx_, &unwrapDesc)));
        // The unwrapped record becomes a new alias to the boxed value. Field
        // -wise retain so it can release independently from the box dtor at
        // scope exit. Caller-side per [[lowered_any]] AnyUnwrapKind::Record
        // contract — depends on `recordStructTy` which does not cross the boundary.
        emitRecordArcFieldsRetain(recordVal, recordStructTy);
        return recordVal;
    }

    // int→float auto-promotion: accept both Float and Int tags.
    if (targetTy == f64Ty_) {
        RyAnyUnwrapDesc unwrapDesc{};
        unwrapDesc.kind = static_cast<int>(AnyUnwrapKind::F64Promote);
        unwrapDesc.any_val_id =
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(anyVal));
        unwrapDesc.any_ty = ry::llvm_emit::asRyType(anyTy_);
        unwrapDesc.target_ty = ry::llvm_emit::asRyType(f64Ty_);
        unwrapDesc.mismatch_msg =
            "runtime error: any type mismatch (expected float or int)\n";
        unwrapDesc.mismatch_global_name = ".any_type_err";
        unwrapDesc.desc_mismatch_msg = "";
        unwrapDesc.desc_mismatch_global_name = "";
        return ry::llvm_emit::asLlvmValue(ry_emit_resolve(
            emit_ctx_, ry_emit_any_unwrap(emit_ctx_, &unwrapDesc)));
    }

    // Standard 2-way: exact tag match or error. For collection unwraps the
    // expected tag is derived from `targetTypeName` (List<…> / Map<…> /
    // Set<…>); empty / "str" / primitive names use the type-driven default
    // (Str tag for ptr).
    int64_t expectedTag = getAnyTypeTag(targetTy);
    bool isCollectionUnwrap = false;
    if (targetTy == ptrTy_ && !targetTypeName.empty()) {
        std::string resolved = resolveTypeAlias(targetTypeName);
        if (ry::util::isListTypeName(resolved)) {
            expectedTag = static_cast<int64_t>(RyAnyTag::List);
            isCollectionUnwrap = true;
        } else if (ry::util::isMapTypeName(resolved)) {
            expectedTag = static_cast<int64_t>(RyAnyTag::Map);
            isCollectionUnwrap = true;
        } else if (ry::util::isSetTypeName(resolved)) {
            expectedTag = static_cast<int64_t>(RyAnyTag::Set);
            isCollectionUnwrap = true;
        }
    }
    // str unwrap creates a new alias to the inner StringHeader.
    bool isStrUnwrap =
        targetTy == ptrTy_ && !isCollectionUnwrap &&
        expectedTag == static_cast<int64_t>(RyAnyTag::Str);

    std::string typeName;
    if (!targetTypeName.empty()) {
        typeName = targetTypeName;
    } else {
        typeName = (targetTy == i64Ty_) ? "int"
                 : (targetTy == i1Ty_)  ? "bool"
                 : (targetTy == ptrTy_) ? "str"
                                        : "unknown";
    }
    std::string mismatchMsg =
        "runtime error: any type mismatch (expected " + typeName + ")\n";

    RyAnyUnwrapDesc unwrapDesc{};
    unwrapDesc.kind = static_cast<int>(AnyUnwrapKind::Standard);
    unwrapDesc.any_val_id =
        ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(anyVal));
    unwrapDesc.any_ty = ry::llvm_emit::asRyType(anyTy_);
    unwrapDesc.target_ty = ry::llvm_emit::asRyType(targetTy);
    unwrapDesc.expected_tag = expectedTag;
    unwrapDesc.do_collection_retain = isCollectionUnwrap ? 1 : 0;
    unwrapDesc.do_str_retain = isStrUnwrap ? 1 : 0;
    unwrapDesc.mismatch_msg = mismatchMsg.c_str();
    unwrapDesc.mismatch_global_name = ".any_type_err";
    unwrapDesc.desc_mismatch_msg = "";
    unwrapDesc.desc_mismatch_global_name = "";
    return ry::llvm_emit::asLlvmValue(ry_emit_resolve(
        emit_ctx_, ry_emit_any_unwrap(emit_ctx_, &unwrapDesc)));
}

llvm::Value *CodeGen::tryUnwrapFromAny(llvm::Value *anyVal, llvm::Type *targetTy,
                                        const std::string &rawTargetTypeName,
                                        const std::string &callerLabel) {
    // Substitute generic type-parameter names so downstream dispatches see the
    // concrete type. Mirrors `unwrapFromAny`'s entry-point substitution.
    const std::string targetTypeName = substituteTypeParamsInName(rawTargetTypeName);
    llvm::StructType *resTy = getResultType(targetTy, errorTy_);

    // Construct an inline `Error{message, code}` value (code = 0 sentinel).
    auto buildInlineError = [&](const std::string &msg) -> llvm::Value * {
        llvm::Value *errStr = cachedGlobalString(msg);
        llvm::Value *errVal = llvm::UndefValue::get(errorTy_);
        errVal = builder_.CreateInsertValue(errVal, errStr, {0});
        errVal = builder_.CreateInsertValue(
            errVal, llvm::ConstantInt::get(i64Ty_, 0), {1});
        return errVal;
    };
    auto emitUnsupportedErr = [&](const std::string &kindLabel) -> llvm::Value * {
        std::string label = targetTypeName.empty() ? "?" : targetTypeName;
        std::string msg = callerLabel + "[" + label + "]: " + kindLabel +
                          " target not yet supported";
        return buildErrValue(buildInlineError(msg), resTy);
    };
    // Source-shape guard (#2315, #2378): every helper that walks `any`-held
    // collection storage as 16-byte RyAny stride (tryUnwrapListFromAny /
    // tryUnwrapMapFromAny / tryUnwrapRecordFromAny) assumes the source any
    // holds JSON-shape List<any> / Map<str, any> with 16-byte stride.
    // Native typed collections wrapped via `wrapInAny` register themselves
    // in the `__ry_any_register_typed_coll` side table; iterating them as
    // RyAny[] reads garbage at 16-byte stride out of a narrower buffer.
    //
    // Two-arm dispatch on the runtime side-table lookup:
    //   - source is registered (typed): if `expectedTypedName` matches the
    //     registered name, the data layout is already exactly what the
    //     target expects → passthrough with ARC retain on the box header
    //     (#2378 enables `asType[List<int>]` to recover a native `List<int>`
    //     roundtrip). If the names mismatch, return Err. If
    //     `expectedTypedName` is empty (record / Set<any>-only targets),
    //     no passthrough is possible and any typed source is Err.
    //   - source is unregistered (untyped, JSON-shape): delegate to
    //     `untypedPath()` which walks the 16-byte RyAny[].
    //
    // `load[T]` from json/json5 sources never registers a typed collection
    // (the parser always emits `List<any>` / `Map<str, any>`), so the typed
    // arm is dead code on the load path in practice — defense-in-depth.
    auto emitTypedCollGuard =
        [&](const llvm::Twine &nameStem,
            const std::string &kindLabel,
            const std::string &expectedTypedName,
            llvm::function_ref<llvm::Value *()> untypedPath) -> llvm::Value * {
        llvm::Function *fn = builder_.GetInsertBlock()->getParent();
        auto lookupFn = getRuntimeFn(
            "__ry_any_lookup_typed_coll", ptrTy_, {ptrTy_});
        llvm::Value *dataPtr =
            loadAnyDataPtr(anyVal, nameStem + ".typedchk");
        llvm::Value *typedName = builder_.CreateCall(
            lookupFn, {dataPtr}, nameStem + ".typed_name");
        llvm::Value *isTyped = builder_.CreateICmpNE(
            typedName,
            llvm::ConstantPointerNull::get(
                llvm::cast<llvm::PointerType>(ptrTy_)),
            nameStem + ".is_typed");

        auto *typedBB = createBBInFn("typedchk.typed", fn);
        auto *untypedBB = createBBInFn("typedchk.untyped", fn);
        auto *guardDoneBB = createBBInFn("typedchk.done", fn);
        emitBranchCond(isTyped, typedBB, untypedBB);

        builder_.SetInsertPoint(typedBB);
        const std::string label =
            targetTypeName.empty() ? "?" : targetTypeName;

        llvm::Value *typedOkResult = nullptr;
        llvm::BasicBlock *typedOkEndBB = nullptr;
        llvm::Value *typedErrResult;
        llvm::BasicBlock *typedErrEndBB;

        if (!expectedTypedName.empty()) {
            // Name-match check: the registered name string is compared
            // byte-for-byte against the canonical wrap-time name format
            // (matching `buildTypeNameFromMeta`'s output: `List<int>`,
            // `Map<str, int>` with a space after the comma, `Set<int>`).
            // Mismatch → Err; match → passthrough with ARC retain on the
            // collection header (the unwrapped value is a new alias to
            // the boxed collection, so the strong count must bump once).
            llvm::Value *expectedNameStr = cachedGlobalString(
                expectedTypedName, ".any.typed_coll.expected");
            auto strcmpFn = getStdlibStrcmp();
            llvm::Value *cmpResult = builder_.CreateCall(
                strcmpFn, {typedName, expectedNameStr},
                (nameStem + ".typed.cmp").str());
            llvm::Value *isMatch = builder_.CreateICmpEQ(
                cmpResult, llvm::ConstantInt::get(i32Ty_, 0),
                (nameStem + ".typed.match").str());

            auto *matchBB = createBBInFn("typedchk.typed.match", fn);
            auto *mismatchBB = createBBInFn("typedchk.typed.mismatch", fn);
            emitBranchCond(isMatch, matchBB, mismatchBB);

            builder_.SetInsertPoint(matchBB);
            auto *hdr = emitArcGetHeaderFromData(dataPtr);
            emitArcRetain(hdr);
            typedOkResult = buildOkValue(dataPtr, resTy);
            typedOkEndBB = builder_.GetInsertBlock();
            emitBranchUncond(guardDoneBB);

            builder_.SetInsertPoint(mismatchBB);
            std::string msg =
                callerLabel + "[" + label + "]: expected " +
                expectedTypedName +
                " but source is a different native typed collection";
            typedErrResult =
                buildErrValue(buildInlineError(msg), resTy);
            typedErrEndBB = builder_.GetInsertBlock();
            emitBranchUncond(guardDoneBB);
        } else {
            std::string msg =
                callerLabel + "[" + label + "]: cannot reconstruct " +
                kindLabel + " from a native typed collection source "
                "(only List<any> / Map<str, any> / Set<any> JSON-shape "
                "sources are supported)";
            typedErrResult =
                buildErrValue(buildInlineError(msg), resTy);
            typedErrEndBB = builder_.GetInsertBlock();
            emitBranchUncond(guardDoneBB);
        }

        builder_.SetInsertPoint(untypedBB);
        llvm::Value *untypedResult = untypedPath();
        llvm::BasicBlock *untypedEndBB = builder_.GetInsertBlock();
        emitBranchUncond(guardDoneBB);

        builder_.SetInsertPoint(guardDoneBB);
        unsigned numIncoming = typedOkResult ? 3u : 2u;
        llvm::PHINode *phi = builder_.CreatePHI(
            resTy, numIncoming, (nameStem + ".result").str());
        if (typedOkResult)
            phi->addIncoming(typedOkResult, typedOkEndBB);
        phi->addIncoming(typedErrResult, typedErrEndBB);
        phi->addIncoming(untypedResult, untypedEndBB);
        return phi;
    };

    // Descriptor-gated enum-like recovery (#2378). Used by Result, ADT enum,
    // and simple enum targets. Branches on the source `any` tag being Enum
    // and on the box's descriptor pointer equalling the expected descriptor
    // (which is looked up by the canonical wrap-time name). On both
    // matches, reuses `unwrapEnumFromAny` — its internal mismatch traps
    // are dead code on the proven-match path. Non-enum sources and
    // descriptor-mismatch sources return a prefixed Err.
    auto emitEnumLikeUnwrap =
        [&](llvm::Type *unwrapTargetTy,
            const std::string &canonicalName,
            const std::string &kindLabel) -> llvm::Value * {
        llvm::Function *fn = builder_.GetInsertBlock()->getParent();
        llvm::Value *entryTag = builder_.CreateExtractValue(
            anyVal, {0}, "tryenum.entry.tag");
        llvm::Value *isEnum = builder_.CreateICmpEQ(
            entryTag,
            llvm::ConstantInt::get(i64Ty_,
                                    static_cast<uint64_t>(RyAnyTag::Enum)),
            "tryenum.entry.is_enum");

        auto *enumPathBB = createBBInFn("tryenum.entry.enum_path", fn);
        auto *tagMismatchBB = createBBInFn("tryenum.entry.tag_err", fn);
        auto *enumDoneBB = createBBInFn("tryenum.entry.done", fn);
        emitBranchCond(isEnum, enumPathBB, tagMismatchBB);

        // Non-enum source: prefixed Err.
        builder_.SetInsertPoint(tagMismatchBB);
        const std::string label =
            targetTypeName.empty() ? "?" : targetTypeName;
        std::string tagMsg = callerLabel + "[" + label + "]: expected " +
                             kindLabel + " " + label +
                             ", got a non-enum source";
        llvm::Value *tagErrVal =
            buildErrValue(buildInlineError(tagMsg), resTy);
        llvm::BasicBlock *tagErrEndBB = builder_.GetInsertBlock();
        emitBranchUncond(enumDoneBB);

        // Enum tag: descriptor walk against the canonical name's global.
        builder_.SetInsertPoint(enumPathBB);
        auto *expectedDesc =
            getOrCreateEnumDescriptor(canonicalName, unwrapTargetTy);
        llvm::Value *enumDataPtr =
            loadAnyDataPtr(anyVal, "tryenum.entry");
        llvm::Value *actualDesc = builder_.CreateLoad(
            ptrTy_, enumDataPtr, "tryenum.entry.actual_desc");
        llvm::Value *descEq = builder_.CreateICmpEQ(
            actualDesc, expectedDesc, "tryenum.entry.desc_eq");

        auto *enumOkBB = createBBInFn("tryenum.entry.enum_ok", fn);
        auto *descMismatchBB =
            createBBInFn("tryenum.entry.desc_err", fn);
        emitBranchCond(descEq, enumOkBB, descMismatchBB);

        // Descriptor matches: reuse the panic-version unwrap. Its
        // tag-mismatch / descriptor-mismatch traps are unreachable
        // because we just proved both predicates.
        builder_.SetInsertPoint(enumOkBB);
        llvm::Value *payloadVal =
            unwrapEnumFromAny(anyVal, unwrapTargetTy, canonicalName);
        llvm::Value *okVal = buildOkValue(payloadVal, resTy);
        llvm::BasicBlock *enumOkEndBB = builder_.GetInsertBlock();
        emitBranchUncond(enumDoneBB);

        // Descriptor mismatch: prefixed Err.
        builder_.SetInsertPoint(descMismatchBB);
        std::string descMsg = callerLabel + "[" + label + "]: expected " +
                              label + ", got a different " + kindLabel +
                              " type";
        llvm::Value *descErrVal =
            buildErrValue(buildInlineError(descMsg), resTy);
        llvm::BasicBlock *descErrEndBB = builder_.GetInsertBlock();
        emitBranchUncond(enumDoneBB);

        builder_.SetInsertPoint(enumDoneBB);
        llvm::PHINode *enumPhi =
            builder_.CreatePHI(resTy, 3, "tryenum.entry.result");
        enumPhi->addIncoming(tagErrVal, tagErrEndBB);
        enumPhi->addIncoming(okVal, enumOkEndBB);
        enumPhi->addIncoming(descErrVal, descErrEndBB);
        return enumPhi;
    };

    // Sub-helper dispatch stays CodeGen-private per [[lowered_any]] Path 1
    // design: each helper uses `record_types_` / `reverse_option_types_` /
    // per-record / per-Map<str, V> reconstruction that depends on CodeGen
    // state not exposed across the boundary surface.
    if (auto *st = llvm::dyn_cast<llvm::StructType>(targetTy)) {
        // ADT enum: descriptor name is the bare enum type name (matches
        // the wrap-time name via `findEnumLikeTypeNameForBoxing`'s
        // `findAdtEnumName` branch).
        if (std::string adtName = findAdtEnumName(st); !adtName.empty()) {
            return emitEnumLikeUnwrap(st, adtName, "enum");
        }
        // Result<V, E>: build the canonical descriptor name with a
        // space after the comma so it matches the wrap-time name from
        // `findEnumLikeTypeNameForBoxing`'s Result arm. The parser-
        // emitted `targetTypeName` for `asType[Result<int, str>]` is
        // `Result<int,str>` (no space), so we must extract V and E and
        // reassemble. Fallback to `reverse_result_types_` if the parse
        // fails (e.g. type alias resolving to a Result struct).
        if (isResultType(st)) {
            std::string resolved = resolveTypeAlias(targetTypeName);
            std::string okName, errName;
            constexpr const char *kResPrefix = "Result<";
            constexpr size_t kResPrefixLen = 7;
            if (resolved.size() > kResPrefixLen + 1 &&
                resolved.compare(0, kResPrefixLen, kResPrefix) == 0 &&
                resolved.back() == '>') {
                std::string inside = resolved.substr(
                    kResPrefixLen,
                    resolved.size() - kResPrefixLen - 1);
                auto parts = splitTypeArgs(inside);
                if (parts.size() == 2) {
                    okName = ry::util::trimTypeNameSpaces(parts[0]);
                    errName = ry::util::trimTypeNameSpaces(parts[1]);
                }
            }
            if (okName.empty() || errName.empty()) {
                auto resIt = reverse_result_types_.find(st);
                if (resIt != reverse_result_types_.end()) {
                    if (okName.empty())
                        okName = reverseResolveTypeName(resIt->second.first);
                    if (errName.empty())
                        errName =
                            reverseResolveTypeName(resIt->second.second);
                }
            }
            if (okName.empty() || errName.empty()) {
                return emitUnsupportedErr("Result");
            }
            std::string canonicalResultName =
                "Result<" + okName + ", " + errName + ">";
            return emitEnumLikeUnwrap(st, canonicalResultName, "Result");
        }
        if (isOptionType(st)) {
            auto it = reverse_option_types_.find(st);
            if (it == reverse_option_types_.end())
                return emitUnsupportedErr("Option");
            llvm::Type *innerTy = it->second;
            // Recover the inner Ry type name from `targetTypeName` when it is
            // an `Option<...>` so the recursive call can route correctly
            // (record / typed list / typed map / nested Option).
            std::string innerName;
            std::string resolved = resolveTypeAlias(targetTypeName);
            constexpr const char *kPrefix = "Option<";
            constexpr size_t kPrefixLen = 7;
            if (resolved.size() > kPrefixLen + 1 &&
                resolved.compare(0, kPrefixLen, kPrefix) == 0 &&
                resolved.back() == '>') {
                innerName = ry::util::trimTypeNameSpaces(
                    resolved.substr(kPrefixLen,
                                    resolved.size() - kPrefixLen - 1));
            } else if (resolved.size() > 1 && resolved.back() == '?') {
                // `T?` shorthand for `Option<T>`.
                innerName = ry::util::trimTypeNameSpaces(
                    resolved.substr(0, resolved.size() - 1));
            }
            // Two-shape source dispatch (#2315) for Option<T>:
            //   tag == Enum + matching descriptor → reuse
            //       `unwrapEnumFromAny` (verified, so its descriptor-
            //       mismatch trap is dead code).
            //   otherwise → `tryUnwrapOptionFromAny` for the JSON-shape
            //       sources (Unit→None, primitive→Some via recurse).
            llvm::Function *fn = builder_.GetInsertBlock()->getParent();
            llvm::Value *entryTag = builder_.CreateExtractValue(
                anyVal, {0}, "tryopt.entry.tag");
            llvm::Value *isEnum = builder_.CreateICmpEQ(
                entryTag,
                llvm::ConstantInt::get(
                    i64Ty_, static_cast<uint64_t>(RyAnyTag::Enum)),
                "tryopt.entry.is_enum");

            auto *enumPathBB =
                createBBInFn("tryopt.entry.enum_path", fn);
            auto *fallbackBB =
                createBBInFn("tryopt.entry.fallback", fn);
            auto *optDoneBB = createBBInFn("tryopt.entry.done", fn);
            emitBranchCond(isEnum, enumPathBB, fallbackBB);

            // Fallback path: existing helper handles Unit / recurse cases.
            builder_.SetInsertPoint(fallbackBB);
            llvm::Value *fallbackResult = tryUnwrapOptionFromAny(
                anyVal, st, innerTy, innerName, targetTypeName, resTy,
                callerLabel);
            llvm::BasicBlock *fallbackEndBB = builder_.GetInsertBlock();
            emitBranchUncond(optDoneBB);

            // Enum path: load actual descriptor, compare against the
            // expected Option<inner> descriptor, then unwrap or Err.
            // Use the canonical `Option<inner>` name for descriptor
            // lookup so `T?` shorthand and type aliases (e.g. `Maybe`)
            // all hit the same global as the wrap site, which always
            // emits the canonical form. `targetTypeName` is preserved
            // for user-facing error messages.
            builder_.SetInsertPoint(enumPathBB);
            std::string canonicalOptName =
                innerName.empty() ? targetTypeName
                                  : "Option<" + innerName + ">";
            auto *expectedEnumDesc =
                getOrCreateEnumDescriptor(canonicalOptName, st);
            llvm::Value *enumDataPtr =
                loadAnyDataPtr(anyVal, "tryopt.entry");
            llvm::Value *actualEnumDesc = builder_.CreateLoad(
                ptrTy_, enumDataPtr, "tryopt.entry.actual_desc");
            llvm::Value *descEq = builder_.CreateICmpEQ(
                actualEnumDesc, expectedEnumDesc,
                "tryopt.entry.desc_eq");

            auto *enumOkBB =
                createBBInFn("tryopt.entry.enum_ok", fn);
            auto *enumErrBB =
                createBBInFn("tryopt.entry.enum_err", fn);
            emitBranchCond(descEq, enumOkBB, enumErrBB);

            // Descriptor matches: reuse the panic-version enum unwrap.
            // Its trap branches are dead because we proved the
            // descriptor matches. Pass the canonical name so the inner
            // descriptor lookup hits the same global.
            builder_.SetInsertPoint(enumOkBB);
            llvm::Value *optVal =
                unwrapEnumFromAny(anyVal, st, canonicalOptName);
            llvm::Value *optOk = buildOkValue(optVal, resTy);
            llvm::BasicBlock *enumOkEndBB = builder_.GetInsertBlock();
            emitBranchUncond(optDoneBB);

            // Descriptor mismatch: prefixed Err.
            builder_.SetInsertPoint(enumErrBB);
            std::string optTypeLabel =
                targetTypeName.empty() ? "?" : targetTypeName;
            std::string optDescMsg =
                callerLabel + "[" + optTypeLabel + "]: expected " +
                optTypeLabel + ", got a different enum type";
            llvm::Value *optDescErr =
                buildErrValue(buildInlineError(optDescMsg), resTy);
            llvm::BasicBlock *enumErrEndBB = builder_.GetInsertBlock();
            emitBranchUncond(optDoneBB);

            // Merge.
            builder_.SetInsertPoint(optDoneBB);
            llvm::PHINode *optPhi = builder_.CreatePHI(
                resTy, 3, "tryopt.entry.result");
            optPhi->addIncoming(fallbackResult, fallbackEndBB);
            optPhi->addIncoming(optOk, enumOkEndBB);
            optPhi->addIncoming(optDescErr, enumErrEndBB);
            return optPhi;
        }
        const RecordInfo *info = findRecordInfoForType(st);
        if (!info) {
            return emitUnsupportedErr("non-record struct");
        }
        // Two-shape source dispatch (#2315): the JSON `load[T]` path always
        // sees a Map<str, any>-tagged source, but `asType[T]` may receive an
        // any already holding a Record (native value). Branch on the runtime
        // tag so we can:
        //   tag == Record → descriptor walk + reuse `unwrapFromAny`. The
        //       internal mismatch-trap inside `unwrapFromAny` is dead code
        //       because we've already proven the descriptor matches.
        //   otherwise → fall back to the existing `tryUnwrapRecordFromAny`
        //       which handles Map-shaped sources and produces the
        //       "expected JSON object" Err on every other tag.
        llvm::Function *fn = builder_.GetInsertBlock()->getParent();
        llvm::Value *entryTag = builder_.CreateExtractValue(
            anyVal, {0}, "tryrec.entry.tag");
        llvm::Value *isRecord = builder_.CreateICmpEQ(
            entryTag,
            llvm::ConstantInt::get(i64Ty_,
                                   static_cast<uint64_t>(RyAnyTag::Record)),
            "tryrec.entry.is_rec");

        auto *recPathBB = createBBInFn("tryrec.entry.rec_path", fn);
        auto *mapPathBB = createBBInFn("tryrec.entry.map_path", fn);
        auto *doneBB = createBBInFn("tryrec.entry.done", fn);
        emitBranchCond(isRecord, recPathBB, mapPathBB);

        // Map / fallback path: existing helper covers Map reconstruction
        // and produces the "expected JSON object" Err on other tags.
        // Wrap in typed-coll guard so a native typed Map<str, X> source
        // (X ≠ any) returns Err instead of mis-iterating the value buffer.
        // No `expectedTypedName` — a record target never accepts a typed
        // collection source as a passthrough.
        builder_.SetInsertPoint(mapPathBB);
        llvm::Value *mapResult =
            emitTypedCollGuard("tryrec.entry.map", "record", "", [&] {
                return tryUnwrapRecordFromAny(anyVal, st, *info,
                                                targetTypeName, resTy,
                                                callerLabel);
            });
        llvm::BasicBlock *mapEndBB = builder_.GetInsertBlock();
        emitBranchUncond(doneBB);

        // Record path: descriptor walk + `unwrapFromAny` reuse.
        builder_.SetInsertPoint(recPathBB);
        std::string recTypeName = findRecordTypeName(st);
        if (recTypeName.empty())
            recTypeName = targetTypeName;
        auto *expectedDesc = getOrCreateRecordDescriptor(recTypeName, st);

        // Load actual descriptor from any.data[8] (treated as a ptr to the
        // record box data region, whose first field is the descriptor*).
        llvm::Value *dataPtr = loadAnyDataPtr(anyVal, "tryrec.entry");
        llvm::Value *actualDesc = builder_.CreateLoad(
            ptrTy_, dataPtr, "tryrec.entry.actual_desc");

        auto subtypeFn = getRuntimeFn(
            "__ry_record_is_subtype_desc", i64Ty_, {ptrTy_, ptrTy_});
        llvm::Value *subtypeRes = builder_.CreateCall(
            subtypeFn, {actualDesc, expectedDesc},
            "tryrec.entry.subtype");
        llvm::Value *isSubtype = builder_.CreateICmpNE(
            subtypeRes, llvm::ConstantInt::get(i64Ty_, 0),
            "tryrec.entry.is_subtype");

        auto *descOkBB = createBBInFn("tryrec.entry.desc_ok", fn);
        auto *descErrBB = createBBInFn("tryrec.entry.desc_err", fn);
        emitBranchCond(isSubtype, descOkBB, descErrBB);

        // Descriptor OK: reuse the panic-version unwrap. The internal
        // mismatch trap will never fire because we just proved subtype.
        builder_.SetInsertPoint(descOkBB);
        llvm::Value *recVal = unwrapFromAny(anyVal, targetTy, targetTypeName);
        llvm::Value *recOk = buildOkValue(recVal, resTy);
        llvm::BasicBlock *descOkEndBB = builder_.GetInsertBlock();
        emitBranchUncond(doneBB);

        // Descriptor mismatch: prefixed Err.
        builder_.SetInsertPoint(descErrBB);
        std::string recTypeLabel =
            targetTypeName.empty() ? "?" : targetTypeName;
        std::string descMsg = callerLabel + "[" + recTypeLabel +
                              "]: expected record " + recTypeLabel +
                              ", got a different record type";
        llvm::Value *descErrResult =
            buildErrValue(buildInlineError(descMsg), resTy);
        llvm::BasicBlock *descErrEndBB = builder_.GetInsertBlock();
        emitBranchUncond(doneBB);

        // Merge all three arms.
        builder_.SetInsertPoint(doneBB);
        llvm::PHINode *recPhi =
            builder_.CreatePHI(resTy, 3, "tryrec.entry.result");
        recPhi->addIncoming(mapResult, mapEndBB);
        recPhi->addIncoming(recOk, descOkEndBB);
        recPhi->addIncoming(descErrResult, descErrEndBB);
        return recPhi;
    }
    if (targetTy == i64Ty_ && !targetTypeName.empty() &&
        isSimpleEnumTypeName(targetTypeName)) {
        // Descriptor name for a simple enum is the bare enum type name
        // (matches `findEnumLikeTypeNameForBoxing`'s `enum_value_type`
        // branch). Reuse `emitEnumLikeUnwrap` for the descriptor-gated
        // recovery (#2378).
        std::string canonicalName = resolveTypeAlias(targetTypeName);
        return emitEnumLikeUnwrap(i64Ty_, canonicalName, "enum");
    }

    // Typed `List<T>` / `Map<str, V>` / `Set<T>` dispatch (#1852, #2378):
    // per-element recursive unwrap that walks the 16-byte RyAny stride
    // (`List<any>` / `Map<str, any>` JSON-shape sources), and native
    // typed-collection roundtrip via the typed-coll side table + ARC
    // passthrough (#2378). `Map<non-str, _>` is still rejected — there is
    // no wrap-side registration for non-str keys.
    if (targetTy == ptrTy_ && !targetTypeName.empty()) {
        std::string resolved = resolveTypeAlias(targetTypeName);
        if (ry::util::isListTypeName(resolved)) {
            std::string inner = ry::util::trimTypeNameSpaces(
                resolved.substr(5, resolved.size() - 6));
            if (inner == "any") {
                // Falls through to the standard pointer-tag dispatch below
                // (allocates a fresh header sharing the original data ptr,
                // see `isCollectionUnwrap` branch).
            } else {
                llvm::Type *elemTy = resolveType(inner);
                if (!elemTy)
                    return emitUnsupportedErr("typed List<" + inner + ">");
                // Canonical wrap-time name: `List<inner>` (no spaces).
                std::string expectedTypedName = "List<" + inner + ">";
                return emitTypedCollGuard(
                    "trylst.entry", "typed List<" + inner + ">",
                    expectedTypedName, [&] {
                        return tryUnwrapListFromAny(
                            anyVal, elemTy, inner, targetTypeName, resTy,
                            callerLabel);
                    });
            }
        } else if (ry::util::isSetTypeName(resolved)) {
            std::string inner = ry::util::trimTypeNameSpaces(
                resolved.substr(4, resolved.size() - 5));
            if (inner != "any") {
                // No JSON-shape Set<any> → typed Set<T> conversion path
                // (out of scope for #1852); typed-coll passthrough is the
                // only supported route. Untyped sources here are Err.
                std::string expectedTypedName = "Set<" + inner + ">";
                return emitTypedCollGuard(
                    "tryset.entry", "typed Set<" + inner + ">",
                    expectedTypedName, [&]() -> llvm::Value * {
                        std::string label = targetTypeName.empty()
                                                ? "?"
                                                : targetTypeName;
                        std::string msg = callerLabel + "[" + label +
                                          "]: typed Set<" + inner +
                                          "> source must be a native typed "
                                          "Set; no JSON-shape Set<any> "
                                          "conversion is supported";
                        return buildErrValue(buildInlineError(msg), resTy);
                    });
            }
        } else if (ry::util::isMapTypeName(resolved)) {
            std::string innerArgs = resolved.substr(4, resolved.size() - 5);
            auto parts = splitTypeArgs(innerArgs);
            if (parts.size() == 2) {
                std::string k = ry::util::trimTypeNameSpaces(parts[0]);
                std::string v = ry::util::trimTypeNameSpaces(parts[1]);
                if (k != "str") {
                    return emitUnsupportedErr(
                        "typed Map<" + k + ", " + v + ">");
                }
                if (v == "any") {
                    // Falls through to standard pointer-tag dispatch below.
                } else {
                    llvm::Type *valTy = resolveType(v);
                    if (!valTy)
                        return emitUnsupportedErr(
                            "typed Map<str, " + v + ">");
                    // Canonical wrap-time name: `Map<str, v>` with a space
                    // after the comma — matches `buildTypeNameFromMeta`.
                    std::string expectedTypedName =
                        "Map<str, " + v + ">";
                    return emitTypedCollGuard(
                        "trymap.entry", "typed Map<str, " + v + ">",
                        expectedTypedName, [&] {
                            return tryUnwrapMapFromAny(
                                anyVal, valTy, v, targetTypeName, resTy,
                                callerLabel);
                        });
                }
            }
        }
    }

    // Float target: accept both `Float` and `Int` tags (Int auto-promote).
    if (targetTy == f64Ty_) {
        std::string typeForMsg =
            targetTypeName.empty() ? "float" : targetTypeName;
        std::string msg =
            callerLabel + "[" + typeForMsg + "]: expected float or int";
        llvm::Value *errMsgStr = cachedGlobalString(msg);
        RyAnyTryUnwrapDesc tryUnwrapDesc{};
        tryUnwrapDesc.kind = static_cast<int>(AnyTryUnwrapKind::F64Promote);
        tryUnwrapDesc.any_val_id =
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(anyVal));
        tryUnwrapDesc.any_ty = ry::llvm_emit::asRyType(anyTy_);
        tryUnwrapDesc.res_ty = ry::llvm_emit::asRyType(resTy);
        tryUnwrapDesc.error_ty = ry::llvm_emit::asRyType(errorTy_);
        tryUnwrapDesc.target_ty = ry::llvm_emit::asRyType(f64Ty_);
        tryUnwrapDesc.err_msg_str_id =
            errMsgStr
                ? ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(errMsgStr))
                : 0;
        return ry::llvm_emit::asLlvmValue(ry_emit_resolve(
            emit_ctx_, ry_emit_any_try_unwrap(emit_ctx_, &tryUnwrapDesc)));
    }

    // Standard 2-way path: tag comparison → Ok(extracted) / Err(msg).
    // Handles int (i64), bool (i1), str (ptr+Str tag), and `List<any>` /
    // `Map<str,any>` / `Set<any>` (ptr+collection tag, requires ARC retain
    // on the unwrapped alias).
    int64_t expectedTag = getAnyTypeTag(targetTy);
    bool isCollectionUnwrap = false;
    if (targetTy == ptrTy_ && !targetTypeName.empty()) {
        std::string resolved = resolveTypeAlias(targetTypeName);
        if (ry::util::isListTypeName(resolved)) {
            expectedTag = static_cast<int64_t>(RyAnyTag::List);
            isCollectionUnwrap = true;
        } else if (ry::util::isMapTypeName(resolved)) {
            expectedTag = static_cast<int64_t>(RyAnyTag::Map);
            isCollectionUnwrap = true;
        } else if (ry::util::isSetTypeName(resolved)) {
            expectedTag = static_cast<int64_t>(RyAnyTag::Set);
            isCollectionUnwrap = true;
        }
    }
    bool isStrUnwrap = targetTy == ptrTy_ && !isCollectionUnwrap &&
                       expectedTag == static_cast<int64_t>(RyAnyTag::Str);

    std::string typeForMsg;
    if (!targetTypeName.empty()) {
        typeForMsg = targetTypeName;
    } else {
        typeForMsg = (targetTy == i64Ty_)   ? "int"
                     : (targetTy == i1Ty_)  ? "bool"
                     : (targetTy == ptrTy_) ? "str"
                                            : "unknown";
    }
    std::string msg = callerLabel + "[" + typeForMsg + "]: expected " + typeForMsg;
    llvm::Value *errMsgStr = cachedGlobalString(msg);

    RyAnyTryUnwrapDesc tryUnwrapDesc{};
    tryUnwrapDesc.kind = static_cast<int>(AnyTryUnwrapKind::Standard);
    tryUnwrapDesc.any_val_id =
        ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(anyVal));
    tryUnwrapDesc.any_ty = ry::llvm_emit::asRyType(anyTy_);
    tryUnwrapDesc.res_ty = ry::llvm_emit::asRyType(resTy);
    tryUnwrapDesc.error_ty = ry::llvm_emit::asRyType(errorTy_);
    tryUnwrapDesc.target_ty = ry::llvm_emit::asRyType(targetTy);
    tryUnwrapDesc.expected_tag = expectedTag;
    tryUnwrapDesc.do_collection_retain = isCollectionUnwrap ? 1 : 0;
    tryUnwrapDesc.do_str_retain = isStrUnwrap ? 1 : 0;
    tryUnwrapDesc.err_msg_str_id =
        errMsgStr
            ? ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(errMsgStr))
            : 0;
    return ry::llvm_emit::asLlvmValue(ry_emit_resolve(
        emit_ctx_, ry_emit_any_try_unwrap(emit_ctx_, &tryUnwrapDesc)));
}

llvm::Value *CodeGen::tryUnwrapRecordFromAny(llvm::Value *anyVal,
                                               llvm::StructType *recordStructTy,
                                               const RecordInfo &info,
                                               const std::string &targetTypeName,
                                               llvm::StructType *resTy,
                                               const std::string &callerLabel) {
    llvm::Function *fn = builder_.GetInsertBlock()->getParent();
    const std::string typeLabel = targetTypeName.empty() ? "?" : targetTypeName;
    const std::string prefix = callerLabel + "[" + typeLabel + "]: ";

    // Field-kind classification drives ARC release on every err exit. Records
    // are built field-by-field via InsertValue; if iter k fails, every
    // already-extracted ARC-bearing value in iter 0..k-1 must be released or
    // ASan flags a leak. Resources, weak refs, closures, fn-pointers, and
    // primitives are conservatively classified `None` (no release needed).
    enum class FieldKind { None, Str, Collection, RecordArc };
    auto classifyField = [&](llvm::Type *fieldLlvmTy,
                             const std::string &fieldTypeName) -> FieldKind {
        if (auto *fst = llvm::dyn_cast<llvm::StructType>(fieldLlvmTy)) {
            if (findRecordInfoForType(fst) && recordHasArcFields(fst))
                return FieldKind::RecordArc;
            return FieldKind::None;
        }
        if (fieldLlvmTy != ptrTy_) return FieldKind::None;
        std::string resolved = resolveTypeAlias(fieldTypeName);
        if (ry::util::isListTypeName(resolved) || ry::util::isMapTypeName(resolved) ||
            ry::util::isSetTypeName(resolved))
            return FieldKind::Collection;
        if (resolved == "str") return FieldKind::Str;
        return FieldKind::None;
    };

    auto buildInlineError = [&](const std::string &msg) -> llvm::Value * {
        llvm::Value *errStr = cachedGlobalString(msg);
        llvm::Value *errVal = llvm::UndefValue::get(errorTy_);
        errVal = builder_.CreateInsertValue(errVal, errStr, {0});
        errVal = builder_.CreateInsertValue(
            errVal, llvm::ConstantInt::get(i64Ty_, 0), {1});
        return errVal;
    };

    struct CollectedField {
        llvm::Value *val;
        FieldKind kind;
        std::string resolvedTypeName;  // for Collection: routes to right destructor
    };
    std::vector<CollectedField> arcCollected;

    auto releaseAllCollected = [&]() {
        for (const auto &p : arcCollected) {
            switch (p.kind) {
                case FieldKind::None: break;
                case FieldKind::Str:
                    emitArcReleaseLoadedElement(p.val, CollectionKind::Str,
                                                "str", "tryrec.rel.str");
                    break;
                case FieldKind::Collection: {
                    CollectionKind ck = CollectionKind::List;
                    if (ry::util::isMapTypeName(p.resolvedTypeName))
                        ck = CollectionKind::Map;
                    else if (ry::util::isSetTypeName(p.resolvedTypeName))
                        ck = CollectionKind::Set;
                    emitArcReleaseLoadedElement(p.val, ck, p.resolvedTypeName,
                                                "tryrec.rel.col");
                    break;
                }
                case FieldKind::RecordArc:
                    emitRecordArcFieldsRelease(
                        p.val, llvm::cast<llvm::StructType>(p.val->getType()));
                    break;
            }
        }
    };

    // Tag check (Map=6): JSON parser produces Map for objects; reject all else.
    llvm::Value *tag = builder_.CreateExtractValue(anyVal, 0, "tryrec.tag");
    llvm::Value *isMap = builder_.CreateICmpEQ(
        tag,
        llvm::ConstantInt::get(i64Ty_, static_cast<int64_t>(RyAnyTag::Map)),
        "tryrec.is_map");

    auto *tagOkBB  = createBBInFn("tryrec.tag_ok", fn);
    auto *tagErrBB = createBBInFn("tryrec.tag_err", fn);
    auto *doneBB   = createBBInFn("tryrec.done", fn);

    emitBranchCond(isMap, tagOkBB, tagErrBB);

    // Tag-mismatch arm: build Err and branch to done. No collected ARC yet.
    builder_.SetInsertPoint(tagErrBB);
    llvm::Value *tagErrVal = buildErrValue(
        buildInlineError(prefix + "expected JSON object"), resTy);
    llvm::BasicBlock *tagErrEndBB = builder_.GetInsertBlock();
    emitBranchUncond(doneBB);

    // Tag-ok: extract Map pointer from any.data[8].
    builder_.SetInsertPoint(tagOkBB);
    llvm::AllocaInst *anyTmp =
        builder_.CreateAlloca(anyTy_, nullptr, "tryrec.any.tmp");
    builder_.CreateStore(anyVal, anyTmp);
    llvm::Value *anyDataSlot =
        builder_.CreateStructGEP(anyTy_, anyTmp, 1, "tryrec.any.data.slot");
    llvm::Value *mapPtr =
        builder_.CreateLoad(ptrTy_, anyDataSlot, "tryrec.map.ptr");

    // Load vals* from MapHeader (index 3 — keys at 2, vals at 3).
    llvm::Value *valsSlot = builder_.CreateStructGEP(
        mapHeaderTy_, mapPtr, 3, "tryrec.vals.slot");
    llvm::Value *valsPtr =
        builder_.CreateLoad(ptrTy_, valsSlot, "tryrec.vals.ptr");

    // Accumulate err-exit incomings for the merge PHI.
    std::vector<std::pair<llvm::Value *, llvm::BasicBlock *>> errIncomings;
    errIncomings.emplace_back(tagErrVal, tagErrEndBB);

    llvm::Value *rec = llvm::UndefValue::get(recordStructTy);

    for (unsigned i = 0; i < info.fields.size(); ++i) {
        const FieldDef &f = info.fields[i];
        llvm::Type *fieldLlvmTy = recordStructTy->getElementType(i);
        std::string fieldTypeName =
            f.type ? f.type->toString() : std::string();
        std::string resolvedFieldTy = resolveTypeAlias(fieldTypeName);
        FieldKind fk = classifyField(fieldLlvmTy, fieldTypeName);

        // Step 1: key lookup via __ry_ht_find_str.
        llvm::Value *keyStr = cachedGlobalString(f.name);
        llvm::Value *slot = emitHashTableLookup(
            mapPtr, mapHeaderTy_, kMapLayout, keyStr, ptrTy_);
        llvm::Value *isMiss = builder_.CreateICmpEQ(
            slot, llvm::ConstantInt::getSigned(i64Ty_, -1),
            ("tryrec." + f.name + ".miss").c_str());

        auto *missBB  = createBBInFn(("tryrec.fld_" + f.name + ".miss").c_str(), fn);
        auto *foundBB = createBBInFn(("tryrec.fld_" + f.name + ".found").c_str(), fn);
        emitBranchCond(isMiss, missBB, foundBB);

        // Missing-field arm.
        builder_.SetInsertPoint(missBB);
        releaseAllCollected();
        llvm::Value *missErr = buildErrValue(
            buildInlineError(prefix + "field '" + f.name + "' missing"),
            resTy);
        llvm::BasicBlock *missEndBB = builder_.GetInsertBlock();
        emitBranchUncond(doneBB);
        errIncomings.emplace_back(missErr, missEndBB);

        // Found arm: load slot value (vals is RyAny[], stride = anyTy_).
        builder_.SetInsertPoint(foundBB);
        llvm::Value *elemPtr = builder_.CreateGEP(
            anyTy_, valsPtr, slot, "tryrec.fld_" + f.name + ".elem.ptr");
        llvm::Value *elemAny = builder_.CreateLoad(
            anyTy_, elemPtr, "tryrec.fld_" + f.name + ".elem");

        // Recursive unwrap.
        llvm::Value *subResult =
            tryUnwrapFromAny(elemAny, fieldLlvmTy, fieldTypeName, callerLabel);
        llvm::Value *subDisc = builder_.CreateExtractValue(
            subResult, {0}, "tryrec.fld_" + f.name + ".sub.disc");

        auto *subOkBB  = createBBInFn(("tryrec.fld_" + f.name + ".sub_ok").c_str(), fn);
        auto *subErrBB = createBBInFn(("tryrec.fld_" + f.name + ".sub_err").c_str(), fn);
        emitBranchCond(subDisc, subOkBB, subErrBB);

        // SubErr arm: release collected, build prefixed Err via runtime concat.
        builder_.SetInsertPoint(subErrBB);
        releaseAllCollected();
        llvm::Value *innerErr = builder_.CreateExtractValue(
            subResult, {2}, "tryrec.fld_" + f.name + ".sub.err");
        llvm::Value *innerMsg = builder_.CreateExtractValue(
            innerErr, {0}, "tryrec.fld_" + f.name + ".sub.err.msg");

        std::string fieldPrefix = prefix + "field '" + f.name + "': ";
        llvm::Value *prefixStr = cachedGlobalString(fieldPrefix);
        llvm::Value *prefixLen = emitStringByteLen(prefixStr);
        llvm::Value *innerMsgLen = emitStringByteLen(innerMsg);
        llvm::Value *totalLen = builder_.CreateAdd(
            prefixLen, innerMsgLen, "tryrec.fld_" + f.name + ".msg.total");
        auto makeUninitFn = getRuntimeFn("__ry_string_make_uninit", ptrTy_, {i64Ty_});
        llvm::Value *newBuf = builder_.CreateCall(
            makeUninitFn, {totalLen}, "tryrec.fld_" + f.name + ".msg.buf");
        builder_.CreateCall(getStdlibMemcpy(),
                            {newBuf, prefixStr, prefixLen});
        llvm::Value *dst = builder_.CreateGEP(
            i8Ty_, newBuf, prefixLen,
            "tryrec.fld_" + f.name + ".msg.dst");
        builder_.CreateCall(getStdlibMemcpy(), {dst, innerMsg, innerMsgLen});
        // Release the inner message. cachedGlobalString-backed messages are
        // ARC_IMMORTAL so emitArcRelease is a safe no-op; heap-allocated
        // nested messages (e.g. Outer → Middle → Inner missing field) get
        // their refcount decremented and freed via this path.
        emitArcRelease(emitStrGetHeaderFromData(innerMsg));

        llvm::Value *prefixedErr = llvm::UndefValue::get(errorTy_);
        prefixedErr = builder_.CreateInsertValue(prefixedErr, newBuf, {0});
        prefixedErr = builder_.CreateInsertValue(
            prefixedErr, llvm::ConstantInt::get(i64Ty_, 0), {1});
        llvm::Value *subErrVal = buildErrValue(prefixedErr, resTy);
        llvm::BasicBlock *subErrEndBB = builder_.GetInsertBlock();
        emitBranchUncond(doneBB);
        errIncomings.emplace_back(subErrVal, subErrEndBB);

        // SubOk arm: extract Ok value, InsertValue into rec, track for ARC.
        builder_.SetInsertPoint(subOkBB);
        llvm::Value *subOkVal = builder_.CreateExtractValue(
            subResult, {1}, "tryrec.fld_" + f.name + ".sub.ok");
        rec = builder_.CreateInsertValue(
            rec, subOkVal, {i},
            "tryrec.rec.f" + std::to_string(i));
        if (fk != FieldKind::None) {
            arcCollected.push_back({subOkVal, fk, resolvedFieldTy});
        }
    }

    // All fields succeeded. buildOkValue does not double-retain for record
    // StructType inputs (its `tryRetainArcSource` runs only for ptrTy_), and
    // we built rec from scratch so every field's refcount is owned by us
    // exactly once already.
    llvm::Value *okVal = buildOkValue(rec, resTy);
    llvm::BasicBlock *okEndBB = builder_.GetInsertBlock();
    emitBranchUncond(doneBB);

    // Merge.
    builder_.SetInsertPoint(doneBB);
    llvm::PHINode *phi = createPhi(resTy, {}, "tryrec.result");
    for (auto &p : errIncomings)
        phi->addIncoming(p.first, p.second);
    phi->addIncoming(okVal, okEndBB);
    return phi;
}

// Helper shared by tryUnwrapListFromAny / tryUnwrapMapFromAny: classify an
// element/value type for ARC release on partial failure. Mirrors
// `tryUnwrapRecordFromAny`'s classifyField but at the element granularity.
namespace {
enum class TryUnwrapElemKind { None, Str, Collection, RecordArc };
}

static TryUnwrapElemKind classifyTryUnwrapElem(
    ry::CodeGen &cg, llvm::Type *elemLlvmTy,
    const std::string &elemTypeName) {
    if (auto *fst = llvm::dyn_cast<llvm::StructType>(elemLlvmTy)) {
        if (cg.findRecordInfoForType(fst) && cg.recordHasArcFields(fst))
            return TryUnwrapElemKind::RecordArc;
        return TryUnwrapElemKind::None;
    }
    if (elemLlvmTy != cg.ptrTy_) return TryUnwrapElemKind::None;
    std::string resolved = cg.resolveTypeAlias(elemTypeName);
    if (ry::util::isListTypeName(resolved) || ry::util::isMapTypeName(resolved) ||
        ry::util::isSetTypeName(resolved))
        return TryUnwrapElemKind::Collection;
    if (resolved == "str") return TryUnwrapElemKind::Str;
    return TryUnwrapElemKind::None;
}

llvm::Value *CodeGen::tryUnwrapListFromAny(llvm::Value *anyVal,
                                             llvm::Type *elemTy,
                                             const std::string &elemTypeName,
                                             const std::string &targetTypeName,
                                             llvm::StructType *resTy,
                                             const std::string &callerLabel) {
    llvm::Function *fn = builder_.GetInsertBlock()->getParent();
    const std::string typeLabel =
        targetTypeName.empty() ? ("List<" + elemTypeName + ">") : targetTypeName;
    const std::string prefix = callerLabel + "[" + typeLabel + "]: ";

    auto buildInlineError = [&](const std::string &msg) -> llvm::Value * {
        llvm::Value *errStr = cachedGlobalString(msg);
        llvm::Value *errVal = llvm::UndefValue::get(errorTy_);
        errVal = builder_.CreateInsertValue(errVal, errStr, {0});
        errVal = builder_.CreateInsertValue(
            errVal, llvm::ConstantInt::get(i64Ty_, 0), {1});
        return errVal;
    };

    // Tag check (List=5).
    llvm::Value *tag = builder_.CreateExtractValue(anyVal, 0, "trylst.tag");
    llvm::Value *isList = builder_.CreateICmpEQ(
        tag,
        llvm::ConstantInt::get(i64Ty_, static_cast<int64_t>(RyAnyTag::List)),
        "trylst.is_list");

    auto *tagOkBB  = createBBInFn("trylst.tag_ok", fn);
    auto *tagErrBB = createBBInFn("trylst.tag_err", fn);
    auto *doneBB   = createBBInFn("trylst.done", fn);

    emitBranchCond(isList, tagOkBB, tagErrBB);

    builder_.SetInsertPoint(tagErrBB);
    llvm::Value *tagErrVal = buildErrValue(
        buildInlineError(prefix + "expected JSON array"), resTy);
    llvm::BasicBlock *tagErrEndBB = builder_.GetInsertBlock();
    emitBranchUncond(doneBB);

    // Tag-ok: load source list header from any.data[8].
    builder_.SetInsertPoint(tagOkBB);
    llvm::AllocaInst *anyTmp =
        builder_.CreateAlloca(anyTy_, nullptr, "trylst.any.tmp");
    builder_.CreateStore(anyVal, anyTmp);
    llvm::Value *anyDataSlot =
        builder_.CreateStructGEP(anyTy_, anyTmp, 1, "trylst.any.data.slot");
    llvm::Value *srcHdrPtr =
        builder_.CreateLoad(ptrTy_, anyDataSlot, "trylst.src.hdr");

    // Source: len at field 0, data ptr at field 2 (listHeaderTy_).
    llvm::Value *srcLenSlot =
        builder_.CreateStructGEP(listHeaderTy_, srcHdrPtr, 0, "trylst.src.len.slot");
    llvm::Value *srcLen =
        builder_.CreateLoad(i64Ty_, srcLenSlot, "trylst.src.len");
    llvm::Value *srcDataSlot =
        builder_.CreateStructGEP(listHeaderTy_, srcHdrPtr, 2, "trylst.src.data.slot");
    llvm::Value *srcDataPtr =
        builder_.CreateLoad(ptrTy_, srcDataSlot, "trylst.src.data");

    // Allocate fresh dest list header + data buffer.
    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t elemSize = dl.getTypeAllocSize(elemTy);
    llvm::Value *destHdr = emitArcAllocCollectionHeader(listHeaderTy_);
    auto mallocFn = getStdlibMalloc();
    llvm::Value *dataBytes = builder_.CreateMul(
        srcLen, llvm::ConstantInt::get(i64Ty_, elemSize), "trylst.dst.bytes");
    llvm::Value *destDataPtr =
        builder_.CreateCall(mallocFn, {dataBytes}, "trylst.dst.data");

    // Element kind for ARC release on partial failure / final stamp.
    TryUnwrapElemKind elemKind =
        classifyTryUnwrapElem(*this, elemTy, elemTypeName);
    std::string resolvedElemTy = resolveTypeAlias(elemTypeName);

    // Loop: i = 0; while (i < srcLen) ...
    llvm::AllocaInst *iSlot =
        builder_.CreateAlloca(i64Ty_, nullptr, "trylst.i.slot");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iSlot);
    // count_done tracks how many dest slots are successfully populated, so a
    // partial-failure cleanup can release exactly those (and no more).
    llvm::AllocaInst *countSlot =
        builder_.CreateAlloca(i64Ty_, nullptr, "trylst.count.slot");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), countSlot);

    auto *headerBB = createBBInFn("trylst.loop.header", fn);
    auto *bodyBB   = createBBInFn("trylst.loop.body", fn);
    auto *errBB    = createBBInFn("trylst.loop.err", fn);
    auto *exitBB   = createBBInFn("trylst.loop.exit", fn);

    emitBranchUncond(headerBB);
    builder_.SetInsertPoint(headerBB);
    llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iSlot, "trylst.i");
    llvm::Value *iLt = builder_.CreateICmpSLT(iCur, srcLen, "trylst.i.lt");
    emitBranchCond(iLt, bodyBB, exitBB);

    builder_.SetInsertPoint(bodyBB);
    // GEP source slot (any stride = 16B) and load any value.
    llvm::Value *srcElemPtr = builder_.CreateGEP(
        anyTy_, srcDataPtr, iCur, "trylst.src.elem.ptr");
    llvm::Value *srcAny =
        builder_.CreateLoad(anyTy_, srcElemPtr, "trylst.src.elem");

    // Recursive unwrap.
    llvm::Value *subResult =
        tryUnwrapFromAny(srcAny, elemTy, elemTypeName, callerLabel);
    llvm::Value *subDisc = builder_.CreateExtractValue(
        subResult, {0}, "trylst.sub.disc");
    auto *subOkBB = createBBInFn("trylst.sub.ok", fn);

    // On sub-Err, propagate to errBB carrying the inner Error value so we can
    // prefix its message before returning.
    auto *prevBlock = builder_.GetInsertBlock();
    emitBranchCond(subDisc, subOkBB, errBB);

    builder_.SetInsertPoint(subOkBB);
    llvm::Value *subOkVal = builder_.CreateExtractValue(
        subResult, {1}, "trylst.sub.ok.val");
    // Store into dest buffer at native stride.
    llvm::Value *destElemPtr = builder_.CreateGEP(
        elemTy, destDataPtr, iCur, "trylst.dst.elem.ptr");
    builder_.CreateStore(subOkVal, destElemPtr);
    // Bump count_done before incrementing i so cleanup walks exactly the
    // populated prefix.
    llvm::Value *newCount = builder_.CreateAdd(
        iCur, llvm::ConstantInt::get(i64Ty_, 1), "trylst.count.next");
    builder_.CreateStore(newCount, countSlot);
    llvm::Value *iNext = builder_.CreateAdd(
        iCur, llvm::ConstantInt::get(i64Ty_, 1), "trylst.i.next");
    builder_.CreateStore(iNext, iSlot);
    emitBranchUncond(headerBB);

    // Err arm: release all populated elements [0, count_done), free dest
    // buffers, build prefixed Err.
    builder_.SetInsertPoint(errBB);
    llvm::Value *innerErr = builder_.CreateExtractValue(
        subResult, {2}, "trylst.sub.err.val");
    llvm::Value *innerMsg = builder_.CreateExtractValue(
        innerErr, {0}, "trylst.sub.err.msg");
    (void)prevBlock;

    // Release loop: walk dest buffer [0, count_done) and release ARC fields.
    if (elemKind != TryUnwrapElemKind::None) {
        llvm::Value *populated = builder_.CreateLoad(i64Ty_, countSlot, "trylst.count");
        llvm::AllocaInst *jSlot =
            builder_.CreateAlloca(i64Ty_, nullptr, "trylst.rel.j.slot");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), jSlot);
        auto *relHdrBB  = createBBInFn("trylst.rel.header", fn);
        auto *relBodyBB = createBBInFn("trylst.rel.body", fn);
        auto *relExitBB = createBBInFn("trylst.rel.exit", fn);
        emitBranchUncond(relHdrBB);
        builder_.SetInsertPoint(relHdrBB);
        llvm::Value *jCur = builder_.CreateLoad(i64Ty_, jSlot, "trylst.rel.j");
        llvm::Value *jLt = builder_.CreateICmpSLT(jCur, populated, "trylst.rel.j.lt");
        emitBranchCond(jLt, relBodyBB, relExitBB);
        builder_.SetInsertPoint(relBodyBB);
        llvm::Value *relSlot = builder_.CreateGEP(
            elemTy, destDataPtr, jCur, "trylst.rel.slot");
        llvm::Value *relVal = builder_.CreateLoad(elemTy, relSlot, "trylst.rel.val");
        switch (elemKind) {
            case TryUnwrapElemKind::None: break;
            case TryUnwrapElemKind::Str:
                emitArcReleaseLoadedElement(relVal, CollectionKind::Str,
                                            "str", "trylst.rel.str");
                break;
            case TryUnwrapElemKind::Collection: {
                CollectionKind ck = CollectionKind::List;
                if (ry::util::isMapTypeName(resolvedElemTy)) ck = CollectionKind::Map;
                else if (ry::util::isSetTypeName(resolvedElemTy)) ck = CollectionKind::Set;
                emitArcReleaseLoadedElement(relVal, ck, resolvedElemTy,
                                            "trylst.rel.col");
                break;
            }
            case TryUnwrapElemKind::RecordArc:
                emitRecordArcFieldsRelease(
                    relVal, llvm::cast<llvm::StructType>(elemTy));
                break;
        }
        llvm::Value *jNext = builder_.CreateAdd(
            jCur, llvm::ConstantInt::get(i64Ty_, 1), "trylst.rel.j.next");
        builder_.CreateStore(jNext, jSlot);
        emitBranchUncond(relHdrBB);
        builder_.SetInsertPoint(relExitBB);
    }

    // Free dest header (ArcHeader 16B prefix; the helper drops the strong
    // refcount → 0 and frees both header and data buffer when we also free
    // the data buffer below). Since the dest list isn't yet a valid handle
    // (header fields uninitialized), call free directly on each buffer.
    auto freeFn = getStdlibFree();
    builder_.CreateCall(freeFn, {destDataPtr});
    // The dest header was obtained from `emitArcAllocCollectionHeader`, which
    // calls `emitArcAlloc` (malloc) + advances past ArcHeader. Free the raw
    // allocation by stepping back ARC_HEADER_SIZE.
    llvm::Value *destHdrRaw = builder_.CreateGEP(
        i8Ty_, destHdr,
        llvm::ConstantInt::getSigned(i64Ty_,
            -static_cast<int64_t>(ARC_HEADER_SIZE)),
        "trylst.dst.hdr.raw");
    builder_.CreateCall(freeFn, {destHdrRaw});

    // Build prefixed Err: `prefix + "element: " + innerMsg`. The failed
    // element index is omitted from the prefix — the inner message from the
    // recursive `tryUnwrapFromAny` carries enough context (e.g. "field 'age'
    // missing") and there is no runtime i64→str helper at this layer.
    std::string elemPrefix = prefix + "element: ";
    llvm::Value *prefixStr = cachedGlobalString(elemPrefix);
    llvm::Value *prefixLen = emitStringByteLen(prefixStr);
    llvm::Value *innerMsgLen = emitStringByteLen(innerMsg);
    llvm::Value *totalLen =
        builder_.CreateAdd(prefixLen, innerMsgLen, "trylst.tot");
    auto makeUninitFn = getRuntimeFn("__ry_string_make_uninit", ptrTy_, {i64Ty_});
    llvm::Value *newBuf =
        builder_.CreateCall(makeUninitFn, {totalLen}, "trylst.err.buf");
    auto memcpyFn = getStdlibMemcpy();
    builder_.CreateCall(memcpyFn, {newBuf, prefixStr, prefixLen});
    llvm::Value *dst = builder_.CreateGEP(
        i8Ty_, newBuf, prefixLen, "trylst.err.dst");
    builder_.CreateCall(memcpyFn, {dst, innerMsg, innerMsgLen});
    // Release innerMsg (owned by the failed sub-Result). cachedGlobalString
    // sources are ARC_IMMORTAL so the release is a safe no-op.
    emitArcRelease(emitStrGetHeaderFromData(innerMsg));

    llvm::Value *prefixedErr = llvm::UndefValue::get(errorTy_);
    prefixedErr = builder_.CreateInsertValue(prefixedErr, newBuf, {0});
    prefixedErr = builder_.CreateInsertValue(
        prefixedErr, llvm::ConstantInt::get(i64Ty_, 0), {1});
    llvm::Value *errResVal = buildErrValue(prefixedErr, resTy);
    llvm::BasicBlock *errEndBB = builder_.GetInsertBlock();
    emitBranchUncond(doneBB);

    // Exit arm: store header fields and Ok.
    builder_.SetInsertPoint(exitBB);
    storeListHeaderFields(destHdr, srcLen, srcLen, destDataPtr);
    setTypeMeta(TypeMeta::ListElem, destHdr, elemTy);
    if (!elemTypeName.empty() && elemTypeName != "int" &&
        elemTypeName != "float" && elemTypeName != "bool") {
        getOrCreateMeta(destHdr).list_elem_type_name = elemTypeName;
    }
    llvm::Value *okVal = buildOkValue(destHdr, resTy);
    llvm::BasicBlock *okEndBB = builder_.GetInsertBlock();
    emitBranchUncond(doneBB);

    // Merge.
    builder_.SetInsertPoint(doneBB);
    llvm::PHINode *phi = createPhi(resTy, {}, "trylst.result");
    phi->addIncoming(tagErrVal, tagErrEndBB);
    phi->addIncoming(errResVal, errEndBB);
    phi->addIncoming(okVal, okEndBB);
    return phi;
}

llvm::Value *CodeGen::tryUnwrapMapFromAny(llvm::Value *anyVal,
                                            llvm::Type *valTy,
                                            const std::string &valTypeName,
                                            const std::string &targetTypeName,
                                            llvm::StructType *resTy,
                                            const std::string &callerLabel) {
    llvm::Function *fn = builder_.GetInsertBlock()->getParent();
    const std::string typeLabel = targetTypeName.empty()
        ? ("Map<str, " + valTypeName + ">") : targetTypeName;
    const std::string prefix = callerLabel + "[" + typeLabel + "]: ";

    auto buildInlineError = [&](const std::string &msg) -> llvm::Value * {
        llvm::Value *errStr = cachedGlobalString(msg);
        llvm::Value *errVal = llvm::UndefValue::get(errorTy_);
        errVal = builder_.CreateInsertValue(errVal, errStr, {0});
        errVal = builder_.CreateInsertValue(
            errVal, llvm::ConstantInt::get(i64Ty_, 0), {1});
        return errVal;
    };

    // Tag check (Map=6).
    llvm::Value *tag = builder_.CreateExtractValue(anyVal, 0, "trymap.tag");
    llvm::Value *isMap = builder_.CreateICmpEQ(
        tag,
        llvm::ConstantInt::get(i64Ty_, static_cast<int64_t>(RyAnyTag::Map)),
        "trymap.is_map");

    auto *tagOkBB  = createBBInFn("trymap.tag_ok", fn);
    auto *tagErrBB = createBBInFn("trymap.tag_err", fn);
    auto *doneBB   = createBBInFn("trymap.done", fn);

    emitBranchCond(isMap, tagOkBB, tagErrBB);

    builder_.SetInsertPoint(tagErrBB);
    llvm::Value *tagErrVal = buildErrValue(
        buildInlineError(prefix + "expected JSON object"), resTy);
    llvm::BasicBlock *tagErrEndBB = builder_.GetInsertBlock();
    emitBranchUncond(doneBB);

    // Tag-ok: load source map header from any.data[8].
    builder_.SetInsertPoint(tagOkBB);
    llvm::AllocaInst *anyTmp =
        builder_.CreateAlloca(anyTy_, nullptr, "trymap.any.tmp");
    builder_.CreateStore(anyVal, anyTmp);
    llvm::Value *anyDataSlot =
        builder_.CreateStructGEP(anyTy_, anyTmp, 1, "trymap.any.data.slot");
    llvm::Value *srcHdrPtr =
        builder_.CreateLoad(ptrTy_, anyDataSlot, "trymap.src.hdr");

    // Source: len at field 0, keys at field 2, vals at field 3.
    llvm::Value *srcLenSlot = builder_.CreateStructGEP(
        mapHeaderTy_, srcHdrPtr, 0, "trymap.src.len.slot");
    llvm::Value *srcLen =
        builder_.CreateLoad(i64Ty_, srcLenSlot, "trymap.src.len");
    llvm::Value *srcKeysSlot = builder_.CreateStructGEP(
        mapHeaderTy_, srcHdrPtr, 2, "trymap.src.keys.slot");
    llvm::Value *srcKeysPtr =
        builder_.CreateLoad(ptrTy_, srcKeysSlot, "trymap.src.keys");
    llvm::Value *srcValsSlot = builder_.CreateStructGEP(
        mapHeaderTy_, srcHdrPtr, 3, "trymap.src.vals.slot");
    llvm::Value *srcValsPtr =
        builder_.CreateLoad(ptrTy_, srcValsSlot, "trymap.src.vals");

    // Allocate fresh dest header + keys/vals buffers.
    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t keyStride = dl.getTypeAllocSize(ptrTy_);
    uint64_t valStride = dl.getTypeAllocSize(valTy);
    llvm::Value *destHdr = emitArcAllocCollectionHeader(mapHeaderTy_);
    auto mallocFn = getStdlibMalloc();
    llvm::Value *keyBytes = builder_.CreateMul(
        srcLen, llvm::ConstantInt::get(i64Ty_, keyStride), "trymap.keys.bytes");
    llvm::Value *destKeysPtr =
        builder_.CreateCall(mallocFn, {keyBytes}, "trymap.dst.keys");
    llvm::Value *valBytes = builder_.CreateMul(
        srcLen, llvm::ConstantInt::get(i64Ty_, valStride), "trymap.vals.bytes");
    llvm::Value *destValsPtr =
        builder_.CreateCall(mallocFn, {valBytes}, "trymap.dst.vals");

    TryUnwrapElemKind valKind =
        classifyTryUnwrapElem(*this, valTy, valTypeName);
    std::string resolvedValTy = resolveTypeAlias(valTypeName);

    llvm::AllocaInst *iSlot =
        builder_.CreateAlloca(i64Ty_, nullptr, "trymap.i.slot");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iSlot);
    llvm::AllocaInst *countSlot =
        builder_.CreateAlloca(i64Ty_, nullptr, "trymap.count.slot");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), countSlot);

    auto *headerBB = createBBInFn("trymap.loop.header", fn);
    auto *bodyBB   = createBBInFn("trymap.loop.body", fn);
    auto *errBB    = createBBInFn("trymap.loop.err", fn);
    auto *exitBB   = createBBInFn("trymap.loop.exit", fn);

    emitBranchUncond(headerBB);
    builder_.SetInsertPoint(headerBB);
    llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iSlot, "trymap.i");
    llvm::Value *iLt = builder_.CreateICmpSLT(iCur, srcLen, "trymap.i.lt");
    emitBranchCond(iLt, bodyBB, exitBB);

    builder_.SetInsertPoint(bodyBB);
    // Load source key (str ptr, native stride 8B).
    llvm::Value *srcKeyPtr = builder_.CreateGEP(
        ptrTy_, srcKeysPtr, iCur, "trymap.src.key.ptr");
    llvm::Value *srcKey =
        builder_.CreateLoad(ptrTy_, srcKeyPtr, "trymap.src.key");
    // Retain the key (alias semantics: dest map shares the key with source).
    {
        llvm::Value *keyHdr = emitStrGetHeaderFromData(srcKey);
        emitArcRetain(keyHdr);
    }
    // GEP source value slot (any stride = 16B) and load any value.
    llvm::Value *srcValAnyPtr = builder_.CreateGEP(
        anyTy_, srcValsPtr, iCur, "trymap.src.val.ptr");
    llvm::Value *srcAny =
        builder_.CreateLoad(anyTy_, srcValAnyPtr, "trymap.src.val");

    llvm::Value *subResult =
        tryUnwrapFromAny(srcAny, valTy, valTypeName, callerLabel);
    llvm::Value *subDisc = builder_.CreateExtractValue(
        subResult, {0}, "trymap.sub.disc");
    auto *subOkBB = createBBInFn("trymap.sub.ok", fn);
    emitBranchCond(subDisc, subOkBB, errBB);

    builder_.SetInsertPoint(subOkBB);
    llvm::Value *subOkVal = builder_.CreateExtractValue(
        subResult, {1}, "trymap.sub.ok.val");
    // Store key (already retained) and value into dest buffers.
    llvm::Value *dstKeyPtr = builder_.CreateGEP(
        ptrTy_, destKeysPtr, iCur, "trymap.dst.key.ptr");
    builder_.CreateStore(srcKey, dstKeyPtr);
    llvm::Value *dstValPtr = builder_.CreateGEP(
        valTy, destValsPtr, iCur, "trymap.dst.val.ptr");
    builder_.CreateStore(subOkVal, dstValPtr);
    llvm::Value *newCount = builder_.CreateAdd(
        iCur, llvm::ConstantInt::get(i64Ty_, 1), "trymap.count.next");
    builder_.CreateStore(newCount, countSlot);
    llvm::Value *iNext = builder_.CreateAdd(
        iCur, llvm::ConstantInt::get(i64Ty_, 1), "trymap.i.next");
    builder_.CreateStore(iNext, iSlot);
    emitBranchUncond(headerBB);

    // Err arm.
    builder_.SetInsertPoint(errBB);
    llvm::Value *innerErr = builder_.CreateExtractValue(
        subResult, {2}, "trymap.sub.err.val");
    llvm::Value *innerMsg = builder_.CreateExtractValue(
        innerErr, {0}, "trymap.sub.err.msg");
    // The current iteration's key was retained but never stored; release it
    // (matched count_done does NOT include this iteration).
    {
        llvm::Value *keyHdr = emitStrGetHeaderFromData(srcKey);
        emitArcRelease(keyHdr);
    }

    // Release populated keys + vals.
    llvm::Value *populated = builder_.CreateLoad(i64Ty_, countSlot, "trymap.count");
    {
        llvm::AllocaInst *jSlot =
            builder_.CreateAlloca(i64Ty_, nullptr, "trymap.rel.j.slot");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), jSlot);
        auto *relHdrBB  = createBBInFn("trymap.rel.header", fn);
        auto *relBodyBB = createBBInFn("trymap.rel.body", fn);
        auto *relExitBB = createBBInFn("trymap.rel.exit", fn);
        emitBranchUncond(relHdrBB);
        builder_.SetInsertPoint(relHdrBB);
        llvm::Value *jCur = builder_.CreateLoad(i64Ty_, jSlot, "trymap.rel.j");
        llvm::Value *jLt = builder_.CreateICmpSLT(jCur, populated, "trymap.rel.j.lt");
        emitBranchCond(jLt, relBodyBB, relExitBB);
        builder_.SetInsertPoint(relBodyBB);
        // Release key.
        llvm::Value *relKeyPtr = builder_.CreateGEP(
            ptrTy_, destKeysPtr, jCur, "trymap.rel.key.ptr");
        llvm::Value *relKey =
            builder_.CreateLoad(ptrTy_, relKeyPtr, "trymap.rel.key");
        emitArcReleaseLoadedElement(relKey, CollectionKind::Str,
                                    "str", "trymap.rel.k");
        // Release val if ARC.
        if (valKind != TryUnwrapElemKind::None) {
            llvm::Value *relValPtr = builder_.CreateGEP(
                valTy, destValsPtr, jCur, "trymap.rel.val.ptr");
            llvm::Value *relVal =
                builder_.CreateLoad(valTy, relValPtr, "trymap.rel.val");
            switch (valKind) {
                case TryUnwrapElemKind::None: break;
                case TryUnwrapElemKind::Str:
                    emitArcReleaseLoadedElement(relVal, CollectionKind::Str,
                                                "str", "trymap.rel.v.str");
                    break;
                case TryUnwrapElemKind::Collection: {
                    CollectionKind ck = CollectionKind::List;
                    if (ry::util::isMapTypeName(resolvedValTy)) ck = CollectionKind::Map;
                    else if (ry::util::isSetTypeName(resolvedValTy)) ck = CollectionKind::Set;
                    emitArcReleaseLoadedElement(relVal, ck, resolvedValTy,
                                                "trymap.rel.v.col");
                    break;
                }
                case TryUnwrapElemKind::RecordArc:
                    emitRecordArcFieldsRelease(
                        relVal, llvm::cast<llvm::StructType>(valTy));
                    break;
            }
        }
        llvm::Value *jNext = builder_.CreateAdd(
            jCur, llvm::ConstantInt::get(i64Ty_, 1), "trymap.rel.j.next");
        builder_.CreateStore(jNext, jSlot);
        emitBranchUncond(relHdrBB);
        builder_.SetInsertPoint(relExitBB);
    }

    auto freeFn = getStdlibFree();
    builder_.CreateCall(freeFn, {destKeysPtr});
    builder_.CreateCall(freeFn, {destValsPtr});
    llvm::Value *destHdrRaw = builder_.CreateGEP(
        i8Ty_, destHdr,
        llvm::ConstantInt::getSigned(i64Ty_,
            -static_cast<int64_t>(ARC_HEADER_SIZE)),
        "trymap.dst.hdr.raw");
    builder_.CreateCall(freeFn, {destHdrRaw});

    // Build prefixed Err: `prefix + "value: " + innerMsg`. The failing key
    // is not included in the prefix — there is no runtime i64→str helper at
    // this layer, and the inner message carries enough context.
    std::string elemPrefix = prefix + "value: ";
    llvm::Value *prefixStr = cachedGlobalString(elemPrefix);
    llvm::Value *prefixLen = emitStringByteLen(prefixStr);
    llvm::Value *innerMsgLen = emitStringByteLen(innerMsg);
    llvm::Value *totalLen =
        builder_.CreateAdd(prefixLen, innerMsgLen, "trymap.tot");
    auto makeUninitFn = getRuntimeFn("__ry_string_make_uninit", ptrTy_, {i64Ty_});
    llvm::Value *newBuf =
        builder_.CreateCall(makeUninitFn, {totalLen}, "trymap.err.buf");
    auto memcpyFn = getStdlibMemcpy();
    builder_.CreateCall(memcpyFn, {newBuf, prefixStr, prefixLen});
    llvm::Value *dst = builder_.CreateGEP(
        i8Ty_, newBuf, prefixLen, "trymap.err.dst");
    builder_.CreateCall(memcpyFn, {dst, innerMsg, innerMsgLen});
    emitArcRelease(emitStrGetHeaderFromData(innerMsg));

    llvm::Value *prefixedErr = llvm::UndefValue::get(errorTy_);
    prefixedErr = builder_.CreateInsertValue(prefixedErr, newBuf, {0});
    prefixedErr = builder_.CreateInsertValue(
        prefixedErr, llvm::ConstantInt::get(i64Ty_, 0), {1});
    llvm::Value *errResVal = buildErrValue(prefixedErr, resTy);
    llvm::BasicBlock *errEndBB = builder_.GetInsertBlock();
    emitBranchUncond(doneBB);

    // Exit: rebuild hash index via __ry_ht_rehash_str, store fields, Ok.
    builder_.SetInsertPoint(exitBB);
    // Compute initBucketCount = max(8, smallest power-of-2 with 3*bc >= 4*len)
    // — replicate the literal pattern at runtime via a simple loop.
    // For simplicity we use 4*len as the initial bucket count rounded up to
    // the next power of two of at least 8. The runtime fn handles arbitrary
    // counts; we just need bc large enough to avoid immediate rehash on
    // first insert.
    llvm::Value *fourLen = builder_.CreateMul(
        srcLen, llvm::ConstantInt::get(i64Ty_, 4), "trymap.bc.4len");
    // bc starts at 8 and doubles while 3*bc < 4*len.
    llvm::AllocaInst *bcSlot =
        builder_.CreateAlloca(i64Ty_, nullptr, "trymap.bc.slot");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 8), bcSlot);
    auto *bcHdrBB  = createBBInFn("trymap.bc.header", fn);
    auto *bcBodyBB = createBBInFn("trymap.bc.body", fn);
    auto *bcExitBB = createBBInFn("trymap.bc.exit", fn);
    emitBranchUncond(bcHdrBB);
    builder_.SetInsertPoint(bcHdrBB);
    llvm::Value *bcCur = builder_.CreateLoad(i64Ty_, bcSlot, "trymap.bc.cur");
    llvm::Value *bcTimes3 = builder_.CreateMul(
        bcCur, llvm::ConstantInt::get(i64Ty_, 3), "trymap.bc.3x");
    llvm::Value *bcLt =
        builder_.CreateICmpSLT(bcTimes3, fourLen, "trymap.bc.lt");
    emitBranchCond(bcLt, bcBodyBB, bcExitBB);
    builder_.SetInsertPoint(bcBodyBB);
    llvm::Value *bcDoubled = builder_.CreateShl(
        bcCur, llvm::ConstantInt::get(i64Ty_, 1), "trymap.bc.x2");
    builder_.CreateStore(bcDoubled, bcSlot);
    emitBranchUncond(bcHdrBB);
    builder_.SetInsertPoint(bcExitBB);
    llvm::Value *bcFinal = builder_.CreateLoad(i64Ty_, bcSlot, "trymap.bc.final");

    auto rehashFn = getRuntimeFn("__ry_ht_rehash_str", ptrTy_,
                                  {ptrTy_, i64Ty_, i64Ty_});
    llvm::Value *buckets = builder_.CreateCall(rehashFn,
        {destKeysPtr, srcLen, bcFinal}, "trymap.buckets");

    storeMapHeaderFields(destHdr, srcLen, srcLen, destKeysPtr, destValsPtr);
    llvm::Value *bcPtr = builder_.CreateStructGEP(
        mapHeaderTy_, destHdr, 4, "trymap.dst.bc.ptr");
    builder_.CreateStore(bcFinal, bcPtr);
    llvm::Value *bpPtr = builder_.CreateStructGEP(
        mapHeaderTy_, destHdr, 5, "trymap.dst.bp.ptr");
    builder_.CreateStore(buckets, bpPtr);

    setTypeMeta(TypeMeta::MapKey, destHdr, ptrTy_);
    setTypeMeta(TypeMeta::MapValue, destHdr, valTy);
    getOrCreateMeta(destHdr).map_key_type_name = "str";
    if (!valTypeName.empty() && valTypeName != "int" &&
        valTypeName != "float" && valTypeName != "bool") {
        getOrCreateMeta(destHdr).map_value_type_name = valTypeName;
    }
    llvm::Value *okVal = buildOkValue(destHdr, resTy);
    llvm::BasicBlock *okEndBB = builder_.GetInsertBlock();
    emitBranchUncond(doneBB);

    builder_.SetInsertPoint(doneBB);
    llvm::PHINode *phi = createPhi(resTy, {}, "trymap.result");
    phi->addIncoming(tagErrVal, tagErrEndBB);
    phi->addIncoming(errResVal, errEndBB);
    phi->addIncoming(okVal, okEndBB);
    return phi;
}

llvm::Value *CodeGen::tryUnwrapOptionFromAny(llvm::Value *anyVal,
                                               llvm::StructType *optTy,
                                               llvm::Type *innerTy,
                                               const std::string &innerTypeName,
                                               const std::string &targetTypeName,
                                               llvm::StructType *resTy,
                                               const std::string &callerLabel) {
    llvm::Function *fn = builder_.GetInsertBlock()->getParent();
    const std::string typeLabel = targetTypeName.empty() ? "?" : targetTypeName;
    const std::string prefix = callerLabel + "[" + typeLabel + "]: ";

    // Three-way dispatch on the source `any` tag:
    //   Unit → Ok(None)
    //   anything else → recurse into innerTy via tryUnwrapFromAny
    //     inner Ok  → Ok(Some(_))
    //     inner Err → Err(prefixed message)
    llvm::Value *tag = builder_.CreateExtractValue(anyVal, 0, "tryopt.tag");
    llvm::Value *isUnit = builder_.CreateICmpEQ(
        tag,
        llvm::ConstantInt::get(i64Ty_, static_cast<int64_t>(RyAnyTag::Unit)),
        "tryopt.is_unit");

    auto *noneBB     = createBBInFn("tryopt.none", fn);
    auto *recurseBB  = createBBInFn("tryopt.recurse", fn);
    auto *innerOkBB  = createBBInFn("tryopt.inner_ok", fn);
    auto *innerErrBB = createBBInFn("tryopt.inner_err", fn);
    auto *doneBB     = createBBInFn("tryopt.done", fn);

    emitBranchCond(isUnit, noneBB, recurseBB);

    // None arm: build Ok(None).
    builder_.SetInsertPoint(noneBB);
    llvm::Value *noneVal = buildNoneValue(optTy);
    llvm::Value *noneOk = buildOkValue(noneVal, resTy);
    llvm::BasicBlock *noneEndBB = builder_.GetInsertBlock();
    emitBranchUncond(doneBB);

    // Recurse arm: hand the same `any` to the inner unwrap. Note the inner
    // call inspects the tag itself and emits its own type-mismatch errors
    // (e.g. "expected JSON object" for record innerTy when tag = Int).
    builder_.SetInsertPoint(recurseBB);
    llvm::Value *subResult =
        tryUnwrapFromAny(anyVal, innerTy, innerTypeName, callerLabel);
    llvm::Value *subDisc = builder_.CreateExtractValue(
        subResult, {0}, "tryopt.sub.disc");
    emitBranchCond(subDisc, innerOkBB, innerErrBB);

    // Inner Ok: extract value, propagate metadata (so str-prefixed metadata
    // from a Result<str, _>-inner doesn't get lost when wrapped in Some), then
    // wrap in Some(_) and Ok. buildSomeValue retains ptr inner once — we do
    // not double-retain because the recursive unwrap already owns one
    // refcount on the extracted value (see tryUnwrapListFromAny / record).
    builder_.SetInsertPoint(innerOkBB);
    llvm::Value *innerOkVal = builder_.CreateExtractValue(
        subResult, {1}, "tryopt.inner.ok");
    if (!innerTypeName.empty())
        propagateTypeMeta(innerTypeName, innerOkVal);
    llvm::Value *someVal = buildSomeValue(innerOkVal, optTy);
    llvm::Value *someOk = buildOkValue(someVal, resTy);
    llvm::BasicBlock *innerOkEndBB = builder_.GetInsertBlock();
    emitBranchUncond(doneBB);

    // Inner Err: prepend `load[Option<X>]: expected null or ` to the inner
    // message via __ry_string_make_uninit + memcpy (the same prefix-concat
    // pattern used by tryUnwrapRecordFromAny / tryUnwrapMapFromAny). The
    // resulting wording satisfies the test's "null" + "JSON object"
    // simultaneous toContain assertions for a primitive (int) source.
    builder_.SetInsertPoint(innerErrBB);
    llvm::Value *innerErr = builder_.CreateExtractValue(
        subResult, {2}, "tryopt.inner.err");
    llvm::Value *innerMsg = builder_.CreateExtractValue(
        innerErr, {0}, "tryopt.inner.err.msg");

    std::string errPrefix = prefix + "expected null or ";
    llvm::Value *prefixStr = cachedGlobalString(errPrefix);
    llvm::Value *prefixLen = emitStringByteLen(prefixStr);
    llvm::Value *innerMsgLen = emitStringByteLen(innerMsg);
    llvm::Value *totalLen = builder_.CreateAdd(
        prefixLen, innerMsgLen, "tryopt.err.total");
    auto makeUninitFn = getRuntimeFn("__ry_string_make_uninit", ptrTy_, {i64Ty_});
    llvm::Value *newBuf =
        builder_.CreateCall(makeUninitFn, {totalLen}, "tryopt.err.buf");
    auto memcpyFn = getStdlibMemcpy();
    builder_.CreateCall(memcpyFn, {newBuf, prefixStr, prefixLen});
    llvm::Value *dst = builder_.CreateGEP(
        i8Ty_, newBuf, prefixLen, "tryopt.err.dst");
    builder_.CreateCall(memcpyFn, {dst, innerMsg, innerMsgLen});
    // Release the inner message (no-op for cachedGlobalString-backed strings
    // since they are ARC_IMMORTAL; required for runtime-built nested messages).
    emitArcRelease(emitStrGetHeaderFromData(innerMsg));

    llvm::Value *prefixedErr = llvm::UndefValue::get(errorTy_);
    prefixedErr = builder_.CreateInsertValue(prefixedErr, newBuf, {0});
    prefixedErr = builder_.CreateInsertValue(
        prefixedErr, llvm::ConstantInt::get(i64Ty_, 0), {1});
    llvm::Value *errResVal = buildErrValue(prefixedErr, resTy);
    llvm::BasicBlock *errEndBB = builder_.GetInsertBlock();
    emitBranchUncond(doneBB);

    // Merge.
    builder_.SetInsertPoint(doneBB);
    llvm::PHINode *phi = createPhi(resTy, {}, "tryopt.result");
    phi->addIncoming(noneOk, noneEndBB);
    phi->addIncoming(someOk, innerOkEndBB);
    phi->addIncoming(errResVal, errEndBB);
    return phi;
}

llvm::Value *CodeGen::unwrapEnumFromAny(llvm::Value *anyVal, llvm::Type *targetTy,
                                          const std::string &targetTypeName) {
    // Enum unwrap path. Symmetric to wrapInAny's enum-box arm: the box
    // stores `[ArcHeader 16B][descriptor ptr 8B][payload]`. The data
    // pointer in `any.data[8]` is the data-region pointer (= headerPtr +
    // 16); descriptor identity at offset +0 is the runtime type check.
    // Cross-type unwrap is intentionally rejected — enums have no
    // inheritance, so descriptor pointer equality is the type identity.
    llvm::Function *fn = builder_.GetInsertBlock()->getParent();
    llvm::Value *tag = builder_.CreateExtractValue(anyVal, 0, "any.enum.tag.val");

    std::string typeName = targetTypeName;
    if (typeName.empty()) {
        // Recover the source-level enum name from the target LLVM type.
        if (auto *st = llvm::dyn_cast<llvm::StructType>(targetTy)) {
            typeName = findAdtEnumName(st);
            if (typeName.empty()) {
                auto optIt = reverse_option_types_.find(st);
                if (optIt != reverse_option_types_.end()) {
                    std::string inner = reverseResolveTypeName(optIt->second);
                    if (!inner.empty())
                        typeName = "Option<" + inner + ">";
                }
            }
            if (typeName.empty()) {
                auto resIt = reverse_result_types_.find(st);
                if (resIt != reverse_result_types_.end()) {
                    std::string okName = reverseResolveTypeName(resIt->second.first);
                    std::string errName = reverseResolveTypeName(resIt->second.second);
                    if (!okName.empty() && !errName.empty())
                        typeName = "Result<" + okName + ", " + errName + ">";
                }
            }
        }
    }
    if (typeName.empty())
        codegenError("type error: 'any' enum unwrap could not resolve "
                     "source-level type name");

    auto *expectedDesc = getOrCreateEnumDescriptor(typeName, targetTy);
    auto *layoutTy = enumBoxLayoutType(targetTy);

    auto *tagMatchBB = createBBInFn("any.enum.tag_ok", fn);
    auto *tagMismatchBB = createBBInFn("any.enum.tag_err", fn);
    auto *descMatchBB = createBBInFn("any.enum.desc_ok", fn);
    auto *descMismatchBB = createBBInFn("any.enum.desc_err", fn);

    llvm::Value *isEnum = builder_.CreateICmpEQ(
        tag, llvm::ConstantInt::get(
                 i64Ty_, static_cast<uint64_t>(RyAnyTag::Enum)),
        "any.is_enum");
    emitBranchCond(isEnum, tagMatchBB, tagMismatchBB);

    builder_.SetInsertPoint(tagMismatchBB);
    emitRuntimeError("runtime error: any type mismatch (expected " + typeName +
                         ", got non-enum)\n",
                     ".any_enum_type_err");

    builder_.SetInsertPoint(tagMatchBB);
    llvm::AllocaInst *tmp = builder_.CreateAlloca(anyTy_, nullptr, "any.enum.tmp");
    builder_.CreateStore(anyVal, tmp);
    auto *anyDataSlot = builder_.CreateStructGEP(anyTy_, tmp, 1, "any.enum.data.ptr");
    auto *dataPtr = builder_.CreateLoad(ptrTy_, anyDataSlot, "any.enum.data");
    auto *descSlot = builder_.CreateStructGEP(layoutTy, dataPtr, 0,
                                              "any.enum.desc.slot");
    auto *actualDesc = builder_.CreateLoad(ptrTy_, descSlot, "any.enum.desc");
    llvm::Value *descEq = builder_.CreateICmpEQ(actualDesc, expectedDesc,
                                                "any.enum.desc.eq");
    emitBranchCond(descEq, descMatchBB, descMismatchBB);

    builder_.SetInsertPoint(descMismatchBB);
    emitRuntimeError("runtime error: any enum type mismatch (expected " +
                         typeName + ", got a different enum type)\n",
                     ".any_enum_desc_err");

    builder_.SetInsertPoint(descMatchBB);
    auto *payloadSlot = builder_.CreateStructGEP(layoutTy, dataPtr, 1,
                                                 "any.enum.payload.slot");
    llvm::Value *payloadVal = builder_.CreateLoad(targetTy, payloadSlot,
                                                  "any.enum.payload");
    // Field-wise retain on ARC payload fields — the unwrapped enum becomes a
    // new alias to the boxed value, so any inner str / List / Map / Set must
    // be retained independently so both the box dtor (when the box is later
    // released) and the unwrapped enum's owner can release safely.
    emitEnumBoxArcFieldsRetain(payloadVal, typeName, targetTy);
    return payloadVal;
}

void CodeGen::registerAnyManagedVar(llvm::AllocaInst *alloca,
                                     const std::string &sourceTypeName) {
    if (!alloca || alloca->getAllocatedType() != anyTy_) return;
    auto it = arc_any_managed_vars_.find(alloca);
    if (it == arc_any_managed_vars_.end()) {
        arc_any_managed_vars_.emplace(alloca, sourceTypeName);
    } else if (it->second.empty() && !sourceTypeName.empty()) {
        // Upgrade from "unknown source" to a concrete type name when later
        // declarations supply it.
        it->second = sourceTypeName;
    }
}

void CodeGen::emitAnyReleaseVar(const std::string &name,
                                 llvm::AllocaInst *alloca,
                                 const std::string &sourceTypeName) {
    if (!alloca || alloca->getAllocatedType() != anyTy_) return;
    auto *parentFn = builder_.GetInsertBlock()->getParent();

    auto *loaded = builder_.CreateLoad(anyTy_, alloca, name + ".any.load");
    auto *tagVal = builder_.CreateExtractValue(loaded, 0, name + ".any.tag");
    auto *dataPtr = builder_.CreateStructGEP(anyTy_, alloca, 1, name + ".any.data");

    auto *strBB  = createBBInFn((name + ".any.rel.str").c_str(), parentFn);
    auto *listBB = createBBInFn((name + ".any.rel.list").c_str(), parentFn);
    auto *mapBB  = createBBInFn((name + ".any.rel.map").c_str(), parentFn);
    auto *setBB  = createBBInFn((name + ".any.rel.set").c_str(), parentFn);
    auto *recBB  = createBBInFn((name + ".any.rel.rec").c_str(), parentFn);
    auto *enumBB = createBBInFn((name + ".any.rel.enum").c_str(), parentFn);
    auto *doneBB = createBBInFn((name + ".any.rel.done").c_str(), parentFn);

    auto *sw = builder_.CreateSwitch(tagVal, doneBB, 6);
    auto *intTy = llvm::cast<llvm::IntegerType>(i64Ty_);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Str)),    strBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::List)),   listBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Map)),    mapBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Set)),    setBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Record)), recBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Enum)),   enumBB);

    // str payload release: load the handle, get the StringHeader (offset -24
    // via emitStrGetHeaderFromData — NOT emitArcGetHeaderFromData which would
    // read the wrong word), and emitArcRelease without a destructor (str has
    // no inner allocations). Mirrors emitArcReleaseVar's str path.
    builder_.SetInsertPoint(strBB);
    auto *strPtr = builder_.CreateLoad(ptrTy_, dataPtr, name + ".any.str.ptr");
    auto *strHdr = emitStrGetHeaderFromData(strPtr);
    emitArcRelease(strHdr, isArcAtomic(strPtr), llvm::FunctionCallee{}, nullptr);
    emitBranchUncond(doneBB);

    auto releaseSlot = [&](llvm::BasicBlock *bb, CollectionKind kind,
                           const std::string &fallbackTypeName) {
        builder_.SetInsertPoint(bb);
        auto *ptr = builder_.CreateLoad(ptrTy_, dataPtr, name + ".any.ptr");
        // Element-type metadata is erased once the value enters `any`; use the
        // declared source type from registration when available, else fall
        // back to the generic destructor for that kind. Only use the source
        // name when its collection kind matches the runtime tag — an `any`
        // alloca may be reassigned to a different collection kind (e.g.
        // declared with a List value then later assigned a Map), and using
        // the stale source name would route through the wrong destructor.
        std::string useTypeName = sourceTypeName;
        bool sourceMatchesKind = false;
        if (!useTypeName.empty()) {
            std::string canon = resolveTypeAlias(useTypeName);
            switch (kind) {
                case CollectionKind::List:
                    sourceMatchesKind = ry::util::isListTypeName(canon);
                    break;
                case CollectionKind::Map:
                    sourceMatchesKind = ry::util::isMapTypeName(canon);
                    break;
                case CollectionKind::Set:
                    sourceMatchesKind = ry::util::isSetTypeName(canon);
                    break;
                case CollectionKind::Str:
                    sourceMatchesKind = false;
                    break;
            }
        }
        if (!sourceMatchesKind) {
            useTypeName = fallbackTypeName;
        }
        emitArcReleaseLoadedElement(ptr, kind, useTypeName,
                                     name + ".any." + fallbackTypeName);
        // emitArcReleaseLoadedElement leaves builder_ on its own continuation
        // block; branch from there to the merge.
        emitBranchUncond(doneBB);
    };

    releaseSlot(listBB, CollectionKind::List, "List");
    releaseSlot(mapBB,  CollectionKind::Map,  "Map");
    releaseSlot(setBB,  CollectionKind::Set,  "Set");

    // Record release: the box stores `[ArcHeader | descriptor ptr | record_struct]`,
    // and the data slot holds the data-region pointer (= headerPtr + 16). Recover
    // the ArcHeader via the standard `-16` offset and dispatch through the
    // runtime trampoline `__ry_arc_dtor_record_dispatch`, which loads the
    // descriptor and calls the per-type dtor. Sole indirection: `emitArcRelease`
    // binds a compile-time FunctionCallee, so we cannot pass the per-type LLVM
    // dtor directly — the descriptor mediation lets every record box share one
    // ARC release call site.
    builder_.SetInsertPoint(recBB);
    auto *recPtr = builder_.CreateLoad(ptrTy_, dataPtr, name + ".any.rec.ptr");
    auto *recHdr = emitArcGetHeaderFromData(recPtr);
    auto trampoline = getRuntimeFn("__ry_arc_dtor_record_dispatch",
                                   builder_.getVoidTy(), {ptrTy_});
    emitArcRelease(recHdr, isArcAtomic(recPtr), trampoline, nullptr);
    emitBranchUncond(doneBB);

    // Enum release path — symmetric to Record. The box layout
    // `[ArcHeader 16B | descriptor ptr 8B | payload]` matches Record's
    // (only the descriptor type differs: enum is 3 ptrs vs record's 4).
    // The data slot holds the data-region pointer (= headerPtr + 16);
    // recover the ArcHeader via the standard `-16` offset and dispatch
    // through `__ry_arc_dtor_enum_dispatch`, which loads the descriptor
    // and calls the per-type enum dtor (no-op for simple enums, ARC
    // field release for ADT / Option / Result with ARC payload).
    builder_.SetInsertPoint(enumBB);
    auto *enumPtr = builder_.CreateLoad(ptrTy_, dataPtr, name + ".any.enum.ptr");
    auto *enumHdr = emitArcGetHeaderFromData(enumPtr);
    auto enumTrampoline = getRuntimeFn("__ry_arc_dtor_enum_dispatch",
                                       builder_.getVoidTy(), {ptrTy_});
    emitArcRelease(enumHdr, isArcAtomic(enumPtr), enumTrampoline, nullptr);
    emitBranchUncond(doneBB);

    builder_.SetInsertPoint(doneBB);
}

void CodeGen::emitAnyRetainPayload(llvm::Value *anyVal,
                                    const std::string &siteLabel) {
    if (!anyVal || anyVal->getType() != anyTy_) return;
    auto *parentFn = builder_.GetInsertBlock()->getParent();

    // Stash the value into a fresh alloca so we can re-GEP the data field as
    // a ptr regardless of the original tag.
    llvm::AllocaInst *tmp = builder_.CreateAlloca(anyTy_, nullptr,
                                                  siteLabel + ".any.retain.tmp");
    builder_.CreateStore(anyVal, tmp);
    auto *tagVal = builder_.CreateExtractValue(anyVal, 0,
                                                siteLabel + ".any.retain.tag");
    auto *dataPtr = builder_.CreateStructGEP(anyTy_, tmp, 1,
                                              siteLabel + ".any.retain.data");

    auto *strBB  = createBBInFn((siteLabel + ".any.retain.str").c_str(), parentFn);
    auto *collBB = createBBInFn((siteLabel + ".any.retain.coll").c_str(), parentFn);
    auto *doneBB = createBBInFn((siteLabel + ".any.retain.done").c_str(), parentFn);

    auto *sw = builder_.CreateSwitch(tagVal, doneBB, 6);
    auto *intTy = llvm::cast<llvm::IntegerType>(i64Ty_);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Str)),
                strBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::List)),
                collBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Map)),
                collBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Set)),
                collBB);
    // Record and Enum boxes use the same `-16` ARC header offset as
    // collections, so a single retain block covers all five (List / Map /
    // Set / Record / Enum). The retain only increments the box's strong
    // count — inner ARC field retains happen at unwrap time, not here.
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Record)),
                collBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Enum)),
                collBB);

    // str retain uses the StringHeader offset (-24) via
    // emitStrGetHeaderFromData — distinct from collection retain which uses
    // the ARC header offset (-16) via emitArcGetHeaderFromData. Confusing the
    // two corrupts the StringHeader / ArcHeader words.
    builder_.SetInsertPoint(strBB);
    auto *strPtrR = builder_.CreateLoad(ptrTy_, dataPtr,
                                         siteLabel + ".any.retain.str.ptr");
    auto *strHdrR = emitStrGetHeaderFromData(strPtrR);
    emitArcRetain(strHdrR);
    emitBranchUncond(doneBB);

    // All three collection tags route to the same retain block — the inner
    // header layout is identical (ARC_HEADER_SIZE prefix) for List / Map /
    // Set, so a single `emitArcGetHeaderFromData` + `emitArcRetain` covers
    // every kind.
    builder_.SetInsertPoint(collBB);
    auto *ptr = builder_.CreateLoad(ptrTy_, dataPtr, siteLabel + ".any.retain.ptr");
    auto *hdr = emitArcGetHeaderFromData(ptr);
    emitArcRetain(hdr);
    emitBranchUncond(doneBB);

    builder_.SetInsertPoint(doneBB);
}

void CodeGen::emitAnyReleasePayload(llvm::Value *anyVal,
                                     const std::string &sourceTypeName,
                                     const std::string &siteLabel) {
    if (!anyVal || anyVal->getType() != anyTy_) return;
    auto *parentFn = builder_.GetInsertBlock()->getParent();

    llvm::AllocaInst *tmp = builder_.CreateAlloca(anyTy_, nullptr,
                                                   siteLabel + ".any.rel.tmp");
    builder_.CreateStore(anyVal, tmp);
    auto *tagVal = builder_.CreateExtractValue(anyVal, 0,
                                                siteLabel + ".any.rel.tag");
    auto *dataPtr = builder_.CreateStructGEP(anyTy_, tmp, 1,
                                              siteLabel + ".any.rel.data");

    auto *strBB  = createBBInFn((siteLabel + ".any.rel.str").c_str(), parentFn);
    auto *listBB = createBBInFn((siteLabel + ".any.rel.list").c_str(), parentFn);
    auto *mapBB  = createBBInFn((siteLabel + ".any.rel.map").c_str(), parentFn);
    auto *setBB  = createBBInFn((siteLabel + ".any.rel.set").c_str(), parentFn);
    auto *recBB  = createBBInFn((siteLabel + ".any.rel.rec").c_str(), parentFn);
    auto *enumBB = createBBInFn((siteLabel + ".any.rel.enum").c_str(), parentFn);
    auto *doneBB = createBBInFn((siteLabel + ".any.rel.done").c_str(), parentFn);

    auto *sw = builder_.CreateSwitch(tagVal, doneBB, 6);
    auto *intTy = llvm::cast<llvm::IntegerType>(i64Ty_);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Str)),    strBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::List)),   listBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Map)),    mapBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Set)),    setBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Record)), recBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Enum)),   enumBB);

    // str payload release: StringHeader offset (-24), no inner destructor.
    // Mirrors emitArcReleaseVar's str path (codegen_arc.cpp:427-441).
    builder_.SetInsertPoint(strBB);
    auto *strPtrRel = builder_.CreateLoad(ptrTy_, dataPtr,
                                           siteLabel + ".any.rel.str.ptr");
    auto *strHdrRel = emitStrGetHeaderFromData(strPtrRel);
    emitArcRelease(strHdrRel, isArcAtomic(strPtrRel), llvm::FunctionCallee{}, nullptr);
    emitBranchUncond(doneBB);

    auto releaseSlot = [&](llvm::BasicBlock *bb, CollectionKind kind,
                           const std::string &fallbackTypeName) {
        builder_.SetInsertPoint(bb);
        auto *ptr = builder_.CreateLoad(ptrTy_, dataPtr,
                                         siteLabel + ".any.rel.ptr");
        // Mirror emitAnyReleaseVar: only use the supplied source name when
        // its kind matches the runtime tag, else fall back to the generic
        // destructor for that kind. This guards against stale names from
        // reassigned `any` slots.
        std::string useTypeName = sourceTypeName;
        bool sourceMatchesKind = false;
        if (!useTypeName.empty()) {
            std::string canon = resolveTypeAlias(useTypeName);
            switch (kind) {
                case CollectionKind::List:
                    sourceMatchesKind = ry::util::isListTypeName(canon);
                    break;
                case CollectionKind::Map:
                    sourceMatchesKind = ry::util::isMapTypeName(canon);
                    break;
                case CollectionKind::Set:
                    sourceMatchesKind = ry::util::isSetTypeName(canon);
                    break;
                case CollectionKind::Str:
                    sourceMatchesKind = false;
                    break;
            }
        }
        if (!sourceMatchesKind) {
            useTypeName = fallbackTypeName;
        }
        emitArcReleaseLoadedElement(ptr, kind, useTypeName,
                                     siteLabel + ".any." + fallbackTypeName);
        emitBranchUncond(doneBB);
    };

    releaseSlot(listBB, CollectionKind::List, "List");
    releaseSlot(mapBB,  CollectionKind::Map,  "Map");
    releaseSlot(setBB,  CollectionKind::Set,  "Set");

    // Record release path — see emitAnyReleaseVar for the same reasoning.
    builder_.SetInsertPoint(recBB);
    auto *recPtr = builder_.CreateLoad(ptrTy_, dataPtr, siteLabel + ".any.rec.ptr");
    auto *recHdr = emitArcGetHeaderFromData(recPtr);
    auto trampoline = getRuntimeFn("__ry_arc_dtor_record_dispatch",
                                   builder_.getVoidTy(), {ptrTy_});
    emitArcRelease(recHdr, isArcAtomic(recPtr), trampoline, nullptr);
    emitBranchUncond(doneBB);

    // Enum release path — symmetric to Record.
    builder_.SetInsertPoint(enumBB);
    auto *enumPtr = builder_.CreateLoad(ptrTy_, dataPtr, siteLabel + ".any.enum.ptr");
    auto *enumHdr = emitArcGetHeaderFromData(enumPtr);
    auto enumTrampoline = getRuntimeFn("__ry_arc_dtor_enum_dispatch",
                                       builder_.getVoidTy(), {ptrTy_});
    emitArcRelease(enumHdr, isArcAtomic(enumPtr), enumTrampoline, nullptr);
    emitBranchUncond(doneBB);

    builder_.SetInsertPoint(doneBB);
}

llvm::Value *CodeGen::emitAnyToString(llvm::Value *anyVal, bool inCollection) {
    llvm::AllocaInst *tmp = builder_.CreateAlloca(anyTy_, nullptr, "any.ts");
    builder_.CreateStore(anyVal, tmp);
    llvm::FunctionType *fnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_}, false);
    const char *fnName = inCollection ? "__ry_any_to_string_in_collection"
                                      : "__ry_any_to_string";
    llvm::FunctionCallee fn = mod_->getOrInsertFunction(fnName, fnTy);
    return builder_.CreateCall(fn, {tmp}, "any.ts.str");
}

llvm::Value *CodeGen::emitAnyBinaryOp(const std::string &op,
                                       llvm::Value *lhs, llvm::Value *rhs) {
    // Only equality comparisons reach this function: arithmetic and
    // ordering operators are rejected upstream by the `any-arithmetic`
    // rule (see isAnyArithOp; the guard fires in emitBinaryOp before any
    // operand is wrapped in any). The runtime entry points take pointer
    // arguments, so wrap each operand in an alloca.
    if (op != "==" && op != "!=")
        codegenError("operator '" + op + "' not supported for any type");

    llvm::AllocaInst *lhsPtr = builder_.CreateAlloca(anyTy_, nullptr, "any.lhs");
    builder_.CreateStore(lhs, lhsPtr);
    llvm::AllocaInst *rhsPtr = builder_.CreateAlloca(anyTy_, nullptr, "any.rhs");
    builder_.CreateStore(rhs, rhsPtr);

    const char *runtimeFn = (op == "==") ? "__ry_any_eq" : "__ry_any_ne";
    llvm::FunctionCallee fn = getRuntimeFn(runtimeFn, i64Ty_, {ptrTy_, ptrTy_});
    llvm::Value *result = builder_.CreateCall(fn, {lhsPtr, rhsPtr}, "any.cmp");
    return builder_.CreateICmpNE(result, builder_.getInt64(0), "any.cmp.bool");
}

llvm::Value *CodeGen::emitAnyPathStep(llvm::Value *anyVal,
                                       llvm::Value *segmentStr,
                                       std::optional<int64_t> intSegment,
                                       bool tryMode,
                                       const std::string &pathLabel,
                                       const std::string &segmentText) {
    llvm::Function *fn = builder_.GetInsertBlock()->getParent();
    llvm::StructType *optAnyTy = tryMode ? getOptionType(anyTy_) : nullptr;

    llvm::Value *tag = builder_.CreateExtractValue(anyVal, 0, "pathstep.tag");
    llvm::Value *isMap = builder_.CreateICmpEQ(
        tag,
        llvm::ConstantInt::get(i64Ty_, static_cast<int64_t>(RyAnyTag::Map)),
        "pathstep.is_map");
    llvm::Value *isList = builder_.CreateICmpEQ(
        tag,
        llvm::ConstantInt::get(i64Ty_, static_cast<int64_t>(RyAnyTag::List)),
        "pathstep.is_list");

    auto *mapBB      = createBBInFn("pathstep.map", fn);
    auto *listCheckBB= createBBInFn("pathstep.list_check", fn);
    auto *listBB     = intSegment ? createBBInFn("pathstep.list", fn) : nullptr;
    auto *mismatchBB = createBBInFn("pathstep.mismatch", fn);
    auto *foundBB    = createBBInFn("pathstep.found", fn);
    auto *missBB     = createBBInFn("pathstep.miss", fn);
    auto *mergeBB    = tryMode ? createBBInFn("pathstep.merge", fn) : nullptr;
    emitBranchCond(isMap, mapBB, listCheckBB);

    // List tag check: when caller supplied an int form for the segment,
    // route a List hop through int-index logic; otherwise mismatch.
    builder_.SetInsertPoint(listCheckBB);
    if (intSegment)
        emitBranchCond(isList, listBB, mismatchBB);
    else
        emitBranchUncond(mismatchBB);

    // Map arm.
    builder_.SetInsertPoint(mapBB);
    llvm::AllocaInst *anyTmp =
        builder_.CreateAlloca(anyTy_, nullptr, "pathstep.any.tmp");
    builder_.CreateStore(anyVal, anyTmp);
    llvm::Value *anyDataSlot =
        builder_.CreateStructGEP(anyTy_, anyTmp, 1, "pathstep.any.data.slot");
    llvm::Value *mapPtr =
        builder_.CreateLoad(ptrTy_, anyDataSlot, "pathstep.map.ptr");

    llvm::Value *slot = emitHashTableLookup(
        mapPtr, mapHeaderTy_, kMapLayout, segmentStr, ptrTy_);
    llvm::Value *mapMiss = builder_.CreateICmpEQ(
        slot, llvm::ConstantInt::getSigned(i64Ty_, -1), "pathstep.map.miss");
    auto *mapHitBB = createBBInFn("pathstep.map_hit", fn);
    emitBranchCond(mapMiss, missBB, mapHitBB);

    builder_.SetInsertPoint(mapHitBB);
    llvm::Value *valsField =
        builder_.CreateStructGEP(mapHeaderTy_, mapPtr, 3, "pathstep.vals.field");
    llvm::Value *valsPtr =
        builder_.CreateLoad(ptrTy_, valsField, "pathstep.vals.ptr");
    llvm::Value *mapElemPtr =
        builder_.CreateGEP(anyTy_, valsPtr, slot, "pathstep.map.elem.ptr");
    llvm::Value *mapFoundAny =
        builder_.CreateLoad(anyTy_, mapElemPtr, "pathstep.map.elem");
    llvm::BasicBlock *mapFoundEndBB = builder_.GetInsertBlock();
    emitBranchUncond(foundBB);

    // List arm: bounds-check intSegment against header.len, GEP data[idx].
    llvm::BasicBlock *listFoundEndBB = nullptr;
    llvm::Value *listFoundAny = nullptr;
    if (intSegment) {
        builder_.SetInsertPoint(listBB);
        llvm::AllocaInst *listAnyTmp =
            builder_.CreateAlloca(anyTy_, nullptr, "pathstep.list.any.tmp");
        builder_.CreateStore(anyVal, listAnyTmp);
        llvm::Value *listAnyDataSlot =
            builder_.CreateStructGEP(anyTy_, listAnyTmp, 1,
                                      "pathstep.list.any.data.slot");
        llvm::Value *listPtr =
            builder_.CreateLoad(ptrTy_, listAnyDataSlot, "pathstep.list.ptr");

        // List header: { len(0), cap(1), data(2) }. anyTy_ stride.
        llvm::Value *lenField = builder_.CreateStructGEP(
            listHeaderTy_, listPtr, 0, "pathstep.list.len.field");
        llvm::Value *len = builder_.CreateLoad(
            i64Ty_, lenField, "pathstep.list.len");
        // `*intSegment` is non-negative by `tryParseSegmentInt`'s
        // invariant (src/codegen_call_collection.cpp), so a negative-
        // wrap + `idx < 0` check would be dead — only the upper bound
        // can fire (#2301).
        llvm::Value *idxVal = llvm::ConstantInt::getSigned(i64Ty_, *intSegment);
        llvm::Value *oob = builder_.CreateICmpSGE(idxVal, len, "pathstep.list.oob");
        auto *listHitBB = createBBInFn("pathstep.list_hit", fn);
        emitBranchCond(oob, missBB, listHitBB);

        builder_.SetInsertPoint(listHitBB);
        llvm::Value *dataField = builder_.CreateStructGEP(
            listHeaderTy_, listPtr, 2, "pathstep.list.data.field");
        llvm::Value *dataPtr = builder_.CreateLoad(
            ptrTy_, dataField, "pathstep.list.data.ptr");
        llvm::Value *listElemPtr = builder_.CreateGEP(
            anyTy_, dataPtr, idxVal, "pathstep.list.elem.ptr");
        listFoundAny = builder_.CreateLoad(
            anyTy_, listElemPtr, "pathstep.list.elem");
        listFoundEndBB = builder_.GetInsertBlock();
        emitBranchUncond(foundBB);
    }

    // Found arm: receives mapFoundAny or listFoundAny via PHI on anyTy_.
    builder_.SetInsertPoint(foundBB);
    llvm::PHINode *foundPhi = createPhi(anyTy_, {}, "pathstep.found.any");
    foundPhi->addIncoming(mapFoundAny, mapFoundEndBB);
    if (listFoundEndBB)
        foundPhi->addIncoming(listFoundAny, listFoundEndBB);
    llvm::Value *foundResult =
        tryMode ? buildSomeValue(foundPhi, optAnyTy) : foundPhi;
    llvm::BasicBlock *foundEndBB = builder_.GetInsertBlock();
    if (tryMode) emitBranchUncond(mergeBB);

    // Miss arm: container hit but key/index absent.
    builder_.SetInsertPoint(missBB);
    llvm::Value *missResult = nullptr;
    llvm::BasicBlock *missEndBB = nullptr;
    if (tryMode) {
        missResult = buildNoneValue(optAnyTy);
        missEndBB = builder_.GetInsertBlock();
        emitBranchUncond(mergeBB);
    } else {
        std::string msg =
            "path '" + pathLabel + "': segment '" + segmentText + "' not found";
        emitRuntimeError(msg, "err.pathstep.miss");
    }

    // Mismatch arm: any tag is not a container the caller can index here.
    builder_.SetInsertPoint(mismatchBB);
    llvm::Value *mismatchResult = nullptr;
    llvm::BasicBlock *mismatchEndBB = nullptr;
    if (tryMode) {
        mismatchResult = buildNoneValue(optAnyTy);
        mismatchEndBB = builder_.GetInsertBlock();
        emitBranchUncond(mergeBB);
    } else {
        std::string msg = "path '" + pathLabel + "': segment '" + segmentText
                          + "' applied to non-container any value";
        emitRuntimeError(msg, "err.pathstep.mismatch");
    }

    if (!tryMode) {
        builder_.SetInsertPoint(foundEndBB);
        return foundResult;
    }

    builder_.SetInsertPoint(mergeBB);
    llvm::PHINode *phi = createPhi(optAnyTy, {}, "pathstep.result");
    phi->addIncoming(foundResult, foundEndBB);
    phi->addIncoming(missResult, missEndBB);
    phi->addIncoming(mismatchResult, mismatchEndBB);
    return phi;
}


} // namespace ry
