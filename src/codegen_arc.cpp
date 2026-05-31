#include "ry/codegen.hpp"
#include "ry/codegen/lowered_arc.hpp"
#include "ry/stdlib_registry.hpp"
#include <cassert>
#include <cstdint>

// Exposes the address of the relaxed-atomic ARC live-count counter so that
// codegen can embed it as an inttoptr constant and emit inline atomicrmw
// instructions.  Using inttoptr avoids introducing __ry_arc_alloc_counted /
// __ry_arc_free_counted as new function-call symbols in the JIT module, which
// would cause JITLink to create GOT stubs that crash Linux teardown.
extern "C" int64_t *__ry_arc_counter_address();

namespace ry {

// Emits an inline `atomicrmw add` on the ARC live-count counter with
// `delta` (+1 for alloc, -1 for free).  No new function symbol is added to
// the IR module — the counter address is encoded as an i64 constant.
static void emitArcCounterDeltaIR(llvm::IRBuilder<> &builder,
                                   llvm::Type *i64Ty, llvm::Type *ptrTy,
                                   int64_t delta) {
    // NOLINTNEXTLINE(performance-no-int-to-ptr)
    auto *ctrAddrConst = llvm::ConstantInt::get(
        i64Ty, static_cast<uint64_t>(
                   reinterpret_cast<uintptr_t>(__ry_arc_counter_address())));
    auto *ctrPtr = builder.CreateIntToPtr(ctrAddrConst, ptrTy, "arc_ctr");
    builder.CreateAtomicRMW(llvm::AtomicRMWInst::Add, ctrPtr,
        llvm::ConstantInt::get(i64Ty, static_cast<uint64_t>(delta)),
        llvm::MaybeAlign(8), llvm::AtomicOrdering::Monotonic);
}

llvm::Value *CodeGen::emitArcAlloc(llvm::Value *dataSize) {
    auto *headerSize = llvm::ConstantInt::get(i64Ty_, ARC_HEADER_SIZE);
    auto *totalSize = builder_.CreateAdd(dataSize, headerSize, "arc_total");
    auto *headerPtr = builder_.CreateCall(getStdlibMalloc(), {totalSize}, "arc_hdr");
    // Increment the ARC live-count balance counter inline (no new symbol).
    emitArcCounterDeltaIR(builder_, i64Ty_, ptrTy_, +1);

    auto *strongPtr = builder_.CreateStructGEP(arcHeaderTy_, headerPtr, 0, "arc_strong_ptr");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 1), strongPtr);

    auto *weakPtr = builder_.CreateStructGEP(arcHeaderTy_, headerPtr, 1, "arc_weak_ptr");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), weakPtr);

    return headerPtr;
}

llvm::Value *CodeGen::emitArcGetDataPtr(llvm::Value *headerPtr) {
    auto *dataPtr = builder_.CreateGEP(i8Ty_, headerPtr,
                              llvm::ConstantInt::get(i64Ty_, ARC_HEADER_SIZE),
                              "arc_data");
    arc_owned_values_.insert(dataPtr);
    return dataPtr;
}

llvm::Value *CodeGen::emitArcAllocCollectionHeader(llvm::Type *headerTy) {
    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t size = dl.getTypeAllocSize(headerTy);
    auto *arcHdr = emitArcAlloc(llvm::ConstantInt::get(i64Ty_, size));
    return emitArcGetDataPtr(arcHdr);
}

llvm::Value *CodeGen::emitArcGetHeaderFromData(llvm::Value *dataPtr) {
    return builder_.CreateGEP(i8Ty_, dataPtr,
                              llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(-static_cast<int64_t>(ARC_HEADER_SIZE))),
                              "arc_hdr_from_data");
}

llvm::Value *CodeGen::emitStrGetHeaderFromData(llvm::Value *strHandle) {
    // Strings have STRING_HEADER_SIZE (24) bytes before the data pointer:
    // { strong_count: i64, weak_count: i64, byte_len: i64, data... }
    return builder_.CreateGEP(i8Ty_, strHandle,
        llvm::ConstantInt::get(i64Ty_,
            static_cast<uint64_t>(-static_cast<int64_t>(STRING_HEADER_SIZE))),
        "str_hdr_from_data");
}

llvm::Value *CodeGen::emitArcHeaderForAlloca(llvm::Value *handle, llvm::AllocaInst *srcAlloca) {
    if (srcAlloca && arc_str_managed_vars_.count(srcAlloca))
        return emitStrGetHeaderFromData(handle);
    return emitArcGetHeaderFromData(handle);
}

llvm::Value *CodeGen::emitStrGetDataPtr(llvm::Value *strHeaderPtr) {
    // Recover the str handle from the StringHeader pointer.
    auto *dataPtr = builder_.CreateGEP(i8Ty_, strHeaderPtr,
        llvm::ConstantInt::get(i64Ty_, STRING_HEADER_SIZE),
        "str_data");
    arc_str_owned_values_.insert(dataPtr);
    return dataPtr;
}

llvm::Value *CodeGen::emitStringByteLen(llvm::Value *handle) {
    // The byte_len field lives STRING_BYTELEN_OFFSET (8) bytes before the
    // string data pointer.  Emit: load i64, (handle - 8)
    auto *bytelenPtr = builder_.CreateGEP(
        i8Ty_, handle,
        llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(-static_cast<int64_t>(STRING_BYTELEN_OFFSET))),
        "str_bytelen_ptr");
    return builder_.CreateLoad(i64Ty_, bytelenPtr, "str_bytelen");
}

llvm::LoadInst *CodeGen::emitAtomicI64Load(llvm::Value *ptr,
                                           llvm::AtomicOrdering ordering,
                                           const llvm::Twine &name) {
    // Non-atomic path must match the old plain CreateLoad behaviour
    // (alignment=1, ABI-default). Forcing Align(8) here would assert a
    // stronger alignment than the surrounding code actually guarantees
    // and crashes on Linux glibc when the underlying pointer happens not
    // to be 8-byte aligned (#630 CI regression).
    if (ordering == llvm::AtomicOrdering::NotAtomic)
        return builder_.CreateLoad(i64Ty_, ptr, name);
    // Atomic i64 loads must be at least 8-byte aligned per LLVM's atomic
    // rules; asserting Align(8) is correct here because the ARC header
    // start is always 16-byte aligned (malloc'd).
    auto *ld = builder_.CreateAlignedLoad(i64Ty_, ptr, llvm::Align(8), name);
    ld->setAtomic(ordering);
    return ld;
}

void CodeGen::emitArcRetain(llvm::Value *headerPtr, bool atomic) {
    // Stage 2-C (#1968): IR construction moved to llvm_emit ABI
    // (ry_emit_arc_retain). This shim is the codegen-side bridge —
    // lower → emit (passthrough lowering).
    auto op = codegen::lowering::lowerArcRetain(*this, headerPtr, atomic);
    codegen::emission::emitArcRetain(*this, op);
}

void CodeGen::emitArcRelease(llvm::Value *headerPtr, bool atomic,
                              llvm::FunctionCallee destructor,
                              llvm::Function *gcVisitFn) {
    // Stage 2-C (#1968): IR construction moved to llvm_emit ABI
    // (ry_emit_arc_release). The lowering layer extracts the C-fnptr
    // Value* from FunctionCallee so the LLVM-typed pair does not need
    // to cross the ABI. used_native_libraries_.insert("gc") is now
    // emitted by codegen_emission_arc.cpp.
    llvm::Value *dtorCallee = destructor
        ? llvm::cast<llvm::Value>(destructor.getCallee())
        : nullptr;
    auto op = codegen::lowering::lowerArcRelease(
        *this, headerPtr, atomic, dtorCallee, gcVisitFn);
    codegen::emission::emitArcRelease(*this, op);
}

bool CodeGen::isArcAtomic(llvm::Value *val) const {
    // Inside a @parallel for thunk every ARC op must be atomic because
    // captured values are shared across workers. The per-value
    // `arc_atomic_values_` tracking only sees one alias depth and would
    // miss values flowing through helper calls or nested expressions.
    // See #630.
    if (parallel_for_depth_ > 0)
        return true;
    if (arc_atomic_values_.count(val))
        return true;
    if (auto *stripped = val->stripPointerCasts())
        if (arc_atomic_values_.count(stripped))
            return true;
    if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
        auto *ptr = load->getPointerOperand();
        if (arc_atomic_values_.count(ptr))
            return true;
        if (auto *strippedPtr = ptr->stripPointerCasts())
            return arc_atomic_values_.count(strippedPtr) > 0;
    }
    return false;
}

void CodeGen::markArcAtomic(llvm::Value *val) {
    arc_atomic_values_.insert(val);
}

void CodeGen::markArcManaged(llvm::AllocaInst *alloca) {
    arc_managed_vars_.insert(alloca);
}

bool CodeGen::isArcManaged(llvm::AllocaInst *alloca) const {
    return arc_managed_vars_.count(alloca) > 0;
}

bool CodeGen::isStrHandle(llvm::Value *v) const {
    if (v->getType() != ptrTy_) return false;
    if (arc_str_owned_values_.count(v) > 0) return true;
    if (auto *ld = llvm::dyn_cast<llvm::LoadInst>(v)) {
        auto *src = llvm::dyn_cast<llvm::AllocaInst>(ld->getPointerOperand());
        if (src && arc_str_managed_vars_.count(src) > 0) return true;
    }
    return false;
}

llvm::FunctionCallee CodeGen::resolveDestructor(llvm::AllocaInst *alloca) {
    auto collDtor = resolveCollectionDestructor(alloca);
    if (collDtor)
        return collDtor;
    auto it = resource_managed_vars_.find(alloca);
    if (it != resource_managed_vars_.end())
        return getOrCreateResourceDestructor(it->second);
    if (closure_managed_vars_.count(alloca)) {
        auto *meta = getMeta(alloca);
        if (meta && meta->fn_type_info) {
            if (meta->fn_type_info->isUniformClosure) {
                auto *dtorFn = getOrCreateUniformClosureDestructor();
                return llvm::FunctionCallee(dtorFn->getFunctionType(), dtorFn);
            }
            if (!meta->fn_type_info->capturedArcKinds.empty())
                return getOrCreateClosureDestructor(*meta->fn_type_info);
        }
    }
    return {};
}

int CodeGen::detectResourceKind(llvm::Value *val) {
    auto *meta = getMeta(val);
    if (meta && !meta->resource_kinds.empty())
        return meta->resource_kinds[0];
    return ResourceKindRegistry::NONE;
}


void CodeGen::nullifyResourceVar(const ExprNode &argExpr) {
    // After explicit free/close, null out the variable's alloca so that
    // ARC scope cleanup (emitArcReleaseVar) skips it via its null check.
    if (auto *varExpr = std::get_if<VariableExpr>(&argExpr.data)) {
        auto *alloca = findVar(varExpr->name);
        if (alloca && resource_managed_vars_.count(alloca)) {
            builder_.CreateStore(
                llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)),
                alloca);
        }
    }
}

llvm::Value *CodeGen::emitResourceFree(llvm::Value *dataPtr, int rk,
                                        const ExprNode &argExpr) {
    // Null check — already freed (nullified) variable is a no-op
    auto *isNull = builder_.CreateICmpEQ(
        dataPtr,
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)),
        "res_free_null");
    auto *fn = builder_.GetInsertBlock()->getParent();
    auto *releaseBB = createBBInFn("res_free.release", fn);
    auto *doneBB = createBBInFn("res_free.done", fn);
    emitBranchCond(isNull, doneBB, releaseBB);

    builder_.SetInsertPoint(releaseBB);
    auto *hdr = emitArcGetHeaderFromData(dataPtr);
    bool atomic = isArcAtomic(dataPtr);
    emitArcRelease(hdr, atomic, getOrCreateResourceDestructor(rk));
    emitBranchUncond(doneBB);

    builder_.SetInsertPoint(doneBB);
    nullifyResourceVar(argExpr);
    return llvm::ConstantInt::get(i8Ty_, 0); // Unit
}

llvm::FunctionCallee CodeGen::getOrCreateResourceDestructor(int rk) {
    auto it = resource_destructors_cache_.find(rk);
    if (it != resource_destructors_cache_.end())
        return it->second;

    auto *info = ResourceKindRegistry::instance().getInfo(rk);
    if (!info || !info->dtorName)
        return {};

    const char *dtorName = info->dtorName;
    const char *cleanupFnName = info->cleanupFnName;
    if (info->library)
        used_native_libraries_.insert(info->library);

    auto *dtorTy = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
    auto *dtorFn = llvm::Function::Create(
        dtorTy, llvm::Function::InternalLinkage, dtorName, mod_.get());
    dtorFn->addFnAttr(llvm::Attribute::NoUnwind);

    auto *entryBB = createBBInFn("entry", dtorFn);

    auto *savedBB = builder_.GetInsertBlock();
    auto savedPt = builder_.GetInsertPoint();
    builder_.SetInsertPoint(entryBB);

    auto *dataPtr = dtorFn->getArg(0);
    auto cleanupTy = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
    auto cleanupFn = mod_->getOrInsertFunction(cleanupFnName, cleanupTy);
    builder_.CreateCall(cleanupFn, {dataPtr});
    builder_.CreateRetVoid();

    if (savedBB)
        builder_.SetInsertPoint(savedBB, savedPt);

    llvm::FunctionCallee callee(dtorTy, dtorFn);
    resource_destructors_cache_[rk] = callee;
    return callee;
}

llvm::FunctionCallee CodeGen::resolveCollectionDestructor(llvm::AllocaInst *alloca) {
    auto *meta = getMeta(alloca);
    if (getTypeMeta(TypeMeta::ListElem, alloca)) {
        std::string elem = (meta && !meta->list_elem_type_name.empty())
            ? resolveTypeAlias(meta->list_elem_type_name) : "";
        return getOrCreateCollectionDestructor(CollectionKind::List, elem, "");
    }
    if (getTypeMeta(TypeMeta::MapKey, alloca)) {
        std::string key = (meta && !meta->map_key_type_name.empty())
            ? resolveTypeAlias(meta->map_key_type_name) : "";
        std::string val = (meta && !meta->map_value_type_name.empty())
            ? resolveTypeAlias(meta->map_value_type_name) : "";
        return getOrCreateCollectionDestructor(CollectionKind::Map, key, val);
    }
    if (getTypeMeta(TypeMeta::SetElem, alloca)) {
        std::string elem = (meta && !meta->set_elem_type_name.empty())
            ? resolveTypeAlias(meta->set_elem_type_name) : "";
        return getOrCreateCollectionDestructor(CollectionKind::Set, elem, "");
    }
    return {};
}

void CodeGen::emitArcReleaseVar(const std::string &name, llvm::AllocaInst *alloca) {
    auto *val = builder_.CreateLoad(ptrTy_, alloca, name + ".arc_cleanup");
    auto *isNull = builder_.CreateICmpEQ(val,
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)),
        "arc_null_check");

    auto *fn = builder_.GetInsertBlock()->getParent();
    auto *releaseBB = createBBInFn("arc.var_release", fn);
    auto *skipBB = createBBInFn("arc.var_skip", fn);
    emitBranchCond(isNull, skipBB, releaseBB);

    builder_.SetInsertPoint(releaseBB);
    const bool isStrVar = arc_str_managed_vars_.count(alloca) > 0;
    auto *headerPtr = isStrVar ? emitStrGetHeaderFromData(val)
                               : emitArcGetHeaderFromData(val);

    // Look up GC visit function for potentially cyclic types.
    llvm::Function *gcVisitFn = nullptr;
    auto *meta = getMeta(alloca);
    if (meta && !meta->enum_value_type.empty() && isPotentiallyCyclic(meta->enum_value_type)) {
        gcVisitFn = getOrCreateVisitFunction(meta->enum_value_type);
    }

    // str values have no inner destructor (StringHeader has no child allocations).
    llvm::FunctionCallee destructor = isStrVar ? llvm::FunctionCallee{}
                                               : resolveDestructor(alloca);
    emitArcRelease(headerPtr, isArcAtomic(val), destructor, gcVisitFn);
    emitBranchUncond(skipBB);

    builder_.SetInsertPoint(skipBB);
}

// ===== Collection type name helpers =====

bool CodeGen::isCollectionTypeName(const std::string &typeName) {
    return ry::util::isListTypeName(typeName) || ry::util::isMapTypeName(typeName) || ry::util::isSetTypeName(typeName);
}

bool CodeGen::fieldTypeIsArcManaged(const std::string &fieldTypeName,
                                     CollectionKind *outFieldKind) {
    // Element-level / field-level ARC management is decided by the Ry *type
    // name*, not the LLVM type — ARC-managed slots are opaque ptr and the
    // LLVM type alone tells us nothing. Only non-weak nested collections
    // own an allocation per slot; strings, weak refs, closures, and records
    // are excluded and handled by different fix paths.
    //
    // Resolve type aliases first: `type Ints = List<int>` used as a field
    // type shows up here as `"Ints"`, which matches none of the prefix
    // predicates. Without this step alias-backed ARC fields would be
    // classified as non-ARC and their overwrite would silently leak.
    if (fieldTypeName.empty())
        return false;
    const std::string resolved = resolveTypeAlias(fieldTypeName);
    if (ry::util::isWeakTypeName(resolved))
        return false;
    if (resolved == "str") {
        // str is ARC-managed via StringHeader (handle - STRING_HEADER_SIZE = 24).
        // CollectionKind::Str signals callers to use emitStrGetHeaderFromData.
        if (outFieldKind) *outFieldKind = CollectionKind::Str;
        return true;
    }
    if (ry::util::isListTypeName(resolved)) {
        if (outFieldKind) *outFieldKind = CollectionKind::List;
        return true;
    }
    if (ry::util::isMapTypeName(resolved)) {
        if (outFieldKind) *outFieldKind = CollectionKind::Map;
        return true;
    }
    if (ry::util::isSetTypeName(resolved)) {
        if (outFieldKind) *outFieldKind = CollectionKind::Set;
        return true;
    }
    return false;
}

// ===== Record ARC field retain/release (#854 Layer 2) =====

bool CodeGen::recordHasArcFields(llvm::StructType *st) {
    if (!st || !st->hasName())
        return false;
    auto it = record_types_.find(st->getName().str());
    if (it == record_types_.end())
        return false;
    for (const auto &fd : it->second.fields) {
        if (!fd.type) continue;
        if (fieldTypeIsArcManaged(fd.type->toString()))
            return true;
    }
    return false;
}

// Trace an InsertValue chain to find the original value inserted at a given
// field index.  LLVM's ConstantFolder does NOT fold
//   ExtractValue(InsertValue(agg, v, {i}), {i}) → v
// for non-constant aggregates, so the ExtractValue result is a fresh
// instruction that is never in arc_owned_values_ / arc_str_owned_values_.
// This helper recovers the original inserted operand so that ownership
// checks work correctly for record construction IR.
llvm::Value *CodeGen::traceInsertValueField(llvm::Value *agg, unsigned idx) {
    while (auto *iv = llvm::dyn_cast<llvm::InsertValueInst>(agg)) {
        if (iv->getIndices().size() == 1 && iv->getIndices()[0] == idx)
            return iv->getInsertedValueOperand();
        agg = iv->getAggregateOperand();
    }
    return nullptr;
}

void CodeGen::emitRecordArcFieldsRetain(llvm::Value *recordVal,
                                          llvm::StructType *st) {
    if (!st || !st->hasName())
        return;
    auto it = record_types_.find(st->getName().str());
    if (it == record_types_.end())
        return;
    const auto &info = it->second;
    for (unsigned i = 0; i < info.fields.size(); ++i) {
        const auto &fd = info.fields[i];
        if (!fd.type) continue;
        CollectionKind fk;
        if (!fieldTypeIsArcManaged(fd.type->toString(), &fk))
            continue;
        llvm::Value *fieldVal = builder_.CreateExtractValue(
            recordVal, i, fd.name + ".record_retain");
        // Skip freshly-owned values: inline list/str allocations (in arc_owned_values_ /
        // arc_str_owned_values_) are being transferred into the record — the record
        // becomes the sole owner so no retain is needed.  Named-variable values (loaded
        // from arc_backed_vars_ / arc_str_managed_vars_ allocas) are reference copies
        // that require a retain.
        //
        // For InsertValue construction IR, CreateExtractValue yields a fresh
        // instruction not in the owned sets.  Trace the original operand instead.
        llvm::Value *checkVal = fieldVal;
        if (llvm::isa<llvm::InsertValueInst>(recordVal))
            if (auto *orig = traceInsertValueField(recordVal, i))
                checkVal = orig;
        if (arc_owned_values_.count(checkVal) || arc_str_owned_values_.count(checkVal))
            continue;
        // Null guard — freshly-inserted fields are always non-null in
        // practice but cheap to defend against for robustness.
        auto *isNull = builder_.CreateICmpEQ(
            fieldVal,
            llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)),
            fd.name + ".record_retain_null");
        auto *fn = builder_.GetInsertBlock()->getParent();
        auto *retainBB = createBBInFn("record.field_retain", fn);
        auto *skipBB = createBBInFn("record.field_retain_skip", fn);
        emitBranchCond(isNull, skipBB, retainBB);
        builder_.SetInsertPoint(retainBB);
        auto *hdr = (fk == CollectionKind::Str) ? emitStrGetHeaderFromData(fieldVal)
                                                 : emitArcGetHeaderFromData(fieldVal);
        emitArcRetain(hdr, /*atomic=*/false);
        emitBranchUncond(skipBB);
        builder_.SetInsertPoint(skipBB);
    }
}

void CodeGen::emitRecordArcFieldsRelease(llvm::Value *recordVal,
                                           llvm::StructType *st) {
    if (!st || !st->hasName())
        return;
    auto it = record_types_.find(st->getName().str());
    if (it == record_types_.end())
        return;
    const auto &info = it->second;
    for (unsigned i = 0; i < info.fields.size(); ++i) {
        const auto &fd = info.fields[i];
        if (!fd.type) continue;
        CollectionKind fk;
        if (!fieldTypeIsArcManaged(fd.type->toString(), &fk))
            continue;
        llvm::Value *fieldVal = builder_.CreateExtractValue(
            recordVal, i, fd.name + ".record_release");
        emitArcReleaseLoadedElement(fieldVal, fk, fd.type->toString(), fd.name);
    }
}

// #1640: Releases the active payload slot of a tagged-union (Result/Option)
// subject alloca at scope exit. Loads the struct, extracts the i1 tag, and
// branches: tag=true picks Ok/Some (slot index 1), tag=false picks Err
// (slot index 2 for Result; Option's None has no payload). Each active
// slot is released via `emitArcReleaseLoadedElement` only when the
// corresponding inner type is ARC-managed; non-ARC slots emit no IR in
// their branch.
void CodeGen::emitTaggedUnionRelease(llvm::AllocaInst *alloca,
                                      const std::string &sourceTypeName) {
    if (!alloca) return;
    llvm::Type *ty = alloca->getAllocatedType();
    auto *st = llvm::dyn_cast<llvm::StructType>(ty);
    if (!st) return;

    const bool isResult = isResultType(ty);
    const bool isOption = isOptionType(ty);
    if (!isResult && !isOption) return;

    // Resolve inner Ok/Err (or Some) type names from the source type string.
    // No fallback to reverseResolveTypeName: per #1156, that channel is lossy
    // and would misclassify Option<List<int>> as Option<str> (wrong header
    // offset, heap corruption).
    std::string resolvedSource = resolveTypeAlias(sourceTypeName);
    std::string okName, errName;
    if (isOption && resolvedSource.size() > 1 && resolvedSource.back() == '?') {
        // T? shorthand for Option<T>
        okName = ry::util::trimTypeNameSpaces(
            resolvedSource.substr(0, resolvedSource.size() - 1));
    } else {
        std::string head;
        std::vector<std::string> innerArgs;
        if (ry::util::splitGenericTypeName(resolvedSource, head, innerArgs)) {
            if (isResult && head == "Result") {
                if (!innerArgs.empty()) okName = ry::util::trimTypeNameSpaces(innerArgs[0]);
                if (innerArgs.size() >= 2) errName = ry::util::trimTypeNameSpaces(innerArgs[1]);
            } else if (isOption && head == "Option") {
                if (!innerArgs.empty()) okName = ry::util::trimTypeNameSpaces(innerArgs[0]);
            }
        }
    }

    CollectionKind okKind = CollectionKind::List;
    CollectionKind errKind = CollectionKind::List;
    const bool okArc = !okName.empty() && fieldTypeIsArcManaged(okName, &okKind);
    const bool errArc = isResult && !errName.empty() &&
                         fieldTypeIsArcManaged(errName, &errKind);
    if (!okArc && !errArc) return;

    auto *okBB = createBB("tu.ok");
    auto *errBB = createBB("tu.err");
    auto *mergeBB = createBB("tu.merge");

    llvm::Value *loaded = builder_.CreateLoad(ty, alloca, "tu.load");
    llvm::Value *tag = builder_.CreateExtractValue(loaded, 0, "tu.tag");
    emitBranchCond(tag, okBB, errBB);

    // Ok / Some path
    builder_.SetInsertPoint(okBB);
    if (okArc) {
        llvm::Value *okVal = builder_.CreateExtractValue(loaded, 1, "tu.ok_val");
        emitArcReleaseLoadedElement(okVal, okKind, okName, "tu.ok");
    }
    emitBranchUncond(mergeBB);

    // Err path (Result only; Option's None has no payload)
    builder_.SetInsertPoint(errBB);
    if (errArc) {
        llvm::Value *errVal = builder_.CreateExtractValue(loaded, 2, "tu.err_val");
        emitArcReleaseLoadedElement(errVal, errKind, errName, "tu.err");
    }
    emitBranchUncond(mergeBB);

    builder_.SetInsertPoint(mergeBB);
}

bool CodeGen::elementTypeIsArcManaged(llvm::Value *containerPtr,
                                       CollectionKind containerKind,
                                       CollectionKind *outElemKind) {
    // Callers reach this only after `objPtr->getType() != ptrTy_` has
    // already been rejected with `codegenError("index assignment requires
    // list or map")`, so `containerPtr` is guaranteed non-null and of
    // pointer type. An assert documents the invariant without paying for
    // a runtime check in release builds.
    assert(containerPtr && containerPtr->getType() == ptrTy_ &&
           "elementTypeIsArcManaged expects a pointer-typed container");

    auto *meta = getMeta(containerPtr);
    if (!meta)
        return false;

    const std::string *elemTypeName = nullptr;
    switch (containerKind) {
    case CollectionKind::List:
        elemTypeName = &meta->list_elem_type_name;
        break;
    case CollectionKind::Map:
        elemTypeName = &meta->map_value_type_name;
        break;
    case CollectionKind::Set:
        elemTypeName = &meta->set_elem_type_name;
        break;
    case CollectionKind::Str:
        // str is a scalar, not a container — no nested element type.
        return false;
    }
    if (!elemTypeName)
        return false;
    return fieldTypeIsArcManaged(*elemTypeName, outElemKind);
}

// ===== Weak reference operations =====

std::string CodeGen::weakInnerTypeName(const std::string &typeName) {
    return typeName.substr(5);
}

void CodeGen::markWeakManaged(llvm::AllocaInst *alloca) {
    weak_managed_vars_.insert(alloca);
}

bool CodeGen::isWeakManaged(llvm::AllocaInst *alloca) const {
    return weak_managed_vars_.count(alloca) > 0;
}

void CodeGen::emitWeakRetain(llvm::Value *headerPtr) {
    // Skip immortal objects (strong_count == ARC_IMMORTAL) — e.g. string literals
    auto *strongPtr = builder_.CreateStructGEP(arcHeaderTy_, headerPtr, 0, "weak_retain_strong");
    auto *strong = builder_.CreateLoad(i64Ty_, strongPtr, "weak_retain_sc");
    auto *isImmortal = builder_.CreateICmpEQ(strong, llvm::ConstantInt::get(i64Ty_, ARC_IMMORTAL), "weak_retain_immortal");

    auto *fn = builder_.GetInsertBlock()->getParent();
    auto *retainBB = createBBInFn("weak.retain", fn);
    auto *doneBB = createBBInFn("weak.retain.done", fn);
    emitBranchCond(isImmortal, doneBB, retainBB);

    builder_.SetInsertPoint(retainBB);
    auto *weakPtr = builder_.CreateStructGEP(arcHeaderTy_, headerPtr, 1, "weak_retain_ptr");
    builder_.CreateAtomicRMW(llvm::AtomicRMWInst::Add, weakPtr,
                             llvm::ConstantInt::get(i64Ty_, 1),
                             llvm::MaybeAlign(),
                             llvm::AtomicOrdering::SequentiallyConsistent);
    emitBranchUncond(doneBB);

    builder_.SetInsertPoint(doneBB);
}

void CodeGen::emitWeakRelease(llvm::Value *headerPtr) {
    // Skip immortal objects (strong_count == ARC_IMMORTAL) — e.g. string literals
    auto *strongCheckPtr = builder_.CreateStructGEP(arcHeaderTy_, headerPtr, 0, "weak_rel_strong");
    auto *strongCheck = builder_.CreateLoad(i64Ty_, strongCheckPtr, "weak_rel_sc");
    auto *isImmortal = builder_.CreateICmpEQ(strongCheck, llvm::ConstantInt::get(i64Ty_, ARC_IMMORTAL), "weak_rel_immortal");

    auto *fn = builder_.GetInsertBlock()->getParent();
    auto *releaseBB = createBBInFn("weak.release.body", fn);
    auto *doneBB = createBBInFn("weak.done", fn);
    emitBranchCond(isImmortal, doneBB, releaseBB);

    builder_.SetInsertPoint(releaseBB);
    auto *weakPtr = builder_.CreateStructGEP(arcHeaderTy_, headerPtr, 1, "weak_rel_ptr");
    auto *oldWeak = builder_.CreateAtomicRMW(
        llvm::AtomicRMWInst::Sub, weakPtr,
        llvm::ConstantInt::get(i64Ty_, 1),
        llvm::MaybeAlign(),
        llvm::AtomicOrdering::SequentiallyConsistent);
    auto *isZeroWeak = builder_.CreateICmpEQ(oldWeak, llvm::ConstantInt::get(i64Ty_, 1), "weak_zero");

    auto *checkStrongBB = createBBInFn("weak.check_strong", fn);
    auto *freeBB = createBBInFn("weak.free", fn);
    emitBranchCond(isZeroWeak, checkStrongBB, doneBB);

    builder_.SetInsertPoint(checkStrongBB);
    auto *strongPtr = builder_.CreateStructGEP(arcHeaderTy_, headerPtr, 0, "weak_strong_ptr");
    auto *strong = builder_.CreateLoad(i64Ty_, strongPtr, "weak_strong");
    strong->setAtomic(llvm::AtomicOrdering::Acquire);
    auto *isZeroStrong = builder_.CreateICmpEQ(strong, llvm::ConstantInt::get(i64Ty_, 0), "strong_zero");
    emitBranchCond(isZeroStrong, freeBB, doneBB);

    builder_.SetInsertPoint(freeBB);
    // Decrement the ARC live-count balance counter inline (weak release path).
    emitArcCounterDeltaIR(builder_, i64Ty_, ptrTy_, -1);
    builder_.CreateCall(getStdlibFree(), {headerPtr});
    emitBranchUncond(doneBB);

    builder_.SetInsertPoint(doneBB);
}

llvm::Value *CodeGen::emitWeakUpgrade(llvm::Value *headerPtr,
                                       const std::string &innerTypeName) {
    const std::string resolvedInner = resolveTypeAlias(innerTypeName);
    auto *innerTy = resolveType(innerTypeName);
    auto *optionTy = getOptionType(innerTy);

    auto *resultAlloca = builder_.CreateAlloca(optionTy, nullptr, "weak_upgrade_result");

    auto *strongPtr = builder_.CreateStructGEP(arcHeaderTy_, headerPtr, 0, "weak_up_strong_ptr");

    auto *fn = builder_.GetInsertBlock()->getParent();
    auto *immortalBB = createBBInFn("weak.immortal", fn);
    auto *loopBB = createBBInFn("weak.cas_loop", fn);
    auto *tryIncBB = createBBInFn("weak.try_inc", fn);
    auto *successBB = createBBInFn("weak.success", fn);
    auto *deadBB = createBBInFn("weak.dead", fn);
    auto *doneBB = createBBInFn("weak.upgrade_done", fn);

    // Immortal objects are always alive — skip CAS and return Some directly
    auto *initCur = builder_.CreateLoad(i64Ty_, strongPtr, "weak_up_init");
    initCur->setAtomic(llvm::AtomicOrdering::Acquire);
    auto *isImmortal = builder_.CreateICmpEQ(initCur, llvm::ConstantInt::get(i64Ty_, ARC_IMMORTAL), "weak_up_immortal");
    emitBranchCond(isImmortal, immortalBB, loopBB);

    // Immortal path: return Some(data_ptr) without incrementing
    builder_.SetInsertPoint(immortalBB);
    // str uses StringHeader (24 bytes); other ARC types use ArcHeader (16 bytes).
    auto *immortalDataPtr = (resolvedInner == "str")
        ? emitStrGetDataPtr(headerPtr)
        : emitArcGetDataPtr(headerPtr);
    auto *immortalSome = buildSomeValue(immortalDataPtr, optionTy);
    builder_.CreateStore(immortalSome, resultAlloca);
    emitBranchUncond(doneBB);

    // CAS loop
    builder_.SetInsertPoint(loopBB);
    auto *cur = builder_.CreateLoad(i64Ty_, strongPtr, "weak_up_cur");
    cur->setAtomic(llvm::AtomicOrdering::Acquire);
    auto *isAlive = builder_.CreateICmpSGT(cur, llvm::ConstantInt::get(i64Ty_, 0), "weak_alive");
    emitBranchCond(isAlive, tryIncBB, deadBB);

    // Try CAS: compare_exchange(strongPtr, cur, cur+1)
    builder_.SetInsertPoint(tryIncBB);
    auto *desired = builder_.CreateAdd(cur, llvm::ConstantInt::get(i64Ty_, 1), "weak_desired");
    auto *cmpxchg = builder_.CreateAtomicCmpXchg(
        strongPtr, cur, desired,
        llvm::MaybeAlign(),
        llvm::AtomicOrdering::AcquireRelease,
        llvm::AtomicOrdering::Monotonic);
    auto *success = builder_.CreateExtractValue(cmpxchg, 1, "weak_cas_ok");
    emitBranchCond(success, successBB, loopBB);

    // Success: strong_count incremented, return Some(data_ptr)
    builder_.SetInsertPoint(successBB);
    auto *dataPtr = (resolvedInner == "str")
        ? emitStrGetDataPtr(headerPtr)
        : emitArcGetDataPtr(headerPtr);
    auto *someVal = buildSomeValue(dataPtr, optionTy);
    builder_.CreateStore(someVal, resultAlloca);
    emitBranchUncond(doneBB);

    // Dead: strong_count == 0, return None
    builder_.SetInsertPoint(deadBB);
    auto *noneVal = buildNoneValue(optionTy);
    builder_.CreateStore(noneVal, resultAlloca);
    emitBranchUncond(doneBB);

    builder_.SetInsertPoint(doneBB);
    return builder_.CreateLoad(optionTy, resultAlloca, "weak_upgraded");
}

void CodeGen::emitWeakReleaseVar(const std::string &name, llvm::AllocaInst *alloca) {
    auto *val = builder_.CreateLoad(ptrTy_, alloca, name + ".weak_cleanup");
    auto *isNull = builder_.CreateICmpEQ(val,
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)),
        "weak_null_check");

    auto *fn = builder_.GetInsertBlock()->getParent();
    auto *releaseBB = createBBInFn("weak.var_release", fn);
    auto *skipBB = createBBInFn("weak.var_skip", fn);
    emitBranchCond(isNull, skipBB, releaseBB);

    builder_.SetInsertPoint(releaseBB);
    emitWeakRelease(val);
    emitBranchUncond(skipBB);

    builder_.SetInsertPoint(skipBB);
}

void CodeGen::retainArcValue(llvm::Value *val) {
    if (tryRetainArcSource(val))
        return;
    const bool isStr = arc_str_owned_values_.count(val) > 0;
    auto *hdr = isStr ? emitStrGetHeaderFromData(val) : emitArcGetHeaderFromData(val);
    emitArcRetain(hdr, /*atomic=*/false);
}

bool CodeGen::tryRetainArcSource(llvm::Value *val) {
    // Case 1: LoadInst from an ARC-managed alloca
    if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
        auto *srcAlloca = llvm::dyn_cast<llvm::AllocaInst>(load->getPointerOperand());
        if (srcAlloca && isArcManaged(srcAlloca)) {
            const bool isStr = arc_str_managed_vars_.count(srcAlloca) > 0;
            auto *hdr = isStr ? emitStrGetHeaderFromData(val)
                              : emitArcGetHeaderFromData(val);
            emitArcRetain(hdr, isArcAtomic(val));
            return true;
        }
    }
    // Case 2: Value produced by emitArcAlloc (e.g., f-string, interpolated string)
    // These already have strong_count=1 from allocation, no retain needed,
    // but signal to caller that this is ARC-owned
    if (arc_owned_values_.count(val))
        return true;
    // Case 2b: str value produced by a runtime function (makeString-backed).
    // strong_count=1 from makeString, so no retain needed.
    if (arc_str_owned_values_.count(val))
        return true;
    // Case 3: ExtractValueInst — record/tuple field access (CreateExtractValue).
    // Guard on collection metadata so closures, weak refs, and other non-ARC
    // ptrTy_ values are not incorrectly retained (#999).
    if (llvm::isa<llvm::ExtractValueInst>(val)) {
        auto *meta = getMeta(val);
        if (meta && (meta->list_elem || meta->map_key || meta->set_elem)) {
            auto *hdr = emitArcGetHeaderFromData(val);
            emitArcRetain(hdr, /*atomic=*/false);
            return true;
        }
    }
    // Case 4: GEP-loaded container element borrowed from a long-lived
    // container. Two metadata gates dispatch to different headers:
    //   - list_elem / map_key / map_value / set_elem → ArcHeader (-16)
    //   - str_elem                                   → StringHeader (-24)
    // Gated on metadata so non-ARC ptrTy_ loads (weak refs, bare fn pointers)
    // are not incorrectly retained — same guard pattern as Case 3 (#1266).
    if (llvm::isa<llvm::LoadInst>(val) && val->getType() == ptrTy_) {
        auto *meta = getMeta(val);
        if (meta) {
            const bool isArcContainerElem =
                meta->list_elem || meta->map_key ||
                meta->map_value || meta->set_elem;
            const bool isStrElem = meta->str_elem;
            if (isArcContainerElem || isStrElem) {
                auto *hdr = isStrElem ? emitStrGetHeaderFromData(val)
                                      : emitArcGetHeaderFromData(val);
                emitArcRetain(hdr, isArcAtomic(val));
                return true;
            }
        }
    }
    return false;
}

// Return-only retain for fn-typed param values. Fn-typed param allocas are
// not registered in arc_managed_vars_ (callers own the uniform-closure wrap
// temp via releaseUniformClosureTemps). When `return f` propagates such a
// value out of the callee, the caller's post-call temp release would
// otherwise free the storage while the returned handle is still in flight.
// Scoping the retain to ReturnStmt avoids unbalanced retains at non-return
// LoadInst sites (e.g. pass-through fn args, where wrapFnTypedArgs already
// skips the wrap and the temp set, so a retain at the load would leak).
// (#1770)
void CodeGen::retainFnTypedParamForReturn(llvm::Value *val) {
    auto *load = llvm::dyn_cast<llvm::LoadInst>(val);
    if (!load)
        return;
    auto *srcAlloca =
        llvm::dyn_cast<llvm::AllocaInst>(load->getPointerOperand());
    if (!srcAlloca)
        return;
    auto *meta = getMeta(srcAlloca);
    if (!meta || !meta->fn_type_info ||
        !meta->fn_type_info->isUniformClosure)
        return;
    auto *hdr = emitArcGetHeaderFromData(val);
    emitArcRetain(hdr, isArcAtomic(val));
}

llvm::FunctionCallee CodeGen::getOrCreateCollectionDestructor(CollectionKind kind,
                                                               const std::string &elemSig,
                                                               const std::string &valSig) {
    auto cacheKey = std::make_tuple(kind, elemSig, valSig);
    auto it = arc_destructors_cache_.find(cacheKey);
    if (it != arc_destructors_cache_.end())
        return it->second;

    // Destructor signature: void(ptr dataPtr)
    // dataPtr points to the type-specific header (after ARC header)
    auto *dtorTy = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
    std::string name;

    switch (kind) {
    case CollectionKind::List:
        name = "__ry_arc_dtor_list";
        if (elemSig == "str") name += "_str";
        break;
    case CollectionKind::Map:
        name = "__ry_arc_dtor_map";
        if (elemSig == "str") name += "_kstr";
        if (valSig == "str") name += "_vstr";
        break;
    case CollectionKind::Set:
        name = "__ry_arc_dtor_set";
        if (elemSig == "str") name += "_str";
        break;
    case CollectionKind::Str:
        llvm_unreachable("getOrCreateCollectionDestructor called with CollectionKind::Str");
    }

    auto *dtorFn = llvm::Function::Create(
        dtorTy, llvm::Function::InternalLinkage, name, mod_.get());
    dtorFn->addFnAttr(llvm::Attribute::NoUnwind);

    auto *entryBB = createBBInFn("entry", dtorFn);

    // Save current builder state
    auto *savedBB = builder_.GetInsertBlock();
    auto savedPt = builder_.GetInsertPoint();
    builder_.SetInsertPoint(entryBB);

    auto *dataPtr = dtorFn->getArg(0);
    auto freeFn = getStdlibFree();

    // Helper: ARC-release each collection element in a dense ptr-array
    // [0, len) via the inner destructor.  Split from `emitStrElemLoop`
    // (below) because str uses a different header layout (StringHeader
    // at elem - 24) and needs no inner destructor. (#1242)
    auto emitCollectionElemLoop = [&](llvm::Value *arrayPtr, llvm::Value *len,
                                       const char *tag,
                                       CollectionKind innerKind,
                                       const std::string &innerElemSig,
                                       const std::string &innerValSig) {
        auto *loopHdrBB  = llvm::BasicBlock::Create(*ctx_,
            std::string("dtor_clhdr_") + tag, dtorFn);
        auto *loopBodyBB = llvm::BasicBlock::Create(*ctx_,
            std::string("dtor_clbody_") + tag, dtorFn);
        auto *doRelBB    = llvm::BasicBlock::Create(*ctx_,
            std::string("dtor_cldorel_") + tag, dtorFn);
        auto *latchBB    = llvm::BasicBlock::Create(*ctx_,
            std::string("dtor_cllatch_") + tag, dtorFn);
        auto *postBB     = llvm::BasicBlock::Create(*ctx_,
            std::string("dtor_clpost_") + tag, dtorFn);

        auto *prevBB = builder_.GetInsertBlock();
        emitBranchUncond(loopHdrBB);

        builder_.SetInsertPoint(loopHdrBB);
        auto *iPhi = builder_.CreatePHI(i64Ty_, 2,
            std::string("dtor_ci_") + tag);
        iPhi->addIncoming(llvm::ConstantInt::get(i64Ty_, 0), prevBB);
        auto *done = builder_.CreateICmpEQ(iPhi, len,
            std::string("dtor_cdone_") + tag);
        emitBranchCond(done, postBB, loopBodyBB);

        builder_.SetInsertPoint(loopBodyBB);
        auto *elemGEP = builder_.CreateGEP(ptrTy_, arrayPtr, {iPhi},
            std::string("dtor_cegep_") + tag);
        auto *elem = builder_.CreateLoad(ptrTy_, elemGEP,
            std::string("dtor_celem_") + tag);
        auto *isNull = builder_.CreateICmpEQ(elem,
            llvm::ConstantPointerNull::get(
                llvm::cast<llvm::PointerType>(ptrTy_)),
            std::string("dtor_cnull_") + tag);
        emitBranchCond(isNull, latchBB, doRelBB);

        builder_.SetInsertPoint(doRelBB);
        auto *hdr = emitArcGetHeaderFromData(elem);
        auto innerDtor = getOrCreateCollectionDestructor(innerKind, innerElemSig, innerValSig);
        emitArcRelease(hdr, /*atomic=*/false, innerDtor, nullptr);
        // emitArcRelease leaves builder_ in its doneBB
        emitBranchUncond(latchBB);

        builder_.SetInsertPoint(latchBB);
        auto *iNext = builder_.CreateAdd(iPhi,
            llvm::ConstantInt::get(i64Ty_, 1),
            std::string("dtor_cinext_") + tag);
        iPhi->addIncoming(iNext, latchBB);
        emitBranchUncond(loopHdrBB);

        builder_.SetInsertPoint(postBB);
    };

    // Helper: parse `sig` (e.g. "List<int>", "Map<str,int>", "Set<List<int>>")
    // into inner kind + inner elem/val sigs and emit a collection release
    // loop, or fall through for non-ARC-managed element types.  str element
    // types dispatch to `emitStrElemLoop` (below) instead.
    auto emitInnerReleaseLoop = [&](llvm::Value *arrayPtr, llvm::Value *len,
                                     const char *tag,
                                     const std::string &sig) -> bool {
        if (sig.empty()) return false;
        std::string resolved = resolveTypeAlias(sig);
        if (resolved.size() >= 2 && resolved.front() == '(' &&
            resolved.back() == ')') {
            auto *tupleTy = llvm::dyn_cast<llvm::StructType>(resolveType(resolved));
            if (!tupleTy) return false;
            emitTupleElemReleaseLoop(arrayPtr, len, tag, resolved, tupleTy);
            return true;
        }
        CollectionKind innerKind;
        if (!fieldTypeIsArcManaged(resolved, &innerKind)) return false;
        if (innerKind == CollectionKind::Str) return false;  // str path handled by caller

        std::string innerElemSig;
        std::string innerValSig;
        std::string head;
        std::vector<std::string> innerArgs;
        if (ry::util::splitGenericTypeName(resolved, head, innerArgs)) {
            if ((innerKind == CollectionKind::List || innerKind == CollectionKind::Set) &&
                !innerArgs.empty()) {
                innerElemSig = resolveTypeAlias(innerArgs[0]);
            } else if (innerKind == CollectionKind::Map && innerArgs.size() >= 2) {
                innerElemSig = resolveTypeAlias(innerArgs[0]);
                innerValSig  = resolveTypeAlias(innerArgs[1]);
            }
        }
        emitCollectionElemLoop(arrayPtr, len, tag, innerKind, innerElemSig, innerValSig);
        return true;
    };

    // Helper: emit a counted loop that ARC-releases each str element in a
    // dense ptr-array [0, len).  After the call builder_ is in post_loop BB.
    auto emitStrElemLoop = [&](llvm::Value *arrayPtr, llvm::Value *len,
                                const char *tag) {
        auto *loopHdrBB  = llvm::BasicBlock::Create(*ctx_,
            std::string("dtor_lhdr_") + tag, dtorFn);
        auto *loopBodyBB = llvm::BasicBlock::Create(*ctx_,
            std::string("dtor_lbody_") + tag, dtorFn);
        auto *doRelBB    = llvm::BasicBlock::Create(*ctx_,
            std::string("dtor_dorel_") + tag, dtorFn);
        auto *latchBB    = llvm::BasicBlock::Create(*ctx_,
            std::string("dtor_latch_") + tag, dtorFn);
        auto *postBB     = llvm::BasicBlock::Create(*ctx_,
            std::string("dtor_post_") + tag, dtorFn);

        auto *prevBB = builder_.GetInsertBlock();
        emitBranchUncond(loopHdrBB);

        // loop header: phi i=0/i_next, exit when i == len
        builder_.SetInsertPoint(loopHdrBB);
        auto *iPhi = builder_.CreatePHI(i64Ty_, 2,
            std::string("dtor_i_") + tag);
        iPhi->addIncoming(llvm::ConstantInt::get(i64Ty_, 0), prevBB);
        auto *done = builder_.CreateICmpEQ(iPhi, len,
            std::string("dtor_done_") + tag);
        emitBranchCond(done, postBB, loopBodyBB);

        // loop body: load element, skip if null
        builder_.SetInsertPoint(loopBodyBB);
        auto *elemGEP = builder_.CreateGEP(ptrTy_, arrayPtr, {iPhi},
            std::string("dtor_egep_") + tag);
        auto *elem = builder_.CreateLoad(ptrTy_, elemGEP,
            std::string("dtor_elem_") + tag);
        auto *isNull = builder_.CreateICmpEQ(elem,
            llvm::ConstantPointerNull::get(
                llvm::cast<llvm::PointerType>(ptrTy_)),
            std::string("dtor_null_") + tag);
        emitBranchCond(isNull, latchBB, doRelBB);

        // do_rel: ARC-release the str element
        builder_.SetInsertPoint(doRelBB);
        auto *hdr = emitStrGetHeaderFromData(elem);
        emitArcRelease(hdr, /*atomic=*/false, {}, nullptr);
        // emitArcRelease leaves builder_ in its doneBB
        emitBranchUncond(latchBB);

        // latch: increment i and loop back
        builder_.SetInsertPoint(latchBB);
        auto *iNext = builder_.CreateAdd(iPhi,
            llvm::ConstantInt::get(i64Ty_, 1),
            std::string("dtor_inext_") + tag);
        iPhi->addIncoming(iNext, latchBB);
        emitBranchUncond(loopHdrBB);

        builder_.SetInsertPoint(postBB);
    };

    switch (kind) {
    case CollectionKind::List: {
        // ListHeader { i64 len, i64 cap, ptr data }
        auto *lenField = builder_.CreateStructGEP(listHeaderTy_, dataPtr, 0, "dtor_len_f");
        auto *len = builder_.CreateLoad(i64Ty_, lenField, "dtor_len");
        auto *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, dataPtr, 2, "dtor_data_field");
        auto *dataBuf = builder_.CreateLoad(ptrTy_, dataPtrField, "dtor_data_buf");
        if (elemSig == "str")
            emitStrElemLoop(dataBuf, len, "lst");
        else
            emitInnerReleaseLoop(dataBuf, len, "lst", elemSig);
        builder_.CreateCall(freeFn, {dataBuf});
        break;
    }
    case CollectionKind::Map: {
        // MapHeader { i64 len, i64 cap, ptr keys, ptr vals, i64 bucket_count, ptr buckets }
        auto *lenField = builder_.CreateStructGEP(mapHeaderTy_, dataPtr, 0, "dtor_len_f");
        auto *len = builder_.CreateLoad(i64Ty_, lenField, "dtor_len");

        auto *keysField = builder_.CreateStructGEP(mapHeaderTy_, dataPtr, 2, "dtor_keys_field");
        auto *keys = builder_.CreateLoad(ptrTy_, keysField, "dtor_keys");
        if (elemSig == "str")
            emitStrElemLoop(keys, len, "mkey");
        else
            emitInnerReleaseLoop(keys, len, "mkey", elemSig);
        builder_.CreateCall(freeFn, {keys});

        auto *valsField = builder_.CreateStructGEP(mapHeaderTy_, dataPtr, 3, "dtor_vals_field");
        auto *vals = builder_.CreateLoad(ptrTy_, valsField, "dtor_vals");
        if (valSig == "str")
            emitStrElemLoop(vals, len, "mval");
        else
            emitInnerReleaseLoop(vals, len, "mval", valSig);
        builder_.CreateCall(freeFn, {vals});

        auto *bucketsField = builder_.CreateStructGEP(mapHeaderTy_, dataPtr, 5, "dtor_buckets_field");
        auto *buckets = builder_.CreateLoad(ptrTy_, bucketsField, "dtor_buckets");
        builder_.CreateCall(freeFn, {buckets});
        break;
    }
    case CollectionKind::Set: {
        // SetHeader { i64 len, i64 cap, ptr elems, i64 bucket_count, ptr buckets }
        auto *lenField = builder_.CreateStructGEP(setHeaderTy_, dataPtr, 0, "dtor_len_f");
        auto *len = builder_.CreateLoad(i64Ty_, lenField, "dtor_len");

        auto *elemsField = builder_.CreateStructGEP(setHeaderTy_, dataPtr, 2, "dtor_elems_field");
        auto *elems = builder_.CreateLoad(ptrTy_, elemsField, "dtor_elems");
        if (elemSig == "str")
            emitStrElemLoop(elems, len, "set");
        else
            emitInnerReleaseLoop(elems, len, "set", elemSig);
        builder_.CreateCall(freeFn, {elems});

        auto *bucketsField = builder_.CreateStructGEP(setHeaderTy_, dataPtr, 4, "dtor_buckets_field");
        auto *buckets = builder_.CreateLoad(ptrTy_, bucketsField, "dtor_buckets");
        builder_.CreateCall(freeFn, {buckets});
        break;
    }
    case CollectionKind::Str:
        llvm_unreachable("getOrCreateCollectionDestructor: Str has no collection destructor");
    }

    builder_.CreateRetVoid();

    // Restore builder state
    if (savedBB)
        builder_.SetInsertPoint(savedBB, savedPt);

    llvm::FunctionCallee callee(dtorTy, dtorFn);
    arc_destructors_cache_[cacheKey] = callee;
    return callee;
}

// ====== Tuple element ARC helpers (#1667) ======
//
// Tuple-element lists (`List<(K, V)>` produced by enumerate/zip/items)
// hold an inline array of tuple struct values, NOT pointers. Per-component
// retain/release dispatch happens by source-level type name so we never
// route a non-ARC component through `fieldTypeIsArcManaged` (which would
// pull in pointer-array predicates that mis-handle inline struct slots).

void CodeGen::emitTupleComponentRetain(llvm::Value *val,
                                        const std::string &fSig) {
    if (fSig.empty() || !val) return;
    const std::string resolved = resolveTypeAlias(fSig);
    if (resolved.empty() || ry::util::isWeakTypeName(resolved)) return;
    if (resolved == "str") {
        emitArcRetain(emitStrGetHeaderFromData(val), isArcAtomic(val));
    } else if (ry::util::isListTypeName(resolved) || ry::util::isMapTypeName(resolved) ||
               ry::util::isSetTypeName(resolved)) {
        emitArcRetain(emitArcGetHeaderFromData(val), isArcAtomic(val));
    } else if (resolved.size() >= 2 && resolved.front() == '(' &&
                resolved.back() == ')') {
        // Nested tuple component (#1667 follow-up): val is an inline struct
        // value whose ARC children must be retained recursively, since the
        // outer tuple destructor now recurses via emitTupleElemReleaseLoop.
        // Skipping this would re-introduce the asymmetric leak/UAF for shapes
        // like enumerate(List<(List<int>, int)>) or items(Map<str, (List<int>, int)>).
        auto *tupleTy = llvm::dyn_cast<llvm::StructType>(val->getType());
        if (!tupleTy) return;
        auto components = splitTupleSig(resolved);
        const unsigned n = static_cast<unsigned>(
            std::min<size_t>(components.size(), tupleTy->getNumElements()));
        for (unsigned i = 0; i < n; ++i) {
            llvm::Value *subVal = builder_.CreateExtractValue(val, {i});
            emitTupleComponentRetain(subVal, components[i]);
        }
    }
    // int / bool / f64 / weak / record / unknown: no-op
}

void CodeGen::emitTupleComponentRetainTraced(llvm::Value *fieldVal,
                                              llvm::Value *sourceAgg,
                                              unsigned topIdx,
                                              const std::string &fSig) {
    if (fSig.empty() || !fieldVal) return;
    const std::string resolved = resolveTypeAlias(fSig);
    if (resolved.empty() || ry::util::isWeakTypeName(resolved)) return;

    // Recover the original SSA value at `topIdx` of `sourceAgg`. When
    // sourceAgg is an InsertValueInst chain (the freshly-built tuple in
    // `xs[i] = (a, b)`), `traceInsertValueField` returns the operand
    // inserted at that index — which may itself be an InsertValue for a
    // nested tuple shape like `(0, (a, b))`. Pass that traced value down
    // as the new sourceAgg so the recursion can keep tracing into deeper
    // levels until it reaches a non-InsertValue leaf.
    llvm::Value *tracedField = nullptr;
    if (sourceAgg && llvm::isa<llvm::InsertValueInst>(sourceAgg))
        tracedField = traceInsertValueField(sourceAgg, topIdx);

    if (resolved.size() >= 2 && resolved.front() == '(' &&
        resolved.back() == ')') {
        // Nested tuple component: recurse into each sub-component, carrying
        // the traced sub-aggregate forward so leaf-level ownership checks
        // see the original SSA value rather than the fresh ExtractValue.
        auto *tupleTy = llvm::dyn_cast<llvm::StructType>(fieldVal->getType());
        if (!tupleTy) return;
        auto components = splitTupleSig(resolved);
        const unsigned n = static_cast<unsigned>(
            std::min<size_t>(components.size(), tupleTy->getNumElements()));
        for (unsigned i = 0; i < n; ++i) {
            llvm::Value *subVal = builder_.CreateExtractValue(fieldVal, {i});
            emitTupleComponentRetainTraced(subVal, tracedField, i,
                                            components[i]);
        }
        return;
    }

    // Leaf component (str / List / Map / Set / primitive). Check ownership
    // on the traced source value — `arc_owned_values_` /
    // `arc_str_owned_values_` are keyed by the original SSA value, not by
    // an ExtractValue produced post-hoc.
    llvm::Value *checkVal = tracedField ? tracedField : fieldVal;
    if (arc_owned_values_.count(checkVal) ||
        arc_str_owned_values_.count(checkVal))
        return;
    emitTupleComponentRetain(fieldVal, resolved);
}

namespace {
// Build a counted loop over [0, len) inside `fn` that branches into
// `bodyEmitter(iPhi)` for each iteration. The body emitter is responsible
// for branching back to the latch (the lambda receives the latch BB so it
// can fall through naturally). Both `pre` and `post` BBs are returned via
// the builder cursor (post is current after the call).
struct CountedLoopBlocks {
    llvm::BasicBlock *body;
    llvm::BasicBlock *latch;
    llvm::BasicBlock *post;
    llvm::PHINode    *iPhi;
};
} // anonymous

static CountedLoopBlocks emitCountedLoopShell(llvm::IRBuilder<> &b,
                                               llvm::LLVMContext &ctx,
                                               llvm::Function *fn,
                                               llvm::Value *len,
                                               llvm::Type *i64Ty,
                                               const std::string &tagPrefix) {
    auto *hdr   = llvm::BasicBlock::Create(ctx, tagPrefix + "_hdr",   fn);
    auto *body  = llvm::BasicBlock::Create(ctx, tagPrefix + "_body",  fn);
    auto *latch = llvm::BasicBlock::Create(ctx, tagPrefix + "_latch", fn);
    auto *post  = llvm::BasicBlock::Create(ctx, tagPrefix + "_post",  fn);

    auto *prevBB = b.GetInsertBlock();
    b.CreateBr(hdr);

    b.SetInsertPoint(hdr);
    auto *iPhi = b.CreatePHI(i64Ty, 2, tagPrefix + "_i");
    iPhi->addIncoming(llvm::ConstantInt::get(i64Ty, 0), prevBB);
    auto *done = b.CreateICmpEQ(iPhi, len, tagPrefix + "_done");
    b.CreateCondBr(done, post, body);

    b.SetInsertPoint(latch);
    auto *iNext = b.CreateAdd(iPhi, llvm::ConstantInt::get(i64Ty, 1),
                               tagPrefix + "_inext");
    iPhi->addIncoming(iNext, latch);
    b.CreateBr(hdr);

    b.SetInsertPoint(body);
    return {body, latch, post, iPhi};
}

void CodeGen::emitTupleElemReleaseSlot(llvm::Value *slotPtr,
                                        const char *tagPrefix,
                                        const std::string &tupleSig,
                                        llvm::StructType *tupleTy) {
    if (!slotPtr || !tupleTy) return;
    std::vector<std::string> components = splitTupleSig(tupleSig);
    if (components.empty()) return;

    llvm::Function *fn = builder_.GetInsertBlock()->getParent();
    const unsigned n = static_cast<unsigned>(
        std::min<size_t>(components.size(), tupleTy->getNumElements()));
    for (unsigned i = 0; i < n; ++i) {
        const std::string fSig = resolveTypeAlias(components[i]);
        if (fSig.empty() || ry::util::isWeakTypeName(fSig)) continue;

        const bool isStr  = (fSig == "str");
        const bool isColl = ry::util::isListTypeName(fSig) || ry::util::isMapTypeName(fSig) ||
                            ry::util::isSetTypeName(fSig);
        const bool isTup  = (fSig.size() >= 2 && fSig.front() == '(' &&
                              fSig.back() == ')');
        if (!isStr && !isColl && !isTup) continue;

        auto *fieldGEP = builder_.CreateStructGEP(tupleTy, slotPtr, i,
            std::string(tagPrefix) + "_f" + std::to_string(i));

        if (isTup) {
            // Recursive tuple component: load nothing (inline struct), recurse
            // with the field's own StructType.
            auto *nestedTy = llvm::dyn_cast<llvm::StructType>(
                tupleTy->getElementType(i));
            if (!nestedTy) continue;
            // Treat a single nested tuple slot as a 1-element array.
            llvm::Value *one = llvm::ConstantInt::get(i64Ty_, 1);
            std::string subTag = std::string(tagPrefix) + "_n" +
                                  std::to_string(i);
            emitTupleElemReleaseLoop(fieldGEP, one, subTag.c_str(), fSig,
                                       nestedTy);
            continue;
        }

        // ARC pointer component (str / List / Map / Set): load, null-guard,
        // release with the appropriate header offset.
        auto *val = builder_.CreateLoad(ptrTy_, fieldGEP,
            std::string(tagPrefix) + "_v" + std::to_string(i));
        auto *nullBB = llvm::BasicBlock::Create(*ctx_,
            std::string(tagPrefix) + "_skip" + std::to_string(i), fn);
        auto *relBB  = llvm::BasicBlock::Create(*ctx_,
            std::string(tagPrefix) + "_rel"  + std::to_string(i), fn);
        auto *isNull = builder_.CreateICmpEQ(val,
            llvm::ConstantPointerNull::get(
                llvm::cast<llvm::PointerType>(ptrTy_)),
            std::string(tagPrefix) + "_isnull" + std::to_string(i));
        emitBranchCond(isNull, nullBB, relBB);

        builder_.SetInsertPoint(relBB);
        if (isStr) {
            emitArcRelease(emitStrGetHeaderFromData(val),
                            /*atomic=*/false, {}, nullptr);
        } else {
            // Resolve inner destructor for nested collection.
            std::string head;
            std::vector<std::string> innerArgs;
            CollectionKind innerKind = CollectionKind::List;
            std::string innerElemSig, innerValSig;
            if (ry::util::isListTypeName(fSig)) innerKind = CollectionKind::List;
            else if (ry::util::isMapTypeName(fSig)) innerKind = CollectionKind::Map;
            else if (ry::util::isSetTypeName(fSig)) innerKind = CollectionKind::Set;
            if (ry::util::splitGenericTypeName(fSig, head, innerArgs)) {
                if ((innerKind == CollectionKind::List ||
                     innerKind == CollectionKind::Set) &&
                    !innerArgs.empty()) {
                    innerElemSig = resolveTypeAlias(innerArgs[0]);
                } else if (innerKind == CollectionKind::Map &&
                            innerArgs.size() >= 2) {
                    innerElemSig = resolveTypeAlias(innerArgs[0]);
                    innerValSig  = resolveTypeAlias(innerArgs[1]);
                }
            }
            auto innerDtor = getOrCreateCollectionDestructor(
                innerKind, innerElemSig, innerValSig);
            emitArcRelease(emitArcGetHeaderFromData(val),
                            /*atomic=*/false, innerDtor, nullptr);
        }
        // emitArcRelease leaves cursor in its own done BB.
        emitBranchUncond(nullBB);

        builder_.SetInsertPoint(nullBB);
    }
}

void CodeGen::emitTupleElemReleaseLoop(llvm::Value *arrayPtr, llvm::Value *len,
                                        const char *tag,
                                        const std::string &tupleSig,
                                        llvm::StructType *tupleTy) {
    if (!arrayPtr || !len || !tupleTy) return;
    std::vector<std::string> components = splitTupleSig(tupleSig);
    if (components.empty()) return;

    // Skip the loop entirely if no component needs ARC release.
    bool anyArc = false;
    for (const auto &c : components) {
        const std::string r = resolveTypeAlias(c);
        if (r.empty() || ry::util::isWeakTypeName(r)) continue;
        if (r == "str" || ry::util::isListTypeName(r) || ry::util::isMapTypeName(r) ||
            ry::util::isSetTypeName(r) ||
            (r.size() >= 2 && r.front() == '(' && r.back() == ')')) {
            anyArc = true;
            break;
        }
    }
    if (!anyArc) return;

    llvm::Function *fn = builder_.GetInsertBlock()->getParent();
    std::string tagPrefix = std::string("dtor_tup_") + tag;
    auto loop = emitCountedLoopShell(builder_, *ctx_, fn, len, i64Ty_,
                                       tagPrefix);

    // body: GEP into the i'th tuple slot, then release each component.
    auto *slotPtr = builder_.CreateGEP(tupleTy, arrayPtr, {loop.iPhi},
                                         tagPrefix + "_slot");
    emitTupleElemReleaseSlot(slotPtr, tagPrefix.c_str(), tupleSig, tupleTy);

    // After all components, branch to the loop latch.
    emitBranchUncond(loop.latch);
    builder_.SetInsertPoint(loop.post);
}

void CodeGen::emitTupleElemRetainLoop(llvm::Value *arrayPtr, llvm::Value *len,
                                       const char *tag,
                                       const std::string &tupleSig,
                                       llvm::StructType *tupleTy) {
    if (!arrayPtr || !len || !tupleTy) return;
    std::vector<std::string> components = splitTupleSig(tupleSig);
    if (components.empty()) return;

    bool anyArc = false;
    for (const auto &c : components) {
        const std::string r = resolveTypeAlias(c);
        if (r.empty() || ry::util::isWeakTypeName(r)) continue;
        if (r == "str" || ry::util::isListTypeName(r) || ry::util::isMapTypeName(r) ||
            ry::util::isSetTypeName(r) ||
            (r.size() >= 2 && r.front() == '(' && r.back() == ')')) {
            anyArc = true;
            break;
        }
    }
    if (!anyArc) return;

    llvm::Function *fn = builder_.GetInsertBlock()->getParent();
    std::string tagPrefix = std::string("ret_tup_") + tag;
    auto loop = emitCountedLoopShell(builder_, *ctx_, fn, len, i64Ty_,
                                       tagPrefix);

    auto *slotPtr = builder_.CreateGEP(tupleTy, arrayPtr, {loop.iPhi},
                                         tagPrefix + "_slot");
    const unsigned n = static_cast<unsigned>(
        std::min<size_t>(components.size(), tupleTy->getNumElements()));
    for (unsigned i = 0; i < n; ++i) {
        const std::string fSig = resolveTypeAlias(components[i]);
        if (fSig.empty() || ry::util::isWeakTypeName(fSig)) continue;

        const bool isStr  = (fSig == "str");
        const bool isColl = ry::util::isListTypeName(fSig) || ry::util::isMapTypeName(fSig) ||
                            ry::util::isSetTypeName(fSig);
        const bool isTup  = (fSig.size() >= 2 && fSig.front() == '(' &&
                              fSig.back() == ')');
        if (!isStr && !isColl && !isTup) continue;

        auto *fieldGEP = builder_.CreateStructGEP(tupleTy, slotPtr, i,
            tagPrefix + "_f" + std::to_string(i));

        if (isTup) {
            auto *nestedTy = llvm::dyn_cast<llvm::StructType>(
                tupleTy->getElementType(i));
            if (!nestedTy) continue;
            llvm::Value *one = llvm::ConstantInt::get(i64Ty_, 1);
            std::string subTag = std::string(tag) + "_n" + std::to_string(i);
            emitTupleElemRetainLoop(fieldGEP, one, subTag.c_str(), fSig,
                                     nestedTy);
            continue;
        }

        auto *val = builder_.CreateLoad(ptrTy_, fieldGEP,
            tagPrefix + "_v" + std::to_string(i));
        auto *nullBB = llvm::BasicBlock::Create(*ctx_,
            tagPrefix + "_skip" + std::to_string(i), fn);
        auto *retBB  = llvm::BasicBlock::Create(*ctx_,
            tagPrefix + "_do"   + std::to_string(i), fn);
        auto *isNull = builder_.CreateICmpEQ(val,
            llvm::ConstantPointerNull::get(
                llvm::cast<llvm::PointerType>(ptrTy_)),
            tagPrefix + "_isnull" + std::to_string(i));
        emitBranchCond(isNull, nullBB, retBB);

        builder_.SetInsertPoint(retBB);
        if (isStr) {
            emitArcRetain(emitStrGetHeaderFromData(val), isArcAtomic(val));
        } else {
            emitArcRetain(emitArcGetHeaderFromData(val), isArcAtomic(val));
        }
        emitBranchUncond(nullBB);

        builder_.SetInsertPoint(nullBB);
    }

    emitBranchUncond(loop.latch);
    builder_.SetInsertPoint(loop.post);
}

// ===== Record-in-any heap-box helpers (#1797) =====
//
// Each record type stored in `any` needs a static descriptor global that
// drives release / equality / to_string dispatch when the static type is
// lost at function boundaries. The descriptor is laid out as
//   { ptr dtor, ptr eq, ptr type_name }  (24 bytes, see RyRecordDescriptor)
// and lives at offset 0 of the box's data region; the inner record struct
// starts at +8. The runtime trampoline `__ry_arc_dtor_record_dispatch`
// is what `emitArcRelease` actually invokes; it loads the descriptor and
// fans out to the per-type LLVM dtor in this cache.

llvm::StructType *CodeGen::recordBoxLayoutType(llvm::StructType *recordStructTy) {
    return llvm::StructType::get(*ctx_, {ptrTy_, recordStructTy});
}

llvm::Function *CodeGen::getOrCreateRecordBoxDtor(const std::string &typeName,
                                                    llvm::StructType *st) {
    if (auto it = record_dtor_cache_.find(typeName); it != record_dtor_cache_.end())
        return it->second;

    FnScope scope(*this);
    auto *fnTy = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
    auto *fn = llvm::Function::Create(fnTy, llvm::Function::InternalLinkage,
                                       "__ry_record_box_dtor_" + typeName, mod_.get());
    record_dtor_cache_[typeName] = fn;

    auto *entry = createBBInFn("entry", fn);
    builder_.SetInsertPoint(entry);
    auto *dataPtr = fn->getArg(0);
    auto *layoutTy = recordBoxLayoutType(st);
    auto *fieldsPtr = builder_.CreateStructGEP(layoutTy, dataPtr, 1, "fields_ptr");
    auto *recordVal = builder_.CreateLoad(st, fieldsPtr, "record_val");
    emitRecordArcFieldsRelease(recordVal, st);
    builder_.CreateRetVoid();
    return fn;
}

llvm::Function *CodeGen::getOrCreateRecordBoxEq(const std::string &typeName,
                                                  llvm::StructType *st) {
    if (auto it = record_eq_cache_.find(typeName); it != record_eq_cache_.end())
        return it->second;

    auto recIt = record_types_.find(typeName);
    if (recIt == record_types_.end())
        codegenError("record-in-any: unknown record type '" + typeName + "'");
    const RecordInfo &info = recIt->second;

    FnScope scope(*this);
    auto *fnTy = llvm::FunctionType::get(i64Ty_, {ptrTy_, ptrTy_}, false);
    auto *fn = llvm::Function::Create(fnTy, llvm::Function::InternalLinkage,
                                       "__ry_record_box_eq_" + typeName, mod_.get());
    record_eq_cache_[typeName] = fn;

    auto *entry = createBBInFn("entry", fn);
    builder_.SetInsertPoint(entry);
    // Args point at the record-struct region inside the box (i.e. data + 8).
    auto *recA = builder_.CreateLoad(st, fn->getArg(0), "rec.a");
    auto *recB = builder_.CreateLoad(st, fn->getArg(1), "rec.b");
    llvm::Value *eq = emitRecordComparison("==", recA, recB, info);
    builder_.CreateRet(builder_.CreateZExt(eq, i64Ty_, "rec.eq.i64"));
    return fn;
}

llvm::GlobalVariable *
CodeGen::getOrCreateRecordDescriptor(const std::string &typeName, llvm::StructType *st) {
    if (auto it = record_descriptor_cache_.find(typeName);
        it != record_descriptor_cache_.end())
        return it->second;

    // Body emission depends on the active IRBuilder insertion point. Snapshot
    // before recursing so the caller's emission point survives the per-type
    // dtor / eq body generation below.
    auto *savedBB = builder_.GetInsertBlock();
    llvm::IRBuilder<>::InsertPoint savedIP = builder_.saveIP();

    auto *dtor = getOrCreateRecordBoxDtor(typeName, st);
    auto *eq = getOrCreateRecordBoxEq(typeName, st);
    auto *typeNameConst = cachedGlobalString(typeName, ".record_type_name");

    // Resolve parent descriptor (or null) so `unwrapFromAny` can walk the
    // chain to admit `let a: Parent = anyHoldingChild` (#1802).
    llvm::Constant *parentDesc =
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));
    if (const RecordInfo *info = findRecordType(typeName);
        info && !info->parentName.empty()) {
        if (const RecordInfo *parentInfo = findRecordType(info->parentName);
            parentInfo && parentInfo->llvmType) {
            parentDesc = getOrCreateRecordDescriptor(info->parentName,
                                                     parentInfo->llvmType);
        }
    }

    auto *descTy = llvm::StructType::get(*ctx_, {ptrTy_, ptrTy_, ptrTy_, ptrTy_});
    auto *initVal = llvm::ConstantStruct::get(
        descTy,
        {dtor, eq, llvm::cast<llvm::Constant>(typeNameConst), parentDesc});
    auto *gv = new llvm::GlobalVariable(
        *mod_, descTy, /*isConstant=*/true,
        llvm::GlobalValue::PrivateLinkage, initVal,
        "__ry_record_desc_" + typeName);
    gv->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    gv->setAlignment(llvm::Align(8));
    record_descriptor_cache_[typeName] = gv;

    if (savedBB)
        builder_.restoreIP(savedIP);
    return gv;
}

// =====================================================================
// Enum-in-`any` heap-box helpers (#1798).
// =====================================================================
//
// Enums (simple, ADT, `Option<T>`, `Result<V, E>`) cannot fit in the
// 8-byte `data[8]` slot of `any` as SSA values, so we heap-box them with
// the same descriptor-driven layout as records (#1797):
//
//   [ ArcHeader (16B) ][ descriptor ptr (8B) ][ payload ]
//
// Payload type is per enum kind:
//   - simple enum: i64
//   - ADT enum: `{ i64 disc, [N x i8] payload }`
//   - Option<T>: `{ i1 has_value, T inner }`
//   - Result<V,E>: `{ i1 is_ok, V ok, E err }`
//
// The descriptor `{ dtor, eq, type_name }` (3 ptrs = 24B; no parent_desc
// since enums have no inheritance) is stored at box+0 so the runtime
// trampoline `__ry_arc_dtor_enum_dispatch` can reach the right dtor and
// `__ry_any_eq` can deep-compare by descriptor identity + per-type eq.

llvm::StructType *CodeGen::enumBoxLayoutType(llvm::Type *payloadTy) {
    return llvm::StructType::get(*ctx_, {ptrTy_, payloadTy});
}

bool CodeGen::isSimpleEnumTypeName(const std::string &name) {
    const std::string resolved = const_cast<CodeGen *>(this)->resolveTypeAlias(name);
    const EnumInfo *info = findEnumType(resolved);
    return info && !info->isADT;
}

std::string CodeGen::findEnumLikeTypeNameForBoxing(llvm::Value *val) {
    if (!val) return {};
    llvm::Type *ty = val->getType();
    if (auto *st = llvm::dyn_cast<llvm::StructType>(ty)) {
        // ADT enum payload struct.
        std::string adtName = findAdtEnumName(st);
        if (!adtName.empty())
            return adtName;
        // Resolve source-level type name from val's metadata. When val is a
        // direct LoadInst from an alloca declared with a source-level annotation
        // (e.g. `r: Result<List<int>, str> = ...`), fall back to the alloca's
        // metadata — `source_type_name` from `propagateMeta` may not have flowed
        // through the Load because Result/Option-typed allocas hold the meta on
        // the storage slot, not on each loaded SSA value (#1798).
        auto resolveSourceTypeName = [&]() -> std::string {
            if (auto *meta = getMeta(val); meta && !meta->source_type_name.empty())
                return meta->source_type_name;
            if (auto *li = llvm::dyn_cast<llvm::LoadInst>(val)) {
                llvm::Value *ptr = li->getPointerOperand();
                if (auto *aMeta = getMeta(ptr); aMeta && !aMeta->source_type_name.empty())
                    return aMeta->source_type_name;
            }
            return {};
        };
        // Option<T>
        if (auto it = reverse_option_types_.find(st); it != reverse_option_types_.end()) {
            std::string innerName;
            std::string src = resolveSourceTypeName();
            if (!src.empty()) {
                if (src.size() > 8 && src.compare(0, 7, "Option<") == 0 &&
                    src.back() == '>')
                    innerName = src.substr(7, src.size() - 8);
                else if (!src.empty() && src.back() == '?')
                    innerName = src.substr(0, src.size() - 1);
            }
            if (innerName.empty())
                innerName = reverseResolveTypeName(it->second);
            return "Option<" + innerName + ">";
        }
        // Result<V, E>
        if (auto it = reverse_result_types_.find(st); it != reverse_result_types_.end()) {
            std::string okName, errName;
            std::string src = resolveSourceTypeName();
            if (!src.empty()) {
                if (src.size() > 9 && src.compare(0, 7, "Result<") == 0 &&
                    src.back() == '>') {
                    // Split on the top-level comma.
                    std::string inside = src.substr(7, src.size() - 8);
                    int depth = 0;
                    size_t splitAt = std::string::npos;
                    for (size_t i = 0; i < inside.size(); ++i) {
                        char c = inside[i];
                        if (c == '<') ++depth;
                        else if (c == '>') --depth;
                        else if (c == ',' && depth == 0) {
                            splitAt = i;
                            break;
                        }
                    }
                    if (splitAt != std::string::npos) {
                        okName = inside.substr(0, splitAt);
                        errName = inside.substr(splitAt + 1);
                        // Trim
                        auto trim = [](std::string &s) {
                            while (!s.empty() && (s.front() == ' ' || s.front() == '\t')) s.erase(s.begin());
                            while (!s.empty() && (s.back() == ' ' || s.back() == '\t')) s.pop_back();
                        };
                        trim(okName);
                        trim(errName);
                    }
                }
            }
            if (okName.empty()) okName = reverseResolveTypeName(it->second.first);
            if (errName.empty()) errName = reverseResolveTypeName(it->second.second);
            return "Result<" + okName + ", " + errName + ">";
        }
        return {};
    }
    if (ty == i64Ty_) {
        if (auto *meta = getMeta(val); meta && !meta->enum_value_type.empty())
            return meta->enum_value_type;
    }
    return {};
}

// Helper: parse "Option<T>" or "Result<V, E>" canonical names; for ADT/simple
// enum names, both outputs stay empty.
static void splitEnumLikeName(const std::string &canonical,
                              std::string *outHead,
                              std::vector<std::string> *outArgs) {
    if (outHead) outHead->clear();
    if (outArgs) outArgs->clear();
    auto lt = canonical.find('<');
    if (lt == std::string::npos || canonical.back() != '>')
        return;
    if (outHead) *outHead = canonical.substr(0, lt);
    std::string inside = canonical.substr(lt + 1, canonical.size() - lt - 2);
    int depth = 0;
    std::string cur;
    for (char c : inside) {
        if (c == ',' && depth == 0) {
            // Trim leading/trailing spaces.
            while (!cur.empty() && (cur.front() == ' ' || cur.front() == '\t')) cur.erase(cur.begin());
            while (!cur.empty() && (cur.back() == ' ' || cur.back() == '\t')) cur.pop_back();
            if (outArgs) outArgs->push_back(cur);
            cur.clear();
            continue;
        }
        if (c == '<') ++depth;
        else if (c == '>') --depth;
        cur += c;
    }
    while (!cur.empty() && (cur.front() == ' ' || cur.front() == '\t')) cur.erase(cur.begin());
    while (!cur.empty() && (cur.back() == ' ' || cur.back() == '\t')) cur.pop_back();
    if (!cur.empty() && outArgs) outArgs->push_back(cur);
}

llvm::Function *CodeGen::getOrCreateEnumBoxDtor(const std::string &typeName,
                                                  llvm::Type *payloadTy) {
    if (auto it = enum_dtor_cache_.find(typeName); it != enum_dtor_cache_.end())
        return it->second;

    FnScope scope(*this);
    auto *fnTy = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
    auto *fn = llvm::Function::Create(fnTy, llvm::Function::InternalLinkage,
                                       "__ry_enum_box_dtor_" + typeName, mod_.get());
    enum_dtor_cache_[typeName] = fn;

    auto *entry = createBBInFn("entry", fn);
    builder_.SetInsertPoint(entry);
    auto *dataPtr = fn->getArg(0);
    auto *layoutTy = enumBoxLayoutType(payloadTy);
    auto *payloadPtr = builder_.CreateStructGEP(layoutTy, dataPtr, 1, "payload_ptr");

    std::string head;
    std::vector<std::string> args;
    splitEnumLikeName(typeName, &head, &args);

    if (head == "Option" && args.size() == 1) {
        // payload = {i1 has_value, T inner}
        auto *payloadSt = llvm::cast<llvm::StructType>(payloadTy);
        auto *payload = builder_.CreateLoad(payloadSt, payloadPtr, "opt.payload");
        CollectionKind innerKind;
        if (fieldTypeIsArcManaged(args[0], &innerKind)) {
            auto *tag = builder_.CreateExtractValue(payload, 0, "opt.has");
            auto *someBB = createBBInFn("opt.some.dtor", fn);
            auto *doneBB = createBBInFn("opt.done.dtor", fn);
            emitBranchCond(tag, someBB, doneBB);
            builder_.SetInsertPoint(someBB);
            auto *inner = builder_.CreateExtractValue(payload, 1, "opt.inner");
            emitArcReleaseLoadedElement(inner, innerKind, args[0], "opt.dtor");
            emitBranchUncond(doneBB);
            builder_.SetInsertPoint(doneBB);
        }
    } else if (head == "Result" && args.size() == 2) {
        auto *payloadSt = llvm::cast<llvm::StructType>(payloadTy);
        auto *payload = builder_.CreateLoad(payloadSt, payloadPtr, "res.payload");
        CollectionKind okKind, errKind;
        bool okArc = fieldTypeIsArcManaged(args[0], &okKind);
        bool errArc = fieldTypeIsArcManaged(args[1], &errKind);
        if (okArc || errArc) {
            auto *tag = builder_.CreateExtractValue(payload, 0, "res.is_ok");
            auto *okBB = createBBInFn("res.ok.dtor", fn);
            auto *errBB = createBBInFn("res.err.dtor", fn);
            auto *doneBB = createBBInFn("res.done.dtor", fn);
            emitBranchCond(tag, okBB, errBB);
            builder_.SetInsertPoint(okBB);
            if (okArc) {
                auto *okVal = builder_.CreateExtractValue(payload, 1, "res.ok_val");
                emitArcReleaseLoadedElement(okVal, okKind, args[0], "res.ok.dtor");
            }
            emitBranchUncond(doneBB);
            builder_.SetInsertPoint(errBB);
            if (errArc) {
                auto *errVal = builder_.CreateExtractValue(payload, 2, "res.err_val");
                emitArcReleaseLoadedElement(errVal, errKind, args[1], "res.err.dtor");
            }
            emitBranchUncond(doneBB);
            builder_.SetInsertPoint(doneBB);
        }
    } else {
        // Organic enum (simple or ADT).
        const EnumInfo *info = findEnumType(typeName);
        if (info && info->isADT && info->adtType && payloadTy == info->adtType) {
            // Switch on disc; release per-variant ARC fields.
            auto *adtSt = info->adtType;
            auto *tagPtr = builder_.CreateStructGEP(adtSt, payloadPtr, 0, "adt.tag.ptr");
            auto *tag = builder_.CreateLoad(i64Ty_, tagPtr, "adt.tag");
            auto *payloadBytesPtr = builder_.CreateStructGEP(adtSt, payloadPtr, 1,
                                                              "adt.payload.ptr");

            auto *doneBB = createBBInFn("adt.dtor.done", fn);
            auto *sw = builder_.CreateSwitch(tag, doneBB,
                                              static_cast<unsigned>(info->variantOrder.size()));

            const llvm::DataLayout &dl = mod_->getDataLayout();
            for (const std::string &vname : info->variantOrder) {
                auto vfIt = info->variantFields.find(vname);
                int64_t tagVal = info->variants.at(vname);
                bool hasArc = false;
                if (vfIt != info->variantFields.end()) {
                    for (const auto &ftn : vfIt->second.fieldTypeNames)
                        if (fieldTypeIsArcManaged(ftn)) { hasArc = true; break; }
                }
                auto *tagConst = llvm::cast<llvm::ConstantInt>(
                    llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(tagVal)));
                if (!hasArc) {
                    sw->addCase(tagConst, doneBB);
                    continue;
                }
                auto *caseBB = llvm::BasicBlock::Create(
                    *ctx_, "adt.dtor." + vname, fn);
                sw->addCase(tagConst, caseBB);
                builder_.SetInsertPoint(caseBB);
                const VariantFieldInfo &vfi = vfIt->second;
                size_t offset = 0;
                for (size_t fi = 0; fi < vfi.fieldTypes.size(); ++fi) {
                    llvm::Type *fieldTy = vfi.fieldTypes[fi];
                    const std::string &fieldTypeName = vfi.fieldTypeNames[fi];
                    uint64_t align = dl.getABITypeAlign(fieldTy).value();
                    if (align > 0)
                        offset = (offset + align - 1) / align * align;
                    CollectionKind fk;
                    if (fieldTypeIsArcManaged(fieldTypeName, &fk)) {
                        auto *fieldPtr = builder_.CreateGEP(
                            i8Ty_, payloadBytesPtr,
                            llvm::ConstantInt::get(i64Ty_, offset),
                            "adt.field.dtor." + std::to_string(fi));
                        auto *fieldVal = builder_.CreateLoad(fieldTy, fieldPtr,
                                                              "adt.field.val");
                        emitArcReleaseLoadedElement(fieldVal, fk, fieldTypeName,
                                                     "adt.dtor." + vname);
                    }
                    offset += dl.getTypeAllocSize(fieldTy);
                }
                emitBranchUncond(doneBB);
            }
            builder_.SetInsertPoint(doneBB);
        }
        // Simple enum (i64 payload): no-op.
    }
    builder_.CreateRetVoid();
    return fn;
}

llvm::Function *CodeGen::getOrCreateEnumBoxEq(const std::string &typeName,
                                                llvm::Type *payloadTy) {
    if (auto it = enum_eq_cache_.find(typeName); it != enum_eq_cache_.end())
        return it->second;

    FnScope scope(*this);
    auto *fnTy = llvm::FunctionType::get(i64Ty_, {ptrTy_, ptrTy_}, false);
    auto *fn = llvm::Function::Create(fnTy, llvm::Function::InternalLinkage,
                                       "__ry_enum_box_eq_" + typeName, mod_.get());
    enum_eq_cache_[typeName] = fn;

    auto *entry = createBBInFn("entry", fn);
    builder_.SetInsertPoint(entry);
    // Args point at the payload region (box+8).
    auto *payloadA = builder_.CreateLoad(payloadTy, fn->getArg(0), "enum.eq.a");
    auto *payloadB = builder_.CreateLoad(payloadTy, fn->getArg(1), "enum.eq.b");

    // For Option/Result/ADT, propagate the source-level name so
    // emitComparisonOp recurses correctly through the nested ARC slots.
    getOrCreateMeta(payloadA).source_type_name = typeName;
    getOrCreateMeta(payloadB).source_type_name = typeName;

    // emitComparisonOp handles i64, Option, Result, ADT (via findAdtEnumName).
    llvm::Value *eq = emitComparisonOp("==", payloadA, payloadB, typeName, typeName);
    builder_.CreateRet(builder_.CreateZExt(eq, i64Ty_, "enum.eq.i64"));
    return fn;
}

llvm::GlobalVariable *
CodeGen::getOrCreateEnumDescriptor(const std::string &typeName,
                                    llvm::Type *payloadTy) {
    if (auto it = enum_descriptor_cache_.find(typeName);
        it != enum_descriptor_cache_.end())
        return it->second;

    auto *savedBB = builder_.GetInsertBlock();
    llvm::IRBuilder<>::InsertPoint savedIP = builder_.saveIP();

    auto *dtor = getOrCreateEnumBoxDtor(typeName, payloadTy);
    auto *eq = getOrCreateEnumBoxEq(typeName, payloadTy);
    auto *typeNameConst = cachedGlobalString(typeName, ".enum_type_name");

    auto *descTy = llvm::StructType::get(*ctx_, {ptrTy_, ptrTy_, ptrTy_});
    auto *initVal = llvm::ConstantStruct::get(
        descTy,
        {dtor, eq, llvm::cast<llvm::Constant>(typeNameConst)});
    auto *gv = new llvm::GlobalVariable(
        *mod_, descTy, /*isConstant=*/true,
        llvm::GlobalValue::PrivateLinkage, initVal,
        "__ry_enum_desc_" + typeName);
    gv->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    gv->setAlignment(llvm::Align(8));
    enum_descriptor_cache_[typeName] = gv;

    if (savedBB)
        builder_.restoreIP(savedIP);
    return gv;
}

void CodeGen::emitEnumBoxArcFieldsRetain(llvm::Value *payloadVal,
                                          const std::string &enumTypeName,
                                          llvm::Type *payloadTy) {
    // Field-wise retain for heap-box reassignment paths. Counterpart to
    // emitRecordArcFieldsRetain. Walks the payload struct per-variant
    // (ADT) or per-slot (Option/Result) and retains ARC pointers.
    if (!payloadVal) return;
    std::string head;
    std::vector<std::string> args;
    splitEnumLikeName(enumTypeName, &head, &args);

    auto retainSlot = [&](llvm::Value *slot, const std::string &innerName,
                          CollectionKind kind) {
        if (slot->getType() != ptrTy_) return;
        auto *isNull = builder_.CreateICmpEQ(
            slot, llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)),
            "enum.retain.null");
        auto *retainBB = createBB("enum.field_retain");
        auto *skipBB = createBB("enum.field_retain_skip");
        emitBranchCond(isNull, skipBB, retainBB);
        builder_.SetInsertPoint(retainBB);
        auto *hdr = (kind == CollectionKind::Str) ? emitStrGetHeaderFromData(slot)
                                                    : emitArcGetHeaderFromData(slot);
        emitArcRetain(hdr, /*atomic=*/false);
        emitBranchUncond(skipBB);
        builder_.SetInsertPoint(skipBB);
        (void)innerName;
    };

    if (head == "Option" && args.size() == 1) {
        CollectionKind innerKind;
        if (!fieldTypeIsArcManaged(args[0], &innerKind)) return;
        auto *tag = builder_.CreateExtractValue(payloadVal, 0, "opt.retain.tag");
        auto *someBB = createBB("opt.retain.some");
        auto *doneBB = createBB("opt.retain.done");
        emitBranchCond(tag, someBB, doneBB);
        builder_.SetInsertPoint(someBB);
        auto *inner = builder_.CreateExtractValue(payloadVal, 1, "opt.retain.inner");
        retainSlot(inner, args[0], innerKind);
        emitBranchUncond(doneBB);
        builder_.SetInsertPoint(doneBB);
        return;
    }
    if (head == "Result" && args.size() == 2) {
        CollectionKind okKind, errKind;
        bool okArc = fieldTypeIsArcManaged(args[0], &okKind);
        bool errArc = fieldTypeIsArcManaged(args[1], &errKind);
        if (!okArc && !errArc) return;
        auto *tag = builder_.CreateExtractValue(payloadVal, 0, "res.retain.tag");
        auto *okBB = createBB("res.retain.ok");
        auto *errBB = createBB("res.retain.err");
        auto *doneBB = createBB("res.retain.done");
        emitBranchCond(tag, okBB, errBB);
        builder_.SetInsertPoint(okBB);
        if (okArc) {
            auto *okVal = builder_.CreateExtractValue(payloadVal, 1, "res.retain.ok_val");
            retainSlot(okVal, args[0], okKind);
        }
        emitBranchUncond(doneBB);
        builder_.SetInsertPoint(errBB);
        if (errArc) {
            auto *errVal = builder_.CreateExtractValue(payloadVal, 2, "res.retain.err_val");
            retainSlot(errVal, args[1], errKind);
        }
        emitBranchUncond(doneBB);
        builder_.SetInsertPoint(doneBB);
        return;
    }
    // Organic enum.
    const EnumInfo *info = findEnumType(enumTypeName);
    if (!info || !info->isADT || !info->adtType || payloadTy != info->adtType)
        return;
    auto *adtSt = info->adtType;
    auto *tag = builder_.CreateExtractValue(payloadVal, 0, "adt.retain.tag");
    auto *doneBB = createBB("adt.retain.done");
    // Stack-spill the payload struct so we can GEP into the byte array.
    auto *spill = builder_.CreateAlloca(adtSt, nullptr, "adt.retain.spill");
    builder_.CreateStore(payloadVal, spill);
    auto *payloadBytesPtr = builder_.CreateStructGEP(adtSt, spill, 1, "adt.retain.bytes_ptr");
    auto *sw = builder_.CreateSwitch(tag, doneBB,
                                      static_cast<unsigned>(info->variantOrder.size()));
    const llvm::DataLayout &dl = mod_->getDataLayout();
    for (const std::string &vname : info->variantOrder) {
        auto vfIt = info->variantFields.find(vname);
        int64_t tagVal = info->variants.at(vname);
        bool hasArc = false;
        if (vfIt != info->variantFields.end())
            for (const auto &ftn : vfIt->second.fieldTypeNames)
                if (fieldTypeIsArcManaged(ftn)) { hasArc = true; break; }
        auto *tagConst = llvm::cast<llvm::ConstantInt>(
            llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(tagVal)));
        if (!hasArc) { sw->addCase(tagConst, doneBB); continue; }
        auto *caseBB = createBB(("adt.retain." + vname).c_str());
        sw->addCase(tagConst, caseBB);
        builder_.SetInsertPoint(caseBB);
        const VariantFieldInfo &vfi = vfIt->second;
        size_t offset = 0;
        for (size_t fi = 0; fi < vfi.fieldTypes.size(); ++fi) {
            llvm::Type *fieldTy = vfi.fieldTypes[fi];
            const std::string &fieldTypeName = vfi.fieldTypeNames[fi];
            uint64_t align = dl.getABITypeAlign(fieldTy).value();
            if (align > 0)
                offset = (offset + align - 1) / align * align;
            CollectionKind fk;
            if (fieldTypeIsArcManaged(fieldTypeName, &fk)) {
                auto *fieldPtr = builder_.CreateGEP(
                    i8Ty_, payloadBytesPtr,
                    llvm::ConstantInt::get(i64Ty_, offset),
                    "adt.retain.field." + std::to_string(fi));
                auto *fieldVal = builder_.CreateLoad(fieldTy, fieldPtr,
                                                      "adt.retain.field.val");
                retainSlot(fieldVal, fieldTypeName, fk);
            }
            offset += dl.getTypeAllocSize(fieldTy);
        }
        emitBranchUncond(doneBB);
    }
    builder_.SetInsertPoint(doneBB);
}

} // namespace ry
