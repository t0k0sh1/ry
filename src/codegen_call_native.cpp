#include "ry/codegen.hpp"
#include "ry/native_call_descriptor.hpp"  // extractResultOkType
#include "ry/stdlib_registry.hpp"

#include <functional>
#include <unordered_map>


namespace ry {

// Installment 2-c (#2381): NUL-check static-global counter, keyed by
// the per-overload err_global_prefix encoded in NativeOverloadOverride.
// Counters live for the whole process — matches the pre-migration
// pattern where each customEmitter held its own `static int <X>Ctr`,
// so byte-exact verification against a baseline IR diff holds when the
// descriptor path replays the same sequence.
static std::unordered_map<std::string, int> &nulErrCounters() {
    static std::unordered_map<std::string, int> counters;
    return counters;
}

int CodeGen::nextNulErrCounter(const std::string &prefix) {
    return nulErrCounters()[prefix]++;
}

llvm::Value *CodeGen::emitHttpStrNulCheckInternal(llvm::Value *strVal,
                                                   const std::string &hint) {
    auto nulFn = getRuntimeFn("__ry_http_str_has_nul", i64Ty_, {ptrTy_});
    llvm::Value *hasNul = builder_.CreateCall(nulFn, {strVal},
                                              hint + "_has_nul");
    return builder_.CreateICmpNE(hasNul,
        llvm::ConstantInt::get(i64Ty_, 0), hint + "_is_nul");
}

llvm::Value *CodeGen::emitIteratorFromHandleNative(const CallExpr &e,
                                                    const NativeCallDescriptor &desc,
                                                    const std::vector<llvm::Value *> &args,
                                                    const std::string &rtName) {
    // Today's sole consumer is io::lines (handle = File, next-fn target =
    // __ry_io_file_read_line). Replicates emitFileLines's SSA-name pattern
    // ("file" / "fl_out" / "fl_status" / "fl_isline" / "fl_line" /
    // "lines_state" / "lines_header") so byte-exact verification against
    // the pre-migration baseline holds.
    if (args.empty() || desc.handle_param_index != 0
        || desc.handle_resource_kind == ResourceKindRegistry::NONE) {
        codegenError(e.callee + "(): IteratorFromHandle requires arg 0 as a handle");
    }
    llvm::Value *fileHandle = args[0];
    int fileRk = desc.handle_resource_kind;

    llvm::StructType *stateTy = llvm::StructType::get(*ctx_,
        llvm::ArrayRef<llvm::Type *>{ptrTy_});
    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t stateSize = dl.getTypeAllocSize(stateTy);

    llvm::StructType *optTy = getOptionType(ptrTy_);
    static int file_lines_counter = 0;
    std::string fnName = "__iter_file_lines_next." + std::to_string(file_lines_counter++);
    llvm::FunctionType *nextFnTy = llvm::FunctionType::get(optTy, {ptrTy_}, false);
    llvm::Function *nextFn = llvm::Function::Create(
        nextFnTy, llvm::Function::ExternalLinkage, fnName, *mod_);

    {
        FnScope guard(*this);
        fn_ = nextFn;
        pushScope();
        llvm::BasicBlock *entry = createBBInFn("entry", nextFn);
        builder_.SetInsertPoint(entry);

        llvm::Value *statePtr = nextFn->getArg(0);
        llvm::Value *file = builder_.CreateLoad(ptrTy_,
            builder_.CreateStructGEP(stateTy, statePtr, 0), "file");

        llvm::Value *outAlloca = builder_.CreateAlloca(ptrTy_, nullptr, "fl_out");
        builder_.CreateStore(
            llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)),
            outAlloca);

        auto readLineFn = getRuntimeFn(rtName.c_str(), i64Ty_, {ptrTy_, ptrTy_});
        llvm::Value *status = builder_.CreateCall(
            readLineFn, {file, outAlloca}, "fl_status");
        llvm::Value *isLine = builder_.CreateICmpEQ(
            status, llvm::ConstantInt::get(i64Ty_, 0), "fl_isline");

        llvm::BasicBlock *someBB = createBBInFn("some", nextFn);
        llvm::BasicBlock *noneBB = createBBInFn("none", nextFn);
        emitBranchCond(isLine, someBB, noneBB);

        builder_.SetInsertPoint(someBB);
        llvm::Value *line = builder_.CreateLoad(ptrTy_, outAlloca, "fl_line");
        builder_.CreateRet(buildSomeValue(line, optTy));

        builder_.SetInsertPoint(noneBB);
        builder_.CreateRet(buildNoneValue(optTy));
        popScope();
    }

    auto mallocFn = getStdlibMalloc();
    llvm::Value *stateAlloc = builder_.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty_, stateSize)}, "lines_state");
    iterator_malloc_stack_.back().push_back(stateAlloc);

    // ARC retain: the iterator must outlive the user's handle binding.
    llvm::Value *fileHdr = emitArcGetHeaderFromData(fileHandle);
    bool atomic = isArcAtomic(fileHandle);
    emitArcRetain(fileHdr, atomic);
    iterator_release_hooks_.back().push_back({fileHandle, fileRk});

    builder_.CreateStore(fileHandle,
        builder_.CreateStructGEP(stateTy, stateAlloc, 0));

    llvm::Value *header = nullptr;
    {
        uint64_t headerSize = dl.getTypeAllocSize(iteratorHeaderTy_);
        header = builder_.CreateCall(
            mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "lines_header");
        builder_.CreateStore(nextFn,
            builder_.CreateStructGEP(iteratorHeaderTy_, header, 0));
        builder_.CreateStore(stateAlloc,
            builder_.CreateStructGEP(iteratorHeaderTy_, header, 1));
        setTypeMeta(TypeMeta::IteratorElem, header, ptrTy_);
        iterator_malloc_stack_.back().push_back(header);
    }
    if (!desc.iterator_elem_type_name.empty())
        getOrCreateMeta(header).list_elem_type_name = desc.iterator_elem_type_name;
    return header;
}

llvm::StructType *CodeGen::getResultTypeForNativeReturn(
    const std::string &returnTypeName) {
    // For Result<Option<T>, Error> the NUL-check chain's emitResultBranch
    // needs the outer Result struct type. Today the only inner Option type
    // we surface through descriptor dispatch is Option<str> (header / query
    // / cookie / formField / formFile / readLine variants), which is
    // Option<ptr>. Other inner types (Option<Map<str,str>>, Option<List>,
    // ...) also have a ptr-stored carrier so the same Option<ptr> struct
    // applies — the element-stride metadata is carried separately via
    // propagateTypeMeta on the merged PHI.
    std::string okType = extractResultOkType(returnTypeName);
    if (okType.size() > 7 && okType.compare(0, 7, "Option<") == 0) {
        llvm::StructType *optTy = getOptionType(ptrTy_);
        return getResultType(optTy, errorTy_);
    }
    // Default: Result<T, Error> where T is a single-word ptr-typed
    // resource (HttpClientResponse, etc.) or a primitive that resolves
    // to a known type.
    llvm::Type *okLlvmTy = okType.empty() ? ptrTy_ : resolveType(okType);
    return getResultType(okLlvmTy, errorTy_);
}

llvm::Value *CodeGen::emitTableDrivenNativeCall(
    const CallExpr &e,
    const char *package,
    const NativeDispatchEntry *table,
    size_t table_size) {

    // Guard: check if this callee has any registered @native signature
    // for this package (or as a bare name for inline declarations).
    // The sigIt is reused later for signature lookup (variadic + normal paths).
    std::string sigKey = ry::util::nativeSigKey(package, e.callee);
    auto sigItHead = native_fn_sigs_.find(sigKey);
    if (sigItHead == native_fn_sigs_.end()) {
        sigItHead = native_fn_sigs_.find(e.callee);
        if (sigItHead == native_fn_sigs_.end())
            return nullptr;
    }

    // Descriptor-driven library auto-registration (#2340, criterion 5).
    // Every sig under one callee belongs to a single declaring module, so
    // pulling the library name off any registered sig — even before arity /
    // type checks succeed — is sound. Doing it once up here retires the three
    // per-branch hand-inserts that used to live in the variadic / customEmitter
    // / normal paths and ensures the JIT loads `libry_<module>` before any
    // early codegenError aborts the rest of the dispatch.
    if (!sigItHead->second.empty() && !sigItHead->second[0].library.empty())
        linkNativeLibrary(sigItHead->second[0].library);

    // Lookup entry in dispatch table
    const NativeDispatchEntry *entry = nullptr;
    for (size_t i = 0; i < table_size; i++) {
        if (e.callee == table[i].fnName) {
            entry = &table[i];
            break;
        }
    }
    if (!entry)
        return nullptr;

    // Lazily build error function name (only for wrappings that need it)
    auto getErrFnName = [&]() -> std::string {
        if (entry->errFnOverride)
            return entry->errFnOverride;
        return ry::util::deriveRuntimeFnName(package, "get_last_error");
    };

    // Resolve the effective runtime suffix
    const char *rtSuffix = entry->rtSuffix ? entry->rtSuffix : entry->fnName;

    // --- Special case: variadic arity (e.g. path::join) ---
    if (entry->arity == -1) {
        size_t n = e.args.size();

        // Check sig key BEFORE arity validation so that a name collision
        // with a different-library function falls through instead of erroring.
        auto sigIt = native_fn_sigs_.find(sigKey);
        if (sigIt == native_fn_sigs_.end()) {
            // Fallback: try bare name for inline @native declarations without module
            sigIt = native_fn_sigs_.find(e.callee);
            if (sigIt == native_fn_sigs_.end())
                return nullptr;  // No sig for this module — fall through
        }

        bool hasArityMatch = false;
        for (const auto &sig : sigIt->second) {
            if (sig.params.size() == n) { hasArityMatch = true; break; }
        }
        if (!hasArityMatch)
            return nullptr;  // Arity mismatch — fall through

        if (n < 2 || n > 4)
            codegenError(std::string(entry->fnName) + "() requires 2, 3, or 4 arguments");

        // Emit args once, then find the sig whose types match
        std::vector<llvm::Value *> args;
        args.reserve(n);
        for (size_t i = 0; i < n; i++)
            args.push_back(emitExpr(*e.args[i]));

        // Two-pass overload resolution (see normal path for rationale): exact
        // match first, widening (e.g. int->float) as fallback.
        const NativeFnSignature *matchedSig = nullptr;
        std::vector<llvm::Type *> argTypes;
        std::vector<bool> needsWidening(n, false);
        std::vector<llvm::Type *> candidateTypes;
        candidateTypes.reserve(n);

        for (const auto &sig : sigIt->second) {
            if (sig.params.size() != n) continue;
            bool typesMatch = true;
            candidateTypes.clear();
            for (size_t i = 0; i < n; i++) {
                llvm::Type *expectedTy = resolveType(sig.params[i].typeName);
                if (args[i]->getType() != expectedTy) {
                    typesMatch = false;
                    break;
                }
                candidateTypes.push_back(expectedTy);
            }
            if (typesMatch) {
                matchedSig = &sig;
                argTypes = std::move(candidateTypes);
                break;
            }
        }

        if (!matchedSig) {
            std::vector<bool> candidateNeedsWidening(n, false);
            for (const auto &sig : sigIt->second) {
                if (sig.params.size() != n) continue;
                bool typesMatch = true;
                candidateTypes.clear();
                candidateNeedsWidening.assign(n, false);
                for (size_t i = 0; i < n; i++) {
                    llvm::Type *expectedTy = resolveType(sig.params[i].typeName);
                    if (args[i]->getType() == expectedTy) {
                        // exact
                    } else if (isWideningConversion(args[i], expectedTy,
                                                    sig.params[i].typeName)) {
                        candidateNeedsWidening[i] = true;
                    } else {
                        typesMatch = false;
                        break;
                    }
                    candidateTypes.push_back(expectedTy);
                }
                if (typesMatch) {
                    matchedSig = &sig;
                    argTypes = std::move(candidateTypes);
                    needsWidening = std::move(candidateNeedsWidening);
                    break;
                }
            }
        }

        if (!matchedSig) {
            std::vector<std::string> actualTypes;
            actualTypes.reserve(n);
            for (auto *v : args)
                actualTypes.push_back(formatActualArgTypeName(v));
            codegenErrorNoMatchingOverload(
                e.callee,
                collectNativeOverloadCandidateSigs(e.callee),
                actualTypes);
        }

        // Apply widening coercions to matched args (no-op when all false).
        for (size_t i = 0; i < n; i++) {
            if (needsWidening[i])
                args[i] = emitWideningConversion(args[i], argTypes[i]);
        }


        std::string rtName = (entry->rtNameOverride
            ? std::string(entry->rtNameOverride)
            : ry::util::deriveRuntimeFnName(package, rtSuffix))
            + std::to_string(n);

        // For ResultPtr, the C runtime returns a raw pointer; wrap after call.
        llvm::Type *cRetTyVariadic = (entry->wrapping == ReturnWrapping::ResultPtr)
            ? ptrTy_
            : resolveType(matchedSig->returnTypeName);
        auto *fnTy = llvm::FunctionType::get(cRetTyVariadic, argTypes, false);
        auto fn = mod_->getOrInsertFunction(rtName, fnTy);
        llvm::Value *callResult = builder_.CreateCall(fn, args, entry->fnName);

        if (entry->wrapping == ReturnWrapping::ResultPtr) {
            std::string errFn = getErrFnName();
            return wrapPtrAsResult(callResult, errFn.c_str());
        }
        return callResult;
    }

    // --- Normal path ---

    // Look up NativeFnSignature matching this call's arity.
    // Check sig key BEFORE requireArgs so that a name collision with a
    // different-library function falls through instead of erroring.
    auto sigIt = native_fn_sigs_.find(sigKey);
    if (sigIt == native_fn_sigs_.end()) {
        // Fallback: try bare name for inline @native declarations without module
        sigIt = native_fn_sigs_.find(e.callee);
        if (sigIt == native_fn_sigs_.end())
            return nullptr;  // No sig for this module — fall through
    }

    // Custom emitter escape hatch: runs only after sig key AND call-arity
    // validation.  The arity check uses the CALL's actual arg count against
    // registered signatures so that same-module overloads with different
    // arities are not hijacked (e.g. a 2-arg overload won't be grabbed by
    // a 1-arg custom emitter entry).
    if (entry->customEmitter) {
        bool hasCallArityMatch = false;
        for (const auto &sig : sigIt->second) {
            if (sig.params.size() == e.args.size()) {
                hasCallArityMatch = true;
                break;
            }
        }
        if (!hasCallArityMatch)
            return nullptr;  // No sig with this arity — fall through

        // #1682: mock/spy interception for customEmitter natives. We resolve
        // the call's target overload at AST level (arity-only when arities
        // differ — sufficient for math.digits-style overloads), build the
        // canonical sig, and only divert through the tri-block dispatch if
        // the sig was registered via mock() / spy(). Otherwise fall straight
        // through to the customEmitter (zero-overhead path).
        if (test_mode_) {
            const NativeFnSignature *picked =
                pickNativeOverloadByCallShape(e, sigIt->second);
            if (picked) {
                std::vector<std::string> pn;
                pn.reserve(picked->params.size());
                for (const auto &p : picked->params) pn.push_back(p.typeName);
                const std::string canonicalSig = buildCanonicalSig(e.callee, pn);
                const bool isMocked = mocked_functions_.count(canonicalSig) > 0;
                const bool isSpied = spied_functions_.count(canonicalSig) > 0;
                if (isMocked || isSpied) {
                    return emitNativeCustomEmitterMockDispatch(
                        e, entry->customEmitter, *picked, canonicalSig,
                        isMocked, isSpied);
                }
            }
        }
        return entry->customEmitter(*this, e);
    }

    // Check if any sig matches the arity before emitting args
    bool hasArityMatch = false;
    for (const auto &sig : sigIt->second) {
        if (static_cast<int>(sig.params.size()) == entry->arity) {
            hasArityMatch = true;
            break;
        }
    }
    if (!hasArityMatch)
        return nullptr;  // Arity mismatch — fall through

    requireArgs(e, static_cast<size_t>(entry->arity));

    // Emit args once, then find the sig whose types match
    std::vector<llvm::Value *> args;
    args.reserve(static_cast<size_t>(entry->arity));
    for (size_t i = 0; i < static_cast<size_t>(entry->arity); i++)
        args.push_back(emitExpr(*e.args[i]));

    // Two-pass overload resolution: exact match first, then widening fallback
    // (e.g. int->float). Mirrors resolveOverload() so user-defined and
    // @native overload sets behave consistently.
    const NativeFnSignature *matchedSig = nullptr;
    std::vector<llvm::Type *> paramLLVMTypes;
    std::vector<bool> needsWidening(static_cast<size_t>(entry->arity), false);
    std::vector<llvm::Type *> candidateTypes;
    candidateTypes.reserve(static_cast<size_t>(entry->arity));

    for (const auto &sig : sigIt->second) {
        if (static_cast<int>(sig.params.size()) != entry->arity) continue;
        bool typesMatch = true;
        candidateTypes.clear();
        for (size_t i = 0; i < static_cast<size_t>(entry->arity); i++) {
            llvm::Type *expectedTy = resolveType(sig.params[i].typeName);
            if (args[i]->getType() != expectedTy) {
                typesMatch = false;
                break;
            }
            candidateTypes.push_back(expectedTy);
        }
        if (typesMatch) {
            matchedSig = &sig;
            paramLLVMTypes = std::move(candidateTypes);
            break;
        }
    }

    if (!matchedSig) {
        std::vector<bool> candidateNeedsWidening(
            static_cast<size_t>(entry->arity), false);
        for (const auto &sig : sigIt->second) {
            if (static_cast<int>(sig.params.size()) != entry->arity) continue;
            bool typesMatch = true;
            candidateTypes.clear();
            candidateNeedsWidening.assign(
                static_cast<size_t>(entry->arity), false);
            for (size_t i = 0; i < static_cast<size_t>(entry->arity); i++) {
                llvm::Type *expectedTy = resolveType(sig.params[i].typeName);
                if (args[i]->getType() == expectedTy) {
                    // exact
                } else if (isWideningConversion(args[i], expectedTy,
                                                sig.params[i].typeName)) {
                    candidateNeedsWidening[i] = true;
                } else {
                    typesMatch = false;
                    break;
                }
                candidateTypes.push_back(expectedTy);
            }
            if (typesMatch) {
                matchedSig = &sig;
                paramLLVMTypes = std::move(candidateTypes);
                needsWidening = std::move(candidateNeedsWidening);
                break;
            }
        }
    }

    if (!matchedSig) {
        std::vector<std::string> actualTypes;
        actualTypes.reserve(args.size());
        for (auto *v : args)
            actualTypes.push_back(formatActualArgTypeName(v));
        codegenErrorNoMatchingOverload(
            e.callee,
            collectNativeOverloadCandidateSigs(e.callee),
            actualTypes);
    }

    // Apply widening coercions to matched args (no-op when all false).
    for (size_t i = 0; i < static_cast<size_t>(entry->arity); i++) {
        if (needsWidening[i])
            args[i] = emitWideningConversion(args[i], paramLLVMTypes[i]);
    }

    // Library registration handled at function entry (#2340 auto-register).

    // Derive runtime function name
    std::string rtName = entry->rtNameOverride
        ? entry->rtNameOverride
        : ry::util::deriveRuntimeFnName(package, rtSuffix);

    // Pre-compute out-param type for ResultOutParam (used in two places)
    llvm::Type *outTy = nullptr;
    if (entry->wrapping == ReturnWrapping::ResultOutParam)
        outTy = entry->outParamType ? resolveType(entry->outParamType) : i64Ty_;

    // Determine C-level return type and build function type
    llvm::Type *cRetTy;
    switch (entry->wrapping) {
    case ReturnWrapping::Direct: {
        cRetTy = resolveType(matchedSig->returnTypeName);
        break;
    }
    case ReturnWrapping::ResultPtr:
    case ReturnWrapping::ResultPtrWithListMeta:
        cRetTy = ptrTy_;
        break;
    case ReturnWrapping::ResultStatus:
    case ReturnWrapping::BoolFromI64:
        cRetTy = i64Ty_;
        break;
    case ReturnWrapping::ResultOutParam:
        paramLLVMTypes.push_back(ptrTy_);
        cRetTy = i64Ty_;
        break;
    // Installment 2-c (#2381): these wrappings only come from the
    // descriptor populate path and are dispatched via
    // emitGenericNativeCall, never via emitTableDrivenNativeCall. The
    // unreachable arms here exist to satisfy -Wswitch.
    case ReturnWrapping::OptionFromNullablePtr:
    case ReturnWrapping::ResultOutParamOption:
    case ReturnWrapping::IteratorFromHandle:
    case ReturnWrapping::ResourceFree:
        llvm_unreachable("descriptor-only wrapping reached emitTableDrivenNativeCall");
    }

    // For ResultOutParam, create alloca and add to args
    llvm::AllocaInst *outSlot = nullptr;
    if (entry->wrapping == ReturnWrapping::ResultOutParam) {
        outSlot = builder_.CreateAlloca(outTy, nullptr,
            std::string(entry->fnName) + "_out");
        args.push_back(outSlot);
    }

    // Reject list arguments whose element type is not i8 (e.g. bare int literals
    // produce i64-stride lists that are incompatible with IOListHeader).
    if (entry->requireListU8Arg >= 0) {
        const auto argIdx = static_cast<size_t>(entry->requireListU8Arg);
        if (argIdx >= args.size())
            codegenError(std::string(entry->fnName) +
                         "() has invalid requireListU8Arg dispatch metadata");
        llvm::Type *elemTy = getListElementType(args[argIdx]);
        if (!elemTy || elemTy != i8Ty_)
            codegenError(std::string(entry->fnName) + "() requires List<u8> as argument " +
                         std::to_string(argIdx) + "; use [97u8, 0u8, 98u8]"
                         " (explicit u8 literals) or toBytes(\"...\") to produce a byte list");
    }

    auto *fnTy = llvm::FunctionType::get(cRetTy, paramLLVMTypes, false);
    auto fn = mod_->getOrInsertFunction(rtName, fnTy);
    llvm::Value *callResult;
    if (cRetTy->isVoidTy())
        callResult = builder_.CreateCall(fn, args);
    else
        callResult = builder_.CreateCall(fn, args, entry->fnName);

    // Apply return wrapping
    switch (entry->wrapping) {
    case ReturnWrapping::Direct:
        if (cRetTy->isVoidTy())
            return llvm::ConstantInt::get(i8Ty_, 0); // Unit
        if (entry->listElemMeta == ListElemMeta::I8)
            setTypeMeta(TypeMeta::ListElem, callResult, i8Ty_);
        else if (entry->listElemMeta == ListElemMeta::Ptr)
            setTypeMeta(TypeMeta::ListElem, callResult, ptrTy_);
        return callResult;

    case ReturnWrapping::ResultPtr: {
        std::string errFn = getErrFnName();
        llvm::Value *result = wrapPtrAsResult(callResult, errFn.c_str());
        if (entry->listElemMeta == ListElemMeta::I8)
            setTypeMeta(TypeMeta::ListElem, result, i8Ty_);
        else if (entry->listElemMeta == ListElemMeta::Ptr)
            setTypeMeta(TypeMeta::ListElem, result, ptrTy_);
        return result;
    }

    case ReturnWrapping::ResultPtrWithListMeta: {
        std::string errFn = getErrFnName();
        llvm::Value *result = wrapPtrAsResult(callResult, errFn.c_str());
        setTypeMeta(TypeMeta::ListElem, result, ptrTy_);
        return result;
    }

    case ReturnWrapping::ResultStatus: {
        std::string errFn = getErrFnName();
        return wrapStatusAsResult(callResult, errFn.c_str());
    }

    case ReturnWrapping::BoolFromI64:
        return builder_.CreateTrunc(callResult, i1Ty_,
            std::string(entry->fnName) + "_bool");

    case ReturnWrapping::ResultOutParam: {
        std::string errFn = getErrFnName();
        llvm::Value *isErr = builder_.CreateICmpNE(callResult,
            llvm::ConstantInt::get(i64Ty_, 0),
            std::string(entry->fnName) + "_err");
        llvm::StructType *resTy = getResultType(outTy, errorTy_);
        return emitResultBranch(isErr, resTy,
            [&]() {
                llvm::Value *loaded = builder_.CreateLoad(outTy, outSlot,
                    std::string(entry->fnName) + "_val");
                return buildOkValue(loaded, resTy);
            },
            [&]() {
                return buildErrValue(
                    buildErrorFromRuntime(errFn.c_str()), resTy);
            });
    }
    // Installment 2-c (#2381): descriptor-only wrappings. See cRetTy
    // switch above for the rationale.
    case ReturnWrapping::OptionFromNullablePtr:
    case ReturnWrapping::ResultOutParamOption:
    case ReturnWrapping::IteratorFromHandle:
    case ReturnWrapping::ResourceFree:
        llvm_unreachable("descriptor-only wrapping reached emitTableDrivenNativeCall");
    }

    llvm_unreachable("unhandled ReturnWrapping variant");
}

// ===== Generic dispatch for @native("libname") functions =====
//
// This handles calls to functions declared with @native("libname") that are
// NOT covered by the hardcoded stdlib dispatch tables. The dispatcher
// consults the NativeCallDescriptor registry (built at @native declaration
// time in src/codegen_fn.cpp) for return wrapping, error channel, and
// List<u8> enforcement metadata — eliminating per-call inference (#2337).

llvm::Value *CodeGen::emitGenericNativeCall(const CallExpr &e) {
    // O(1) lookup via secondary index: fn_name → candidate library names
    auto libIt = native_lib_index_.find(e.callee);
    if (libIt == native_lib_index_.end())
        return nullptr;

    // Emit args once, then try each candidate library for a type match.
    // This avoids committing to the first arity match when a different
    // library's signature may match the actual argument types.
    std::vector<llvm::Value *> args;
    args.reserve(e.args.size());
    for (size_t i = 0; i < e.args.size(); i++)
        args.push_back(emitExpr(*e.args[i]));

    // Two-pass overload resolution across libraries: exact match first,
    // widening (e.g. int->float) as fallback. Ambiguity is detected per tier
    // (two exact matches across libs = ambiguous; two widening matches across
    // libs = ambiguous). An exact match in one lib beats a widening match in
    // another — Pass 2 only runs on empty Pass 1.
    const NativeFnSignature *matchedSig = nullptr;
    std::string matchedPackage;
    size_t matchedIdx = 0;  // overload index within native_fn_sigs_[matchedSigKey]
    std::vector<llvm::Type *> paramTypes;
    std::vector<bool> needsWidening(e.args.size(), false);
    std::vector<llvm::Type *> candidateTypes;
    candidateTypes.reserve(e.args.size());

    for (const auto &lib : libIt->second) {
        std::string sigKey = ry::util::nativeSigKey(lib, e.callee);
        auto sigIt = native_fn_sigs_.find(sigKey);
        if (sigIt == native_fn_sigs_.end()) continue;
        for (size_t sigIdx = 0; sigIdx < sigIt->second.size(); ++sigIdx) {
            const auto &sig = sigIt->second[sigIdx];
            if (sig.params.size() != e.args.size()) continue;
            bool typesMatch = true;
            candidateTypes.clear();
            for (size_t i = 0; i < args.size(); i++) {
                llvm::Type *expectedTy = resolveType(sig.params[i].typeName);
                if (args[i]->getType() != expectedTy) {
                    typesMatch = false;
                    break;
                }
                // Installment 2-c (#2381): resource-kind disambiguation.
                // Two @native overloads with identical LLVM signatures
                // but different declared resource types (e.g. body(HttpRequest)
                // vs body(HttpClientResponse), both `(ptr) -> ptr`) must
                // be disambiguated by the arg's resource_kind annotation.
                // Two symmetric rules:
                //   (a) param IS a resource type: arg must carry the
                //       matching kind (rejects str/int args on File slots).
                //   (b) param is a primitive type: arg must NOT carry a
                //       resource_kind annotation (rejects File args on str
                //       slots — without this, writeText(file, "x") would
                //       ambiguously match writeText(File, str) AND the
                //       legacy writeText(str, str) overload).
                int expectedRk =
                    ResourceKindRegistry::instance().lookupByTypeName(
                        sig.params[i].typeName);
                if (expectedRk != ResourceKindRegistry::NONE) {
                    if (!isResourceKind(expectedRk, args[i])) {
                        typesMatch = false;
                        break;
                    }
                } else {
                    const auto *meta = getMeta(args[i]);
                    if (meta && meta->hasAnyResourceKind()) {
                        typesMatch = false;
                        break;
                    }
                }
                candidateTypes.push_back(expectedTy);
            }
            if (typesMatch) {
                if (matchedSig) {
                    std::string ambigMsg = "ambiguous @native call: '";
                    ambigMsg += e.callee;
                    ambigMsg += "' matches both @native(\"";
                    ambigMsg += matchedPackage;
                    ambigMsg += "\") and @native(\"";
                    ambigMsg += lib;
                    ambigMsg += "\")";
                    codegenError(ambigMsg);
                }
                matchedSig = &sig;
                matchedPackage = lib;
                matchedIdx = sigIdx;
                paramTypes = std::move(candidateTypes);
            }
        }
    }

    if (!matchedSig) {
        std::vector<bool> candidateNeedsWidening(e.args.size(), false);
        for (const auto &lib : libIt->second) {
            std::string sigKey = ry::util::nativeSigKey(lib, e.callee);
            auto sigIt = native_fn_sigs_.find(sigKey);
            if (sigIt == native_fn_sigs_.end()) continue;
            for (size_t sigIdx = 0; sigIdx < sigIt->second.size(); ++sigIdx) {
                const auto &sig = sigIt->second[sigIdx];
                if (sig.params.size() != e.args.size()) continue;
                bool typesMatch = true;
                candidateTypes.clear();
                candidateNeedsWidening.assign(e.args.size(), false);
                for (size_t i = 0; i < args.size(); i++) {
                    llvm::Type *expectedTy = resolveType(sig.params[i].typeName);
                    if (args[i]->getType() == expectedTy) {
                        // exact
                    } else if (isWideningConversion(args[i], expectedTy,
                                                    sig.params[i].typeName)) {
                        candidateNeedsWidening[i] = true;
                    } else {
                        typesMatch = false;
                        break;
                    }
                    // Installment 2-c (#2381): resource-kind disambiguation
                    // mirrored from Pass 1. Same two-rule structure; see
                    // Pass 1 comment for the full rationale.
                    int expectedRk =
                        ResourceKindRegistry::instance().lookupByTypeName(
                            sig.params[i].typeName);
                    if (expectedRk != ResourceKindRegistry::NONE) {
                        if (!isResourceKind(expectedRk, args[i])) {
                            typesMatch = false;
                            break;
                        }
                    } else {
                        const auto *meta = getMeta(args[i]);
                        if (meta && meta->hasAnyResourceKind()) {
                            typesMatch = false;
                            break;
                        }
                    }
                    candidateTypes.push_back(expectedTy);
                }
                if (typesMatch) {
                    if (matchedSig) {
                        std::string ambigMsg = "ambiguous @native call: '";
                        ambigMsg += e.callee;
                        ambigMsg += "' matches both @native(\"";
                        ambigMsg += matchedPackage;
                        ambigMsg += "\") and @native(\"";
                        ambigMsg += lib;
                        ambigMsg += "\") via implicit widening";
                        codegenError(ambigMsg);
                    }
                    matchedSig = &sig;
                    matchedPackage = lib;
                    matchedIdx = sigIdx;
                    paramTypes = std::move(candidateTypes);
                    needsWidening = std::move(candidateNeedsWidening);
                }
            }
        }
    }

    // No library matched both arity and types — fall through to user functions
    if (!matchedSig) return nullptr;

    linkNativeLibrary(matchedPackage);

    // Look up the descriptor populated alongside this sig in codegen_fn.cpp.
    // Descriptors and sigs are pushed in lock-step under the same sigKey, so
    // matchedIdx indexes both. The descriptor carries pre-computed return
    // wrapping, error channel, and List<u8>-arg metadata so we don't
    // re-infer at every call (#2337).
    const std::string matchedSigKey = ry::util::nativeSigKey(matchedPackage, e.callee);
    const NativeCallDescriptor &desc =
        native_call_descriptors_.at(matchedSigKey)[matchedIdx];

    // Reject list arguments whose declared param type is `List<u8>` but
    // whose actual element stride is not i8 (e.g. bare int literals
    // produce i64-stride lists incompatible with IOListHeader). Sig-level
    // type match cannot catch this because `List<u8>` and `List<int>`
    // both resolve to `ptrTy_`; the byte-stride invariant lives in
    // TypeMeta::ListElem. Mirrors the table-driven enforcement at
    // `emitTableDrivenNativeCall:364-374` so generic-dispatch callers
    // see the same error wording. The descriptor caches the first
    // List<u8> param index found at @native declaration time.
    if (desc.require_list_u8_arg >= 0) {
        size_t argIdx = static_cast<size_t>(desc.require_list_u8_arg);
        llvm::Type *elemTy = getListElementType(args[argIdx]);
        if (!elemTy || elemTy != i8Ty_)
            codegenError(e.callee + "() requires List<u8> as argument "
                         + std::to_string(argIdx)
                         + "; use [97u8, 0u8, 98u8] (explicit u8 literals) or"
                           " toBytes(\"...\") to produce a byte list");
    }

    // Apply widening coercions to matched args (no-op when all false).
    for (size_t i = 0; i < e.args.size(); i++) {
        if (needsWidening[i])
            args[i] = emitWideningConversion(args[i], paramTypes[i]);
    }

    // Adjust bool params to match the C calling convention: native functions pass bools as i64.
    // Widen i1→i64 in both the prototype and the arg values.
    for (size_t i = 0; i < paramTypes.size(); i++) {
        if (paramTypes[i] == i1Ty_) {
            args[i] = builder_.CreateZExt(args[i], i64Ty_, "bool_zext");
            paramTypes[i] = i64Ty_;
        }
    }

    // Derive runtime function name. Three sources, in precedence order:
    //   (1) descriptor.exported_symbol (Installment 2-c #2381 override
    //       for entries whose runtime symbol predates the convention,
    //       e.g. io::open -> __ry_io_file_open)
    //   (2) convention: __ry_<matchedPackage>_<snake_case_when_flagged>(callee)
    //   (3) arity-suffix for path::join
    std::string rtName;
    std::string symbolName;  // for arity-suffix decision below
    if (!desc.exported_symbol.empty()) {
        rtName = desc.exported_symbol;
    } else {
        const auto *pkgEntry = StdlibRegistry::instance().findPackage(matchedPackage);
        symbolName = (pkgEntry && pkgEntry->snake_case_symbols)
            ? ry::util::camelToSnakeCase(e.callee)
            : e.callee;
        rtName = ry::util::deriveRuntimeFnName(matchedPackage, symbolName);
    }

    // Arity-suffix runtime symbol for path::join's __ry_path_join2/3/4
    // convention. The gate is intentionally scoped to (matchedPackage,
    // symbolName) — a generic "multi-arity overload set" heuristic would
    // silently break any future @native overload whose runtime side does
    // NOT use the suffix convention (caught by CodeRabbit on PR #2342).
    // When another module adopts the same convention, extend the gate
    // here or migrate to an explicit descriptor field. The exported_symbol
    // override path skips the suffix because it's an explicit override.
    if (desc.exported_symbol.empty()) {
        const bool usesAritySuffixedRuntimeSymbol =
            matchedPackage == "path" && symbolName == "join";
        if (usesAritySuffixedRuntimeSymbol) {
            const auto &overloads = native_fn_sigs_.at(matchedSigKey);
            if (overloads.size() > 1) {
                size_t firstArity = overloads[0].params.size();
                for (size_t i = 1; i < overloads.size(); ++i) {
                    if (overloads[i].params.size() != firstArity) {
                        rtName += std::to_string(e.args.size());
                        break;
                    }
                }
            }
        }
    }

    // Determine C-level calling convention from the Ry return type (pre-
    // computed in the descriptor at @native declaration time).
    CodeGenReturnWrapping wrapping = desc.return_wrapping;
    const std::string &outParamType = desc.out_param_type_name;

    // Error function name for Result wrappings. The descriptor pre-computes
    // it from library_name for packages that own a per-module
    // `__ry_<pkg>_get_last_error` symbol (see codegen_fn.cpp's
    // kHasModuleLastError list). Packages without one (thread / gc /
    // testing / json / json5) fall through here to the default
    // `__ry_get_last_error` channel, matching what the pre-2393 thread
    // customEmitters consumed via `wrapStatusAsResult(status)` (the helper's
    // default param). Bare @native fns outside knownNativeLibs that never
    // declare an error channel similarly land here, which is the only
    // existing behavior they could have relied on (no runtime defines
    // `__ry_<unknown_pkg>_get_last_error`).
    std::string errFnName = desc.error_channel.empty()
        ? "__ry_get_last_error"
        : desc.error_channel;

    // Installment 2-c (#2381): handle-coupled overload library linkage.
    // For non-ResultPtr returns (e.g. setTimeout(TlsStream) -> Unit), the
    // handle param's resource library must still be linked because the
    // runtime function lives there (`__ry_tls_set_timeout` is in libry_http
    // even though @native("net")). For ResultPtr returns, the existing
    // resource_kind path already links — see the post-wrap branch below.
    if (desc.handle_param_index >= 0
        && desc.handle_resource_kind != ResourceKindRegistry::NONE) {
        if (const auto *info = ResourceKindRegistry::instance().getInfo(
                desc.handle_resource_kind))
            linkNativeLibrary(info->library);
    }

    // Installment 2-c (#2381): IteratorFromHandle — synthesize an iterator
    // whose next-fn calls exported_symbol(handle, &out) per next(). Today
    // only io::lines uses this; the synthesis lives inline below so the
    // descriptor consumption stays one well-scoped switch arm.
    if (wrapping == ReturnWrapping::IteratorFromHandle) {
        return emitIteratorFromHandleNative(e, desc, args, rtName);
    }

    // #2393: ResourceFree — thread *Free entries (lockFree / rwlockFree /
    // semaphoreFree / barrierFree / atomicIntFree / atomicBoolFree) emit
    // a null-check + ARC-release sequence via emitResourceFree. No runtime
    // fn is dialed. The handle arg lives at desc.handle_param_index (must
    // be 0 — all six *Free entries take the resource as their sole arg)
    // with desc.handle_resource_kind selecting the destructor. The arg has
    // already been emit-checked against the registered resource kind by
    // overload resolution, so we go straight to emitResourceFree.
    if (wrapping == ReturnWrapping::ResourceFree) {
        if (desc.handle_param_index < 0
            || desc.handle_resource_kind == ResourceKindRegistry::NONE
            || args.empty()) {
            codegenError(e.callee +
                "(): ResourceFree requires handle_param_index=0 and a "
                "registered handle_resource_kind");
        }
        return emitResourceFree(args[0], desc.handle_resource_kind,
                                *e.args[0]);
    }

    // Build C-level return type and handle out-param. For OptionFromNullablePtr
    // / ResultOutParamOption / IteratorFromHandle we synthesize the wrapping
    // shape inside the call emit; cRetTy is the runtime fn's bare return.
    llvm::Type *outTy = nullptr;
    llvm::Type *cRetTy;

    switch (wrapping) {
    case ReturnWrapping::Direct:
        cRetTy = resolveType(matchedSig->returnTypeName);
        break;
    case ReturnWrapping::ResultPtr:
    case ReturnWrapping::OptionFromNullablePtr:
        cRetTy = ptrTy_;
        break;
    case ReturnWrapping::ResultStatus:
    case ReturnWrapping::BoolFromI64:
        cRetTy = i64Ty_;
        break;
    case ReturnWrapping::ResultOutParam:
        outTy = resolveType(outParamType);
        paramTypes.push_back(ptrTy_);
        cRetTy = i64Ty_;
        break;
    case ReturnWrapping::ResultOutParamOption:
        outTy = ptrTy_;  // out-param is always a ptr; loaded into Option<ptr>
        paramTypes.push_back(ptrTy_);
        cRetTy = i64Ty_;
        break;
    case ReturnWrapping::ResultPtrWithListMeta:
        llvm_unreachable("ResultPtrWithListMeta not used in generic native dispatch");
    case ReturnWrapping::IteratorFromHandle:
        llvm_unreachable("IteratorFromHandle handled above (early return)");
    case ReturnWrapping::ResourceFree:
        llvm_unreachable("ResourceFree handled above (early return)");
    }

    // Create alloca for out-param (ResultOutParam / ResultOutParamOption)
    llvm::AllocaInst *outSlot = nullptr;
    if (wrapping == ReturnWrapping::ResultOutParam
        || wrapping == ReturnWrapping::ResultOutParamOption) {
        outSlot = builder_.CreateAlloca(outTy, nullptr, e.callee + "_out");
        if (wrapping == ReturnWrapping::ResultOutParamOption) {
            // Mirror emitFileReadLine: pre-store null so a runtime that
            // forgets to write the out-param produces None, not garbage.
            builder_.CreateStore(
                llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)),
                outSlot);
        }
        args.push_back(outSlot);
    }

    // Helper: emit the runtime call + apply return wrapping. Used both
    // directly (no NUL check) and inside a NUL-check chain (when
    // desc.nul_checks is non-empty).
    auto emitCallAndWrap = [&]() -> llvm::Value * {
        auto *fnTy = llvm::FunctionType::get(cRetTy, paramTypes, false);
        auto fn = mod_->getOrInsertFunction(rtName, fnTy);
        // #2393: the pre-migration thread customEmitters named the i64
        // status call `<callee>_status` (see emitThreadSyncOp). Mirror that
        // so the descriptor migration's IR byte-exact diff stays empty for
        // the `lockAcquire` / `rwlockReadLock` / `semaphoreAcquire` /
        // `barrierWait` / etc. chain. Other Pattern C consumers (io / net /
        // http ResultStatus entries) use the bare callee name; the gate
        // on (matchedPackage == "thread") preserves their existing IR.
        std::string callNameHint = e.callee;
        if (wrapping == ReturnWrapping::ResultStatus
            && matchedPackage == "thread") {
            callNameHint = e.callee + "_status";
        }
        llvm::Value *callResult;
        if (cRetTy->isVoidTy())
            callResult = builder_.CreateCall(fn, args);
        else
            callResult = builder_.CreateCall(fn, args, callNameHint);

        llvm::Value *res;
        switch (wrapping) {
        case ReturnWrapping::Direct:
            if (cRetTy->isVoidTy())
                return llvm::ConstantInt::get(i8Ty_, 0);
            res = callResult;
            // #2393: bare-resource returns (e.g. lockNew() -> Lock,
            // atomicIntNew() -> AtomicInt). Tag the value with the
            // descriptor's resource kind so ARC sees it correctly, mirroring
            // the pre-migration thread customEmitters' `addResourceKind`
            // call. Stdlib audit confirmed no existing Pattern C consumer
            // returns a bare resource via Direct wrapping, so adding the
            // tag here is IR-neutral outside the newly-migrated thread
            // constructors.
            if (desc.resource_kind != ResourceKindRegistry::NONE) {
                addResourceKind(res, desc.resource_kind);
                if (const auto *info =
                        ResourceKindRegistry::instance().getInfo(desc.resource_kind))
                    linkNativeLibrary(info->library);
            }
            break;

        case ReturnWrapping::ResultPtr:
            res = wrapPtrAsResult(callResult, errFnName.c_str());
            // Installment 2-a (#2338): resource-coupled natives (e.g.
            // io::open returning Result<File, Error>) tag the wrapped
            // result with the descriptor's resource kind so ARC sees it
            // correctly.
            if (desc.resource_kind != ResourceKindRegistry::NONE) {
                addResourceKind(res, desc.resource_kind);
                // Installment 2-b (#2339): when the resource's owning
                // library differs from matchedPackage, link that library
                // too. Idempotent with the matchedPackage insert.
                if (const auto *info =
                        ResourceKindRegistry::instance().getInfo(desc.resource_kind))
                    linkNativeLibrary(info->library);
            }
            break;

        case ReturnWrapping::OptionFromNullablePtr: {
            // wrapPtrAsOption(ptr) — null -> None, else Some(ptr). Then
            // buildOkValue wraps into the final Result<Option<T>, Error>.
            llvm::StructType *optTy = getOptionType(ptrTy_);
            llvm::StructType *resTy = getResultType(optTy, errorTy_);
            llvm::Value *opt = wrapPtrAsOption(callResult, e.callee);
            res = buildOkValue(opt, resTy);
            break;
        }

        case ReturnWrapping::ResultStatus:
            res = wrapStatusAsResult(callResult, errFnName.c_str());
            break;

        case ReturnWrapping::BoolFromI64:
            return builder_.CreateTrunc(callResult, i1Ty_, e.callee + "_bool");

        case ReturnWrapping::ResultOutParam: {
            llvm::Value *isErr = builder_.CreateICmpNE(callResult,
                llvm::ConstantInt::get(i64Ty_, 0), e.callee + "_err");

            bool isBoolResult =
                (matchedSig->returnTypeName.find("Result<bool") == 0);
            llvm::Type *okTy = isBoolResult ? i1Ty_ : outTy;
            llvm::StructType *resTy = getResultType(okTy, errorTy_);
            res = emitResultBranch(isErr, resTy,
                [&]() {
                    llvm::Value *loaded = builder_.CreateLoad(outTy, outSlot,
                        e.callee + "_val");
                    if (isBoolResult)
                        loaded = builder_.CreateTrunc(loaded, i1Ty_,
                            e.callee + "_bool");
                    return buildOkValue(loaded, resTy);
                },
                [&]() {
                    return buildErrValue(
                        buildErrorFromRuntime(errFnName.c_str()), resTy);
                });
            break;
        }

        case ReturnWrapping::ResultOutParamOption: {
            // status < 0 -> Err(runtime); else load+Option+Ok. Mirrors
            // emitFileReadLine / emitStdinReadLine in codegen_call_io.cpp.
            llvm::Value *isErr = builder_.CreateICmpSLT(callResult,
                llvm::ConstantInt::get(i64Ty_, 0), e.callee + "_iserr");
            llvm::StructType *optTy = getOptionType(ptrTy_);
            llvm::StructType *resTy = getResultType(optTy, errorTy_);
            res = emitResultBranch(isErr, resTy,
                [&]() {
                    llvm::Value *linePtr = builder_.CreateLoad(ptrTy_, outSlot,
                        e.callee + "_line");
                    return buildOkValue(
                        wrapPtrAsOption(linePtr, e.callee), resTy);
                },
                [&]() {
                    return buildErrValue(
                        buildErrorFromRuntime(errFnName.c_str()), resTy);
                });
            break;
        }

        case ReturnWrapping::ResultPtrWithListMeta:
            llvm_unreachable("ResultPtrWithListMeta not used in generic native dispatch");
        case ReturnWrapping::IteratorFromHandle:
            llvm_unreachable("IteratorFromHandle handled above (early return)");
        }
        return res;
    };

    // Installment 2-c (#2381): NUL-check wrapping. When non-empty, wrap
    // the entire call+wrap chain in nested emitResultBranch — outer-to-
    // inner ordering matches desc.nul_checks's vector order (httpRequest
    // checks method first, url second). Each NUL-branch's err side builds
    // a static error global named with a per-prefix counter that matches
    // the pre-migration customEmitter counter sequence so the byte-exact
    // IR diff stays empty.
    llvm::Value *result;
    if (!desc.nul_checks.empty()) {
        llvm::StructType *finalResTy = getResultTypeForNativeReturn(
            matchedSig->returnTypeName);

        std::function<llvm::Value *(size_t)> emitChain;
        emitChain = [&](size_t i) -> llvm::Value * {
            if (i == desc.nul_checks.size()) return emitCallAndWrap();

            const NativeNulCheckSpec &nc = desc.nul_checks[i];
            llvm::Value *strVal = args[static_cast<size_t>(nc.param_index)];
            llvm::Value *isNul = emitHttpStrNulCheckInternal(strVal, nc.hint);

            const std::string globName = "." + nc.err_global_prefix + "_"
                + std::to_string(nextNulErrCounter(nc.err_global_prefix));

            return emitResultBranch(isNul, finalResTy,
                [&]() { return emitChain(i + 1); },
                [&]() {
                    return buildErrValue(
                        buildStaticError(nc.err_message, globName), finalResTy);
                });
        };
        result = emitChain(0);
    } else {
        result = emitCallAndWrap();
        // BoolFromI64 returns early from emitCallAndWrap; the value here
        // is already the i1 truncation. Skip the metadata-propagation
        // pass below — there is no return-type Map/List meta to lift.
        if (wrapping == ReturnWrapping::BoolFromI64)
            return result;
    }

    // Propagate collection type metadata from the Ry return type annotation.
    // For Result<T, Error>, propagate the metadata of the inner Ok type T
    // onto the Result value so downstream operations can inspect elements.
    const std::string &retType = matchedSig->returnTypeName;
    std::string okType = extractResultOkType(retType);
    if (!okType.empty())
        propagateTypeMeta(okType, result);
    else if (retType.size() <= 7 || retType.compare(0, 7, "Result<") != 0)
        propagateTypeMeta(retType, result);

    return result;
}

// ===== #1682: mock/spy for @native customEmitter overloads =====

llvm::Value *CodeGen::emitBuiltinNativeOrMock(
    const CallExpr &e,
    const std::string &package,
    CodeGenCustomEmitterFn emitter) {
    // #2340: Pattern B compiler-builtin wrapper. Looks the call up under the
    // package key the carved-out emitter belongs to (e.g. "math" for
    // emitMathPow), runs the same #1682 mock/spy gate the Pattern A2 path
    // uses inside emitTableDrivenNativeCall, and falls back to a direct
    // emitter call when no mock is registered. Without this the carve-out
    // would silently bypass any `mock("pow(int, int)", ...)` registration.
    if (test_mode_) {
        std::string sigKey = ry::util::nativeSigKey(package, e.callee);
        auto sigIt = native_fn_sigs_.find(sigKey);
        if (sigIt == native_fn_sigs_.end())
            sigIt = native_fn_sigs_.find(e.callee);
        if (sigIt != native_fn_sigs_.end()) {
            const NativeFnSignature *picked =
                pickNativeOverloadByCallShape(e, sigIt->second);
            if (picked) {
                std::vector<std::string> pn;
                pn.reserve(picked->params.size());
                for (const auto &p : picked->params) pn.push_back(p.typeName);
                const std::string canonicalSig =
                    buildCanonicalSig(e.callee, pn);
                const bool isMocked = mocked_functions_.count(canonicalSig) > 0;
                const bool isSpied  = spied_functions_.count(canonicalSig) > 0;
                if (isMocked || isSpied) {
                    return emitNativeCustomEmitterMockDispatch(
                        e, emitter, *picked, canonicalSig, isMocked, isSpied);
                }
            }
        }
    }
    return emitter(*this, e);
}

const CodeGen::NativeFnSignature *CodeGen::pickNativeOverloadByCallShape(
    const CallExpr &e, const std::vector<NativeFnSignature> &sigs) {
    // Pass 1: arity-unique match. Covers the digits(n) / digits(n, base)
    // pattern where each arity has exactly one overload. Most existing
    // customEmitter natives are arity-disjoint, so this short-circuit
    // resolves them without needing AST type inference.
    const NativeFnSignature *picked = nullptr;
    int matchCount = 0;
    for (const auto &sig : sigs) {
        if (sig.params.size() == e.args.size()) {
            picked = &sig;
            ++matchCount;
        }
    }
    if (matchCount == 1) return picked;
    if (matchCount == 0) return nullptr;

    // Pass 2: multiple same-arity overloads (e.g. abs(int) / abs(float),
    // pow(int, int) / pow(float, float)). Resolve by inferring each arg's
    // type at the AST level and matching against sig.params[i].typeName
    // verbatim. Widening (int → float fallback) is NOT applied here — the
    // unmocked customEmitter path handles widening internally, so for mock
    // dispatch we only intercept exact-match calls. Mismatched-type calls
    // fall through to the customEmitter (unchanged behavior).
    std::unordered_map<std::string, llvm::Type *> emptyParamMap;
    std::unordered_map<std::string, std::string> emptyParamNameMap;
    std::vector<std::string> actualNames;
    actualNames.reserve(e.args.size());
    for (const auto &arg : e.args) {
        std::string n = inferExprTypeName(*arg, emptyParamMap, emptyParamNameMap);
        actualNames.push_back(resolveTypeAlias(ry::util::trimTypeNameSpaces(n)));
    }

    picked = nullptr;
    matchCount = 0;
    for (const auto &sig : sigs) {
        if (sig.params.size() != e.args.size()) continue;
        bool eq = true;
        for (size_t i = 0; i < sig.params.size(); ++i) {
            if (resolveTypeAlias(ry::util::trimTypeNameSpaces(sig.params[i].typeName))
                    != actualNames[i]) {
                eq = false; break;
            }
        }
        if (eq) { picked = &sig; ++matchCount; }
    }
    return matchCount == 1 ? picked : nullptr;
}

llvm::Value *CodeGen::emitNativeCustomEmitterMockDispatch(
    const CallExpr &e,
    CodeGenCustomEmitterFn emitter,
    const NativeFnSignature &sig,
    const std::string &canonicalSig,
    bool isMocked,
    bool isSpied) {
    // Mock/spy intercept for customEmitter natives. Three runtime cases mirror
    // the user-fn dispatch (see codegen_call_user.cpp):
    //   - spy-only: linear increment, fall through to customEmitter
    //   - mocked: tri-block (mockBB calls replacement via __ry_mock_get,
    //             origBB delegates to customEmitter), PHI-merged
    //   - mocked+spied: tri-block; origBB additionally increments before
    //                   delegating (spy semantics when mock is runtime-inactive)
    //
    // v1 limitation: customEmitter-path natives do NOT record args for
    // verifyCalledWith. Only count-style verify("name") is supported. The
    // recording side requires an OverloadEntry (paramTypes/paramTypeNames) to
    // dispatch the per-arg store function; NativeFnSignature.params carry
    // only Ry-level type names, and synthesizing a fake OverloadEntry would
    // duplicate the dispatch logic. Defer to a follow-up.

    auto &nameStr = mock_name_strings_[canonicalSig];
    if (!nameStr) nameStr = cachedGlobalString(canonicalSig, ".mock." + canonicalSig);

    llvm::FunctionCallee mockIncFn =
        getRuntimeFn("__ry_mock_increment_call", llvm::Type::getVoidTy(*ctx_), {ptrTy_});

    // Spy-only: emit linear increment, then delegate to the supplied emitter
    // (which emits args + the original call).
    if (isSpied && !isMocked) {
        builder_.CreateCall(mockIncFn, {nameStr});
        return emitter(*this, e);
    }

    // Mocked (with or without spy). Use the mock-get probe BEFORE emitting
    // args so we can split into mockBB / origBB and emit args independently
    // in each branch — args may have side effects, so we want exactly one
    // evaluation per runtime path (not two if we pre-emitted).
    llvm::FunctionCallee mockGetFn    = getRuntimeFn("__ry_mock_get",     ptrTy_, {ptrTy_});
    llvm::FunctionCallee mockGetEnvFn = getRuntimeFn("__ry_mock_get_env", ptrTy_, {ptrTy_});

    llvm::Value *mockPtr = builder_.CreateCall(mockGetFn, {nameStr}, "mock_ptr");
    llvm::Value *nullPtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));
    llvm::Value *mockActive = builder_.CreateICmpNE(mockPtr, nullPtr, "is_mocked");

    llvm::BasicBlock *mockBB = createBB("mock_bb");
    llvm::BasicBlock *origBB = createBB("orig_bb");
    llvm::BasicBlock *mergeBB = createBB("merge_bb");
    emitBranchCond(mockActive, mockBB, origBB);

    // Mock path: emit args fresh, increment, dispatch via plain or capture
    // calling convention based on env pointer.
    builder_.SetInsertPoint(mockBB);
    builder_.CreateCall(mockIncFn, {nameStr});

    std::vector<llvm::Value *> mockArgs;
    mockArgs.reserve(e.args.size());
    std::vector<llvm::Type *> mockParamTys;
    mockParamTys.reserve(sig.params.size());
    for (size_t i = 0; i < e.args.size(); ++i) {
        llvm::Value *v = emitExpr(*e.args[i]);
        llvm::Type *want = resolveType(sig.params[i].typeName);
        if (v->getType() != want && isWideningConversion(v, want, sig.params[i].typeName))
            v = emitWideningConversion(v, want);
        mockArgs.push_back(v);
        mockParamTys.push_back(want);
    }
    llvm::Type *mockRetTy = resolveType(sig.returnTypeName);
    llvm::FunctionType *mockFnTy = llvm::FunctionType::get(mockRetTy, mockParamTys, false);

    llvm::Value *envPtr = builder_.CreateCall(mockGetEnvFn, {nameStr}, "mock_env");
    llvm::Value *isCapture = builder_.CreateICmpNE(envPtr, nullPtr, "is_capture_mock");

    llvm::BasicBlock *plainBB = createBB("mock_plain_bb");
    llvm::BasicBlock *captureBB = createBB("mock_capture_bb");
    emitBranchCond(isCapture, captureBB, plainBB);

    std::vector<llvm::Type *> captureParamTys = mockParamTys;
    captureParamTys.push_back(ptrTy_);
    llvm::FunctionType *captureFnTy = llvm::FunctionType::get(
        mockRetTy, captureParamTys, false);
    std::vector<llvm::Value *> captureArgs = mockArgs;
    captureArgs.push_back(envPtr);

    builder_.SetInsertPoint(plainBB);
    llvm::Value *mockResultPlain = mockRetTy->isVoidTy()
        ? nullptr
        : builder_.CreateCall(mockFnTy, mockPtr, mockArgs, "mock_result_plain");
    if (mockRetTy->isVoidTy())
        builder_.CreateCall(mockFnTy, mockPtr, mockArgs);
    emitBranchUncond(mergeBB);
    llvm::BasicBlock *plainEndBB = builder_.GetInsertBlock();

    builder_.SetInsertPoint(captureBB);
    llvm::Value *mockResultCapture = mockRetTy->isVoidTy()
        ? nullptr
        : builder_.CreateCall(captureFnTy, mockPtr, captureArgs, "mock_result_capture");
    if (mockRetTy->isVoidTy())
        builder_.CreateCall(captureFnTy, mockPtr, captureArgs);
    emitBranchUncond(mergeBB);
    llvm::BasicBlock *captureEndBB = builder_.GetInsertBlock();

    // Original path: delegate to the supplied emitter (which emits its own args).
    builder_.SetInsertPoint(origBB);
    if (isSpied)
        builder_.CreateCall(mockIncFn, {nameStr});
    llvm::Value *origResult = emitter(*this, e);
    emitBranchUncond(mergeBB);
    llvm::BasicBlock *origEndBB = builder_.GetInsertBlock();

    builder_.SetInsertPoint(mergeBB);
    if (mockRetTy->isVoidTy())
        return llvm::ConstantInt::get(i8Ty_, 0);

    llvm::PHINode *phi = createPhi(mockRetTy, {}, "call_result");
    phi->addIncoming(mockResultPlain, plainEndBB);
    phi->addIncoming(mockResultCapture, captureEndBB);
    phi->addIncoming(origResult, origEndBB);
    propagateTypeMeta(sig.returnTypeName, phi);
    return phi;
}

} // namespace ry
