#include "ry/codegen.hpp"
#include "ry/runtime/core/regex_error_sentinels.hpp"
#include "ry/stdlib_registry.hpp"


namespace ry {

// Resource kind IDs (assigned at static init)
static int rk_tcp_listener, rk_tcp_stream, rk_tls_stream;
static int rk_http_request, rk_http_response, rk_http_client_response;
namespace {
struct NetResourceReg { NetResourceReg() {
    auto &r = ResourceKindRegistry::instance();
    rk_tcp_listener = r.registerKind("TcpListener", "__ry_arc_dtor_tcp_listener", "__ry_tcp_listener_cleanup", "net");
    rk_tcp_stream = r.registerKind("TcpStream", "__ry_arc_dtor_tcp_stream", "__ry_tcp_cleanup", "net");
    rk_tls_stream = r.registerKind("TlsStream", "__ry_arc_dtor_tls_stream", "__ry_tls_cleanup", "http",
                                   /*errorChannelLibrary=*/"tls");
    rk_http_request = r.registerKind("HttpRequest", "__ry_arc_dtor_http_request", "__ry_http_request_cleanup", "http");
    rk_http_response = r.registerKind("HttpResponse", "__ry_arc_dtor_http_response", "__ry_http_response_cleanup", "http");
    rk_http_client_response = r.registerKind("HttpClientResponse", "__ry_arc_dtor_http_client_response", "__ry_http_client_response_cleanup", "http");
}} net_resource_reg;
}

static int rk_file;
namespace {
struct FileResourceReg { FileResourceReg() {
    rk_file = ResourceKindRegistry::instance().registerKind(
        "File", "__ry_arc_dtor_file", "__ry_io_file_cleanup", "io");
}} file_resource_reg;
}

bool CodeGen::isFile(llvm::Value *val) {
    return isResourceKind(rk_file, val);
}

// ===== Builtin Regex =====

llvm::Value *CodeGen::emitBuiltinRegex(const CallExpr &e) {
    // Collect raw str args, then swap so index 0 = pattern, 1 = text.
    // After the swap, interleave each arg with its NUL-safe byte length.
    auto emitRegexCall = [&](const std::string &runtimeName,
                             const std::string &publicName, size_t nargs,
                             llvm::Type *retTy,
                             llvm::ArrayRef<llvm::Type *> argTys) -> llvm::Value * {
        requireArgs(e, nargs);
        std::vector<llvm::Value *> raw;
        for (size_t i = 0; i < nargs; ++i) {
            raw.push_back(emitExpr(*e.args[i]));
            if (!isStrLike(raw.back()))
                codegenError(publicName + "() requires str arguments");
        }
        // Legacy API accepts (text, pattern, ...) but runtime expects
        // (pattern, text, ...) — swap the first two arguments.
        if (nargs >= 2) std::swap(raw[0], raw[1]);
        // Build length-interleaved arg list: [arg0, len0, arg1, len1, ...]
        std::vector<llvm::Value *> args;
        args.reserve(nargs * 2);
        for (auto *a : raw) {
            args.push_back(a);
            args.push_back(emitStringByteLen(a));
        }
        auto fn = getRuntimeFn(("__ry_" + runtimeName).c_str(), retTy, argTys);
        return builder_.CreateCall(fn, args, runtimeName);
    };

    auto emitRegexRuntimeError = [&]() {
        auto errFn = getRuntimeFn("__ry_regex_get_last_error", ptrTy_, {});
        llvm::Value *msgPtr = builder_.CreateCall(errFn, {}, "regex_err_msg");
        emitRuntimeError("error: %s\n", ".regex_runtime_err", {msgPtr});
    };

    auto emitRegexI64Guard = [&](llvm::Value *result, int64_t errSentinel,
                                 const std::string &prefix) -> llvm::Value * {
        llvm::Value *isErr = builder_.CreateICmpEQ(
            result, llvm::ConstantInt::get(*ctx_, llvm::APInt(64, static_cast<uint64_t>(errSentinel), true)),
            prefix + "_is_err");
        llvm::BasicBlock *errBB = createBBInFn((prefix + ".err").c_str(), fn_);
        llvm::BasicBlock *okBB = createBBInFn((prefix + ".ok").c_str(), fn_);
        emitBranchCond(isErr, errBB, okBB);
        builder_.SetInsertPoint(errBB);
        emitRegexRuntimeError();
        builder_.SetInsertPoint(okBB);
        return result;
    };

    auto emitRegexPtrGuard = [&](llvm::Value *result, const std::string &prefix) -> llvm::Value * {
        llvm::Value *isNull = builder_.CreateICmpEQ(
            result, llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)),
            prefix + "_is_null");
        llvm::BasicBlock *errBB = createBBInFn((prefix + ".err").c_str(), fn_);
        llvm::BasicBlock *okBB = createBBInFn((prefix + ".ok").c_str(), fn_);
        emitBranchCond(isNull, errBB, okBB);
        builder_.SetInsertPoint(errBB);
        emitRegexRuntimeError();
        builder_.SetInsertPoint(okBB);
        return result;
    };

    // regexMatch(text, pattern) -> bool
    if (e.callee == "regexMatch") {
        llvm::Value *r = emitRegexCall("regex_match", "regexMatch", 2,
                                       i64Ty_, {ptrTy_, i64Ty_, ptrTy_, i64Ty_});
        r = emitRegexI64Guard(r, kRegexMatchError, "regex_match");
        return builder_.CreateTrunc(r, i1Ty_, "regex_match_bool");
    }
    // regexSearch(text, pattern) -> int
    if (e.callee == "regexSearch") {
        llvm::Value *r = emitRegexCall("regex_search", "regexSearch", 2,
                                       i64Ty_, {ptrTy_, i64Ty_, ptrTy_, i64Ty_});
        return emitRegexI64Guard(r, kRegexSearchError, "regex_search");
    }
    // regexReplace(text, pattern, replacement) -> str
    if (e.callee == "regexReplace")
        return emitRegexPtrGuard(
            emitRegexCall("regex_replace", "regexReplace", 3,
                          ptrTy_, {ptrTy_, i64Ty_, ptrTy_, i64Ty_, ptrTy_, i64Ty_}),
            "regex_replace");
    // regexSplit(text, pattern) -> List<str>
    if (e.callee == "regexSplit") {
        llvm::Value *r = emitRegexCall("regex_split", "regexSplit", 2,
                                       ptrTy_, {ptrTy_, i64Ty_, ptrTy_, i64Ty_});
        r = emitRegexPtrGuard(r, "regex_split");
        setTypeMeta(TypeMeta::ListElem, r, ptrTy_);
        return r;
    }
    // regexFindAll(text, pattern) -> List<Match>
    if (e.callee == "regexFindAll") {
        llvm::Value *r = emitRegexCall("regex_find_all", "regexFindAll", 2,
                                       ptrTy_, {ptrTy_, i64Ty_, ptrTy_, i64Ty_});
        r = emitRegexPtrGuard(r, "regex_find_all");
        setTypeMeta(TypeMeta::ListElem, r, record_types_["Match"].llvmType);
        getOrCreateMeta(r).list_elem_type_name = "Match";
        return r;
    }

    // --- Unprefixed regex functions (text-first for UFCS) ---
    // Emit __ry_regex_<rtName>(pattern, patternLen, text, textLen) from
    // UFCS (text, pattern) args where the second arg is a Regex value.
    // Regex values are StringHeader-backed so emitStringByteLen is safe (#1052).
    auto emitUfcsRegex = [&](const std::string &rtName,
                              llvm::Type *retTy,
                              llvm::ArrayRef<llvm::Type *> argTys) -> llvm::Value * {
        llvm::Value *text    = emitExpr(*e.args[0]);
        llvm::Value *pattern = emitExpr(*e.args[1]);
        if (!isRegex(pattern) || !isStrLike(text)) return nullptr;
        llvm::Value *patternLen = emitStringByteLen(pattern);
        llvm::Value *textLen    = emitStringByteLen(text);
        auto fn = getRuntimeFn(("__ry_" + rtName).c_str(), retTy, argTys);
        // Runtime expects (pattern, patternLen, text, textLen).
        llvm::Value *r = builder_.CreateCall(fn, {pattern, patternLen, text, textLen},
                                             rtName);
        if (retTy == i64Ty_) {
            int64_t sentinel = rtName == "regex_search" ? kRegexSearchError : kRegexMatchError;
            return emitRegexI64Guard(r, sentinel, rtName);
        }
        return emitRegexPtrGuard(r, rtName);
    };

    if (e.callee == "isMatch" && e.args.size() == 2) {
        if (auto *r = emitUfcsRegex("regex_is_match",
                                    i64Ty_, {ptrTy_, i64Ty_, ptrTy_, i64Ty_}))
            return builder_.CreateTrunc(r, i1Ty_, "is_match_bool");
    }
    if (e.callee == "search" && e.args.size() == 2) {
        if (auto *r = emitUfcsRegex("regex_search",
                                    i64Ty_, {ptrTy_, i64Ty_, ptrTy_, i64Ty_}))
            return r;
    }
    if (e.callee == "findAll" && e.args.size() == 2) {
        if (auto *r = emitUfcsRegex("regex_find_all",
                                    ptrTy_, {ptrTy_, i64Ty_, ptrTy_, i64Ty_})) {
            setTypeMeta(TypeMeta::ListElem, r, record_types_["Match"].llvmType);
            getOrCreateMeta(r).list_elem_type_name = "Match";
            return r;
        }
    }

    return nullptr;
}

// ===== File handle emitters =====

static llvm::Value *emitFileOpen(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 2);
    llvm::Value *path = cg.emitExpr(*e.args[0]);
    llvm::Value *mode = cg.emitExpr(*e.args[1]);
    if (!cg.isStrLike(path) || !cg.isStrLike(mode))
        cg.codegenError("open() requires str arguments (path, mode)");
    auto fn = cg.getRuntimeFn("__ry_io_file_open", cg.ptrTy_, {cg.ptrTy_, cg.ptrTy_});
    llvm::Value *ptr = cg.builder_.CreateCall(fn, {path, mode}, "file_open_ptr");
    llvm::Value *res = cg.wrapPtrAsResult(ptr, "__ry_io_get_last_error");
    cg.addResourceKind(res, rk_file);
    return res;
}

static llvm::Value *emitFileReadAll(CodeGen &cg, const CallExpr & /*e*/, llvm::Value *fileHandle) {
    auto fn = cg.getRuntimeFn("__ry_io_file_read_all", cg.ptrTy_, {cg.ptrTy_});
    llvm::Value *ptr = cg.builder_.CreateCall(fn, {fileHandle}, "file_read_all_ptr");
    return cg.wrapPtrAsResult(ptr, "__ry_io_get_last_error");
}

static llvm::Value *emitFileReadLine(CodeGen &cg, const CallExpr & /*e*/, llvm::Value *fileHandle) {
    llvm::Value *outAlloca = cg.builder_.CreateAlloca(cg.ptrTy_, nullptr, "rl_out");
    cg.builder_.CreateStore(
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(cg.ptrTy_)), outAlloca);
    auto fn = cg.getRuntimeFn("__ry_io_file_read_line", cg.i64Ty_, {cg.ptrTy_, cg.ptrTy_});
    llvm::Value *status = cg.builder_.CreateCall(fn, {fileHandle, outAlloca}, "rl_status");
    llvm::Value *isErr = cg.builder_.CreateICmpSLT(
        status, llvm::ConstantInt::get(cg.i64Ty_, 0), "rl_iserr");
    llvm::StructType *optTy = cg.getOptionType(cg.ptrTy_);
    llvm::StructType *resTy = cg.getResultType(optTy, cg.errorTy_);
    return cg.emitResultBranch(isErr, resTy,
        [&]() -> llvm::Value * {
            llvm::Value *linePtr = cg.builder_.CreateLoad(cg.ptrTy_, outAlloca, "rl_line");
            return cg.buildOkValue(cg.wrapPtrAsOption(linePtr, "readLine"), resTy);
        },
        [&]() -> llvm::Value * {
            return cg.buildErrValue(cg.buildErrorFromRuntime("__ry_io_get_last_error"), resTy);
        });
}

static llvm::Value *emitStdinReadLine(CodeGen &cg, const CallExpr & /*e*/) {
    llvm::Value *outAlloca = cg.builder_.CreateAlloca(cg.ptrTy_, nullptr, "srl_out");
    cg.builder_.CreateStore(
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(cg.ptrTy_)), outAlloca);
    auto fn = cg.getRuntimeFn("__ry_io_read_line", cg.i64Ty_, {cg.ptrTy_});
    llvm::Value *status = cg.builder_.CreateCall(fn, {outAlloca}, "srl_status");
    llvm::Value *isErr = cg.builder_.CreateICmpSLT(
        status, llvm::ConstantInt::get(cg.i64Ty_, 0), "srl_iserr");
    llvm::StructType *optTy = cg.getOptionType(cg.ptrTy_);
    llvm::StructType *resTy = cg.getResultType(optTy, cg.errorTy_);
    return cg.emitResultBranch(isErr, resTy,
        [&]() -> llvm::Value * {
            llvm::Value *linePtr = cg.builder_.CreateLoad(cg.ptrTy_, outAlloca, "srl_line");
            return cg.buildOkValue(cg.wrapPtrAsOption(linePtr, "readLine"), resTy);
        },
        [&]() -> llvm::Value * {
            return cg.buildErrValue(cg.buildErrorFromRuntime("__ry_io_get_last_error"), resTy);
        });
}

static llvm::Value *emitFileLines(CodeGen &cg, const CallExpr & /*e*/, llvm::Value *fileHandle, int fileRk) {
    // State struct: { ptr file } — iterator holds a retained reference to the
    // File handle so the underlying FILE* stays alive even if the user lets
    // their `f` binding go out of scope before iteration finishes.
    llvm::StructType *stateTy = llvm::StructType::get(*cg.ctx_,
        llvm::ArrayRef<llvm::Type *>{cg.ptrTy_});
    const llvm::DataLayout &dl = cg.mod_->getDataLayout();
    uint64_t stateSize = dl.getTypeAllocSize(stateTy);

    // Emit the next function: reads one line via __ry_io_file_read_line,
    // returns Some(line) on status==0, None on EOF / error / closed.
    llvm::StructType *optTy = cg.getOptionType(cg.ptrTy_);
    static int file_lines_counter = 0;
    std::string fnName = "__iter_file_lines_next." + std::to_string(file_lines_counter++);
    llvm::FunctionType *nextFnTy = llvm::FunctionType::get(optTy, {cg.ptrTy_}, false);
    llvm::Function *nextFn = llvm::Function::Create(
        nextFnTy, llvm::Function::ExternalLinkage, fnName, *cg.mod_);

    {
        CodeGen::FnScope guard(cg);
        cg.fn_ = nextFn;
        cg.pushScope();
        llvm::BasicBlock *entry = cg.createBBInFn("entry", nextFn);
        cg.builder_.SetInsertPoint(entry);

        llvm::Value *statePtr = nextFn->getArg(0);
        llvm::Value *file = cg.builder_.CreateLoad(cg.ptrTy_,
            cg.builder_.CreateStructGEP(stateTy, statePtr, 0), "file");

        llvm::Value *outAlloca = cg.builder_.CreateAlloca(cg.ptrTy_, nullptr, "fl_out");
        cg.builder_.CreateStore(
            llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(cg.ptrTy_)),
            outAlloca);

        auto readLineFn = cg.getRuntimeFn("__ry_io_file_read_line", cg.i64Ty_, {cg.ptrTy_, cg.ptrTy_});
        llvm::Value *status = cg.builder_.CreateCall(
            readLineFn, {file, outAlloca}, "fl_status");
        llvm::Value *isLine = cg.builder_.CreateICmpEQ(
            status, llvm::ConstantInt::get(cg.i64Ty_, 0), "fl_isline");

        llvm::BasicBlock *someBB = cg.createBBInFn("some", nextFn);
        llvm::BasicBlock *noneBB = cg.createBBInFn("none", nextFn);
        cg.emitBranchCond(isLine, someBB, noneBB);

        cg.builder_.SetInsertPoint(someBB);
        llvm::Value *line = cg.builder_.CreateLoad(cg.ptrTy_, outAlloca, "fl_line");
        cg.builder_.CreateRet(cg.buildSomeValue(line, optTy));

        cg.builder_.SetInsertPoint(noneBB);
        cg.builder_.CreateRet(cg.buildNoneValue(optTy));
        cg.popScope();
    }

    // Allocate the iterator state and retain the File handle into it.
    auto mallocFn = cg.getStdlibMalloc();
    llvm::Value *stateAlloc = cg.builder_.CreateCall(
        mallocFn, {llvm::ConstantInt::get(cg.i64Ty_, stateSize)}, "lines_state");
    cg.iterator_malloc_stack_.back().push_back(stateAlloc);

    // ARC retain: the iterator must outlive the user's `f` binding.
    llvm::Value *fileHdr = cg.emitArcGetHeaderFromData(fileHandle);
    bool atomic = cg.isArcAtomic(fileHandle);
    cg.emitArcRetain(fileHdr, atomic);
    cg.iterator_release_hooks_.back().push_back({fileHandle, fileRk});

    cg.builder_.CreateStore(fileHandle,
        cg.builder_.CreateStructGEP(stateTy, stateAlloc, 0));

    llvm::Value *header = nullptr;
    {
        uint64_t headerSize = dl.getTypeAllocSize(cg.iteratorHeaderTy_);
        header = cg.builder_.CreateCall(
            mallocFn, {llvm::ConstantInt::get(cg.i64Ty_, headerSize)}, "lines_header");
        cg.builder_.CreateStore(nextFn,
            cg.builder_.CreateStructGEP(cg.iteratorHeaderTy_, header, 0));
        cg.builder_.CreateStore(stateAlloc,
            cg.builder_.CreateStructGEP(cg.iteratorHeaderTy_, header, 1));
        cg.setTypeMeta(CodeGen::TypeMeta::IteratorElem, header, cg.ptrTy_);
        cg.iterator_malloc_stack_.back().push_back(header);
    }
    // Tag the iterator's element type as str so downstream consumers
    // (`for line in ...`, `toList`) treat each value as a Ry string handle.
    cg.getOrCreateMeta(header).list_elem_type_name = "str";
    return header;
}

static llvm::Value *emitFileWriteText(CodeGen &cg, const CallExpr &e, llvm::Value *fileHandle) {
    llvm::Value *s = cg.emitExpr(*e.args[1]);
    if (!cg.isStrLike(s))
        cg.codegenError("writeText(file, s): second argument must be str");
    auto fn = cg.getRuntimeFn("__ry_io_file_write_text", cg.i64Ty_, {cg.ptrTy_, cg.ptrTy_});
    llvm::Value *status = cg.builder_.CreateCall(fn, {fileHandle, s}, "file_wt_status");
    return cg.wrapStatusAsResult(status, "__ry_io_get_last_error");
}

// ===== Builtin IO =====

// Sig-presence gate for dispatchIO. Prefix scan over native_fn_sigs_ for
// "io::*" — the canonical key populated both by `from io import ...`
// (ModuleLoader path) and `@native("io") fn ...` declarations. Post-#2338
// the bare-name fallback list that #2332 originally added is no longer
// needed: declarative entries dispatch through emitGenericNativeCall
// (which does its own sig-key lookup), all C++ test harnesses inline
// io @native decls with the explicit `@native("io")` tag, and the
// remaining custom-emitter branches below do their own arity/callee
// matching that is harmless when the gate over-passes for unrelated
// callees.
static bool isIoImported(CodeGen &cg) {
    const auto &sigs = cg.getNativeFnSigs();
    for (const auto &kv : sigs) {
        if (kv.first.rfind("io::", 0) == 0)  // C++17 prefix-check idiom
            return true;
    }
    return false;
}

RY_REGISTER_STDLIB_PACKAGE_NAMING(io, "share/std/io/io.ry", dispatchIO, /*snake_case=*/true)
static llvm::Value *dispatchIO(CodeGen &cg, const CallExpr &e) {
    // Gate: skip when io isn't in the program's import set, so a sibling
    // dispatcher fall-through (e.g. base64-only program reaching dispatchIO
    // through the StdlibRegistry loop) returns nullptr without side effects
    // (#2299 close criterion 2).
    if (!isIoImported(cg))
        return nullptr;

    // Per-emit insert for the remaining custom branches (open and the
    // File-coupled overloads). The Group A declarative entries that #2338
    // migrated to descriptor-driven dispatch register the library
    // automatically inside emitGenericNativeCall's matched-sig branch —
    // this lambda covers only the paths that bypass that consume site.
    auto markIo = [&](llvm::Value *v) -> llvm::Value * {
        if (v) cg.used_native_libraries_.insert("io");
        return v;
    };

    const auto &n = e.callee;
    const auto sz = e.args.size();

    // open(path, mode) — runtime symbol is __ry_io_file_open, which does
    // not follow the __ry_io_<snake>(callee) convention emitGenericNativeCall
    // derives. Stays a custom emitter pending the future @native("io",
    // symbol="...") descriptor field (architecture doc §"Pilot"). The
    // descriptor's resource_kind is still populated at @native registration
    // time (rk_file via inferResourceKind) but is not read on this path —
    // emitFileOpen continues the manual addResourceKind call.
    if (n == "open" && sz == 2)
        return markIo(emitFileOpen(cg, e));

    // readAll(f: File) — 1-arg (0-arg stdin is descriptor-driven post-#2338)
    if (n == "readAll" && sz == 1) {
        llvm::Value *arg0 = cg.emitExpr(*e.args[0]);
        if (!cg.isFile(arg0))
            cg.codegenError("readAll(f): argument must be a File handle; "
                            "use readAll() with no arguments to read from stdin");
        return markIo(emitFileReadAll(cg, e, arg0));
    }

    // readLine() — 0-arg stdin reader, returns Result<Option<str>, Error>
    if (n == "readLine" && sz == 0)
        return markIo(emitStdinReadLine(cg, e));

    // readLine(f: File) — 1-arg
    if (n == "readLine" && sz == 1) {
        llvm::Value *arg0 = cg.emitExpr(*e.args[0]);
        if (!cg.isFile(arg0))
            cg.codegenError("readLine(f): argument must be a File handle; "
                            "use readLine() with no arguments to read from stdin");
        return markIo(emitFileReadLine(cg, e, arg0));
    }

    // lines(f: File) -> Iterator<str>
    if (n == "lines" && sz == 1) {
        llvm::Value *arg0 = cg.emitExpr(*e.args[0]);
        if (!cg.isFile(arg0))
            cg.codegenError("lines(f): argument must be a File handle");
        return markIo(emitFileLines(cg, e, arg0, rk_file));
    }

    // writeText — 2-arg: dispatch the File overload here; inline the
    // str/str overload too, because the type-check requires emitting arg0
    // and returning nullptr would force emitGenericNativeCall to re-emit
    // all args (double side effect on the str payload). Both branches go
    // through __ry_io_<snake>(callee) so the runtime symbol matches what
    // emitGenericNativeCall would derive — keeping the inline path here
    // is purely a side-effect-preservation choice.
    if (n == "writeText" && sz == 2) {
        llvm::Value *arg0 = cg.emitExpr(*e.args[0]);
        if (cg.isFile(arg0))
            return markIo(emitFileWriteText(cg, e, arg0));
        // Non-File: inline str-based writeText (arg0 already emitted)
        llvm::Value *content = cg.emitExpr(*e.args[1]);
        if (!cg.isStrLike(arg0) || !cg.isStrLike(content))
            cg.codegenError("writeText(path, content) requires str arguments");
        auto fn = cg.getRuntimeFn("__ry_io_write_text", cg.i64Ty_, {cg.ptrTy_, cg.ptrTy_});
        llvm::Value *status = cg.builder_.CreateCall(fn, {arg0, content}, "write_text_status");
        return markIo(cg.wrapStatusAsResult(status, "__ry_io_get_last_error"));
    }

    // readText / appendText / writeBytes / deleteFile / exists / readBytes /
    // bytesToStr / toBytes / readAll() (0-arg) — all declarative entries
    // fall through to emitGenericNativeCall (descriptor-driven path).
    return nullptr;
}

// ===== Shared helper: ptr → Result<T, Error> with static error message =====

llvm::Value *CodeGen::emitPtrToResult(llvm::Value *ptr, const std::string &name,
                                       const std::string &errMsg, int rk) {
    llvm::Value *isNull = builder_.CreateICmpEQ(ptr,
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)), name + "_null");
    llvm::StructType *resTy = getResultType(ptrTy_, errorTy_);
    llvm::Value *okVal = buildOkValue(ptr, resTy);
    llvm::Value *errVal = buildErrValue(buildStaticError(errMsg, "." + name + "_err_msg"), resTy);
    llvm::Value *res = builder_.CreateSelect(isNull, errVal, okVal, name + "_result");
    addResourceKind(res, rk);
    return res;
}

// ===== Net custom emitters =====

// bind / connect / tlsConnect were migrated to descriptor-driven dispatch in
// #2339 (Installment 2-b); their static emitters were deleted along with
// net_table[]. The handle-coupled overloads below remain custom because
// emitGenericNativeCall has no typed-handle check at consume time (#2338
// kept io's File-coupled overloads custom for the same reason).

static llvm::Value *emitNetTcpListen(CodeGen &cg, const CallExpr &e) {
    // Guard: TCP listen is 2-arg only; 3+ arg calls are HTTP listen
    if (e.args.size() != 2) return nullptr;
    llvm::Value *listener = cg.emitExpr(*e.args[0]);
    if (!cg.isTcpListener(listener))
        cg.codegenError("listen() requires TcpListener as first argument");
    llvm::Value *backlog = cg.emitExpr(*e.args[1]);
    auto fn = cg.getRuntimeFn("__ry_listen", cg.i64Ty_, {cg.ptrTy_, cg.i64Ty_});
    llvm::Value *status = cg.builder_.CreateCall(fn, {listener, backlog}, "listen_status");
    llvm::Value *isErr = cg.emitICmpNE(status,
        llvm::ConstantInt::get(cg.i64Ty_, 0), "listen_err");
    llvm::StructType *resTy = cg.getResultType(cg.i8Ty_, cg.errorTy_);
    llvm::Value *okVal = cg.buildOkValue(llvm::ConstantInt::get(cg.i8Ty_, 0), resTy);
    llvm::Value *errVal = cg.buildErrValue(cg.buildStaticError("listen failed", ".listen_err_msg"), resTy);
    return cg.emitSelect(isErr, errVal, okVal, "listen_result");
}

static llvm::Value *emitNetAccept(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *listener = cg.emitExpr(*e.args[0]);
    if (!cg.isTcpListener(listener))
        cg.codegenError("accept() requires TcpListener argument");
    auto fn = cg.getRuntimeFn("__ry_accept", cg.ptrTy_, {cg.ptrTy_});
    llvm::Value *result = cg.builder_.CreateCall(fn, {listener}, "accept_result");
    llvm::Value *res = cg.wrapPtrAsResult(result, "__ry_net_get_last_error");
    cg.addResourceKind(res, rk_tcp_stream);
    return res;
}

static llvm::Value *emitNetListenerPort(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *listener = cg.emitExpr(*e.args[0]);
    if (!cg.isTcpListener(listener))
        cg.codegenError("listenerPort() requires TcpListener argument");
    auto fn = cg.getRuntimeFn("__ry_listener_port", cg.i64Ty_, {cg.ptrTy_});
    return cg.builder_.CreateCall(fn, {listener}, "listener_port");
}

static llvm::Value *emitNetShutdown(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *val = cg.emitExpr(*e.args[0]);
    if (!cg.isTcpListener(val))
        cg.codegenError("shutdown() requires TcpListener argument");
    auto fn = cg.getRuntimeFn("__ry_tcp_listener_shutdown", llvm::Type::getVoidTy(*cg.ctx_), {cg.ptrTy_});
    return cg.builder_.CreateCall(fn, {val});
}

static llvm::Value *emitNetTimeout(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 2);
    llvm::Value *stream = cg.emitExpr(*e.args[0]);
    llvm::Value *ms = cg.emitExpr(*e.args[1]);
    if (!cg.isTcpStream(stream) && !cg.isTlsStream(stream))
        cg.codegenError(e.callee + "() requires TcpStream or TlsStream as first argument");
    bool isTls = cg.isTlsStream(stream);
    std::string prefix = isTls ? "__ry_tls_" : "__ry_tcp_";
    cg.used_native_libraries_.insert(isTls ? "http" : "net");
    std::string rtName;
    if (e.callee == "setTimeout")             rtName = "set_timeout";
    else if (e.callee == "setReceiveTimeout") rtName = "set_recv_timeout";
    else if (e.callee == "setSendTimeout")    rtName = "set_send_timeout";
    else                                      rtName = e.callee;
    auto fn = cg.getRuntimeFn((prefix + rtName).c_str(),
                              llvm::Type::getVoidTy(*cg.ctx_), {cg.ptrTy_, cg.i64Ty_});
    return cg.builder_.CreateCall(fn, {stream, ms});
}

// ===== Net dispatcher =====

// Gate: skip when net isn't in the program's import set, so a sibling
// dispatcher fall-through (e.g. http-only program reaching dispatchNet
// through the StdlibRegistry loop) returns nullptr without side effects.
// Mirrors dispatchIO's gate (#2338).
static bool isNetImported(CodeGen &cg) {
    const auto &sigs = cg.getNativeFnSigs();
    for (const auto &kv : sigs) {
        if (kv.first.rfind("net::", 0) == 0)  // C++17 prefix-check idiom
            return true;
    }
    return false;
}

// Priority 50: net must dispatch before http (priority 100) because
// net::listen (2-arg) falls through to http::listen (3+-arg).
// snake_case=true: tlsConnect → __ry_net_tls_connect for the descriptor-
// driven entries (#2339). The handle-coupled custom branches below use
// hand-named symbols (__ry_listen / __ry_accept / __ry_tcp_set_timeout
// etc.) and bypass the symbol derivation entirely.
RY_REGISTER_STDLIB_PACKAGE_FULL(net, "share/std/net/net.ry", dispatchNet, 50, true)
static llvm::Value *dispatchNet(CodeGen &cg, const CallExpr &e) {
    if (!isNetImported(cg))
        return nullptr;

    // Per-emit insert for the remaining custom branches (handle-coupled
    // overloads). The Group A declarative entries that #2339 migrated to
    // descriptor-driven dispatch register the library automatically inside
    // emitGenericNativeCall — this lambda covers only the paths that
    // bypass that consume site.
    auto markNet = [&](llvm::Value *v) -> llvm::Value * {
        if (v) cg.used_native_libraries_.insert("net");
        return v;
    };

    const auto &n = e.callee;
    const auto sz = e.args.size();

    // listen(listener: TcpListener, backlog: int) — TcpListener handle
    // check + static error string. 3+ arg `listen` (the HTTP overload)
    // returns nullptr below to fall through to dispatchHttp.
    if (n == "listen" && sz == 2)
        return markNet(emitNetTcpListen(cg, e));

    // accept / listenerPort / shutdown — TcpListener handle check.
    if (n == "accept" && sz == 1)
        return markNet(emitNetAccept(cg, e));
    if (n == "listenerPort" && sz == 1)
        return markNet(emitNetListenerPort(cg, e));
    if (n == "shutdown" && sz == 1)
        return markNet(emitNetShutdown(cg, e));

    // setTimeout / setReceiveTimeout / setSendTimeout — overload + library
    // switching (TcpStream → net, TlsStream → http). emitNetTimeout does
    // its own `used_native_libraries_` insert; do not double-mark.
    if ((n == "setTimeout" || n == "setReceiveTimeout" || n == "setSendTimeout")
        && sz == 2)
        return emitNetTimeout(cg, e);

    // bind / connect / tlsConnect — descriptor-driven; fall through to
    // emitGenericNativeCall. tlsConnect's TlsStream resource_kind drives
    // the error_channel override to __ry_tls_get_last_error and adds the
    // http library to used_native_libraries_ at consume time (#2339).
    return nullptr;
}

// ===== Http custom emitters =====

// Emit a NUL check on a Ry str value. Returns an i1: true if NUL found (= Err path).
static llvm::Value *emitHttpNulCheck(CodeGen &cg, llvm::Value *strVal, const std::string &hint) {
    auto nulFn = cg.getRuntimeFn("__ry_http_str_has_nul", cg.i64Ty_, {cg.ptrTy_});
    llvm::Value *hasNul = cg.builder_.CreateCall(nulFn, {strVal}, hint + "_has_nul");
    return cg.builder_.CreateICmpNE(hasNul, llvm::ConstantInt::get(cg.i64Ty_, 0), hint + "_is_nul");
}

// `response` was migrated to descriptor-driven dispatch in #2339; its
// static emitter was deleted along with http_table[]. The runtime symbol
// was renamed `__ry_http_response_create` → `__ry_http_response` to match
// the `__ry_<module>_<snake>(callee)` convention emitGenericNativeCall
// derives. The descriptor populates rk_http_response from the return type
// (Result<HttpResponse, Error>) automatically.

static llvm::Value *emitHttpRequestStr(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *req = cg.emitExpr(*e.args[0]);
    if (!cg.isHttpRequest(req))
        cg.codegenError(e.callee + "() requires HttpRequest argument");
    auto fn = cg.getRuntimeFn(("__ry_http_" + e.callee).c_str(), cg.ptrTy_, {cg.ptrTy_});
    return cg.builder_.CreateCall(fn, {req}, e.callee);
}

static llvm::Value *emitHttpBody(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *arg = cg.emitExpr(*e.args[0]);
    if (cg.isHttpRequest(arg)) {
        auto fn = cg.getRuntimeFn("__ry_http_body", cg.ptrTy_, {cg.ptrTy_});
        return cg.builder_.CreateCall(fn, {arg}, "body");
    }
    if (cg.isHttpClientResponse(arg)) {
        auto fn = cg.getRuntimeFn("__ry_http_client_body", cg.ptrTy_, {cg.ptrTy_});
        return cg.builder_.CreateCall(fn, {arg}, "body");
    }
    cg.codegenError("body() requires HttpRequest or HttpClientResponse argument");
}

static llvm::Value *emitHttpBodyBytes(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *arg = cg.emitExpr(*e.args[0]);
    const char *rtName;
    if (cg.isHttpRequest(arg))
        rtName = "__ry_http_body_bytes";
    else if (cg.isHttpClientResponse(arg))
        rtName = "__ry_http_client_body_bytes";
    else
        cg.codegenError("bodyBytes() requires HttpRequest or HttpClientResponse argument");
    auto fn = cg.getRuntimeFn(rtName, cg.ptrTy_, {cg.ptrTy_});
    llvm::Value *result = cg.builder_.CreateCall(fn, {arg}, "body_bytes");
    cg.setTypeMeta(CodeGen::TypeMeta::ListElem, result, cg.i8Ty_);
    return result;
}

static llvm::Value *emitHttpHeader(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 2);
    llvm::Value *arg = cg.emitExpr(*e.args[0]);
    llvm::Value *key = cg.emitExpr(*e.args[1]);
    if (key->getType() != cg.ptrTy_)
        cg.codegenError("header() key must be str");

    llvm::Value *isNul = emitHttpNulCheck(cg, key, "hdr_key");
    llvm::StructType *optTy = cg.getOptionType(cg.ptrTy_);
    llvm::StructType *resTy = cg.getResultType(optTy, cg.errorTy_);
    static int hdrNulCtr = 0;

    if (cg.isHttpRequest(arg)) {
        return cg.emitResultBranch(isNul, resTy,
            [&]() {
                auto fn = cg.getRuntimeFn("__ry_http_header", cg.ptrTy_, {cg.ptrTy_, cg.ptrTy_});
                llvm::Value *r = cg.builder_.CreateCall(fn, {arg, key}, "http_hdr");
                return cg.buildOkValue(cg.wrapPtrAsOption(r, "http_hdr"), resTy);
            },
            [&]() {
                return cg.buildErrValue(
                    cg.buildStaticError("header: key contains embedded NUL",
                                        ".http_hdr_nul_" + std::to_string(hdrNulCtr++)), resTy);
            });
    }
    if (cg.isHttpClientResponse(arg)) {
        return cg.emitResultBranch(isNul, resTy,
            [&]() {
                auto fn = cg.getRuntimeFn("__ry_http_client_header", cg.ptrTy_, {cg.ptrTy_, cg.ptrTy_});
                llvm::Value *r = cg.builder_.CreateCall(fn, {arg, key}, "http_client_hdr");
                return cg.buildOkValue(cg.wrapPtrAsOption(r, "http_client_hdr"), resTy);
            },
            [&]() {
                return cg.buildErrValue(
                    cg.buildStaticError("header: key contains embedded NUL",
                                        ".http_hdr_nul_" + std::to_string(hdrNulCtr++)), resTy);
            });
    }
    cg.codegenError("header() requires HttpRequest or HttpClientResponse argument");
}

static llvm::Value *emitHttpOptionField(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 2);
    llvm::Value *req = cg.emitExpr(*e.args[0]);
    llvm::Value *key = cg.emitExpr(*e.args[1]);
    if (!cg.isHttpRequest(req))
        cg.codegenError(e.callee + "() requires HttpRequest argument");
    if (key->getType() != cg.ptrTy_) {
        std::string param = (e.callee == "cookie") ? "name" : "key";
        cg.codegenError(e.callee + "() " + param + " must be str");
    }
    std::string hint = (e.callee == "query") ? "http_qry"
                     : (e.callee == "cookie") ? "http_ck" : "http_ff";
    // Map camelCase callee to snake_case runtime symbol name.
    std::string rtName = (e.callee == "formField") ? "form_field" : e.callee;

    llvm::Value *isNul = emitHttpNulCheck(cg, key, hint);
    llvm::StructType *optTy = cg.getOptionType(cg.ptrTy_);
    llvm::StructType *resTy = cg.getResultType(optTy, cg.errorTy_);
    static int optNulCtr = 0;
    std::string errMsg = e.callee + ": key contains embedded NUL";
    std::string errName = ".http_opt_nul_" + std::to_string(optNulCtr++);

    auto fn = cg.getRuntimeFn(("__ry_http_" + rtName).c_str(), cg.ptrTy_, {cg.ptrTy_, cg.ptrTy_});
    return cg.emitResultBranch(isNul, resTy,
        [&]() {
            llvm::Value *r = cg.builder_.CreateCall(fn, {req, key}, hint);
            return cg.buildOkValue(cg.wrapPtrAsOption(r, hint), resTy);
        },
        [&]() {
            return cg.buildErrValue(cg.buildStaticError(errMsg, errName), resTy);
        });
}

static llvm::Value *emitHttpMapAll(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *req = cg.emitExpr(*e.args[0]);
    if (!cg.isHttpRequest(req))
        cg.codegenError(e.callee + "() requires HttpRequest argument");
    // Map camelCase callee to snake_case runtime symbol name.
    std::string rtName;
    if (e.callee == "queryAll")        rtName = "query_all";
    else if (e.callee == "formFields") rtName = "form_fields";
    else                               rtName = e.callee;
    auto fn = cg.getRuntimeFn(("__ry_http_" + rtName).c_str(), cg.ptrTy_, {cg.ptrTy_});
    llvm::Value *result = cg.builder_.CreateCall(fn, {req}, "http_" + rtName);
    cg.setTypeMeta(CodeGen::TypeMeta::MapKey, result, cg.ptrTy_);
    cg.setTypeMeta(CodeGen::TypeMeta::MapValue, result, cg.ptrTy_);
    return result;
}

static llvm::Value *emitHttpFormFile(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 2);
    llvm::Value *req = cg.emitExpr(*e.args[0]);
    llvm::Value *key = cg.emitExpr(*e.args[1]);
    if (!cg.isHttpRequest(req))
        cg.codegenError("formFile() requires HttpRequest argument");
    if (key->getType() != cg.ptrTy_)
        cg.codegenError("formFile() name must be str");

    llvm::Value *isNul = emitHttpNulCheck(cg, key, "ff_key");
    llvm::StructType *optTy = cg.getOptionType(cg.ptrTy_);
    llvm::StructType *resTy = cg.getResultType(optTy, cg.errorTy_);
    static int ffNulCtr = 0;

    auto fn = cg.getRuntimeFn("__ry_http_form_file", cg.ptrTy_, {cg.ptrTy_, cg.ptrTy_});
    llvm::Value *res = cg.emitResultBranch(isNul, resTy,
        [&]() {
            llvm::Value *r = cg.builder_.CreateCall(fn, {req, key}, "http_ffile");
            cg.setTypeMeta(CodeGen::TypeMeta::MapKey, r, cg.ptrTy_);
            cg.setTypeMeta(CodeGen::TypeMeta::MapValue, r, cg.ptrTy_);
            llvm::Value *opt = cg.wrapPtrAsOption(r, "http_ffile");
            return cg.buildOkValue(opt, resTy);
        },
        [&]() {
            return cg.buildErrValue(
                cg.buildStaticError("formFile: name contains embedded NUL",
                                    ".http_ff_nul_" + std::to_string(ffNulCtr++)), resTy);
        });
    cg.setTypeMeta(CodeGen::TypeMeta::MapKey, res, cg.ptrTy_);
    cg.setTypeMeta(CodeGen::TypeMeta::MapValue, res, cg.ptrTy_);
    return res;
}

static llvm::Value *emitResultBranchWithMeta(
    CodeGen &cg,
    llvm::Value *isErr,
    llvm::StructType *resTy,
    llvm::function_ref<llvm::Value *(llvm::Value *&)> buildOk,
    llvm::function_ref<llvm::Value *()> buildErr) {
    llvm::Value *okIncoming = nullptr;
    llvm::Value *merged = cg.emitResultBranch(isErr, resTy,
        [&]() { return buildOk(okIncoming); },
        [&]() { return buildErr(); });
    if (okIncoming)
        cg.propagateMeta(okIncoming, merged);
    return merged;
}

static llvm::Value *emitHttpListen(CodeGen &cg, const CallExpr &e) {
    cg.used_native_libraries_.insert("net");
    cg.used_native_libraries_.insert("http");
    if (e.args.size() < 3 || e.args.size() > 5)
        cg.codegenError("listen() takes 3 to 5 arguments");
    llvm::Value *host = cg.emitExpr(*e.args[0]);
    llvm::Value *port = cg.emitExpr(*e.args[1]);
    llvm::Value *handler = cg.emitExpr(*e.args[2]);
    if (host->getType() != cg.ptrTy_)
        cg.codegenError("listen() host must be str");
    if (port->getType() != cg.i64Ty_)
        cg.codegenError("listen() port must be int");

    auto *fnInfo = cg.lookupFnTypeInfo(handler);
    if (!fnInfo)
        cg.codegenError("listen() handler must be fn(HttpRequest) -> Result<HttpResponse, Error>");
    CodeGen::FnTypeInfo handlerInfo = *fnInfo;

    if (handlerInfo.paramTypes.size() != 1 || handlerInfo.paramTypes[0] != cg.ptrTy_)
        cg.codegenError("listen() handler must be fn(HttpRequest) -> Result<HttpResponse, Error>");
    bool handlerReturnsResult = cg.isResultType(handlerInfo.returnType);
    if (handlerReturnsResult) {
        if (cg.resolveTypeAlias(handlerInfo.returnTypeName) != "Result<HttpResponse, Error>")
            cg.codegenError("listen() handler must return HttpResponse or Result<HttpResponse, Error>");
    } else if (handlerInfo.returnType != cg.ptrTy_ ||
               cg.resolveTypeAlias(handlerInfo.returnTypeName) != "HttpResponse") {
        cg.codegenError("listen() handler must return HttpResponse or Result<HttpResponse, Error>");
    }

    llvm::Value *maxReqs = llvm::ConstantInt::get(cg.i64Ty_, 0);
    if (e.args.size() >= 4) {
        maxReqs = cg.emitExpr(*e.args[3]);
        if (maxReqs->getType() != cg.i64Ty_)
            cg.codegenError("listen() maxRequests must be int");
        if (auto *maxConst = llvm::dyn_cast<llvm::ConstantInt>(maxReqs)) {
            if (maxConst->getSExtValue() <= 0)
                cg.codegenError("listen() maxRequests must be a positive integer");
        }
    }

    llvm::Value *portCallback = nullptr;
    if (e.args.size() == 5) {
        portCallback = cg.emitExpr(*e.args[4]);
        if (portCallback->getType() != cg.ptrTy_)
            cg.codegenError("listen() portCallback must be fn(int) -> Unit");
    }

    // Return type: Result<Unit, Error>
    llvm::StructType *unitResTy = cg.getResultType(cg.i8Ty_, cg.errorTy_);
    // Alloca to hold the final result (avoids complex PHI over multiple error paths)
    llvm::Value *resultAlloca = cg.builder_.CreateAlloca(unitResTy, nullptr, "listen_result");
    llvm::BasicBlock *returnBB = cg.createBB("http.listen_return");

    // 1. bind(host, port)
    auto bindFn = cg.getRuntimeFn("__ry_net_bind", cg.ptrTy_, {cg.ptrTy_, cg.i64Ty_});
    llvm::Value *listener = cg.builder_.CreateCall(bindFn, {host, port}, "http_listener");

    llvm::Value *isNull = cg.builder_.CreateICmpEQ(listener,
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(cg.ptrTy_)), "bind_null");
    llvm::BasicBlock *bindFailBB = cg.createBB("http.bind_fail");
    llvm::BasicBlock *bindOkBB = cg.createBB("http.bind_ok");
    cg.emitBranchCond(isNull, bindFailBB, bindOkBB);

    cg.builder_.SetInsertPoint(bindFailBB);
    {
        llvm::Value *errVal = cg.buildErrValue(
            cg.buildErrorFromRuntime("__ry_net_get_last_error"), unitResTy);
        cg.builder_.CreateStore(errVal, resultAlloca);
        cg.emitBranchUncond(returnBB);
    }

    // 2. listen(listener, 128)
    cg.builder_.SetInsertPoint(bindOkBB);
    auto listenFn = cg.getRuntimeFn("__ry_listen", cg.i64Ty_, {cg.ptrTy_, cg.i64Ty_});
    llvm::Value *listenStatus = cg.builder_.CreateCall(
        listenFn, {listener, llvm::ConstantInt::get(cg.i64Ty_, 128)}, "listen_status");
    llvm::Value *listenFailed = cg.builder_.CreateICmpNE(listenStatus,
        llvm::ConstantInt::get(cg.i64Ty_, 0), "listen_failed");
    llvm::BasicBlock *listenFailBB = cg.createBB("http.listen_fail");
    llvm::BasicBlock *listenOkBB = cg.createBB("http.listen_ok");
    cg.emitBranchCond(listenFailed, listenFailBB, listenOkBB);

    cg.builder_.SetInsertPoint(listenFailBB);
    {
        auto earlyCloseFn = cg.getRuntimeFn("__ry_tcp_listener_close", llvm::Type::getVoidTy(*cg.ctx_), {cg.ptrTy_});
        cg.builder_.CreateCall(earlyCloseFn, {listener});
        static int listenErrCtr = 0;
        llvm::Value *errVal = cg.buildErrValue(
            cg.buildStaticError("listen failed", ".http_listen_err_" + std::to_string(listenErrCtr++)),
            unitResTy);
        cg.builder_.CreateStore(errVal, resultAlloca);
        cg.emitBranchUncond(returnBB);
    }

    cg.builder_.SetInsertPoint(listenOkBB);

    if (portCallback) {
        auto portFn = cg.getRuntimeFn("__ry_listener_port", cg.i64Ty_, {cg.ptrTy_});
        llvm::Value *actualPort = cg.builder_.CreateCall(portFn, {listener}, "actual_port");
        auto callbackFnTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*cg.ctx_), {cg.i64Ty_}, false);
        cg.builder_.CreateCall(callbackFnTy, portCallback, {actualPort});
    }

    bool hasMaxRequests = (e.args.size() >= 4);
    llvm::Value *counterAlloca = nullptr;
    if (hasMaxRequests) {
        counterAlloca = cg.builder_.CreateAlloca(cg.i64Ty_, nullptr, "req_counter");
        cg.builder_.CreateStore(llvm::ConstantInt::get(cg.i64Ty_, 0), counterAlloca);
    }

    auto *voidTy = llvm::Type::getVoidTy(*cg.ctx_);
    auto closeFn = cg.getRuntimeFn("__ry_tcp_close", voidTy, {cg.ptrTy_});
    auto listenerCloseFn = cg.getRuntimeFn("__ry_tcp_listener_close", voidTy, {cg.ptrTy_});
    auto freeReqFn = cg.getRuntimeFn("__ry_http_request_free", voidTy, {cg.ptrTy_});
    auto freeRespFn = cg.getRuntimeFn("__ry_http_response_free", voidTy, {cg.ptrTy_});

    // 3. accept loop
    llvm::BasicBlock *loopBB = cg.createBB("http.loop");
    llvm::BasicBlock *loopBodyBB = cg.createBB("http.loop_body");
    llvm::BasicBlock *loopEndBB = cg.createBB("http.loop_end");

    cg.emitBranchUncond(loopBB);
    cg.builder_.SetInsertPoint(loopBB);

    auto acceptFn = cg.getRuntimeFn("__ry_accept", cg.ptrTy_, {cg.ptrTy_});
    llvm::Value *conn = cg.builder_.CreateCall(acceptFn, {listener}, "http_conn");

    llvm::Value *connNull = cg.builder_.CreateICmpEQ(conn,
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(cg.ptrTy_)), "conn_null");
    cg.emitBranchCond(connNull, loopBB, loopBodyBB);

    cg.builder_.SetInsertPoint(loopBodyBB);

    auto readReqFn = cg.getRuntimeFn("__ry_http_read_request", cg.ptrTy_, {cg.ptrTy_});
    llvm::Value *req = cg.builder_.CreateCall(readReqFn, {conn}, "http_req");
    cg.addResourceKind(req, rk_http_request);

    llvm::Value *reqNull = cg.builder_.CreateICmpEQ(req,
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(cg.ptrTy_)), "req_null");
    llvm::BasicBlock *reqOkBB = cg.createBB("http.req_ok");
    llvm::BasicBlock *reqBadBB = cg.createBB("http.req_bad");
    cg.emitBranchCond(reqNull, reqBadBB, reqOkBB);

    cg.builder_.SetInsertPoint(reqBadBB);
    cg.builder_.CreateCall(closeFn, {conn});
    cg.emitBranchUncond(loopBB);

    cg.builder_.SetInsertPoint(reqOkBB);

    auto keepAliveFn = cg.getRuntimeFn("__ry_http_should_keep_alive", cg.i64Ty_, {cg.ptrTy_});
    llvm::Value *keepAlive = cg.builder_.CreateCall(keepAliveFn, {req}, "keep_alive");

    llvm::Value *handlerResult = cg.emitLambdaCall(handler, handlerInfo, {req}, "http_resp_val");
    llvm::Value *resp;
    if (handlerReturnsResult) {
        // Extract Ok ptr or synthesize 500 response on Err
        llvm::Value *disc = cg.builder_.CreateExtractValue(handlerResult, {0}, "resp_disc");
        llvm::Value *isOk = cg.builder_.CreateICmpEQ(
            disc, llvm::ConstantInt::get(cg.i1Ty_, 1), "resp_is_ok");
        llvm::BasicBlock *respOkBB = cg.createBB("http.resp_ok");
        llvm::BasicBlock *respErrBB = cg.createBB("http.resp_err");
        llvm::BasicBlock *respMergeBB = cg.createBB("http.resp_merge");
        cg.emitBranchCond(isOk, respOkBB, respErrBB);

        cg.builder_.SetInsertPoint(respOkBB);
        llvm::Value *okResp = cg.builder_.CreateExtractValue(handlerResult, {1}, "resp_ok_ptr");
        cg.emitBranchUncond(respMergeBB);

        cg.builder_.SetInsertPoint(respErrBB);
        auto defRespFn = cg.getRuntimeFn("__ry_http_default_error_response", cg.ptrTy_, {cg.i64Ty_});
        llvm::Value *errResp = cg.builder_.CreateCall(
            defRespFn, {llvm::ConstantInt::get(cg.i64Ty_, 500)}, "default_500");
        cg.emitBranchUncond(respMergeBB);

        cg.builder_.SetInsertPoint(respMergeBB);
        llvm::PHINode *phi = cg.createPhi(cg.ptrTy_, {}, "final_resp");
        phi->addIncoming(okResp, respOkBB);
        phi->addIncoming(errResp, respErrBB);
        resp = phi;
    } else {
        resp = handlerResult;
    }
    cg.addResourceKind(resp, rk_http_response);

    auto sendRespFn = cg.getRuntimeFn("__ry_http_send_response",
        llvm::Type::getVoidTy(*cg.ctx_), {cg.ptrTy_, cg.ptrTy_, cg.i64Ty_});
    cg.builder_.CreateCall(sendRespFn, {conn, resp, keepAlive});

    cg.builder_.CreateCall(freeReqFn, {req});
    cg.builder_.CreateCall(freeRespFn, {resp});

    if (hasMaxRequests) {
        llvm::Value *oldCount = cg.builder_.CreateLoad(cg.i64Ty_, counterAlloca, "old_count");
        llvm::Value *newCount = cg.builder_.CreateAdd(oldCount,
            llvm::ConstantInt::get(cg.i64Ty_, 1), "new_count");
        cg.builder_.CreateStore(newCount, counterAlloca);

        llvm::Value *limitReached = cg.builder_.CreateICmpSGE(newCount, maxReqs, "limit_reached");
        llvm::BasicBlock *shutdownBB = cg.createBB("http.shutdown");
        llvm::BasicBlock *kaCheckBB = cg.createBB("http.ka_check");
        cg.emitBranchCond(limitReached, shutdownBB, kaCheckBB);

        cg.builder_.SetInsertPoint(shutdownBB);
        cg.builder_.CreateCall(closeFn, {conn});
        cg.builder_.CreateCall(listenerCloseFn, {listener});
        cg.emitBranchUncond(loopEndBB);

        cg.builder_.SetInsertPoint(kaCheckBB);
    }

    llvm::Value *isKeepAlive = cg.builder_.CreateICmpNE(keepAlive,
        llvm::ConstantInt::get(cg.i64Ty_, 0), "is_keep_alive");
    llvm::BasicBlock *closeBB = cg.createBB("http.close_conn");
    cg.emitBranchCond(isKeepAlive, loopBodyBB, closeBB);

    cg.builder_.SetInsertPoint(closeBB);
    cg.builder_.CreateCall(closeFn, {conn});
    cg.emitBranchUncond(loopBB);

    // Loop completed normally
    cg.builder_.SetInsertPoint(loopEndBB);
    {
        llvm::Value *okVal = cg.buildOkValue(llvm::ConstantInt::get(cg.i8Ty_, 0), unitResTy);
        cg.builder_.CreateStore(okVal, resultAlloca);
        cg.emitBranchUncond(returnBB);
    }

    // Final return block
    cg.builder_.SetInsertPoint(returnBB);
    return cg.builder_.CreateLoad(unitResTy, resultAlloca, "listen_final");
}

static llvm::Value *emitHttpClientCall(CodeGen &cg, const CallExpr &e) {
    if (e.callee == "httpGet") {
        cg.requireArgs(e, 1);
        llvm::Value *url = cg.emitExpr(*e.args[0]);
        if (url->getType() != cg.ptrTy_)
            cg.codegenError("httpGet() url must be str");
        {
            llvm::Value *urlNul = emitHttpNulCheck(cg, url, "get_url");
            llvm::StructType *getResTy = cg.getResultType(cg.ptrTy_, cg.errorTy_);
            static int getUrlNulCtr = 0;
            return emitResultBranchWithMeta(cg, urlNul, getResTy,
                [&](llvm::Value *&okIncoming) {
                    auto fn = cg.getRuntimeFn("__ry_http_get", cg.ptrTy_, {cg.ptrTy_});
                    llvm::Value *result = cg.builder_.CreateCall(fn, {url}, "http_get_result");
                    llvm::Value *res = cg.wrapPtrAsResult(result, "__ry_http_get_last_error");
                    cg.addResourceKind(res, rk_http_client_response);
                    okIncoming = res;
                    return res;
                },
                [&]() {
                    return cg.buildErrValue(
                        cg.buildStaticError("httpGet: url contains embedded NUL",
                            ".http_get_url_nul_" + std::to_string(getUrlNulCtr++)), getResTy);
                });
        }
    }
    if (e.callee == "httpPost") {
        cg.requireArgs(e, 3);
        llvm::Value *url = cg.emitExpr(*e.args[0]);
        llvm::Value *body = cg.emitExpr(*e.args[1]);
        llvm::Value *headers = cg.emitExpr(*e.args[2]);
        if (url->getType() != cg.ptrTy_)
            cg.codegenError("httpPost() url must be str");
        if (body->getType() != cg.ptrTy_)
            cg.codegenError("httpPost() body must be str");
        if (headers->getType() != cg.ptrTy_)
            cg.codegenError("httpPost() headers must be Map<str, str>");
        {
            llvm::Value *urlNul = emitHttpNulCheck(cg, url, "post_url");
            llvm::StructType *postResTy = cg.getResultType(cg.ptrTy_, cg.errorTy_);
            static int postUrlNulCtr = 0;
            return emitResultBranchWithMeta(cg, urlNul, postResTy,
                [&](llvm::Value *&okIncoming) {
                    auto fn = cg.getRuntimeFn("__ry_http_post", cg.ptrTy_, {cg.ptrTy_, cg.ptrTy_, cg.ptrTy_});
                    llvm::Value *result = cg.builder_.CreateCall(fn, {url, body, headers}, "http_post_result");
                    llvm::Value *res = cg.wrapPtrAsResult(result, "__ry_http_get_last_error");
                    cg.addResourceKind(res, rk_http_client_response);
                    okIncoming = res;
                    return res;
                },
                [&]() {
                    return cg.buildErrValue(
                        cg.buildStaticError("httpPost: url contains embedded NUL",
                            ".http_post_url_nul_" + std::to_string(postUrlNulCtr++)), postResTy);
                });
        }
    }
    // httpRequest
    if (e.callee != "httpRequest") return nullptr;
    cg.requireArgs(e, 4);
    llvm::Value *method = cg.emitExpr(*e.args[0]);
    llvm::Value *url = cg.emitExpr(*e.args[1]);
    llvm::Value *headers = cg.emitExpr(*e.args[2]);
    llvm::Value *body = cg.emitExpr(*e.args[3]);
    if (method->getType() != cg.ptrTy_)
        cg.codegenError("httpRequest() method must be str");
    if (url->getType() != cg.ptrTy_)
        cg.codegenError("httpRequest() url must be str");
    if (headers->getType() != cg.ptrTy_)
        cg.codegenError("httpRequest() headers must be Map<str, str>");
    if (body->getType() != cg.ptrTy_)
        cg.codegenError("httpRequest() body must be str");
    // NUL check for method (user-supplied Ry str); __ry_http_post/__ry_http_get
    // pass C literals so the check lives here, not in the runtime.
    llvm::Value *methodNul = emitHttpNulCheck(cg, method, "req_method");
    llvm::StructType *reqResTy = cg.getResultType(cg.ptrTy_, cg.errorTy_);
    static int reqMethodNulCtr = 0;
    static int reqUrlNulCtr = 0;
    return emitResultBranchWithMeta(cg, methodNul, reqResTy,
        [&](llvm::Value *&okIncoming) {
            llvm::Value *urlNul = emitHttpNulCheck(cg, url, "req_url");
            return cg.emitResultBranch(urlNul, reqResTy,
                [&]() {
                    auto fn = cg.getRuntimeFn("__ry_http_client_request", cg.ptrTy_, {cg.ptrTy_, cg.ptrTy_, cg.ptrTy_, cg.ptrTy_});
                    llvm::Value *result = cg.builder_.CreateCall(fn, {method, url, headers, body}, "http_request_result");
                    llvm::Value *res = cg.wrapPtrAsResult(result, "__ry_http_get_last_error");
                    cg.addResourceKind(res, rk_http_client_response);
                    okIncoming = res;
                    return res;
                },
                [&]() {
                    return cg.buildErrValue(
                        cg.buildStaticError("httpRequest: url contains embedded NUL",
                            ".http_req_url_nul_" + std::to_string(reqUrlNulCtr++)), reqResTy);
                });
        },
        [&]() {
            return cg.buildErrValue(
                cg.buildStaticError("httpRequest: method contains embedded NUL",
                    ".http_req_method_nul_" + std::to_string(reqMethodNulCtr++)), reqResTy);
        });
}

static llvm::Value *emitHttpStatus(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *resp = cg.emitExpr(*e.args[0]);
    if (!cg.isHttpClientResponse(resp))
        cg.codegenError("status() requires HttpClientResponse argument");
    auto fn = cg.getRuntimeFn("__ry_http_client_status", cg.i64Ty_, {cg.ptrTy_});
    return cg.builder_.CreateCall(fn, {resp}, "http_client_status");
}

static llvm::Value *emitHttpClientFree(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *resp = cg.emitExpr(*e.args[0]);
    if (!cg.isHttpClientResponse(resp))
        cg.codegenError("httpClientResponseFree() requires HttpClientResponse argument");
    auto fn = cg.getRuntimeFn("__ry_http_client_response_free", llvm::Type::getVoidTy(*cg.ctx_), {cg.ptrTy_});
    cg.builder_.CreateCall(fn, {resp});
    return llvm::ConstantInt::get(cg.i64Ty_, 0);
}

// ===== Http dispatcher =====

// Gate: skip when http isn't in the program's import set, so a sibling
// dispatcher fall-through returns nullptr without side effects. Mirrors
// dispatchIO / dispatchNet (#2338, #2339).
static bool isHttpImported(CodeGen &cg) {
    const auto &sigs = cg.getNativeFnSigs();
    for (const auto &kv : sigs) {
        if (kv.first.rfind("http::", 0) == 0)
            return true;
    }
    return false;
}

RY_REGISTER_STDLIB_PACKAGE(http, "share/std/http/http.ry", dispatchHttp)
static llvm::Value *dispatchHttp(CodeGen &cg, const CallExpr &e) {
    if (!isHttpImported(cg))
        return nullptr;

    // Per-emit insert for the remaining custom branches. Descriptor-driven
    // entries (response) register the library automatically via
    // emitGenericNativeCall; this lambda covers only the bypass paths.
    auto markHttp = [&](llvm::Value *v) -> llvm::Value * {
        if (v) cg.used_native_libraries_.insert("http");
        return v;
    };

    const auto &n = e.callee;
    const auto sz = e.args.size();

    // listen — 3+ args: HTTP server overload (control flow synthesis).
    // 2-arg listen was already handled by dispatchNet's TCP listen branch
    // and won't reach here.
    if (n == "listen" && sz >= 3)
        return markHttp(emitHttpListen(cg, e));

    // method / path — HttpRequest typed-handle check; Direct str return.
    if ((n == "method" || n == "path") && sz == 1)
        return markHttp(emitHttpRequestStr(cg, e));

    // body / bodyBytes — overloaded HttpRequest vs HttpClientResponse.
    if (n == "body" && sz == 1)
        return markHttp(emitHttpBody(cg, e));
    if (n == "bodyBytes" && sz == 1)
        return markHttp(emitHttpBodyBytes(cg, e));

    // header — Option<str> return + NUL check.
    if (n == "header" && sz == 2)
        return markHttp(emitHttpHeader(cg, e));

    // query / cookie / formField — Option<str> return + NUL check.
    if ((n == "query" || n == "cookie" || n == "formField") && sz == 2)
        return markHttp(emitHttpOptionField(cg, e));

    // queryAll / cookies / formFields — Map<str, str> return.
    if ((n == "queryAll" || n == "cookies" || n == "formFields") && sz == 1)
        return markHttp(emitHttpMapAll(cg, e));

    // formFile — Option<Map<str, str>> return + NUL check.
    if (n == "formFile" && sz == 2)
        return markHttp(emitHttpFormFile(cg, e));

    // httpGet / httpPost / httpRequest — URL NUL check + ResultPtr +
    // rk_http_client_response. Stays custom for NUL check.
    if ((n == "httpGet" && sz == 1)
        || (n == "httpPost" && sz == 3)
        || (n == "httpRequest" && sz == 4))
        return markHttp(emitHttpClientCall(cg, e));

    // status — HttpClientResponse typed-handle check; Direct int return.
    if (n == "status" && sz == 1)
        return markHttp(emitHttpStatus(cg, e));

    // httpClientResponseFree — HttpClientResponse typed-handle check.
    if (n == "httpClientResponseFree" && sz == 1)
        return markHttp(emitHttpClientFree(cg, e));

    // response — descriptor-driven; fall through to emitGenericNativeCall
    // (#2339). The descriptor populates rk_http_response automatically.
    return nullptr;
}

// ===== Print =====

void CodeGen::emitPrint(const std::vector<ExprPtr> &args, const std::vector<NamedArg> &named_args) {
    // Extract named parameters: end and sep
    const ExprNode *endExpr = nullptr;
    const ExprNode *sepExpr = nullptr;
    for (const auto &na : named_args) {
        if (na.name == "end")
            endExpr = na.value.get();
        else if (na.name == "sep")
            sepExpr = na.value.get();
        else
            codegenError("unknown named argument '" + na.name + "' for print()");
    }

    builder_.CreateCall(getRuntimeFn("__ry_print_begin",
        llvm::Type::getVoidTy(*ctx_), {}));

    auto printfFn = getBufferedPrintf();
    llvm::Constant *fmtS = cachedGlobalString("%s", ".fmt_print_s");

    // Emit a named string argument (sep or end), requiring str type
    auto emitNamedStr = [&](const ExprNode &expr, const char *param) -> llvm::Value * {
        llvm::Value *v = emitExpr(expr);
        if (!isStrLike(v))
            codegenError(std::string("print() '") + param + "' must be str");
        return v;
    };

    // Determine separator value (emit once, reuse across iterations)
    llvm::Value *separator = nullptr;
    if (args.size() > 1) {
        if (sepExpr)
            separator = emitNamedStr(*sepExpr, "sep");
        else
            separator = cachedGlobalString(" ", ".fmt_space");
    }

    for (size_t i = 0; i < args.size(); ++i) {
        if (i > 0 && separator != nullptr)
            builder_.CreateCall(printfFn, {fmtS, separator});
        llvm::Value *str = valueToString(emitExpr(*args[i]));
        builder_.CreateCall(printfFn, {fmtS, str});
    }

    // Emit end string (default: newline)
    if (endExpr) {
        llvm::Value *endStr = emitNamedStr(*endExpr, "end");
        builder_.CreateCall(printfFn, {fmtS, endStr});
    } else {
        builder_.CreateCall(printfFn, {fmtS, cachedGlobalString("\n", ".fmt_nl")});
    }

    builder_.CreateCall(getRuntimeFn("__ry_print_end",
        llvm::Type::getVoidTy(*ctx_), {}));
}

// ===== Path =====
//
// path dispatch is fully descriptor-driven via emitGenericNativeCall (#2337).
// The package is registered here only so isStdlibPackageName("path") stays
// true (used by the "module not imported" diagnostic in
// codegen_call_dispatch.cpp). The dispatcher returns nullptr to let dispatch
// fall through to emitGenericNativeCall, which reads `@native("path")`
// declarations from share/std/path/path.ry as the source of truth for
// arity, return wrapping (ResultPtr / BoolFromI64), and error channel
// (__ry_path_get_last_error). The runtime symbol for `join` uses the
// arity-suffix convention (`__ry_path_join2/3/4`) which emitGenericNativeCall
// applies when sigKey has multiple overloads with different arities.
//
// See `.claude/rules/codegen-stdlib-dispatcher.md` "Descriptor-migrated
// stdlib dispatcher stubs must return nullptr" — routing here would cause
// 3× arg re-emission on type mismatch.
RY_REGISTER_STDLIB_PACKAGE(path, "share/std/path/path.ry", dispatchPath)
static llvm::Value *dispatchPath(CodeGen &, const CallExpr &) {
    return nullptr;
}

} // namespace ry
