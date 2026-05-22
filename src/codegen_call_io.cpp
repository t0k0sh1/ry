#include "ry/codegen.hpp"
#include "ry/runtime_regex_error_sentinels.hpp"
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
    rk_tls_stream = r.registerKind("TlsStream", "__ry_arc_dtor_tls_stream", "__ry_tls_cleanup", "http");
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
                             llvm::FunctionType *fnTy) -> llvm::Value * {
        requireArgs(e, nargs);
        std::vector<llvm::Value *> raw;
        for (size_t i = 0; i < nargs; ++i) {
            raw.push_back(emitExpr(*e.args[i]));
            if (!isStringValue(raw.back()))
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
        auto fn = mod_->getOrInsertFunction("__ry_" + runtimeName, fnTy);
        return builder_.CreateCall(fn, args, runtimeName);
    };

    auto emitRegexRuntimeError = [&]() {
        auto errFnTy = llvm::FunctionType::get(ptrTy_, {}, false);
        auto errFn = mod_->getOrInsertFunction("__ry_regex_get_last_error", errFnTy);
        llvm::Value *msgPtr = builder_.CreateCall(errFn, {}, "regex_err_msg");
        emitRuntimeError("error: %s\n", ".regex_runtime_err", {msgPtr});
    };

    auto emitRegexI64Guard = [&](llvm::Value *result, int64_t errSentinel,
                                 const std::string &prefix) -> llvm::Value * {
        llvm::Value *isErr = builder_.CreateICmpEQ(
            result, llvm::ConstantInt::get(*ctx_, llvm::APInt(64, static_cast<uint64_t>(errSentinel), true)),
            prefix + "_is_err");
        llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, prefix + ".err", fn_);
        llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, prefix + ".ok", fn_);
        builder_.CreateCondBr(isErr, errBB, okBB);
        builder_.SetInsertPoint(errBB);
        emitRegexRuntimeError();
        builder_.SetInsertPoint(okBB);
        return result;
    };

    auto emitRegexPtrGuard = [&](llvm::Value *result, const std::string &prefix) -> llvm::Value * {
        llvm::Value *isNull = builder_.CreateICmpEQ(
            result, llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)),
            prefix + "_is_null");
        llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, prefix + ".err", fn_);
        llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, prefix + ".ok", fn_);
        builder_.CreateCondBr(isNull, errBB, okBB);
        builder_.SetInsertPoint(errBB);
        emitRegexRuntimeError();
        builder_.SetInsertPoint(okBB);
        return result;
    };

    // regexMatch(text, pattern) -> bool
    if (e.callee == "regexMatch") {
        llvm::Value *r = emitRegexCall("regex_match", "regexMatch", 2,
                                       fnTy_ptr_i64_ptr_i64_to_i64_);
        r = emitRegexI64Guard(r, kRegexMatchError, "regex_match");
        return builder_.CreateTrunc(r, i1Ty_, "regex_match_bool");
    }
    // regexSearch(text, pattern) -> int
    if (e.callee == "regexSearch") {
        llvm::Value *r = emitRegexCall("regex_search", "regexSearch", 2,
                                       fnTy_ptr_i64_ptr_i64_to_i64_);
        return emitRegexI64Guard(r, kRegexSearchError, "regex_search");
    }
    // regexReplace(text, pattern, replacement) -> str
    if (e.callee == "regexReplace")
        return emitRegexPtrGuard(
            emitRegexCall("regex_replace", "regexReplace", 3,
                          fnTy_ptr_i64_ptr_i64_ptr_i64_to_ptr_),
            "regex_replace");
    // regexSplit(text, pattern) -> List<str>
    if (e.callee == "regexSplit") {
        llvm::Value *r = emitRegexCall("regex_split", "regexSplit", 2,
                                       fnTy_ptr_i64_ptr_i64_to_ptr_);
        r = emitRegexPtrGuard(r, "regex_split");
        setTypeMeta(TypeMeta::ListElem, r, ptrTy_);
        return r;
    }
    // regexFindAll(text, pattern) -> List<Match>
    if (e.callee == "regexFindAll") {
        llvm::Value *r = emitRegexCall("regex_find_all", "regexFindAll", 2,
                                       fnTy_ptr_i64_ptr_i64_to_ptr_);
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
                              llvm::FunctionType *fnTy) -> llvm::Value * {
        llvm::Value *text    = emitExpr(*e.args[0]);
        llvm::Value *pattern = emitExpr(*e.args[1]);
        if (!isRegex(pattern) || !isStringValue(text)) return nullptr;
        llvm::Value *patternLen = emitStringByteLen(pattern);
        llvm::Value *textLen    = emitStringByteLen(text);
        auto fn = mod_->getOrInsertFunction("__ry_" + rtName, fnTy);
        // Runtime expects (pattern, patternLen, text, textLen).
        llvm::Value *r = builder_.CreateCall(fn, {pattern, patternLen, text, textLen},
                                             rtName);
        if (fnTy->getReturnType() == i64Ty_) {
            int64_t sentinel = rtName == "regex_search" ? kRegexSearchError : kRegexMatchError;
            return emitRegexI64Guard(r, sentinel, rtName);
        }
        return emitRegexPtrGuard(r, rtName);
    };

    if (e.callee == "isMatch" && e.args.size() == 2) {
        if (auto *r = emitUfcsRegex("regex_is_match",
                                    fnTy_ptr_i64_ptr_i64_to_i64_))
            return builder_.CreateTrunc(r, i1Ty_, "is_match_bool");
    }
    if (e.callee == "search" && e.args.size() == 2) {
        if (auto *r = emitUfcsRegex("regex_search",
                                    fnTy_ptr_i64_ptr_i64_to_i64_))
            return r;
    }
    if (e.callee == "findAll" && e.args.size() == 2) {
        if (auto *r = emitUfcsRegex("regex_find_all",
                                    fnTy_ptr_i64_ptr_i64_to_ptr_)) {
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
    if (!cg.isStringValue(path) || !cg.isStringValue(mode))
        cg.codegenError("open() requires str arguments (path, mode)");
    auto fn = cg.mod_->getOrInsertFunction(
        "__ry_io_file_open",
        llvm::FunctionType::get(cg.ptrTy_, {cg.ptrTy_, cg.ptrTy_}, false));
    llvm::Value *ptr = cg.builder_.CreateCall(fn, {path, mode}, "file_open_ptr");
    llvm::Value *res = cg.wrapPtrAsResult(ptr);
    cg.addResourceKind(res, rk_file);
    return res;
}

static llvm::Value *emitFileReadAll(CodeGen &cg, const CallExpr & /*e*/, llvm::Value *fileHandle) {
    auto fn = cg.mod_->getOrInsertFunction(
        "__ry_io_file_read_all",
        llvm::FunctionType::get(cg.ptrTy_, {cg.ptrTy_}, false));
    llvm::Value *ptr = cg.builder_.CreateCall(fn, {fileHandle}, "file_read_all_ptr");
    return cg.wrapPtrAsResult(ptr);
}

static llvm::Value *emitFileReadLine(CodeGen &cg, const CallExpr & /*e*/, llvm::Value *fileHandle) {
    llvm::Value *outAlloca = cg.builder_.CreateAlloca(cg.ptrTy_, nullptr, "rl_out");
    cg.builder_.CreateStore(
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(cg.ptrTy_)), outAlloca);
    auto fn = cg.mod_->getOrInsertFunction(
        "__ry_io_file_read_line",
        llvm::FunctionType::get(cg.i64Ty_, {cg.ptrTy_, cg.ptrTy_}, false));
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
            return cg.buildErrValue(cg.buildErrorFromRuntime(), resTy);
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
        llvm::BasicBlock *entry = llvm::BasicBlock::Create(*cg.ctx_, "entry", nextFn);
        cg.builder_.SetInsertPoint(entry);

        llvm::Value *statePtr = nextFn->getArg(0);
        llvm::Value *file = cg.builder_.CreateLoad(cg.ptrTy_,
            cg.builder_.CreateStructGEP(stateTy, statePtr, 0), "file");

        llvm::Value *outAlloca = cg.builder_.CreateAlloca(cg.ptrTy_, nullptr, "fl_out");
        cg.builder_.CreateStore(
            llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(cg.ptrTy_)),
            outAlloca);

        auto readLineFn = cg.mod_->getOrInsertFunction(
            "__ry_io_file_read_line",
            llvm::FunctionType::get(cg.i64Ty_, {cg.ptrTy_, cg.ptrTy_}, false));
        llvm::Value *status = cg.builder_.CreateCall(
            readLineFn, {file, outAlloca}, "fl_status");
        llvm::Value *isLine = cg.builder_.CreateICmpEQ(
            status, llvm::ConstantInt::get(cg.i64Ty_, 0), "fl_isline");

        llvm::BasicBlock *someBB = llvm::BasicBlock::Create(*cg.ctx_, "some", nextFn);
        llvm::BasicBlock *noneBB = llvm::BasicBlock::Create(*cg.ctx_, "none", nextFn);
        cg.builder_.CreateCondBr(isLine, someBB, noneBB);

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
    if (!cg.isStringValue(s))
        cg.codegenError("writeText(file, s): second argument must be str");
    auto fn = cg.mod_->getOrInsertFunction(
        "__ry_io_file_write_text",
        llvm::FunctionType::get(cg.i64Ty_, {cg.ptrTy_, cg.ptrTy_}, false));
    llvm::Value *status = cg.builder_.CreateCall(fn, {fileHandle, s}, "file_wt_status");
    return cg.wrapStatusAsResult(status);
}

// ===== Builtin IO =====

static constexpr const char *IO_ERR = "__ry_get_last_error";

static const CodeGen::NativeDispatchEntry io_table[] = {
    // 0-arg -> str (stdin)
    {"readLine",    nullptr, CodeGen::ReturnWrapping::Direct,       0, nullptr,
     nullptr, "__ry_read_line"},
    {"readAll",     nullptr, CodeGen::ReturnWrapping::Direct,       0, nullptr,
     nullptr, "__ry_read_all"},
    // 1-arg -> Result<str, Error>
    {"readText",    nullptr, CodeGen::ReturnWrapping::ResultPtr,    1, nullptr,
     nullptr, "__ry_read_text", IO_ERR},
    {"bytesToStr",  nullptr, CodeGen::ReturnWrapping::ResultPtr,    1, nullptr,
     nullptr, "__ry_bytes_to_str", IO_ERR, CodeGen::ListElemMeta::None, 0},
    // 2-arg -> Result<Unit, Error>
    {"writeText",   nullptr, CodeGen::ReturnWrapping::ResultStatus, 2, nullptr,
     nullptr, "__ry_write_text", IO_ERR},
    {"appendText",  nullptr, CodeGen::ReturnWrapping::ResultStatus, 2, nullptr,
     nullptr, "__ry_append_text", IO_ERR},
    {"writeBytes",  nullptr, CodeGen::ReturnWrapping::ResultStatus, 2, nullptr,
     nullptr, "__ry_write_bytes", IO_ERR, CodeGen::ListElemMeta::None, 1},
    // 1-arg -> Result<Unit, Error>
    {"deleteFile",  nullptr, CodeGen::ReturnWrapping::ResultStatus, 1, nullptr,
     nullptr, "__ry_delete_file", IO_ERR},
    // exists -> BoolFromI64 with name remap
    {"exists",      nullptr, CodeGen::ReturnWrapping::BoolFromI64,  1, nullptr,
     nullptr, "__ry_file_exists"},
    // readBytes -> ResultPtr + list_elem=I8
    {"readBytes",   nullptr, CodeGen::ReturnWrapping::ResultPtr,    1, nullptr,
     nullptr, "__ry_read_bytes", IO_ERR, CodeGen::ListElemMeta::I8},
    // toBytes -> Direct + list_elem=I8
    {"toBytes",     nullptr, CodeGen::ReturnWrapping::Direct,       1, nullptr,
     nullptr, "__ry_str_to_bytes", nullptr, CodeGen::ListElemMeta::I8},
};

RY_REGISTER_STDLIB_PACKAGE(io, "share/std/io/io.ry", dispatchIO)
static llvm::Value *dispatchIO(CodeGen &cg, const CallExpr &e) {
    // Register libry_io.dylib for JIT loading. The custom emitters and
    // inline paths below (open / readAll(File) / readLine(File) / lines /
    // writeText File and str/str overloads) bypass emitTableDrivenNativeCall
    // and therefore miss the sig.library-driven insert.
    cg.used_native_libraries_.insert("io");

    const auto &n = e.callee;
    const auto sz = e.args.size();

    // open(path, mode) — new name, always File
    if (n == "open" && sz == 2)
        return emitFileOpen(cg, e);

    // readAll(f: File) — 1-arg (0-arg stdin handled by table)
    if (n == "readAll" && sz == 1) {
        llvm::Value *arg0 = cg.emitExpr(*e.args[0]);
        if (!cg.isFile(arg0))
            cg.codegenError("readAll(f): argument must be a File handle; "
                            "use readAll() with no arguments to read from stdin");
        return emitFileReadAll(cg, e, arg0);
    }

    // readLine(f: File) — 1-arg (0-arg stdin handled by table)
    if (n == "readLine" && sz == 1) {
        llvm::Value *arg0 = cg.emitExpr(*e.args[0]);
        if (!cg.isFile(arg0))
            cg.codegenError("readLine(f): argument must be a File handle; "
                            "use readLine() with no arguments to read from stdin");
        return emitFileReadLine(cg, e, arg0);
    }

    // lines(f: File) -> Iterator<str>
    if (n == "lines" && sz == 1) {
        llvm::Value *arg0 = cg.emitExpr(*e.args[0]);
        if (!cg.isFile(arg0))
            cg.codegenError("lines(f): argument must be a File handle");
        return emitFileLines(cg, e, arg0, rk_file);
    }

    // writeText — 2-arg: check if arg0 is File or str
    if (n == "writeText" && sz == 2) {
        llvm::Value *arg0 = cg.emitExpr(*e.args[0]);
        if (cg.isFile(arg0))
            return emitFileWriteText(cg, e, arg0);
        // Non-File: inline str-based writeText (arg0 already emitted)
        llvm::Value *content = cg.emitExpr(*e.args[1]);
        if (!cg.isStringValue(arg0) || !cg.isStringValue(content))
            cg.codegenError("writeText(path, content) requires str arguments");
        auto fn = cg.mod_->getOrInsertFunction("__ry_write_text",
            llvm::FunctionType::get(cg.i64Ty_, {cg.ptrTy_, cg.ptrTy_}, false));
        llvm::Value *status = cg.builder_.CreateCall(fn, {arg0, content}, "write_text_status");
        return cg.wrapStatusAsResult(status);
    }

    return cg.emitTableDrivenNativeCall(e, "io", io_table, std::size(io_table));
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

static llvm::Value *emitNetBind(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 2);
    llvm::Value *host = cg.emitExpr(*e.args[0]);
    llvm::Value *port = cg.emitExpr(*e.args[1]);
    auto fn = cg.mod_->getOrInsertFunction("__ry_bind", cg.fnTy_ptr_i64_to_ptr_);
    llvm::Value *result = cg.builder_.CreateCall(fn, {host, port}, "bind_result");
    llvm::Value *res = cg.wrapPtrAsResult(result, "__ry_net_get_last_error");
    cg.addResourceKind(res, rk_tcp_listener);
    return res;
}

static llvm::Value *emitNetTcpListen(CodeGen &cg, const CallExpr &e) {
    // Guard: TCP listen is 2-arg only; 3+ arg calls are HTTP listen
    if (e.args.size() != 2) return nullptr;
    llvm::Value *listener = cg.emitExpr(*e.args[0]);
    if (!cg.isTcpListener(listener))
        cg.codegenError("listen() requires TcpListener as first argument");
    llvm::Value *backlog = cg.emitExpr(*e.args[1]);
    auto fn = cg.getRuntimeFn("__ry_listen", cg.i64Ty_, {cg.ptrTy_, cg.i64Ty_});
    llvm::Value *status = cg.builder_.CreateCall(fn, {listener, backlog}, "listen_status");
    llvm::Value *isErr = cg.builder_.CreateICmpNE(status,
        llvm::ConstantInt::get(cg.i64Ty_, 0), "listen_err");
    llvm::StructType *resTy = cg.getResultType(cg.i8Ty_, cg.errorTy_);
    llvm::Value *okVal = cg.buildOkValue(llvm::ConstantInt::get(cg.i8Ty_, 0), resTy);
    llvm::Value *errVal = cg.buildErrValue(cg.buildStaticError("listen failed", ".listen_err_msg"), resTy);
    return cg.builder_.CreateSelect(isErr, errVal, okVal, "listen_result");
}

static llvm::Value *emitNetAccept(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *listener = cg.emitExpr(*e.args[0]);
    if (!cg.isTcpListener(listener))
        cg.codegenError("accept() requires TcpListener argument");
    auto fn = cg.mod_->getOrInsertFunction("__ry_accept", cg.fnTy_ptr_to_ptr_);
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
    auto fn = cg.mod_->getOrInsertFunction("__ry_listener_port", cg.fnTy_ptr_to_i64_);
    return cg.builder_.CreateCall(fn, {listener}, "listener_port");
}

static llvm::Value *emitNetShutdown(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *val = cg.emitExpr(*e.args[0]);
    if (!cg.isTcpListener(val))
        cg.codegenError("shutdown() requires TcpListener argument");
    auto fn = cg.mod_->getOrInsertFunction("__ry_tcp_listener_shutdown", cg.fnTy_ptr_to_void_);
    return cg.builder_.CreateCall(fn, {val});
}

static llvm::Value *emitNetConnect(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 2);
    llvm::Value *host = cg.emitExpr(*e.args[0]);
    llvm::Value *port = cg.emitExpr(*e.args[1]);
    bool isTls = e.callee == "tlsConnect";
    auto fn = cg.mod_->getOrInsertFunction(
        isTls ? "__ry_tls_connect" : "__ry_connect", cg.fnTy_ptr_i64_to_ptr_);
    cg.used_native_libraries_.insert(isTls ? "http" : "net");
    llvm::Value *result = cg.builder_.CreateCall(fn, {host, port}, e.callee + "_result");
    if (isTls) {
        llvm::Value *res = cg.wrapPtrAsResult(result, "__ry_tls_get_last_error");
        cg.addResourceKind(res, rk_tls_stream);
        return res;
    }
    llvm::Value *res = cg.wrapPtrAsResult(result, "__ry_net_get_last_error");
    cg.addResourceKind(res, rk_tcp_stream);
    return res;
}

static llvm::Value *emitNetTimeout(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 2);
    llvm::Value *stream = cg.emitExpr(*e.args[0]);
    llvm::Value *ms = cg.emitExpr(*e.args[1]);
    if (!cg.isTcpStream(stream) && !cg.isTlsStream(stream))
        cg.codegenError(e.callee + "() requires TcpStream or TlsStream as first argument");
    auto *voidTy = llvm::Type::getVoidTy(*cg.ctx_);
    auto fnTy = llvm::FunctionType::get(voidTy, {cg.ptrTy_, cg.i64Ty_}, false);
    bool isTls = cg.isTlsStream(stream);
    std::string prefix = isTls ? "__ry_tls_" : "__ry_tcp_";
    cg.used_native_libraries_.insert(isTls ? "http" : "net");
    std::string rtName;
    if (e.callee == "setTimeout")             rtName = "set_timeout";
    else if (e.callee == "setReceiveTimeout") rtName = "set_recv_timeout";
    else if (e.callee == "setSendTimeout")    rtName = "set_send_timeout";
    else                                      rtName = e.callee;
    auto fn = cg.mod_->getOrInsertFunction(prefix + rtName, fnTy);
    return cg.builder_.CreateCall(fn, {stream, ms});
}

// ===== Net dispatch table =====

static const CodeGen::NativeDispatchEntry net_table[] = {
    {"bind",               nullptr, {}, 0, nullptr, emitNetBind},
    {"listen",             nullptr, {}, 0, nullptr, emitNetTcpListen},
    {"accept",             nullptr, {}, 0, nullptr, emitNetAccept},
    {"listenerPort",       nullptr, {}, 0, nullptr, emitNetListenerPort},
    {"shutdown",           nullptr, {}, 0, nullptr, emitNetShutdown},
    {"connect",            nullptr, {}, 0, nullptr, emitNetConnect},
    {"tlsConnect",         nullptr, {}, 0, nullptr, emitNetConnect},
    {"setTimeout",         nullptr, {}, 0, nullptr, emitNetTimeout},
    {"setReceiveTimeout",  nullptr, {}, 0, nullptr, emitNetTimeout},
    {"setSendTimeout",     nullptr, {}, 0, nullptr, emitNetTimeout},
};

// Priority 50: net must dispatch before http (priority 100) because
// net::listen (2-arg) falls through to http::listen (3+-arg).
RY_REGISTER_STDLIB_PACKAGE_PRIO(net, "share/std/net/net.ry", dispatchNet, 50)
static llvm::Value *dispatchNet(CodeGen &cg, const CallExpr &e) {
    return cg.emitTableDrivenNativeCall(e, "net", net_table, std::size(net_table));
}

// ===== Http custom emitters =====

// Emit a NUL check on a Ry str value. Returns an i1: true if NUL found (= Err path).
static llvm::Value *emitHttpNulCheck(CodeGen &cg, llvm::Value *strVal, const std::string &hint) {
    auto nulFnTy = llvm::FunctionType::get(cg.i64Ty_, {cg.ptrTy_}, false);
    auto nulFn = cg.mod_->getOrInsertFunction("__ry_http_str_has_nul", nulFnTy);
    llvm::Value *hasNul = cg.builder_.CreateCall(nulFn, {strVal}, hint + "_has_nul");
    return cg.builder_.CreateICmpNE(hasNul, llvm::ConstantInt::get(cg.i64Ty_, 0), hint + "_is_nul");
}

static llvm::Value *emitHttpResponse(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 3);
    llvm::Value *status = cg.emitExpr(*e.args[0]);
    llvm::Value *headers = cg.emitExpr(*e.args[1]);
    llvm::Value *body = cg.emitExpr(*e.args[2]);
    if (status->getType() != cg.i64Ty_)
        cg.codegenError("response() status must be int");
    if (headers->getType() != cg.ptrTy_)
        cg.codegenError("response() headers must be Map<str, str>");
    if (body->getType() != cg.ptrTy_)
        cg.codegenError("response() body must be str");
    auto fn = cg.getRuntimeFn("__ry_http_response_create", cg.ptrTy_, {cg.i64Ty_, cg.ptrTy_, cg.ptrTy_});
    llvm::Value *result = cg.builder_.CreateCall(fn, {status, headers, body}, "http_resp");
    llvm::Value *res = cg.wrapPtrAsResult(result, "__ry_http_get_last_error");
    cg.addResourceKind(res, rk_http_response);
    return res;
}

static llvm::Value *emitHttpRequestStr(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *req = cg.emitExpr(*e.args[0]);
    if (!cg.isHttpRequest(req))
        cg.codegenError(e.callee + "() requires HttpRequest argument");
    auto fn = cg.mod_->getOrInsertFunction("__ry_http_" + e.callee, cg.fnTy_ptr_to_ptr_);
    return cg.builder_.CreateCall(fn, {req}, e.callee);
}

static llvm::Value *emitHttpBody(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *arg = cg.emitExpr(*e.args[0]);
    if (cg.isHttpRequest(arg)) {
        auto fn = cg.mod_->getOrInsertFunction("__ry_http_body", cg.fnTy_ptr_to_ptr_);
        return cg.builder_.CreateCall(fn, {arg}, "body");
    }
    if (cg.isHttpClientResponse(arg)) {
        auto fn = cg.mod_->getOrInsertFunction("__ry_http_client_body", cg.fnTy_ptr_to_ptr_);
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
    auto fn = cg.mod_->getOrInsertFunction(rtName, cg.fnTy_ptr_to_ptr_);
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
                auto fn = cg.mod_->getOrInsertFunction("__ry_http_header", cg.fnTy_ptr_ptr_to_ptr_);
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
                auto fn = cg.mod_->getOrInsertFunction("__ry_http_client_header", cg.fnTy_ptr_ptr_to_ptr_);
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

    auto fn = cg.mod_->getOrInsertFunction("__ry_http_" + rtName, cg.fnTy_ptr_ptr_to_ptr_);
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
    auto fn = cg.mod_->getOrInsertFunction("__ry_http_" + rtName, cg.fnTy_ptr_to_ptr_);
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

    auto fn = cg.mod_->getOrInsertFunction("__ry_http_form_file", cg.fnTy_ptr_ptr_to_ptr_);
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
    llvm::BasicBlock *returnBB = llvm::BasicBlock::Create(*cg.ctx_, "http.listen_return", cg.fn_);

    // 1. bind(host, port)
    auto bindFn = cg.mod_->getOrInsertFunction("__ry_bind", cg.fnTy_ptr_i64_to_ptr_);
    llvm::Value *listener = cg.builder_.CreateCall(bindFn, {host, port}, "http_listener");

    llvm::Value *isNull = cg.builder_.CreateICmpEQ(listener,
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(cg.ptrTy_)), "bind_null");
    llvm::BasicBlock *bindFailBB = llvm::BasicBlock::Create(*cg.ctx_, "http.bind_fail", cg.fn_);
    llvm::BasicBlock *bindOkBB = llvm::BasicBlock::Create(*cg.ctx_, "http.bind_ok", cg.fn_);
    cg.builder_.CreateCondBr(isNull, bindFailBB, bindOkBB);

    cg.builder_.SetInsertPoint(bindFailBB);
    {
        llvm::Value *errVal = cg.buildErrValue(
            cg.buildErrorFromRuntime("__ry_net_get_last_error"), unitResTy);
        cg.builder_.CreateStore(errVal, resultAlloca);
        cg.builder_.CreateBr(returnBB);
    }

    // 2. listen(listener, 128)
    cg.builder_.SetInsertPoint(bindOkBB);
    auto listenFn = cg.getRuntimeFn("__ry_listen", cg.i64Ty_, {cg.ptrTy_, cg.i64Ty_});
    llvm::Value *listenStatus = cg.builder_.CreateCall(
        listenFn, {listener, llvm::ConstantInt::get(cg.i64Ty_, 128)}, "listen_status");
    llvm::Value *listenFailed = cg.builder_.CreateICmpNE(listenStatus,
        llvm::ConstantInt::get(cg.i64Ty_, 0), "listen_failed");
    llvm::BasicBlock *listenFailBB = llvm::BasicBlock::Create(*cg.ctx_, "http.listen_fail", cg.fn_);
    llvm::BasicBlock *listenOkBB = llvm::BasicBlock::Create(*cg.ctx_, "http.listen_ok", cg.fn_);
    cg.builder_.CreateCondBr(listenFailed, listenFailBB, listenOkBB);

    cg.builder_.SetInsertPoint(listenFailBB);
    {
        auto earlyCloseFn = cg.mod_->getOrInsertFunction("__ry_tcp_listener_close", cg.fnTy_ptr_to_void_);
        cg.builder_.CreateCall(earlyCloseFn, {listener});
        static int listenErrCtr = 0;
        llvm::Value *errVal = cg.buildErrValue(
            cg.buildStaticError("listen failed", ".http_listen_err_" + std::to_string(listenErrCtr++)),
            unitResTy);
        cg.builder_.CreateStore(errVal, resultAlloca);
        cg.builder_.CreateBr(returnBB);
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

    auto closeFn = cg.mod_->getOrInsertFunction("__ry_tcp_close", cg.fnTy_ptr_to_void_);
    auto listenerCloseFn = cg.mod_->getOrInsertFunction("__ry_tcp_listener_close", cg.fnTy_ptr_to_void_);
    auto freeReqFn = cg.mod_->getOrInsertFunction("__ry_http_request_free", cg.fnTy_ptr_to_void_);
    auto freeRespFn = cg.mod_->getOrInsertFunction("__ry_http_response_free", cg.fnTy_ptr_to_void_);

    // 3. accept loop
    llvm::BasicBlock *loopBB = llvm::BasicBlock::Create(*cg.ctx_, "http.loop", cg.fn_);
    llvm::BasicBlock *loopBodyBB = llvm::BasicBlock::Create(*cg.ctx_, "http.loop_body", cg.fn_);
    llvm::BasicBlock *loopEndBB = llvm::BasicBlock::Create(*cg.ctx_, "http.loop_end", cg.fn_);

    cg.builder_.CreateBr(loopBB);
    cg.builder_.SetInsertPoint(loopBB);

    auto acceptFn = cg.mod_->getOrInsertFunction("__ry_accept", cg.fnTy_ptr_to_ptr_);
    llvm::Value *conn = cg.builder_.CreateCall(acceptFn, {listener}, "http_conn");

    llvm::Value *connNull = cg.builder_.CreateICmpEQ(conn,
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(cg.ptrTy_)), "conn_null");
    cg.builder_.CreateCondBr(connNull, loopBB, loopBodyBB);

    cg.builder_.SetInsertPoint(loopBodyBB);

    auto readReqFn = cg.mod_->getOrInsertFunction("__ry_http_read_request", cg.fnTy_ptr_to_ptr_);
    llvm::Value *req = cg.builder_.CreateCall(readReqFn, {conn}, "http_req");
    cg.addResourceKind(req, rk_http_request);

    llvm::Value *reqNull = cg.builder_.CreateICmpEQ(req,
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(cg.ptrTy_)), "req_null");
    llvm::BasicBlock *reqOkBB = llvm::BasicBlock::Create(*cg.ctx_, "http.req_ok", cg.fn_);
    llvm::BasicBlock *reqBadBB = llvm::BasicBlock::Create(*cg.ctx_, "http.req_bad", cg.fn_);
    cg.builder_.CreateCondBr(reqNull, reqBadBB, reqOkBB);

    cg.builder_.SetInsertPoint(reqBadBB);
    cg.builder_.CreateCall(closeFn, {conn});
    cg.builder_.CreateBr(loopBB);

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
        llvm::BasicBlock *respOkBB = llvm::BasicBlock::Create(*cg.ctx_, "http.resp_ok", cg.fn_);
        llvm::BasicBlock *respErrBB = llvm::BasicBlock::Create(*cg.ctx_, "http.resp_err", cg.fn_);
        llvm::BasicBlock *respMergeBB = llvm::BasicBlock::Create(*cg.ctx_, "http.resp_merge", cg.fn_);
        cg.builder_.CreateCondBr(isOk, respOkBB, respErrBB);

        cg.builder_.SetInsertPoint(respOkBB);
        llvm::Value *okResp = cg.builder_.CreateExtractValue(handlerResult, {1}, "resp_ok_ptr");
        cg.builder_.CreateBr(respMergeBB);

        cg.builder_.SetInsertPoint(respErrBB);
        auto defRespFnTy = llvm::FunctionType::get(cg.ptrTy_, {cg.i64Ty_}, false);
        auto defRespFn = cg.mod_->getOrInsertFunction("__ry_http_default_error_response", defRespFnTy);
        llvm::Value *errResp = cg.builder_.CreateCall(
            defRespFn, {llvm::ConstantInt::get(cg.i64Ty_, 500)}, "default_500");
        cg.builder_.CreateBr(respMergeBB);

        cg.builder_.SetInsertPoint(respMergeBB);
        llvm::PHINode *phi = cg.builder_.CreatePHI(cg.ptrTy_, 2, "final_resp");
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
        llvm::BasicBlock *shutdownBB = llvm::BasicBlock::Create(*cg.ctx_, "http.shutdown", cg.fn_);
        llvm::BasicBlock *kaCheckBB = llvm::BasicBlock::Create(*cg.ctx_, "http.ka_check", cg.fn_);
        cg.builder_.CreateCondBr(limitReached, shutdownBB, kaCheckBB);

        cg.builder_.SetInsertPoint(shutdownBB);
        cg.builder_.CreateCall(closeFn, {conn});
        cg.builder_.CreateCall(listenerCloseFn, {listener});
        cg.builder_.CreateBr(loopEndBB);

        cg.builder_.SetInsertPoint(kaCheckBB);
    }

    llvm::Value *isKeepAlive = cg.builder_.CreateICmpNE(keepAlive,
        llvm::ConstantInt::get(cg.i64Ty_, 0), "is_keep_alive");
    llvm::BasicBlock *closeBB = llvm::BasicBlock::Create(*cg.ctx_, "http.close_conn", cg.fn_);
    cg.builder_.CreateCondBr(isKeepAlive, loopBodyBB, closeBB);

    cg.builder_.SetInsertPoint(closeBB);
    cg.builder_.CreateCall(closeFn, {conn});
    cg.builder_.CreateBr(loopBB);

    // Loop completed normally
    cg.builder_.SetInsertPoint(loopEndBB);
    {
        llvm::Value *okVal = cg.buildOkValue(llvm::ConstantInt::get(cg.i8Ty_, 0), unitResTy);
        cg.builder_.CreateStore(okVal, resultAlloca);
        cg.builder_.CreateBr(returnBB);
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
                    auto fn = cg.mod_->getOrInsertFunction("__ry_http_get", cg.fnTy_ptr_to_ptr_);
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
                    auto fn = cg.mod_->getOrInsertFunction("__ry_http_post", cg.fnTy_ptr_ptr_ptr_to_ptr_);
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
    auto fn = cg.mod_->getOrInsertFunction("__ry_http_client_status", cg.fnTy_ptr_to_i64_);
    return cg.builder_.CreateCall(fn, {resp}, "http_client_status");
}

static llvm::Value *emitHttpClientFree(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *resp = cg.emitExpr(*e.args[0]);
    if (!cg.isHttpClientResponse(resp))
        cg.codegenError("httpClientResponseFree() requires HttpClientResponse argument");
    auto fn = cg.mod_->getOrInsertFunction("__ry_http_client_response_free", cg.fnTy_ptr_to_void_);
    cg.builder_.CreateCall(fn, {resp});
    return llvm::ConstantInt::get(cg.i64Ty_, 0);
}

// ===== Http dispatch table =====

static const CodeGen::NativeDispatchEntry http_table[] = {
    {"response",                  nullptr, {}, 0, nullptr, emitHttpResponse},
    {"method",                    nullptr, {}, 0, nullptr, emitHttpRequestStr},
    {"path",                      nullptr, {}, 0, nullptr, emitHttpRequestStr},
    {"body",                      nullptr, {}, 0, nullptr, emitHttpBody},
    {"bodyBytes",                 nullptr, {}, 0, nullptr, emitHttpBodyBytes},
    {"header",                    nullptr, {}, 0, nullptr, emitHttpHeader},
    {"query",                     nullptr, {}, 0, nullptr, emitHttpOptionField},
    {"cookie",                    nullptr, {}, 0, nullptr, emitHttpOptionField},
    {"formField",                 nullptr, {}, 0, nullptr, emitHttpOptionField},
    {"queryAll",                  nullptr, {}, 0, nullptr, emitHttpMapAll},
    {"cookies",                   nullptr, {}, 0, nullptr, emitHttpMapAll},
    {"formFields",                nullptr, {}, 0, nullptr, emitHttpMapAll},
    {"formFile",                  nullptr, {}, 0, nullptr, emitHttpFormFile},
    {"listen",                    nullptr, {}, 0, nullptr, emitHttpListen},
    {"httpGet",                   nullptr, {}, 0, nullptr, emitHttpClientCall},
    {"httpPost",                  nullptr, {}, 0, nullptr, emitHttpClientCall},
    {"httpRequest",               nullptr, {}, 0, nullptr, emitHttpClientCall},
    {"status",                    nullptr, {}, 0, nullptr, emitHttpStatus},
    {"httpClientResponseFree",    nullptr, {}, 0, nullptr, emitHttpClientFree},
};

RY_REGISTER_STDLIB_PACKAGE(http, "share/std/http/http.ry", dispatchHttp)
static llvm::Value *dispatchHttp(CodeGen &cg, const CallExpr &e) {
    return cg.emitTableDrivenNativeCall(e, "http", http_table, std::size(http_table));
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
        if (!isStringValue(v))
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

static const CodeGen::NativeDispatchEntry path_table[] = {
    {"join",        nullptr, CodeGen::ReturnWrapping::ResultPtr,   -1, nullptr},
    {"basename",    nullptr, CodeGen::ReturnWrapping::ResultPtr,    1, nullptr},
    {"dirname",     nullptr, CodeGen::ReturnWrapping::ResultPtr,    1, nullptr},
    {"ext",         nullptr, CodeGen::ReturnWrapping::ResultPtr,    1, nullptr},
    {"resolve",     nullptr, CodeGen::ReturnWrapping::ResultPtr,    1, nullptr},
    {"isAbsolute",  nullptr, CodeGen::ReturnWrapping::BoolFromI64,  1, nullptr},
};

RY_REGISTER_STDLIB_PACKAGE(path, "share/std/path/path.ry", dispatchPath)
static llvm::Value *dispatchPath(CodeGen &cg, const CallExpr &e) {
    return cg.emitTableDrivenNativeCall(e, "path", path_table, std::size(path_table));
}

} // namespace ry
