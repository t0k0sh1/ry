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

// Stub: io dispatch is fully descriptor-driven via emitGenericNativeCall
// after #2381's Installment 2-c. The pre-2-c file-coupled custom emitters
// (open / readAll(File) / readLine variants / writeText(File) / lines)
// consume per-overload exported_symbol overrides + wrapping overrides
// (ResultOutParamOption, IteratorFromHandle) from kOverrides in
// src/codegen_native_call_descriptor.cpp. See
// `docs/architecture/native-call-boundary.md` §"Installment 2-c".
RY_REGISTER_STDLIB_PACKAGE_NAMING(io, "share/std/io/io.ry", dispatchIO, /*snake_case=*/true)
static llvm::Value *dispatchIO(CodeGen &, const CallExpr &) {
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

// ===== Net dispatcher =====
//
// All net handle-coupled entries (listen-2-arg / accept / listenerPort /
// shutdown / setTimeout / setReceiveTimeout / setSendTimeout on TcpStream
// and TlsStream) are descriptor-driven via emitGenericNativeCall after
// #2381's Installment 2-c — see src/codegen_native_call_descriptor.cpp's
// kOverrides table for the exported_symbol overrides (the runtime symbols
// predate the `__ry_net_<callee>` convention: `__ry_listen` / `__ry_accept`
// / `__ry_tcp_set_timeout` / `__ry_tls_set_timeout` / …) and
// handle_resource_kind that selects the right library to link (TlsStream
// → http, TcpStream → net).

// Priority 50: net must dispatch before http (priority 100) because
// net::listen (2-arg) falls through to http::listen (3+-arg). Stub
// after #2381 — handle-coupled entries (listen-2-arg / accept /
// listenerPort / shutdown / setTimeout / setReceiveTimeout /
// setSendTimeout on TcpStream + TlsStream) carry exported_symbol +
// handle_resource_kind in kOverrides (src/codegen_native_call_descriptor.cpp)
// and emit through emitGenericNativeCall. snake_case=true preserves
// the #2339 derivation for the bare-runtime-symbol entries (bind /
// connect / tlsConnect).
RY_REGISTER_STDLIB_PACKAGE_FULL(net, "share/std/net/net.ry", dispatchNet, 50, true)
static llvm::Value *dispatchNet(CodeGen &, const CallExpr &) {
    return nullptr;
}

// ===== Http control-flow synthesizer (listen 3+ arg) =====
//
// All other http handle-coupled / NUL-checked entries (method / path /
// body / bodyBytes / header / query / cookie / formField / queryAll /
// cookies / formFields / formFile / httpGet / httpPost / httpRequest /
// status / httpClientResponseFree) are descriptor-driven through
// emitGenericNativeCall after #2381's Installment 2-c, with their
// per-overload exported_symbol / nul_checks / OptionFromNullablePtr
// wrapping picked up from kOverrides (src/codegen_native_call_descriptor.cpp).
//
// `listen` 3+ arg stays custom: it synthesizes a multi-block control
// flow (bind → listen → accept loop → per-request handler dispatch →
// send_response → close) that the declarative descriptor model does not
// cover. `net` + `http` library registration is handled by the symbol→
// library auto-link in `getRuntimeFn`: every `__ry_net_bind` / `__ry_listen`
// / `__ry_accept` / `__ry_tcp_*` / `__ry_http_*` call below routes through
// the auto-link table (see codegen.cpp::kRuntimeSymbolLibraries), so this
// synthesizer no longer hand-names libraries.

static llvm::Value *emitHttpListen(CodeGen &cg, const CallExpr &e) {
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
    std::optional<CodeGen::FnTypeInfo> portCallbackInfo;
    if (e.args.size() == 5) {
        portCallback = cg.emitExpr(*e.args[4]);
        if (portCallback->getType() != cg.ptrTy_)
            cg.codegenError("listen() portCallback must be fn(int) -> Unit");
        auto *cbInfo = cg.lookupFnTypeInfo(portCallback);
        if (!cbInfo || cbInfo->paramTypes.size() != 1 ||
            cbInfo->paramTypes[0] != cg.i64Ty_ || !cbInfo->returnType->isVoidTy())
            cg.codegenError("listen() portCallback must be fn(int) -> Unit");
        portCallbackInfo = *cbInfo;
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
        // Invoke through emitLambdaCall so every closure calling convention
        // (uniform closure / plain fn ptr / captured closure) is handled; a raw
        // void(i64) CreateCall mis-invokes a capturing closure struct.
        cg.emitLambdaCall(portCallback, *portCallbackInfo, {actualPort}, "");
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

// Installment 2-c (#2381): handle-coupled / NUL-checked / Option-Result
// entries (method / path / body / bodyBytes / header / query / cookie /
// formField / queryAll / cookies / formFields / formFile / httpGet /
// httpPost / httpRequest / status / httpClientResponseFree) are
// descriptor-driven via emitGenericNativeCall. The descriptor's
// exported_symbol / nul_checks / handle_resource_kind / wrapping_override
// fields encode the per-overload metadata; see
// src/codegen_native_call_descriptor.cpp's kOverrides table. listen 3+
// arg (control-flow synthesis: bind / listen / accept loop / handler
// dispatch / send_response) stays custom because the synthesized control
// flow does not fit the declarative descriptor model — emitHttpListen's
// `net` + `http` library registration is auto-derived inside `getRuntimeFn`
// from each emitted runtime symbol (#2393).
RY_REGISTER_STDLIB_PACKAGE(http, "share/std/http/http.ry", dispatchHttp)
static llvm::Value *dispatchHttp(CodeGen &cg, const CallExpr &e) {
    if (!isHttpImported(cg))
        return nullptr;

    if (e.callee == "listen" && e.args.size() >= 3)
        return emitHttpListen(cg, e);

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
