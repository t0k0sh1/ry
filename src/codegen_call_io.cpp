#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"

// ===== Builtin Regex =====

llvm::Value *CodeGen::emitBuiltinRegex(const CallExpr &e) {
    auto emitRegexCall = [&](const std::string &name, size_t nargs,
                             llvm::FunctionType *fnTy) -> llvm::Value * {
        requireArgs(e, nargs);
        std::vector<llvm::Value *> args;
        for (size_t i = 0; i < nargs; ++i) {
            args.push_back(emitExpr(*e.args[i]));
            if (!isStringValue(args.back()))
                codegenError(name + "() requires str arguments");
        }
        // Legacy API accepts (text, pattern, ...) but runtime expects
        // (pattern, text, ...) — swap the first two arguments.
        if (nargs >= 2) std::swap(args[0], args[1]);
        auto fn = mod_->getOrInsertFunction("__ry_" + name, fnTy);
        return builder_.CreateCall(fn, args, name);
    };

    // regex_match(text, pattern) -> bool
    if (e.callee == "regex_match") {
        llvm::Value *r = emitRegexCall("regex_match", 2, fnTy_ptr_ptr_to_i64_);
        return builder_.CreateTrunc(r, i1Ty_, "regex_match_bool");
    }
    // regex_search(text, pattern) -> int
    if (e.callee == "regex_search")
        return emitRegexCall("regex_search", 2, fnTy_ptr_ptr_to_i64_);
    // regex_replace(text, pattern, replacement) -> str
    if (e.callee == "regex_replace")
        return emitRegexCall("regex_replace", 3, fnTy_ptr_ptr_ptr_to_ptr_);
    // regex_split(text, pattern) -> List<str>
    if (e.callee == "regex_split") {
        llvm::Value *r = emitRegexCall("regex_split", 2, fnTy_ptr_ptr_to_ptr_);
        type_meta_[TM_ListElem][r] = ptrTy_;
        return r;
    }
    // regex_find_all(text, pattern) -> List<str>
    if (e.callee == "regex_find_all") {
        llvm::Value *r = emitRegexCall("regex_find_all", 2, fnTy_ptr_ptr_to_ptr_);
        type_meta_[TM_ListElem][r] = ptrTy_;
        return r;
    }

    // --- Unprefixed regex functions (text-first for UFCS) ---
    // Emit __ry_regex_<rtName>(pattern, text) from UFCS (text, pattern) args.
    // Returns nullptr if the second arg is not a Regex value.
    auto emitUfcsRegex = [&](const std::string &rtName,
                              llvm::FunctionType *fnTy) -> llvm::Value * {
        llvm::Value *text    = emitExpr(*e.args[0]);
        llvm::Value *pattern = emitExpr(*e.args[1]);
        if (!isRegex(pattern) || !isStringValue(text)) return nullptr;
        auto fn = mod_->getOrInsertFunction("__ry_" + rtName, fnTy);
        // Runtime expects (pattern, text) — pass in that order.
        return builder_.CreateCall(fn, {pattern, text}, rtName);
    };

    if (e.callee == "is_match" && e.args.size() == 2) {
        if (auto *r = emitUfcsRegex("regex_match", fnTy_ptr_ptr_to_i64_))
            return builder_.CreateTrunc(r, i1Ty_, "regex_match_bool");
    }
    if (e.callee == "search" && e.args.size() == 2) {
        if (auto *r = emitUfcsRegex("regex_search", fnTy_ptr_ptr_to_i64_))
            return r;
    }
    if (e.callee == "find_all" && e.args.size() == 2) {
        if (auto *r = emitUfcsRegex("regex_find_all", fnTy_ptr_ptr_to_ptr_)) {
            type_meta_[TM_ListElem][r] = ptrTy_;
            return r;
        }
    }

    return nullptr;
}

// ===== Builtin IO =====

static constexpr const char *IO_ERR = "__ry_get_last_error";

static const CodeGen::NativeDispatchEntry io_table[] = {
    // 0-arg -> str (stdin)
    {"read_line",   nullptr, CodeGen::ReturnWrapping::Direct,       0, nullptr,
     nullptr, "__ry_read_line"},
    {"read_all",    nullptr, CodeGen::ReturnWrapping::Direct,       0, nullptr,
     nullptr, "__ry_read_all"},
    // 1-arg -> Result<str, Error>
    {"read_text",   nullptr, CodeGen::ReturnWrapping::ResultPtr,    1, nullptr,
     nullptr, "__ry_read_text", IO_ERR},
    {"bytes_to_str",nullptr, CodeGen::ReturnWrapping::ResultPtr,    1, nullptr,
     nullptr, "__ry_bytes_to_str", IO_ERR},
    // 2-arg -> Result<Unit, Error>
    {"write_text",  nullptr, CodeGen::ReturnWrapping::ResultStatus, 2, nullptr,
     nullptr, "__ry_write_text", IO_ERR},
    {"append_text", nullptr, CodeGen::ReturnWrapping::ResultStatus, 2, nullptr,
     nullptr, "__ry_append_text", IO_ERR},
    {"write_bytes", nullptr, CodeGen::ReturnWrapping::ResultStatus, 2, nullptr,
     nullptr, "__ry_write_bytes", IO_ERR},
    // 1-arg -> Result<Unit, Error>
    {"delete_file", nullptr, CodeGen::ReturnWrapping::ResultStatus, 1, nullptr,
     nullptr, "__ry_delete_file", IO_ERR},
    // exists -> BoolFromI64 with name remap
    {"exists",      nullptr, CodeGen::ReturnWrapping::BoolFromI64,  1, nullptr,
     nullptr, "__ry_file_exists"},
    // read_bytes -> ResultPtr + list_elem=I8
    {"read_bytes",  nullptr, CodeGen::ReturnWrapping::ResultPtr,    1, nullptr,
     nullptr, "__ry_read_bytes", IO_ERR, CodeGen::ListElemMeta::I8},
    // to_bytes -> Direct + list_elem=I8
    {"to_bytes",    nullptr, CodeGen::ReturnWrapping::Direct,       1, nullptr,
     nullptr, "__ry_str_to_bytes", nullptr, CodeGen::ListElemMeta::I8},
};

llvm::Value *CodeGen::emitBuiltinIO(const CallExpr &e) {
    return emitTableDrivenNativeCall(e, "io", io_table,
                                     std::size(io_table));
}

// ===== Shared helper: ptr → Result<T, Error> with static error message =====

llvm::Value *CodeGen::emitPtrToResult(llvm::Value *ptr, const std::string &name,
                                       const std::string &errMsg, ResourceKind rk) {
    llvm::Value *isNull = builder_.CreateICmpEQ(ptr,
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)), name + "_null");
    llvm::StructType *resTy = getResultType(ptrTy_, errorTy_);
    llvm::Value *okVal = buildOkValue(ptr, resTy);
    llvm::Value *errVal = buildErrValue(buildStaticError(errMsg, "." + name + "_err_msg"), resTy);
    llvm::Value *res = builder_.CreateSelect(isNull, errVal, okVal, name + "_result");
    resource_sets_[rk].insert(res);
    return res;
}

// ===== Net custom emitters =====

llvm::Value *CodeGen::emitNetBind(const CallExpr &e) {
    requireArgs(e, 2);
    llvm::Value *host = emitExpr(*e.args[0]);
    llvm::Value *port = emitExpr(*e.args[1]);
    auto fn = mod_->getOrInsertFunction("__ry_bind", fnTy_ptr_i64_to_ptr_);
    llvm::Value *result = builder_.CreateCall(fn, {host, port}, "bind_result");
    return emitPtrToResult(result, "bind", "bind failed", RK_TcpListener);
}

llvm::Value *CodeGen::emitNetTcpListen(const CallExpr &e) {
    // Guard: TCP listen is 2-arg only; 3+ arg calls are HTTP listen
    if (e.args.size() != 2) return nullptr;
    llvm::Value *listener = emitExpr(*e.args[0]);
    if (!isTcpListener(listener))
        codegenError("listen() requires TcpListener as first argument");
    llvm::Value *backlog = emitExpr(*e.args[1]);
    auto fn = getRuntimeFn("__ry_listen", i64Ty_, {ptrTy_, i64Ty_});
    llvm::Value *status = builder_.CreateCall(fn, {listener, backlog}, "listen_status");
    llvm::Value *isErr = builder_.CreateICmpNE(status,
        llvm::ConstantInt::get(i64Ty_, 0), "listen_err");
    llvm::StructType *resTy = getResultType(i8Ty_, errorTy_);
    llvm::Value *okVal = buildOkValue(llvm::ConstantInt::get(i8Ty_, 0), resTy);
    llvm::Value *errVal = buildErrValue(buildStaticError("listen failed", ".listen_err_msg"), resTy);
    return builder_.CreateSelect(isErr, errVal, okVal, "listen_result");
}

llvm::Value *CodeGen::emitNetAccept(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *listener = emitExpr(*e.args[0]);
    if (!isTcpListener(listener))
        codegenError("accept() requires TcpListener argument");
    auto fn = mod_->getOrInsertFunction("__ry_accept", fnTy_ptr_to_ptr_);
    llvm::Value *result = builder_.CreateCall(fn, {listener}, "accept_result");
    return emitPtrToResult(result, "accept", "accept failed", RK_TcpStream);
}

llvm::Value *CodeGen::emitNetListenerPort(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *listener = emitExpr(*e.args[0]);
    if (!isTcpListener(listener))
        codegenError("listener_port() requires TcpListener argument");
    auto fn = mod_->getOrInsertFunction("__ry_listener_port", fnTy_ptr_to_i64_);
    return builder_.CreateCall(fn, {listener}, "listener_port");
}

llvm::Value *CodeGen::emitNetShutdown(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *val = emitExpr(*e.args[0]);
    if (!isTcpListener(val))
        codegenError("shutdown() requires TcpListener argument");
    auto fn = mod_->getOrInsertFunction("__ry_tcp_listener_shutdown", fnTy_ptr_to_void_);
    return builder_.CreateCall(fn, {val});
}

llvm::Value *CodeGen::emitNetConnect(const CallExpr &e) {
    requireArgs(e, 2);
    llvm::Value *host = emitExpr(*e.args[0]);
    llvm::Value *port = emitExpr(*e.args[1]);
    bool isTls = e.callee == "tls_connect";
    auto fn = mod_->getOrInsertFunction(
        isTls ? "__ry_tls_connect" : "__ry_connect", fnTy_ptr_i64_to_ptr_);
    used_native_libraries_.insert(isTls ? "http" : "net");
    llvm::Value *result = builder_.CreateCall(fn, {host, port}, e.callee + "_result");
    if (isTls)
        return emitPtrToResult(result, "tls_connect", "TLS connection failed", RK_TlsStream);
    return emitPtrToResult(result, "connect", "connection failed", RK_TcpStream);
}

llvm::Value *CodeGen::emitNetTimeout(const CallExpr &e) {
    requireArgs(e, 2);
    llvm::Value *stream = emitExpr(*e.args[0]);
    llvm::Value *ms = emitExpr(*e.args[1]);
    if (!isTcpStream(stream) && !isTlsStream(stream))
        codegenError(e.callee + "() requires TcpStream or TlsStream as first argument");
    auto *voidTy = llvm::Type::getVoidTy(*ctx_);
    auto fnTy = llvm::FunctionType::get(voidTy, {ptrTy_, i64Ty_}, false);
    std::string prefix = isTlsStream(stream) ? "__ry_tls_" : "__ry_tcp_";
    std::string rtName = e.callee;
    if (rtName == "set_receive_timeout") rtName = "set_recv_timeout";
    auto fn = mod_->getOrInsertFunction(prefix + rtName, fnTy);
    return builder_.CreateCall(fn, {stream, ms});
}

// ===== Net dispatch table =====

static const CodeGen::NativeDispatchEntry net_table[] = {
    {"bind",                 nullptr, {}, 0, nullptr, &CodeGen::emitNetBind},
    {"listen",               nullptr, {}, 0, nullptr, &CodeGen::emitNetTcpListen},
    {"accept",               nullptr, {}, 0, nullptr, &CodeGen::emitNetAccept},
    {"listener_port",        nullptr, {}, 0, nullptr, &CodeGen::emitNetListenerPort},
    {"shutdown",             nullptr, {}, 0, nullptr, &CodeGen::emitNetShutdown},
    {"connect",              nullptr, {}, 0, nullptr, &CodeGen::emitNetConnect},
    {"tls_connect",          nullptr, {}, 0, nullptr, &CodeGen::emitNetConnect},
    {"set_timeout",          nullptr, {}, 0, nullptr, &CodeGen::emitNetTimeout},
    {"set_receive_timeout",  nullptr, {}, 0, nullptr, &CodeGen::emitNetTimeout},
    {"set_send_timeout",     nullptr, {}, 0, nullptr, &CodeGen::emitNetTimeout},
};

llvm::Value *CodeGen::emitBuiltinNet(const CallExpr &e) {
    return emitTableDrivenNativeCall(e, "net", net_table, std::size(net_table));
}

// ===== Http custom emitters =====

llvm::Value *CodeGen::emitHttpResponse(const CallExpr &e) {
    requireArgs(e, 3);
    llvm::Value *status = emitExpr(*e.args[0]);
    llvm::Value *headers = emitExpr(*e.args[1]);
    llvm::Value *body = emitExpr(*e.args[2]);
    if (status->getType() != i64Ty_)
        codegenError("response() status must be int");
    if (headers->getType() != ptrTy_)
        codegenError("response() headers must be Map<str, str>");
    if (body->getType() != ptrTy_)
        codegenError("response() body must be str");
    auto fn = getRuntimeFn("__ry_http_response_create", ptrTy_, {i64Ty_, ptrTy_, ptrTy_});
    llvm::Value *result = builder_.CreateCall(fn, {status, headers, body}, "http_resp");
    resource_sets_[RK_HttpResponse].insert(result);
    return result;
}

llvm::Value *CodeGen::emitHttpRequestStr(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *req = emitExpr(*e.args[0]);
    if (!isHttpRequest(req))
        codegenError(e.callee + "() requires HttpRequest argument");
    auto fn = mod_->getOrInsertFunction("__ry_http_" + e.callee, fnTy_ptr_to_ptr_);
    return builder_.CreateCall(fn, {req}, e.callee);
}

llvm::Value *CodeGen::emitHttpBody(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *arg = emitExpr(*e.args[0]);
    if (isHttpRequest(arg)) {
        auto fn = mod_->getOrInsertFunction("__ry_http_body", fnTy_ptr_to_ptr_);
        return builder_.CreateCall(fn, {arg}, "body");
    }
    if (isHttpClientResponse(arg)) {
        auto fn = mod_->getOrInsertFunction("__ry_http_client_body", fnTy_ptr_to_ptr_);
        return builder_.CreateCall(fn, {arg}, "body");
    }
    codegenError("body() requires HttpRequest or HttpClientResponse argument");
}

llvm::Value *CodeGen::emitHttpBodyBytes(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *arg = emitExpr(*e.args[0]);
    const char *rtName;
    if (isHttpRequest(arg))
        rtName = "__ry_http_body_bytes";
    else if (isHttpClientResponse(arg))
        rtName = "__ry_http_client_body_bytes";
    else
        codegenError("body_bytes() requires HttpRequest or HttpClientResponse argument");
    auto fn = mod_->getOrInsertFunction(rtName, fnTy_ptr_to_ptr_);
    llvm::Value *result = builder_.CreateCall(fn, {arg}, "body_bytes");
    type_meta_[TM_ListElem][result] = i8Ty_;
    return result;
}

llvm::Value *CodeGen::emitHttpHeader(const CallExpr &e) {
    requireArgs(e, 2);
    llvm::Value *arg = emitExpr(*e.args[0]);
    llvm::Value *key = emitExpr(*e.args[1]);
    if (key->getType() != ptrTy_)
        codegenError("header() key must be str");
    if (isHttpRequest(arg)) {
        auto fn = mod_->getOrInsertFunction("__ry_http_header", fnTy_ptr_ptr_to_ptr_);
        llvm::Value *result = builder_.CreateCall(fn, {arg, key}, "http_hdr");
        return wrapPtrAsOption(result, "http_hdr");
    }
    if (isHttpClientResponse(arg)) {
        auto fn = mod_->getOrInsertFunction("__ry_http_client_header", fnTy_ptr_ptr_to_ptr_);
        llvm::Value *result = builder_.CreateCall(fn, {arg, key}, "http_client_hdr");
        return wrapPtrAsOption(result, "http_client_hdr");
    }
    codegenError("header() requires HttpRequest or HttpClientResponse argument");
}

llvm::Value *CodeGen::emitHttpOptionField(const CallExpr &e) {
    requireArgs(e, 2);
    llvm::Value *req = emitExpr(*e.args[0]);
    llvm::Value *key = emitExpr(*e.args[1]);
    if (!isHttpRequest(req))
        codegenError(e.callee + "() requires HttpRequest argument");
    if (key->getType() != ptrTy_) {
        std::string param = (e.callee == "cookie") ? "name" : "key";
        codegenError(e.callee + "() " + param + " must be str");
    }
    auto fn = mod_->getOrInsertFunction("__ry_http_" + e.callee, fnTy_ptr_ptr_to_ptr_);
    std::string hint = (e.callee == "query") ? "http_qry"
                     : (e.callee == "cookie") ? "http_ck" : "http_ff";
    llvm::Value *result = builder_.CreateCall(fn, {req, key}, hint);
    return wrapPtrAsOption(result, hint);
}

llvm::Value *CodeGen::emitHttpMapAll(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *req = emitExpr(*e.args[0]);
    if (!isHttpRequest(req))
        codegenError(e.callee + "() requires HttpRequest argument");
    auto fn = mod_->getOrInsertFunction("__ry_http_" + e.callee, fnTy_ptr_to_ptr_);
    llvm::Value *result = builder_.CreateCall(fn, {req}, "http_" + e.callee);
    type_meta_[TM_MapKey][result] = ptrTy_;
    type_meta_[TM_MapValue][result] = ptrTy_;
    return result;
}

llvm::Value *CodeGen::emitHttpFormFile(const CallExpr &e) {
    requireArgs(e, 2);
    llvm::Value *req = emitExpr(*e.args[0]);
    llvm::Value *key = emitExpr(*e.args[1]);
    if (!isHttpRequest(req))
        codegenError("form_file() requires HttpRequest argument");
    if (key->getType() != ptrTy_)
        codegenError("form_file() name must be str");
    auto fn = mod_->getOrInsertFunction("__ry_http_form_file", fnTy_ptr_ptr_to_ptr_);
    llvm::Value *result = builder_.CreateCall(fn, {req, key}, "http_ffile");
    llvm::Value *optResult = wrapPtrAsOption(result, "http_ffile");
    type_meta_[TM_MapKey][optResult] = ptrTy_;
    type_meta_[TM_MapValue][optResult] = ptrTy_;
    return optResult;
}

llvm::Value *CodeGen::emitHttpListen(const CallExpr &e) {
    if (e.args.size() > 5)
        codegenError("listen() takes 3 to 5 arguments");
    llvm::Value *host = emitExpr(*e.args[0]);
    llvm::Value *port = emitExpr(*e.args[1]);
    llvm::Value *handler = emitExpr(*e.args[2]);
    if (host->getType() != ptrTy_)
        codegenError("listen() host must be str");
    if (port->getType() != i64Ty_)
        codegenError("listen() port must be int");

    auto fnIt = lookupFnTypeInfo(handler);
    if (fnIt == fn_type_info_.end())
        codegenError("listen() handler must be a function fn(HttpRequest) -> HttpResponse");
    FnTypeInfo handlerInfo = fnIt->second;

    if (handlerInfo.paramTypes.size() != 1 ||
        handlerInfo.returnType != ptrTy_ ||
        handlerInfo.paramTypes[0] != ptrTy_) {
        codegenError("listen() handler must be a function fn(HttpRequest) -> HttpResponse");
    }

    llvm::Value *maxReqs = llvm::ConstantInt::get(i64Ty_, 0);
    if (e.args.size() >= 4) {
        maxReqs = emitExpr(*e.args[3]);
        if (maxReqs->getType() != i64Ty_)
            codegenError("listen() max_requests must be int");
        if (auto *maxConst = llvm::dyn_cast<llvm::ConstantInt>(maxReqs)) {
            if (maxConst->getSExtValue() <= 0)
                codegenError("listen() max_requests must be a positive integer");
        }
    }

    llvm::Value *portCallback = nullptr;
    if (e.args.size() == 5) {
        portCallback = emitExpr(*e.args[4]);
        if (portCallback->getType() != ptrTy_)
            codegenError("listen() port_callback must be fn(int) -> Unit");
    }

    // 1. bind(host, port)
    auto bindFn = mod_->getOrInsertFunction("__ry_bind", fnTy_ptr_i64_to_ptr_);
    llvm::Value *listener = builder_.CreateCall(bindFn, {host, port}, "http_listener");

    llvm::Value *isNull = builder_.CreateICmpEQ(listener,
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)), "bind_null");
    llvm::BasicBlock *bindFailBB = llvm::BasicBlock::Create(*ctx_, "http.bind_fail", fn_);
    llvm::BasicBlock *bindOkBB = llvm::BasicBlock::Create(*ctx_, "http.bind_ok", fn_);
    builder_.CreateCondBr(isNull, bindFailBB, bindOkBB);

    builder_.SetInsertPoint(bindFailBB);
    static int httpErrCounter = 0;
    emitRuntimeError("runtime error: listen() bind failed\n",
                      ".http_err_" + std::to_string(httpErrCounter++));

    // 2. listen(listener, 128)
    builder_.SetInsertPoint(bindOkBB);
    auto listenFn = getRuntimeFn("__ry_listen", i64Ty_, {ptrTy_, i64Ty_});
    llvm::Value *listenStatus = builder_.CreateCall(listenFn, {listener, llvm::ConstantInt::get(i64Ty_, 128)}, "listen_status");
    llvm::Value *listenFailed = builder_.CreateICmpNE(listenStatus,
        llvm::ConstantInt::get(i64Ty_, 0), "listen_failed");
    llvm::BasicBlock *listenFailBB = llvm::BasicBlock::Create(*ctx_, "http.listen_fail", fn_);
    llvm::BasicBlock *listenOkBB = llvm::BasicBlock::Create(*ctx_, "http.listen_ok", fn_);
    builder_.CreateCondBr(listenFailed, listenFailBB, listenOkBB);
    builder_.SetInsertPoint(listenFailBB);
    static int httpListenErrCounter = 0;
    emitRuntimeError("runtime error: listen() listen failed\n",
                      ".http_listen_err_" + std::to_string(httpListenErrCounter++));
    builder_.SetInsertPoint(listenOkBB);

    if (portCallback) {
        auto portFn = getRuntimeFn("__ry_listener_port", i64Ty_, {ptrTy_});
        llvm::Value *actualPort = builder_.CreateCall(portFn, {listener}, "actual_port");
        auto callbackFnTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*ctx_), {i64Ty_}, false);
        builder_.CreateCall(callbackFnTy, portCallback, {actualPort});
    }

    bool hasMaxRequests = (e.args.size() >= 4);
    llvm::Value *counterAlloca = nullptr;
    if (hasMaxRequests) {
        counterAlloca = builder_.CreateAlloca(i64Ty_, nullptr, "req_counter");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), counterAlloca);
    }

    auto closeFn = mod_->getOrInsertFunction("__ry_tcp_close", fnTy_ptr_to_void_);
    auto listenerCloseFn = mod_->getOrInsertFunction("__ry_tcp_listener_close", fnTy_ptr_to_void_);
    auto freeReqFn = mod_->getOrInsertFunction("__ry_http_request_free", fnTy_ptr_to_void_);
    auto freeRespFn = mod_->getOrInsertFunction("__ry_http_response_free", fnTy_ptr_to_void_);

    // 3. accept loop
    llvm::BasicBlock *loopBB = llvm::BasicBlock::Create(*ctx_, "http.loop", fn_);
    llvm::BasicBlock *loopBodyBB = llvm::BasicBlock::Create(*ctx_, "http.loop_body", fn_);
    llvm::BasicBlock *loopEndBB = llvm::BasicBlock::Create(*ctx_, "http.loop_end", fn_);

    builder_.CreateBr(loopBB);
    builder_.SetInsertPoint(loopBB);

    auto acceptFn = mod_->getOrInsertFunction("__ry_accept", fnTy_ptr_to_ptr_);
    llvm::Value *conn = builder_.CreateCall(acceptFn, {listener}, "http_conn");

    llvm::Value *connNull = builder_.CreateICmpEQ(conn,
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)), "conn_null");
    builder_.CreateCondBr(connNull, loopBB, loopBodyBB);

    builder_.SetInsertPoint(loopBodyBB);

    auto readReqFn = mod_->getOrInsertFunction("__ry_http_read_request", fnTy_ptr_to_ptr_);
    llvm::Value *req = builder_.CreateCall(readReqFn, {conn}, "http_req");
    resource_sets_[RK_HttpRequest].insert(req);

    llvm::Value *reqNull = builder_.CreateICmpEQ(req,
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)), "req_null");
    llvm::BasicBlock *reqOkBB = llvm::BasicBlock::Create(*ctx_, "http.req_ok", fn_);
    llvm::BasicBlock *reqBadBB = llvm::BasicBlock::Create(*ctx_, "http.req_bad", fn_);
    builder_.CreateCondBr(reqNull, reqBadBB, reqOkBB);

    builder_.SetInsertPoint(reqBadBB);
    builder_.CreateCall(closeFn, {conn});
    builder_.CreateBr(loopBB);

    builder_.SetInsertPoint(reqOkBB);

    auto keepAliveFn = getRuntimeFn("__ry_http_should_keep_alive", i64Ty_, {ptrTy_});
    llvm::Value *keepAlive = builder_.CreateCall(keepAliveFn, {req}, "keep_alive");

    llvm::Value *resp = emitLambdaCall(handler, handlerInfo, {req}, "http_resp_val");
    resource_sets_[RK_HttpResponse].insert(resp);

    auto sendRespFn = getRuntimeFn("__ry_http_send_response", llvm::Type::getVoidTy(*ctx_), {ptrTy_, ptrTy_, i64Ty_});
    builder_.CreateCall(sendRespFn, {conn, resp, keepAlive});

    builder_.CreateCall(freeReqFn, {req});
    builder_.CreateCall(freeRespFn, {resp});

    if (hasMaxRequests) {
        llvm::Value *oldCount = builder_.CreateLoad(i64Ty_, counterAlloca, "old_count");
        llvm::Value *newCount = builder_.CreateAdd(oldCount,
            llvm::ConstantInt::get(i64Ty_, 1), "new_count");
        builder_.CreateStore(newCount, counterAlloca);

        llvm::Value *limitReached = builder_.CreateICmpSGE(newCount, maxReqs, "limit_reached");
        llvm::BasicBlock *shutdownBB = llvm::BasicBlock::Create(*ctx_, "http.shutdown", fn_);
        llvm::BasicBlock *kaCheckBB = llvm::BasicBlock::Create(*ctx_, "http.ka_check", fn_);
        builder_.CreateCondBr(limitReached, shutdownBB, kaCheckBB);

        builder_.SetInsertPoint(shutdownBB);
        builder_.CreateCall(closeFn, {conn});
        builder_.CreateCall(listenerCloseFn, {listener});
        builder_.CreateBr(loopEndBB);

        builder_.SetInsertPoint(kaCheckBB);
    }

    llvm::Value *isKeepAlive = builder_.CreateICmpNE(keepAlive,
        llvm::ConstantInt::get(i64Ty_, 0), "is_keep_alive");
    llvm::BasicBlock *closeBB = llvm::BasicBlock::Create(*ctx_, "http.close_conn", fn_);
    builder_.CreateCondBr(isKeepAlive, loopBodyBB, closeBB);

    builder_.SetInsertPoint(closeBB);
    builder_.CreateCall(closeFn, {conn});
    builder_.CreateBr(loopBB);

    builder_.SetInsertPoint(loopEndBB);

    return llvm::ConstantInt::get(i64Ty_, 0);
}

llvm::Value *CodeGen::emitHttpClientCall(const CallExpr &e) {
    if (e.callee == "http_get") {
        requireArgs(e, 1);
        llvm::Value *url = emitExpr(*e.args[0]);
        if (url->getType() != ptrTy_)
            codegenError("http_get() url must be str");
        auto fn = mod_->getOrInsertFunction("__ry_http_get", fnTy_ptr_to_ptr_);
        llvm::Value *result = builder_.CreateCall(fn, {url}, "http_get_result");
        return emitPtrToResult(result, "http_get", "HTTP request failed", RK_HttpClientResponse);
    }
    if (e.callee == "http_post") {
        requireArgs(e, 3);
        llvm::Value *url = emitExpr(*e.args[0]);
        llvm::Value *body = emitExpr(*e.args[1]);
        llvm::Value *headers = emitExpr(*e.args[2]);
        if (url->getType() != ptrTy_)
            codegenError("http_post() url must be str");
        if (body->getType() != ptrTy_)
            codegenError("http_post() body must be str");
        if (headers->getType() != ptrTy_)
            codegenError("http_post() headers must be Map<str, str>");
        auto fn = mod_->getOrInsertFunction("__ry_http_post", fnTy_ptr_ptr_ptr_to_ptr_);
        llvm::Value *result = builder_.CreateCall(fn, {url, body, headers}, "http_post_result");
        return emitPtrToResult(result, "http_post", "HTTP request failed", RK_HttpClientResponse);
    }
    // http_request
    if (e.callee != "http_request") return nullptr;
    requireArgs(e, 4);
    llvm::Value *method = emitExpr(*e.args[0]);
    llvm::Value *url = emitExpr(*e.args[1]);
    llvm::Value *headers = emitExpr(*e.args[2]);
    llvm::Value *body = emitExpr(*e.args[3]);
    if (method->getType() != ptrTy_)
        codegenError("http_request() method must be str");
    if (url->getType() != ptrTy_)
        codegenError("http_request() url must be str");
    if (headers->getType() != ptrTy_)
        codegenError("http_request() headers must be Map<str, str>");
    if (body->getType() != ptrTy_)
        codegenError("http_request() body must be str");
    auto fn = getRuntimeFn("__ry_http_client_request", ptrTy_, {ptrTy_, ptrTy_, ptrTy_, ptrTy_});
    llvm::Value *result = builder_.CreateCall(fn, {method, url, headers, body}, "http_request_result");
    return emitPtrToResult(result, "http_request", "HTTP request failed", RK_HttpClientResponse);
}

llvm::Value *CodeGen::emitHttpStatus(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *resp = emitExpr(*e.args[0]);
    if (!isHttpClientResponse(resp))
        codegenError("status() requires HttpClientResponse argument");
    auto fn = mod_->getOrInsertFunction("__ry_http_client_status", fnTy_ptr_to_i64_);
    return builder_.CreateCall(fn, {resp}, "http_client_status");
}

llvm::Value *CodeGen::emitHttpClientFree(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *resp = emitExpr(*e.args[0]);
    if (!isHttpClientResponse(resp))
        codegenError("http_client_response_free() requires HttpClientResponse argument");
    auto fn = mod_->getOrInsertFunction("__ry_http_client_response_free", fnTy_ptr_to_void_);
    builder_.CreateCall(fn, {resp});
    return llvm::ConstantInt::get(i64Ty_, 0);
}

// ===== Http dispatch table =====

static const CodeGen::NativeDispatchEntry http_table[] = {
    {"response",                  nullptr, {}, 0, nullptr, &CodeGen::emitHttpResponse},
    {"method",                    nullptr, {}, 0, nullptr, &CodeGen::emitHttpRequestStr},
    {"path",                      nullptr, {}, 0, nullptr, &CodeGen::emitHttpRequestStr},
    {"body",                      nullptr, {}, 0, nullptr, &CodeGen::emitHttpBody},
    {"body_bytes",                nullptr, {}, 0, nullptr, &CodeGen::emitHttpBodyBytes},
    {"header",                    nullptr, {}, 0, nullptr, &CodeGen::emitHttpHeader},
    {"query",                     nullptr, {}, 0, nullptr, &CodeGen::emitHttpOptionField},
    {"cookie",                    nullptr, {}, 0, nullptr, &CodeGen::emitHttpOptionField},
    {"form_field",                nullptr, {}, 0, nullptr, &CodeGen::emitHttpOptionField},
    {"query_all",                 nullptr, {}, 0, nullptr, &CodeGen::emitHttpMapAll},
    {"cookies",                   nullptr, {}, 0, nullptr, &CodeGen::emitHttpMapAll},
    {"form_fields",               nullptr, {}, 0, nullptr, &CodeGen::emitHttpMapAll},
    {"form_file",                 nullptr, {}, 0, nullptr, &CodeGen::emitHttpFormFile},
    {"listen",                    nullptr, {}, 0, nullptr, &CodeGen::emitHttpListen},
    {"http_get",                  nullptr, {}, 0, nullptr, &CodeGen::emitHttpClientCall},
    {"http_post",                 nullptr, {}, 0, nullptr, &CodeGen::emitHttpClientCall},
    {"http_request",              nullptr, {}, 0, nullptr, &CodeGen::emitHttpClientCall},
    {"status",                    nullptr, {}, 0, nullptr, &CodeGen::emitHttpStatus},
    {"http_client_response_free", nullptr, {}, 0, nullptr, &CodeGen::emitHttpClientFree},
};

llvm::Value *CodeGen::emitBuiltinHttp(const CallExpr &e) {
    return emitTableDrivenNativeCall(e, "http", http_table, std::size(http_table));
}

