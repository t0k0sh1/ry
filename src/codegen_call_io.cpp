#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"

// ===== Builtin Regex =====

llvm::Value *CodeGen::emitBuiltinRegex(const CallExpr &e) {
    // regex_match(pattern, text) -> bool
    if (e.callee == "regex_match") {
        if (e.args.size() != 2)
            codegenError("regex_match() takes exactly 2 arguments");
        llvm::Value *pattern = emitExpr(*e.args[0]);
        llvm::Value *text = emitExpr(*e.args[1]);
        if (pattern->getType() != ptrTy_ || text->getType() != ptrTy_)
            codegenError("regex_match() requires str arguments");
        auto fnTy = llvm::FunctionType::get(i64Ty_, {ptrTy_, ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_regex_match", fnTy);
        llvm::Value *result = builder_.CreateCall(fn, {pattern, text}, "regex_match");
        return builder_.CreateTrunc(result, i1Ty_, "regex_match_bool");
    }

    // regex_search(pattern, text) -> int
    if (e.callee == "regex_search") {
        if (e.args.size() != 2)
            codegenError("regex_search() takes exactly 2 arguments");
        llvm::Value *pattern = emitExpr(*e.args[0]);
        llvm::Value *text = emitExpr(*e.args[1]);
        if (pattern->getType() != ptrTy_ || text->getType() != ptrTy_)
            codegenError("regex_search() requires str arguments");
        auto fnTy = llvm::FunctionType::get(i64Ty_, {ptrTy_, ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_regex_search", fnTy);
        return builder_.CreateCall(fn, {pattern, text}, "regex_search");
    }

    // regex_replace(pattern, text, replacement) -> str
    if (e.callee == "regex_replace") {
        if (e.args.size() != 3)
            codegenError("regex_replace() takes exactly 3 arguments");
        llvm::Value *pattern = emitExpr(*e.args[0]);
        llvm::Value *text = emitExpr(*e.args[1]);
        llvm::Value *replacement = emitExpr(*e.args[2]);
        if (pattern->getType() != ptrTy_ || text->getType() != ptrTy_ ||
            replacement->getType() != ptrTy_)
            codegenError("regex_replace() requires str arguments");
        auto fnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_regex_replace", fnTy);
        return builder_.CreateCall(fn, {pattern, text, replacement}, "regex_replace");
    }

    // regex_split(pattern, text) -> List<str>
    if (e.callee == "regex_split") {
        if (e.args.size() != 2)
            codegenError("regex_split() takes exactly 2 arguments");
        llvm::Value *pattern = emitExpr(*e.args[0]);
        llvm::Value *text = emitExpr(*e.args[1]);
        if (pattern->getType() != ptrTy_ || text->getType() != ptrTy_)
            codegenError("regex_split() requires str arguments");
        auto fnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_regex_split", fnTy);
        llvm::Value *result = builder_.CreateCall(fn, {pattern, text}, "regex_split");
        list_element_types_[result] = ptrTy_;
        return result;
    }

    // regex_find_all(pattern, text) -> List<str>
    if (e.callee == "regex_find_all") {
        if (e.args.size() != 2)
            codegenError("regex_find_all() takes exactly 2 arguments");
        llvm::Value *pattern = emitExpr(*e.args[0]);
        llvm::Value *text = emitExpr(*e.args[1]);
        if (pattern->getType() != ptrTy_ || text->getType() != ptrTy_)
            codegenError("regex_find_all() requires str arguments");
        auto fnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_regex_find_all", fnTy);
        llvm::Value *result = builder_.CreateCall(fn, {pattern, text}, "regex_find_all");
        list_element_types_[result] = ptrTy_;
        return result;
    }

    return nullptr;
}

// ===== Builtin IO =====

llvm::Value *CodeGen::emitBuiltinIO(const CallExpr &e) {
    if (!native_fn_arg_counts_.count(e.callee))
        return nullptr;

    // Helper: emit a call to __ry_<name> with ptr args, validate count and types
    auto emitIOCall = [&](const std::string &name, size_t nargs,
                          llvm::Type *retTy) -> llvm::Value * {
        if (e.args.size() != nargs)
            codegenError(name + "() takes exactly " + std::to_string(nargs) + " argument(s)");
        std::vector<llvm::Value *> args;
        std::vector<llvm::Type *> argTys;
        for (size_t i = 0; i < nargs; ++i) {
            args.push_back(emitExpr(*e.args[i]));
            if (args.back()->getType() != ptrTy_)
                codegenError(name + "() requires str/ptr arguments");
            argTys.push_back(ptrTy_);
        }
        auto fnTy = llvm::FunctionType::get(retTy, argTys, false);
        auto fn = mod_->getOrInsertFunction("__ry_" + name, fnTy);
        return builder_.CreateCall(fn, args, name);
    };

    // Helper: emit conditional branch to build Result, deferring error construction to error path
    auto emitResultBranch = [&](llvm::Value *isErr, llvm::StructType *resTy,
                                 std::function<llvm::Value*()> buildOk,
                                 std::function<llvm::Value*()> buildErr) -> llvm::Value * {
        llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "io.ok", fn_);
        llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, "io.err", fn_);
        llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "io.merge", fn_);
        builder_.CreateCondBr(isErr, errBB, okBB);

        builder_.SetInsertPoint(okBB);
        llvm::Value *okVal = buildOk();
        builder_.CreateBr(mergeBB);
        okBB = builder_.GetInsertBlock();

        builder_.SetInsertPoint(errBB);
        llvm::Value *errVal = buildErr();
        builder_.CreateBr(mergeBB);
        errBB = builder_.GetInsertBlock();

        builder_.SetInsertPoint(mergeBB);
        llvm::PHINode *phi = builder_.CreatePHI(resTy, 2, "io_result");
        phi->addIncoming(okVal, okBB);
        phi->addIncoming(errVal, errBB);
        return phi;
    };

    // Helper: build Error struct from __ry_get_last_error()
    auto buildErrorFromLastError = [&]() -> llvm::Value * {
        auto errFnTy = llvm::FunctionType::get(ptrTy_, {}, false);
        auto errFn = mod_->getOrInsertFunction("__ry_get_last_error", errFnTy);
        llvm::Value *errMsg = builder_.CreateCall(errFn, {}, "err_msg");
        llvm::Value *errStruct = llvm::UndefValue::get(errorTy_);
        errStruct = builder_.CreateInsertValue(errStruct, errMsg, 0, "err.msg");
        errStruct = builder_.CreateInsertValue(errStruct, llvm::ConstantInt::get(i64Ty_, 0), 1, "err.code");
        return errStruct;
    };

    // Helper: wrap a nullable ptr result into Result<str, Error>
    auto wrapPtrResult = [&](llvm::Value *ptr) -> llvm::Value * {
        llvm::StructType *resTy = getResultType(ptrTy_, errorTy_);
        llvm::Value *isNull = builder_.CreateICmpEQ(ptr,
            llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)), "is_null");
        return emitResultBranch(isNull, resTy,
            [&]() { return buildOkValue(ptr, resTy); },
            [&]() { return buildErrValue(buildErrorFromLastError(), resTy); });
    };

    // Helper: wrap an int64 status (0=ok, non-zero=err) into Result<Unit, Error>
    auto wrapStatusResult = [&](llvm::Value *status) -> llvm::Value * {
        llvm::StructType *resTy = getResultType(i8Ty_, errorTy_);
        llvm::Value *isErr = builder_.CreateICmpNE(status,
            llvm::ConstantInt::get(i64Ty_, 0), "is_err");
        return emitResultBranch(isErr, resTy,
            [&]() { return buildOkValue(llvm::ConstantInt::get(i8Ty_, 0), resTy); },
            [&]() { return buildErrValue(buildErrorFromLastError(), resTy); });
    };

    // 0-arg -> str (stdin functions: no Result wrapping)
    if (e.callee == "read_line" || e.callee == "read_all")
        return emitIOCall(e.callee, 0, ptrTy_);

    // read_text(path) -> Result<str, Error>
    if (e.callee == "read_text") {
        llvm::Value *ptr = emitIOCall(e.callee, 1, ptrTy_);
        return wrapPtrResult(ptr);
    }

    // bytes_to_str(bytes) -> Result<str, Error>
    if (e.callee == "bytes_to_str") {
        llvm::Value *ptr = emitIOCall(e.callee, 1, ptrTy_);
        return wrapPtrResult(ptr);
    }

    // write_text(path, content) -> Result<Unit, Error>
    if (e.callee == "write_text" || e.callee == "append_text") {
        llvm::Value *status = emitIOCall(e.callee, 2, i64Ty_);
        return wrapStatusResult(status);
    }

    // write_bytes(path, bytes) -> Result<Unit, Error>
    if (e.callee == "write_bytes") {
        llvm::Value *status = emitIOCall(e.callee, 2, i64Ty_);
        return wrapStatusResult(status);
    }

    // delete_file(path) -> Result<Unit, Error>
    if (e.callee == "delete_file") {
        llvm::Value *status = emitIOCall(e.callee, 1, i64Ty_);
        return wrapStatusResult(status);
    }

    // file_exists(path) -> bool (i64 truncated to i1) — no Result wrapping
    if (e.callee == "file_exists") {
        llvm::Value *result = emitIOCall(e.callee, 1, i64Ty_);
        return builder_.CreateTrunc(result, i1Ty_, "file_exists_bool");
    }

    // read_bytes(path) -> Result<List<byte>, Error>
    if (e.callee == "read_bytes") {
        llvm::Value *ptr = emitIOCall(e.callee, 1, ptrTy_);
        llvm::StructType *resTy = getResultType(ptrTy_, errorTy_);
        llvm::Value *isNull = builder_.CreateICmpEQ(ptr,
            llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)), "is_null");
        llvm::Value *result = emitResultBranch(isNull, resTy,
            [&]() { return buildOkValue(ptr, resTy); },
            [&]() { return buildErrValue(buildErrorFromLastError(), resTy); });
        list_element_types_[result] = i8Ty_;
        return result;
    }

    // str_to_bytes(str) -> List<byte> — always succeeds, no Result wrapping
    if (e.callee == "str_to_bytes") {
        llvm::Value *result = emitIOCall(e.callee, 1, ptrTy_);
        list_element_types_[result] = i8Ty_;
        return result;
    }

    return nullptr;
}

// ===== Builtin Net =====

llvm::Value *CodeGen::emitBuiltinNet(const CallExpr &e) {
    if (!native_fn_arg_counts_.count(e.callee))
        return nullptr;

    // Helper: wrap a nullable ptr result in Result<ptr, Error> and register it in a tracking set
    auto emitPtrToResult = [&](llvm::Value *ptr, const std::string &name,
                               const std::string &errMsg,
                               std::unordered_set<llvm::Value*> &trackingSet) -> llvm::Value * {
        llvm::Value *isNull = builder_.CreateICmpEQ(ptr,
            llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)), name + "_null");
        llvm::StructType *resTy = getResultType(ptrTy_, errorTy_);
        llvm::Value *okVal = buildOkValue(ptr, resTy);
        llvm::Value *errVal = buildErrValue(buildStaticError(errMsg, "." + name + "_err_msg"), resTy);
        llvm::Value *res = builder_.CreateSelect(isNull, errVal, okVal, name + "_result");
        trackingSet.insert(res);
        return res;
    };

    // bind(host, port) -> Result<TcpListener, Error>
    if (e.callee == "bind") {
        if (e.args.size() != 2)
            codegenError("bind() takes exactly 2 arguments");
        llvm::Value *host = emitExpr(*e.args[0]);
        llvm::Value *port = emitExpr(*e.args[1]);
        auto fnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, i64Ty_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_bind", fnTy);
        llvm::Value *result = builder_.CreateCall(fn, {host, port}, "bind_result");
        return emitPtrToResult(result, "bind", "bind failed", tcp_listener_values_);
    }

    // listen(listener, backlog) -> Result<Unit, Error>
    if (e.callee == "listen") {
        if (e.args.size() != 2)
            codegenError("listen() takes exactly 2 arguments");
        llvm::Value *listener = emitExpr(*e.args[0]);
        if (!isTcpListener(listener))
            codegenError("listen() requires TcpListener as first argument");
        llvm::Value *backlog = emitExpr(*e.args[1]);
        auto fnTy = llvm::FunctionType::get(i64Ty_, {ptrTy_, i64Ty_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_listen", fnTy);
        llvm::Value *status = builder_.CreateCall(fn, {listener, backlog}, "listen_status");
        // Wrap in Result<Unit, Error>
        llvm::Value *isErr = builder_.CreateICmpNE(status,
            llvm::ConstantInt::get(i64Ty_, 0), "listen_err");
        llvm::StructType *resTy = getResultType(i8Ty_, errorTy_);
        llvm::Value *okVal = buildOkValue(llvm::ConstantInt::get(i8Ty_, 0), resTy);
        llvm::Value *errVal = buildErrValue(buildStaticError("listen failed", ".listen_err_msg"), resTy);
        return builder_.CreateSelect(isErr, errVal, okVal, "listen_result");
    }

    // listener_port(listener) -> int
    if (e.callee == "listener_port") {
        if (e.args.size() != 1)
            codegenError("listener_port() takes exactly 1 argument");
        llvm::Value *listener = emitExpr(*e.args[0]);
        if (!isTcpListener(listener))
            codegenError("listener_port() requires TcpListener argument");
        auto fnTy = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_listener_port", fnTy);
        return builder_.CreateCall(fn, {listener}, "listener_port");
    }

    // shutdown(listener) -> Unit
    if (e.callee == "shutdown") {
        if (e.args.size() != 1)
            codegenError("shutdown() takes exactly 1 argument");
        llvm::Value *val = emitExpr(*e.args[0]);
        if (!isTcpListener(val))
            codegenError("shutdown() requires TcpListener argument");
        auto *voidPtrFnTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_tcp_listener_shutdown", voidPtrFnTy);
        return builder_.CreateCall(fn, {val});
    }

    // accept(listener) -> Result<TcpStream, Error>
    if (e.callee == "accept") {
        if (e.args.size() != 1)
            codegenError("accept() takes exactly 1 argument");
        llvm::Value *listener = emitExpr(*e.args[0]);
        if (!isTcpListener(listener))
            codegenError("accept() requires TcpListener argument");
        auto fnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_accept", fnTy);
        llvm::Value *result = builder_.CreateCall(fn, {listener}, "accept_result");
        return emitPtrToResult(result, "accept", "accept failed", tcp_stream_values_);
    }

    // connect(host, port) -> Result<TcpStream, Error>
    if (e.callee == "connect") {
        if (e.args.size() != 2)
            codegenError("connect() takes exactly 2 arguments");
        llvm::Value *host = emitExpr(*e.args[0]);
        llvm::Value *port = emitExpr(*e.args[1]);
        auto fnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, i64Ty_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_connect", fnTy);
        llvm::Value *result = builder_.CreateCall(fn, {host, port}, "connect_result");
        return emitPtrToResult(result, "connect", "connection failed", tcp_stream_values_);
    }

    return nullptr;
}

// ===== Builtin HTTP =====

llvm::Value *CodeGen::emitBuiltinHttp(const CallExpr &e) {
    if (!native_fn_arg_counts_.count(e.callee))
        return nullptr;

    // http_response(status, headers, body) -> HttpResponse
    if (e.callee == "http_response") {
        if (e.args.size() != 3)
            codegenError("http_response() takes exactly 3 arguments");
        llvm::Value *status = emitExpr(*e.args[0]);
        llvm::Value *headers = emitExpr(*e.args[1]);
        llvm::Value *body = emitExpr(*e.args[2]);
        if (status->getType() != i64Ty_)
            codegenError("http_response() status must be int");
        if (headers->getType() != ptrTy_)
            codegenError("http_response() headers must be Map<str, str>");
        if (body->getType() != ptrTy_)
            codegenError("http_response() body must be str");
        auto fnTy = llvm::FunctionType::get(ptrTy_, {i64Ty_, ptrTy_, ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_http_response_create", fnTy);
        llvm::Value *result = builder_.CreateCall(fn, {status, headers, body}, "http_resp");
        http_response_values_.insert(result);
        return result;
    }

    // http_method/http_path/http_body: single-arg HttpRequest accessors returning str
    if (e.callee == "http_method" || e.callee == "http_path" || e.callee == "http_body") {
        if (e.args.size() != 1)
            codegenError(e.callee + "() takes exactly 1 argument");
        llvm::Value *req = emitExpr(*e.args[0]);
        if (!isHttpRequest(req))
            codegenError(e.callee + "() requires HttpRequest argument");
        auto fnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_" + e.callee, fnTy);
        return builder_.CreateCall(fn, {req}, e.callee);
    }

    // http_header(req, key) -> Option<str> / http_query(req, key) -> Option<str> / http_cookie(req, name) -> Option<str>
    if (e.callee == "http_header" || e.callee == "http_query" || e.callee == "http_cookie") {
        if (e.args.size() != 2)
            codegenError(e.callee + "() takes exactly 2 arguments");
        llvm::Value *req = emitExpr(*e.args[0]);
        llvm::Value *key = emitExpr(*e.args[1]);
        if (!isHttpRequest(req))
            codegenError(e.callee + "() requires HttpRequest argument");
        if (key->getType() != ptrTy_) {
            std::string param = (e.callee == "http_cookie") ? "name" : "key";
            codegenError(e.callee + "() " + param + " must be str");
        }
        auto fnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_" + e.callee, fnTy);
        std::string hint = (e.callee == "http_header") ? "http_hdr"
                         : (e.callee == "http_query") ? "http_qry" : "http_ck";
        llvm::Value *result = builder_.CreateCall(fn, {req, key}, hint);
        llvm::Value *isNull = builder_.CreateICmpEQ(result,
            llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)), hint + "_null");
        llvm::StructType *optTy = getOptionType(ptrTy_);
        llvm::Value *someVal = buildSomeValue(result, optTy);
        llvm::Value *noneVal = buildNoneValue(optTy);
        return builder_.CreateSelect(isNull, noneVal, someVal, hint + "_opt");
    }

    // http_query_all(req) -> Map<str, str>
    if (e.callee == "http_query_all") {
        if (e.args.size() != 1)
            codegenError("http_query_all() takes exactly 1 argument");
        llvm::Value *req = emitExpr(*e.args[0]);
        if (!isHttpRequest(req))
            codegenError("http_query_all() requires HttpRequest argument");
        auto fnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_http_query_all", fnTy);
        llvm::Value *result = builder_.CreateCall(fn, {req}, "http_qry_all");
        map_key_types_[result] = ptrTy_;
        map_value_types_[result] = ptrTy_;
        return result;
    }

    // http_cookies(req) -> Map<str, str>
    if (e.callee == "http_cookies") {
        if (e.args.size() != 1)
            codegenError("http_cookies() takes exactly 1 argument");
        llvm::Value *req = emitExpr(*e.args[0]);
        if (!isHttpRequest(req))
            codegenError("http_cookies() requires HttpRequest argument");
        auto fnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_http_cookies", fnTy);
        llvm::Value *result = builder_.CreateCall(fn, {req}, "http_cookies");
        map_key_types_[result] = ptrTy_;
        map_value_types_[result] = ptrTy_;
        return result;
    }

    // http_listen(host, port, handler) -> Unit
    if (e.callee == "http_listen") {
        if (e.args.size() != 3)
            codegenError("http_listen() takes exactly 3 arguments");
        llvm::Value *host = emitExpr(*e.args[0]);
        llvm::Value *port = emitExpr(*e.args[1]);
        llvm::Value *handler = emitExpr(*e.args[2]);
        if (host->getType() != ptrTy_)
            codegenError("http_listen() host must be str");
        if (port->getType() != i64Ty_)
            codegenError("http_listen() port must be int");

        // Get handler FnTypeInfo
        FnTypeInfo handlerInfo;
        bool foundInfo = false;
        // Try fn_type_info_ lookup on handler value directly
        auto fnIt = fn_type_info_.find(handler);
        if (fnIt != fn_type_info_.end()) {
            handlerInfo = fnIt->second;
            foundInfo = true;
        }
        if (!foundInfo) {
            // Check if handler is a LoadInst, look up the alloca
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(handler)) {
                auto fnIt2 = fn_type_info_.find(load->getPointerOperand());
                if (fnIt2 != fn_type_info_.end()) {
                    handlerInfo = fnIt2->second;
                    foundInfo = true;
                }
            }
        }
        if (!foundInfo)
            codegenError("http_listen() handler must be a function fn(HttpRequest) -> HttpResponse");

        // Validate handler signature: one ptr param, ptr return
        if (handlerInfo.paramTypes.size() != 1 ||
            handlerInfo.returnType != ptrTy_ ||
            handlerInfo.paramTypes[0] != ptrTy_) {
            codegenError("http_listen() handler must be a function fn(HttpRequest) -> HttpResponse");
        }

        // 1. bind(host, port)
        auto bindFnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, i64Ty_}, false);
        auto bindFn = mod_->getOrInsertFunction("__ry_bind", bindFnTy);
        llvm::Value *listener = builder_.CreateCall(bindFn, {host, port}, "http_listener");

        // null check
        llvm::Value *isNull = builder_.CreateICmpEQ(listener,
            llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)), "bind_null");
        llvm::BasicBlock *bindFailBB = llvm::BasicBlock::Create(*ctx_, "http.bind_fail", fn_);
        llvm::BasicBlock *bindOkBB = llvm::BasicBlock::Create(*ctx_, "http.bind_ok", fn_);
        builder_.CreateCondBr(isNull, bindFailBB, bindOkBB);

        builder_.SetInsertPoint(bindFailBB);
        static int httpErrCounter = 0;
        emitRuntimeError("runtime error: http_listen() bind failed\n",
                          ".http_err_" + std::to_string(httpErrCounter++));

        // 2. listen(listener, 128)
        builder_.SetInsertPoint(bindOkBB);
        auto listenFnTy = llvm::FunctionType::get(i64Ty_, {ptrTy_, i64Ty_}, false);
        auto listenFn = mod_->getOrInsertFunction("__ry_listen", listenFnTy);
        llvm::Value *listenStatus = builder_.CreateCall(listenFn, {listener, llvm::ConstantInt::get(i64Ty_, 128)}, "listen_status");
        // Check listen result — fatal for http_listen
        llvm::Value *listenFailed = builder_.CreateICmpNE(listenStatus,
            llvm::ConstantInt::get(i64Ty_, 0), "listen_failed");
        llvm::BasicBlock *listenFailBB = llvm::BasicBlock::Create(*ctx_, "http.listen_fail", fn_);
        llvm::BasicBlock *listenOkBB = llvm::BasicBlock::Create(*ctx_, "http.listen_ok", fn_);
        builder_.CreateCondBr(listenFailed, listenFailBB, listenOkBB);
        builder_.SetInsertPoint(listenFailBB);
        static int httpListenErrCounter = 0;
        emitRuntimeError("runtime error: http_listen() listen failed\n",
                          ".http_listen_err_" + std::to_string(httpListenErrCounter++));
        builder_.SetInsertPoint(listenOkBB);

        // 3. accept loop
        llvm::BasicBlock *loopBB = llvm::BasicBlock::Create(*ctx_, "http.loop", fn_);
        llvm::BasicBlock *loopBodyBB = llvm::BasicBlock::Create(*ctx_, "http.loop_body", fn_);
        llvm::BasicBlock *loopEndBB = llvm::BasicBlock::Create(*ctx_, "http.loop_end", fn_);

        builder_.CreateBr(loopBB);
        builder_.SetInsertPoint(loopBB);

        // accept(listener) -> conn
        auto acceptFnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_}, false);
        auto acceptFn = mod_->getOrInsertFunction("__ry_accept", acceptFnTy);
        llvm::Value *conn = builder_.CreateCall(acceptFn, {listener}, "http_conn");

        llvm::Value *connNull = builder_.CreateICmpEQ(conn,
            llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)), "conn_null");
        // Retry accept on null (e.g., transient errors) instead of exiting the loop
        builder_.CreateCondBr(connNull, loopBB, loopBodyBB);

        // Loop body: read request, call handler, send response, cleanup
        builder_.SetInsertPoint(loopBodyBB);

        // __ry_http_read_request(conn) -> req
        auto readReqFnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_}, false);
        auto readReqFn = mod_->getOrInsertFunction("__ry_http_read_request", readReqFnTy);
        llvm::Value *req = builder_.CreateCall(readReqFn, {conn}, "http_req");
        http_request_values_.insert(req);

        // Check if req is null (malformed request)
        llvm::Value *reqNull = builder_.CreateICmpEQ(req,
            llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)), "req_null");
        llvm::BasicBlock *reqOkBB = llvm::BasicBlock::Create(*ctx_, "http.req_ok", fn_);
        llvm::BasicBlock *reqBadBB = llvm::BasicBlock::Create(*ctx_, "http.req_bad", fn_);
        builder_.CreateCondBr(reqNull, reqBadBB, reqOkBB);

        // Shared function types for cleanup calls
        auto voidPtrFnTy = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
        auto closeFn = mod_->getOrInsertFunction("__ry_tcp_close", voidPtrFnTy);
        auto freeReqFn = mod_->getOrInsertFunction("__ry_http_request_free", voidPtrFnTy);
        auto freeRespFn = mod_->getOrInsertFunction("__ry_http_response_free", voidPtrFnTy);

        // Bad request: close conn and continue loop
        builder_.SetInsertPoint(reqBadBB);
        builder_.CreateCall(closeFn, {conn});
        builder_.CreateBr(loopBB);

        builder_.SetInsertPoint(reqOkBB);

        // Call handler(req) -> resp
        llvm::Value *resp = emitLambdaCall(handler, handlerInfo, {req}, "http_resp_val");
        http_response_values_.insert(resp);

        // __ry_http_send_response(conn, resp)
        auto sendRespFnTy = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), {ptrTy_, ptrTy_}, false);
        auto sendRespFn = mod_->getOrInsertFunction("__ry_http_send_response", sendRespFnTy);
        builder_.CreateCall(sendRespFn, {conn, resp});

        // Cleanup
        builder_.CreateCall(freeReqFn, {req});
        builder_.CreateCall(freeRespFn, {resp});
        builder_.CreateCall(closeFn, {conn});
        builder_.CreateBr(loopBB);

        // Loop end (unreachable in practice — accept retries on null)
        builder_.SetInsertPoint(loopEndBB);

        return llvm::ConstantInt::get(i64Ty_, 0);
    }

    // Helper: wrap a nullable ptr result in Result<ptr, Error> and register it in a tracking set
    auto emitPtrToResult = [&](llvm::Value *ptr, const std::string &name,
                               const std::string &errMsg,
                               std::unordered_set<llvm::Value*> &trackingSet) -> llvm::Value * {
        llvm::Value *isNull = builder_.CreateICmpEQ(ptr,
            llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)), name + "_null");
        llvm::StructType *resTy = getResultType(ptrTy_, errorTy_);
        llvm::Value *okVal = buildOkValue(ptr, resTy);
        llvm::Value *errVal = buildErrValue(buildStaticError(errMsg, "." + name + "_err_msg"), resTy);
        llvm::Value *res = builder_.CreateSelect(isNull, errVal, okVal, name + "_result");
        trackingSet.insert(res);
        return res;
    };

    // http_get(url) -> Result<HttpClientResponse, Error>
    if (e.callee == "http_get") {
        if (e.args.size() != 1)
            codegenError("http_get() takes exactly 1 argument");
        llvm::Value *url = emitExpr(*e.args[0]);
        if (url->getType() != ptrTy_)
            codegenError("http_get() url must be str");
        auto fnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_http_get", fnTy);
        llvm::Value *result = builder_.CreateCall(fn, {url}, "http_get_result");
        return emitPtrToResult(result, "http_get", "HTTP request failed", http_client_response_values_);
    }

    // http_post(url, body, headers) -> Result<HttpClientResponse, Error>
    if (e.callee == "http_post") {
        if (e.args.size() != 3)
            codegenError("http_post() takes exactly 3 arguments");
        llvm::Value *url = emitExpr(*e.args[0]);
        llvm::Value *body = emitExpr(*e.args[1]);
        llvm::Value *headers = emitExpr(*e.args[2]);
        if (url->getType() != ptrTy_)
            codegenError("http_post() url must be str");
        if (body->getType() != ptrTy_)
            codegenError("http_post() body must be str");
        if (headers->getType() != ptrTy_)
            codegenError("http_post() headers must be Map<str, str>");
        auto fnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_http_post", fnTy);
        llvm::Value *result = builder_.CreateCall(fn, {url, body, headers}, "http_post_result");
        return emitPtrToResult(result, "http_post", "HTTP request failed", http_client_response_values_);
    }

    // http_request(method, url, headers, body) -> Result<HttpClientResponse, Error>
    if (e.callee == "http_request") {
        if (e.args.size() != 4)
            codegenError("http_request() takes exactly 4 arguments");
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
        auto fnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, ptrTy_, ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_http_client_request", fnTy);
        llvm::Value *result = builder_.CreateCall(fn, {method, url, headers, body}, "http_request_result");
        return emitPtrToResult(result, "http_request", "HTTP request failed", http_client_response_values_);
    }

    // http_client_status(resp) -> int
    if (e.callee == "http_client_status") {
        if (e.args.size() != 1)
            codegenError("http_client_status() takes exactly 1 argument");
        llvm::Value *resp = emitExpr(*e.args[0]);
        if (!isHttpClientResponse(resp))
            codegenError("http_client_status() requires HttpClientResponse argument");
        auto fnTy = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_http_client_status", fnTy);
        return builder_.CreateCall(fn, {resp}, "http_client_status");
    }

    // http_client_body(resp) -> str
    if (e.callee == "http_client_body") {
        if (e.args.size() != 1)
            codegenError("http_client_body() takes exactly 1 argument");
        llvm::Value *resp = emitExpr(*e.args[0]);
        if (!isHttpClientResponse(resp))
            codegenError("http_client_body() requires HttpClientResponse argument");
        auto fnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_http_client_body", fnTy);
        return builder_.CreateCall(fn, {resp}, "http_client_body");
    }

    // http_client_header(resp, key) -> Option<str>
    if (e.callee == "http_client_header") {
        if (e.args.size() != 2)
            codegenError("http_client_header() takes exactly 2 arguments");
        llvm::Value *resp = emitExpr(*e.args[0]);
        llvm::Value *key = emitExpr(*e.args[1]);
        if (!isHttpClientResponse(resp))
            codegenError("http_client_header() requires HttpClientResponse argument");
        if (key->getType() != ptrTy_)
            codegenError("http_client_header() key must be str");
        auto fnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_http_client_header", fnTy);
        llvm::Value *result = builder_.CreateCall(fn, {resp, key}, "http_client_hdr");
        llvm::Value *isNull = builder_.CreateICmpEQ(result,
            llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)), "http_client_hdr_null");
        llvm::StructType *optTy = getOptionType(ptrTy_);
        llvm::Value *someVal = buildSomeValue(result, optTy);
        llvm::Value *noneVal = buildNoneValue(optTy);
        return builder_.CreateSelect(isNull, noneVal, someVal, "http_client_hdr_opt");
    }

    // http_client_response_free(resp) -> Unit
    if (e.callee == "http_client_response_free") {
        if (e.args.size() != 1)
            codegenError("http_client_response_free() takes exactly 1 argument");
        llvm::Value *resp = emitExpr(*e.args[0]);
        if (!isHttpClientResponse(resp))
            codegenError("http_client_response_free() requires HttpClientResponse argument");
        auto fnTy = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_http_client_response_free", fnTy);
        builder_.CreateCall(fn, {resp});
        return llvm::ConstantInt::get(i64Ty_, 0);
    }

    return nullptr;
}

