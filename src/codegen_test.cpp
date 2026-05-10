#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"
#include <algorithm>
#include <cmath>
#include <initializer_list>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <stdexcept>


namespace ry {

// ===== Outline helper =====

void CodeGen::emitOutlinePrintf(const std::string &label, llvm::Value *nameVal) {
    auto printfFn = getStdlibPrintf();
    std::string indent(static_cast<size_t>(outline_depth_ * 2), ' ');
    llvm::Value *fmt = cachedGlobalString(indent + label, ".outline_fmt");
    if (nameVal)
        builder_.CreateCall(printfFn, {fmt, nameVal});
    else
        builder_.CreateCall(printfFn, {fmt});
}

// ===== Test helpers =====

llvm::SmallVector<llvm::Value*, 4> CodeGen::loadCapturedArgs(const OverloadEntry &entry, const std::string &directive) {
    llvm::SmallVector<llvm::Value*, 4> args;
    for (const auto &capName : entry.capturedNames) {
        llvm::AllocaInst *alloca = findVar(capName);
        if (!alloca) {
            std::string capErrMsg = directive;
            capErrMsg += ": captured variable '";
            capErrMsg += capName;
            capErrMsg += "' not found in scope";
            codegenError(capErrMsg);
        }
        args.push_back(builder_.CreateLoad(alloca->getAllocatedType(), alloca, capName + ".cap_pass"));
    }
    return args;
}

std::pair<llvm::FunctionCallee, llvm::FunctionCallee> CodeGen::getTestItFunctions() {
    llvm::FunctionType *voidStrTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
    llvm::FunctionType *voidTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), false);
    return {
        mod_->getOrInsertFunction("__ry_test_it_begin", voidStrTy),
        mod_->getOrInsertFunction("__ry_test_it_end", voidTy)
    };
}

std::pair<llvm::FunctionCallee, llvm::FunctionCallee> CodeGen::getTestDescribeFunctions() {
    llvm::FunctionType *voidStrTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
    llvm::FunctionType *voidTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), false);
    return {
        mod_->getOrInsertFunction("__ry_test_describe_begin", voidStrTy),
        mod_->getOrInsertFunction("__ry_test_describe_end", voidTy)
    };
}

// Helper: create a test function, bind params, emit body, verify
llvm::Function *CodeGen::emitTestFunction(
    const std::string &namePrefix,
    const std::vector<llvm::Type*> &paramTypes,
    LambdaExpr &lam, const std::string &context) {

    std::string testFnName = namePrefix + std::to_string(test_fn_counter_++);
    llvm::FunctionType *testFt = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), paramTypes, false);
    llvm::Function *testFunc = llvm::Function::Create(
        testFt, llvm::Function::InternalLinkage, testFnName, *mod_);

    {
        FnScope guard(*this);
        fn_ = testFunc;
        pushScope();

        llvm::BasicBlock *entry = llvm::BasicBlock::Create(*ctx_, "entry", testFunc);
        builder_.SetInsertPoint(entry);

        for (unsigned i = 0; i < paramTypes.size(); ++i) {
            llvm::Argument *arg = testFunc->getArg(i);
            arg->setName(lam.params[i].name);
            llvm::AllocaInst *alloca = builder_.CreateAlloca(paramTypes[i], nullptr, lam.params[i].name);
            builder_.CreateStore(arg, alloca);
            scope_stack_.back()[lam.params[i].name] = alloca;
            immutable_scope_stack_.back().insert(lam.params[i].name);
        }

        for (auto &stmt : lam.body)
            std::visit([this](auto &st) { emitStmt(st); }, stmt);

        if (!builder_.GetInsertBlock()->getTerminator())
            builder_.CreateRetVoid();

        std::string err;
        llvm::raw_string_ostream errStream(err);
        if (llvm::verifyFunction(*testFunc, &errStream))
            codegenError("IR verify error in " + context + ": " + err);
    }

    return testFunc;
}

// Helper: parse format placeholders like {0}, {1} → C format string + field indices
static void parseFormatPlaceholders(const std::string &fmtStr,
                                     std::string &cFmt, std::vector<unsigned> &fieldOrder) {
    for (size_t i = 0; i < fmtStr.size(); ++i) {
        if (fmtStr[i] == '{' && i + 2 < fmtStr.size() && fmtStr[i+2] == '}' &&
            fmtStr[i+1] >= '0' && fmtStr[i+1] <= '9') {
            cFmt += "%s";
            fieldOrder.push_back(static_cast<unsigned>(fmtStr[i+1] - '0'));
            i += 2;
        } else if (fmtStr[i] == '%') {
            cFmt += "%%";
        } else {
            cFmt += fmtStr[i];
        }
    }
}

void CodeGen::emitEachItLoop(llvm::Value *listPtr, llvm::Type *elemTy, unsigned numFields,
                              const std::string &fmtStr, llvm::Function *testFunc,
                              const std::vector<llvm::Value*> &capturedVals) {
    llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, listPtr, 0, "each_len_ptr");
    llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "each_len");
    llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, listPtr, 2, "each_data_ptr");
    llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataPtrField, "each_data");

    auto [itBeginFn, itEndFn] = getTestItFunctions();
    auto snprintfFn = getStdlibSnprintf();

    std::string cFmt;
    std::vector<unsigned> fieldOrder;
    parseFormatPlaceholders(fmtStr, cFmt, fieldOrder);

    llvm::Value *iAlloca = builder_.CreateAlloca(i64Ty_, nullptr, "each_i");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iAlloca);

    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "each.cond", fn_);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "each.body", fn_);
    llvm::BasicBlock *endBB  = llvm::BasicBlock::Create(*ctx_, "each.end",  fn_);

    builder_.CreateBr(condBB);
    builder_.SetInsertPoint(condBB);
    llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iAlloca, "i");
    llvm::Value *cond = builder_.CreateICmpSLT(iVal, length, "each_cond");
    builder_.CreateCondBr(cond, bodyBB, endBB);

    builder_.SetInsertPoint(bodyBB);
    llvm::Value *elemPtr = builder_.CreateGEP(elemTy, dataPtr, {iVal}, "each_elem_ptr");
    llvm::Value *tupleVal = builder_.CreateLoad(elemTy, elemPtr, "each_tuple");

    std::vector<llvm::Value*> fieldVals;
    std::vector<llvm::Value*> fieldStrs;
    fieldVals.reserve(numFields);
    fieldStrs.reserve(numFields);
    for (unsigned i = 0; i < numFields; ++i) {
        llvm::Value *field = builder_.CreateExtractValue(tupleVal, i, "field_" + std::to_string(i));
        fieldVals.push_back(field);
        fieldStrs.push_back(valueToString(field));
    }

    llvm::Value *fmtBuf = builder_.CreateAlloca(
        llvm::ArrayType::get(llvm::Type::getInt8Ty(*ctx_), 256), nullptr, "fmt_buf");
    llvm::Value *fmtGlobal = cachedGlobalString(cFmt, ".each_fmt");

    for (unsigned idx : fieldOrder) {
        if (idx >= fieldStrs.size())
            codegenError("@each: placeholder {" + std::to_string(idx) + "} exceeds tuple arity");
    }

    std::vector<llvm::Value*> snprintfArgs = {
        fmtBuf, llvm::ConstantInt::get(i64Ty_, 256), fmtGlobal
    };
    for (unsigned idx : fieldOrder)
        snprintfArgs.push_back(fieldStrs[idx]);
    builder_.CreateCall(snprintfFn, snprintfArgs);

    llvm::SmallVector<llvm::Value*, 8> callArgs(fieldVals.begin(), fieldVals.end());
    callArgs.append(capturedVals.begin(), capturedVals.end());
    builder_.CreateCall(itBeginFn, {fmtBuf});
    builder_.CreateCall(testFunc, callArgs);
    builder_.CreateCall(itEndFn);

    llvm::Value *nextI = builder_.CreateAdd(iVal, llvm::ConstantInt::get(i64Ty_, 1), "next_i");
    builder_.CreateStore(nextI, iAlloca);
    builder_.CreateBr(condBB);

    builder_.SetInsertPoint(endBB);
}

void CodeGen::emitPropertyItLoop(llvm::Function *testFunc, llvm::Value *descVal,
                                  const std::vector<llvm::Type*> &paramTypes,
                                  const std::vector<std::string> &paramNames, int64_t count,
                                  const std::vector<llvm::Value*> &capturedVals) {
    auto [itBeginFn, itEndFn] = getTestItFunctions();

    llvm::FunctionType *initRngTy = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), false);
    llvm::FunctionCallee initRngFn = mod_->getOrInsertFunction("__ry_test_prop_init_rng", initRngTy);
    llvm::FunctionType *randIntTy = llvm::FunctionType::get(i64Ty_, false);
    llvm::FunctionCallee randIntFn = mod_->getOrInsertFunction("__ry_test_rand_int", randIntTy);
    llvm::FunctionType *randFloatTy = llvm::FunctionType::get(f64Ty_, false);
    llvm::FunctionCallee randFloatFn = mod_->getOrInsertFunction("__ry_test_rand_float", randFloatTy);
    llvm::FunctionType *randBoolTy = llvm::FunctionType::get(i64Ty_, false);
    llvm::FunctionCallee randBoolFn = mod_->getOrInsertFunction("__ry_test_rand_bool", randBoolTy);
    llvm::FunctionType *randStrTy = llvm::FunctionType::get(ptrTy_, false);
    llvm::FunctionCallee randStrFn = mod_->getOrInsertFunction("__ry_test_rand_str", randStrTy);
    llvm::FunctionType *isFailedTy = llvm::FunctionType::get(i64Ty_, false);
    llvm::FunctionCallee isFailedFn = mod_->getOrInsertFunction("__ry_test_it_is_failed", isFailedTy);

    builder_.CreateCall(initRngFn);
    builder_.CreateCall(itBeginFn, {descVal});

    llvm::Value *iAlloca = builder_.CreateAlloca(i64Ty_, nullptr, "prop_i");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iAlloca);

    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "prop.cond", fn_);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "prop.body", fn_);
    llvm::BasicBlock *endBB  = llvm::BasicBlock::Create(*ctx_, "prop.end",  fn_);

    builder_.CreateBr(condBB);
    builder_.SetInsertPoint(condBB);
    llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iAlloca, "i");
    llvm::Value *cond = builder_.CreateICmpSLT(iVal, llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(count)), "prop_cond");
    builder_.CreateCondBr(cond, bodyBB, endBB);

    builder_.SetInsertPoint(bodyBB);

    std::vector<llvm::Value*> randVals;
    for (unsigned i = 0; i < paramTypes.size(); ++i) {
        llvm::Value *val;
        if (paramTypes[i] == i64Ty_) {
            val = builder_.CreateCall(randIntFn, {}, "rand_int");
        } else if (paramTypes[i] == f64Ty_) {
            val = builder_.CreateCall(randFloatFn, {}, "rand_float");
        } else if (paramTypes[i] == i1Ty_) {
            llvm::Value *r = builder_.CreateCall(randBoolFn, {}, "rand_bool_i64");
            val = builder_.CreateICmpNE(r, llvm::ConstantInt::get(i64Ty_, 0), "rand_bool");
        } else if (paramTypes[i] == ptrTy_) {
            val = builder_.CreateCall(randStrFn, {}, "rand_str");
        } else {
            codegenError("@property: unsupported parameter type for '" + paramNames[i] + "'");
        }
        randVals.push_back(val);
    }

    llvm::SmallVector<llvm::Value*, 8> propCallArgs(randVals.begin(), randVals.end());
    propCallArgs.append(capturedVals.begin(), capturedVals.end());
    builder_.CreateCall(testFunc, propCallArgs);

    llvm::Value *failed = builder_.CreateCall(isFailedFn, {}, "is_failed");
    llvm::Value *didFail = builder_.CreateICmpNE(failed, llvm::ConstantInt::get(i64Ty_, 0), "did_fail");

    llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, "prop.fail", fn_);
    llvm::BasicBlock *contBB = llvm::BasicBlock::Create(*ctx_, "prop.cont", fn_);
    builder_.CreateCondBr(didFail, failBB, contBB);

    builder_.SetInsertPoint(failBB);
    {
        auto printfFn = getStdlibPrintf();
        // Fetch the current describe-nesting indent at runtime so Counterexample:
        // aligns with the surrounding test output regardless of nesting depth.
        llvm::FunctionType *indentTy = llvm::FunctionType::get(
            ptrTy_, {llvm::Type::getInt32Ty(*ctx_)}, false);
        llvm::FunctionCallee indentFn = mod_->getOrInsertFunction("__ry_test_indent", indentTy);
        llvm::Value *indentStr = builder_.CreateCall(
            indentFn, {llvm::ConstantInt::get(llvm::Type::getInt32Ty(*ctx_), 2)}, "ce_indent");

        std::string ceFmt = "%s\033[31mCounterexample: (";
        for (unsigned i = 0; i < paramTypes.size(); ++i) {
            if (i > 0) ceFmt += ", ";
            ceFmt += paramNames[i] + " = %s";
        }
        ceFmt += ")\033[0m\n";
        llvm::Value *ceFmtStr = cachedGlobalString(ceFmt, ".prop_ce_fmt");
        std::vector<llvm::Value*> ceArgs = {ceFmtStr, indentStr};
        for (unsigned i = 0; i < randVals.size(); ++i)
            ceArgs.push_back(valueToString(randVals[i]));
        builder_.CreateCall(printfFn, ceArgs);
    }
    for (unsigned i = 0; i < paramTypes.size(); ++i) {
        if (paramTypes[i] == ptrTy_) {
            llvm::Value *hdr = emitStrGetHeaderFromData(randVals[i]);
            builder_.CreateCall(getStdlibFree(), {hdr});
        }
    }
    builder_.CreateBr(endBB);

    builder_.SetInsertPoint(contBB);
    for (unsigned i = 0; i < paramTypes.size(); ++i) {
        if (paramTypes[i] == ptrTy_) {
            llvm::Value *hdr = emitStrGetHeaderFromData(randVals[i]);
            builder_.CreateCall(getStdlibFree(), {hdr});
        }
    }
    llvm::Value *nextI = builder_.CreateAdd(iVal, llvm::ConstantInt::get(i64Ty_, 1), "next_i");
    builder_.CreateStore(nextI, iAlloca);
    builder_.CreateBr(condBB);

    builder_.SetInsertPoint(endBB);
    builder_.CreateCall(itEndFn);
}

// ===== Test: @it / @describe directive on named functions =====

// Helper: erase directives matching any of the given names
static void stripDirectives(
    std::vector<Directive> &directives,
    std::initializer_list<const char*> names)
{
    directives.erase(
        std::remove_if(directives.begin(), directives.end(), [&](const Directive &d) {
            for (const char *n : names)
                if (d.name == n) return true;
            return false;
        }),
        directives.end());
}

void CodeGen::emitItDirective(std::unique_ptr<FnStmt> &s) {
    if (!test_mode_)
        codegenError("@it is only allowed in test mode (use 'ry test')");
    if (s->is_async)
        codegenError("@it: fn '" + s->name + "' cannot be async");
    if (!s->type_params.empty())
        codegenError("@it: fn '" + s->name + "' cannot be generic");
    if (s->return_type)
        codegenError("@it: fn '" + s->name + "' cannot have a return type annotation");

    if (hasDirective(s->directives, "each")) {
        emitEachItDirective(s);
        return;
    }
    if (hasDirective(s->directives, "property")) {
        emitPropertyItDirective(s);
        return;
    }

    // Basic @it: fn must have no parameters
    if (!s->params.empty())
        codegenError("@it: fn '" + s->name + "' has parameters but no @each or @property directive");

    std::string desc = getDirectivePositionalArg(s->directives, "it");

    if (outline_mode_) {
        llvm::Value *descVal = cachedGlobalString(desc, ".it_desc");
        emitOutlinePrintf("it %s\n", descVal);
        return;
    }

    // Strip @it and emit the function normally, then emit it_begin/call/it_end
    stripDirectives(s->directives, {"it"});
    emitStmt(s);

    auto *overloads = findFunction(s->name);
    if (!overloads || overloads->empty())
        codegenError("@it: internal error — fn '" + s->name + "' not found after emit");
    const auto &entry = overloads->back();
    auto capturedArgs = loadCapturedArgs(entry, "@it");

    auto [itBeginFn, itEndFn] = getTestItFunctions();
    llvm::Value *descVal = cachedGlobalString(desc, ".it_desc");
    builder_.CreateCall(itBeginFn, {descVal});
    builder_.CreateCall(entry.func, capturedArgs);
    builder_.CreateCall(itEndFn);
}

void CodeGen::emitEachItDirective(std::unique_ptr<FnStmt> &s) {
    Directive *eachDir = findDirective(s->directives, "each");
    if (!eachDir || eachDir->args.empty() || !eachDir->args[0].value || eachDir->args[0].name.has_value())
        codegenError("@each directive requires a list expression");

    std::string fmtStr = getDirectivePositionalArg(s->directives, "it");

    if (outline_mode_) {
        llvm::Value *fmtStrVal = cachedGlobalString(fmtStr, ".it_each_desc");
        emitOutlinePrintf("it %s (@each)\n", fmtStrVal);
        return;
    }

    llvm::Value *listPtr = emitExpr(*eachDir->args[0].value);
    llvm::Type *elemTy = getListElementType(listPtr);
    if (!elemTy)
        codegenError("@each requires a list of tuples");
    auto *tupleTy = llvm::dyn_cast<llvm::StructType>(elemTy);
    if (!tupleTy)
        codegenError("@each requires a list of tuples");

    unsigned numFields = tupleTy->getNumElements();
    if (numFields != s->params.size())
        codegenError("@each: tuple arity (" + std::to_string(numFields) +
                     ") doesn't match function parameter count (" + std::to_string(s->params.size()) + ")");

    stripDirectives(s->directives, {"it", "each"});
    emitStmt(s);

    auto *overloads = findFunction(s->name);
    if (!overloads || overloads->empty())
        codegenError("@each @it: internal error — fn '" + s->name + "' not found after emit");
    const auto &entry = overloads->back();
    auto capturedVals = loadCapturedArgs(entry, "@each @it");
    emitEachItLoop(listPtr, elemTy, numFields, fmtStr, entry.func,
                   std::vector<llvm::Value*>(capturedVals.begin(), capturedVals.end()));
}

void CodeGen::emitPropertyItDirective(std::unique_ptr<FnStmt> &s) {
    std::string desc = getDirectivePositionalArg(s->directives, "it");

    if (outline_mode_) {
        llvm::Value *descVal = cachedGlobalString(desc, ".it_desc");
        emitOutlinePrintf("it %s (@property)\n", descVal);
        return;
    }

    int64_t count = 100;
    if (const ExprNode *countExpr = getDirectiveNamedArg(s->directives, "property", "count")) {
        if (auto *n = std::get_if<NumberExpr>(&countExpr->data)) {
            if (n->value <= 0)
                codegenError("@property 'count' must be a positive integer");
            count = static_cast<int64_t>(n->value);
        } else {
            codegenError("@property 'count' must be an integer literal");
        }
    }

    std::vector<llvm::Type*> paramTypes;
    std::vector<std::string> paramNames;
    for (auto &p : s->params) {
        if (!p.type)
            codegenError("@property: parameter '" + p.name + "' must have an explicit type annotation");
        paramTypes.push_back(resolveType(p.type->toString()));
        paramNames.push_back(p.name);
    }

    stripDirectives(s->directives, {"it", "property"});
    emitStmt(s);

    auto *overloads = findFunction(s->name);
    if (!overloads || overloads->empty())
        codegenError("@property @it: internal error — fn '" + s->name + "' not found after emit");
    const auto &entry = overloads->back();
    auto capturedVals = loadCapturedArgs(entry, "@property @it");
    llvm::Value *descVal = cachedGlobalString(desc, ".it_desc");
    emitPropertyItLoop(entry.func, descVal, paramTypes, paramNames, count,
                       std::vector<llvm::Value*>(capturedVals.begin(), capturedVals.end()));
}

void CodeGen::emitDescribeDirective(std::unique_ptr<FnStmt> &s) {
    if (!test_mode_)
        codegenError("@describe is only allowed in test mode (use 'ry test')");
    if (s->is_async)
        codegenError("@describe: fn '" + s->name + "' cannot be async");
    if (!s->type_params.empty())
        codegenError("@describe: fn '" + s->name + "' cannot be generic");
    if (!s->params.empty())
        codegenError("@describe: fn '" + s->name + "' cannot have parameters");
    if (s->return_type)
        codegenError("@describe: fn '" + s->name + "' cannot have a return type annotation");

    std::string desc = getDirectivePositionalArg(s->directives, "describe");
    llvm::Value *descVal = cachedGlobalString(desc, ".describe_desc");

    if (outline_mode_) {
        emitOutlinePrintf("describe %s\n", descVal);
        ++outline_depth_;
        for (auto &stmt : s->body) {
            if (auto *fnPtr = std::get_if<std::unique_ptr<FnStmt>>(&stmt)) {
                auto &fn = *fnPtr;
                if (hasDirective(fn->directives, "it") || hasDirective(fn->directives, "describe"))
                    std::visit([this](auto &st) { emitStmt(st); }, stmt);
            }
        }
        --outline_depth_;
        return;
    }

    stripDirectives(s->directives, {"describe"});
    emitStmt(s);

    auto *overloads = findFunction(s->name);
    if (!overloads || overloads->empty())
        codegenError("@describe: internal error — fn '" + s->name + "' not found after emit");
    const auto &entry = overloads->back();
    auto capturedArgs = loadCapturedArgs(entry, "@describe");

    auto [descBeginFn, descEndFn] = getTestDescribeFunctions();
    builder_.CreateCall(descBeginFn, {descVal});
    builder_.CreateCall(entry.func, capturedArgs);
    builder_.CreateCall(descEndFn);
}

// ===== Test: mock(fn_name, replacement) =====

void CodeGen::emitMockCall(CallStmt &s) {
    if (!test_mode_)
        codegenError("'mock' is only allowed in test mode (use 'ry test')");
    if (!testing_intrinsics_imported_.count("mock"))
        codegenError(s.loc, "'mock' requires 'from testing import mock'");

    if (s.args.size() != 2)
        codegenError("mock() requires exactly 2 arguments: function name and replacement");

    // First arg is the function name (converted to StringExpr by parser)
    auto *strExpr = std::get_if<StringExpr>(&s.args[0]->data);
    if (!strExpr)
        codegenError("mock() first argument must be a function name");
    const std::string &fnName = strExpr->value;

    // Check function exists
    auto *fitOverloads = findFunction(fnName);
    if (!fitOverloads)
        codegenError("mock(): unknown function '" + fnName + "'");

    // Check no overloads (v1 limitation)
    if (fitOverloads->size() > 1)
        codegenError("mock(): overloaded functions are not supported");

    auto &entry = (*fitOverloads)[0];
    llvm::Function *origFn = entry.func;

    // Emit the replacement lambda
    llvm::Value *replacement = emitExpr(*s.args[1]);

    auto *fnInfo = lookupFnTypeInfo(replacement);
    if (!fnInfo)
        codegenError("mock(): second argument must be a lambda or function reference");

    // Verify type compatibility
    llvm::Type *origRetTy = origFn->getReturnType();
    if (fnInfo->returnType != origRetTy)
        codegenError("mock(): replacement return type does not match '" + fnName + "'");
    if (fnInfo->paramTypes.size() != entry.paramTypes.size())
        codegenError("mock(): replacement parameter count does not match '" + fnName + "'");
    for (size_t i = 0; i < entry.paramTypes.size(); ++i) {
        if (fnInfo->paramTypes[i] != entry.paramTypes[i])
            codegenError("mock(): replacement parameter type " + std::to_string(i) +
                         " does not match '" + fnName + "'");
    }

    // Track that this function is mocked (for selective dispatch in emitUserFnCall)
    mocked_functions_.insert(fnName);

    // Cache global string per function name
    auto &nameStr = mock_name_strings_[fnName];
    if (!nameStr) nameStr = cachedGlobalString(fnName, ".mock." + fnName);

    if (fnInfo->capturedVars.empty()) {
        // Non-capturing: register plain function pointer.
        llvm::FunctionType *mockSetTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*ctx_), {ptrTy_, ptrTy_}, false);
        llvm::FunctionCallee mockSetFn = mod_->getOrInsertFunction("__ry_mock_set", mockSetTy);
        builder_.CreateCall(mockSetFn, {nameStr, replacement});
        return;
    }

    // Capturing closure (#1678): register {thunk, env, env_dtor}.
    // The thunk takes (env_ptr, ...origArgs) and forwards via the captured
    // closure body. The runtime dispatch site reads env via __ry_mock_get_env
    // and routes to the matching ABI.
    llvm::Function *realFn = fnInfo->sourceFn;
    if (!realFn)
        codegenError("mock(): cannot wrap capturing closure (missing sourceFn)");
    llvm::Function *thunk = getOrCreateCapturingThunk(realFn, *fnInfo);
    auto envDtorCallee = getOrCreateClosureDestructor(*fnInfo);
    auto *nullPtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));
    llvm::Value *envDtorVal = envDtorCallee
        ? llvm::cast<llvm::Value>(envDtorCallee.getCallee())
        : llvm::cast<llvm::Value>(nullPtr);

    // Retain env so the registry owns a reference; mockReleaseClosureEnv
    // performs the matching release at clear time.
    auto *envHdr = emitArcGetHeaderFromData(replacement);
    emitArcRetain(envHdr, false);

    llvm::FunctionType *mockSetClosureTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_),
        {ptrTy_, ptrTy_, ptrTy_, ptrTy_}, false);
    llvm::FunctionCallee mockSetClosureFn =
        mod_->getOrInsertFunction("__ry_mock_set_closure", mockSetClosureTy);
    builder_.CreateCall(mockSetClosureFn, {nameStr, thunk, replacement, envDtorVal});
}

// ===== Test: verifyCalledWith(name, args...) -> int =====

llvm::Value *CodeGen::emitVerifyCalledWithCall(const CallExpr &e) {
    if (!test_mode_)
        codegenError("'verifyCalledWith' is only allowed in test mode (use 'ry test')");
    if (!testing_intrinsics_imported_.count("verifyCalledWith"))
        codegenError("'verifyCalledWith' requires 'from testing import verifyCalledWith'");

    if (e.args.empty())
        codegenError("verifyCalledWith() requires at least 1 argument: function name");

    auto *strExpr = std::get_if<StringExpr>(&e.args[0]->data);
    if (!strExpr)
        codegenError("verifyCalledWith() first argument must be a function name string literal");
    const std::string &fnName = strExpr->value;

    if (!mocked_functions_.count(fnName))
        codegenError("verifyCalledWith: '" + fnName + "' is not mocked");

    auto *overloads = findFunction(fnName);
    if (!overloads || overloads->empty())
        codegenError("verifyCalledWith: unknown function '" + fnName + "'");
    if (overloads->size() > 1)
        codegenError("verifyCalledWith: overloaded functions are not supported");
    auto &entry = (*overloads)[0];

    const size_t expectedNumArgs = e.args.size() - 1;
    if (expectedNumArgs != entry.paramTypes.size())
        codegenError("verifyCalledWith: expected " +
                     std::to_string(entry.paramTypes.size()) +
                     " argument(s) for '" + fnName + "', got " +
                     std::to_string(expectedNumArgs));

    // Cache global string for the function name.
    auto &nameStr = mock_name_strings_[fnName];
    if (!nameStr) nameStr = cachedGlobalString(fnName, ".mock." + fnName);

    // Allocate kinds[] and values[] arrays sized to expectedNumArgs.
    llvm::Value *numArgsConst = llvm::ConstantInt::get(i64Ty_, expectedNumArgs);
    llvm::ArrayType *arrTy = llvm::ArrayType::get(i64Ty_, expectedNumArgs);
    llvm::Value *kindsArr = builder_.CreateAlloca(arrTy, nullptr, "vcw_kinds");
    llvm::Value *valuesArr = builder_.CreateAlloca(arrTy, nullptr, "vcw_values");

    llvm::FunctionType *makeListSnapshotTy = llvm::FunctionType::get(
        ptrTy_, {ptrTy_, i64Ty_, i64Ty_}, false);
    llvm::FunctionCallee makeListSnapshotFn = mod_->getOrInsertFunction(
        "__ry_mock_make_list_snapshot", makeListSnapshotTy);
    llvm::FunctionCallee makeSetSnapshotFn = mod_->getOrInsertFunction(
        "__ry_mock_make_set_snapshot", makeListSnapshotTy);
    llvm::FunctionType *makeMapSnapshotTy = llvm::FunctionType::get(
        ptrTy_, {ptrTy_, i64Ty_, i64Ty_, i64Ty_, i64Ty_}, false);
    llvm::FunctionCallee makeMapSnapshotFn = mod_->getOrInsertFunction(
        "__ry_mock_make_map_snapshot", makeMapSnapshotTy);
    llvm::FunctionType *makeRecordSnapshotTy = llvm::FunctionType::get(
        ptrTy_, {ptrTy_, i64Ty_, ptrTy_, ptrTy_}, false);
    llvm::FunctionCallee makeRecordSnapshotFn = mod_->getOrInsertFunction(
        "__ry_mock_make_record_snapshot", makeRecordSnapshotTy);
    llvm::FunctionType *makeTupleSnapshotTy = llvm::FunctionType::get(
        ptrTy_, {i64Ty_, ptrTy_, ptrTy_}, false);
    llvm::FunctionCallee makeTupleSnapshotFn = mod_->getOrInsertFunction(
        "__ry_mock_make_tuple_snapshot", makeTupleSnapshotTy);
    llvm::FunctionType *makeFnSnapshotTy = llvm::FunctionType::get(
        ptrTy_, {ptrTy_, ptrTy_}, false);
    llvm::FunctionCallee makeFnSnapshotFn = mod_->getOrInsertFunction(
        "__ry_mock_make_fn_snapshot", makeFnSnapshotTy);

    auto isSupportedCollElemName = [](const std::string &n) {
        return n == "int" || n == "float" || n == "bool" || n == "str";
    };

    for (size_t i = 0; i < expectedNumArgs; ++i) {
        const std::string declaredParamName =
            i < entry.paramTypeNames.size()
                ? resolveTypeAlias(entry.paramTypeNames[i])
                : std::string{};
        const bool paramIsList = isListTypeName(declaredParamName);
        const bool paramIsSet = !paramIsList && isSetTypeName(declaredParamName);
        const bool paramIsMap = !paramIsList && !paramIsSet &&
                                isMapTypeName(declaredParamName);
        const bool paramIsRecord = !paramIsList && !paramIsSet && !paramIsMap &&
                                   record_types_.count(declaredParamName) > 0;
        const bool paramIsTuple = !paramIsList && !paramIsSet && !paramIsMap &&
                                  !paramIsRecord &&
                                  declaredParamName.size() >= 2 &&
                                  declaredParamName.front() == '(' &&
                                  declaredParamName.back() == ')';
        const bool paramIsFn = !paramIsList && !paramIsSet && !paramIsMap &&
                               !paramIsRecord && !paramIsTuple &&
                               isFunctionTypeName(declaredParamName);
        std::string paramListInner;
        std::string paramSetInner;
        std::string paramMapKeyInner;
        std::string paramMapValInner;
        std::vector<int64_t> paramRecordKinds;
        std::vector<int64_t> paramTupleKinds;
        std::vector<std::string> paramTupleElemNames;
        if (paramIsList) {
            paramListInner = resolveTypeAlias(
                declaredParamName.substr(5, declaredParamName.size() - 6));
            if (!isSupportedCollElemName(paramListInner)) {
                std::string msg = "verifyCalledWith: parameter ";
                msg += std::to_string(i);
                msg += " of '";
                msg += fnName;
                msg += "' has type '";
                msg += declaredParamName;
                msg += "'; only List<int>, List<float>, List<bool>, List<str> "
                       "are supported";
                codegenError(msg);
            }
        } else if (paramIsSet) {
            paramSetInner = resolveTypeAlias(
                declaredParamName.substr(4, declaredParamName.size() - 5));
            if (!isSupportedCollElemName(paramSetInner)) {
                std::string msg = "verifyCalledWith: parameter ";
                msg += std::to_string(i);
                msg += " of '";
                msg += fnName;
                msg += "' has type '";
                msg += declaredParamName;
                msg += "'; only Set<int>, Set<float>, Set<bool>, Set<str> "
                       "are supported";
                codegenError(msg);
            }
        } else if (paramIsMap) {
            // Strip the outer "Map<...>" then split "K, V" honouring nested
            // angle brackets via splitTypeArgs.
            auto parts = splitTypeArgs(
                declaredParamName.substr(4, declaredParamName.size() - 5));
            if (parts.size() == 2) {
                paramMapKeyInner = resolveTypeAlias(trimTypeNameSpaces(parts[0]));
                paramMapValInner = resolveTypeAlias(trimTypeNameSpaces(parts[1]));
            }
            if (!isSupportedCollElemName(paramMapKeyInner) ||
                !isSupportedCollElemName(paramMapValInner)) {
                std::string msg = "verifyCalledWith: parameter ";
                msg += std::to_string(i);
                msg += " of '";
                msg += fnName;
                msg += "' has type '";
                msg += declaredParamName;
                msg += "'; only Map<K, V> with K, V ∈ {int, float, bool, str} "
                       "are supported";
                codegenError(msg);
            }
        } else if (paramIsRecord) {
            // #1706: record params accepted iff every field is primitive or str.
            const auto &info = record_types_.at(declaredParamName);
            for (const auto &fld : info.fields) {
                std::string fldName = resolveTypeAlias(fld.type->toString());
                int64_t k = 0;
                if (fldName == "int") k = 1;
                else if (fldName == "float") k = 2;
                else if (fldName == "bool") k = 3;
                else if (fldName == "str") k = 4;
                if (k == 0) {
                    std::string msg = "verifyCalledWith: parameter ";
                    msg += std::to_string(i);
                    msg += " of '";
                    msg += fnName;
                    msg += "' has type '";
                    msg += declaredParamName;
                    msg += "' whose field '";
                    msg += fld.name;
                    msg += "' has type '";
                    msg += fldName;
                    msg += "'; only records whose fields are int, float, bool, "
                           "or str are supported";
                    codegenError(msg);
                }
                paramRecordKinds.push_back(k);
            }
        } else if (paramIsTuple) {
            // #1706: tuple params accepted iff every element is primitive or str.
            paramTupleElemNames = splitTupleSig(declaredParamName);
            if (paramTupleElemNames.empty()) {
                std::string msg = "verifyCalledWith: parameter ";
                msg += std::to_string(i);
                msg += " of '";
                msg += fnName;
                msg += "' has tuple type '";
                msg += declaredParamName;
                msg += "' that could not be parsed";
                codegenError(msg);
            }
            for (const auto &elemName : paramTupleElemNames) {
                std::string resolved = resolveTypeAlias(elemName);
                int64_t k = 0;
                if (resolved == "int") k = 1;
                else if (resolved == "float") k = 2;
                else if (resolved == "bool") k = 3;
                else if (resolved == "str") k = 4;
                if (k == 0) {
                    std::string msg = "verifyCalledWith: parameter ";
                    msg += std::to_string(i);
                    msg += " of '";
                    msg += fnName;
                    msg += "' has tuple type '";
                    msg += declaredParamName;
                    msg += "' whose element '";
                    msg += elemName;
                    msg += "' has type '";
                    msg += resolved;
                    msg += "'; only tuples whose elements are int, float, bool, "
                           "or str are supported";
                    codegenError(msg);
                }
                paramTupleKinds.push_back(k);
            }
        } else if (paramIsFn) {
            // #1707: fn-typed params accepted; identity is the {thunk_ptr, env_ptr}
            // pair extracted from the uniform closure struct.
            // #1715: signature compatibility is enforced after argVal is emitted
            // (see post-emit block below) so we can read fnInfo->paramTypeNames /
            // returnTypeName via lookupFnTypeInfo. Mismatched signatures could
            // never produce equal closure pairs at runtime, so silent no-match
            // would mask test bugs.
        } else if (!declaredParamName.empty() &&
                   declaredParamName != "int" && declaredParamName != "float" &&
                   declaredParamName != "bool" && declaredParamName != "str") {
            std::string msg = "verifyCalledWith: parameter ";
            msg += std::to_string(i);
            msg += " of '";
            msg += fnName;
            msg += "' has type '";
            msg += declaredParamName;
            msg += "'; only int, float, bool, str, fn(...) -> R, List<T>, Set<T>, "
                   "Map<K, V>, record types whose fields are primitives or str, "
                   "and tuple types whose elements are primitives or str are supported";
            codegenError(msg);
        }

        llvm::Value *argVal = emitExpr(*e.args[i + 1]);
        llvm::Type *argTy = argVal->getType();
        llvm::Type *expectedTy = entry.paramTypes[i];

        // For fn params (#1707), normalize the expected value to a uniform
        // closure (`{thunk, env, env_dtor}` ARC ptr) so {thunk, env} can be
        // extracted in the kind=11 dispatch below. Bare fn refs and source-level
        // lambda variables both flow through here; wrapFnTypedArgs only runs on
        // the recording side (codegen_call_user.cpp:263), so the verify side
        // must wrap explicitly. Skip the strict argTy/expectedTy compare —
        // expectedTy is the post-wrap ptrTy_ in the caller's signature, but
        // argVal at this point may still be a raw fn ptr (Function*) or a
        // pre-wrap closure value, depending on the source expression.
        bool fnWrapAllocated = false;
        if (paramIsFn) {
            auto *fnInfo = lookupFnTypeInfo(argVal);
            if (!fnInfo) {
                codegenError("verifyCalledWith: argument " +
                             std::to_string(i + 1) +
                             " of '" + fnName +
                             "' is fn-typed but its FnTypeInfo could not be "
                             "recovered; pass a named lambda or function "
                             "reference (e.g. `let f = (...) => ...; "
                             "verifyCalledWith(\"...\", f)`)");
            }

            // #1715: enforce exact signature match between the recorded fn
            // parameter and the verify-side value. Without this, signatures
            // that differ silently fail to match at runtime (closure pair
            // identity can never be equal across signatures), masking bugs
            // such as passing `(s: str) => s` to a `fn(int) -> int` slot.
            FnTypeInfo expectedInfo = parseFnTypeAnnotation(declaredParamName);
            std::string declaredReturnName;
            {
                size_t arrowPos = declaredParamName.find("->");
                if (arrowPos != std::string::npos) {
                    declaredReturnName = declaredParamName.substr(arrowPos + 2);
                    size_t s = declaredReturnName.find_first_not_of(' ');
                    if (s != std::string::npos)
                        declaredReturnName = declaredReturnName.substr(s);
                    size_t e = declaredReturnName.find_last_not_of(' ');
                    if (e != std::string::npos)
                        declaredReturnName.resize(e + 1);
                }
            }
            bool sigMismatch = false;
            if (expectedInfo.paramTypeNames.size() != fnInfo->paramTypeNames.size()) {
                sigMismatch = true;
            } else {
                for (size_t pi = 0; pi < expectedInfo.paramTypeNames.size(); ++pi) {
                    if (resolveTypeAlias(expectedInfo.paramTypeNames[pi]) !=
                        resolveTypeAlias(fnInfo->paramTypeNames[pi])) {
                        sigMismatch = true;
                        break;
                    }
                }
                if (!sigMismatch &&
                    resolveTypeAlias(declaredReturnName) !=
                        resolveTypeAlias(fnInfo->returnTypeName)) {
                    sigMismatch = true;
                }
            }
            if (sigMismatch) {
                std::string actualSig = "fn(";
                for (size_t pi = 0; pi < fnInfo->paramTypeNames.size(); ++pi) {
                    if (pi > 0) actualSig += ", ";
                    actualSig += fnInfo->paramTypeNames[pi];
                }
                actualSig += ") -> ";
                actualSig += fnInfo->returnTypeName;
                std::string msg = "verifyCalledWith: argument ";
                msg += std::to_string(i + 1);
                msg += " of '";
                msg += fnName;
                msg += "' is declared as ";
                msg += declaredParamName;
                msg += " but expected value has type ";
                msg += actualSig;
                codegenError(msg);
            }

            fnWrapAllocated = !fnInfo->isUniformClosure;
            argVal = wrapAsUniformClosure(argVal, *fnInfo);
            argTy = argVal->getType();
        } else if (argTy != expectedTy) {
            // Allow widening int -> float when the original parameter is float.
            if (argTy == i64Ty_ && expectedTy == f64Ty_) {
                argVal = builder_.CreateSIToFP(argVal, f64Ty_, "vcw_int2f");
                argTy = f64Ty_;
            } else {
                codegenError("verifyCalledWith: argument " + std::to_string(i + 1) +
                             " type does not match '" + fnName + "' parameter " +
                             std::to_string(i));
            }
        }

        // Explicit collection-vs-scalar consistency check: argTy != expectedTy
        // above only compares LLVM types, but List<T> / Set<T> / str / Map all
        // resolve to ptrTy_ under opaque pointers. Distinguish explicitly via
        // metadata so a List<int> parameter does not silently accept a str
        // argument (and vice versa).
        if (argTy == ptrTy_) {
            llvm::Type *argListElemTy = getListElementType(argVal);
            llvm::Type *argSetElemTy = getSetElementType(argVal);
            llvm::Type *argMapKeyTy = getMapKeyType(argVal);
            llvm::Type *argMapValTy = getMapValueType(argVal);
            const bool argIsList = (argListElemTy != nullptr);
            const bool argIsSet = (argSetElemTy != nullptr);
            const bool argIsMap = (argMapKeyTy != nullptr && argMapValTy != nullptr);
            auto otherShapeStr = [&]() -> const char * {
                if (argIsSet) return " is a Set";
                if (argIsMap) return " is a Map";
                if (argIsList) return " is a List";
                return " has an unsupported type";
            };
            if (paramIsList && !argIsList) {
                std::string msg = "verifyCalledWith: argument ";
                msg += std::to_string(i + 1);
                msg += argIsSet || argIsMap ? otherShapeStr() : " is not a List";
                msg += " but parameter ";
                msg += std::to_string(i);
                msg += " of '";
                msg += fnName;
                msg += "' has type '";
                msg += declaredParamName;
                msg += "'";
                codegenError(msg);
            }
            if (!paramIsList && !paramIsSet && !paramIsMap && argIsList) {
                std::string msg = "verifyCalledWith: argument ";
                msg += std::to_string(i + 1);
                msg += " is a List but parameter ";
                msg += std::to_string(i);
                msg += " of '";
                msg += fnName;
                msg += "' has type '";
                msg += declaredParamName;
                msg += "'";
                codegenError(msg);
            }
            if (paramIsSet && !argIsSet) {
                std::string msg = "verifyCalledWith: argument ";
                msg += std::to_string(i + 1);
                msg += argIsList || argIsMap ? otherShapeStr() : " is not a Set";
                msg += " but parameter ";
                msg += std::to_string(i);
                msg += " of '";
                msg += fnName;
                msg += "' has type '";
                msg += declaredParamName;
                msg += "'";
                codegenError(msg);
            }
            if (!paramIsSet && !paramIsList && !paramIsMap && argIsSet) {
                std::string msg = "verifyCalledWith: argument ";
                msg += std::to_string(i + 1);
                msg += " is a Set but parameter ";
                msg += std::to_string(i);
                msg += " of '";
                msg += fnName;
                msg += "' has type '";
                msg += declaredParamName;
                msg += "'";
                codegenError(msg);
            }
            if (paramIsMap && !argIsMap) {
                std::string msg = "verifyCalledWith: argument ";
                msg += std::to_string(i + 1);
                msg += argIsList || argIsSet ? otherShapeStr() : " is not a Map";
                msg += " but parameter ";
                msg += std::to_string(i);
                msg += " of '";
                msg += fnName;
                msg += "' has type '";
                msg += declaredParamName;
                msg += "'";
                codegenError(msg);
            }
            if (!paramIsSet && !paramIsList && !paramIsMap && argIsMap) {
                std::string msg = "verifyCalledWith: argument ";
                msg += std::to_string(i + 1);
                msg += " is a Map but parameter ";
                msg += std::to_string(i);
                msg += " of '";
                msg += fnName;
                msg += "' has type '";
                msg += declaredParamName;
                msg += "'";
                codegenError(msg);
            }
            if (paramIsList && argIsList) {
                const auto *meta = getMeta(argVal);
                std::string argElemName =
                    meta ? meta->list_elem_type_name : std::string();
                if (argElemName.empty()) {
                    if (argListElemTy == i64Ty_) argElemName = "int";
                    else if (argListElemTy == f64Ty_) argElemName = "float";
                    else if (argListElemTy == i1Ty_ || argListElemTy == i8Ty_)
                        argElemName = "bool";
                    else if (argListElemTy == ptrTy_) argElemName = "str";
                }
                std::string argElemResolved = resolveTypeAlias(argElemName);
                if (argElemResolved != paramListInner) {
                    std::string msg = "verifyCalledWith: argument ";
                    msg += std::to_string(i + 1);
                    msg += " is List<";
                    msg += argElemName;
                    msg += "> but parameter ";
                    msg += std::to_string(i);
                    msg += " of '";
                    msg += fnName;
                    msg += "' has type '";
                    msg += declaredParamName;
                    msg += "'";
                    codegenError(msg);
                }
            }
            if (paramIsSet && argIsSet) {
                const auto *meta = getMeta(argVal);
                std::string argElemName =
                    meta ? meta->set_elem_type_name : std::string();
                if (argElemName.empty()) {
                    if (argSetElemTy == i64Ty_) argElemName = "int";
                    else if (argSetElemTy == f64Ty_) argElemName = "float";
                    else if (argSetElemTy == i1Ty_ || argSetElemTy == i8Ty_)
                        argElemName = "bool";
                    // Mirror the record-side gate (codegen_call_user.cpp):
                    // do not infer str from a bare ptr element type so
                    // unsupported pointer-backed sets surface as a clear
                    // "Set<>" mismatch instead of being miscompared as
                    // Set<str>. emitSetLiteral stamps set_elem_type_name
                    // = "str" via anyElemIsStrLike for plain literals.
                }
                std::string argElemResolved = resolveTypeAlias(argElemName);
                if (argElemResolved != paramSetInner) {
                    std::string msg = "verifyCalledWith: argument ";
                    msg += std::to_string(i + 1);
                    msg += " is Set<";
                    msg += argElemName;
                    msg += "> but parameter ";
                    msg += std::to_string(i);
                    msg += " of '";
                    msg += fnName;
                    msg += "' has type '";
                    msg += declaredParamName;
                    msg += "'";
                    codegenError(msg);
                }
            }
            if (paramIsMap && argIsMap) {
                const auto *meta = getMeta(argVal);
                auto resolveCollName = [&](llvm::Type *ty,
                                            const std::string &stamped) {
                    if (!stamped.empty()) return stamped;
                    if (ty == i64Ty_) return std::string("int");
                    if (ty == f64Ty_) return std::string("float");
                    if (ty == i1Ty_ || ty == i8Ty_) return std::string("bool");
                    // Mirror the Set guard: never infer str from bare ptr.
                    return std::string{};
                };
                std::string argKeyName = resolveCollName(
                    argMapKeyTy, meta ? meta->map_key_type_name : std::string());
                std::string argValName = resolveCollName(
                    argMapValTy, meta ? meta->map_value_type_name : std::string());
                std::string argKeyResolved = resolveTypeAlias(argKeyName);
                std::string argValResolved = resolveTypeAlias(argValName);
                if (argKeyResolved != paramMapKeyInner ||
                    argValResolved != paramMapValInner) {
                    std::string msg = "verifyCalledWith: argument ";
                    msg += std::to_string(i + 1);
                    msg += " is Map<";
                    msg += argKeyName;
                    msg += ", ";
                    msg += argValName;
                    msg += "> but parameter ";
                    msg += std::to_string(i);
                    msg += " of '";
                    msg += fnName;
                    msg += "' has type '";
                    msg += declaredParamName;
                    msg += "'";
                    codegenError(msg);
                }
            }
        } else if (auto *argSt = llvm::dyn_cast<llvm::StructType>(argTy)) {
            // #1706: struct arg + record/tuple param. Verify shape match — both
            // sides must agree on declared identity (named record vs anonymous
            // tuple) and arity. The earlier `argTy != expectedTy` check already
            // catches LLVM-type mismatches between struct args and record/tuple
            // params, but cross-shape detection (e.g. record arg + tuple param,
            // or two structurally-identical records with different names)
            // requires explicit identity comparison.
            if (paramIsRecord) {
                if (!argSt->hasName() ||
                    argSt->getName().str() != declaredParamName) {
                    std::string argName =
                        argSt->hasName() ? argSt->getName().str()
                                          : std::string("<anonymous>");
                    std::string msg = "verifyCalledWith: argument ";
                    msg += std::to_string(i + 1);
                    msg += " is record '";
                    msg += argName;
                    msg += "' but parameter ";
                    msg += std::to_string(i);
                    msg += " of '";
                    msg += fnName;
                    msg += "' has type '";
                    msg += declaredParamName;
                    msg += "'";
                    codegenError(msg);
                }
            } else if (paramIsTuple) {
                if (!isTupleStructType(argSt) ||
                    argSt->getNumElements() != paramTupleKinds.size()) {
                    std::string msg = "verifyCalledWith: argument ";
                    msg += std::to_string(i + 1);
                    msg += " has shape mismatch for tuple parameter ";
                    msg += std::to_string(i);
                    msg += " of '";
                    msg += fnName;
                    msg += "': expected '";
                    msg += declaredParamName;
                    msg += "'";
                    codegenError(msg);
                }
                // #1706: per-element type-signature check. Anonymous tuple
                // StructType collapses str / List / Map / closure / record /
                // nested tuple all to ptr, so arity alone is not enough —
                // `(int, [1, 2])` would otherwise satisfy a `(int, str)` param
                // and the kind=10 snapshot would copy a List header where the
                // verifier expects a str (memcmp byte_len at -8 reads the
                // List size_t and treats it as the string byte_len). Read the
                // lossless source_type_name metadata stamped by
                // emitExprVariant(TupleExpr) and reject any mismatch.
                const auto *argMeta = getMeta(argVal);
                std::string argTupleSig =
                    argMeta ? argMeta->source_type_name : std::string();
                std::vector<std::string> argElemNames;
                if (!argTupleSig.empty())
                    argElemNames = splitTupleSig(argTupleSig);
                if (argElemNames.size() != paramTupleElemNames.size()) {
                    std::string msg = "verifyCalledWith: argument ";
                    msg += std::to_string(i + 1);
                    msg += " is a tuple whose element types could not be "
                           "recovered from metadata; only literal tuple "
                           "expressions whose elements are int, float, bool, "
                           "or str are supported as arguments to '";
                    msg += fnName;
                    msg += "' parameter ";
                    msg += std::to_string(i);
                    msg += " of type '";
                    msg += declaredParamName;
                    msg += "'";
                    codegenError(msg);
                }
                for (size_t k = 0; k < paramTupleElemNames.size(); ++k) {
                    std::string argElemResolved =
                        resolveTypeAlias(argElemNames[k]);
                    std::string paramElemResolved =
                        resolveTypeAlias(paramTupleElemNames[k]);
                    if (argElemResolved != paramElemResolved) {
                        std::string msg = "verifyCalledWith: argument ";
                        msg += std::to_string(i + 1);
                        msg += " element ";
                        msg += std::to_string(k);
                        msg += " has type '";
                        msg += argElemNames[k];
                        msg += "' but parameter ";
                        msg += std::to_string(i);
                        msg += " of '";
                        msg += fnName;
                        msg += "' has type '";
                        msg += declaredParamName;
                        msg += "' (element '";
                        msg += paramTupleElemNames[k];
                        msg += "')";
                        codegenError(msg);
                    }
                }
            } else {
                std::string msg = "verifyCalledWith: argument ";
                msg += std::to_string(i + 1);
                msg += " has struct type but parameter ";
                msg += std::to_string(i);
                msg += " of '";
                msg += fnName;
                msg += "' has type '";
                msg += declaredParamName;
                msg += "'";
                codegenError(msg);
            }
        }

        int64_t kind = 0;
        llvm::Value *valI64 = nullptr;
        if (paramIsList && argTy == ptrTy_ && getListElementType(argVal)) {
            int64_t elemKind = 0;
            if (paramListInner == "int") elemKind = 1;
            else if (paramListInner == "float") elemKind = 2;
            else if (paramListInner == "bool") elemKind = 3;
            else if (paramListInner == "str") elemKind = 4;
            llvm::Type *elemTy = getListElementType(argVal);
            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t elemSize = dl.getTypeAllocSize(elemTy);
            llvm::Value *snap = builder_.CreateCall(
                makeListSnapshotFn,
                {argVal,
                 llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(elemKind), true),
                 llvm::ConstantInt::get(i64Ty_, elemSize, false)},
                "vcw_list_snap");
            kind = 6;
            valI64 = builder_.CreatePtrToInt(snap, i64Ty_, "vcw_snap2i");
        } else if (paramIsSet && argTy == ptrTy_ && getSetElementType(argVal)) {
            int64_t elemKind = 0;
            if (paramSetInner == "int") elemKind = 1;
            else if (paramSetInner == "float") elemKind = 2;
            else if (paramSetInner == "bool") elemKind = 3;
            else if (paramSetInner == "str") elemKind = 4;
            llvm::Type *elemTy = getSetElementType(argVal);
            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t elemSize = dl.getTypeAllocSize(elemTy);
            llvm::Value *snap = builder_.CreateCall(
                makeSetSnapshotFn,
                {argVal,
                 llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(elemKind), true),
                 llvm::ConstantInt::get(i64Ty_, elemSize, false)},
                "vcw_set_snap");
            kind = 7;
            valI64 = builder_.CreatePtrToInt(snap, i64Ty_, "vcw_snap2i");
        } else if (paramIsMap && argTy == ptrTy_ &&
                   getMapKeyType(argVal) && getMapValueType(argVal)) {
            auto kindOf = [](const std::string &n) -> int64_t {
                if (n == "int") return 1;
                if (n == "float") return 2;
                if (n == "bool") return 3;
                if (n == "str") return 4;
                return 0;
            };
            int64_t keyKind = kindOf(paramMapKeyInner);
            int64_t valKind = kindOf(paramMapValInner);
            llvm::Type *keyTy = getMapKeyType(argVal);
            llvm::Type *valTy = getMapValueType(argVal);
            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t keySize = dl.getTypeAllocSize(keyTy);
            uint64_t valSize = dl.getTypeAllocSize(valTy);
            llvm::Value *snap = builder_.CreateCall(
                makeMapSnapshotFn,
                {argVal,
                 llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(keyKind), true),
                 llvm::ConstantInt::get(i64Ty_, keySize, false),
                 llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(valKind), true),
                 llvm::ConstantInt::get(i64Ty_, valSize, false)},
                "vcw_map_snap");
            kind = 8;
            valI64 = builder_.CreatePtrToInt(snap, i64Ty_, "vcw_snap2i");
        } else if (paramIsRecord && llvm::isa<llvm::StructType>(argTy)) {
            // #1706: build per-slot kinds[] and values[] alloca buffers, then
            // call __ry_mock_make_record_snapshot. paramRecordKinds was populated
            // in Stage 1 (every kind ∈ {1..4}).
            const int64_t fieldCount =
                static_cast<int64_t>(paramRecordKinds.size());
            llvm::Value *kindsAlloca = builder_.CreateAlloca(
                i8Ty_,
                llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(fieldCount)),
                "vcw_rec_kinds");
            llvm::Value *valsAlloca = builder_.CreateAlloca(
                i64Ty_,
                llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(fieldCount)),
                "vcw_rec_vals");
            for (size_t fi = 0; fi < paramRecordKinds.size(); ++fi) {
                int64_t k = paramRecordKinds[fi];
                llvm::Value *fieldVal = builder_.CreateExtractValue(
                    argVal, {static_cast<unsigned>(fi)}, "vcw_rec_fld");
                llvm::Value *fieldI64;
                if (k == 1) fieldI64 = fieldVal;
                else if (k == 2)
                    fieldI64 = builder_.CreateBitCast(
                        fieldVal, i64Ty_, "vcw_rec_f2i");
                else if (k == 3)
                    fieldI64 = builder_.CreateZExt(
                        fieldVal, i64Ty_, "vcw_rec_b2i");
                else // k == 4
                    fieldI64 = builder_.CreatePtrToInt(
                        fieldVal, i64Ty_, "vcw_rec_p2i");
                llvm::Value *kindGEP = builder_.CreateGEP(
                    i8Ty_, kindsAlloca,
                    llvm::ConstantInt::get(
                        i64Ty_, static_cast<uint64_t>(fi)));
                builder_.CreateStore(
                    llvm::ConstantInt::get(
                        i8Ty_, static_cast<uint64_t>(k), true),
                    kindGEP);
                llvm::Value *valGEP = builder_.CreateGEP(
                    i64Ty_, valsAlloca,
                    llvm::ConstantInt::get(
                        i64Ty_, static_cast<uint64_t>(fi)));
                builder_.CreateStore(fieldI64, valGEP);
            }
            llvm::Constant *typeNameStr = cachedGlobalString(
                declaredParamName,
                llvm::Twine(".mock.record_name.") + declaredParamName);
            llvm::Value *snap = builder_.CreateCall(
                makeRecordSnapshotFn,
                {typeNameStr,
                 llvm::ConstantInt::get(
                     i64Ty_, static_cast<uint64_t>(fieldCount), true),
                 kindsAlloca, valsAlloca},
                "vcw_rec_snap");
            kind = 9;
            valI64 = builder_.CreatePtrToInt(snap, i64Ty_, "vcw_snap2i");
        } else if (paramIsTuple && llvm::isa<llvm::StructType>(argTy)) {
            // #1706: same per-slot pattern as record, but no type_name and the
            // shape is gated by arity + per-element kind alone. paramTupleKinds
            // populated in Stage 1.
            const int64_t arity =
                static_cast<int64_t>(paramTupleKinds.size());
            llvm::Value *kindsAlloca = builder_.CreateAlloca(
                i8Ty_,
                llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(arity)),
                "vcw_tup_kinds");
            llvm::Value *valsAlloca = builder_.CreateAlloca(
                i64Ty_,
                llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(arity)),
                "vcw_tup_vals");
            for (size_t ei = 0; ei < paramTupleKinds.size(); ++ei) {
                int64_t k = paramTupleKinds[ei];
                llvm::Value *elemVal = builder_.CreateExtractValue(
                    argVal, {static_cast<unsigned>(ei)}, "vcw_tup_elem");
                llvm::Value *elemI64;
                if (k == 1) elemI64 = elemVal;
                else if (k == 2)
                    elemI64 = builder_.CreateBitCast(
                        elemVal, i64Ty_, "vcw_tup_f2i");
                else if (k == 3)
                    elemI64 = builder_.CreateZExt(
                        elemVal, i64Ty_, "vcw_tup_b2i");
                else // k == 4
                    elemI64 = builder_.CreatePtrToInt(
                        elemVal, i64Ty_, "vcw_tup_p2i");
                llvm::Value *kindGEP = builder_.CreateGEP(
                    i8Ty_, kindsAlloca,
                    llvm::ConstantInt::get(
                        i64Ty_, static_cast<uint64_t>(ei)));
                builder_.CreateStore(
                    llvm::ConstantInt::get(
                        i8Ty_, static_cast<uint64_t>(k), true),
                    kindGEP);
                llvm::Value *valGEP = builder_.CreateGEP(
                    i64Ty_, valsAlloca,
                    llvm::ConstantInt::get(
                        i64Ty_, static_cast<uint64_t>(ei)));
                builder_.CreateStore(elemI64, valGEP);
            }
            llvm::Value *snap = builder_.CreateCall(
                makeTupleSnapshotFn,
                {llvm::ConstantInt::get(
                     i64Ty_, static_cast<uint64_t>(arity), true),
                 kindsAlloca, valsAlloca},
                "vcw_tup_snap");
            kind = 10;
            valI64 = builder_.CreatePtrToInt(snap, i64Ty_, "vcw_snap2i");
        } else if (paramIsFn) {
            // #1707: extract {thunk_ptr, env_ptr} from the uniform closure
            // struct; argVal was normalized to a uniform closure pointer above
            // via lookupFnTypeInfo + wrapAsUniformClosure. The env_dtor slot
            // is uniquely determined by thunk and is omitted from the snapshot.
            auto *ucTy = getUniformClosureTy();
            llvm::Value *thunkField = builder_.CreateStructGEP(
                ucTy, argVal, 0, "vcw_fn_thunk_gep");
            llvm::Value *envField = builder_.CreateStructGEP(
                ucTy, argVal, 1, "vcw_fn_env_gep");
            llvm::Value *thunkPtr = builder_.CreateLoad(
                ptrTy_, thunkField, "vcw_fn_thunk");
            llvm::Value *envPtr = builder_.CreateLoad(
                ptrTy_, envField, "vcw_fn_env");
            llvm::Value *snap = builder_.CreateCall(
                makeFnSnapshotFn, {thunkPtr, envPtr}, "vcw_fn_snap");
            kind = 11;
            valI64 = builder_.CreatePtrToInt(snap, i64Ty_, "vcw_snap2i");
            // Release the wrap struct allocated above by wrapAsUniformClosure.
            // wrapAsUniformClosure passes through unchanged when input was
            // already a uniform closure (no allocation), but for non-uniform
            // inputs it allocates a fresh ARC struct and retains the captured
            // env. The default scope-end ARC release uses the generic dtor,
            // which doesn't know to release env — so we must explicitly
            // release with the uniform-closure dtor. The snapshot stores raw
            // {thunk, env} pointers per the issue's pointer-equality contract;
            // the caller scope keeps the underlying closure alive for the
            // duration of the test block.
            if (fnWrapAllocated) {
                releaseUniformClosureTemps({argVal});
            }
        } else if (argTy == i64Ty_) {
            kind = 1;
            valI64 = argVal;
        } else if (argTy == f64Ty_) {
            kind = 2;
            valI64 = builder_.CreateBitCast(argVal, i64Ty_, "vcw_f2i");
        } else if (argTy == i1Ty_) {
            kind = 3;
            valI64 = builder_.CreateZExt(argVal, i64Ty_, "vcw_b2i");
        } else if (argTy == ptrTy_ && isStringValue(argVal)) {
            kind = 4;
            valI64 = builder_.CreatePtrToInt(argVal, i64Ty_, "vcw_p2i");
        } else {
            codegenError("verifyCalledWith: argument " + std::to_string(i + 1) +
                         " has unsupported type; only int, float, bool, str, "
                         "List<T>, Set<T>, Map<K, V>, record types whose fields "
                         "are primitives or str, and tuple types whose elements "
                         "are primitives or str are supported");
        }

        llvm::Value *kindGEP = builder_.CreateInBoundsGEP(
            arrTy, kindsArr,
            {llvm::ConstantInt::get(i64Ty_, 0), llvm::ConstantInt::get(i64Ty_, i)},
            "vcw_kind_gep");
        llvm::Value *valGEP = builder_.CreateInBoundsGEP(
            arrTy, valuesArr,
            {llvm::ConstantInt::get(i64Ty_, 0), llvm::ConstantInt::get(i64Ty_, i)},
            "vcw_val_gep");
        builder_.CreateStore(
            llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(kind), true),
            kindGEP);
        builder_.CreateStore(valI64, valGEP);
    }

    llvm::FunctionType *countFnTy = llvm::FunctionType::get(
        i64Ty_, {ptrTy_, i64Ty_, ptrTy_, ptrTy_}, false);
    llvm::FunctionCallee countFn = mod_->getOrInsertFunction(
        "__ry_mock_count_matching_calls", countFnTy);
    return builder_.CreateCall(countFn, {nameStr, numArgsConst, kindsArr, valuesArr},
                                "vcw_count");
}

// ===== Test: ExpectStmt =====

void CodeGen::emitStmt(ExpectStmt &s) {
    emitCoverage(s.loc);
    if (!test_mode_)
        codegenError("'expect' is only allowed in test mode (use 'ry test')");
    if (!testing_intrinsics_imported_.count("expect"))
        codegenError(s.loc, "'expect' requires 'from testing import expect'");

    llvm::Value *actualVal = emitExpr(*s.actual);
    llvm::Type *actualTy = actualVal->getType();

    llvm::Value *cmpResult = nullptr;
    // Save expectedVal from comparison section to reuse in failure message
    llvm::Value *savedExpectedVal = nullptr;
    // For toBeCloseTo: capture decimals so the failure formatter can print it.
    int64_t savedDecimals = 0;
    bool savedDecimalsValid = false;

    if (s.matcher == "toEq" || s.matcher == "toNotEq") {
        llvm::Value *expectedVal = emitExpr(*s.expected);
        savedExpectedVal = expectedVal;

        // Delegate equality to the same path used by the == operator (#737).
        // int/float/bool use direct IR comparisons; str uses isStringValue() to
        // distinguish string pointers from collection pointers (both are ptrTy_).
        // Complex types (Option/Result/Record/Tuple/ADT enum/union/List/Set/Map)
        // are delegated to emitComparisonOp which handles them via SSA-value metadata.
        llvm::Value *eqResult = nullptr;
        llvm::Type *expectedTy = expectedVal->getType();
        if (actualTy == i64Ty_ && expectedTy == i64Ty_) { // NOLINT(bugprone-branch-clone)
            eqResult = builder_.CreateICmpEQ(actualVal, expectedVal, "eq");
        } else if (actualTy == f64Ty_ && expectedTy == f64Ty_) {
            eqResult = builder_.CreateFCmpOEQ(actualVal, expectedVal, "eq");
        } else if (actualTy == i1Ty_ && expectedTy == i1Ty_) {
            eqResult = builder_.CreateICmpEQ(actualVal, expectedVal, "eq");
        } else if (isStringValue(actualVal) && isStringValue(expectedVal)) {
            auto strcmpFn = getStdlibStrcmp();
            llvm::Value *result = builder_.CreateCall(strcmpFn, {actualVal, expectedVal}, "strcmp");
            eqResult = builder_.CreateICmpEQ(result, llvm::ConstantInt::get(i32Ty_, 0), "eq");
        } else if ((actualTy == i64Ty_ && expectedTy == f64Ty_) ||
                   (actualTy == f64Ty_ && expectedTy == i64Ty_)) {
            auto [lf, rf] = promoteToFloat(actualVal, expectedVal);
            eqResult = builder_.CreateFCmpOEQ(lf, rf, "eq");
        } else if (isAnyType(actualTy) || isAnyType(expectedTy)) {
            if (!isAnyType(actualTy)) actualVal = wrapInAny(actualVal);
            if (!isAnyType(expectedTy)) expectedVal = wrapInAny(expectedVal);
            eqResult = emitAnyBinaryOp("==", actualVal, expectedVal);
        } else {
            // Option, Result, Record, Tuple, ADT enum, union, List, Set, Map.
            // Pass low-level suffix hints so checkLowLevelTypeMix works correctly.
            std::string actualHint   = getExprLowLevelSuffix(*s.actual);
            std::string expectedHint = getExprLowLevelSuffix(*s.expected);
            eqResult = emitComparisonOp("==", actualVal, expectedVal, actualHint, expectedHint);
        }
        cmpResult = (s.matcher == "toNotEq")
            ? builder_.CreateNot(eqResult, "not_eq")
            : eqResult;
    } else if (s.matcher == "toBeTrue") {
        if (actualTy != i1Ty_)
            codegenError("line " + std::to_string(s.loc.line) +
                                     ": toBeTrue: expected bool");
        cmpResult = actualVal;
    } else if (s.matcher == "toBeFalse") {
        if (actualTy != i1Ty_)
            codegenError("line " + std::to_string(s.loc.line) +
                                     ": toBeFalse: expected bool");
        cmpResult = builder_.CreateNot(actualVal, "not");
    } else if (s.matcher == "toBeNone") {
        if (!isOptionType(actualTy))
            codegenError("line " + std::to_string(s.loc.line) +
                                     ": toBeNone: expected Option type");
        llvm::Value *hasVal = builder_.CreateExtractValue(actualVal, {0}, "has_val");
        cmpResult = builder_.CreateNot(hasVal, "is_none");
    } else if (s.matcher == "toBeSome") {
        if (!isOptionType(actualTy))
            codegenError("line " + std::to_string(s.loc.line) +
                                     ": toBeSome: expected Option type");
        cmpResult = builder_.CreateExtractValue(actualVal, {0}, "is_some");
    } else if (s.matcher == "toBeOk") {
        if (!isResultType(actualTy))
            codegenError("line " + std::to_string(s.loc.line) +
                                     ": toBeOk: expected Result type");
        cmpResult = builder_.CreateExtractValue(actualVal, {0}, "is_ok");
    } else if (s.matcher == "toBeErr") {
        if (!isResultType(actualTy))
            codegenError("line " + std::to_string(s.loc.line) +
                                     ": toBeErr: expected Result type");
        llvm::Value *isOk = builder_.CreateExtractValue(actualVal, {0}, "is_ok");
        cmpResult = builder_.CreateNot(isOk, "is_err");
    } else if (s.matcher == "toContain" || s.matcher == "toNotContain") {
        llvm::Value *expectedVal = emitExpr(*s.expected);
        savedExpectedVal = expectedVal;

        llvm::Value *containResult = nullptr;
        // Check for collection types first (Map/List/Set), then fall back to string
        llvm::Type *mapKeyTy = (actualTy == ptrTy_) ? getMapKeyType(actualVal) : nullptr;
        llvm::Type *listElemTy = (actualTy == ptrTy_ && !mapKeyTy) ? getListElementType(actualVal) : nullptr;
        llvm::Type *setElemTy = (actualTy == ptrTy_ && !mapKeyTy && !listElemTy) ? getSetElementType(actualVal) : nullptr;

        if (mapKeyTy) {
            // Map key containment
            if (expectedVal->getType() != mapKeyTy)
                codegenError("line " + std::to_string(s.loc.line) +
                                         ": " + s.matcher + ": key type mismatch");
            llvm::Value *idx = emitMapKeyLookup(actualVal, expectedVal, mapKeyTy);
            containResult = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "map_contains");
        } else if (listElemTy || setElemTy) {
            llvm::Type *elemTy = listElemTy ? listElemTy : setElemTy;
            llvm::StructType *headerTy = listElemTy ? listHeaderTy_ : setHeaderTy_;

            llvm::Value *lenPtr = builder_.CreateStructGEP(headerTy, actualVal, 0, "len_ptr");
            llvm::Value *len = builder_.CreateLoad(i64Ty_, lenPtr, "len");
            llvm::Value *dataField = builder_.CreateStructGEP(headerTy, actualVal, 2, "data_field");
            llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataField, "data_ptr");

            llvm::AllocaInst *foundVar = builder_.CreateAlloca(i1Ty_, nullptr, "found");
            builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 0), foundVar);
            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

            llvm::Function *currentFnContain = builder_.GetInsertBlock()->getParent();
            llvm::BasicBlock *cBB = llvm::BasicBlock::Create(*ctx_, "contain.cond", currentFnContain);
            llvm::BasicBlock *bBB = llvm::BasicBlock::Create(*ctx_, "contain.body", currentFnContain);
            llvm::BasicBlock *nBB = llvm::BasicBlock::Create(*ctx_, "contain.next", currentFnContain);
            llvm::BasicBlock *eBB = llvm::BasicBlock::Create(*ctx_, "contain.end", currentFnContain);

            builder_.CreateBr(cBB);
            builder_.SetInsertPoint(cBB);
            llvm::Value *ci = builder_.CreateLoad(i64Ty_, iVar, "ci");
            builder_.CreateCondBr(builder_.CreateICmpSLT(ci, len, "clt"), bBB, eBB);

            builder_.SetInsertPoint(bBB);
            llvm::Value *curI = builder_.CreateLoad(i64Ty_, iVar, "cur_i");
            llvm::Value *ePtr = builder_.CreateGEP(elemTy, dataPtr, {curI}, "elem_ptr");
            llvm::Value *elem = builder_.CreateLoad(elemTy, ePtr, "elem");
            if (expectedVal->getType() != elemTy)
                codegenError("line " + std::to_string(s.loc.line) +
                                         ": " + s.matcher + ": element type mismatch");
            llvm::Value *eq;
            if (elemTy == i64Ty_)
                eq = builder_.CreateICmpEQ(elem, expectedVal, "eq"); // NOLINT(bugprone-branch-clone)
            else if (elemTy == ptrTy_) {
                auto strcmpFn = getStdlibStrcmp();
                llvm::Value *cmp = builder_.CreateCall(strcmpFn, {elem, expectedVal}, "strcmp");
                eq = builder_.CreateICmpEQ(cmp, llvm::ConstantInt::get(i32Ty_, 0), "eq");
            } else
                eq = builder_.CreateICmpEQ(elem, expectedVal, "eq");

            llvm::BasicBlock *foundBB = llvm::BasicBlock::Create(*ctx_, "contain.found", currentFnContain);
            builder_.CreateCondBr(eq, foundBB, nBB);
            builder_.SetInsertPoint(foundBB);
            builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 1), foundVar);
            builder_.CreateBr(eBB);

            builder_.SetInsertPoint(nBB);
            llvm::Value *nextI = builder_.CreateAdd(
                builder_.CreateLoad(i64Ty_, iVar, "ni"), llvm::ConstantInt::get(i64Ty_, 1), "next_i");
            builder_.CreateStore(nextI, iVar);
            builder_.CreateBr(cBB);

            builder_.SetInsertPoint(eBB);
            containResult = builder_.CreateLoad(i1Ty_, foundVar, "contain_result");
        } else if (actualTy == ptrTy_ && expectedVal->getType() == ptrTy_) {
            // String contains: use strstr
            auto strstrFn = getStdlibStrstr();
            llvm::Value *result = builder_.CreateCall(strstrFn, {actualVal, expectedVal}, "strstr");
            containResult = builder_.CreateICmpNE(result, llvm::ConstantPointerNull::get(
                llvm::PointerType::getUnqual(*ctx_)), "contains");
        } else {
            codegenError("line " + std::to_string(s.loc.line) +
                                     ": " + s.matcher + ": expected list, set, map, or string");
        }
        cmpResult = (s.matcher == "toNotContain")
            ? builder_.CreateNot(containResult, "not_contain")
            : containResult;
    } else if (s.matcher == "toBeGreaterThan" || s.matcher == "toBeLessThan" ||
               s.matcher == "toBeGreaterThanOrEq" || s.matcher == "toBeLessThanOrEq") {
        llvm::Value *expectedVal = emitExpr(*s.expected);
        savedExpectedVal = expectedVal;
        llvm::Type *expectedTy = expectedVal->getType();

        // Map matcher name to ICmp/FCmp predicates
        llvm::CmpInst::Predicate iPred, fPred;
        if (s.matcher == "toBeGreaterThan") {
            iPred = llvm::CmpInst::ICMP_SGT; fPred = llvm::CmpInst::FCMP_OGT;
        } else if (s.matcher == "toBeLessThan") {
            iPred = llvm::CmpInst::ICMP_SLT; fPred = llvm::CmpInst::FCMP_OLT;
        } else if (s.matcher == "toBeGreaterThanOrEq") {
            iPred = llvm::CmpInst::ICMP_SGE; fPred = llvm::CmpInst::FCMP_OGE;
        } else {
            iPred = llvm::CmpInst::ICMP_SLE; fPred = llvm::CmpInst::FCMP_OLE;
        }

        if ((actualTy == i64Ty_ || actualTy == f64Ty_) &&
            (expectedTy == i64Ty_ || expectedTy == f64Ty_)) {
            if (actualTy == i64Ty_ && expectedTy == i64Ty_) {
                cmpResult = builder_.CreateICmp(iPred, actualVal, expectedVal, "cmp");
            } else {
                auto [lf, rf] = promoteToFloat(actualVal, expectedVal);
                cmpResult = builder_.CreateFCmp(fPred, lf, rf, "cmp");
            }
        } else {
            codegenError("line " + std::to_string(s.loc.line) +
                ": " + s.matcher + ": requires int or float operands");
        }
    } else if (s.matcher == "toHaveLen" || s.matcher == "toBeEmpty") {
        if (actualTy != ptrTy_)
            codegenError("line " + std::to_string(s.loc.line) +
                ": " + s.matcher + ": expected list, set, map, or string");

        llvm::Value *len = nullptr;
        if (getSetElementType(actualVal)) {
            len = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(setHeaderTy_, actualVal, 0), "set_len");
        } else if (getMapKeyType(actualVal)) {
            len = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(mapHeaderTy_, actualVal, 0), "map_len");
        } else if (getListElementType(actualVal)) {
            len = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(listHeaderTy_, actualVal, 0), "list_len");
        } else {
            // NUL-safe string character count via __ry_utf8_len_n
            llvm::Value *byteLen = emitStringByteLen(actualVal);
            auto utf8LenTy = llvm::FunctionType::get(i64Ty_, {ptrTy_, i64Ty_}, false);
            auto utf8LenFn = mod_->getOrInsertFunction("__ry_utf8_len_n", utf8LenTy);
            len = builder_.CreateCall(utf8LenFn, {actualVal, byteLen}, "str_len");
        }

        if (s.matcher == "toHaveLen") {
            llvm::Value *expectedVal = emitExpr(*s.expected);
            savedExpectedVal = expectedVal;
            if (expectedVal->getType() != i64Ty_)
                codegenError("line " + std::to_string(s.loc.line) +
                    ": toHaveLen: expected int argument");
            cmpResult = builder_.CreateICmpEQ(len, expectedVal, "has_length");
        } else {
            cmpResult = builder_.CreateICmpEQ(len, llvm::ConstantInt::get(i64Ty_, 0), "isEmpty");
        }
    } else if (s.matcher == "toStartWith") {
        llvm::Value *expectedVal = emitExpr(*s.expected);
        savedExpectedVal = expectedVal;
        if (actualTy != ptrTy_ || expectedVal->getType() != ptrTy_)
            codegenError("line " + std::to_string(s.loc.line) +
                ": toStartWith: requires str operands");
        auto strlenFn = getStdlibStrlen();
        auto strncmpFn = getStdlibStrncmp();
        llvm::Value *prefixLen = builder_.CreateCall(strlenFn, {expectedVal}, "prefix_len");
        llvm::Value *cmp = builder_.CreateCall(strncmpFn, {actualVal, expectedVal, prefixLen}, "strncmp");
        cmpResult = builder_.CreateICmpEQ(cmp, llvm::ConstantInt::get(i32Ty_, 0), "starts_with");
    } else if (s.matcher == "toEndWith") {
        llvm::Value *expectedVal = emitExpr(*s.expected);
        savedExpectedVal = expectedVal;
        if (actualTy != ptrTy_ || expectedVal->getType() != ptrTy_)
            codegenError("line " + std::to_string(s.loc.line) +
                ": toEndWith: requires str operands");
        auto strlenFn = getStdlibStrlen();
        auto strncmpFn = getStdlibStrncmp();
        llvm::Value *sLen = builder_.CreateCall(strlenFn, {actualVal}, "s_len");
        llvm::Value *suffixLen = builder_.CreateCall(strlenFn, {expectedVal}, "suffix_len");

        llvm::Value *tooLong = builder_.CreateICmpUGT(suffixLen, sLen, "too_long");

        llvm::Function *curFnEW = builder_.GetInsertBlock()->getParent();
        llvm::BasicBlock *checkBB = llvm::BasicBlock::Create(*ctx_, "ew.check", curFnEW);
        llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "ew.merge", curFnEW);
        llvm::BasicBlock *curBB = builder_.GetInsertBlock();

        builder_.CreateCondBr(tooLong, mergeBB, checkBB);

        builder_.SetInsertPoint(checkBB);
        llvm::Value *offset = builder_.CreateSub(sLen, suffixLen, "offset");
        llvm::Value *tailPtr = builder_.CreateGEP(builder_.getInt8Ty(), actualVal, offset, "tail_ptr");
        llvm::Value *cmp = builder_.CreateCall(strncmpFn, {tailPtr, expectedVal, suffixLen}, "strncmp");
        llvm::Value *match = builder_.CreateICmpEQ(cmp, llvm::ConstantInt::get(i32Ty_, 0), "match");
        builder_.CreateBr(mergeBB);

        builder_.SetInsertPoint(mergeBB);
        llvm::PHINode *phi = builder_.CreatePHI(i1Ty_, 2, "ends_with");
        phi->addIncoming(llvm::ConstantInt::get(i1Ty_, 0), curBB);
        phi->addIncoming(match, checkBB);
        cmpResult = phi;
    } else if (s.matcher == "toMatch") {
        llvm::Value *expectedVal = emitExpr(*s.expected);
        savedExpectedVal = expectedVal;
        if (!isStringValue(actualVal) || !isStringValue(expectedVal))
            codegenError("line " + std::to_string(s.loc.line) +
                ": toMatch: requires str operands");
        llvm::Value *patternLen = emitStringByteLen(expectedVal);
        llvm::Value *textLen    = emitStringByteLen(actualVal);
        auto isMatchTy = llvm::FunctionType::get(
            i64Ty_, {ptrTy_, i64Ty_, ptrTy_, i64Ty_}, false);
        auto isMatchFn = mod_->getOrInsertFunction("__ry_regex_is_match", isMatchTy);
        llvm::Value *r = builder_.CreateCall(
            isMatchFn, {expectedVal, patternLen, actualVal, textLen}, "regex_is_match");

        // -1 sentinel (kRegexMatchError) → runtime panic via emitRegexI64Guard pattern.
        llvm::Value *isErr = builder_.CreateICmpEQ(
            r, llvm::ConstantInt::get(*ctx_, llvm::APInt(64, static_cast<uint64_t>(-1), true)),
            "regex_match_is_err");
        llvm::Function *curFnTM = builder_.GetInsertBlock()->getParent();
        llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, "regex_match.err", curFnTM);
        llvm::BasicBlock *okBB  = llvm::BasicBlock::Create(*ctx_, "regex_match.ok",  curFnTM);
        builder_.CreateCondBr(isErr, errBB, okBB);

        builder_.SetInsertPoint(errBB);
        auto errFnTy = llvm::FunctionType::get(ptrTy_, {}, false);
        auto errFn = mod_->getOrInsertFunction("__ry_regex_get_last_error", errFnTy);
        llvm::Value *msgPtr = builder_.CreateCall(errFn, {}, "regex_err_msg");
        emitRuntimeError("error: %s\n", ".regex_runtime_err", {msgPtr});

        builder_.SetInsertPoint(okBB);
        cmpResult = builder_.CreateTrunc(r, i1Ty_, "to_match_bool");
    } else if (s.matcher == "toBeCloseTo") {
        llvm::Value *expectedVal = emitExpr(*s.expected);
        savedExpectedVal = expectedVal;
        llvm::Type *expectedTy = expectedVal->getType();
        if ((actualTy != i64Ty_ && actualTy != f64Ty_) ||
            (expectedTy != i64Ty_ && expectedTy != f64Ty_))
            codegenError("line " + std::to_string(s.loc.line) +
                ": toBeCloseTo: requires int or float operands");

        // Resolve `decimals` argument (defaults to 2; must be a non-negative integer literal in [0, 15]).
        int64_t decimals = 2;
        if (!s.extra_args.empty()) {
            auto *n = std::get_if<NumberExpr>(&s.extra_args[0]->data);
            if (!n || !n->suffix.empty())
                codegenError("line " + std::to_string(s.loc.line) +
                    ": toBeCloseTo: 'decimals' must be a plain integer literal");
            // NumberExpr.value stores the unsigned bit pattern of the literal as int64_t
            // (see parser-conventions.md "NumberExpr.value holds a non-negative bit pattern").
            // Compare via uint64_t so oversized literals (e.g. UINT64_MAX, which lands as
            // int64_t(-1)) cannot wrap past the signed range and silently bypass the bound.
            // Negative literals arrive as UnaryExpr-wrapped NumberExpr and fail the get_if
            // check above, so we never see a true negative magnitude here.
            uint64_t decimalsMag = static_cast<uint64_t>(n->value);
            if (decimalsMag > 15)
                codegenError("line " + std::to_string(s.loc.line) +
                    ": toBeCloseTo: 'decimals' must be in [0, 15]");
            decimals = static_cast<int64_t>(decimalsMag);
        }
        savedDecimals = decimals;
        savedDecimalsValid = true;

        auto [lf, rf] = promoteToFloat(actualVal, expectedVal);
        llvm::Value *diff = builder_.CreateFSub(lf, rf, "close_diff");
        llvm::Function *fabsDecl = llvm::Intrinsic::getDeclaration(
            mod_.get(), llvm::Intrinsic::fabs, {f64Ty_});
        llvm::Value *absDiff = builder_.CreateCall(fabsDecl, {diff}, "close_abs");
        double thresholdVal = 0.5 * std::pow(10.0, -static_cast<double>(decimals));
        llvm::Value *threshold = llvm::ConstantFP::get(f64Ty_, thresholdVal);
        cmpResult = builder_.CreateFCmpOLT(absDiff, threshold, "close_lt");
    } else {
        codegenError("line " + std::to_string(s.loc.line) +
                     ": unknown matcher '" + s.matcher + "'");
    }

    // Branch: if cmpResult is false, call __ry_test_expect_fail
    llvm::Function *currentFn = builder_.GetInsertBlock()->getParent();
    llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, "expect.fail", currentFn);
    llvm::BasicBlock *contBB = llvm::BasicBlock::Create(*ctx_, "expect.cont", currentFn);

    builder_.CreateCondBr(cmpResult, contBB, failBB);

    // Fail block: call __ry_test_expect_fail(line, actual_str, expected_str)
    builder_.SetInsertPoint(failBB);

    llvm::FunctionType *failFnTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {i32Ty_, ptrTy_, ptrTy_}, false);
    llvm::FunctionCallee failFn = mod_->getOrInsertFunction("__ry_test_expect_fail", failFnTy);

    // For now, format actual and expected as string representations
    // Use snprintf to format values at runtime
    auto snprintfFn = getStdlibSnprintf();

    auto formatValue = [&](llvm::Value *val, llvm::Type *ty, const std::string &bufName) -> llvm::Value* {
        llvm::Value *buf = builder_.CreateAlloca(
            llvm::ArrayType::get(llvm::Type::getInt8Ty(*ctx_), 64), nullptr, bufName);
        llvm::Value *bufSize = llvm::ConstantInt::get(i64Ty_, 64);

        if (ty == i64Ty_) {
            llvm::Value *fmt = cachedGlobalString("%ld", ".fmt_i");
            builder_.CreateCall(snprintfFn, {buf, bufSize, fmt, val});
        } else if (ty == i1Ty_) {
            llvm::Value *trueStr = cachedGlobalString("true", ".true");
            llvm::Value *falseStr = cachedGlobalString("false", ".false");
            return builder_.CreateSelect(val, trueStr, falseStr, "bool_str");
        } else if (ty == ptrTy_ && isStringValue(val)) {
            // str pointer: return directly (already a C string)
            return val;
        } else if (isAnyType(ty)) {
            llvm::Value *anyStr = emitAnyToString(val);
            llvm::Value *fmt = cachedGlobalString("%s", ".fmt_any");
            builder_.CreateCall(snprintfFn, {buf, bufSize, fmt, anyStr});
        } else {
            // For Option, Result, Record, Tuple, ADT enum, union, List, Set, Map
            // and any other complex type: use the same value-to-string path as print().
            return valueToString(val);
        }
        return buf;
    };

    llvm::Value *actualStr = formatValue(actualVal, actualTy, "actual_buf");

    llvm::Value *expectedStr;
    if (s.matcher == "toEq" || s.matcher == "toNotEq") {
        expectedStr = formatValue(savedExpectedVal, savedExpectedVal->getType(), "expected_buf");
    } else if (s.matcher == "toBeTrue") {
        expectedStr = cachedGlobalString("true", ".exp_true");
    } else if (s.matcher == "toBeFalse") {
        expectedStr = cachedGlobalString("false", ".exp_false");
    } else if (s.matcher == "toBeSome") {
        expectedStr = cachedGlobalString("Some(...)", ".exp_some");
    } else if (s.matcher == "toBeOk") {
        expectedStr = cachedGlobalString("Ok(...)", ".exp_ok");
    } else if (s.matcher == "toBeErr") {
        expectedStr = cachedGlobalString("Err(...)", ".exp_err");
    } else if (s.matcher == "toContain" || s.matcher == "toNotContain") {
        if (s.matcher == "toNotContain") {
            llvm::Value *valStr = formatValue(savedExpectedVal, savedExpectedVal->getType(), "expected_buf");
            llvm::Value *buf = builder_.CreateAlloca(
                llvm::ArrayType::get(llvm::Type::getInt8Ty(*ctx_), 128), nullptr, "nc_buf");
            llvm::Value *fmt = cachedGlobalString("not contain %s", ".fmt_nc");
            builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 128), fmt, valStr});
            expectedStr = buf;
        } else {
            expectedStr = formatValue(savedExpectedVal, savedExpectedVal->getType(), "expected_buf");
        }
    } else if (s.matcher == "toBeGreaterThan" || s.matcher == "toBeLessThan" ||
               s.matcher == "toBeGreaterThanOrEq" || s.matcher == "toBeLessThanOrEq") {
        std::string op;
        if (s.matcher == "toBeGreaterThan") op = "> ";
        else if (s.matcher == "toBeLessThan") op = "< ";
        else if (s.matcher == "toBeGreaterThanOrEq") op = ">= ";
        else op = "<= ";
        llvm::Value *valStr = formatValue(savedExpectedVal, savedExpectedVal->getType(), "expected_buf");
        llvm::Value *buf = builder_.CreateAlloca(
            llvm::ArrayType::get(llvm::Type::getInt8Ty(*ctx_), 128), nullptr, "cmp_buf");
        llvm::Value *fmt = cachedGlobalString(op + "%s", ".fmt_cmp");
        builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 128), fmt, valStr});
        expectedStr = buf;
    } else if (s.matcher == "toHaveLen") {
        llvm::Value *buf = builder_.CreateAlloca(
            llvm::ArrayType::get(llvm::Type::getInt8Ty(*ctx_), 128), nullptr, "len_buf");
        llvm::Value *fmt = cachedGlobalString("length %ld", ".fmt_len");
        builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 128), fmt, savedExpectedVal});
        expectedStr = buf;
    } else if (s.matcher == "toBeEmpty") {
        expectedStr = cachedGlobalString("empty", ".exp_empty");
    } else if (s.matcher == "toStartWith") {
        llvm::Value *buf = builder_.CreateAlloca(
            llvm::ArrayType::get(llvm::Type::getInt8Ty(*ctx_), 128), nullptr, "sw_buf");
        llvm::Value *fmt = cachedGlobalString("start with \"%s\"", ".fmt_sw");
        builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 128), fmt, savedExpectedVal});
        expectedStr = buf;
    } else if (s.matcher == "toEndWith") {
        llvm::Value *buf = builder_.CreateAlloca(
            llvm::ArrayType::get(llvm::Type::getInt8Ty(*ctx_), 128), nullptr, "ew_buf");
        llvm::Value *fmt = cachedGlobalString("end with \"%s\"", ".fmt_ew");
        builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 128), fmt, savedExpectedVal});
        expectedStr = buf;
    } else if (s.matcher == "toMatch") {
        llvm::Value *buf = builder_.CreateAlloca(
            llvm::ArrayType::get(llvm::Type::getInt8Ty(*ctx_), 128), nullptr, "tm_buf");
        llvm::Value *fmt = cachedGlobalString("match \"%s\"", ".fmt_tm");
        builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 128), fmt, savedExpectedVal});
        expectedStr = buf;
    } else if (s.matcher == "toBeCloseTo") {
        llvm::Value *valStr = formatValue(savedExpectedVal, savedExpectedVal->getType(), "expected_buf");
        llvm::Value *buf = builder_.CreateAlloca(
            llvm::ArrayType::get(llvm::Type::getInt8Ty(*ctx_), 128), nullptr, "ctc_buf");
        llvm::Value *fmt = cachedGlobalString("close to %s (decimals=%ld)", ".fmt_ctc");
        llvm::Value *decimalsVal = llvm::ConstantInt::get(
            i64Ty_, static_cast<uint64_t>(savedDecimalsValid ? savedDecimals : 2));
        builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 128), fmt, valStr, decimalsVal});
        expectedStr = buf;
    } else {
        expectedStr = cachedGlobalString("None", ".exp_none");
    }

    builder_.CreateCall(failFn, {llvm::ConstantInt::get(i32Ty_, static_cast<uint64_t>(s.loc.line)), actualStr, expectedStr});
    builder_.CreateBr(contBB);

    // Continue block
    builder_.SetInsertPoint(contBB);
}

// ===== Test: fail() / fail(msg) =====

void CodeGen::emitFailCall(CallStmt &s) {
    if (!test_mode_)
        codegenError("'fail' is only allowed in test mode (use 'ry test')");
    if (!testing_intrinsics_imported_.count("fail"))
        codegenError(s.loc, "'fail' requires 'from testing import fail'");

    if (s.args.size() > 1)
        codegenError("fail() expects 0 or 1 argument(s), but got " + std::to_string(s.args.size()));

    // Inject the call-site line as the first argument and dispatch through the
    // Ry-level `fail(line: int, message: str = "")` function declared in
    // share/std/testing/testing.ry. The compiler still special-cases the
    // callee name so it can synthesize the line number; the function body
    // itself runs as ordinary Ry code that calls the @native _report_fail.
    std::vector<ExprPtr> synthArgs;

    auto lineNode = std::make_unique<ExprNode>();
    lineNode->data = NumberExpr{static_cast<int64_t>(s.loc.line), ""};
    lineNode->loc = s.loc;
    synthArgs.push_back(std::move(lineNode));

    if (s.args.size() == 1) {
        synthArgs.push_back(std::move(s.args[0]));
    } else {
        auto msgNode = std::make_unique<ExprNode>();
        msgNode->data = StringExpr{""};
        msgNode->loc = s.loc;
        synthArgs.push_back(std::move(msgNode));
    }

    emitUserFnCall("fail", synthArgs);
}

} // namespace ry
