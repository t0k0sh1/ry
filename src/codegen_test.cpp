#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <stdexcept>


namespace ry {

// ===== Outline helper =====

void CodeGen::emitOutlinePrintf(const std::string &label, llvm::Value *nameVal) {
    auto printfFn = getStdlibPrintf();
    std::string indent(outline_depth_ * 2, ' ');
    llvm::Value *fmt = cachedGlobalString(indent + label, ".outline_fmt");
    if (nameVal)
        builder_.CreateCall(printfFn, {fmt, nameVal});
    else
        builder_.CreateCall(printfFn, {fmt});
}

// ===== Test: describe/it (lambda argument) =====

static LambdaExpr &extractLambdaArg(CallStmt &s, const std::string &callee) {
    if (s.args.size() != 2)
        throw std::runtime_error(callee + "() requires exactly one description string and a lambda argument");
    auto *lambda = std::get_if<std::unique_ptr<LambdaExpr>>(&s.args.back()->data);
    if (!lambda)
        throw std::runtime_error(callee + "() last argument must be a lambda argument");
    return **lambda;
}

void CodeGen::emitDescribeCall(CallStmt &s) {
    if (!test_mode_)
        codegenError("'describe' is only allowed in test mode (use 'ry test')");

    auto &lambda = extractLambdaArg(s, "describe");

    llvm::Value *descName = emitExpr(*s.args[0]);
    if (!descName->getType()->isPointerTy())
        codegenError("describe() first argument must be a string");

    if (outline_mode_) {
        emitOutlinePrintf("describe %s\n", descName);
        ++outline_depth_;
        for (auto &stmt : lambda.body) {
            if (auto *cs = std::get_if<CallStmt>(&stmt)) {
                if (cs->callee == "describe" || cs->callee == "it")
                    emitStmt(*cs);
            }
        }
        --outline_depth_;
        return;
    }

    auto [descBeginFn, descEndFn] = getTestDescribeFunctions();

    builder_.CreateCall(descBeginFn, {descName});

    for (auto &stmt : lambda.body)
        std::visit([this](auto &st) { emitStmt(st); }, stmt);

    builder_.CreateCall(descEndFn);
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
            fieldOrder.push_back(fmtStr[i+1] - '0');
            i += 2;
        } else if (fmtStr[i] == '%') {
            cFmt += "%%";
        } else {
            cFmt += fmtStr[i];
        }
    }
}

void CodeGen::emitItCall(CallStmt &s) {
    if (!test_mode_)
        codegenError("'it' is only allowed in test mode (use 'ry test')");

    // Check for @each / @property directives
    if (hasDirective(s.directives, "each")) {
        emitEachItCall(s);
        return;
    }
    if (hasDirective(s.directives, "property")) {
        emitPropertyItCall(s);
        return;
    }

    if (outline_mode_) {
        llvm::Value *itName = emitExpr(*s.args[0]);
        if (!itName->getType()->isPointerTy())
            codegenError("it() first argument must be a string");
        emitOutlinePrintf("it %s\n", itName);
        return;
    }

    auto &lambda = extractLambdaArg(s, "it");
    auto [itBeginFn, itEndFn] = getTestItFunctions();

    llvm::Value *itName = emitExpr(*s.args[0]);
    if (!itName->getType()->isPointerTy())
        codegenError("it() first argument must be a string");

    llvm::Function *testFunc = emitTestFunction("__test_", {}, lambda, "test");

    builder_.CreateCall(itBeginFn, {itName});
    builder_.CreateCall(testFunc);
    builder_.CreateCall(itEndFn);
}

// ===== Test: @each parameterized test =====

void CodeGen::emitEachItCall(CallStmt &s) {
    // Find @each directive
    Directive *eachDir = nullptr;
    for (auto &d : s.directives) {
        if (d.name == "each") { eachDir = &d; break; }
    }
    if (!eachDir || eachDir->args.empty() || !eachDir->args[0].value || eachDir->args[0].name.has_value())
        codegenError("@each directive requires a list expression");

    if (s.args.size() != 2)
        codegenError("@each it() requires exactly one description string and a lambda argument");
    auto *lambda = std::get_if<std::unique_ptr<LambdaExpr>>(&s.args.back()->data);
    if (!lambda)
        codegenError("@each it() last argument must be a lambda");
    auto &lam = **lambda;

    // Get the description format string
    auto *descStr = std::get_if<StringExpr>(&s.args[0]->data);
    if (!descStr)
        codegenError("@each it() first argument must be a string literal");
    std::string fmtStr = descStr->value;

    if (outline_mode_) {
        emitOutlinePrintf("it " + fmtStr + " (@each)\n");
        return;
    }

    // Evaluate the list expression to get the list header
    llvm::Value *listPtr = emitExpr(*eachDir->args[0].value);
    llvm::Type *elemTy = getListElementType(listPtr);
    if (!elemTy)
        codegenError("@each requires a list of tuples");

    auto *tupleTy = llvm::dyn_cast<llvm::StructType>(elemTy);
    if (!tupleTy)
        codegenError("@each requires a list of tuples");

    unsigned numFields = tupleTy->getNumElements();
    if (numFields != lam.params.size())
        codegenError("@each: tuple arity (" + std::to_string(numFields) +
                     ") doesn't match lambda parameter count (" + std::to_string(lam.params.size()) + ")");

    // Build parameter types from tuple
    std::vector<llvm::Type*> paramTypes;
    for (unsigned i = 0; i < numFields; ++i)
        paramTypes.push_back(tupleTy->getElementType(i));

    llvm::Function *testFunc = emitTestFunction("__test_each_", paramTypes, lam, "@each test");

    emitEachItLoop(listPtr, elemTy, numFields, fmtStr, testFunc);
}

void CodeGen::emitEachItLoop(llvm::Value *listPtr, llvm::Type *elemTy, unsigned numFields,
                              const std::string &fmtStr, llvm::Function *testFunc) {
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

    builder_.CreateCall(itBeginFn, {fmtBuf});
    builder_.CreateCall(testFunc, fieldVals);
    builder_.CreateCall(itEndFn);

    llvm::Value *nextI = builder_.CreateAdd(iVal, llvm::ConstantInt::get(i64Ty_, 1), "next_i");
    builder_.CreateStore(nextI, iAlloca);
    builder_.CreateBr(condBB);

    builder_.SetInsertPoint(endBB);
}

// ===== Test: @property property-based test =====

void CodeGen::emitPropertyItCall(CallStmt &s) {
    if (outline_mode_) {
        llvm::Value *itName = emitExpr(*s.args[0]);
        if (!itName->getType()->isPointerTy())
            codegenError("@property it() first argument must be a string");
        emitOutlinePrintf("it %s (@property)\n", itName);
        return;
    }

    // Find @property directive and get count
    int64_t count = 100; // default
    if (const ExprNode *countExpr = getDirectiveNamedArg(s.directives, "property", "count")) {
        if (auto *n = std::get_if<NumberExpr>(&countExpr->data)) {
            if (n->value <= 0)
                codegenError("@property 'count' must be a positive integer");
            count = static_cast<int64_t>(n->value);
        } else {
            codegenError("@property 'count' must be an integer literal");
        }
    }

    if (s.args.size() != 2)
        codegenError("@property it() requires exactly one description string and a lambda argument");
    auto *lambda = std::get_if<std::unique_ptr<LambdaExpr>>(&s.args.back()->data);
    if (!lambda)
        codegenError("@property it() last argument must be a lambda");
    auto &lam = **lambda;

    llvm::Value *itName = emitExpr(*s.args[0]);
    if (!itName->getType()->isPointerTy())
        codegenError("@property it() first argument must be a string");

    // Resolve parameter types
    std::vector<llvm::Type*> paramTypes;
    for (auto &p : lam.params)
        paramTypes.push_back(resolveType(p.type->toString()));

    llvm::Function *testFunc = emitTestFunction("__prop_test_", paramTypes, lam, "@property test");

    std::vector<std::string> paramNames;
    for (auto &p : lam.params)
        paramNames.push_back(p.name);

    emitPropertyItLoop(testFunc, itName, paramTypes, paramNames, count);
}

void CodeGen::emitPropertyItLoop(llvm::Function *testFunc, llvm::Value *descVal,
                                  const std::vector<llvm::Type*> &paramTypes,
                                  const std::vector<std::string> &paramNames, int64_t count) {
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
    llvm::Value *cond = builder_.CreateICmpSLT(iVal, llvm::ConstantInt::get(i64Ty_, count), "prop_cond");
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

    builder_.CreateCall(testFunc, randVals);

    llvm::Value *failed = builder_.CreateCall(isFailedFn, {}, "is_failed");
    llvm::Value *didFail = builder_.CreateICmpNE(failed, llvm::ConstantInt::get(i64Ty_, 0), "did_fail");

    llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, "prop.fail", fn_);
    llvm::BasicBlock *contBB = llvm::BasicBlock::Create(*ctx_, "prop.cont", fn_);
    builder_.CreateCondBr(didFail, failBB, contBB);

    builder_.SetInsertPoint(failBB);
    {
        auto printfFn = getStdlibPrintf();
        std::string ceFmt = "    \033[31mCounterexample: (";
        for (unsigned i = 0; i < paramTypes.size(); ++i) {
            if (i > 0) ceFmt += ", ";
            ceFmt += paramNames[i] + " = %s";
        }
        ceFmt += ")\033[0m\n";
        llvm::Value *ceFmtStr = cachedGlobalString(ceFmt, ".prop_ce_fmt");
        std::vector<llvm::Value*> ceArgs = {ceFmtStr};
        for (unsigned i = 0; i < randVals.size(); ++i)
            ceArgs.push_back(valueToString(randVals[i]));
        builder_.CreateCall(printfFn, ceArgs);
    }
    for (unsigned i = 0; i < paramTypes.size(); ++i) {
        if (paramTypes[i] == ptrTy_)
            builder_.CreateCall(getStdlibFree(), {randVals[i]});
    }
    builder_.CreateBr(endBB);

    builder_.SetInsertPoint(contBB);
    for (unsigned i = 0; i < paramTypes.size(); ++i) {
        if (paramTypes[i] == ptrTy_)
            builder_.CreateCall(getStdlibFree(), {randVals[i]});
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

    if (hasDirective(s->directives, "each")) {
        emitEachItDirective(s);
        return;
    }
    if (hasDirective(s->directives, "property")) {
        emitPropertyItDirective(s);
        return;
    }

    // Basic @it: function must have no parameters
    if (!s->params.empty())
        codegenError("@it: function '" + s->name + "' has parameters but no @each or @property directive");

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
        codegenError("@it: internal error — function '" + s->name + "' not found after emit");
    llvm::Function *testFunc = overloads->back().func;

    auto [itBeginFn, itEndFn] = getTestItFunctions();
    llvm::Value *descVal = cachedGlobalString(desc, ".it_desc");
    builder_.CreateCall(itBeginFn, {descVal});
    builder_.CreateCall(testFunc);
    builder_.CreateCall(itEndFn);
}

void CodeGen::emitEachItDirective(std::unique_ptr<FnStmt> &s) {
    Directive *eachDir = findDirective(s->directives, "each");
    if (!eachDir || eachDir->args.empty() || !eachDir->args[0].value || eachDir->args[0].name.has_value())
        codegenError("@each directive requires a list expression");

    std::string fmtStr = getDirectivePositionalArg(s->directives, "it");

    if (outline_mode_) {
        emitOutlinePrintf("it " + fmtStr + " (@each)\n");
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
        codegenError("@each @it: internal error — function '" + s->name + "' not found after emit");
    llvm::Function *testFunc = overloads->back().func;

    emitEachItLoop(listPtr, elemTy, numFields, fmtStr, testFunc);
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
        codegenError("@property @it: internal error — function '" + s->name + "' not found after emit");
    llvm::Function *testFunc = overloads->back().func;

    llvm::Value *descVal = cachedGlobalString(desc, ".it_desc");
    emitPropertyItLoop(testFunc, descVal, paramTypes, paramNames, count);
}

void CodeGen::emitDescribeDirective(std::unique_ptr<FnStmt> &s) {
    if (!test_mode_)
        codegenError("@describe is only allowed in test mode (use 'ry test')");

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
            } else if (auto *cs = std::get_if<CallStmt>(&stmt)) {
                if (cs->callee == "describe" || cs->callee == "it")
                    emitStmt(*cs);
            }
        }
        --outline_depth_;
        return;
    }

    stripDirectives(s->directives, {"describe"});
    emitStmt(s);

    auto *overloads = findFunction(s->name);
    if (!overloads || overloads->empty())
        codegenError("@describe: internal error — function '" + s->name + "' not found after emit");
    llvm::Function *descFunc = overloads->back().func;

    auto [descBeginFn, descEndFn] = getTestDescribeFunctions();
    builder_.CreateCall(descBeginFn, {descVal});
    builder_.CreateCall(descFunc);
    builder_.CreateCall(descEndFn);
}

// ===== Test: mock(fn_name, replacement) =====

void CodeGen::emitMockCall(CallStmt &s) {
    if (!test_mode_)
        codegenError("'mock' is only allowed in test mode (use 'ry test')");

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
        codegenError("mock(): second argument must be a non-capturing lambda or function reference");

    // Verify it's a function pointer (not a closure)
    if (!fnInfo->capturedVars.empty())
        codegenError("mock(): capture-based closures are not supported, use a plain lambda");

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

    // Call __ry_mock_set(name, fn_ptr)
    llvm::FunctionType *mockSetTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {ptrTy_, ptrTy_}, false);
    llvm::FunctionCallee mockSetFn = mod_->getOrInsertFunction("__ry_mock_set", mockSetTy);

    // Cache global string per function name
    auto &nameStr = mock_name_strings_[fnName];
    if (!nameStr) nameStr = cachedGlobalString(fnName, ".mock." + fnName);
    builder_.CreateCall(mockSetFn, {nameStr, replacement});
}

// ===== Test: ExpectStmt =====

void CodeGen::emitStmt(ExpectStmt &s) {
    emitCoverage(s.loc);
    if (!test_mode_)
        codegenError("'expect' is only allowed in test mode (use 'ry test')");

    llvm::Value *actualVal = emitExpr(*s.actual);
    llvm::Type *actualTy = actualVal->getType();

    llvm::Value *cmpResult = nullptr;
    // Save expectedVal from comparison section to reuse in failure message
    llvm::Value *savedExpectedVal = nullptr;

    if (s.matcher == "to_eq" || s.matcher == "to_not_eq") {
        llvm::Value *expectedVal = emitExpr(*s.expected);
        savedExpectedVal = expectedVal;
        llvm::Type *expectedTy = expectedVal->getType();

        llvm::Value *eqResult = nullptr;
        if (actualTy == i64Ty_ && expectedTy == i64Ty_) {
            eqResult = builder_.CreateICmpEQ(actualVal, expectedVal, "eq");
        } else if (actualTy == f64Ty_ && expectedTy == f64Ty_) {
            eqResult = builder_.CreateFCmpOEQ(actualVal, expectedVal, "eq");
        } else if (actualTy == i1Ty_ && expectedTy == i1Ty_) {
            eqResult = builder_.CreateICmpEQ(actualVal, expectedVal, "eq");
        } else if (actualTy == ptrTy_ && expectedTy == ptrTy_) {
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
        } else if (isOptionType(actualTy) && isOptionType(expectedTy) && actualTy == expectedTy) {
            // Option<T> == Option<T>: both None or both Some with equal inner
            llvm::Value *aHas = builder_.CreateExtractValue(actualVal, 0, "opt_a_has");
            llvm::Value *bHas = builder_.CreateExtractValue(expectedVal, 0, "opt_b_has");
            llvm::Value *bothNone = builder_.CreateAnd(
                builder_.CreateNot(aHas), builder_.CreateNot(bHas), "both_none");
            llvm::Value *bothSome = builder_.CreateAnd(aHas, bHas, "both_some");

            llvm::Value *aInner = builder_.CreateExtractValue(actualVal, 1, "opt_a_inner");
            llvm::Value *bInner = builder_.CreateExtractValue(expectedVal, 1, "opt_b_inner");
            llvm::Type *innerTy = aInner->getType();

            llvm::Value *innerEq;
            if (innerTy == i64Ty_)
                innerEq = builder_.CreateICmpEQ(aInner, bInner, "opt_inner_eq");
            else if (innerTy == f64Ty_)
                innerEq = builder_.CreateFCmpOEQ(aInner, bInner, "opt_inner_eq");
            else if (innerTy == i1Ty_)
                innerEq = builder_.CreateICmpEQ(aInner, bInner, "opt_inner_eq");
            else if (innerTy == ptrTy_) {
                auto strcmpFn = getStdlibStrcmp();
                llvm::Value *r = builder_.CreateCall(strcmpFn, {aInner, bInner}, "strcmp");
                innerEq = builder_.CreateICmpEQ(r, llvm::ConstantInt::get(i32Ty_, 0), "opt_inner_eq");
            } else {
                codegenError("line " + std::to_string(s.loc.line) +
                    ": " + s.matcher + ": unsupported Option inner type for comparison");
            }

            eqResult = builder_.CreateOr(bothNone, builder_.CreateAnd(bothSome, innerEq), "opt_eq");
        } else {
            codegenError("line " + std::to_string(s.loc.line) +
                                     ": " + s.matcher + ": unsupported types for comparison");
        }
        cmpResult = (s.matcher == "to_not_eq")
            ? builder_.CreateNot(eqResult, "not_eq")
            : eqResult;
    } else if (s.matcher == "to_be_true") {
        if (actualTy != i1Ty_)
            codegenError("line " + std::to_string(s.loc.line) +
                                     ": to_be_true: expected bool");
        cmpResult = actualVal;
    } else if (s.matcher == "to_be_false") {
        if (actualTy != i1Ty_)
            codegenError("line " + std::to_string(s.loc.line) +
                                     ": to_be_false: expected bool");
        cmpResult = builder_.CreateNot(actualVal, "not");
    } else if (s.matcher == "to_be_none") {
        if (!isOptionType(actualTy))
            codegenError("line " + std::to_string(s.loc.line) +
                                     ": to_be_none: expected Option type");
        llvm::Value *hasVal = builder_.CreateExtractValue(actualVal, {0}, "has_val");
        cmpResult = builder_.CreateNot(hasVal, "is_none");
    } else if (s.matcher == "to_be_some") {
        if (!isOptionType(actualTy))
            codegenError("line " + std::to_string(s.loc.line) +
                                     ": to_be_some: expected Option type");
        cmpResult = builder_.CreateExtractValue(actualVal, {0}, "is_some");
    } else if (s.matcher == "to_be_ok") {
        if (!isResultType(actualTy))
            codegenError("line " + std::to_string(s.loc.line) +
                                     ": to_be_ok: expected Result type");
        cmpResult = builder_.CreateExtractValue(actualVal, {0}, "is_ok");
    } else if (s.matcher == "to_be_err") {
        if (!isResultType(actualTy))
            codegenError("line " + std::to_string(s.loc.line) +
                                     ": to_be_err: expected Result type");
        llvm::Value *isOk = builder_.CreateExtractValue(actualVal, {0}, "is_ok");
        cmpResult = builder_.CreateNot(isOk, "is_err");
    } else if (s.matcher == "to_contain" || s.matcher == "to_not_contain") {
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
                eq = builder_.CreateICmpEQ(elem, expectedVal, "eq");
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
        cmpResult = (s.matcher == "to_not_contain")
            ? builder_.CreateNot(containResult, "not_contain")
            : containResult;
    } else if (s.matcher == "to_be_greater_than" || s.matcher == "to_be_less_than" ||
               s.matcher == "to_be_greater_than_or_eq" || s.matcher == "to_be_less_than_or_eq") {
        llvm::Value *expectedVal = emitExpr(*s.expected);
        savedExpectedVal = expectedVal;
        llvm::Type *expectedTy = expectedVal->getType();

        // Map matcher name to ICmp/FCmp predicates
        llvm::CmpInst::Predicate iPred, fPred;
        if (s.matcher == "to_be_greater_than") {
            iPred = llvm::CmpInst::ICMP_SGT; fPred = llvm::CmpInst::FCMP_OGT;
        } else if (s.matcher == "to_be_less_than") {
            iPred = llvm::CmpInst::ICMP_SLT; fPred = llvm::CmpInst::FCMP_OLT;
        } else if (s.matcher == "to_be_greater_than_or_eq") {
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
    } else if (s.matcher == "to_have_length" || s.matcher == "to_be_empty") {
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
            auto utf8LenTy = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
            auto utf8LenFn = mod_->getOrInsertFunction("__ry_utf8_len", utf8LenTy);
            len = builder_.CreateCall(utf8LenFn, {actualVal}, "str_len");
        }

        if (s.matcher == "to_have_length") {
            llvm::Value *expectedVal = emitExpr(*s.expected);
            savedExpectedVal = expectedVal;
            if (expectedVal->getType() != i64Ty_)
                codegenError("line " + std::to_string(s.loc.line) +
                    ": to_have_length: expected int argument");
            cmpResult = builder_.CreateICmpEQ(len, expectedVal, "has_length");
        } else {
            cmpResult = builder_.CreateICmpEQ(len, llvm::ConstantInt::get(i64Ty_, 0), "is_empty");
        }
    } else if (s.matcher == "to_start_with") {
        llvm::Value *expectedVal = emitExpr(*s.expected);
        savedExpectedVal = expectedVal;
        if (actualTy != ptrTy_ || expectedVal->getType() != ptrTy_)
            codegenError("line " + std::to_string(s.loc.line) +
                ": to_start_with: requires str operands");
        auto strlenFn = getStdlibStrlen();
        auto strncmpFn = getStdlibStrncmp();
        llvm::Value *prefixLen = builder_.CreateCall(strlenFn, {expectedVal}, "prefix_len");
        llvm::Value *cmp = builder_.CreateCall(strncmpFn, {actualVal, expectedVal, prefixLen}, "strncmp");
        cmpResult = builder_.CreateICmpEQ(cmp, llvm::ConstantInt::get(i32Ty_, 0), "starts_with");
    } else if (s.matcher == "to_end_with") {
        llvm::Value *expectedVal = emitExpr(*s.expected);
        savedExpectedVal = expectedVal;
        if (actualTy != ptrTy_ || expectedVal->getType() != ptrTy_)
            codegenError("line " + std::to_string(s.loc.line) +
                ": to_end_with: requires str operands");
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
        } else if (ty == f64Ty_) {
            llvm::Value *fmt = cachedGlobalString("%g", ".fmt_f");
            builder_.CreateCall(snprintfFn, {buf, bufSize, fmt, val});
        } else if (ty == i1Ty_) {
            llvm::Value *trueStr = cachedGlobalString("true", ".true");
            llvm::Value *falseStr = cachedGlobalString("false", ".false");
            return builder_.CreateSelect(val, trueStr, falseStr, "bool_str");
        } else if (ty == ptrTy_) {
            // Assume string pointer, return directly
            return val;
        } else if (isAnyType(ty)) {
            llvm::Value *anyStr = emitAnyToString(val);
            llvm::Value *fmt = cachedGlobalString("%s", ".fmt_any");
            builder_.CreateCall(snprintfFn, {buf, bufSize, fmt, anyStr});
        } else if (isOptionType(ty)) {
            llvm::Value *hasVal = builder_.CreateExtractValue(val, 0, "fmt_opt_has");
            llvm::Value *innerVal = builder_.CreateExtractValue(val, 1, "fmt_opt_inner");
            llvm::Type *innerTy = innerVal->getType();

            llvm::BasicBlock *someBB = llvm::BasicBlock::Create(*ctx_, "fmt.some", fn_);
            llvm::BasicBlock *noneBB = llvm::BasicBlock::Create(*ctx_, "fmt.none", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "fmt.end", fn_);
            builder_.CreateCondBr(hasVal, someBB, noneBB);

            builder_.SetInsertPoint(someBB);
            // Format as "Some(<inner>)"
            if (innerTy == i64Ty_) {
                llvm::Value *fmt = cachedGlobalString("Some(%ld)", ".fmt_opt_i");
                builder_.CreateCall(snprintfFn, {buf, bufSize, fmt, innerVal});
            } else if (innerTy == f64Ty_) {
                llvm::Value *fmt = cachedGlobalString("Some(%g)", ".fmt_opt_f");
                builder_.CreateCall(snprintfFn, {buf, bufSize, fmt, innerVal});
            } else if (innerTy == i1Ty_) {
                llvm::Value *trueStr = cachedGlobalString("Some(true)", ".fmt_opt_bt");
                llvm::Value *falseStr = cachedGlobalString("Some(false)", ".fmt_opt_bf");
                llvm::Value *boolFmt = builder_.CreateSelect(innerVal, trueStr, falseStr, "opt_bool_fmt");
                builder_.CreateCall(snprintfFn, {buf, bufSize, boolFmt});
            } else if (innerTy == ptrTy_) {
                llvm::Value *fmt = cachedGlobalString("Some(%s)", ".fmt_opt_s");
                builder_.CreateCall(snprintfFn, {buf, bufSize, fmt, innerVal});
            } else {
                llvm::Value *fmt = cachedGlobalString("Some(...)", ".fmt_opt_u");
                builder_.CreateCall(snprintfFn, {buf, bufSize, fmt});
            }
            builder_.CreateBr(endBB);

            builder_.SetInsertPoint(noneBB);
            llvm::Value *noneFmt = cachedGlobalString("None", ".fmt_opt_none");
            builder_.CreateCall(snprintfFn, {buf, bufSize, noneFmt});
            builder_.CreateBr(endBB);

            builder_.SetInsertPoint(endBB);
            return buf;
        } else {
            llvm::Value *fmt = cachedGlobalString("<value>", ".fmt_val");
            builder_.CreateCall(snprintfFn, {buf, bufSize, fmt});
        }
        return buf;
    };

    llvm::Value *actualStr = formatValue(actualVal, actualTy, "actual_buf");

    llvm::Value *expectedStr;
    if (s.matcher == "to_eq" || s.matcher == "to_not_eq") {
        expectedStr = formatValue(savedExpectedVal, savedExpectedVal->getType(), "expected_buf");
    } else if (s.matcher == "to_be_true") {
        expectedStr = cachedGlobalString("true", ".exp_true");
    } else if (s.matcher == "to_be_false") {
        expectedStr = cachedGlobalString("false", ".exp_false");
    } else if (s.matcher == "to_be_some") {
        expectedStr = cachedGlobalString("Some(...)", ".exp_some");
    } else if (s.matcher == "to_be_ok") {
        expectedStr = cachedGlobalString("Ok(...)", ".exp_ok");
    } else if (s.matcher == "to_be_err") {
        expectedStr = cachedGlobalString("Err(...)", ".exp_err");
    } else if (s.matcher == "to_contain" || s.matcher == "to_not_contain") {
        if (s.matcher == "to_not_contain") {
            llvm::Value *valStr = formatValue(savedExpectedVal, savedExpectedVal->getType(), "expected_buf");
            llvm::Value *buf = builder_.CreateAlloca(
                llvm::ArrayType::get(llvm::Type::getInt8Ty(*ctx_), 128), nullptr, "nc_buf");
            llvm::Value *fmt = cachedGlobalString("not contain %s", ".fmt_nc");
            builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 128), fmt, valStr});
            expectedStr = buf;
        } else {
            expectedStr = formatValue(savedExpectedVal, savedExpectedVal->getType(), "expected_buf");
        }
    } else if (s.matcher == "to_be_greater_than" || s.matcher == "to_be_less_than" ||
               s.matcher == "to_be_greater_than_or_eq" || s.matcher == "to_be_less_than_or_eq") {
        std::string op;
        if (s.matcher == "to_be_greater_than") op = "> ";
        else if (s.matcher == "to_be_less_than") op = "< ";
        else if (s.matcher == "to_be_greater_than_or_eq") op = ">= ";
        else op = "<= ";
        llvm::Value *valStr = formatValue(savedExpectedVal, savedExpectedVal->getType(), "expected_buf");
        llvm::Value *buf = builder_.CreateAlloca(
            llvm::ArrayType::get(llvm::Type::getInt8Ty(*ctx_), 128), nullptr, "cmp_buf");
        llvm::Value *fmt = cachedGlobalString(op + "%s", ".fmt_cmp");
        builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 128), fmt, valStr});
        expectedStr = buf;
    } else if (s.matcher == "to_have_length") {
        llvm::Value *buf = builder_.CreateAlloca(
            llvm::ArrayType::get(llvm::Type::getInt8Ty(*ctx_), 128), nullptr, "len_buf");
        llvm::Value *fmt = cachedGlobalString("length %ld", ".fmt_len");
        builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 128), fmt, savedExpectedVal});
        expectedStr = buf;
    } else if (s.matcher == "to_be_empty") {
        expectedStr = cachedGlobalString("empty", ".exp_empty");
    } else if (s.matcher == "to_start_with") {
        llvm::Value *buf = builder_.CreateAlloca(
            llvm::ArrayType::get(llvm::Type::getInt8Ty(*ctx_), 128), nullptr, "sw_buf");
        llvm::Value *fmt = cachedGlobalString("start with \"%s\"", ".fmt_sw");
        builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 128), fmt, savedExpectedVal});
        expectedStr = buf;
    } else if (s.matcher == "to_end_with") {
        llvm::Value *buf = builder_.CreateAlloca(
            llvm::ArrayType::get(llvm::Type::getInt8Ty(*ctx_), 128), nullptr, "ew_buf");
        llvm::Value *fmt = cachedGlobalString("end with \"%s\"", ".fmt_ew");
        builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 128), fmt, savedExpectedVal});
        expectedStr = buf;
    } else {
        expectedStr = cachedGlobalString("None", ".exp_none");
    }

    builder_.CreateCall(failFn, {llvm::ConstantInt::get(i32Ty_, s.loc.line), actualStr, expectedStr});
    builder_.CreateBr(contBB);

    // Continue block
    builder_.SetInsertPoint(contBB);
}

// ===== Test: fail() / fail(msg) =====

void CodeGen::emitFailCall(CallStmt &s) {
    if (!test_mode_)
        codegenError("'fail' is only allowed in test mode (use 'ry test')");

    if (s.args.size() > 1)
        codegenError("fail() expects 0 or 1 argument(s), but got " + std::to_string(s.args.size()));

    llvm::FunctionType *failTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {i32Ty_, ptrTy_}, false);
    llvm::FunctionCallee failFn = mod_->getOrInsertFunction("__ry_test_fail", failTy);

    llvm::Value *msg;
    if (s.args.size() == 1) {
        msg = emitExpr(*s.args[0]);
        if (msg->getType() != ptrTy_)
            codegenError("fail() argument must be a string");
    } else {
        if (!fail_empty_msg_)
            fail_empty_msg_ = cachedGlobalString("", ".fail_empty");
        msg = fail_empty_msg_;
    }

    builder_.CreateCall(failFn, {llvm::ConstantInt::get(i32Ty_, s.loc.line), msg});
}

} // namespace ry
