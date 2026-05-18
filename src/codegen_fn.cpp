#include "ry/codegen.hpp"
#include "ry/stdlib_registry.hpp"
#include "ry/diagnostic.hpp"
#include "ry/directive_meta.hpp"
#include "ry/sema_return.hpp"
#include <algorithm>
#include <filesystem>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>


namespace ry {

namespace {

bool sameFnTypeInfoShape(const CodeGen::FnTypeInfo &lhs, const CodeGen::FnTypeInfo &rhs) {
    return lhs.paramTypes == rhs.paramTypes &&
           lhs.paramTypeNames == rhs.paramTypeNames &&
           lhs.returnType == rhs.returnType &&
           lhs.returnTypeName == rhs.returnTypeName &&
           lhs.isUniformClosure == rhs.isUniformClosure;
}

} // namespace

// --- JNI-like naming convention helpers ---

std::string CodeGen::deriveRuntimeFnName(const std::string &package,
                                         const std::string &fn_name) {
    if (package.empty())
        return "__ry_" + fn_name;
    return "__ry_" + package + "_" + fn_name;
}

const std::unordered_set<std::string>& CodeGen::getRequiredLibraries() const {
    return used_native_libraries_;
}

void CodeGen::recordReturnFnTypeInfo(llvm::Function *fn, const FnTypeInfo &info,
                                     const std::string &fnNameForErrors) {
    auto it = return_fn_type_info_.find(fn);
    if (it == return_fn_type_info_.end()) {
        return_fn_type_info_[fn] = info;
        return;
    }
    if (!sameFnTypeInfoShape(it->second, info)) {
        codegenError("fn '" + fnNameForErrors +
                     "' returns incompatible function metadata across branches");
    }
}

void CodeGen::recordReturnTypeMetaSnapshot(llvm::Function *fn, llvm::Value *val,
                                           const std::string &fnNameForErrors) {
    llvm::Type *taskTy = getTaskResultType(val);
    auto [taskIt, taskInserted] = return_task_result_types_.emplace(fn, taskTy);
    if (!taskInserted && taskIt->second != taskTy) {
        codegenError("fn '" + fnNameForErrors +
                     "' returns incompatible Task result metadata across branches");
    }

    llvm::Type *threadTy = getThreadResultType(val);
    auto [threadIt, threadInserted] = return_thread_result_types_.emplace(fn, threadTy);
    if (!threadInserted && threadIt->second != threadTy) {
        codegenError("fn '" + fnNameForErrors +
                     "' returns incompatible Thread result metadata across branches");
    }

    auto *fnInfo = lookupFnTypeInfo(val);
    if (fnInfo)
        recordReturnFnTypeInfo(fn, *fnInfo, fnNameForErrors);
}

std::string CodeGen::deriveNativePackage(const SourceLocation &loc) const {
    if (!sm_ || loc.file_id < 0 || loc.file_id >= sm_->getFileCount())
        return "";
    const std::string &fname = sm_->getFilename(loc.file_id);

    namespace fs = std::filesystem;
    fs::path p(fname);
    fs::path stem = p.stem();
    fs::path parent = p.parent_path();

    // /std/<mod>/<mod>.ry (directory module)
    if (parent.filename() == stem && parent.parent_path().filename() == "std")
        return stem.string();

    // /std/<mod>.ry (single-file module, excluding builtins.ry)
    if (parent.filename() == "std" && stem != "builtins")
        return stem.string();

    return "";
}

EnumVariantRegistry CodeGen::buildEnumVariantRegistry() const {
    EnumVariantRegistry reg;
    for (auto &[name, info] : enum_types_) {
        std::unordered_set<std::string> vars;
        for (auto &[vname, _] : info.variants)
            vars.insert(vname);
        reg[name] = std::move(vars);
    }
    return reg;
}

void CodeGen::registerResourceByTypeName(const std::string &typeName, llvm::Value *val) {
    int rk = ResourceKindRegistry::instance().lookupByTypeName(typeName);
    if (rk != ResourceKindRegistry::NONE) {
        addResourceKind(val, rk);
    }
}

void CodeGen::applyParamTypeMeta(const std::string &ptype,
                                  llvm::AllocaInst *alloca,
                                  llvm::Type *paramLLVMType,
                                  const std::string &paramName) {
    propagateTypeMeta(ptype, alloca);
    if (enum_types_.count(ptype))
        getOrCreateMeta(alloca).enum_value_type = ptype;
    registerResourceByTypeName(ptype, alloca);
    if (isCollectionTypeName(ptype))
        markArcManaged(alloca);
    {
        std::string resolvedPtype = resolveTypeAlias(ptype);
        if (isFunctionTypeName(resolvedPtype))
            getOrCreateMeta(alloca).fn_type_info = parseFnTypeAnnotation(resolvedPtype);
        auto constraint = parseTypeConstraint(resolvedPtype);
        if (constraint) {
            getOrCreateMeta(alloca).type_constraint = *constraint;
            llvm::Value *argVal = builder_.CreateLoad(
                paramLLVMType, alloca, paramName + ".load");
            emitConstraintCheck(argVal, *constraint, paramName);
        } else {
            if (isUnionType(resolvedPtype))
                storeFlattenedUnionMeta(alloca, ptype);
        }
    }
}

void CodeGen::emitStmt(AwaitStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;
    emitCoverage(s.loc);

    auto awaitExpr = std::make_unique<AwaitExpr>();
    awaitExpr->operand = std::move(s.operand);
    auto node = std::make_unique<ExprNode>();
    node->data = std::move(awaitExpr);
    node->loc = s.loc;
    (void)emitExpr(*node);
}

void CodeGen::emitStmt(ReturnStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;
    emitCoverage(s.loc);
    emitTraceReturn(s.loc);
    if (!s.value) {
        if (isAnyType(fn_->getReturnType())) {
            llvm::Value *unitAny = buildUnitAny();
            emitEnsureChecks(unitAny);
            emitScopeCleanupToDepth(0);
            emitTraceFunctionExit(current_function_name_, s.loc);
            builder_.CreateRet(unitAny);
        } else if (!fn_->getReturnType()->isVoidTy()) {
            codegenError("return without value in non-Unit function");
        } else {
            emitScopeCleanupToDepth(0);
            emitTraceFunctionExit(current_function_name_, s.loc);
            builder_.CreateRetVoid();
        }
    } else {
        // Build None with correct Option type from fn return type instead of
        // the default Option<int> that emitExpr(NoneExpr) would produce.
        llvm::Value *val;
        if (isNoneLiteral(*s.value) && isOptionType(fn_->getReturnType())) {
            val = buildNoneValue(fn_->getReturnType());
        } else {
            val = emitExpr(*s.value);

            // Propagate fn_type_info for function-type return values;
            // wrap raw function pointers as uniform closures so callers
            // can always treat function-typed returns uniformly.
            {
                auto *fnInfo = lookupFnTypeInfo(val);
                if (fnInfo) {
                    if (!fnInfo->isUniformClosure) {
                        val = wrapAsUniformClosure(val, *fnInfo);
                        fnInfo = lookupFnTypeInfo(val);
                    }
                }
            }

            // Tail call optimization: self-recursive tail call → musttail
            if (!current_postconditions_) {
                if (auto *ci = llvm::dyn_cast<llvm::CallInst>(val)) {
                    if (ci->getCalledFunction() == fn_) {
                        ci->setTailCallKind(llvm::CallInst::TCK_MustTail);
                        // Note: Do not emit function-exit tracing here; LLVM requires
                        // a musttail call to be immediately followed by the ret.
                        builder_.CreateRet(val);
                        llvm::BasicBlock *dead = llvm::BasicBlock::Create(*ctx_, "dead", fn_);
                        builder_.SetInsertPoint(dead);
                        return;
                    }
                }
            }

            llvm::Type *retTy = fn_->getReturnType();
            if (retTy->isVoidTy())
                codegenError("cannot return a value from Unit function '" +
                                         std::string(fn_->getName()) + "'");
            if (val->getType() != retTy) {
                std::string resolvedRetName = resolveTypeAlias(current_fn_return_type_);
                if (isAnyType(retTy)) {
                    val = wrapInAny(val);
                } else if (isAnyType(val->getType()) && !isAnyType(retTy) && canAnyHoldType(retTy)) {
                    val = unwrapFromAny(val, retTy);
                } else if (isAnyType(val->getType()) && !isAnyType(retTy) &&
                           llvm::isa<llvm::StructType>(retTy) &&
                           findRecordInfoForType(llvm::cast<llvm::StructType>(retTy))) {
                    // #1797: any → record unwrap on return.
                    val = unwrapFromAny(val, retTy,
                                        findRecordTypeName(
                                            llvm::cast<llvm::StructType>(retTy)));
                } else if (isUnionType(resolvedRetName)) {
                    val = wrapInUnion(val, resolvedRetName);
                } else if (auto *sliced = tryEmitSubtypeCoerce(val, retTy)) {
                    val = sliced;
                } else if (auto *coercedRet = coerceToLowLevelType(
                               val, retTy, resolvedRetName, "", "ret.coerce")) {
                    val = coercedRet;
                } else {
                    // Try tuple element coercion (e.g., Option<int> none → Option<Error>)
                    auto *retST = llvm::dyn_cast<llvm::StructType>(retTy);
                    auto *valST = llvm::dyn_cast<llvm::StructType>(val->getType());
                    const std::string mismatchMsg =
                        "return type mismatch: expected '" +
                        reverseResolveTypeName(retTy) + "', found '" +
                        reverseResolveTypeName(val->getType()) + "'";
                    if (!retST || !valST || retST->getNumElements() != valST->getNumElements())
                        codegenError(mismatchMsg);

                    // Find which elements need coercion
                    bool needsCoercion = false;
                    for (unsigned i = 0; i < retST->getNumElements(); ++i) {
                        if (valST->getElementType(i) != retST->getElementType(i)) {
                            if (!(isOptionType(valST->getElementType(i)) &&
                                  isOptionType(retST->getElementType(i))))
                                codegenError(mismatchMsg);
                            needsCoercion = true;
                        }
                    }
                    if (needsCoercion) {
                        llvm::Value *coerced = llvm::UndefValue::get(retTy);
                        for (unsigned i = 0; i < retST->getNumElements(); ++i) {
                            llvm::Value *elem = builder_.CreateExtractValue(val, i);
                            if (valST->getElementType(i) != retST->getElementType(i))
                                elem = buildNoneValue(retST->getElementType(i));
                            coerced = builder_.CreateInsertValue(coerced, elem, i);
                        }
                        val = coerced;
                    }
                }
            }

            // Snapshot only branch-stable dynamic return metadata. Writing the
            // full ValueMetadata onto fn_ would merge unrelated return-site
            // state (e.g. thread_result) and misapply it at every call site.
            recordReturnTypeMetaSnapshot(fn_, val, current_function_name_);
        }

        // Emit ensure checks (postconditions) before return
        emitEnsureChecks(val);

        // Retain return value before scope cleanup to prevent dangling pointer
        // when returning a value loaded from an ARC-managed local/parameter
        tryRetainArcSource(val);
        // Fn-typed param values are not in arc_managed_vars_; without this
        // the caller's releaseUniformClosureTemps frees the value while the
        // caller still holds the returned handle in a local. (#1770)
        retainFnTypedParamForReturn(val);
        emitScopeCleanupToDepth(0);

        emitTraceFunctionExit(current_function_name_, s.loc);
        builder_.CreateRet(val);
    }
    llvm::BasicBlock *deadBB = llvm::BasicBlock::Create(*ctx_, "dead", fn_);
    builder_.SetInsertPoint(deadBB);
}

// ===== Shared function declaration helpers =====

llvm::Type *CodeGen::tryResolveType(const std::string &typeName) {
    try {
        return resolveType(typeName);
    } catch (const DiagnosticError &) {
        return nullptr;
    }
}

llvm::Function *CodeGen::declareFunction(
    const std::string &name,
    std::vector<llvm::Type*> &paramTypes,
    std::vector<FnParam> &params,
    llvm::Type *exposedRetTy,
    const std::string &exposedReturnTypeName,
    const std::vector<Directive> &directives,
    const std::vector<ExprPtr> *preconditions,
    const std::vector<ExprPtr> *postconditions,
    const std::vector<std::string> *ensureBindings) {

    // Compute minArity and collect default values
    size_t newMinArity = 0;
    std::vector<ExprPtr> defaults;
    defaults.reserve(params.size());
    for (size_t i = 0; i < params.size(); ++i) {
        if (!params[i].default_value) newMinArity = i + 1;
        defaults.push_back(std::move(params[i].default_value));
    }
    size_t newMaxArity = paramTypes.size();

    // Determine registration target:
    // - Nested fns use fn_scope_stack_
    // - Qualified-imported user-defined module fns use the namespace bucket
    // - Otherwise the flat functions_ table
    bool isNested = fn_nesting_depth_ > 0 && !fn_scope_stack_.empty();
    bool isNamespaced = !isNested && current_namespace_target_ != nullptr;
    auto &overloads = isNested
        ? fn_scope_stack_.back()[name]
        : (isNamespaced
            ? current_namespace_target_->fn_overloads[name]
            : functions_[name]);

    // Check for duplicate/conflicting signatures
    for (auto &entry : overloads) {
        if (entry.paramTypes == paramTypes) {
            if (entry.func->getReturnType() == exposedRetTy)
                codegenError("fn '" + name +
                    "' is already defined with the same signature");
            else
                codegenError("fn '" + name +
                    "': overloads with same parameter types but different return types");
        }
        size_t overlapMin = std::max(newMinArity, entry.minArity);
        size_t overlapMax = std::min(newMaxArity, entry.paramTypes.size());
        for (size_t n = overlapMin; n <= overlapMax; ++n) {
            bool typesMatch = true;
            for (size_t i = 0; i < n; ++i) {
                if (paramTypes[i] != entry.paramTypes[i]) { typesMatch = false; break; }
            }
            if (typesMatch)
                codegenError("fn '" + name +
                    "' with default arguments creates ambiguous overload");
        }
    }

    // LLVM IR function name: nested functions get unique mangled names
    std::string irName;
    llvm::Function::LinkageTypes linkage;
    if (isNested) {
        irName = current_function_name_ + "." + name + "." + std::to_string(nested_fn_counter_++);
        linkage = llvm::Function::InternalLinkage;
    } else if (isNamespaced) {
        // Mangle namespace fns to avoid LLVM-level symbol collisions when
        // multiple user-defined modules export the same fn name.
        irName = current_namespace_target_->original_name + "__" + name;
        if (!overloads.empty())
            irName += "." + std::to_string(overloads.size());
        linkage = llvm::Function::InternalLinkage;
    } else {
        irName = name;
        if (!overloads.empty())
            irName = name + "." + std::to_string(overloads.size());
        linkage = llvm::Function::ExternalLinkage;
    }

    llvm::FunctionType *ft = llvm::FunctionType::get(exposedRetTy, paramTypes, false);
    llvm::Function *func = llvm::Function::Create(ft, linkage, irName, *mod_);

    std::vector<std::string> paramNames;
    paramNames.reserve(params.size());
    for (auto &p : params)
        paramNames.push_back(p.name);
    std::vector<std::string> paramTypeNames;
    paramTypeNames.reserve(params.size());
    for (auto &p : params)
        paramTypeNames.push_back(p.type->toString());
    overloads.push_back({func, paramTypes, paramNames, paramTypeNames, exposedReturnTypeName,
                         newMinArity, std::move(defaults),
                         preconditions, postconditions, ensureBindings,
                         {}, {}, {}, {}, {}});

    if (hasDirective(directives, "deprecated"))
        deprecated_functions_.insert(name);

    return func;
}

void CodeGen::applyInlineDirective(llvm::Function *func, const std::vector<Directive> &directives) {
    if (!hasDirective(directives, "inline")) return;
    std::string mode = "always";
    if (const ExprNode *modeExpr = getDirectiveNamedArg(directives, "inline", "mode")) {
        if (auto *s = std::get_if<StringExpr>(&modeExpr->data))
            mode = s->value;
        else
            codegenError("@inline 'mode' must be a string literal");
    }
    if (mode == "always")
        func->addFnAttr(llvm::Attribute::AlwaysInline);
    else if (mode == "never")
        func->addFnAttr(llvm::Attribute::NoInline);
    else if (mode == "hint")
        func->addFnAttr(llvm::Attribute::InlineHint);
    else
        codegenError("unknown @inline mode: '" + mode +
                     "' (expected 'always', 'never', or 'hint')");
}

// ===== Forward declaration shared logic =====

void CodeGen::forwardDeclareFunctionsInBody(std::vector<StmtNode> &stmts, bool validateOperatorReturn) {
    for (auto &stmt : stmts) {
        auto *fnPtr = std::get_if<std::unique_ptr<FnStmt>>(&stmt);
        if (!fnPtr) continue;
        auto &s = *fnPtr;
        if (s->loc.isValid()) current_loc_ = s->loc;

        if (!s->type_params.empty()) continue;
        if (hasDirective(s->directives, "native")) continue;
        if (!s->return_type) continue;

        std::vector<llvm::Type*> paramTypes;
        paramTypes.reserve(s->params.size());
        bool typesFailed = false;
        for (auto &p : s->params) {
            if (auto *ty = tryResolveType(p.type->toString()))
                paramTypes.push_back(ty);
            else { typesFailed = true; break; }
        }
        if (typesFailed) continue;
        llvm::Type *bodyRetTy = tryResolveType(s->return_type->toString());
        if (!bodyRetTy) continue;

        std::string returnTypeStr = s->return_type->toString();
        std::string exposedReturnTypeName = s->is_async ? "Task<" + returnTypeStr + ">" : returnTypeStr;
        llvm::Type *exposedRetTy = s->is_async ? resolveType(exposedReturnTypeName) : bodyRetTy;

        if (validateOperatorReturn && s->is_operator && isBoolConstrainedOperator(s->name)) {
            if (bodyRetTy != llvm::Type::getInt1Ty(*ctx_))
                codegenError("operator '" + operatorSymbol(s->name) + "' must return 'bool', but returns '" +
                             returnTypeStr + "'");
        }

        llvm::Function *func = declareFunction(
            s->name, paramTypes, s->params, exposedRetTy,
            exposedReturnTypeName, s->directives,
            &s->preconditions, &s->postconditions, &s->ensure_bindings);
        applyInlineDirective(func, s->directives);

        // For nested functions, run capture analysis and extend the LLVM function
        // signature with capture parameters. This must happen at forward-declaration
        // time so that sibling functions that call this function pass capture args.
        if (fn_nesting_depth_ > 0) {
            std::unordered_set<std::string> paramNameSet;
            for (auto &p : s->params)
                paramNameSet.insert(p.name);
            auto captures = analyzeFreeVariables(s->body, nullptr, paramNameSet, /*emitLoads=*/false);

            if (!captures.capturedNames.empty()) {
                // Build extended param types (user params + captured vars)
                std::vector<llvm::Type*> extParamTypes = paramTypes;
                for (auto *capTy : captures.capturedTypes)
                    extParamTypes.push_back(capTy);

                // Create new function with extended signature
                llvm::FunctionType *extFt = llvm::FunctionType::get(
                    func->getReturnType(), extParamTypes, false);
                llvm::Function *extFunc = llvm::Function::Create(
                    extFt, func->getLinkage(), func->getName().str(), *mod_);
                applyInlineDirective(extFunc, s->directives);

                // Safe to erase: no users exist at forward-declaration time
                func->eraseFromParent();

                // Update the OverloadEntry that declareFunction just created
                bool isNestedScope = fn_nesting_depth_ > 0 && !fn_scope_stack_.empty();
                auto &overloads = isNestedScope ? fn_scope_stack_.back()[s->name] : functions_[s->name];
                auto &entry = overloads.back();
                entry.func = extFunc;
                entry.capturedNames = std::move(captures.capturedNames);
                entry.capturedTypes = std::move(captures.capturedTypes);
                entry.capturedArcKinds = std::move(captures.capturedArcKinds);
                entry.capturedResourceKinds = std::move(captures.capturedResourceKinds);
                if (!captures.capturedClosureInfos.empty())
                    entry.capturedClosureInfos = std::make_unique<std::unordered_map<size_t, FnTypeInfo>>(std::move(captures.capturedClosureInfos));

                func = extFunc;
            }
        }

        forward_declared_fns_.insert(func);
    }
}

void CodeGen::forwardDeclareFunctions(Program &prog) {
    // First, forward-declare functions inside `QualifiedImportStmt::definitions`
    // (user-defined modules, #1730). Set up the namespace target so the
    // declarations land in `module_namespaces_[effective].fn_overloads` rather
    // than the flat `functions_` table. Without this, intra-module peer-fn
    // forward references would miss; and if a sibling `from <mod> import x`
    // re-extracts the same fn at the top level, the top-level entry must end
    // up in flat while the qimp-inner entry lives only in the namespace bucket.
    for (auto &stmt : prog) {
        auto *qp = std::get_if<std::unique_ptr<QualifiedImportStmt>>(&stmt);
        if (!qp) continue;
        auto &qs = **qp;
        if (qs.is_stdlib || qs.definitions.empty()) continue;
        // Bucket keyed by canonical module name (#1730) — multiple
        // imports of the same module share one bucket. Aliases register
        // an `effective_to_canonical_` redirect in `emitStmt`.
        const std::string &canonical = qs.module_name;
        auto [it, inserted] = module_namespaces_.try_emplace(canonical);
        if (inserted) {
            it->second.original_name = canonical;
            it->second.is_stdlib = qs.is_stdlib;
        }
        NamespaceEmitScope guard(*this, &it->second);
        forwardDeclareFunctionsInBody(qs.definitions, /*validateOperatorReturn=*/true);
    }
    forwardDeclareFunctionsInBody(prog, /*validateOperatorReturn=*/true);
}

void CodeGen::forwardDeclareNestedFunctions(std::vector<StmtNode> &body) {
    forwardDeclareFunctionsInBody(body, /*validateOperatorReturn=*/true);
}

// ===== Directive validation helper =====

void CodeGen::validateDirectives(const std::vector<Directive> &directives, DirectiveTarget current) {
    SourceLocation saved_loc = current_loc_;
    for (const auto &d : directives) {
        if (d.loc.isValid()) current_loc_ = d.loc;

        try {
            auto userIt = user_directive_registry_.find(d.name);
            if (userIt != user_directive_registry_.end()) {
                // Parser guarantees allowed_targets != 0 for user-defined
                // directives; the != 0 guard is defensive against a future
                // relaxation of that invariant.
                const auto &sig = userIt->second;
                if (sig.allowed_targets != 0 && !hasTarget(sig.allowed_targets, current))
                    continue;
                validateDirectiveSignature(d.name, d.args, sig);
            } else {
                validateDirectiveArgs(d.name, d.args);
            }
        } catch (const std::runtime_error &e) {
            codegenError(e.what());
        }
    }
    current_loc_ = saved_loc;
}

// ===== B5: FnStmt using FnScope RAII =====

void CodeGen::emitStmt(std::unique_ptr<FnStmt> &s) {
    if (s->loc.isValid()) current_loc_ = s->loc;

    validateDirectives(s->directives, DirectiveTarget::Function);

    // Directive-based test case: @it("description") on a named function.
    // Dispatch before coverage/trace so the inner emitStmt(s) call (after
    // directive stripping) handles those for the actual function definition.
    if (hasDirective(s->directives, "it")) {
        emitItDirective(s);
        return;
    }
    // Directive-based test group: @describe("group") on a named function
    if (hasDirective(s->directives, "describe")) {
        emitDescribeDirective(s);
        return;
    }
    // Lifecycle hooks (#1686). At normal codegen these are pre-scanned out
    // by emitDescribeDirective; reaching this dispatch means the hook fn is
    // outside an @describe and the handler emits a clear diagnostic.
    if (hasDirective(s->directives, "beforeEach")) {
        emitBeforeEachDirective(s);
        return;
    }
    if (hasDirective(s->directives, "afterEach")) {
        emitAfterEachDirective(s);
        return;
    }
    if (hasDirective(s->directives, "beforeAll")) {
        emitBeforeAllDirective(s);
        return;
    }
    if (hasDirective(s->directives, "afterAll")) {
        emitAfterAllDirective(s);
        return;
    }

    emitCoverage(s->loc);
    emitTraceSymbolDefine("fn", s->name, s->loc);

    // Qualified import of user-defined module: reject generic fns (Phase 2)
    if (current_namespace_target_ && !s->type_params.empty()) {
        codegenError("generic functions in qualified-imported user-defined modules are not yet supported; use 'from <mod> import <fn>' instead");
    }

    // Generic function: save as template, don't instantiate yet
    if (!s->type_params.empty()) {
        for (auto &p : s->params) {
            if (p.default_value)
                codegenError("default arguments are not yet supported in generic functions");
        }
        GenericFnTemplate tmpl;
        std::string name = s->name;
        tmpl.fnStmt = std::move(s);
        generic_fn_templates_[name] = std::move(tmpl);
        return;
    }

    if (hasDirective(s->directives, "native")) {
        if (s->is_async)
            codegenError("async native functions are not supported");
        if (hasDirective(s->directives, "inline"))
            codegenError("@inline cannot be used with @native fn");
        if (hasDirective(s->directives, "deprecated"))
            deprecated_functions_.insert(s->name);

        // Validate return type for comparison/logical operators
        if (s->is_operator && isBoolConstrainedOperator(s->name)) {
            std::string retStr = s->return_type ? s->return_type->toString() : "";
            if (!retStr.empty() && retStr != "bool") {
                codegenError("operator '" + operatorSymbol(s->name) + "' must return 'bool', but returns '" +
                             retStr + "'");
            }
        }

        // Build rich signature
        NativeFnSignature sig;
        sig.name = s->name;
        sig.package = deriveNativePackage(s->loc);
        sig.library = getDirectivePositionalArg(s->directives, "native");
        sig.params.reserve(s->params.size());

        for (auto &p : s->params)
            sig.params.push_back({p.name, p.type ? p.type->toString() : ""});
        sig.returnTypeName = s->return_type ? s->return_type->toString() : "Unit";
        for (auto &d : s->directives)
            sig.directiveNames.push_back(d.name);

        // For registry key: use library name as the effective module when the
        // source file is not under std/<mod>/. This ensures @native("base64")
        // fn encode(...) in user code registers under "base64::encode",
        // matching what the table-driven dispatchers expect.
        std::string effectivePackage = sig.package.empty() && !sig.library.empty()
            ? sig.library : sig.package;
        // Populate secondary index for O(1) lookup in emitGenericNativeCall
        if (!sig.library.empty()) {
            auto &libs = native_lib_index_[sig.name];
            if (std::find(libs.begin(), libs.end(), sig.library) == libs.end())
                libs.push_back(sig.library);
        }

        // Deduplicate: skip if an identical signature (same arity AND types)
        // already exists under this key. This prevents duplicate entries when
        // a user declares @native("base64") fn encode(str) alongside
        // `from base64 import encode`, while preserving valid type-based
        // overloads like encode(str) and encode(int).
        auto &sigVec = native_fn_sigs_[nativeSigKey(effectivePackage, sig.name)];
        bool duplicate = false;
        for (const auto &existing : sigVec) {
            if (existing.params.size() != sig.params.size()) continue;
            bool same = true;
            for (size_t i = 0; i < sig.params.size(); ++i) {
                if (existing.params[i].typeName != sig.params[i].typeName) {
                    same = false;
                    break;
                }
            }
            if (same) { duplicate = true; break; }
        }
        if (!duplicate)
            sigVec.push_back(std::move(sig));
        return;
    }

    std::vector<llvm::Type*> paramTypes;
    paramTypes.reserve(s->params.size());
    for (auto &p : s->params)
        paramTypes.push_back(resolveType(p.type->toString()));

    // Check if this function was already forward-declared
    llvm::Function *func = nullptr;
    auto *fwdOverloads = findFunction(s->name);
    if (fwdOverloads) {
        for (auto &entry : *fwdOverloads) {
            if (forward_declared_fns_.count(entry.func) &&
                paramTypes == entry.paramTypes) {
                func = entry.func;
                forward_declared_fns_.erase(func);
                break;
            }
        }
    }

    llvm::Type *bodyRetTy;
    if (!s->return_type) {
        // Infer return type from body
        std::unordered_map<std::string, llvm::Type*> paramTypeMap;
        for (auto &p : s->params)
            paramTypeMap[p.name] = resolveType(p.type->toString());
        buildLocalTypeMap(s->body, paramTypeMap);
        std::vector<llvm::Type*> retTypes;
        collectReturnTypes(s->body, paramTypeMap, retTypes);
        bodyRetTy = deduceReturnType(retTypes);
        // Build return type name string
        // Deduplicate for name construction
        std::vector<llvm::Type*> unique;
        unique.reserve(retTypes.size());
        for (auto *ty : retTypes)
            if (std::find(unique.begin(), unique.end(), ty) == unique.end())
                unique.push_back(ty);
        if (unique.size() <= 1) {
            s->return_type = TypeNode::makeBasic(reverseResolveTypeName(bodyRetTy));
        } else {
            std::vector<TypeNodePtr> comps;
            comps.reserve(unique.size());
            for (auto *ty : unique)
                comps.push_back(TypeNode::makeBasic(reverseResolveTypeName(ty)));
            s->return_type = TypeNode::makeUnion(std::move(comps));
        }
    } else {
        bodyRetTy = resolveType(s->return_type->toString());
    }

    std::string returnTypeStr = s->return_type->toString();

    // Validate return type for comparison/logical operators (skip if already done in forward declaration)
    if (!func && s->is_operator && isBoolConstrainedOperator(s->name)) {
        if (bodyRetTy != llvm::Type::getInt1Ty(*ctx_)) {
            codegenError("operator '" + operatorSymbol(s->name) + "' must return 'bool', but returns '" +
                         returnTypeStr + "'");
        }
    }

    // Check that non-Unit, non-any functions return on all paths
    if (!isAnyType(bodyRetTy) && !bodyRetTy->isVoidTy()
        && !hasDirective(s->directives, "native")) {
        if (!allPathsReturn(s->body, buildEnumVariantRegistry()))
            codegenError("fn '" + s->name + "' with return type '" +
                         returnTypeStr + "' does not return a value on all code paths");
    }
    std::string exposedReturnTypeName = s->is_async ? "Task<" + returnTypeStr + ">" : returnTypeStr;
    llvm::Type *exposedRetTy = s->is_async ? resolveType(exposedReturnTypeName) : bodyRetTy;

    // If not forward-declared, do the full declaration now
    if (!func) {
        func = declareFunction(
            s->name, paramTypes, s->params, exposedRetTy,
            exposedReturnTypeName, s->directives,
            &s->preconditions, &s->postconditions, &s->ensure_bindings);
        applyInlineDirective(func, s->directives);
    }

    std::vector<std::string> paramTypeNames;
    paramTypeNames.reserve(s->params.size());
    for (auto &p : s->params)
        paramTypeNames.push_back(p.type->toString());

    // Capture analysis for nested functions.
    // Re-run analysis (without emitting loads) to get metadata for body emission.
    // If the LLVM function doesn't yet have capture params (e.g. forward-declared
    // before local vars were in scope), extend the signature now.
    CaptureAnalysisResult captures;
    if (fn_nesting_depth_ > 0) {
        std::unordered_set<std::string> paramNameSet;
        for (auto &p : s->params)
            paramNameSet.insert(p.name);
        captures = analyzeFreeVariables(s->body, nullptr, paramNameSet, /*emitLoads=*/false);

        if (!captures.capturedNames.empty()) {
            if (s->is_async)
                codegenError("captured nested async fns are not yet supported (fn '" +
                    s->name + "' captures variables from the enclosing scope)");

            size_t expectedArgCount = s->params.size() + captures.capturedNames.size();
            if (func->arg_size() != expectedArgCount) {
                // Function signature needs extension (captures not yet in signature).
                if (!func->use_empty())
                    codegenError("nested function '" + s->name +
                        "' captures variables from the enclosing scope but is called "
                        "from a sibling function before its definition; move '" +
                        s->name + "' before the calling function, or restructure the code");

                std::vector<llvm::Type*> extParamTypes = paramTypes;
                for (auto *capTy : captures.capturedTypes)
                    extParamTypes.push_back(capTy);

                llvm::FunctionType *extFt = llvm::FunctionType::get(
                    func->getReturnType(), extParamTypes, false);
                llvm::Function *extFunc = llvm::Function::Create(
                    extFt, func->getLinkage(), func->getName().str(), *mod_);
                applyInlineDirective(extFunc, s->directives);
                func->eraseFromParent();

                // Update the OverloadEntry
                bool isNestedScope = fn_nesting_depth_ > 0 && !fn_scope_stack_.empty();
                auto &overloads = isNestedScope ? fn_scope_stack_.back()[s->name] : functions_[s->name];
                bool entryUpdated = false;
                for (auto &entry : overloads) {
                    if (entry.paramTypes == paramTypes) {
                        entry.func = extFunc;
                        entry.capturedNames = captures.capturedNames;
                        entry.capturedTypes = captures.capturedTypes;
                        entry.capturedArcKinds = captures.capturedArcKinds;
                        entry.capturedResourceKinds = captures.capturedResourceKinds;
                        if (!captures.capturedClosureInfos.empty())
                            entry.capturedClosureInfos = std::make_unique<std::unordered_map<size_t, FnTypeInfo>>(captures.capturedClosureInfos);
                        entryUpdated = true;
                        break;
                    }
                }
                if (!entryUpdated)
                    codegenError("internal error: no matching OverloadEntry for nested function '" + s->name + "'");
                func = extFunc;
            }
        }
    }

    auto emitFunctionBody = [&](llvm::Function *targetFunc, llvm::Type *retTy,
                                const std::string &returnTypeName, const std::string &fnNameForErrors) {
        FnScope guard(*this);
        fn_ = targetFunc;
        current_fn_return_type_ = returnTypeName;
        current_function_name_ = s->name;
        pushScope();

        llvm::BasicBlock *entry = llvm::BasicBlock::Create(*ctx_, "entry", targetFunc);
        builder_.SetInsertPoint(entry);
        emitTraceFunctionEnter(s->name, s->loc);

        // Set up user parameters BEFORE forward-declaring nested functions,
        // so that capture analysis during forward declaration can find outer params.
        unsigned idx = 0;
        for (auto &arg : targetFunc->args()) {
            if (idx < s->params.size()) {
                arg.setName(s->params[idx].name);
                llvm::AllocaInst *alloca = builder_.CreateAlloca(
                    paramTypes[idx], nullptr, s->params[idx].name);
                builder_.CreateStore(&arg, alloca);
                scope_stack_.back()[s->params[idx].name] = alloca;
                const std::string &ptype = paramTypeNames[idx];
                applyParamTypeMeta(ptype, alloca, paramTypes[idx], s->params[idx].name);
            } else {
                emitCapturedParamSetup(arg, captures, idx - s->params.size());
            }
            ++idx;
        }

        // Forward-declare nested functions for mutual recursion within this body.
        // Must come after user params are in scope so capture analysis can find them.
        forwardDeclareNestedFunctions(s->body);

        // Emit require checks (preconditions)
        for (size_t i = 0; i < s->preconditions.size(); ++i)
            emitContractCheck("require", s->name, s->preconditions[i]);

        // Set up postcondition context
        current_postconditions_ = s->postconditions.empty() ? nullptr : &s->postconditions;
        ensure_bindings_ = s->ensure_bindings.empty() ? nullptr : &s->ensure_bindings;

        for (auto &stmt : s->body)
            std::visit([this](auto &st) { emitStmt(st); }, stmt);

        if (!builder_.GetInsertBlock()->getTerminator()) {
            llvm::Value *defaultRet = nullptr;
            if (retTy->isVoidTy()) {
                // no default value needed
            } else if (retTy == i64Ty_) {
                defaultRet = llvm::ConstantInt::get(i64Ty_, 0);
            } else if (retTy == i8Ty_) {
                defaultRet = llvm::ConstantInt::get(i8Ty_, 0);
            } else if (retTy == f64Ty_) {
                defaultRet = llvm::ConstantFP::get(f64Ty_, 0.0);
            } else if (retTy == i1Ty_) {
                defaultRet = llvm::ConstantInt::get(i1Ty_, 0);
            } else if (retTy == ptrTy_) {
                defaultRet = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));
            } else if (isAnyType(retTy)) {
                defaultRet = buildUnitAny();
            } else if (llvm::isa<llvm::StructType>(retTy)) {
                defaultRet = llvm::UndefValue::get(retTy);
            }

            // Emit ensure checks on implicit return path
            if (defaultRet)
                emitEnsureChecks(defaultRet);

            if (retTy->isVoidTy()) {
                emitTraceFunctionExit(s->name, s->loc);
                builder_.CreateRetVoid();
            } else if (defaultRet) {
                emitTraceFunctionExit(s->name, s->loc);
                builder_.CreateRet(defaultRet);
            } else {
                emitTraceFunctionExit(s->name, s->loc);
                builder_.CreateRet(llvm::UndefValue::get(retTy));
            }
        }

        std::string err;
        llvm::raw_string_ostream errStream(err);
        if (llvm::verifyFunction(*targetFunc, &errStream))
            codegenError("IR verify error in function '" + fnNameForErrors + "': " + err);
    };

    if (!s->is_async) {
        emitFunctionBody(func, bodyRetTy, returnTypeStr, s->name);
        return;
    }

    std::string funcIrName = func->getName().str();
    llvm::FunctionType *bodyFt = llvm::FunctionType::get(bodyRetTy, paramTypes, false);
    llvm::Function *bodyFunc = llvm::Function::Create(
        bodyFt, llvm::Function::InternalLinkage, funcIrName + ".__async_body", *mod_);
    emitFunctionBody(bodyFunc, bodyRetTy, returnTypeStr, s->name);

    std::vector<llvm::Type*> envFields = paramTypes;
    if (envFields.empty())
        envFields.push_back(i8Ty_);
    llvm::StructType *envTy = llvm::StructType::get(*ctx_, envFields);

    llvm::FunctionType *thunkTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {ptrTy_, ptrTy_}, false);
    llvm::Function *thunk = llvm::Function::Create(
        thunkTy, llvm::Function::InternalLinkage,
        "__ry_async." + std::to_string(lambda_counter_++), *mod_);

    // FnScope destructor restores fn_, scope_stack_, immutable_scope_stack_, builder_ insert point,
    // and contract state (current_postconditions_, result_alloca_, in_ensure_context_, old_value_map_)

    {
        FnScope guard(*this);
        fn_ = thunk;
        llvm::BasicBlock *entry = llvm::BasicBlock::Create(*ctx_, "entry", thunk);
        builder_.SetInsertPoint(entry);

        auto argIt = thunk->arg_begin();
        llvm::Value *envRaw = &*argIt++;
        envRaw->setName("env_raw");
        llvm::Value *outRaw = &*argIt;
        outRaw->setName("out_raw");

        llvm::Value *typedEnv = builder_.CreateBitCast(envRaw, ptrTy_, "async_env_typed");
        std::vector<llvm::Value*> thunkArgs;
        thunkArgs.reserve(paramTypes.size());
        for (size_t i = 0; i < paramTypes.size(); ++i) {
            llvm::Value *argField = builder_.CreateStructGEP(
                envTy, typedEnv, static_cast<unsigned>(i), "async_arg_field." + std::to_string(i));
            thunkArgs.push_back(builder_.CreateLoad(paramTypes[i], argField, "async_arg." + std::to_string(i)));
        }

        llvm::Value *result = builder_.CreateCall(bodyFunc, thunkArgs, bodyRetTy->isVoidTy() ? "" : "async_result");
        if (!bodyRetTy->isVoidTy()) {
            llvm::Value *outTyped = builder_.CreateBitCast(outRaw, ptrTy_, "async_out_typed");
            builder_.CreateStore(result, outTyped);
        }
        builder_.CreateRetVoid();
    }

    {
        FnScope guard(*this);
        fn_ = func;
        current_fn_return_type_ = exposedReturnTypeName;
        llvm::BasicBlock *entry = llvm::BasicBlock::Create(*ctx_, "entry", func);
        builder_.SetInsertPoint(entry);

        llvm::FunctionType *mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
        llvm::FunctionCallee mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
        const llvm::DataLayout &dl = mod_->getDataLayout();
        llvm::Value *envPtr = builder_.CreateCall(
            mallocFn,
            {llvm::ConstantInt::get(i64Ty_, std::max<uint64_t>(1, dl.getTypeAllocSize(envTy)))},
            "async_env");

        if (paramTypes.empty()) {
            llvm::Value *dummyField = builder_.CreateStructGEP(envTy, envPtr, 0, "async_env_dummy");
            builder_.CreateStore(llvm::ConstantInt::get(i8Ty_, 0), dummyField);
        } else {
            size_t idx = 0;
            for (auto &arg : func->args()) {
                llvm::Value *argField = builder_.CreateStructGEP(
                    envTy, envPtr, static_cast<unsigned>(idx++), "async_env_arg");
                builder_.CreateStore(&arg, argField);
            }
        }

        llvm::FunctionType *spawnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, i64Ty_}, false);
        llvm::FunctionCallee spawnFn = mod_->getOrInsertFunction("__ry_task_spawn", spawnTy);
        llvm::Value *task = builder_.CreateCall(
            spawnFn,
            {
                builder_.CreateBitCast(thunk, ptrTy_),
                envPtr,
                llvm::ConstantInt::get(i64Ty_, bodyRetTy->isVoidTy() ? static_cast<uint64_t>(0) : static_cast<uint64_t>(dl.getTypeAllocSize(bodyRetTy)))
            },
            "task");
        builder_.CreateRet(task);

        std::string err;
        llvm::raw_string_ostream errStream(err);
        if (llvm::verifyFunction(*func, &errStream))
            codegenError("IR verify error in function '" + s->name + "': " + err);
    }
}

} // namespace ry
