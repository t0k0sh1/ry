#include "ry/codegen.hpp"
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>


namespace ry {

// ===== Contract helpers =====

void CodeGen::emitContractCheck(const std::string &kind, const std::string &fn_name,
                                 const ExprPtr &cond) {
    llvm::Value *condVal = emitExpr(*cond);
    condVal = toBool(condVal);

    std::string errName = ".contract_err_" + std::to_string(contract_err_counter_++);
    std::string suffix = (kind == "invariant") ? "" : "()";
    std::string preposition = (kind == "invariant") ? " for " : " in ";
    std::string msg = "Contract violation: " + kind + " failed" + preposition + fn_name + suffix + "\n";

    llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, kind + ".fail", fn_);
    llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, kind + ".ok", fn_);

    builder_.CreateCondBr(condVal, nextBB, failBB);

    builder_.SetInsertPoint(failBB);
    emitRuntimeError(msg, errName);

    builder_.SetInsertPoint(nextBB);
}

void CodeGen::emitInvariantCheck(const std::string &typeName, const RecordInfo &info,
                                  llvm::Value *recordVal) {
    if (info.invariants.empty() && info.parentName.empty()) return;

    std::vector<std::pair<std::string, const ExprPtr*>> allInvariants;
    allInvariants.reserve(8);
    for (auto &inv : info.invariants)
        allInvariants.push_back({typeName, &inv});
    const std::string *parent = &info.parentName;
    while (!parent->empty()) {
        auto pit = record_types_.find(*parent);
        if (pit == record_types_.end()) break;
        for (auto &inv : pit->second.invariants)
            allInvariants.push_back({*parent, &inv});
        parent = &pit->second.parentName;
    }
    if (allInvariants.empty()) return;

    pushScope();
    for (unsigned f = 0; f < info.fields.size(); ++f) {
        llvm::Type *fieldTy = info.llvmType->getElementType(f);
        llvm::AllocaInst *fieldAlloca = builder_.CreateAlloca(fieldTy, nullptr, info.fields[f].name);
        llvm::Value *fieldVal = builder_.CreateExtractValue(recordVal, f, info.fields[f].name + "_val");
        builder_.CreateStore(fieldVal, fieldAlloca);
        scope_stack_.back()[info.fields[f].name] = fieldAlloca;
    }
    for (auto &[name, inv] : allInvariants)
        emitContractCheck("invariant", name, *inv);
    popScope();
}

void CodeGen::emitEnsureChecks(llvm::Value *retVal) {
    if (!current_postconditions_ || current_postconditions_->empty() || !ensure_bindings_)
        return;
    pushScope();
    auto &bindings = *ensure_bindings_;
    if (bindings.size() == 1) {
        llvm::AllocaInst *alloca = builder_.CreateAlloca(retVal->getType(), nullptr, bindings[0]);
        builder_.CreateStore(retVal, alloca);
        scope_stack_.back()[bindings[0]] = alloca;
        immutable_scope_stack_.back().insert(bindings[0]);
    } else {
        auto *structTy = llvm::dyn_cast<llvm::StructType>(retVal->getType());
        if (!structTy || !structTy->isLiteral() || structTy->getNumElements() != bindings.size())
            codegenError("ensure destructuring requires tuple return; binding count does not match tuple element count");
        for (unsigned i = 0; i < bindings.size(); ++i) {
            llvm::Value *elem = builder_.CreateExtractValue(retVal, i);
            llvm::AllocaInst *alloca = builder_.CreateAlloca(elem->getType(), nullptr, bindings[i]);
            builder_.CreateStore(elem, alloca);
            scope_stack_.back()[bindings[i]] = alloca;
            immutable_scope_stack_.back().insert(bindings[i]);
        }
    }
    in_ensure_context_ = true;
    std::string fnName = fn_->getName().str();
    for (size_t i = 0; i < current_postconditions_->size(); ++i)
        emitContractCheck("ensure", fnName, (*current_postconditions_)[i]);
    in_ensure_context_ = false;
    popScope();
}

void CodeGen::validateTypeBounds(const std::vector<TypeParam> &typeParams,
                                  const std::vector<std::string> &typeArgs,
                                  const std::string &context) {
    for (size_t i = 0; i < typeParams.size(); ++i) {
        if (!typeParams[i].bound) continue;
        const std::string &bound = *typeParams[i].bound;
        const std::string &concrete = typeArgs[i];
        const std::string resolvedBound = resolveTypeAlias(bound);
        const std::string resolvedConcrete = resolveTypeAlias(concrete);

        if (!record_types_.count(resolvedBound))
            codegenError("unknown type constraint: '" + bound + "'");

        if (resolvedConcrete != resolvedBound && !isSubtypeOf(resolvedConcrete, resolvedBound)) {
            std::string msg = "type '";
            msg += concrete;
            msg += "' does not satisfy constraint '";
            msg += bound;
            msg += "' (";
            msg += context;
            msg += ")";
            codegenError(msg);
        }
    }
}

void CodeGen::instantiateGenericEnum(const std::string &fullName, const std::string &baseName,
                                      const std::vector<std::string> &typeArgs) {
    if (enum_types_.count(fullName))
        return; // already instantiated

    auto it = generic_enum_templates_.find(baseName);
    if (it == generic_enum_templates_.end())
        codegenError("unknown generic enum: " + baseName);

    auto &tmpl = it->second;
    if (typeArgs.size() != tmpl.typeParams.size())
        codegenError("generic enum '" + baseName + "' expects " +
            std::to_string(tmpl.typeParams.size()) + " type parameters");

    validateTypeBounds(tmpl.typeParams, typeArgs, "in generic enum '" + baseName + "'");

    // Create a concrete EnumStmt by substituting type parameters.
    // Publish the bindings through `type_param_scope_` so `resolveType` can
    // rewrite nested generic args like `Inner<T>` → `Inner<int>` via
    // `substituteTypeParamsInName`. Mirrors the save/populate/restore dance
    // in `instantiateGenericFn` above.
    auto savedScope = std::move(type_param_scope_);
    type_param_scope_.clear();
    for (size_t i = 0; i < tmpl.typeParams.size(); ++i)
        type_param_scope_[tmpl.typeParams[i].name] = typeArgs[i];

    EnumInfo info;
    info.name = fullName;
    info.variantCount = tmpl.variants.size();
    info.type_id = next_type_id_++;

    bool hasADT = false;
    std::vector<llvm::Constant*> nameStrings;
    nameStrings.reserve(tmpl.variants.size());
    for (size_t i = 0; i < tmpl.variants.size(); ++i) {
        auto &v = tmpl.variants[i];
        info.variants[v.name] = static_cast<int64_t>(i);
        info.variantOrder.push_back(v.name);
        llvm::Constant *str = cachedGlobalString(
            v.name, ".enum_" + fullName + "_" + v.name);
        nameStrings.push_back(str);

        if (!v.field_types.empty()) {
            hasADT = true;
            VariantFieldInfo vfi;
            for (auto &ft : v.field_types) {
                std::string resolved = substituteTypeParamsInName(ft->toString());
                vfi.fieldTypes.push_back(resolveType(resolved));
                vfi.fieldTypeNames.push_back(resolved);
            }
            info.variantFields[v.name] = std::move(vfi);
        }
    }
    info.isADT = hasADT;

    type_param_scope_ = std::move(savedScope);

    auto *arrTy = llvm::ArrayType::get(ptrTy_, tmpl.variants.size());
    auto *init = llvm::ConstantArray::get(arrTy, nameStrings);
    auto *gv = new llvm::GlobalVariable(
        *mod_, arrTy, true, llvm::GlobalValue::PrivateLinkage,
        init, ".enum_names_" + fullName);
    info.nameArray = gv;

    if (hasADT) {
        const llvm::DataLayout &dl = mod_->getDataLayout();
        size_t maxPayload = 0;
        for (auto &[vname, vfi] : info.variantFields) {
            size_t payloadSize = 0;
            for (auto *ty : vfi.fieldTypes) {
                uint64_t align = dl.getABITypeAlign(ty).value();
                payloadSize = (payloadSize + align - 1) / align * align;
                payloadSize += dl.getTypeAllocSize(ty);
            }
            if (payloadSize > maxPayload) maxPayload = payloadSize;
        }
        info.maxPayloadSize = maxPayload;
        llvm::Type *payloadTy = llvm::ArrayType::get(
            llvm::Type::getInt8Ty(*ctx_), maxPayload > 0 ? maxPayload : 1);
        info.adtType = llvm::StructType::create(
            *ctx_, {i64Ty_, payloadTy}, "enum." + fullName);
    }

    enum_types_[fullName] = std::move(info);
}

// ===== Generic function type inference =====

std::string CodeGen::reverseResolveType(llvm::Value *val) {
    // Check low-level type metadata first (resolves ambiguous LLVM types)
    std::string llName = getLowLevelTypeName(val);
    if (!llName.empty()) return llName;

    llvm::Type *ty = val->getType();
    if (ty == i64Ty_) return "int";
    if (ty == f64Ty_) return "float";
    if (ty == i1Ty_)  return "bool";
    if (ty == i8Ty_)  return "u8";
    if (ty == i16Ty_) return "i16";
    if (ty == i32Ty_) return "i32";
    if (ty == f32Ty_) return "f32";
    if (ty == typeTy_) return "Type";
    if (isAnyType(ty)) return "any";

    if (ty == ptrTy_) {
        // Look through LoadInst to find metadata on the underlying alloca
        if (auto *elemTy = getTypeMeta(TypeMeta::ListElem, val))
            return "List<" + reverseResolveType(
                llvm::UndefValue::get(elemTy)) + ">";

        auto *meta = getMeta(val);
        if (meta && meta->fn_type_info) {
            auto &fti = *meta->fn_type_info;
            std::string result = "fn(";
            for (size_t i = 0; i < fti.paramTypeNames.size(); ++i) {
                if (i > 0) result += ",";
                result += fti.paramTypeNames[i];
            }
            result += ")";
            if (fti.returnType && !fti.returnType->isVoidTy())
                result += " -> " + reverseResolveType(
                    llvm::UndefValue::get(fti.returnType));
            return result;
        }
        return "str";
    }

    if (auto *st = llvm::dyn_cast<llvm::StructType>(ty)) {
        std::string n = findRecordTypeName(st);
        if (!n.empty()) return n;
    }

    return "any";
}

// ===== Helpers for structural type-argument unification =====

// Trim ASCII whitespace from both ends. Used by the type-name string
// splitters below which consume output of `TypeNode::toString()` and
// `inferExprTypeName` — both produce human-readable strings with
// optional spaces after commas.
static std::string trimWs(const std::string &s) {
    size_t a = 0, b = s.size();
    while (a < b && std::isspace(static_cast<unsigned char>(s[a]))) ++a;
    while (b > a && std::isspace(static_cast<unsigned char>(s[b - 1]))) --b;
    return s.substr(a, b - a);
}

// Split `body` on top-level commas, honoring nesting of <, >, (, ),
// [, ], and the lexer-compound tokens `>>` / `>>>` (which can appear
// when `toString()` emits nested generics).
static std::vector<std::string> splitTopLevelCommas(const std::string &body) {
    std::vector<std::string> out;
    out.reserve(static_cast<size_t>(std::count(body.begin(), body.end(), ',')) + 1);
    int depth = 0;
    size_t start = 0;
    for (size_t i = 0; i < body.size(); ++i) {
        char c = body[i];
        if (c == '<' || c == '(' || c == '[') depth++;
        else if (c == '>' || c == ')' || c == ']') {
            // Clamp: malformed input with unmatched closers should not
            // drop `depth` below zero and start splitting mid-group.
            if (depth > 0) depth--;
        }
        else if (c == ',' && depth == 0) {
            out.push_back(trimWs(body.substr(start, i - start)));
            start = i + 1;
        }
    }
    out.push_back(trimWs(body.substr(start)));
    return out;
}

bool CodeGen::splitGenericTypeName(const std::string &s,
                                    std::string &head,
                                    std::vector<std::string> &inner) {
    std::string t = trimWs(s);
    size_t lt = t.find('<');
    if (lt == std::string::npos) return false;
    if (t.back() != '>') return false;
    head = trimWs(t.substr(0, lt));
    std::string body = t.substr(lt + 1, t.size() - lt - 2);
    inner = splitTopLevelCommas(body);
    return true;
}

static bool splitTupleTypeName(const std::string &s,
                                std::vector<std::string> &elements) {
    std::string t = trimWs(s);
    if (t.size() < 2 || t.front() != '(' || t.back() != ')') return false;
    std::string body = t.substr(1, t.size() - 2);
    // Single-element tuples spell as "(x,)" — drop the empty trailing slice.
    elements = splitTopLevelCommas(body);
    if (!elements.empty() && elements.back().empty())
        elements.pop_back();
    return true;
}

static bool splitFunctionTypeName(const std::string &s,
                                   std::vector<std::string> &params,
                                   std::string &returnType) {
    std::string t = trimWs(s);
    std::string prefix;
    if (t.rfind("fn(", 0) == 0) {
        prefix = "fn(";
    } else if (t.rfind("function(", 0) == 0) {
        prefix = "function(";
    } else {
        return false;
    }
    int depth = 1;
    size_t i = prefix.size();
    for (; i < t.size() && depth > 0; ++i) {
        if (t[i] == '(') depth++;
        else if (t[i] == ')') depth--;
        if (depth == 0) break;
    }
    if (depth != 0) return false;
    std::string body = t.substr(prefix.size(), i - prefix.size());
    params = splitTopLevelCommas(body);
    if (params.size() == 1 && params[0].empty()) params.clear();
    size_t j = i + 1;
    while (j < t.size() && std::isspace(static_cast<unsigned char>(t[j]))) ++j;
    if (j < t.size() && t[j] == '-' && j + 1 < t.size() && t[j + 1] == '>') {
        j += 2;
        while (j < t.size() && std::isspace(static_cast<unsigned char>(t[j]))) ++j;
        returnType = trimWs(t.substr(j));
    } else {
        returnType.clear();
    }
    return true;
}

void CodeGen::mergeInferredBinding(
    std::unordered_map<std::string, std::string> &inferred,
    const std::string &paramName,
    const std::string &resolved,
    const std::string &fnName) {
    if (resolved.empty()) return;
    auto it = inferred.find(paramName);
    if (it == inferred.end()) {
        inferred[paramName] = resolved;
        return;
    }
    if (it->second != resolved) {
        codegenError("conflicting type inference for '" + paramName +
                     "' in call to generic function '" + fnName + "': '" +
                     it->second + "' vs '" + resolved + "'");
    }
}

bool CodeGen::unifyTypeParam(
    const TypeNode &paramType,
    const std::string &argTypeName,
    const std::unordered_set<std::string> &typeParamSet,
    std::unordered_map<std::string, std::string> &inferred,
    const std::string &fnName) {
    if (argTypeName.empty()) return false;

    return std::visit([&](const auto &v) -> bool {
        using T = std::decay_t<decltype(v)>;
        if constexpr (std::is_same_v<T, BasicType>) {
            if (typeParamSet.count(v.name)) {
                mergeInferredBinding(inferred, v.name, argTypeName, fnName);
                return true;
            }
            // Concrete leaf — no binding to produce. We do not emit an
            // error on mismatch here because the existing callee
            // dispatch will catch shape errors later with a clearer
            // call-site diagnostic.
            return false;
        } else if constexpr (std::is_same_v<T, GenericType>) {
            std::string head;
            std::vector<std::string> innerArgs;
            if (!splitGenericTypeName(argTypeName, head, innerArgs))
                return false;
            if (head != v.name) return false;
            if (innerArgs.size() != v.type_args.size()) return false;
            bool any = false;
            for (size_t k = 0; k < v.type_args.size(); ++k)
                if (unifyTypeParam(*v.type_args[k], innerArgs[k],
                                   typeParamSet, inferred, fnName))
                    any = true;
            return any;
        } else if constexpr (std::is_same_v<T, TupleType>) {
            std::vector<std::string> elems;
            if (!splitTupleTypeName(argTypeName, elems)) return false;
            if (elems.size() != v.elements.size()) return false;
            bool any = false;
            for (size_t k = 0; k < v.elements.size(); ++k)
                if (unifyTypeParam(*v.elements[k], elems[k],
                                   typeParamSet, inferred, fnName))
                    any = true;
            return any;
        } else if constexpr (std::is_same_v<T, FnType>) {
            std::vector<std::string> fparams;
            std::string fret;
            if (!splitFunctionTypeName(argTypeName, fparams, fret))
                return false;
            if (fparams.size() != v.param_types.size()) return false;
            bool any = false;
            for (size_t k = 0; k < v.param_types.size(); ++k)
                if (unifyTypeParam(*v.param_types[k], fparams[k],
                                   typeParamSet, inferred, fnName))
                    any = true;
            if (v.return_type && !fret.empty())
                if (unifyTypeParam(*v.return_type, fret,
                                   typeParamSet, inferred, fnName))
                    any = true;
            return any;
        } else if constexpr (std::is_same_v<T, OptionalType>) {
            // Accept either `"T?"` or the equivalent `"Option<T>"` spelling.
            std::string t = trimWs(argTypeName);
            if (!t.empty() && t.back() == '?') {
                return unifyTypeParam(*v.inner, trimWs(t.substr(0, t.size() - 1)),
                                      typeParamSet, inferred, fnName);
            }
            std::string head;
            std::vector<std::string> innerArgs;
            if (splitGenericTypeName(t, head, innerArgs) &&
                head == "Option" && innerArgs.size() == 1) {
                return unifyTypeParam(*v.inner, innerArgs[0],
                                      typeParamSet, inferred, fnName);
            }
            return false;
        } else {
            return false;
        }
    }, paramType.data);
}

std::vector<std::string> CodeGen::inferTypeArgs(
    const std::string &baseName, const std::vector<ExprPtr> &args) {

    auto it = generic_fn_templates_.find(baseName);
    if (it == generic_fn_templates_.end()) return {};

    const FnStmt &tmpl = *it->second.fnStmt;
    if (args.size() != tmpl.params.size()) return {};

    std::unordered_map<std::string, std::string> inferred;
    std::unordered_set<std::string> typeParamSet;
    for (auto &tp : tmpl.type_params)
        typeParamSet.insert(tp.name);

    std::unordered_map<std::string, llvm::Type*> emptyParamMap;
    std::unordered_map<std::string, std::string> emptyParamNameMap;
    for (size_t i = 0; i < args.size(); ++i) {
        const TypeNode &pt = *tmpl.params[i].type;
        std::string argName = inferExprTypeName(*args[i], emptyParamMap,
                                                 emptyParamNameMap);

        if (!argName.empty()) {
            if (unifyTypeParam(pt, argName, typeParamSet, inferred, baseName))
                continue;
        }

        // Fallback for bare type variables when `inferExprTypeName` could
        // not produce a shaped name (e.g., scalar literal arguments whose
        // Ry-level name equals their LLVM type).
        if (auto *bt = std::get_if<BasicType>(&pt.data);
            bt && typeParamSet.count(bt->name)) {
            llvm::Type *argTy = inferExprType(*args[i], emptyParamMap);
            std::string resolved;
            if (argTy == i64Ty_)  resolved = "int";
            else if (argTy == f64Ty_) resolved = "float";
            else if (argTy == i1Ty_)  resolved = "bool";
            else if (argTy == i8Ty_)  resolved = "u8";
            else if (argTy == ptrTy_) resolved = "str";
            else if (argTy == typeTy_) resolved = "Type";
            else if (isAnyType(argTy)) resolved = "any"; // NOLINT(bugprone-branch-clone)
            else if (auto *st = llvm::dyn_cast<llvm::StructType>(argTy)) {
                std::string sname = st->getName().str();
                if (record_types_.count(sname))
                    resolved = sname;
                else {
                    std::string n = findAdtEnumName(st);
                    resolved = n.empty() ? "any" : n;
                }
            }
            else resolved = "any";
            mergeInferredBinding(inferred, bt->name, resolved, baseName);
        }
    }

    // Build result in template parameter order
    std::vector<std::string> result;
    result.reserve(tmpl.type_params.size());
    for (auto &tp : tmpl.type_params) {
        auto found = inferred.find(tp.name);
        if (found == inferred.end())
            codegenError("could not infer type parameter '" + tp.name +
                         "' in call to generic function '" + baseName + "'");
        result.push_back(found->second);
    }
    return result;
}

// ===== Generic function instantiation =====

void CodeGen::instantiateGenericFn(const std::string &baseName,
                                    const std::vector<std::string> &typeArgs) {
    // Build full name: "identity<int>" or "map<int,str>"
    std::string fullName = baseName;
    fullName += '<';
    for (size_t i = 0; i < typeArgs.size(); ++i) {
        if (i > 0) fullName += ",";
        fullName += typeArgs[i];
    }
    fullName += ">";

    // Check cache
    if (generic_fn_instantiated_.count(fullName))
        return;

    auto it = generic_fn_templates_.find(baseName);
    if (it == generic_fn_templates_.end())
        codegenError("undefined generic function: " + baseName);

    FnStmt &s = *it->second.fnStmt;

    if (typeArgs.size() != s.type_params.size())
        codegenError("generic function '" + baseName + "' expects " +
                     std::to_string(s.type_params.size()) + " type argument(s), got " +
                     std::to_string(typeArgs.size()));

    // Set type parameter scope
    auto savedScope = std::move(type_param_scope_);
    type_param_scope_.clear();
    for (size_t i = 0; i < s.type_params.size(); ++i)
        type_param_scope_[s.type_params[i].name] = typeArgs[i];

    validateTypeBounds(s.type_params, typeArgs, "in generic function '" + baseName + "'");

    // Resolve parameter types and return type using type_param_scope_
    std::vector<llvm::Type*> paramTypes;
    paramTypes.reserve(s.params.size());
    for (auto &p : s.params)
        paramTypes.push_back(resolveType(p.type->toString()));
    std::string sReturnType = s.return_type ? s.return_type->toString() : "";
    llvm::Type *bodyRetTy = resolveType(sReturnType);

    std::string exposedReturnTypeName = substituteTypeParamsInName(sReturnType);
    llvm::Type *exposedRetTy = bodyRetTy;

    // Register in functions_ before body emission (enables recursion)
    std::string irName = fullName;
    llvm::FunctionType *ft = llvm::FunctionType::get(exposedRetTy, paramTypes, false);
    llvm::Function *func = llvm::Function::Create(
        ft, llvm::Function::InternalLinkage, irName, *mod_);

    std::vector<std::string> paramNames;
    paramNames.reserve(s.params.size());
    for (auto &p : s.params)
        paramNames.push_back(p.name);
    std::vector<std::string> paramTypeNames;
    paramTypeNames.reserve(s.params.size());
    for (auto &p : s.params)
        paramTypeNames.push_back(substituteTypeParamsInName(p.type->toString()));
    functions_[fullName].push_back({func, paramTypes, paramNames, paramTypeNames, exposedReturnTypeName,
                                    0, {}, &s.preconditions, &s.postconditions, &s.ensure_bindings,
                                    {}, {}, {}, {}, {}});
    generic_fn_instantiated_.insert(fullName);

    // Emit function body
    {
        FnScope guard(*this);
        fn_ = func;
        current_fn_return_type_ = exposedReturnTypeName;
        pushScope();

        llvm::BasicBlock *entry = llvm::BasicBlock::Create(*ctx_, "entry", func);
        builder_.SetInsertPoint(entry);

        unsigned idx = 0;
        for (auto &arg : func->args()) {
            arg.setName(s.params[idx].name);
            llvm::AllocaInst *alloca = builder_.CreateAlloca(
                paramTypes[idx], nullptr, s.params[idx].name);
            builder_.CreateStore(&arg, alloca);
            scope_stack_.back()[s.params[idx].name] = alloca;

            const auto &ptype = paramTypeNames[idx];
            applyParamTypeMeta(ptype, alloca, paramTypes[idx], s.params[idx].name);
            ++idx;
        }

        for (auto &stmt : s.body)
            std::visit([this](auto &st) { emitStmt(st); }, stmt);

        if (!builder_.GetInsertBlock()->getTerminator()) {
            llvm::Value *defaultRet = nullptr;
            if (bodyRetTy->isVoidTy()) {
                // no default value needed
            } else if (bodyRetTy == i64Ty_) {
                defaultRet = llvm::ConstantInt::get(i64Ty_, 0);
            } else if (bodyRetTy == f64Ty_) {
                defaultRet = llvm::ConstantFP::get(f64Ty_, 0.0);
            } else if (bodyRetTy == i1Ty_) {
                defaultRet = llvm::ConstantInt::get(i1Ty_, 0);
            } else if (bodyRetTy == ptrTy_) {
                defaultRet = llvm::ConstantPointerNull::get(
                    llvm::cast<llvm::PointerType>(ptrTy_));
            } else if (isAnyType(bodyRetTy)) {
                defaultRet = buildUnitAny();
            } else if (llvm::isa<llvm::StructType>(bodyRetTy)) {
                defaultRet = llvm::UndefValue::get(bodyRetTy);
            }

            if (bodyRetTy->isVoidTy())
                builder_.CreateRetVoid();
            else if (defaultRet)
                builder_.CreateRet(defaultRet);
            else
                builder_.CreateRet(llvm::UndefValue::get(bodyRetTy));
        }

        std::string err;
        llvm::raw_string_ostream errStream(err);
        if (llvm::verifyFunction(*func, &errStream))
            codegenError("IR verify error in generic function '" + fullName + "': " + err);
    }

    // Restore type parameter scope
    type_param_scope_ = std::move(savedScope);
}

} // namespace ry
