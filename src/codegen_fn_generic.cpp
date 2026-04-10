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

void CodeGen::emitInvariantCheck(const std::string &typeName, const StructInfo &info,
                                  llvm::Value *structVal) {
    if (info.invariants.empty() && info.parentName.empty()) return;

    std::vector<std::pair<std::string, const ExprPtr*>> allInvariants;
    allInvariants.reserve(8);
    for (auto &inv : info.invariants)
        allInvariants.push_back({typeName, &inv});
    const std::string *parent = &info.parentName;
    while (!parent->empty()) {
        auto pit = struct_types_.find(*parent);
        if (pit == struct_types_.end()) break;
        for (auto &inv : pit->second.invariants)
            allInvariants.push_back({*parent, &inv});
        parent = &pit->second.parentName;
    }
    if (allInvariants.empty()) return;

    pushScope();
    for (unsigned f = 0; f < info.fields.size(); ++f) {
        llvm::Type *fieldTy = info.llvmType->getElementType(f);
        llvm::AllocaInst *fieldAlloca = builder_.CreateAlloca(fieldTy, nullptr, info.fields[f].name);
        llvm::Value *fieldVal = builder_.CreateExtractValue(structVal, f, info.fields[f].name + "_val");
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
    for (int i = 0; i < static_cast<int>(current_postconditions_->size()); ++i)
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

        if (!struct_types_.count(bound))
            codegenError("unknown type constraint: '" + bound + "'");

        if (concrete != bound && !isSubtypeOf(concrete, bound))
            codegenError("type '" + concrete + "' does not satisfy constraint '" +
                         bound + "': not a subtype of '" + bound +
                         "' (" + context + ")");
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

    // Build type parameter mapping
    std::unordered_map<std::string, std::string> typeMap;
    for (size_t i = 0; i < tmpl.typeParams.size(); ++i)
        typeMap[tmpl.typeParams[i].name] = typeArgs[i];

    // Create a concrete EnumStmt by substituting type parameters
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
        llvm::Constant *str = cachedGlobalString(
            v.name, ".enum_" + fullName + "_" + v.name);
        nameStrings.push_back(str);

        if (!v.field_types.empty()) {
            hasADT = true;
            VariantFieldInfo vfi;
            for (auto &ft : v.field_types) {
                std::string ftStr = ft->toString();
                std::string resolved = ftStr;
                auto mit = typeMap.find(ftStr);
                if (mit != typeMap.end()) resolved = mit->second;
                vfi.fieldTypes.push_back(resolveType(resolved));
                vfi.fieldTypeNames.push_back(resolved);
            }
            info.variantFields[v.name] = std::move(vfi);
        }
    }
    info.isADT = hasADT;

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
        llvm::Value *origin = val;
        if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val))
            origin = load->getPointerOperand();

        if (auto *elemTy = getTypeMeta(TypeMeta::ListElem, val))
            return "List<" + reverseResolveType(
                llvm::UndefValue::get(elemTy)) + ">";

        auto *meta = getMeta(val);
        if (meta && meta->fn_type_info) {
            auto &fti = *meta->fn_type_info;
            std::string result = "function(";
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
        std::string n = findStructTypeName(st);
        if (!n.empty()) return n;
    }

    return "any";
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

    // Use AST-based type inference to avoid emitting IR for arguments
    std::unordered_map<std::string, llvm::Type*> emptyParamMap;
    for (size_t i = 0; i < args.size(); ++i) {
        const std::string paramType = tmpl.params[i].type->toString();
        llvm::Type *argTy = inferExprType(*args[i], emptyParamMap);

        if (typeParamSet.count(paramType)) {
            std::string resolved;
            if (argTy == i64Ty_)  resolved = "int";
            else if (argTy == f64Ty_) resolved = "float";
            else if (argTy == i1Ty_)  resolved = "bool";
            else if (argTy == i8Ty_)  resolved = "u8";
            else if (argTy == ptrTy_) resolved = "str";
            else if (argTy == typeTy_) resolved = "Type";
            else if (isAnyType(argTy)) resolved = "any";
            else if (auto *st = llvm::dyn_cast<llvm::StructType>(argTy)) {
                std::string sname = st->getName().str();
                if (struct_types_.count(sname))
                    resolved = sname;
                else {
                    std::string n = findAdtEnumName(st);
                    resolved = n.empty() ? "any" : n;
                }
            }
            else resolved = "any";

            if (inferred.count(paramType) && inferred[paramType] != resolved)
                codegenError("conflicting type inference for '" + paramType +
                             "': '" + inferred[paramType] + "' vs '" + resolved + "'");
            inferred[paramType] = resolved;
        }
    }

    // Build result in template parameter order
    std::vector<std::string> result;
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
    std::string fullName = baseName + "<";
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

    // Substitute type params in return type name
    std::string exposedReturnTypeName = sReturnType;
    auto retTpit = type_param_scope_.find(sReturnType);
    if (retTpit != type_param_scope_.end()) {
        exposedReturnTypeName = retTpit->second;
    } else if (!sReturnType.empty() && sReturnType.back() == '?') {
        std::string inner = sReturnType.substr(0, sReturnType.size() - 1);
        auto innerIt = type_param_scope_.find(inner);
        if (innerIt != type_param_scope_.end())
            exposedReturnTypeName = innerIt->second + "?";
    }
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
    for (auto &p : s.params) {
        // Substitute type params in param type names for FnTypeInfo
        std::string pTypeStr = p.type->toString();
        std::string resolvedName = pTypeStr;
        auto tpit = type_param_scope_.find(pTypeStr);
        if (tpit != type_param_scope_.end())
            resolvedName = tpit->second;
        paramTypeNames.push_back(resolvedName);
    }
    functions_[fullName].push_back({func, paramTypes, paramNames, paramTypeNames, exposedReturnTypeName,
                                    0, {}, &s.preconditions, &s.postconditions, &s.ensure_bindings});
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

            std::string ptype = paramTypeNames[idx];
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
