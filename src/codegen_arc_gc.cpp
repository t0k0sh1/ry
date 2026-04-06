#include "ry/codegen.hpp"


namespace ry {

// ===== Cycle Collector — static analysis & visit function generation =====

// Collect referenced type names from a TypeNode tree.
static void collectReferencedTypes(const TypeNodePtr &tn,
                                   std::unordered_set<std::string> &out) {
    if (!tn) return;
    if (auto *bt = std::get_if<BasicType>(&tn->data)) {
        out.insert(bt->name);
    } else if (auto *gt = std::get_if<GenericType>(&tn->data)) {
        out.insert(gt->name);
        for (auto &arg : gt->type_args)
            collectReferencedTypes(arg, out);
    } else if (auto *ot = std::get_if<OptionalType>(&tn->data)) {
        collectReferencedTypes(ot->inner, out);
    } else if (auto *wt = std::get_if<WeakType>(&tn->data)) {
        // Weak references don't form cycles (that's their purpose)
        (void)wt;
    } else if (auto *ut = std::get_if<UnionType>(&tn->data)) {
        for (auto &c : ut->components)
            collectReferencedTypes(c, out);
    } else if (auto *tt = std::get_if<TupleType>(&tn->data)) {
        for (auto &e : tt->elements)
            collectReferencedTypes(e, out);
    } else if (auto *at = std::get_if<ArrayType>(&tn->data)) {
        collectReferencedTypes(at->element_type, out);
    }
    // FnType, RangeType: skip (functions don't form ownership cycles)
}

void CodeGen::collectTypeGraphFromStmt(
    const StmtNode &stmt,
    std::unordered_map<std::string, std::unordered_set<std::string>> &graph,
    std::unordered_set<std::string> &all_types) {
    if (auto *es = std::get_if<EnumStmt>(&stmt)) {
        if (!es->type_params.empty()) return;  // skip generic templates
        all_types.insert(es->name);
        auto &refs = graph[es->name];
        for (auto &v : es->variants) {
            for (auto &ft : v.field_types) {
                collectReferencedTypes(ft, refs);
            }
        }
    } else if (auto *rs = std::get_if<RecordStmt>(&stmt)) {
        all_types.insert(rs->name);
        auto &refs = graph[rs->name];
        for (auto &f : rs->fields) {
            collectReferencedTypes(f.type, refs);
        }
    }
}

void CodeGen::runCyclicTypeAnalysis(
    std::unordered_map<std::string, std::unordered_set<std::string>> &graph,
    const std::unordered_set<std::string> &all_types) {
    // Remove edges to types not in our type set (built-in types like int, str, etc.).
    for (auto &[name, refs] : graph) {
        std::unordered_set<std::string> filtered;
        for (auto &r : refs) {
            if (all_types.count(r))
                filtered.insert(r);
        }
        refs = std::move(filtered);
    }

    // DFS to find types that participate in cycles.
    // Standard cycle detection: white/grey/black colouring.
    enum class Color { White, Grey, Black };
    std::unordered_map<std::string, Color> color;
    for (auto &t : all_types) color[t] = Color::White;

    std::unordered_set<std::string> cyclic;

    // DFS that returns true if the node is part of or reaches a cycle.
    std::function<bool(const std::string &, std::vector<std::string> &)> dfs;
    dfs = [&](const std::string &node, std::vector<std::string> &stack) -> bool {
        color[node] = Color::Grey;
        stack.push_back(node);
        bool found_cycle = false;

        auto it = graph.find(node);
        if (it != graph.end()) {
            for (auto &neighbor : it->second) {
                if (color[neighbor] == Color::Grey) {
                    // Found a cycle — mark all nodes on the stack from neighbor onward.
                    bool marking = false;
                    for (auto &s : stack) {
                        if (s == neighbor) marking = true;
                        if (marking) cyclic.insert(s);
                    }
                    found_cycle = true;
                } else if (color[neighbor] == Color::White) {
                    if (dfs(neighbor, stack))
                        found_cycle = true;
                }
            }
        }

        stack.pop_back();
        color[node] = Color::Black;
        return found_cycle;
    };

    for (auto &t : all_types) {
        if (color[t] == Color::White) {
            std::vector<std::string> stack;
            dfs(t, stack);
        }
    }

    potentially_cyclic_types_ = std::move(cyclic);
}

bool CodeGen::isPotentiallyCyclic(const std::string &typeName) const {
    if (potentially_cyclic_types_.count(typeName))
        return true;
    // Check wrapped types: Option<T> -> T, List<T> -> T, etc.
    // Extract base name from generic wrappers.
    auto checkInner = [&](const std::string &prefix) -> bool {
        if (typeName.size() > prefix.size() + 1 &&
            typeName.compare(0, prefix.size(), prefix) == 0 &&
            typeName.back() == '>') {
            std::string inner = typeName.substr(prefix.size(),
                                                 typeName.size() - prefix.size() - 1);
            return isPotentiallyCyclic(inner);
        }
        return false;
    };
    if (checkInner("Option<")) return true;
    if (checkInner("List<")) return true;
    if (checkInner("Set<")) return true;
    // Unwrap T? suffix (OptionalType::toString() produces "T?")
    if (typeName.size() > 1 && typeName.back() == '?')
        return isPotentiallyCyclic(typeName.substr(0, typeName.size() - 1));
    // Map<K,V> — check both K and V
    if (isMapTypeName(typeName) && typeName.back() == '>') {
        std::string inner = typeName.substr(4, typeName.size() - 5);
        // Find the comma separating K and V (handle nested generics)
        int depth = 0;
        for (size_t i = 0; i < inner.size(); ++i) {
            if (inner[i] == '<') depth++;
            else if (inner[i] == '>') depth--;
            else if (inner[i] == ',' && depth == 0) {
                std::string k = inner.substr(0, i);
                std::string v = inner.substr(i + 2); // skip ", "
                if (isPotentiallyCyclic(k) || isPotentiallyCyclic(v))
                    return true;
                break;
            }
        }
    }
    return false;
}

llvm::Function *CodeGen::getOrCreateVisitFunction(const std::string &typeName) {
    auto it = gc_visit_functions_.find(typeName);
    if (it != gc_visit_functions_.end())
        return it->second;

    // Try ADT enum type first.
    auto enumIt = enum_types_.find(typeName);
    if (enumIt != enum_types_.end() && enumIt->second.isADT)
        return createAdtVisitFunction(typeName, enumIt->second);

    // Try record (struct) type.
    auto structIt = struct_types_.find(typeName);
    if (structIt != struct_types_.end())
        return createStructVisitFunction(typeName, structIt->second);

    // Unknown type — no visit function needed.
    gc_visit_functions_[typeName] = nullptr;
    return nullptr;
}

// Emit IR to visit a single potentially-cyclic field during GC traversal.
// Handles ARC pointer fields (null check + visitor call) and embedded record
// fields (recursive visit function call).
void CodeGen::emitGcVisitField(llvm::Value *fieldPtr, llvm::Type *fieldTy,
                                const std::string &fieldTypeName,
                                llvm::Value *visitorFn,
                                llvm::FunctionType *visitorCallTy,
                                llvm::FunctionType *visitFnTy,
                                llvm::Function *parentFn) {
    if (fieldTy == ptrTy_) {
        auto *fieldVal = builder_.CreateLoad(ptrTy_, fieldPtr, "gc.visit.val");
        auto *isNull = builder_.CreateICmpEQ(
            fieldVal,
            llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)),
            "gc.visit.null");
        auto *visitBB = llvm::BasicBlock::Create(*ctx_, "gc.visit.ptr", parentFn);
        auto *skipBB = llvm::BasicBlock::Create(*ctx_, "gc.skip.ptr", parentFn);
        builder_.CreateCondBr(isNull, skipBB, visitBB);

        builder_.SetInsertPoint(visitBB);
        auto *hdr = emitArcGetHeaderFromData(fieldVal);
        builder_.CreateCall(visitorCallTy, visitorFn, {hdr});
        builder_.CreateBr(skipBB);

        builder_.SetInsertPoint(skipBB);
    } else if (llvm::isa<llvm::StructType>(fieldTy)) {
        if (auto *nestedVisitFn = getOrCreateVisitFunction(fieldTypeName))
            builder_.CreateCall(visitFnTy, nestedVisitFn, {fieldPtr, visitorFn});
    }
}

// Visit function for record (struct) types: iterate fixed-layout fields.
llvm::Function *CodeGen::createStructVisitFunction(const std::string &typeName,
                                                    const StructInfo &info) {
    gc_visit_functions_[typeName] = nullptr;

    auto *savedBB = builder_.GetInsertBlock();
    auto savedPt = builder_.GetInsertPoint();

    auto *visitFnTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {ptrTy_, ptrTy_}, false);
    auto *visitFn = llvm::Function::Create(
        visitFnTy, llvm::Function::InternalLinkage,
        "__ry_gc_visit_" + typeName, *mod_);
    visitFn->setDoesNotThrow();

    auto *entryBB = llvm::BasicBlock::Create(*ctx_, "entry", visitFn);
    builder_.SetInsertPoint(entryBB);

    llvm::Value *dataPtr = visitFn->getArg(0);
    llvm::Value *visitorFnArg = visitFn->getArg(1);

    auto *visitorCallTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);

    for (unsigned i = 0; i < info.fields.size(); ++i) {
        llvm::Type *fieldTy = info.llvmType->getElementType(i);
        const std::string fieldTypeName = info.fields[i].type->toString();

        if (!isPotentiallyCyclic(fieldTypeName))
            continue;

        auto *fieldPtr = builder_.CreateStructGEP(info.llvmType, dataPtr, i,
                                                   "gc.struct.field." + std::to_string(i));
        emitGcVisitField(fieldPtr, fieldTy, fieldTypeName,
                         visitorFnArg, visitorCallTy, visitFnTy, visitFn);
    }

    builder_.CreateRetVoid();

    if (savedBB)
        builder_.SetInsertPoint(savedBB, savedPt);

    gc_visit_functions_[typeName] = visitFn;
    return visitFn;
}

// Visit function for ADT enum types: switch on tag, visit variant fields.
llvm::Function *CodeGen::createAdtVisitFunction(const std::string &typeName,
                                                  const EnumInfo &info) {
    // Insert a placeholder to break mutual recursion.
    gc_visit_functions_[typeName] = nullptr;

    auto *savedBB = builder_.GetInsertBlock();
    auto savedPt = builder_.GetInsertPoint();

    auto *visitFnTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {ptrTy_, ptrTy_}, false);
    auto *visitFn = llvm::Function::Create(
        visitFnTy, llvm::Function::InternalLinkage,
        "__ry_gc_visit_" + typeName, *mod_);
    visitFn->setDoesNotThrow();

    auto *entryBB = llvm::BasicBlock::Create(*ctx_, "entry", visitFn);
    builder_.SetInsertPoint(entryBB);

    llvm::Value *dataPtr = visitFn->getArg(0);
    llvm::Value *visitorFn = visitFn->getArg(1);

    // Cast data to the ADT struct type: { i64 tag, [N x i8] payload }
    // Load the tag.
    auto *tagPtr = builder_.CreateStructGEP(info.adtType, dataPtr, 0, "gc_tag_ptr");
    auto *tag = builder_.CreateLoad(i64Ty_, tagPtr, "gc_tag");

    // Payload starts after the tag.
    auto *payloadPtr = builder_.CreateStructGEP(info.adtType, dataPtr, 1, "gc_payload");

    // Create switch on tag to visit the right variant's fields.
    auto *doneBB = llvm::BasicBlock::Create(*ctx_, "gc.visit.done", visitFn);
    auto *sw = builder_.CreateSwitch(tag, doneBB, info.variantOrder.size());

    // Visitor call function type: void(ptr)
    auto *visitorCallTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);

    const llvm::DataLayout &dl = mod_->getDataLayout();

    for (size_t vi = 0; vi < info.variantOrder.size(); ++vi) {
        const std::string &variantName = info.variantOrder[vi];
        auto vfIt = info.variantFields.find(variantName);
        int64_t tagVal = info.variants.at(variantName);

        // If this variant has no fields or no ARC fields, skip it.
        bool hasArcField = false;
        if (vfIt != info.variantFields.end()) {
            for (auto &ftName : vfIt->second.fieldTypeNames) {
                if (isPotentiallyCyclic(ftName) || ftName == typeName) {
                    hasArcField = true;
                    break;
                }
            }
        }
        if (!hasArcField) {
            // Wire this tag to doneBB (no ARC fields to visit).
            sw->addCase(llvm::cast<llvm::ConstantInt>(llvm::ConstantInt::get(i64Ty_, tagVal)), doneBB);
            continue;
        }

        auto *caseBB = llvm::BasicBlock::Create(
            *ctx_, "gc.visit." + variantName, visitFn);
        sw->addCase(llvm::cast<llvm::ConstantInt>(llvm::ConstantInt::get(i64Ty_, tagVal)), caseBB);
        builder_.SetInsertPoint(caseBB);

        // Walk through fields with proper alignment (same layout as codegen_call_dispatch).
        const VariantFieldInfo &vfi = vfIt->second;
        size_t offset = 0;
        for (size_t fi = 0; fi < vfi.fieldTypes.size(); ++fi) {
            llvm::Type *fieldTy = vfi.fieldTypes[fi];
            const std::string &fieldTypeName = vfi.fieldTypeNames[fi];

            uint64_t align = dl.getABITypeAlign(fieldTy).value();
            offset = (offset + align - 1) / align * align;

            if (isPotentiallyCyclic(fieldTypeName)) {
                auto *fieldPtr = builder_.CreateGEP(
                    i8Ty_, payloadPtr,
                    llvm::ConstantInt::get(i64Ty_, offset),
                    "gc.field." + std::to_string(fi));
                emitGcVisitField(fieldPtr, fieldTy, fieldTypeName,
                                 visitorFn, visitorCallTy, visitFnTy, visitFn);
            }

            offset += dl.getTypeAllocSize(fieldTy);
        }

        builder_.CreateBr(doneBB);
    }

    builder_.SetInsertPoint(doneBB);
    builder_.CreateRetVoid();

    // Restore insertion point.
    if (savedBB)
        builder_.SetInsertPoint(savedBB, savedPt);

    gc_visit_functions_[typeName] = visitFn;
    return visitFn;
}

} // namespace ry
