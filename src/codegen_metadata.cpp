#include "ry/codegen.hpp"
#include <algorithm>
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/Instructions.h>

namespace ry {

// ======== ValueMetadata query helpers ========

bool CodeGen::ValueMetadata::hasAnyCollectionType() const {
    return list_elem || map_key || map_value || set_elem ||
           nested_list_elem || task_result || thread_result || iterator_elem;
}

bool CodeGen::ValueMetadata::hasAnyResourceKind() const {
    return !resource_kinds.empty() || json_type_only;
}

bool CodeGen::ValueMetadata::hasAnyMeta() const {
    return hasAnyCollectionType() || hasAnyResourceKind() ||
           fn_type_info.has_value() ||
           !low_level_type_name.empty() ||
           !map_key_type_name.empty() ||
           !map_value_type_name.empty() ||
           !union_value_type.empty() ||
           !enum_value_type.empty() ||
           !list_elem_type_name.empty() ||
           !set_elem_type_name.empty() ||
           list_elem_fn_type_info.has_value() ||
           map_value_fn_type_info.has_value() ||
           set_elem_fn_type_info.has_value();
}

llvm::Type *CodeGen::ValueMetadata::getCollectionType(TypeMeta kind) const {
    switch (kind) {
    case TypeMeta::ListElem:       return list_elem;
    case TypeMeta::MapKey:         return map_key;
    case TypeMeta::MapValue:       return map_value;
    case TypeMeta::SetElem:        return set_elem;
    case TypeMeta::NestedListElem: return nested_list_elem;
    case TypeMeta::TaskResult:     return task_result;
    case TypeMeta::ThreadResult:   return thread_result;
    case TypeMeta::IteratorElem:   return iterator_elem;
    case TypeMeta::COUNT:          return nullptr;
    }
    return nullptr;
}

void CodeGen::ValueMetadata::setCollectionType(TypeMeta kind, llvm::Type *ty) {
    switch (kind) {
    case TypeMeta::ListElem:       list_elem = ty; break;
    case TypeMeta::MapKey:         map_key = ty; break;
    case TypeMeta::MapValue:       map_value = ty; break;
    case TypeMeta::SetElem:        set_elem = ty; break;
    case TypeMeta::NestedListElem: nested_list_elem = ty; break;
    case TypeMeta::TaskResult:     task_result = ty; break;
    case TypeMeta::ThreadResult:   thread_result = ty; break;
    case TypeMeta::IteratorElem:   iterator_elem = ty; break;
    case TypeMeta::COUNT:          break;
    }
}

// ======== CodeGen metadata accessors ========

CodeGen::ValueMetadata *CodeGen::getMeta(llvm::Value *val) {
    auto it = value_metadata_.find(val);
    if (it != value_metadata_.end()) return &it->second;
    // Resolve through LoadInst
    if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
        it = value_metadata_.find(load->getPointerOperand());
        if (it != value_metadata_.end()) return &it->second;
    }
    return nullptr;
}

const CodeGen::ValueMetadata *CodeGen::getMeta(llvm::Value *val) const {
    auto it = value_metadata_.find(val);
    if (it != value_metadata_.end()) return &it->second;
    if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
        it = value_metadata_.find(load->getPointerOperand());
        if (it != value_metadata_.end()) return &it->second;
    }
    return nullptr;
}

CodeGen::ValueMetadata &CodeGen::getOrCreateMeta(llvm::Value *val) {
    return value_metadata_[val];
}

// ======== Type-name accessors ========

std::string CodeGen::getSetElemName(llvm::Value *setPtr) const {
    const ValueMetadata *meta = getMeta(setPtr);
    return meta ? meta->set_elem_type_name : std::string{};
}

void CodeGen::validateSetElemType(const std::string &elemName, llvm::Value *elem,
                                   const std::string &errorContext) {
    if (elemName.empty()) return;
    if (elem->getType() == ptrTy_) {
        std::string actualName = inferCollectionTypeName(elem);
        if (!actualName.empty() && actualName != elemName)
            codegenError(errorContext + ": element type mismatch: expected '" +
                         elemName + "', got '" + actualName + "'");
    }
    propagateTypeMeta(elemName, elem);
}

// ======== TypeMeta convenience ========

void CodeGen::setTypeMeta(TypeMeta kind, llvm::Value *val, llvm::Type *ty) {
    getOrCreateMeta(val).setCollectionType(kind, ty);
}

llvm::Type *CodeGen::getTypeMeta(TypeMeta kind, llvm::Value *val) const {
    if (auto *meta = getMeta(val))
        return meta->getCollectionType(kind);
    return nullptr;
}

// ======== Unified propagation ========

void CodeGen::propagateMeta(llvm::Value *src, llvm::Value *dst) {
    if (!getMeta(src)) return;
    // Call getOrCreateMeta(dst) first so any rehash of value_metadata_ happens
    // before we take a pointer to src's metadata.  Then re-fetch src metadata
    // to avoid the deep-copy overhead of copying the full ValueMetadata struct.
    ValueMetadata &dstMeta = getOrCreateMeta(dst);
    const ValueMetadata *srcMetaPtr = getMeta(src);
    if (!srcMetaPtr) return;  // src == dst edge case: dst was just created, nothing to copy
    const ValueMetadata &srcMeta = *srcMetaPtr;

    // Collection types
    if (srcMeta.list_elem)       dstMeta.list_elem = srcMeta.list_elem;
    if (srcMeta.map_key)         dstMeta.map_key = srcMeta.map_key;
    if (srcMeta.map_value)       dstMeta.map_value = srcMeta.map_value;
    if (srcMeta.set_elem)        dstMeta.set_elem = srcMeta.set_elem;
    if (srcMeta.nested_list_elem) dstMeta.nested_list_elem = srcMeta.nested_list_elem;
    if (srcMeta.task_result)     dstMeta.task_result = srcMeta.task_result;
    if (srcMeta.thread_result)   dstMeta.thread_result = srcMeta.thread_result;
    if (srcMeta.iterator_elem)   dstMeta.iterator_elem = srcMeta.iterator_elem;

    // String metadata
    if (!srcMeta.low_level_type_name.empty())
        dstMeta.low_level_type_name = srcMeta.low_level_type_name;
    if (!srcMeta.map_key_type_name.empty())
        dstMeta.map_key_type_name = srcMeta.map_key_type_name;
    if (!srcMeta.map_value_type_name.empty())
        dstMeta.map_value_type_name = srcMeta.map_value_type_name;
    if (!srcMeta.list_elem_type_name.empty())
        dstMeta.list_elem_type_name = srcMeta.list_elem_type_name;
    if (!srcMeta.set_elem_type_name.empty())
        dstMeta.set_elem_type_name = srcMeta.set_elem_type_name;
    if (srcMeta.list_elem_fn_type_info)
        dstMeta.list_elem_fn_type_info = srcMeta.list_elem_fn_type_info;
    if (srcMeta.map_key_fn_type_info)
        dstMeta.map_key_fn_type_info = srcMeta.map_key_fn_type_info;
    if (srcMeta.map_value_fn_type_info)
        dstMeta.map_value_fn_type_info = srcMeta.map_value_fn_type_info;
    if (srcMeta.set_elem_fn_type_info)
        dstMeta.set_elem_fn_type_info = srcMeta.set_elem_fn_type_info;
    if (!srcMeta.union_value_type.empty())
        dstMeta.union_value_type = srcMeta.union_value_type;
    if (!srcMeta.enum_value_type.empty())
        dstMeta.enum_value_type = srcMeta.enum_value_type;

    // FnTypeInfo
    if (srcMeta.fn_type_info)
        dstMeta.fn_type_info = srcMeta.fn_type_info;

    // Resource kinds
    for (int rk : srcMeta.resource_kinds)
        dstMeta.addResourceKind(rk);
    if (srcMeta.json_type_only)
        dstMeta.json_type_only = true;

    // ARC managed status propagation
    auto *dstAlloca = llvm::dyn_cast<llvm::AllocaInst>(dst);
    if (dstAlloca) {
        llvm::Value *resolved = src;
        if (auto *load = llvm::dyn_cast<llvm::LoadInst>(src))
            resolved = load->getPointerOperand();
        auto *srcAlloca = llvm::dyn_cast<llvm::AllocaInst>(resolved);
        if (srcAlloca && isArcManaged(srcAlloca))
            markArcManaged(dstAlloca);
    }
}

void CodeGen::propagateMetaWide(llvm::Value *src, llvm::Value *dst) {
    // getMeta() already resolves through LoadInst, so propagateMeta handles
    // both the direct key and the LoadInst operand path.
    propagateMeta(src, dst);
}

// ======== Resource kind helpers ========

void CodeGen::addResourceKind(llvm::Value *val, int rk) {
    auto &meta = getOrCreateMeta(val);
    meta.addResourceKind(rk);
}

bool CodeGen::hasResourceKind(llvm::Value *val, int rk) const {
    if (auto *meta = getMeta(val))
        return llvm::is_contained(meta->resource_kinds, rk);
    return false;
}

void CodeGen::removeResourceKind(llvm::Value *val, int rk) {
    auto it = value_metadata_.find(val);
    if (it == value_metadata_.end()) return;
    auto &kinds = it->second.resource_kinds;
    kinds.erase(std::remove(kinds.begin(), kinds.end(), rk), kinds.end());
}

// ValueMetadata::addResourceKind helper (avoid duplicates)
void CodeGen::ValueMetadata::addResourceKind(int rk) {
    if (!llvm::is_contained(resource_kinds, rk))
        resource_kinds.push_back(rk);
}

} // namespace ry
