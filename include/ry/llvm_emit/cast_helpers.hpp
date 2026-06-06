// C++-side cast helpers for crossing the emit boundary.
//
// The boundary in `include/ry/llvm_emit/api.h` exposes only opaque pointer typedefs
// (RyValueRef, RyTypeRef, RyModuleRef, etc.) so no LLVM types appear in the
// public surface (#1973 AC). CodeGen-side callers obtain the underlying
// llvm::* pointers from CodeGen's existing state and need a small set of
// reinterpret_cast wrappers to bridge to the opaque boundary types without
// sprinkling reinterpret_cast across every call site.
//
// This header is C++-only and re-includes both `api.h` and the LLVM headers.
// It is for the CodeGen (caller) side only. The emission side lives in the Rust
// crate `crates/emit/src/lib.rs`, which does its own conversions in the
// reverse direction (opaque boundary handle → llvm::* via llvm-sys) and shares none
// of this C++ bridging code.

#ifndef RY_LLVM_EMIT_CAST_HELPERS_HPP
#define RY_LLVM_EMIT_CAST_HELPERS_HPP

#include "ry/llvm_emit/api.h"

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

namespace ry::llvm_emit {

// === CodeGen → boundary (LLVM pointer to opaque handle) ===

inline RyValueRef asRyValue(llvm::Value *v) {
    return reinterpret_cast<RyValueRef>(v);
}

inline RyTypeRef asRyType(llvm::Type *t) {
    return reinterpret_cast<RyTypeRef>(t);
}

inline RyTypeRef asRyType(llvm::StructType *t) {
    return reinterpret_cast<RyTypeRef>(static_cast<llvm::Type *>(t));
}

inline RyFuncTypeRef asRyFuncType(llvm::FunctionType *t) {
    return reinterpret_cast<RyFuncTypeRef>(t);
}

inline RyModuleRef asRyModule(llvm::Module *m) {
    return reinterpret_cast<RyModuleRef>(m);
}

inline RyBuilderRef asRyBuilder(llvm::IRBuilder<> *b) {
    return reinterpret_cast<RyBuilderRef>(b);
}

inline RyContextRef asRyContext(llvm::LLVMContext *c) {
    return reinterpret_cast<RyContextRef>(c);
}

inline RyFunctionRef asRyFunction(llvm::Function *f) {
    return reinterpret_cast<RyFunctionRef>(f);
}

inline RyBasicBlockRef asRyBasicBlock(llvm::BasicBlock *bb) {
    return reinterpret_cast<RyBasicBlockRef>(bb);
}

// === boundary → CodeGen (opaque handle to LLVM pointer) ===

inline llvm::Value *asLlvmValue(RyValueRef p) {
    return reinterpret_cast<llvm::Value *>(p);
}

inline llvm::Type *asLlvmType(RyTypeRef p) {
    return reinterpret_cast<llvm::Type *>(p);
}

inline llvm::FunctionType *asLlvmFuncType(RyFuncTypeRef p) {
    return reinterpret_cast<llvm::FunctionType *>(p);
}

inline llvm::BasicBlock *asLlvmBasicBlock(RyBasicBlockRef p) {
    return reinterpret_cast<llvm::BasicBlock *>(p);
}

} // namespace ry::llvm_emit

#endif // RY_LLVM_EMIT_CAST_HELPERS_HPP
