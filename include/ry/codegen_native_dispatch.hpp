#pragma once

#include <cstddef>
#include <cstdint>

namespace llvm {
class Value;
}

namespace ry {

class CodeGen;
struct CallExpr;

enum class CodeGenReturnWrapping : uint8_t {
    Direct,               // return as-is (ptr->ptr, void->Unit, i64->int)
    ResultPtr,            // wrapPtrAsResult(ptr, errFn)
    ResultStatus,         // wrapStatusAsResult(status, errFn)
    ResultOutParam,       // alloca + out-param + emitResultBranch
    BoolFromI64,          // i64 -> trunc to i1
    ResultPtrWithListMeta,// ResultPtr + type_meta_[ListElem] annotation

    // #2381 (Installment 2-c): handle-coupled and NUL-checked overloads.
    OptionFromNullablePtr,// wrapPtrAsOption(ptr): null -> None, else Some(ptr)
    ResultOutParamOption, // status<0 -> Err(runtime); status>=0 -> Ok(Option<loaded ptr>)
    IteratorFromHandle,   // synthesize Iterator<T> with next-fn calling exported_symbol(handle, &out)

    // #2393: thread sync-primitive *Free entries (lockFree / rwlockFree /
    // semaphoreFree / barrierFree / atomicIntFree / atomicBoolFree). No
    // runtime fn is called; the destructor lives in ResourceKindRegistry
    // and emitResourceFree emits the null-check + ARC-release sequence.
    // The handle arg is at `desc.handle_param_index` (must be 0 — the
    // *Free customEmitters all take the resource as the sole arg) with
    // `desc.handle_resource_kind` selecting the destructor.
    ResourceFree,
};

using CodeGenCustomEmitterFn = llvm::Value *(*)(CodeGen &cg, const CallExpr &e);

enum class CodeGenListElemMeta : uint8_t { None, I8, Ptr };

struct CodeGenNativeDispatchEntry {
    const char *fnName = nullptr;         // e.g. "encode", "collect"
    const char *rtSuffix = nullptr;       // runtime name suffix override (nullptr = use fnName)
    CodeGenReturnWrapping wrapping = CodeGenReturnWrapping::Direct;
    int arity = 0;                        // -1 = variadic (e.g. path::join 2-4 args)
    const char *outParamType = nullptr;   // for ResultOutParam only (e.g. "int"); nullptr otherwise

    // --- Tier 2 additions ---
    CodeGenCustomEmitterFn customEmitter = nullptr;   // escape hatch for complex logic
    const char *rtNameOverride = nullptr;             // full runtime name (e.g. "sin", "__ry_file_exists")
    const char *errFnOverride = nullptr;              // error function override (nullptr = derive from module)
    CodeGenListElemMeta listElemMeta = CodeGenListElemMeta::None;  // post-call TypeMeta::ListElem annotation
    int requireListU8Arg = -1;  // arg index that must carry TypeMeta::ListElem==i8; -1=no check
};

} // namespace ry
