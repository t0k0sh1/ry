#include "ry/codegen.hpp"
#include "ry/stdlib_registry.hpp"


namespace ry {

// runtime_internal package dispatcher
//
// Provides the `arcLiveCount() -> int` function to Ry test code.
// The backing C++ symbol (__ry_runtime_internal_arc_live_count) lives in
// src/runtime_arc_counter.cpp and is linked directly into ry_lib / the host
// process, so no separate shared library is needed.

static const CodeGen::NativeDispatchEntry runtime_internal_table[] = {
    {"arcLiveCount", nullptr, CodeGen::ReturnWrapping::Direct, 0, nullptr,
     nullptr, "__ry_runtime_internal_arc_live_count", nullptr, CodeGen::ListElemMeta::None, -1},
};

RY_REGISTER_STDLIB_PACKAGE(runtime_internal,
    "share/std/runtime_internal/runtime_internal.ry",
    dispatchRuntimeInternal)
static llvm::Value *dispatchRuntimeInternal(CodeGen &cg, const CallExpr &e) {
    return cg.emitTableDrivenNativeCall(
        e, "runtime_internal",
        runtime_internal_table,
        std::size(runtime_internal_table));
}

} // namespace ry
