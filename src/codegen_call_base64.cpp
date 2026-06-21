#include "ry/codegen.hpp"
#include "ry/stdlib_registry.hpp"


namespace ry {

// base64 dispatch is fully descriptor-driven via emitGenericNativeCall:
// the package is registered here only so isStdlibPackageName("base64")
// stays true (used by the "module not imported" diagnostic in
// codegen_call_dispatch.cpp), and so the generic path picks up the
// snake_case_symbols flag — Ry-side `encodeUrlSafe` derives the C
// symbol `__ry_base64_encode_url_safe`. The dispatcher returns nullptr
// to let dispatch fall through to emitGenericNativeCall, which reads
// `@native("base64")` declarations from share/std/base64/base64.ry as
// the source of truth for arity, return wrapping, error channel, and
// List<u8> argument enforcement.
RY_REGISTER_STDLIB_PACKAGE_NAMING(base64, "share/std/base64/base64.ry", dispatchBase64, /*snake_case=*/true)
static llvm::Value *dispatchBase64(CodeGen &, const CallExpr &) {
    // Stub: base64 dispatch is descriptor-driven via emitGenericNativeCall.
    // See `.claude/rules/codegen-stdlib-dispatcher.md` "Descriptor-migrated
    // stdlib dispatcher stubs must return nullptr" — routing here would
    // cause 3× arg re-emission on type mismatch.
    return nullptr;
}

} // namespace ry
