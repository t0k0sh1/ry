#pragma once

#include <optional>
#include <string>
#include <unordered_set>
#include <utility>

#include "ry/codegen_native_dispatch.hpp"  // CodeGenReturnWrapping

namespace ry {

// NativeCallDescriptor: declarative @native call shape (foundation for
// docs/architecture/native-call-boundary.md follow-up #1, tracking #2299).
//
// v1 (#2335) stored only `library_name`. The pilot in #2337 adds the
// three declarative fields the architecture doc §"Pilot" specifies —
// `return_wrapping`, `error_channel`, `require_list_u8_arg` — plus
// `out_param_type_name` which `inferReturnWrapping` returns alongside
// `return_wrapping` (filesystem's `Result<int>` / `Result<bool>` entries
// need it for `ResultOutParam`, and filesystem shares the consume path).
struct NativeCallDescriptor {
    std::optional<std::string> library_name;

    // Pilot fields (#2337): populated at @native declaration time so
    // emitGenericNativeCall consumes pre-computed values instead of
    // re-inferring them at every call site.
    CodeGenReturnWrapping return_wrapping = CodeGenReturnWrapping::Direct;
    std::string out_param_type_name;   // non-empty only for ResultOutParam
    std::string error_channel;         // e.g. "__ry_base64_get_last_error"; empty = none
    int require_list_u8_arg = -1;      // arg index requiring List<u8>; -1 = no check
};

// inferLibraryName: derives the descriptor's library_name from the
// declaration state per docs/architecture/native-call-boundary.md
// §"Library inference rule":
//
//   (a) explicit tag wins — non-empty `directiveTag` (the `@native("<lib>")`
//       argument) is returned as-is.
//   (b) module-keyed fallback — when `directiveTag` is empty and
//       `declaringModule` matches a name in `knownNativeLibs()`, returns
//       `declaringModule`. Otherwise returns nullopt (the symbol resolves
//       through `ry_lib` + the in-process search generator, no dlopen).
//
// Pure function — testable directly without driving a full compile.
std::optional<std::string> inferLibraryName(const std::string &directiveTag,
                                            const std::string &declaringModule);

// inferReturnWrapping: derives the descriptor's return_wrapping (and the
// out-param type name for ResultOutParam) from the Ry return type spelling.
// Pure function — moved here from the anonymous namespace in
// codegen_call_native.cpp so the descriptor population path (codegen_fn.cpp)
// and the dispatch consume path (emitGenericNativeCall) share one source.
//
// Returns {wrapping, out_param_type_name}. out_param_type_name is non-empty
// only when wrapping == ResultOutParam.
std::pair<CodeGenReturnWrapping, std::string>
inferReturnWrapping(const std::string &returnTypeName);

// knownNativeLibs: mirror of CMakeLists.txt's RY_NATIVE_LIBS list
// (12 entries: base64, path, convert, filesystem, gc, testing, io,
// json, json5, net, thread, http). Drives rule (b) inference. The
// CMake list is the source of truth; this C++ copy is hand-maintained.
// See tests/test_native_call_descriptor.cpp's KnownNativeLibsLocalLiteral
// test for the local-only consistency guard.
const std::unordered_set<std::string> &knownNativeLibs();

}  // namespace ry
