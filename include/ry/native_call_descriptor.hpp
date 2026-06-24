#pragma once

#include <optional>
#include <string>
#include <unordered_set>
#include <utility>

#include "ry/codegen_native_dispatch.hpp"  // CodeGenReturnWrapping
#include "ry/stdlib_registry.hpp"  // ResourceKindRegistry::NONE

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
//
// Installment 2-a (#2338) adds `resource_kind` for resource-coupled
// modules (io, filesystem, net, http, thread). When non-NONE the consume
// path attaches the kind to the wrapped result via addResourceKind.
struct NativeCallDescriptor {
    std::optional<std::string> library_name;

    // Pilot fields (#2337): populated at @native declaration time so
    // emitGenericNativeCall consumes pre-computed values instead of
    // re-inferring them at every call site.
    CodeGenReturnWrapping return_wrapping = CodeGenReturnWrapping::Direct;
    std::string out_param_type_name;   // non-empty only for ResultOutParam
    std::string error_channel;         // e.g. "__ry_base64_get_last_error"; empty = none
    int require_list_u8_arg = -1;      // arg index requiring List<u8>; -1 = no check

    // Installment 2-a (#2338): ResourceKindRegistry index for resource-
    // returning natives (e.g. io::open returns Result<File, Error>; the
    // descriptor carries rk_file so the consume path tags the wrapped
    // result automatically). NONE (-1) means "not a resource".
    int resource_kind = ResourceKindRegistry::NONE;
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
// extractResultOkType: returns the trimmed Ok type from "Result<T, Error>"
// (e.g. "File" from "Result<File, Error>"), or an empty string when the
// input does not name a Result<T, _>. Pure depth-aware comma finder used
// by inferReturnWrapping, inferResourceKind, and the return-type metadata
// propagation path in emitGenericNativeCall — keep one copy.
std::string extractResultOkType(const std::string &returnTypeName);

std::pair<CodeGenReturnWrapping, std::string>
inferReturnWrapping(const std::string &returnTypeName);

// inferResourceKind: extracts the inner type name from a return type
// spelling and looks it up against ResourceKindRegistry. Returns the
// registered kind id when the inner type names a registered resource
// (e.g. "Result<File, Error>" -> rk_file), or ResourceKindRegistry::NONE
// otherwise. Mirrors inferReturnWrapping in shape (pure, declaration-
// time) so codegen_fn.cpp populates both at @native registration time.
//
// Result wrapping is the only currently-supported carrier: a bare
// resource return type (e.g. `fn open() -> File`) would also resource-
// tag the value, but no stdlib uses that shape today. Direct returns
// are intentionally excluded to keep the inference's blast radius
// scoped to error-throwable resource constructors.
int inferResourceKind(const std::string &returnTypeName);

// knownNativeLibs: mirror of CMakeLists.txt's RY_NATIVE_LIBS list
// (12 entries: base64, path, convert, filesystem, gc, testing, io,
// json, json5, net, thread, http). Drives rule (b) inference. The
// CMake list is the source of truth; this C++ copy is hand-maintained.
// See tests/test_native_call_descriptor.cpp's KnownNativeLibsLocalLiteral
// test for the local-only consistency guard.
const std::unordered_set<std::string> &knownNativeLibs();

}  // namespace ry
