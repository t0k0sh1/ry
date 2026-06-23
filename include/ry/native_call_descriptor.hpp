#pragma once

#include <optional>
#include <string>
#include <unordered_set>

namespace ry {

// NativeCallDescriptor: declarative @native call shape (foundation for
// docs/architecture/native-call-boundary.md follow-up #1, tracking #2299).
//
// v1 stores only `library_name` (the dlopen target inferred by
// inferLibraryName below). Subsequent consumer PRs add fields alongside
// the code that reads them, per the project YAGNI rule.
struct NativeCallDescriptor {
    std::optional<std::string> library_name;
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

// knownNativeLibs: mirror of CMakeLists.txt's RY_NATIVE_LIBS list
// (12 entries: base64, path, convert, filesystem, gc, testing, io,
// json, json5, net, thread, http). Drives rule (b) inference. The
// CMake list is the source of truth; this C++ copy is hand-maintained.
// See tests/test_native_call_descriptor.cpp's KnownNativeLibsLocalLiteral
// test for the local-only consistency guard.
const std::unordered_set<std::string> &knownNativeLibs();

}  // namespace ry
