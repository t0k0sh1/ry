#include "ry/native_call_descriptor.hpp"

namespace ry {

const std::unordered_set<std::string> &knownNativeLibs() {
    // Mirror of CMakeLists.txt:386 RY_NATIVE_LIBS. Keep these in sync.
    // KnownNativeLibsLocalLiteral test catches a local-only edit drift
    // (this list vs the test's expected literal); the CMake cross-file
    // invariant is hand-maintained and is not caught by any automated
    // guard today.
    static const std::unordered_set<std::string> kKnownNativeLibs = {
        "base64",
        "path",
        "convert",
        "filesystem",
        "gc",
        "testing",
        "io",
        "json",
        "json5",
        "net",
        "thread",
        "http",
    };
    return kKnownNativeLibs;
}

std::optional<std::string> inferLibraryName(const std::string &directiveTag,
                                            const std::string &declaringModule) {
    if (!directiveTag.empty())
        return directiveTag;
    if (!declaringModule.empty() && knownNativeLibs().count(declaringModule))
        return declaringModule;
    return std::nullopt;
}

}  // namespace ry
