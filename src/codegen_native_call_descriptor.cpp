#include "ry/native_call_descriptor.hpp"

#include <cstddef>

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

std::pair<CodeGenReturnWrapping, std::string>
inferReturnWrapping(const std::string &returnType) {
    using RW = CodeGenReturnWrapping;

    // Result<T, Error> patterns
    if (returnType.size() > 7 && returnType.substr(0, 7) == "Result<") {
        // Extract the Ok type from Result<OkType, Error>, handling nested
        // generics like Result<Map<K, V>, Error> by counting bracket depth.
        int depth = 0;
        std::size_t commaPos = std::string::npos;
        for (std::size_t i = 7; i < returnType.size(); ++i) {
            if (returnType[i] == '<') ++depth;
            else if (returnType[i] == '>') --depth;
            else if (returnType[i] == ',' && depth == 0) { commaPos = i; break; }
        }
        if (commaPos == std::string::npos) return {RW::Direct, ""};
        std::string okType = returnType.substr(7, commaPos - 7);
        while (!okType.empty() && okType.back() == ' ') okType.pop_back();

        if (okType == "Unit")  return {RW::ResultStatus, ""};
        if (okType == "int")   return {RW::ResultOutParam, "int"};
        if (okType == "float") return {RW::ResultOutParam, "float"};
        if (okType == "bool")  return {RW::ResultOutParam, "int"};
        // str, List<...>, Map<...>, or any pointer type → ResultPtr
        return {RW::ResultPtr, ""};
    }

    if (returnType == "bool") return {RW::BoolFromI64, ""};

    // str, int, float, Unit, or any other type → Direct
    return {RW::Direct, ""};
}

}  // namespace ry
