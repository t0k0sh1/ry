#pragma once

#include <string>
#include <vector>

namespace ry {
namespace util {

// Pure string utilities operating on Ry source-level type names.
// These helpers carry no codegen state and may be called from any
// layer (lexer, parser, sema, codegen, runtime). They were extracted
// from CodeGen by issue #1820 as part of the v0.0.26 boundary cleanup.

// Trim spaces (ASCII ' ' only) from both ends.
std::string trimTypeNameSpaces(const std::string &s);

// Split a generic type name like "List<int>" into head ("List") and
// comma-separated inner args (["int"]). Returns false when the input
// is not in `Head<...>` shape.
bool splitGenericTypeName(const std::string &s,
                          std::string &head,
                          std::vector<std::string> &inner);

bool isListTypeName(const std::string &typeName);
bool isMapTypeName(const std::string &typeName);
bool isSetTypeName(const std::string &typeName);
bool isWeakTypeName(const std::string &typeName);

// Recognises `fn(...)` function type names produced by TypeNode::toString.
inline bool isFunctionTypeName(const std::string &s) {
    return s.size() > 3 && s.compare(0, 3, "fn(") == 0;
}

// Low-level numeric type names (i8 / i16 / i32 / i64 / u8 / u16 / u32 / u64 / f32).
// Used by codegen to distinguish native-width arithmetic from default int/float.
bool isLowLevelTypeName(const std::string &name);

// Build the runtime symbol name for a stdlib module function.
// Empty package returns "__ry_<fn>"; non-empty returns "__ry_<pkg>_<fn>".
std::string deriveRuntimeFnName(const std::string &package,
                                const std::string &fn_name);

// Build the registry key for native_fn_sigs_. The format must stay in
// sync with how CodeGen's native_fn_sigs_ map indexes signatures.
inline std::string nativeSigKey(const std::string &package,
                                const std::string &name) {
    return package.empty() ? name : package + "::" + name;
}

}  // namespace util
}  // namespace ry
