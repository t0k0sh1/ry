#include "ry/util/type_name.hpp"

#include <algorithm>
#include <cctype>

namespace ry {
namespace util {

namespace {

// trimWs / splitTopLevelCommas are intentionally duplicated from the
// file-local statics in src/codegen_fn_generic.cpp where they support
// splitTupleTypeName / splitFunctionTypeName. Consolidation is deferred
// per #1820's scoped extraction (those siblings remain file-local
// because they are out of scope for the v0.0.26 stage 2 cleanup).

// Local ASCII-whitespace trim used by splitGenericTypeName and
// splitTopLevelCommas. Differs from trimTypeNameSpaces (which trims
// only ' ') because the type-name splitters consume strings from
// TypeNode::toString() that may contain tabs/newlines on malformed
// inputs.
std::string trimWs(const std::string &s) {
    size_t a = 0;
    size_t b = s.size();
    while (a < b && std::isspace(static_cast<unsigned char>(s[a]))) ++a;
    while (b > a && std::isspace(static_cast<unsigned char>(s[b - 1]))) --b;
    return s.substr(a, b - a);
}

// Split `body` on top-level commas, honoring nesting of <, >, (, ),
// [, ]. Used by splitGenericTypeName for the inner argument list.
std::vector<std::string> splitTopLevelCommas(const std::string &body) {
    std::vector<std::string> out;
    out.reserve(static_cast<size_t>(std::count(body.begin(), body.end(), ',')) + 1);
    int depth = 0;
    size_t start = 0;
    for (size_t i = 0; i < body.size(); ++i) {
        char c = body[i];
        if (c == '<' || c == '(' || c == '[') {
            depth++;
        } else if (c == '>' || c == ')' || c == ']') {
            if (depth > 0) depth--;
        } else if (c == ',' && depth == 0) {
            out.push_back(trimWs(body.substr(start, i - start)));
            start = i + 1;
        }
    }
    out.push_back(trimWs(body.substr(start)));
    return out;
}

}  // namespace

std::string trimTypeNameSpaces(const std::string &s) {
    size_t b = 0;
    while (b < s.size() && s[b] == ' ') ++b;
    size_t e = s.size();
    while (e > b && s[e - 1] == ' ') --e;
    return s.substr(b, e - b);
}

bool splitGenericTypeName(const std::string &s,
                          std::string &head,
                          std::vector<std::string> &inner) {
    std::string t = trimWs(s);
    size_t lt = t.find('<');
    if (lt == std::string::npos) return false;
    if (t.back() != '>') return false;
    head = trimWs(t.substr(0, lt));
    std::string body = t.substr(lt + 1, t.size() - lt - 2);
    inner = splitTopLevelCommas(body);
    return true;
}

bool isListTypeName(const std::string &typeName) {
    return typeName.size() > 5 && typeName.compare(0, 5, "List<") == 0;
}

bool isMapTypeName(const std::string &typeName) {
    return typeName.size() > 4 && typeName.compare(0, 4, "Map<") == 0;
}

bool isSetTypeName(const std::string &typeName) {
    return typeName.size() > 4 && typeName.compare(0, 4, "Set<") == 0;
}

bool isWeakTypeName(const std::string &typeName) {
    return typeName.size() > 5 && typeName.compare(0, 5, "weak ") == 0;
}

bool isLowLevelTypeName(const std::string &name) {
    return name == "i8" || name == "i16" || name == "i32" || name == "i64" ||
           name == "u8" || name == "u16" || name == "u32" || name == "u64" || name == "f32";
}

std::string deriveRuntimeFnName(const std::string &package,
                                const std::string &fn_name) {
    if (package.empty()) return "__ry_" + fn_name;
    return "__ry_" + package + "_" + fn_name;
}

}  // namespace util
}  // namespace ry
