#include "ry/util/type_name.hpp"

#include <algorithm>
#include <cctype>

namespace ry {
namespace util {

std::string trimTypeNameSpaces(const std::string &s) {
    size_t b = 0;
    while (b < s.size() && s[b] == ' ') ++b;
    size_t e = s.size();
    while (e > b && s[e - 1] == ' ') --e;
    return s.substr(b, e - b);
}

std::string trimTypeNameWhitespace(const std::string &s) {
    size_t a = 0;
    size_t b = s.size();
    while (a < b && std::isspace(static_cast<unsigned char>(s[a]))) ++a;
    while (b > a && std::isspace(static_cast<unsigned char>(s[b - 1]))) --b;
    return s.substr(a, b - a);
}

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
            out.push_back(trimTypeNameWhitespace(body.substr(start, i - start)));
            start = i + 1;
        }
    }
    out.push_back(trimTypeNameWhitespace(body.substr(start)));
    return out;
}

std::vector<std::string> splitTypeArgs(const std::string &argsStr) {
    std::vector<std::string> typeArgs;
    std::string curr;
    int angleBrackets = 0;
    int parens = 0;
    for (char c : argsStr) {
        if (c == '<') angleBrackets++;
        else if (c == '>') angleBrackets--;
        else if (c == '(') parens++;
        else if (c == ')') parens--;
        else if (c == ',' && angleBrackets == 0 && parens == 0) {
            typeArgs.push_back(curr);
            curr.clear();
            continue;
        }
        curr += c;
    }
    if (!curr.empty()) typeArgs.push_back(curr);
    return typeArgs;
}

std::size_t findMatchingCloseParen(const std::string &s, std::size_t openParen) {
    if (openParen >= s.size() || s[openParen] != '(')
        return std::string::npos;
    int depth = 1;
    for (std::size_t i = openParen + 1; i < s.size(); ++i) {
        if (s[i] == '(') ++depth;
        else if (s[i] == ')' && --depth == 0)
            return i;
    }
    return std::string::npos;
}

std::vector<std::string> parseUnionComponents(const std::string &typeName) {
    std::vector<std::string> components;
    size_t sepCount = 0;
    for (size_t p = 0; (p = typeName.find(" | ", p)) != std::string::npos; p += 3)
        ++sepCount;
    components.reserve(sepCount + 1);
    size_t start = 0;
    while (start < typeName.size()) {
        size_t pos = typeName.find(" | ", start);
        if (pos == std::string::npos) {
            std::string comp = typeName.substr(start);
            size_t s = comp.find_first_not_of(' ');
            size_t e = comp.find_last_not_of(' ');
            if (s != std::string::npos)
                components.push_back(comp.substr(s, e - s + 1));
            break;
        }
        std::string comp = typeName.substr(start, pos - start);
        size_t s = comp.find_first_not_of(' ');
        size_t e = comp.find_last_not_of(' ');
        if (s != std::string::npos)
            components.push_back(comp.substr(s, e - s + 1));
        start = pos + 3;
    }
    return components;
}

std::string normalizeUnionType(const std::string &typeName) {
    auto components = parseUnionComponents(typeName);
    std::sort(components.begin(), components.end());
    std::string result;
    for (size_t i = 0; i < components.size(); ++i) {
        if (i > 0) result += " | ";
        result += components[i];
    }
    return result;
}

bool isIntLiteralType(const std::string &typeName) {
    if (typeName.empty()) return false;
    size_t start = (typeName[0] == '-') ? 1 : 0;
    if (start >= typeName.size()) return false;
    for (size_t i = start; i < typeName.size(); ++i) {
        if (!std::isdigit(static_cast<unsigned char>(typeName[i]))) return false;
    }
    return true;
}

bool isStrLiteralType(const std::string &typeName) {
    if (typeName.size() < 2 || typeName.front() != '"' || typeName.back() != '"')
        return false;
    // Ensure it's a single quoted string, not a union like "N" | "S"
    size_t quoteCount = 0;
    for (char c : typeName)
        if (c == '"') quoteCount++;
    return quoteCount == 2;
}

bool isRangeType(const std::string &typeName) {
    auto pos = typeName.find("..");
    if (pos == std::string::npos || pos == 0 || pos == typeName.size() - 2)
        return false;
    std::string lo = typeName.substr(0, pos);
    std::string hi = typeName.substr(pos + 2);
    return isIntLiteralType(lo) && isIntLiteralType(hi);
}

bool isLiteralUnionType(const std::string &typeName) {
    if (typeName.find(" | ") == std::string::npos) return false;
    auto components = parseUnionComponents(typeName);
    if (components.empty()) return false;
    bool allInt = true, allStr = true;
    for (auto &c : components) {
        if (allInt && !isIntLiteralType(c)) allInt = false;
        if (allStr && !isStrLiteralType(c)) allStr = false;
        if (!allInt && !allStr) return false;
    }
    return allInt || allStr;
}

bool isCollectionTypeName(const std::string &typeName) {
    return isListTypeName(typeName) || isMapTypeName(typeName) || isSetTypeName(typeName);
}

std::string weakInnerTypeName(const std::string &typeName) {
    return typeName.substr(5);
}

std::string extractMapKeyTypeName(const std::string &mapTypeName) {
    std::string inner = mapTypeName.substr(4, mapTypeName.size() - 5);
    auto parts = splitTypeArgs(inner);
    if (parts.size() != 2) return "";
    return trimTypeNameSpaces(parts[0]);
}

std::string extractMapValueTypeName(const std::string &mapTypeName) {
    std::string inner = mapTypeName.substr(4, mapTypeName.size() - 5);
    auto parts = splitTypeArgs(inner);
    if (parts.size() != 2) return "";
    return trimTypeNameSpaces(parts[1]);
}

bool isUnsignedLowLevelName(const std::string &name) {
    return !name.empty() && name[0] == 'u';
}

bool splitTupleTypeName(const std::string &s, std::vector<std::string> &elements) {
    std::string t = trimTypeNameWhitespace(s);
    if (t.size() < 2 || t.front() != '(' || t.back() != ')') return false;
    std::string body = t.substr(1, t.size() - 2);
    // Single-element tuples spell as "(x,)" — drop the empty trailing slice.
    elements = splitTopLevelCommas(body);
    if (!elements.empty() && elements.back().empty())
        elements.pop_back();
    return true;
}

std::string extractGenericTypeArg(const std::string &typeStr,
                                  const std::string &prefix,
                                  std::size_t argIdx) {
    // T? suffix is OptionalType::toString() shorthand for Option<T>
    // (#1003, #1015). Only Option has a shorthand form; Result<T, E>
    // does not.
    if (prefix == "Option<" && typeStr.size() > 1 && typeStr.back() == '?') {
        if (argIdx != 0)
            return {};
        return trimTypeNameSpaces(typeStr.substr(0, typeStr.size() - 1));
    }
    if (typeStr.size() <= prefix.size() ||
        typeStr.compare(0, prefix.size(), prefix) != 0 ||
        typeStr.back() != '>')
        return {};
    const std::string inner = typeStr.substr(prefix.size(),
                                              typeStr.size() - prefix.size() - 1);
    const auto parts = splitTypeArgs(inner);
    if (argIdx >= parts.size()) return {};
    return trimTypeNameSpaces(parts[argIdx]);
}

bool splitFunctionTypeName(const std::string &s,
                           std::vector<std::string> &params,
                           std::string &returnType) {
    std::string t = trimTypeNameWhitespace(s);
    if (t.rfind("fn(", 0) != 0)
        return false;
    const std::string prefix = "fn(";
    int depth = 1;
    size_t i = prefix.size();
    for (; i < t.size() && depth > 0; ++i) {
        if (t[i] == '(') depth++;
        else if (t[i] == ')') depth--;
        if (depth == 0) break;
    }
    if (depth != 0) return false;
    std::string body = t.substr(prefix.size(), i - prefix.size());
    params = splitTopLevelCommas(body);
    if (params.size() == 1 && params[0].empty()) params.clear();
    size_t j = i + 1;
    while (j < t.size() && std::isspace(static_cast<unsigned char>(t[j]))) ++j;
    if (j < t.size() && t[j] == '-' && j + 1 < t.size() && t[j + 1] == '>') {
        j += 2;
        while (j < t.size() && std::isspace(static_cast<unsigned char>(t[j]))) ++j;
        returnType = trimTypeNameWhitespace(t.substr(j));
    } else {
        returnType.clear();
    }
    return true;
}

bool splitGenericTypeName(const std::string &s,
                          std::string &head,
                          std::vector<std::string> &inner) {
    std::string t = trimTypeNameWhitespace(s);
    size_t lt = t.find('<');
    if (lt == std::string::npos) return false;
    if (t.back() != '>') return false;
    head = trimTypeNameWhitespace(t.substr(0, lt));
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
