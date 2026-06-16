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
// Distinct from trimTypeNameWhitespace, which trims all std::isspace
// characters. The two coexist intentionally — typename string sources
// vary: some emit ASCII space only, others may include tabs/newlines
// on malformed inputs.
std::string trimTypeNameSpaces(const std::string &s);

// Trim std::isspace characters (space, tab, LF, CR, VT, FF) from both
// ends. Used by typename splitters that consume strings whose origins
// may include non-space whitespace (e.g. TypeNode::toString output on
// malformed input). Distinct from trimTypeNameSpaces by design.
std::string trimTypeNameWhitespace(const std::string &s);

// Split `body` on top-level commas, honoring nesting of <, >, (, ),
// [, ]. Differs from ry::util::splitTypeArgs in that splitTypeArgs
// tracks only <, >, (, ). Use this variant when the body may contain
// bracket-typed elements (List index in a tuple/fn signature). Each
// element is trimmed via trimTypeNameWhitespace.
std::vector<std::string> splitTopLevelCommas(const std::string &body);

// Split `argsStr` on top-level commas, honoring nesting of <, >, (, ).
// Unlike splitTopLevelCommas, this does NOT track [, ]. Used by codegen
// to split the inner of a generic type (e.g. "K, V" from "Map<K, V>")
// or the parameters of a function type. Elements are NOT trimmed —
// callers that need whitespace removal apply trimTypeNameSpaces or
// trimTypeNameWhitespace explicitly.
std::vector<std::string> splitTypeArgs(const std::string &argsStr);

// Given the index of an opening `(` in `s`, return the index of the
// matching `)` honoring nested parens. Returns std::string::npos when
// s[openParen] is not '(' or no matching close exists.
std::size_t findMatchingCloseParen(const std::string &s,
                                   std::size_t openParen);

// Split a union type string `"A | B | C"` on the ` | ` separator and
// trim ASCII spaces from each component. Empty components are dropped.
std::vector<std::string> parseUnionComponents(const std::string &typeName);

// Sort the union components alphabetically and join them with ` | `.
// Mutates `components` (sorts in place).
std::string normalizeUnionType(const std::string &typeName);

// `42` / `-7` / digit-only sequences (with optional leading `-`).
bool isIntLiteralType(const std::string &typeName);

// Single double-quoted literal `"N"` (exactly 2 quote chars). Rejects
// `"A" | "B"` because it has 4 quote chars.
bool isStrLiteralType(const std::string &typeName);

// Range constraint `"1..12"`: requires both sides to be isIntLiteralType.
// Rejects open-ended `"..5"` / `"1.."` and chained `"1..2..3"`.
bool isRangeType(const std::string &typeName);

// Union where every component is isIntLiteralType, or every component
// is isStrLiteralType. Mixed-kind unions return false.
bool isLiteralUnionType(const std::string &typeName);

// Predicate: any of isList / isMap / isSet.
bool isCollectionTypeName(const std::string &typeName);

// Strip the leading `"weak "` from a weak-typed name. Caller must
// ensure isWeakTypeName(typeName) first; this performs no validation.
std::string weakInnerTypeName(const std::string &typeName);

// Extract `K` from `"Map<K, V>"`. Returns "" if the input is malformed
// (not exactly 2 comma-separated args after stripping `Map<` / `>`).
// Caller must ensure isMapTypeName(typeName) first.
std::string extractMapKeyTypeName(const std::string &mapTypeName);

// Extract `V` from `"Map<K, V>"`. Same contract as extractMapKeyTypeName.
std::string extractMapValueTypeName(const std::string &mapTypeName);

// True when `name` is a low-level UNSIGNED numeric type name
// (u8 / u16 / u32 / u64). Treats any name starting with 'u' as
// unsigned; callers are responsible for first ensuring
// isLowLevelTypeName(name) when strict classification is required.
bool isUnsignedLowLevelName(const std::string &name);

// Parse `"(T1, T2, ...)"` into its element list. Returns false when the
// input is not in parenthesized form. A single-element tuple spells as
// `"(T,)"` and yields a 1-element list (the empty trailing slot is
// dropped). Uses splitTopLevelCommas internally so bracket-typed
// elements are honored.
bool splitTupleTypeName(const std::string &s,
                        std::vector<std::string> &elements);

// Parse `"fn(T1, T2) -> Ret"` into params and return type. Returns
// false when the input does not start with `"fn("` or the parens are
// unbalanced. `returnType` is empty when no `-> Ret` segment follows
// the closing paren.
bool splitFunctionTypeName(const std::string &s,
                           std::vector<std::string> &params,
                           std::string &returnType);

// Extract the Nth (0-indexed) type argument from a generic type string
// like `"Option<T>"`, `"T?"` (OptionalType shorthand for `Option<T>`),
// or `"Result<T, E>"`. Returns "" when `typeStr` does not start with
// `prefix` or the index is out of range.
//
// Behavior:
// - `prefix == "Option<"` AND `typeStr` ends with `'?'`: treat as the
//   `T?` shorthand. Only argIdx == 0 returns the inner; other indices
//   return "".
// - Otherwise: typeStr must start with `prefix` and end with `'>'`;
//   the body is split via splitTypeArgs and parts[argIdx] is returned
//   after trimTypeNameSpaces.
//
// Splits via splitTypeArgs so both angle-bracket and parenthesis depth
// are tracked correctly (handles function types such as
// "Option<(int, str) -> bool>").
std::string extractGenericTypeArg(const std::string &typeStr,
                                  const std::string &prefix,
                                  std::size_t argIdx);

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
