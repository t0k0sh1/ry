#include "ry/util/type_name.hpp"

#include <gtest/gtest.h>

namespace {

using ry::util::deriveRuntimeFnName;
using ry::util::isFunctionTypeName;
using ry::util::isListTypeName;
using ry::util::isLowLevelTypeName;
using ry::util::isMapTypeName;
using ry::util::isSetTypeName;
using ry::util::isWeakTypeName;
using ry::util::nativeSigKey;
using ry::util::extractGenericTypeArg;
using ry::util::extractMapKeyTypeName;
using ry::util::extractMapValueTypeName;
using ry::util::findMatchingCloseParen;
using ry::util::isCollectionTypeName;
using ry::util::isIntLiteralType;
using ry::util::isLiteralUnionType;
using ry::util::isRangeType;
using ry::util::isStrLiteralType;
using ry::util::isUnsignedLowLevelName;
using ry::util::normalizeUnionType;
using ry::util::parseUnionComponents;
using ry::util::splitFunctionTypeName;
using ry::util::splitGenericTypeName;
using ry::util::splitTopLevelCommas;
using ry::util::splitTupleTypeName;
using ry::util::splitTypeArgs;
using ry::util::trimTypeNameSpaces;
using ry::util::trimTypeNameWhitespace;
using ry::util::weakInnerTypeName;

TEST(UtilTypeName, TrimTypeNameSpacesStripsLeadingTrailingSpacesOnly) {
    EXPECT_EQ(trimTypeNameSpaces("  int  "), "int");
    EXPECT_EQ(trimTypeNameSpaces("int"), "int");
    EXPECT_EQ(trimTypeNameSpaces(""), "");
    EXPECT_EQ(trimTypeNameSpaces("a b"), "a b");
}

TEST(UtilTypeName, TrimTypeNameWhitespaceTrimsAllIsspaceChars) {
    // Distinct from trimTypeNameSpaces: covers tab, CR, LF, VT, FF.
    EXPECT_EQ(trimTypeNameWhitespace("\tint\t"), "int");
    EXPECT_EQ(trimTypeNameWhitespace("\nint\n"), "int");
    EXPECT_EQ(trimTypeNameWhitespace("\r\n int \r\n"), "int");
    EXPECT_EQ(trimTypeNameWhitespace("  int  "), "int");
    EXPECT_EQ(trimTypeNameWhitespace(""), "");
    // Internal whitespace preserved.
    EXPECT_EQ(trimTypeNameWhitespace("a\tb"), "a\tb");
}

TEST(UtilTypeName, SplitTypeArgsTracksAngleAndParenDepth) {
    auto parts = splitTypeArgs("int,str");
    ASSERT_EQ(parts.size(), 2u);
    EXPECT_EQ(parts[0], "int");
    EXPECT_EQ(parts[1], "str");

    // Nested generic: inner comma is at angle-depth 1, not top-level.
    parts = splitTypeArgs("Map<str,int>,List<bool>");
    ASSERT_EQ(parts.size(), 2u);
    EXPECT_EQ(parts[0], "Map<str,int>");
    EXPECT_EQ(parts[1], "List<bool>");

    // Tuple (paren depth): inner comma at paren-depth 1 is not top-level.
    parts = splitTypeArgs("(int,str),bool");
    ASSERT_EQ(parts.size(), 2u);
    EXPECT_EQ(parts[0], "(int,str)");
    EXPECT_EQ(parts[1], "bool");

    // Empty input: returns empty vector (NOT 1-element vector — differs
    // from splitTopLevelCommas which returns ["" ] for empty body).
    parts = splitTypeArgs("");
    EXPECT_TRUE(parts.empty());

    // Elements NOT trimmed (callers apply trim explicitly).
    parts = splitTypeArgs(" int , str ");
    ASSERT_EQ(parts.size(), 2u);
    EXPECT_EQ(parts[0], " int ");
    EXPECT_EQ(parts[1], " str ");
}

TEST(UtilTypeName, ParseUnionComponentsSplitsOnPipeWithSpaceTrim) {
    auto parts = parseUnionComponents("int | str | bool");
    ASSERT_EQ(parts.size(), 3u);
    EXPECT_EQ(parts[0], "int");
    EXPECT_EQ(parts[1], "str");
    EXPECT_EQ(parts[2], "bool");

    // Single-component "union" returns 1-element vector.
    parts = parseUnionComponents("int");
    ASSERT_EQ(parts.size(), 1u);
    EXPECT_EQ(parts[0], "int");

    // Leading/trailing whitespace inside components is trimmed.
    parts = parseUnionComponents("  int  |  str  ");
    ASSERT_EQ(parts.size(), 2u);
    EXPECT_EQ(parts[0], "int");
    EXPECT_EQ(parts[1], "str");

    // Empty input: returns empty vector.
    parts = parseUnionComponents("");
    EXPECT_TRUE(parts.empty());
}

TEST(UtilTypeName, NormalizeUnionTypeSortsComponentsAlphabetically) {
    EXPECT_EQ(normalizeUnionType("str | int | bool"), "bool | int | str");
    EXPECT_EQ(normalizeUnionType("int"), "int");
    EXPECT_EQ(normalizeUnionType("\"N\" | \"S\" | \"E\" | \"W\""),
              "\"E\" | \"N\" | \"S\" | \"W\"");
}

TEST(UtilTypeName, IsIntLiteralTypeAcceptsDigitsAndOptionalSign) {
    EXPECT_TRUE(isIntLiteralType("0"));
    EXPECT_TRUE(isIntLiteralType("42"));
    EXPECT_TRUE(isIntLiteralType("-1"));
    EXPECT_TRUE(isIntLiteralType("12345"));
    EXPECT_FALSE(isIntLiteralType(""));
    EXPECT_FALSE(isIntLiteralType("-"));
    EXPECT_FALSE(isIntLiteralType("1a"));
    EXPECT_FALSE(isIntLiteralType("1.5"));
    EXPECT_FALSE(isIntLiteralType("int"));
}

TEST(UtilTypeName, IsStrLiteralTypeRequiresExactlyTwoQuotes) {
    EXPECT_TRUE(isStrLiteralType("\"N\""));
    EXPECT_TRUE(isStrLiteralType("\"hello\""));
    EXPECT_TRUE(isStrLiteralType("\"\""));
    // Union of literals has 4 quotes — must be rejected.
    EXPECT_FALSE(isStrLiteralType("\"A\" | \"B\""));
    EXPECT_FALSE(isStrLiteralType("\""));
    EXPECT_FALSE(isStrLiteralType(""));
    EXPECT_FALSE(isStrLiteralType("str"));
    EXPECT_FALSE(isStrLiteralType("'N'"));  // single-quote not accepted
}

TEST(UtilTypeName, IsRangeTypeRequiresIntLiteralOnBothSides) {
    EXPECT_TRUE(isRangeType("1..12"));
    EXPECT_TRUE(isRangeType("-5..5"));
    EXPECT_TRUE(isRangeType("0..100"));
    // Open-ended rejected.
    EXPECT_FALSE(isRangeType("1.."));
    EXPECT_FALSE(isRangeType("..5"));
    // Triple-dot not supported.
    EXPECT_FALSE(isRangeType("1..2..3"));
    // Non-int sides rejected.
    EXPECT_FALSE(isRangeType("\"a\"..\"z\""));
    EXPECT_FALSE(isRangeType(""));
}

TEST(UtilTypeName, IsLiteralUnionTypeAcceptsHomogeneousIntOrStrUnions) {
    EXPECT_TRUE(isLiteralUnionType("1 | 2 | 3"));
    EXPECT_TRUE(isLiteralUnionType("\"N\" | \"S\" | \"E\" | \"W\""));
    EXPECT_TRUE(isLiteralUnionType("-1 | 0 | 1"));
    // Mixed int + str rejected.
    EXPECT_FALSE(isLiteralUnionType("1 | \"a\""));
    // Non-literal components rejected.
    EXPECT_FALSE(isLiteralUnionType("int | str"));
    // Single component is NOT a union.
    EXPECT_FALSE(isLiteralUnionType("\"N\""));
    EXPECT_FALSE(isLiteralUnionType("1"));
    EXPECT_FALSE(isLiteralUnionType(""));
}

TEST(UtilTypeName, ExtractGenericTypeArgHandlesOptionResultAndShorthand) {
    // Option<T>: idx 0 returns T.
    EXPECT_EQ(extractGenericTypeArg("Option<int>", "Option<", 0), "int");
    // Option<T> idx 1 returns "" (no second arg).
    EXPECT_EQ(extractGenericTypeArg("Option<int>", "Option<", 1), "");

    // T? shorthand: only argIdx==0 returns inner.
    EXPECT_EQ(extractGenericTypeArg("int?", "Option<", 0), "int");
    EXPECT_EQ(extractGenericTypeArg("int?", "Option<", 1), "");
    EXPECT_EQ(extractGenericTypeArg("List<int>?", "Option<", 0), "List<int>");

    // T? shorthand only applies when prefix is "Option<".
    EXPECT_EQ(extractGenericTypeArg("int?", "Result<", 0), "");

    // Result<T, E>: idx 0 = T, idx 1 = E.
    EXPECT_EQ(extractGenericTypeArg("Result<int, str>", "Result<", 0), "int");
    EXPECT_EQ(extractGenericTypeArg("Result<int, str>", "Result<", 1), "str");
    EXPECT_EQ(extractGenericTypeArg("Result<int, str>", "Result<", 2), "");

    // Nested generic + paren-aware: Option<(int, str) -> bool>.
    EXPECT_EQ(extractGenericTypeArg("Option<(int, str) -> bool>", "Option<", 0),
              "(int, str) -> bool");

    // Mismatched prefix returns "".
    EXPECT_EQ(extractGenericTypeArg("Option<int>", "Result<", 0), "");
    EXPECT_EQ(extractGenericTypeArg("int", "Option<", 0), "");

    // Empty / minimal inputs.
    EXPECT_EQ(extractGenericTypeArg("", "Option<", 0), "");
    EXPECT_EQ(extractGenericTypeArg("Option<>", "Option<", 0), "");
}

TEST(UtilTypeName, IsCollectionTypeNameUnionsListMapSet) {
    EXPECT_TRUE(isCollectionTypeName("List<int>"));
    EXPECT_TRUE(isCollectionTypeName("Map<str, int>"));
    EXPECT_TRUE(isCollectionTypeName("Set<int>"));
    EXPECT_FALSE(isCollectionTypeName("int"));
    EXPECT_FALSE(isCollectionTypeName("weak List<int>"));
    EXPECT_FALSE(isCollectionTypeName(""));
}

TEST(UtilTypeName, WeakInnerTypeNameStripsLeadingFiveChars) {
    EXPECT_EQ(weakInnerTypeName("weak str"), "str");
    EXPECT_EQ(weakInnerTypeName("weak List<int>"), "List<int>");
    EXPECT_EQ(weakInnerTypeName("weak MyRecord"), "MyRecord");
}

TEST(UtilTypeName, ExtractMapKeyAndValueTypeNamesParseInner) {
    EXPECT_EQ(extractMapKeyTypeName("Map<str, int>"), "str");
    EXPECT_EQ(extractMapValueTypeName("Map<str, int>"), "int");
    EXPECT_EQ(extractMapKeyTypeName("Map<List<int>, str>"), "List<int>");
    EXPECT_EQ(extractMapValueTypeName("Map<List<int>, str>"), "str");
    // Malformed (not 2 parts) returns empty.
    EXPECT_EQ(extractMapKeyTypeName("Map<int>"), "");
    EXPECT_EQ(extractMapValueTypeName("Map<int>"), "");
}

TEST(UtilTypeName, IsUnsignedLowLevelNameTreatsUPrefixAsUnsigned) {
    EXPECT_TRUE(isUnsignedLowLevelName("u8"));
    EXPECT_TRUE(isUnsignedLowLevelName("u16"));
    EXPECT_TRUE(isUnsignedLowLevelName("u32"));
    EXPECT_TRUE(isUnsignedLowLevelName("u64"));
    EXPECT_FALSE(isUnsignedLowLevelName("i8"));
    EXPECT_FALSE(isUnsignedLowLevelName("i64"));
    EXPECT_FALSE(isUnsignedLowLevelName("f32"));
    EXPECT_FALSE(isUnsignedLowLevelName("int"));
    EXPECT_FALSE(isUnsignedLowLevelName(""));
}

TEST(UtilTypeName, SplitTupleTypeNameParsesParenForm) {
    std::vector<std::string> elems;

    EXPECT_TRUE(splitTupleTypeName("(int, str)", elems));
    ASSERT_EQ(elems.size(), 2u);
    EXPECT_EQ(elems[0], "int");
    EXPECT_EQ(elems[1], "str");

    // Single-element tuple "(T,)" yields 1 element (trailing empty dropped).
    EXPECT_TRUE(splitTupleTypeName("(int,)", elems));
    ASSERT_EQ(elems.size(), 1u);
    EXPECT_EQ(elems[0], "int");

    // Nested generic inside tuple.
    EXPECT_TRUE(splitTupleTypeName("(Map<str, int>, List<bool>)", elems));
    ASSERT_EQ(elems.size(), 2u);
    EXPECT_EQ(elems[0], "Map<str, int>");
    EXPECT_EQ(elems[1], "List<bool>");

    // Not parenthesized.
    EXPECT_FALSE(splitTupleTypeName("int, str", elems));
    EXPECT_FALSE(splitTupleTypeName("", elems));
    // Empty tuple "()" is accepted (legacy behavior — yields empty elems).
    EXPECT_TRUE(splitTupleTypeName("()", elems));
    EXPECT_TRUE(elems.empty());
}

TEST(UtilTypeName, SplitFunctionTypeNameParsesFnForm) {
    std::vector<std::string> params;
    std::string ret;

    EXPECT_TRUE(splitFunctionTypeName("fn(int, str) -> bool", params, ret));
    ASSERT_EQ(params.size(), 2u);
    EXPECT_EQ(params[0], "int");
    EXPECT_EQ(params[1], "str");
    EXPECT_EQ(ret, "bool");

    // No params.
    EXPECT_TRUE(splitFunctionTypeName("fn() -> int", params, ret));
    EXPECT_TRUE(params.empty());
    EXPECT_EQ(ret, "int");

    // No return type.
    EXPECT_TRUE(splitFunctionTypeName("fn(int)", params, ret));
    ASSERT_EQ(params.size(), 1u);
    EXPECT_EQ(params[0], "int");
    EXPECT_EQ(ret, "");

    // Not fn shape.
    EXPECT_FALSE(splitFunctionTypeName("int", params, ret));
    EXPECT_FALSE(splitFunctionTypeName("(int) -> bool", params, ret));  // no "fn"
}

TEST(UtilTypeName, FindMatchingCloseParenWalksParenDepth) {
    // Simple match.
    EXPECT_EQ(findMatchingCloseParen("()", 0), 1u);
    EXPECT_EQ(findMatchingCloseParen("(int)", 0), 4u);

    // Nested parens.
    EXPECT_EQ(findMatchingCloseParen("(fn(int) -> bool)", 0), 16u);
    EXPECT_EQ(findMatchingCloseParen("(fn(int) -> bool)", 3), 7u);

    // Open paren not at openParen index: returns npos.
    EXPECT_EQ(findMatchingCloseParen("int", 0), std::string::npos);

    // No matching close: returns npos.
    EXPECT_EQ(findMatchingCloseParen("(", 0), std::string::npos);
    EXPECT_EQ(findMatchingCloseParen("(int", 0), std::string::npos);

    // openParen >= s.size(): returns npos.
    EXPECT_EQ(findMatchingCloseParen("()", 10), std::string::npos);
}

TEST(UtilTypeName, SplitTopLevelCommasHonorsNestingForAllBracketKinds) {
    auto parts = splitTopLevelCommas("int, str");
    ASSERT_EQ(parts.size(), 2u);
    EXPECT_EQ(parts[0], "int");
    EXPECT_EQ(parts[1], "str");

    // Angle-bracket nesting: inner ',' is NOT a top-level split.
    parts = splitTopLevelCommas("Map<str, int>, List<bool>");
    ASSERT_EQ(parts.size(), 2u);
    EXPECT_EQ(parts[0], "Map<str, int>");
    EXPECT_EQ(parts[1], "List<bool>");

    // Paren nesting (fn types).
    parts = splitTopLevelCommas("fn(int, str) -> bool, int");
    ASSERT_EQ(parts.size(), 2u);
    EXPECT_EQ(parts[0], "fn(int, str) -> bool");
    EXPECT_EQ(parts[1], "int");

    // Bracket nesting (distinguishes from splitTypeArgs).
    parts = splitTopLevelCommas("a[1, 2], b");
    ASSERT_EQ(parts.size(), 2u);
    EXPECT_EQ(parts[0], "a[1, 2]");
    EXPECT_EQ(parts[1], "b");

    // Single-element body: returns 1-element vector.
    parts = splitTopLevelCommas("int");
    ASSERT_EQ(parts.size(), 1u);
    EXPECT_EQ(parts[0], "int");

    // Empty body: returns 1-element vector with empty string.
    parts = splitTopLevelCommas("");
    ASSERT_EQ(parts.size(), 1u);
    EXPECT_EQ(parts[0], "");

    // Each element is trimmed via trimTypeNameWhitespace.
    parts = splitTopLevelCommas("  int  ,\tstr\t");
    ASSERT_EQ(parts.size(), 2u);
    EXPECT_EQ(parts[0], "int");
    EXPECT_EQ(parts[1], "str");
}

TEST(UtilTypeName, SplitGenericTypeNameParsesListAndMap) {
    std::string head;
    std::vector<std::string> inner;

    EXPECT_TRUE(splitGenericTypeName("List<int>", head, inner));
    EXPECT_EQ(head, "List");
    ASSERT_EQ(inner.size(), 1u);
    EXPECT_EQ(inner[0], "int");

    EXPECT_TRUE(splitGenericTypeName("Map<str, int>", head, inner));
    EXPECT_EQ(head, "Map");
    ASSERT_EQ(inner.size(), 2u);
    EXPECT_EQ(inner[0], "str");
    EXPECT_EQ(inner[1], "int");
}

TEST(UtilTypeName, SplitGenericTypeNameHandlesNestedGenerics) {
    std::string head;
    std::vector<std::string> inner;

    EXPECT_TRUE(splitGenericTypeName("List<Map<str, int>>", head, inner));
    EXPECT_EQ(head, "List");
    ASSERT_EQ(inner.size(), 1u);
    EXPECT_EQ(inner[0], "Map<str, int>");
}

TEST(UtilTypeName, SplitGenericTypeNameRejectsNonGeneric) {
    std::string head;
    std::vector<std::string> inner;
    EXPECT_FALSE(splitGenericTypeName("int", head, inner));
    EXPECT_FALSE(splitGenericTypeName("List<int", head, inner));
}

TEST(UtilTypeName, IsListTypeNameMatchesPrefix) {
    EXPECT_TRUE(isListTypeName("List<int>"));
    EXPECT_TRUE(isListTypeName("List<List<int>>"));
    EXPECT_FALSE(isListTypeName("List"));
    EXPECT_FALSE(isListTypeName("Map<str, int>"));
    EXPECT_FALSE(isListTypeName(""));
}

TEST(UtilTypeName, IsMapTypeNameMatchesPrefix) {
    EXPECT_TRUE(isMapTypeName("Map<str, int>"));
    EXPECT_FALSE(isMapTypeName("Map"));
    EXPECT_FALSE(isMapTypeName("List<int>"));
}

TEST(UtilTypeName, IsSetTypeNameMatchesPrefix) {
    EXPECT_TRUE(isSetTypeName("Set<int>"));
    EXPECT_FALSE(isSetTypeName("Set"));
    EXPECT_FALSE(isSetTypeName("List<int>"));
}

TEST(UtilTypeName, IsWeakTypeNameRequiresWeakSpacePrefix) {
    EXPECT_TRUE(isWeakTypeName("weak Point"));
    EXPECT_FALSE(isWeakTypeName("weak"));
    EXPECT_FALSE(isWeakTypeName("Weak Point"));
}

TEST(UtilTypeName, IsFunctionTypeNameMatchesFnPrefix) {
    EXPECT_TRUE(isFunctionTypeName("fn() -> int"));
    EXPECT_TRUE(isFunctionTypeName("fn(int, int) -> int"));
    EXPECT_FALSE(isFunctionTypeName("fn"));
    EXPECT_FALSE(isFunctionTypeName("Function<int>"));
}

TEST(UtilTypeName, IsTupleTypeNameMatchesParenthesizedShape) {
    EXPECT_TRUE(ry::util::isTupleTypeName("(int, str)"));
    EXPECT_TRUE(ry::util::isTupleTypeName("(int, Map<str, int>)"));
    EXPECT_TRUE(ry::util::isTupleTypeName("()"));
    EXPECT_FALSE(ry::util::isTupleTypeName("(int"));
    EXPECT_FALSE(ry::util::isTupleTypeName("int, str)"));
    EXPECT_FALSE(ry::util::isTupleTypeName("fn() -> int"));
    EXPECT_FALSE(ry::util::isTupleTypeName(""));
}

TEST(UtilTypeName, IsLowLevelTypeNameCoversIntegerAndFloatVariants) {
    EXPECT_TRUE(isLowLevelTypeName("i8"));
    EXPECT_TRUE(isLowLevelTypeName("i64"));
    EXPECT_TRUE(isLowLevelTypeName("u32"));
    EXPECT_TRUE(isLowLevelTypeName("f32"));
    EXPECT_FALSE(isLowLevelTypeName("int"));
    EXPECT_FALSE(isLowLevelTypeName("float"));
    EXPECT_FALSE(isLowLevelTypeName("f64"));
    EXPECT_FALSE(isLowLevelTypeName(""));
}

TEST(UtilTypeName, DeriveRuntimeFnNamePrefixesUnderscores) {
    EXPECT_EQ(deriveRuntimeFnName("", "print"), "__ry_print");
    EXPECT_EQ(deriveRuntimeFnName("path", "join"), "__ry_path_join");
    EXPECT_EQ(deriveRuntimeFnName("json", "load"), "__ry_json_load");
}

TEST(UtilTypeName, NativeSigKeyJoinsPackageAndName) {
    EXPECT_EQ(nativeSigKey("", "len"), "len");
    EXPECT_EQ(nativeSigKey("math", "sqrt"), "math::sqrt");
}

}  // namespace
