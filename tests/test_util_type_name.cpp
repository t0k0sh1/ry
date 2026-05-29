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
using ry::util::splitGenericTypeName;
using ry::util::trimTypeNameSpaces;

TEST(UtilTypeName, TrimTypeNameSpacesStripsLeadingTrailingSpacesOnly) {
    EXPECT_EQ(trimTypeNameSpaces("  int  "), "int");
    EXPECT_EQ(trimTypeNameSpaces("int"), "int");
    EXPECT_EQ(trimTypeNameSpaces(""), "");
    EXPECT_EQ(trimTypeNameSpaces("a b"), "a b");
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
