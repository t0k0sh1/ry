#include "test_codegen_common.hpp"

class DirectiveTest : public CodeGenTest {};

// 1. @deprecated fn called -> warning, execution normal
TEST_F(DirectiveTest, DeprecatedFunctionWarning) {
    auto [output, warnings] = runSourceWithWarnings(
        "@deprecated\n"
        "fn old_func() -> int:\n"
        "    return 42\n"
        "print(old_func())\n"
    );
    EXPECT_EQ(output, "42\n");
    ASSERT_EQ(warnings.size(), 1);
    EXPECT_EQ(warnings[0], "warning: 'old_func' is deprecated");
}

// 2. @deprecated type constructed -> warning
TEST_F(DirectiveTest, DeprecatedTypeWarning) {
    auto [output, warnings] = runSourceWithWarnings(
        "@deprecated\n"
        "type OldPoint:\n"
        "    x: int\n"
        "    y: int\n"
        "let p = OldPoint(1, 2)\n"
        "print(p.x)\n"
    );
    EXPECT_EQ(output, "1\n");
    ASSERT_EQ(warnings.size(), 1);
    EXPECT_EQ(warnings[0], "warning: 'OldPoint' is deprecated");
}

// 3. @deprecated let referenced -> warning
TEST_F(DirectiveTest, DeprecatedVariableWarning) {
    auto [output, warnings] = runSourceWithWarnings(
        "@deprecated\n"
        "let old_val = 99\n"
        "print(old_val)\n"
    );
    EXPECT_EQ(output, "99\n");
    ASSERT_EQ(warnings.size(), 1);
    EXPECT_EQ(warnings[0], "warning: 'old_val' is deprecated");
}

// 4. @deprecated field accessed -> warning
TEST_F(DirectiveTest, DeprecatedFieldWarning) {
    auto [output, warnings] = runSourceWithWarnings(
        "type MyType:\n"
        "    @deprecated\n"
        "    old_field: int\n"
        "    new_field: int\n"
        "let m = MyType(1, 2)\n"
        "print(m.old_field)\n"
        "print(m.new_field)\n"
    );
    EXPECT_EQ(output, "1\n2\n");
    ASSERT_EQ(warnings.size(), 1);
    EXPECT_EQ(warnings[0], "warning: 'MyType.old_field' is deprecated");
}

// 5. Definition alone does not produce warnings
TEST_F(DirectiveTest, DeprecatedNoWarningOnDefinition) {
    auto [output, warnings] = runSourceWithWarnings(
        "@deprecated\n"
        "fn unused_func() -> int:\n"
        "    return 1\n"
        "@deprecated\n"
        "let unused_val = 42\n"
        "print(0)\n"
    );
    EXPECT_EQ(output, "0\n");
    EXPECT_TRUE(warnings.empty());
}

// 6. Non-deprecated entities produce no warnings
TEST_F(DirectiveTest, NonDeprecatedNoWarning) {
    auto [output, warnings] = runSourceWithWarnings(
        "fn good_func() -> int:\n"
        "    return 10\n"
        "let good_val = 20\n"
        "print(good_func())\n"
        "print(good_val)\n"
    );
    EXPECT_EQ(output, "10\n20\n");
    EXPECT_TRUE(warnings.empty());
}

// 7. Deprecated function still works correctly
TEST_F(DirectiveTest, DeprecatedFunctionStillWorks) {
    auto [output, warnings] = runSourceWithWarnings(
        "@deprecated\n"
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "print(add(3, 4))\n"
    );
    EXPECT_EQ(output, "7\n");
    ASSERT_EQ(warnings.size(), 1);
}

// 8. Multiple directives stacked (parse check)
TEST_F(DirectiveTest, MultipleDirectives) {
    auto [output, warnings] = runSourceWithWarnings(
        "@deprecated\n"
        "@deprecated\n"
        "fn multi() -> int:\n"
        "    return 1\n"
        "print(multi())\n"
    );
    EXPECT_EQ(output, "1\n");
    EXPECT_FALSE(warnings.empty());
}

// 9. Directive with params parses correctly
TEST_F(DirectiveTest, DirectiveWithParams) {
    auto [output, warnings] = runSourceWithWarnings(
        "@deprecated(reason=\"use new_func instead\")\n"
        "fn old_api() -> int:\n"
        "    return 0\n"
        "print(old_api())\n"
    );
    EXPECT_EQ(output, "0\n");
    ASSERT_EQ(warnings.size(), 1);
    EXPECT_EQ(warnings[0], "warning: 'old_api' is deprecated");
}

// 10. Unknown directive causes parse error
TEST_F(DirectiveTest, UnknownDirectiveError) {
    EXPECT_THROW({
        runSource("@unknown\nfn foo() -> int:\n    return 1\n");
    }, std::runtime_error);
}

// 11. Directive on invalid target causes parse error
TEST_F(DirectiveTest, DirectiveOnInvalidTarget) {
    EXPECT_THROW({
        runSource("@deprecated\nif true\n    print(1)\n");
    }, std::runtime_error);
}
