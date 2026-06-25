// Test taxonomy: docs/reference/test-taxonomy.md
// Section header tags: [contract] / [regression #NNNN] / [internal].
// Per-test exceptions use an inline `// [regression: #NNNN]` comment.

#include "test_codegen_common.hpp"


using namespace ry;
// ===== [contract] @each: basic parameterized test =====

TEST_F(CodeGenTest, EachBasicInt) {
    auto output = runTestSource(withStdlibDirectiveDecls(
        "@describe(\"each\")\n"
        "fn eachGroup():\n"
        "    @each([(1, 2, 3), (0, 0, 0)])\n"
        "    @it(\"adds {0} + {1} = {2}\")\n"
        "    fn addsCase(a: int, b: int, expected: int):\n"
        "        expect(a + b).toEq(expected)\n"
    ));
    EXPECT_NE(output.find("adds 1 + 2 = 3"), std::string::npos);
    EXPECT_NE(output.find("adds 0 + 0 = 0"), std::string::npos);
    EXPECT_NE(output.find("2 passed, 0 failed"), std::string::npos);
}

// ===== [contract] @each: string parameters =====

TEST_F(CodeGenTest, EachStringParam) {
    auto output = runTestSource(withStdlibDirectiveDecls(
        "@describe(\"each str\")\n"
        "fn eachStr():\n"
        "    @each([(\"hi\", 2), (\"\", 0)])\n"
        "    @it(\"len of {0} is {1}\")\n"
        "    fn lenCase(s: str, expected: int):\n"
        "        expect(len(s)).toEq(expected)\n"
    ));
    EXPECT_NE(output.find("2 passed, 0 failed"), std::string::npos);
}

// ===== [contract] @each: failing test =====

TEST_F(CodeGenTest, EachFailingTest) {
    auto output = runTestSource(withStdlibDirectiveDecls(
        "@describe(\"each fail\")\n"
        "fn eachFail():\n"
        "    @each([(1, 2, 4)])\n"
        "    @it(\"{0}+{1}={2}\")\n"
        "    fn failCase(a: int, b: int, expected: int):\n"
        "        expect(a + b).toEq(expected)\n"
    ));
    EXPECT_NE(output.find("0 passed, 1 failed"), std::string::npos);
}

// ===== [contract] @property: basic commutative test =====

TEST_F(CodeGenTest, PropertyCommutative) {
    auto output = runTestSource(withStdlibDirectiveDecls(
        "@describe(\"prop\")\n"
        "fn propGroup():\n"
        "    @property(count=50)\n"
        "    @it(\"a+b == b+a\")\n"
        "    fn commutative(a: int, b: int):\n"
        "        expect(a + b).toEq(b + a)\n"
    ));
    EXPECT_NE(output.find("1 passed, 0 failed"), std::string::npos);
}

// ===== [contract] @property: bool parameter =====

TEST_F(CodeGenTest, PropertyBoolParam) {
    auto output = runTestSource(withStdlibDirectiveDecls(
        "@describe(\"prop bool\")\n"
        "fn propBool():\n"
        "    @property(count=10)\n"
        "    @it(\"x==x\")\n"
        "    fn boolEq(x: bool):\n"
        "        expect(x == x).toBeTrue()\n"
    ));
    EXPECT_NE(output.find("1 passed, 0 failed"), std::string::npos);
}
