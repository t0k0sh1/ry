#include "test_codegen_common.hpp"


using namespace ry;
// ============================================================
// @each: basic parameterized test
// ============================================================

TEST_F(CodeGenTest, EachBasicInt) {
    auto output = runTestSource(withStdlibDirectiveDecls(
        "describe(\"each\", ():\n"
        "    @each([(1, 2, 3), (0, 0, 0)])\n"
        "    it(\"adds {0} + {1} = {2}\", (a: int, b: int, expected: int):\n"
        "        expect(a + b).to_eq(expected)\n"
        "    )\n"
        ")\n"
    ));
    EXPECT_NE(output.find("adds 1 + 2 = 3"), std::string::npos);
    EXPECT_NE(output.find("adds 0 + 0 = 0"), std::string::npos);
    EXPECT_NE(output.find("2 passed, 0 failed"), std::string::npos);
}

// ============================================================
// @each: string parameters
// ============================================================

TEST_F(CodeGenTest, EachStringParam) {
    auto output = runTestSource(withStdlibDirectiveDecls(
        "describe(\"each str\", ():\n"
        "    @each([(\"hi\", 2), (\"\", 0)])\n"
        "    it(\"len of {0} is {1}\", (s: str, expected: int):\n"
        "        expect(length(s)).to_eq(expected)\n"
        "    )\n"
        ")\n"
    ));
    EXPECT_NE(output.find("2 passed, 0 failed"), std::string::npos);
}

// ============================================================
// @each: failing test
// ============================================================

TEST_F(CodeGenTest, EachFailingTest) {
    auto output = runTestSource(withStdlibDirectiveDecls(
        "describe(\"each fail\", ():\n"
        "    @each([(1, 2, 4)])\n"
        "    it(\"{0}+{1}={2}\", (a: int, b: int, expected: int):\n"
        "        expect(a + b).to_eq(expected)\n"
        "    )\n"
        ")\n"
    ));
    EXPECT_NE(output.find("0 passed, 1 failed"), std::string::npos);
}

// ============================================================
// @property: basic commutative test
// ============================================================

TEST_F(CodeGenTest, PropertyCommutative) {
    auto output = runTestSource(withStdlibDirectiveDecls(
        "describe(\"prop\", ():\n"
        "    @property(count=50)\n"
        "    it(\"a+b == b+a\", (a: int, b: int):\n"
        "        expect(a + b).to_eq(b + a)\n"
        "    )\n"
        ")\n"
    ));
    EXPECT_NE(output.find("1 passed, 0 failed"), std::string::npos);
}

// ============================================================
// @property: bool parameter
// ============================================================

TEST_F(CodeGenTest, PropertyBoolParam) {
    auto output = runTestSource(withStdlibDirectiveDecls(
        "describe(\"prop bool\", ():\n"
        "    @property(count=10)\n"
        "    it(\"x==x\", (x: bool):\n"
        "        expect(x == x).to_be_true()\n"
        "    )\n"
        ")\n"
    ));
    EXPECT_NE(output.find("1 passed, 0 failed"), std::string::npos);
}
