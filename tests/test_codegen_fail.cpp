#include "test_codegen_common.hpp"


using namespace ry;
// ============================================================
// fail(msg) marks current test as failed with message
// ============================================================

TEST_F(CodeGenTest, FailWithMessage) {
    EXPECT_EQ(runTestSource(
        "describe(\"fail helper\", ():\n"
        "    it(\"marks test as failed\", ():\n"
        "        fail(\"intentional failure\")\n"
        "    )\n"
        ")\n"
    ), "fail helper\n    \033[31mline 3: intentional failure\033[0m\n  \033[31m- marks test as failed\033[0m\n\n0 passed, 1 failed\n");
}

// ============================================================
// fail() with no argument uses generic message
// ============================================================

TEST_F(CodeGenTest, FailWithoutMessage) {
    EXPECT_EQ(runTestSource(
        "describe(\"fail bare\", ():\n"
        "    it(\"fails generically\", ():\n"
        "        fail()\n"
        "    )\n"
        ")\n"
    ), "fail bare\n    \033[31mline 3: test failed\033[0m\n  \033[31m- fails generically\033[0m\n\n0 passed, 1 failed\n");
}

// ============================================================
// fail() outside test mode is rejected at compile time
// ============================================================

TEST_F(CodeGenTest, FailOutsideTestModeIsRejected) {
    EXPECT_THROW(runSource(
        "fail(\"should not compile\")\n"
    ), std::runtime_error);
}

// ============================================================
// Null coalescing (`??`) rejects non-Option/non-Result operands
// ============================================================

TEST_F(CodeGenTest, NullCoalesceRequiresOptionOrResult) {
    // Plain int on the LHS is not allowed.
    expectCompileError(
        "x = 1 ?? 0\n",
        "Option or Result");
}

TEST_F(CodeGenTest, NullCoalesceRhsTypeMismatchOnOption) {
    expectCompileError(
        "a: int? = Some(1)\n"
        "x = a ?? \"str\"\n",
        "match Option's inner type");
}

TEST_F(CodeGenTest, NullCoalesceRhsTypeMismatchOnResult) {
    expectCompileError(
        "function mk() -> Result<int, Error>:\n"
        "  return Ok(1)\n"
        "x = mk() ?? \"str\"\n",
        "match Result's Ok type");
}

// ============================================================
// `?` operator rejects non-Result/non-Option operands
// ============================================================

TEST_F(CodeGenTest, QuestionRequiresResultOrOption) {
    expectCompileError(
        "function f() -> int:\n"
        "  x = 1?\n"
        "  return x\n",
        "Result or Option");
}

// ============================================================
// `?` on Option requires an Option-returning enclosing function
// ============================================================

TEST_F(CodeGenTest, QuestionOnOptionRequiresOptionReturn) {
    expectCompileError(
        "function f() -> int:\n"
        "  a: int? = Some(1)\n"
        "  v = a?\n"
        "  return v\n",
        "function that returns Option");
}

TEST_F(CodeGenTest, QuestionOnOptionInResultFnRejected) {
    expectCompileError(
        "function f() -> Result<int, Error>:\n"
        "  a: int? = Some(1)\n"
        "  v = a?\n"
        "  return Ok(v)\n",
        "function that returns Option");
}

// ============================================================
// `?` on Result requires a Result-returning enclosing function
// ============================================================

TEST_F(CodeGenTest, QuestionOnResultInOptionFnRejected) {
    expectCompileError(
        "function mk() -> Result<int, Error>:\n"
        "  return Ok(1)\n"
        "function f() -> Option<int>:\n"
        "  v = mk()?\n"
        "  return Some(v)\n",
        "function that returns Result");
}

// ============================================================
// Nested function is not visible outside its enclosing scope
// ============================================================

TEST_F(CodeGenTest, NestedFunctionNotVisibleOutsideScope) {
    EXPECT_THROW(runSource(
        "function outer() -> int:\n"
        "    function inner() -> int:\n"
        "        return 1\n"
        "    return inner()\n"
        "inner()\n"
    ), std::runtime_error);
}

// ============================================================
// Captured variable cannot be modified inside nested named function
// ============================================================

TEST_F(CodeGenTest, NestedFunctionCannotModifyCapturedVariable) {
    EXPECT_THROW(runSource(
        "function outer() -> int:\n"
        "    x = 10\n"
        "    function helper() -> int:\n"
        "        x = 20\n"
        "        return x\n"
        "    return helper()\n"
        "print(outer())\n"
    ), std::runtime_error);
}
