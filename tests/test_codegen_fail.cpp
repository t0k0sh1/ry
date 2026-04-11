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
        "'" "??" "' on Option");
}

TEST_F(CodeGenTest, NullCoalesceRhsTypeMismatchOnResult) {
    expectCompileError(
        "function mk() -> Result<int, Error>:\n"
        "  return Ok(1)\n"
        "x = mk() ?? \"str\"\n",
        "'" "??" "' on Result");
}

// Raw LLVM-type equality would wrongly accept `Result<List<int>, Error>`
// and `str` because both are pointer-backed. The check must use Ry-level
// metadata via `validateBranchTypes`.
TEST_F(CodeGenTest, NullCoalesceRejectsPtrBackedTypeMismatch) {
    expectCompileError(
        "xs: List<int> = [1, 2, 3]\n"
        "r: Result<List<int>, Error> = Ok(xs)\n"
        "v = r ?? \"bad\"\n",
        "'" "??" "' on Result");
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
// Top-level `?` on `Result<_, E>` requires E == Error
// ============================================================

TEST_F(CodeGenTest, TopLevelQuestionRejectsNonErrorResult) {
    expectCompileError(
        "function mk() -> Result<int, str>:\n"
        "  return Err(\"boom\")\n"
        "v = mk()?\n",
        "err type must be Error");
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

// ============================================================
// math.pow(int, int) with a negative exponent aborts at runtime.
//
// Declares `pow` via a bare `@native` so the dispatcher's fallback
// bare-name lookup routes the call through `emitMathPow` in
// `math_table` without needing `ModuleLoader` to resolve the real
// stdlib import (see KNOWLEDGE.md "CodeGenTest::runSource cannot
// compile stdlib-import code").
// ============================================================

TEST_F(CodeGenTest, MathPowIntNegativeExponentAborts) {
    EXPECT_EXIT(
        runSource(
            "@native\n"
            "function pow(x: int, y: int) -> int\n"
            "print(pow(2, -1))\n"),
        ::testing::ExitedWithCode(1),
        "pow\\(\\) integer exponent must be non-negative");
}

// ============================================================
// Generic inference for container parameters (#823).
// An empty container literal yields no information for the
// type variable, so inference must emit a clear error naming
// the parameter and the function, not a vague codegen error or
// an "undefined variable" message.
// ============================================================

TEST_F(CodeGenTest, GenericInferenceEmptyListHasClearError) {
    expectCompileError(
        "function first_of<T>(xs: List<T>) -> T:\n"
        "  return xs[0]\n"
        "print(first_of([]))\n",
        "could not infer type parameter 'T' in call to generic function 'first_of'");
}

// ============================================================
// When the same type parameter appears in two argument slots
// and the concrete arguments disagree, inference must report a
// clear conflict naming the parameter.
// ============================================================

TEST_F(CodeGenTest, GenericInferenceConflictingBindingError) {
    expectCompileError(
        "function same<T>(a: T, b: T) -> T:\n"
        "  return a\n"
        "print(same(1, \"x\"))\n",
        "conflicting type inference for 'T'");
}
