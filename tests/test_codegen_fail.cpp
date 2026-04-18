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
// Option.map() requires a callable second argument
// ============================================================

TEST_F(CodeGenTest, OptionMapRejectsNonCallableSecondArg) {
    expectCompileError(
        "o: int? = Some(1)\n"
        "v = o.map(42)\n",
        "map() on Option requires a function as second argument");
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

// ============================================================
// print() named argument error cases
// ============================================================

TEST_F(CodeGenTest, PrintUnknownNamedArgError) {
    expectCompileError(
        "print(\"hello\", file=\"stderr\")\n",
        "unknown named argument 'file' for print()");
}

TEST_F(CodeGenTest, NamedArgsOnNonBuiltinError) {
    expectCompileError(
        "function greet(name: str):\n"
        "  print(name)\n"
        "greet(name=\"world\")\n",
        "named arguments are only supported for builtin functions");
}

// ============================================================
// `+` on Map / Set collection operands: type-aware diagnostics.
// Before #863 these fell through to the str-vs-non-str reject path.
// After #866 Map + Map (merge) and Set + Set (union) are supported,
// but mismatched types must still produce a clear diagnostic.
// ============================================================

TEST_F(CodeGenTest, ArithPlusRejectsMapPlusMapKeyMismatch) {
    // Different key types → error
    expectCompileError(
        "a: Map<str, int> = {\"a\": 1}\n"
        "b: Map<int, int> = {2: 2}\n"
        "c = a + b\n",
        "map merge requires matching key/value types");
}

TEST_F(CodeGenTest, ArithPlusRejectsMapPlusMapValueMismatch) {
    // Different value types → error
    expectCompileError(
        "a: Map<str, int> = {\"a\": 1}\n"
        "b: Map<str, str> = {\"b\": \"x\"}\n"
        "c = a + b\n",
        "map merge requires matching key/value types");
}

TEST_F(CodeGenTest, ArithPlusRejectsSetPlusSetElemMismatch) {
    // Different element types → error
    expectCompileError(
        "a: Set<int> = {1}\n"
        "b: Set<str> = {\"x\"}\n"
        "c = a + b\n",
        "set union requires matching element types");
}

TEST_F(CodeGenTest, ArithPlusRejectsListPlusMap) {
    expectCompileError(
        "a: List<int> = [1]\n"
        "b: Map<str, int> = {\"b\": 2}\n"
        "c = a + b\n",
        "operator '+' is not defined for List<int> and Map<str, int>");
}

// The existing list-concat mismatch diagnostic must still fire for
// List<T> + List<U> so the new Map/Set branch does not overshadow it.
TEST_F(CodeGenTest, ArithPlusListConcatMismatchMessageUnchanged) {
    expectCompileError(
        "a: List<int> = [1]\n"
        "b: List<str> = [\"x\"]\n"
        "c = a + b\n",
        "list concatenation requires matching element types");
}

// ============================================================
// Map<K, any>: collection / non-str-pointer types are rejected by wrapInAny
// ============================================================

TEST_F(CodeGenTest, MapAnyValueRejectsCollectionType) {
    // wrapInAny rejects non-str pointers (collections, resources, etc.)
    // with "any can only hold int/float/bool/str".
    expectCompileError(
        "m: Map<str, any> = {}\n"
        "m[\"bad\"] = [1, 2, 3]\n",
        "'any' can only hold int/float/bool/str");
}

TEST_F(CodeGenTest, SetAddAnyRejectsCollectionType) {
    expectCompileError(
        "s: Set<any> = {}\n"
        "add(s, [1, 2, 3])\n",
        "'any' can only hold int/float/bool/str");
}

TEST_F(CodeGenTest, ListAppendAnyRejectsCollectionType) {
    expectCompileError(
        "xs: List<any> = []\n"
        "append!(xs, [1, 2, 3])\n",
        "'any' can only hold int/float/bool/str");
}

TEST_F(CodeGenTest, ListAppendedAnyRejectsCollectionType) {
    expectCompileError(
        "xs: List<any> = []\n"
        "ys = appended(xs, [1, 2, 3])\n",
        "'any' can only hold int/float/bool/str");
}

TEST_F(CodeGenTest, ListInsertAnyRejectsCollectionType) {
    expectCompileError(
        "xs: List<any> = []\n"
        "insert(xs, 0, [1, 2, 3])\n",
        "'any' can only hold int/float/bool/str");
}

// ============================================================
// fold(): seed/return-type mismatch must still fire for typed lambda
// ============================================================

TEST_F(CodeGenTest, FoldTypedLambdaSeedTypeMismatch) {
    expectCompileError(
        "xs = [1, 2, 3]\n"
        "fold(xs, \"hello\", (a: int, b: int) => a + b)\n",
        "fold() initial value type must match function return type");
}

// ============================================================
// #1027: octal literals must produce a targeted diagnostic
// ============================================================

TEST_F(CodeGenTest, OctalLiteralRejectedWithTargetedDiagnostic) {
    expectCompileError("x = 0o17\n",
                       "octal literals (0o...) are not supported");
    expectCompileError("x = 0o755\n",
                       "use hex (0x...) or binary (0b...) instead");
    expectCompileError("x = 0O17\n",
                       "octal literals (0o...) are not supported");
}

// ============================================================
// None() rejects non-zero arguments (#1043)
// ============================================================

TEST_F(CodeGenTest, NoneWithArgsIsRejected) {
    expectCompileError("x: int? = None(1)\n",
                       "None() takes no arguments");
}

// ============================================================
// base64.encode_bytes / encode_bytes_url_safe reject non-u8 list
// at compile time via requireListU8Arg gate (#1130)
// ============================================================

TEST_F(CodeGenTest, Base64EncodeBytesRejectsNonU8List) {
    expectCompileError(
        "@native(\"base64\")\n"
        "function encode_bytes(input: List<int>) -> str\n"
        "xs: List<int> = [1, 2, 3]\n"
        "print(encode_bytes(xs))\n",
        "requires List<u8>");
}

TEST_F(CodeGenTest, Base64EncodeBytesUrlSafeRejectsNonU8List) {
    expectCompileError(
        "@native(\"base64\")\n"
        "function encode_bytes_url_safe(input: List<int>) -> str\n"
        "xs: List<int> = [1, 2, 3]\n"
        "print(encode_bytes_url_safe(xs))\n",
        "requires List<u8>");
}
