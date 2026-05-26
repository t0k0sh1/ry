#include "test_codegen_common.hpp"


using namespace ry;
// ============================================================
// fail(msg) marks current test as failed with message
// ============================================================

TEST_F(CodeGenTest, FailWithMessage) {
    EXPECT_EQ(runTestSource(withStdlibDirectiveDecls(
        "@describe(\"fail helper\")\n"
        "fn failHelper():\n"
        "    @it(\"marks test as failed\")\n"
        "    fn marksTestAsFailed():\n"
        "        fail(\"intentional failure\")\n"
    )), "fail helper\n    \033[31mline 37: intentional failure\033[0m\n  \033[31m- marks test as failed\033[0m\n\n0 passed, 1 failed, 0 skipped, 0 todo\n");
}

// ============================================================
// fail() with no argument uses generic message
// ============================================================

TEST_F(CodeGenTest, FailWithoutMessage) {
    EXPECT_EQ(runTestSource(withStdlibDirectiveDecls(
        "@describe(\"fail bare\")\n"
        "fn failBare():\n"
        "    @it(\"fails generically\")\n"
        "    fn failsGenerically():\n"
        "        fail()\n"
    )), "fail bare\n    \033[31mline 37: test failed\033[0m\n  \033[31m- fails generically\033[0m\n\n0 passed, 1 failed, 0 skipped, 0 todo\n");
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
// Diagnostic precedence: when a call is made outside test mode
// AND without `from testing import ...`, the "only allowed in
// test mode" error must win over the missing-import error
// (#715). This pins the order of the two checks at every site:
// the `!test_mode_` guard fires first, the import guard second.
// Use `runSource` (test_mode = false, no import injection) so
// both conditions are simultaneously violated.
// ============================================================

TEST_F(CodeGenTest, ExpectOutsideTestModeWinsOverImportCheck) {
    try {
        runSource("expect(1).toEq(1)\n");
        FAIL() << "Expected compile error";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("only allowed in test mode"), std::string::npos)
            << "got: " << msg;
        EXPECT_EQ(msg.find("requires 'from testing import"), std::string::npos)
            << "import check must NOT fire first; got: " << msg;
    }
}

// ============================================================
// Testing intrinsics (expect / mock / fail) require the matching
// `from testing import <name>`. Without it codegen must reject the
// use even when running in test mode (#715).
// `runTestSourceNoTestingImports` is the only fixture that does
// not auto-inject the imports.
//
// Note: as of #722, `verify` is no longer a compiler-recognized
// intrinsic — it lives as an ordinary `@public fn verify` in
// `share/std/testing/testing.ry`. Calling `verify` without the
// import now fails with the generic `undefined function: verify`
// path rather than the import-enforcement diagnostic, so it is
// not covered here.
// ============================================================

TEST_F(CodeGenTest, ExpectRequiresTestingImport) {
    try {
        runTestSourceNoTestingImports("expect(1).toEq(1)\n");
        FAIL() << "Expected compile error";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("requires 'from testing import expect'"),
                  std::string::npos)
            << "got: " << msg;
    }
}

TEST_F(CodeGenTest, MockRequiresTestingImport) {
    try {
        runTestSourceNoTestingImports(
            "fn greet() -> str:\n"
            "  return \"hi\"\n"
            "mock(greet, () => \"mocked\")\n"
        );
        FAIL() << "Expected compile error";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("requires 'from testing import mock'"),
                  std::string::npos)
            << "got: " << msg;
    }
}

TEST_F(CodeGenTest, FailRequiresTestingImport) {
    try {
        runTestSourceNoTestingImports("fail(\"oops\")\n");
        FAIL() << "Expected compile error";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("requires 'from testing import fail'"),
                  std::string::npos)
            << "got: " << msg;
    }
}

// fail() now bypasses the user-fn signature type-check (#1690 made it a
// pure codegen intrinsic), so guard non-str message args explicitly —
// otherwise non-str pointers (List/Map/closure handles) flow into
// `__ry_test_fail`'s `%s` format and read garbage at runtime.
TEST_F(CodeGenTest, FailRejectsNonStrMessage) {
    try {
        runTestSource("fail(42)\n");
        FAIL() << "Expected compile error for non-str fail() arg";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("fail() message argument must be a str"),
                  std::string::npos)
            << "got: " << msg;
    }
}

TEST_F(CodeGenTest, FailRejectsListMessage) {
    try {
        runTestSource("fail([1, 2, 3])\n");
        FAIL() << "Expected compile error for List fail() arg";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("fail() message argument must be a str"),
                  std::string::npos)
            << "got: " << msg;
    }
}

// ============================================================
// `@it` / `@describe` directives are declared in
// `share/std/testing/testing.ry`. Without `from testing import it,
// describe`, the directive declarations never enter
// `user_directive_registry_`, so codegen rejects them via the general
// `@directive` mechanism with `unknown directive '@<name>'` (#721).
// `withStdlibDirectiveDecls` is intentionally NOT applied so the
// rejection path mirrors what real user code sees (the only legal way
// to use `@it` / `@describe` is via the testing import).
// ============================================================

TEST_F(CodeGenTest, ItRejectedAsUnknownDirectiveWithoutTestingImport) {
    try {
        runTestSourceNoTestingImports(
            "@it(\"sample\")\n"
            "fn sample():\n"
            "    x = 1\n"
        );
        FAIL() << "Expected compile error";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("unknown directive '@it'"),
                  std::string::npos)
            << "got: " << msg;
    }
}

TEST_F(CodeGenTest, DescribeRejectedAsUnknownDirectiveWithoutTestingImport) {
    try {
        runTestSourceNoTestingImports(
            "@describe(\"sample group\")\n"
            "fn sampleGroup():\n"
            "    x = 1\n"
        );
        FAIL() << "Expected compile error";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("unknown directive '@describe'"),
                  std::string::npos)
            << "got: " << msg;
    }
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
        "fn mk() -> Result<int, Error>:\n"
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
        "fn f() -> int:\n"
        "  x = 1?\n"
        "  return x\n",
        "Result or Option");
}

// ============================================================
// `?` on Option requires an Option-returning enclosing function
// ============================================================

TEST_F(CodeGenTest, QuestionOnOptionRequiresOptionReturn) {
    expectCompileError(
        "fn f() -> int:\n"
        "  a: int? = Some(1)\n"
        "  v = a?\n"
        "  return v\n",
        "fn that returns Option");
}

TEST_F(CodeGenTest, QuestionOnOptionInResultFnRejected) {
    expectCompileError(
        "fn f() -> Result<int, Error>:\n"
        "  a: int? = Some(1)\n"
        "  v = a?\n"
        "  return Ok(v)\n",
        "fn that returns Option");
}

// ============================================================
// `?` on Result requires a Result-returning enclosing function
// ============================================================

TEST_F(CodeGenTest, QuestionOnResultInOptionFnRejected) {
    expectCompileError(
        "fn mk() -> Result<int, Error>:\n"
        "  return Ok(1)\n"
        "fn f() -> Option<int>:\n"
        "  v = mk()?\n"
        "  return Some(v)\n",
        "fn that returns Result");
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
        "fn mk() -> Result<int, str>:\n"
        "  return Err(\"boom\")\n"
        "v = mk()?\n",
        "err type must be Error");
}

// ============================================================
// Nested function is not visible outside its enclosing scope
// ============================================================

TEST_F(CodeGenTest, NestedFunctionNotVisibleOutsideScope) {
    EXPECT_THROW(runSource(
        "fn outer() -> int:\n"
        "    fn inner() -> int:\n"
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
        "fn outer() -> int:\n"
        "    x = 10\n"
        "    fn helper() -> int:\n"
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
// stdlib import. CodeGenTest::runSource goes Parser → CodeGen
// directly without invoking ModuleLoader, so `from math import ...`
// would fail with "unresolved import".
// ============================================================

TEST_F(CodeGenTest, MathPowIntNegativeExponentAborts) {
    EXPECT_EXIT(
        runSource(
            "@native\n"
            "fn pow(x: int, y: int) -> int\n"
            "print(pow(2, -1))\n"),
        ::testing::ExitedWithCode(1),
        "pow\\(\\) integer exponent must be non-negative");
}

TEST_F(CodeGenTest, DigitsNegativeNAborts) {
    EXPECT_EXIT(
        runSource(
            "@native\n"
            "fn digits(n: int, base: int) -> List<int>\n"
            "print(digits(-1, 10))\n"),
        ::testing::ExitedWithCode(1),
        "digits\\(\\) n must be non-negative");
}

TEST_F(CodeGenTest, DigitsBaseLessThan2Aborts) {
    EXPECT_EXIT(
        runSource(
            "@native\n"
            "fn digits(n: int, base: int) -> List<int>\n"
            "print(digits(10, 1))\n"),
        ::testing::ExitedWithCode(1),
        "digits\\(\\) base must be >= 2");
}

TEST_F(CodeGenTest, DigitsBaseZeroAborts) {
    EXPECT_EXIT(
        runSource(
            "@native\n"
            "fn digits(n: int, base: int) -> List<int>\n"
            "print(digits(10, 0))\n"),
        ::testing::ExitedWithCode(1),
        "digits\\(\\) base must be >= 2");
}

TEST_F(CodeGenTest, HelperReturnRejectsIncompatibleThreadResultMetadataAcrossBranches) {
    expectCompileError(
        "@native(\"thread\")\n"
        "fn threadSpawn(body: fn() -> any) -> Thread\n"
        "fn mkThread(flag: bool) -> Thread:\n"
        "  if flag:\n"
        "    return threadSpawn(() => 1)\n"
        "  return threadSpawn(() => true)\n",
        "returns incompatible Thread result metadata across branches");
}

TEST_F(CodeGenTest, HelperReturnRejectsMissingAndPresentThreadResultMetadataMix) {
    expectCompileError(
        "@native(\"thread\")\n"
        "fn threadSpawn(body: fn() -> any) -> Thread\n"
        "fn mkThread(flag: bool, fallback: Thread) -> Thread:\n"
        "  if flag:\n"
        "    return threadSpawn(() => 1)\n"
        "  return fallback\n",
        "returns incompatible Thread result metadata across branches");
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
        "fn firstOf<T>(xs: List<T>) -> T:\n"
        "  return xs[0]\n"
        "print(firstOf([]))\n",
        "could not infer type parameter 'T' in call to generic function 'firstOf'");
}

// ============================================================
// When the same type parameter appears in two argument slots
// and the concrete arguments disagree, inference must report a
// clear conflict naming the parameter.
// ============================================================

TEST_F(CodeGenTest, GenericInferenceConflictingBindingError) {
    expectCompileError(
        "fn same<T>(a: T, b: T) -> T:\n"
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
        "fn greet(name: str):\n"
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
// Map<K, any> / Set<any> / List<any>: #1697 lifts the old wrapInAny
// rejection so List / Map / Set values can be assigned through an
// `any` slot in any collection-mutating builtin. Flipped from
// EXPECT_THROW to EXPECT_NO_THROW per "Relaxing a rejection branch
// requires flipping (not deleting) existing EXPECT_THROW tests"
// (.claude/rules/tests-rejection-tdd.md). The old "'any' can only
// hold int/float/bool/str" error path is gone for collection inputs;
// fn-ptr / resource still hit it (covered by
// AnyTypeRejectionForFunctionPointer in test_codegen.cpp).
// ============================================================

TEST_F(CodeGenTest, MapAnyValueAcceptsCollectionType) {
    // Verify wrap → store → load → to_string round-trip (#1697): printing
    // a collection-holding `any` emits the opaque `<List>` marker (element
    // metadata is erased on wrap, see docs/reference/types.md).
    EXPECT_EQ(runSource(
        "m: Map<str, any> = {}\n"
        "m[\"ok\"] = [1, 2, 3]\n"
        "print(m[\"ok\"])\n"),
        "<List>\n");
}

TEST_F(CodeGenTest, SetAddAnyAcceptsCollectionType) {
    EXPECT_NO_THROW(compileSource(
        "s: Set<any> = {}\n"
        "add(s, [1, 2, 3])\n"));
}

// Map / Set markers for the wrap → store → to_string round-trip (#1697).
// Element metadata is erased on wrap, so these render as opaque markers.
TEST_F(CodeGenTest, AnyHoldingMapPrintsAsOpaqueMarker) {
    EXPECT_EQ(runSource(
        "m: Map<str, int> = {\"a\": 1}\n"
        "x: any = m\n"
        "print(x)\n"),
        "<Map>\n");
}

TEST_F(CodeGenTest, AnyHoldingSetPrintsAsOpaqueMarker) {
    EXPECT_EQ(runSource(
        "s: Set<int> = {}\n"
        "add(s, 1)\n"
        "x: any = s\n"
        "print(x)\n"),
        "<Set>\n");
}

TEST_F(CodeGenTest, ListAppendAnyAcceptsCollectionType) {
    EXPECT_NO_THROW(compileSource(
        "xs: List<any> = []\n"
        "append!(xs, [1, 2, 3])\n"));
}

TEST_F(CodeGenTest, ListAppendedAnyAcceptsCollectionType) {
    EXPECT_NO_THROW(compileSource(
        "xs: List<any> = []\n"
        "ys = appended(xs, [1, 2, 3])\n"));
}

TEST_F(CodeGenTest, ListInsertAnyAcceptsCollectionType) {
    EXPECT_NO_THROW(compileSource(
        "xs: List<any> = []\n"
        "insert(xs, 0, [1, 2, 3])\n"));
}

// #1698: collection unwrap of `any` inside generic monomorphization with a
// non-`any` element type produces silent corruption because the `any` data
// slot is 16 bytes per element while the unwrap target strides 8 bytes per
// element. The guard rejects at compile time after substitution rewrites the
// type parameter to its concrete shape. Direct (non-generic) unwraps remain
// legitimate (covered by any.test.ry "should implicit-unwrap any back to
// List<int>") and are not blocked.
TEST_F(CodeGenTest, AnyUnwrapToListIntInGenericRejected) {
    expectCompileError(
        "fn convert<T>(a: any) -> T:\n"
        "  v: T = a\n"
        "  return v\n"
        "xs: List<int> = [1, 2, 3]\n"
        "boxed: any = xs\n"
        "out: List<int> = convert[List<int>](boxed)\n"
        "print(out[0])\n",
        "unwrapping 'any' to 'List<int>' is not supported");
}

TEST_F(CodeGenTest, AnyUnwrapToMapStrIntInGenericRejected) {
    expectCompileError(
        "fn convert<T>(a: any) -> T:\n"
        "  v: T = a\n"
        "  return v\n"
        "m: Map<str, int> = {}\n"
        "m[\"x\"] = 42\n"
        "boxed: any = m\n"
        "out: Map<str, int> = convert[Map<str, int>](boxed)\n"
        "print(out[\"x\"])\n",
        "unwrapping 'any' to 'Map<str,int>' is not supported");
}

TEST_F(CodeGenTest, AnyUnwrapToSetIntInGenericRejected) {
    expectCompileError(
        "fn convert<T>(a: any) -> T:\n"
        "  v: T = a\n"
        "  return v\n"
        "s: Set<int> = {}\n"
        "add(s, 1)\n"
        "boxed: any = s\n"
        "out: Set<int> = convert[Set<int>](boxed)\n"
        "print(1 in out)\n",
        "unwrapping 'any' to 'Set<int>' is not supported");
}

TEST_F(CodeGenTest, AnyUnwrapToListAnyInGenericAccepted) {
    EXPECT_NO_THROW(compileSource(
        "fn convert<T>(a: any) -> T:\n"
        "  v: T = a\n"
        "  return v\n"
        "xs: List<any> = []\n"
        "boxed: any = xs\n"
        "out: List<any> = convert[List<any>](boxed)\n"));
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
// #1209: reduce(xs, init, fn) (3-arg, Python/JS style) suggests fold
// ============================================================

TEST_F(CodeGenTest, ReduceThreeArgsSuggestsFold) {
    expectCompileError(
        "xs = [1, 2, 3]\n"
        "reduce(xs, 0, (a: int, b: int) => a + b)\n",
        "to use an initial value, call fold");
}

TEST_F(CodeGenTest, ReduceFourArgsUsesGenericError) {
    expectCompileError(
        "xs = [1, 2, 3]\n"
        "reduce(xs, 0, (a: int, b: int) => a + b, 42)\n",
        "takes exactly 2 arguments");
}

// ============================================================
// #1570: sequence() rejects non-Result/Option element type
// ============================================================

TEST_F(CodeGenTest, SequenceRejectsPlainIntList) {
    expectCompileError(
        "xs = [1, 2, 3]\n"
        "v = sequence(xs)\n",
        "sequence() requires a list of Result or Option");
}

TEST_F(CodeGenTest, SequenceRejectsNonListArg) {
    expectCompileError(
        "v = sequence(42)\n",
        "sequence() requires a list of Result or Option");
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
// base64.encodeBytes / encodeBytesUrlSafe reject non-u8 list
// at compile time via requireListU8Arg gate (#1130)
// ============================================================

TEST_F(CodeGenTest, Base64EncodeBytesRejectsNonU8List) {
    expectCompileError(
        "@native(\"base64\")\n"
        "fn encodeBytes(input: List<int>) -> str\n"
        "xs: List<int> = [1, 2, 3]\n"
        "print(encodeBytes(xs))\n",
        "requires List<u8>");
}

TEST_F(CodeGenTest, Base64EncodeBytesUrlSafeRejectsNonU8List) {
    expectCompileError(
        "@native(\"base64\")\n"
        "fn encodeBytesUrlSafe(input: List<int>) -> str\n"
        "xs: List<int> = [1, 2, 3]\n"
        "print(encodeBytesUrlSafe(xs))\n",
        "requires List<u8>");
}

// ============================================================
// #1157: coerceResultType must reject function-returned Result
// when the discriminator is a runtime value, not a provably
// literal Ok/Err. Without the fix, these silently miscompile.
// ============================================================

TEST_F(CodeGenTest, ResultCoerceFnReturnDifferentErrType) {
    // f() returns Result<bool, MyErr1>; binding to Result<bool, MyErr2> is
    // a single-slot Err mismatch. srcOkTy==dstOkTy so the old compile-time
    // path would copy the Ok slot — silently zeroing the Err payload when
    // the runtime disc is 0 (Err). The fix rejects this with a type error.
    expectCompileError(
        "record MyErr1:\n"
        "  msg: str\n"
        "record MyErr2:\n"
        "  msg: str\n"
        "fn f() -> Result<bool, MyErr1>:\n"
        "  return Err(MyErr1(\"x\"))\n"
        "r: Result<bool, MyErr2> = f()\n",
        "type error: annotation");
}

TEST_F(CodeGenTest, ResultCoerceFnReturnNarrowOkType) {
    // g() returns Result<i8, Error>; binding to Result<int, Error> is a
    // single-slot Ok mismatch. srcErrTy==dstErrTy so the old compile-time
    // path would copy the Err slot — silently zeroing the Ok payload when
    // the runtime disc is 1 (Ok). The fix rejects this with a type error.
    expectCompileError(
        "fn g() -> Result<i8, Error>:\n"
        "  return Err(Error(\"test\"))\n"
        "r: Result<int, Error> = g()\n",
        "type error: annotation");
}

TEST_F(CodeGenTest, ResultCoerceFnReturnWideOkType) {
    // mk() returns Result<int, Error>; binding to Result<bool, Error> is a
    // single-slot Ok mismatch. srcErrTy==dstErrTy so the old compile-time
    // path would copy the Err slot — silently zeroing the Ok payload when
    // the runtime disc is 1 (Ok). The fix rejects this with a type error.
    expectCompileError(
        "fn mk() -> Result<int, Error>:\n"
        "  return Ok(42)\n"
        "r: Result<bool, Error> = mk()\n",
        "type error: annotation");
}

// ============================================================
// #1156: tuple pattern on Option / Result must be rejected
// ============================================================

// Option<T> is represented as {i1, T} — a 2-element struct.  Before #1156
// the structural guard only fired when subjectEnumType was non-empty, so an
// Option<int> subject silently passed the arity check for `(a, b)`.
TEST_F(CodeGenTest, TuplePatternOnOptionSubjectRejected) {
    expectCompileError(
        "opt: int? = Some(1)\n"
        "case opt:\n"
        "  (a, b):\n"
        "    print(a)\n"
        "  _:\n"
        "    print(\"other\")\n",
        "tuple pattern applied to non-tuple subject");
}

// Result<T, E> is {i1, T, E}.  Arity 3 already triggered an arity-mismatch
// error for a 2-element pattern, but the error text was wrong.  After #1156
// both Option and Result use the same structural rejection path.
TEST_F(CodeGenTest, TuplePatternOnResultSubjectRejected) {
    expectCompileError(
        "fn mk() -> Result<int, str>:\n"
        "  return Ok(1)\n"
        "r = mk()\n"
        "case r:\n"
        "  (a, b, c):\n"
        "    print(a)\n"
        "  _:\n"
        "    print(\"other\")\n",
        "tuple pattern applied to non-tuple subject");
}

// #1203 Case 1: direct inline self-referential enum (e.g.
// `enum Tree: Leaf(int), Node(int, Tree, Tree)`) currently cannot be laid out
// because the payload struct would have infinite size.  Emit a precise
// diagnostic pointing users to indirection wrappers instead of the cryptic
// "unknown type: Tree".
TEST_F(CodeGenTest, RecursiveEnumInlineSelfRefDiagnostic) {
    expectCompileError(
        "enum Tree:\n"
        "  Leaf(int)\n"
        "  Node(int, Tree, Tree)\n",
        {"enum 'Tree'", "self-referential", "List<Tree>", "Task<Tree>",
         "Channel<Tree>"});
}

TEST_F(CodeGenTest, RecursiveEnumInlineSelfRefAllowedInsideList) {
    // `List<Tree>` makes the self-reference indirect through a pointer, so
    // the layout is finite — this must compile.
    ASSERT_NO_THROW(compileSource(
        "enum Tree:\n"
        "  Leaf(int)\n"
        "  Node(int, List<Tree>)\n"));
}

TEST_F(CodeGenTest, RecursiveEnumInlineSelfRefAllowedInsideMap) {
    ASSERT_NO_THROW(compileSource(
        "enum Tree:\n"
        "  Leaf(int)\n"
        "  Node(int, Map<int, Tree>)\n"));
}

TEST_F(CodeGenTest, RecursiveEnumInlineSelfRefAllowedInsideSet) {
    ASSERT_NO_THROW(compileSource(
        "enum Tree:\n"
        "  Leaf(int)\n"
        "  Node(int, Set<Tree>)\n"));
}

TEST_F(CodeGenTest, RecursiveEnumInlineSelfRefAllowedInsideTask) {
    // `Task<Tree>` stores the payload behind an async-handle pointer, so the
    // enum layout is finite.
    ASSERT_NO_THROW(compileSource(
        "enum Tree:\n"
        "  Leaf(int)\n"
        "  Node(int, Task<Tree>)\n"));
}

TEST_F(CodeGenTest, RecursiveEnumInlineSelfRefAllowedInsideChannel) {
    // `Channel<Tree>` stores the payload behind a channel-handle pointer, so
    // the enum layout is finite.
    ASSERT_NO_THROW(compileSource(
        "enum Tree:\n"
        "  Leaf(int)\n"
        "  Node(int, Channel<Tree>)\n"));
}

// #1203: generic enums that self-reference (e.g.
// `enum LList<T>: Cons(T, LList<T>)`) must emit the same wrapper-type
// diagnostic at declaration time — not the cryptic "unknown type: T"
// that surfaces only when an instantiation like `LList<int>` is attempted.
// The suggestion must include the type parameters so the hint is a
// syntactically valid Ry type (`List<LList<T>>`, not `List<LList>`).
TEST_F(CodeGenTest, RecursiveGenericEnumInlineSelfRefDiagnostic) {
    expectCompileError(
        "enum LList<T>:\n"
        "  Cons(T, LList<T>)\n"
        "  Nil\n",
        {"enum 'LList'", "self-referential", "List<LList<T>>"});
}

// #1203: the counter-example — wrapping the generic self-ref in a
// `List<...>` must compile cleanly and not over-trigger the
// `ftBase == s.name` base-name match.
TEST_F(CodeGenTest, RecursiveGenericEnumInlineSelfRefAllowedInsideList) {
    ASSERT_NO_THROW(compileSource(
        "enum LList<T>:\n"
        "  Cons(T, List<LList<T>>)\n"
        "  Nil\n"));
}

// #1203: using the bare name of a generic enum as a type (without type
// arguments) now produces a specific diagnostic instead of the cryptic
// "unknown type: MyOpt". The hint echoes the enum's actual type-parameter
// name so the suggestion is concrete — here `Item`, not a hardcoded `T`.
TEST_F(CodeGenTest, BareGenericEnumNameWithoutTypeArgsRejected) {
    expectCompileError(
        "enum MyOpt<Item>:\n"
        "  MySome(Item)\n"
        "  MyNone\n"
        "fn unwrapOr(opt: MyOpt, default: int) -> int:\n"
        "  return default\n",
        {"generic enum 'MyOpt'", "without type arguments", "MyOpt<Item>"});
}

// #1203: optional-wrapped self reference (`Tree?`) must be rejected with the
// same wrapper diagnostic — an `Option<T>` payload is inlined, not boxed,
// so the layout would still be infinite. The previous substr-based check
// missed this shape because the field's stringified type is `Tree?`, whose
// base-before-`<` is `Tree?`, not `Tree`.
TEST_F(CodeGenTest, RecursiveEnumOptionalSelfRefRejected) {
    expectCompileError(
        "enum Tree:\n"
        "  Leaf(int)\n"
        "  Node(int, Tree?)\n",
        {"enum 'Tree'", "self-referential", "List<Tree>"});
}

// #1203: tuple-wrapped self reference — tuples are inlined into the payload,
// so `(int, Tree)` still has infinite size.
TEST_F(CodeGenTest, RecursiveEnumTupleSelfRefRejected) {
    expectCompileError(
        "enum Tree:\n"
        "  Leaf(int)\n"
        "  Node((int, Tree))\n",
        {"enum 'Tree'", "self-referential", "List<Tree>"});
}

// #1203: `Option<Tree>` is an ADT whose payload is inlined (no pointer
// indirection), so the self-reference is still infinite and must be
// rejected. `List<Tree>` is the right fix and is already covered above.
TEST_F(CodeGenTest, RecursiveEnumOptionGenericSelfRefRejected) {
    expectCompileError(
        "enum Tree:\n"
        "  Leaf(int)\n"
        "  Node(int, Option<Tree>)\n",
        {"enum 'Tree'", "self-referential", "List<Tree>"});
}

// #1203: nested generic enums of the form `enum Outer<T>: Wrap(Inner<T>)`
// now compile — `instantiateGenericEnum` must route field-type names
// through `substituteTypeParamsInName` (not only the outermost bare `T`)
// so `Inner<T>` is rewritten to `Inner<int>` at instantiation time.
TEST_F(CodeGenTest, NestedGenericEnumFieldCompiles) {
    ASSERT_NO_THROW(compileSource(
        "enum Inner<T>:\n"
        "  In(T)\n"
        "enum Outer<T>:\n"
        "  Wrap(Inner<T>)\n"
        "fn mk() -> Outer<int>:\n"
        "  return Outer<int>::Wrap(Inner<int>::In(1))\n"));
}

// ============================================================
// #1569: Referencing a multi-overload @native function as a
// first-class value must produce a clear "ambiguous" diagnostic,
// mirroring the user-fn behavior in emitExprVariant(VariableExpr).
// Single-overload @native references succeed (covered by
// tests/spec/native_first_class.test.ry); multi-overload
// references must reject because the materialized thunk would
// have no unambiguous signature.
//
// Inline @native declarations match what convert.ry exposes.
// CodeGenTest::runSource bypasses ModuleLoader so we cannot
// `from convert import toStr`.
// ============================================================

TEST_F(CodeGenTest, MultiOverloadNativeReferenceRejected) {
    expectCompileError(
        "@native\n"
        "fn toStr(value: int) -> str\n"
        "@native\n"
        "fn toStr(value: float) -> str\n"
        "@native\n"
        "fn toStr(value: bool) -> str\n"
        "f = toStr\n",
        {"toStr", "ambiguous"});
}

// math.pow has multi-arity overloads ((float, float) and (int, int))
// declared in share/std/math/math.ry. Even though codegen routes to
// custom emitters via emitMathPow, `let f = pow` must hit the same
// multi-overload reject path because the materialized thunk has no
// unambiguous arity/type signature.
TEST_F(CodeGenTest, MultiOverloadNativeCustomEmitterReferenceRejected) {
    expectCompileError(
        "@native\n"
        "fn pow(x: float, y: float) -> float\n"
        "@native\n"
        "fn pow(x: int, y: int) -> int\n"
        "f = pow\n",
        {"pow", "ambiguous"});
}

// Multi-overload user fn must shadow @native so that VariableExpr
// resolution yields a "undefined variable" diagnostic, not the
// misleading "no matching overload" error that would arise if
// materializeNativeThunk synthesized a CallExpr that re-dispatched
// through the user-fn overload set.
TEST_F(CodeGenTest, UserFnMultiOverloadShadowsNativeFirstClass) {
    expectCompileError(
        "@native\n"
        "fn natFoo(x: str) -> int\n"
        "fn natFoo(x: int) -> int:\n"
        "  return x\n"
        "fn natFoo(x: float) -> int:\n"
        "  return x as int\n"
        "f = natFoo\n",
        "undefined variable");
}

// ============================================================
// #1577: type-check overloaded calls before codegen
//
// Calling `range`/`len`/`enumerate` with arg types that don't match any
// overload must produce the canonical "no matching overload" diagnostic
// with a candidate list, NOT a low-level LLVM IR verify error.
// ============================================================

// Note: `range` is a builtin whose @native signatures are normally provided
// by `share/std/builtins.ry`, but the C++ test harness (`runSource` /
// `expectCompileError`) bypasses ModuleLoader. To exercise the candidate-list
// branch of the diagnostic, declare the three overloads inline so they
// populate `native_fn_sigs_["range"]`.
TEST_F(CodeGenTest, RangeRejectsListArgWithCandidateList) {
    expectCompileError(
        "@native\n"
        "fn range(count: int) -> List<int>\n"
        "@native\n"
        "fn range(start: int, end: int) -> List<int>\n"
        "@native\n"
        "fn range(start: int, end: int, step: int) -> List<int>\n"
        "for x in range(1..5):\n"
        "  print(x)\n",
        {"no matching overload for `range`",
         "candidates:",
         "range(int) -> List<int>",
         "range(int, int) -> List<int>",
         "range(int, int, int) -> List<int>",
         "but called with: range(List<int>)"});
}

TEST_F(CodeGenTest, LenRejectsIntArgWithCandidateList) {
    expectCompileError(
        "x = len(42)\n",
        {"no matching overload for `len`",
         "but called with: len(int)"});
}

TEST_F(CodeGenTest, EnumerateRejectsIntArgWithCandidateList) {
    expectCompileError(
        "for i, v in enumerate(42):\n"
        "  print(v)\n",
        {"no matching overload for `enumerate`",
         "but called with: enumerate(int)"});
}

// Path 3: user-defined fn overloads must produce the canonical "no matching
// overload" diagnostic with a candidate list when no overload accepts the
// supplied argument types.
TEST_F(CodeGenTest, UserFnOverloadRejectsMismatchWithCandidateList) {
    expectCompileError(
        "fn foo(x: int) -> int:\n"
        "  return x\n"
        "fn foo(x: str) -> int:\n"
        "  return len(x)\n"
        "y = foo(1.5)\n",
        {"no matching overload for `foo`",
         "candidates:",
         "foo(int) -> int",
         "foo(str) -> int",
         "but called with: foo(float)"});
}

// Path 3: ambiguous user-defined overloads (multiple candidates tied on the
// ranking metric — here, both score 1 unionMatch on a `1` argument) must
// produce the canonical diagnostic with a candidate list rather than an
// unsigned-comparison or terse message.
TEST_F(CodeGenTest, UserFnOverloadAmbiguousCallEmitsCandidateList) {
    expectCompileError(
        "fn foo(x: int|str) -> int:\n"
        "  return 1\n"
        "fn foo(x: int|float) -> int:\n"
        "  return 2\n"
        "y = foo(1)\n",
        {"ambiguous call to `foo`",
         "candidates:",
         "foo(int | str) -> int",
         "foo(int | float) -> int",
         "but called with: foo(int)"});
}

// ============================================================
// `toMatch` matcher requires str operands on both sides
// (#1676). The rejection branch lives in `emitStmt(ExpectStmt&)`
// so it is only reachable in test_mode; use `runTestSource` plus
// a manual try/catch (the `expectCompileError` helper compiles
// outside test mode and would fail earlier on the `expect()`
// guard). See `.claude/rules/tests-rejection-tdd.md` P8.
// ============================================================

TEST_F(CodeGenTest, ExpectToMatchRejectsNonStrActual) {
    try {
        runTestSource("expect(42).toMatch(\"foo\")\n");
        FAIL() << "Expected compile error for non-str actual";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("toMatch: requires str operands"),
                  std::string::npos)
            << "got: " << msg;
    }
}

TEST_F(CodeGenTest, ExpectToMatchRejectsNonStrPattern) {
    try {
        runTestSource("expect(\"foo\").toMatch(42)\n");
        FAIL() << "Expected compile error for non-str pattern";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("toMatch: requires str operands"),
                  std::string::npos)
            << "got: " << msg;
    }
}

// ============================================================
// `toBeCloseTo` matcher rejection branches (#1675).
// Same harness pattern as the `toMatch` rejection tests above —
// the checks live inside `emitStmt(ExpectStmt&)` so they only
// fire under test_mode (`runTestSource`).
// ============================================================

TEST_F(CodeGenTest, ExpectToBeCloseToRejectsNonNumericActual) {
    try {
        runTestSource("expect(\"foo\").toBeCloseTo(0.3)\n");
        FAIL() << "Expected compile error for non-numeric actual";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("toBeCloseTo: requires int or float operands"),
                  std::string::npos)
            << "got: " << msg;
    }
}

TEST_F(CodeGenTest, ExpectToBeCloseToRejectsNonNumericExpected) {
    try {
        runTestSource("expect(0.3).toBeCloseTo(\"foo\")\n");
        FAIL() << "Expected compile error for non-numeric expected";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("toBeCloseTo: requires int or float operands"),
                  std::string::npos)
            << "got: " << msg;
    }
}

TEST_F(CodeGenTest, ExpectToBeCloseToRejectsNonLiteralDecimals) {
    try {
        runTestSource(
            "n = 4\n"
            "expect(0.1 + 0.2).toBeCloseTo(0.3, n)\n"
        );
        FAIL() << "Expected compile error for non-literal decimals";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("'decimals' must be a plain integer literal"),
                  std::string::npos)
            << "got: " << msg;
    }
}

TEST_F(CodeGenTest, ExpectToBeCloseToRejectsDecimalsOverflow) {
    try {
        runTestSource("expect(0.1 + 0.2).toBeCloseTo(0.3, 16)\n");
        FAIL() << "Expected compile error for decimals > 15";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("'decimals' must be in [0, 15]"),
                  std::string::npos)
            << "got: " << msg;
    }
}

TEST_F(CodeGenTest, ExpectToBeCloseToRejectsNegativeDecimals) {
    try {
        runTestSource("expect(0.1 + 0.2).toBeCloseTo(0.3, -1)\n");
        FAIL() << "Expected compile error for negative decimals";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        // Negative literals arrive as UnaryExpr-wrapped NumberExpr
        // and fail the std::get_if<NumberExpr> check, producing
        // the "must be a plain integer literal" diagnostic.
        EXPECT_NE(msg.find("'decimals' must be a plain integer literal"),
                  std::string::npos)
            << "got: " << msg;
    }
}

// ============================================================
// `using` statement (#1817): init expression must produce an
// `io.File` value.  Codegen rejects int, str, List, and any
// other non-File type at the `detectResourceKind` / `isFile`
// gate in `emitStmt(UsingStmt)`.
// ============================================================

TEST_F(CodeGenTest, UsingRejectsIntInit) {
    expectCompileError(
        "using x = 5:\n"
        "    print(x)\n",
        "using requires an io.File value");
}

TEST_F(CodeGenTest, UsingRejectsStringInit) {
    expectCompileError(
        "using x = \"hello\":\n"
        "    print(x)\n",
        "using requires an io.File value");
}

TEST_F(CodeGenTest, UsingRejectsListInit) {
    expectCompileError(
        "using x = [1, 2, 3]:\n"
        "    print(x)\n",
        "using requires an io.File value");
}

// ============================================================
// #1889: stdlib built-in function names cannot be shadowed by
// user-defined top-level `fn`. Without this guard, the hardcoded
// dispatch chain (`emitBuiltin*`) silently overrides the user fn,
// most dangerously when the signature matches exactly
// (`fn sum(v: List<int>) -> int: return 999` — body ignored,
// stdlib executes instead). The guard fires for the user fn
// declaration, not at the call site, so the error is surfaced
// before any silent shadow has a chance to occur.
// ============================================================

// Issue reproduction: 4 signature variants of `fn sum` — every one
// must be rejected, including the exact-signature shadow that was
// the original silent-shadow bug.
TEST_F(CodeGenTest, ReservedBuiltinSumTwoIntArgs) {
    expectCompileError(
        "fn sum(a: int, b: int) -> int:\n"
        "    return a + b\n",
        "is reserved for a built-in");
}

TEST_F(CodeGenTest, ReservedBuiltinSumStrArg) {
    expectCompileError(
        "fn sum(s: str) -> int:\n"
        "    return 0\n",
        "is reserved for a built-in");
}

TEST_F(CodeGenTest, ReservedBuiltinSumNoArgs) {
    expectCompileError(
        "fn sum() -> int:\n"
        "    return 0\n",
        "is reserved for a built-in");
}

TEST_F(CodeGenTest, ReservedBuiltinSumListIntExactSignature) {
    expectCompileError(
        "fn sum(v: List<int>) -> int:\n"
        "    return 999\n",
        "is reserved for a built-in");
}

// Family representatives: at least one rejection per stdlib family
// (builtins.ry auto-loaded fn / collection ops / conversion / query /
// ADT constructor / regex / IO / channel / set ops / arithmetic
// checked/saturating/wrapping).
TEST_F(CodeGenTest, ReservedBuiltinPrint) {
    expectCompileError(
        "fn print(x: int) -> int:\n"
        "    return x\n",
        "is reserved for a built-in");
}

TEST_F(CodeGenTest, ReservedBuiltinLen) {
    expectCompileError(
        "fn len(x: int) -> int:\n"
        "    return x\n",
        "is reserved for a built-in");
}

TEST_F(CodeGenTest, ReservedBuiltinRange) {
    expectCompileError(
        "fn range(n: int) -> int:\n"
        "    return n\n",
        "is reserved for a built-in");
}

TEST_F(CodeGenTest, ReservedBuiltinMin) {
    expectCompileError(
        "fn min(x: int) -> int:\n"
        "    return x\n",
        "is reserved for a built-in");
}

TEST_F(CodeGenTest, ReservedBuiltinMax) {
    expectCompileError(
        "fn max(x: int) -> int:\n"
        "    return x\n",
        "is reserved for a built-in");
}

// ADT constructor names (Ok, Err, Some, None, Error) appear in
// `kReservedBuiltinFunctionNames` for defense-in-depth, but the parser
// independently rejects PascalCase `fn` names with "must be camelCase".
// That sibling rule renders the reserved-builtin guard unreachable for
// these specific names today, so no direct rejection test is added.

// `toStr` is intentionally excluded from kReservedBuiltinFunctionNames:
// type-aware dispatch for records consults user `fn toStr(p: RecordType)`
// BEFORE the auto-generated builtin (see tests/spec/records.test.ry's
// "should override auto-generated toStr with user-defined" case and the
// CodeGenTest.RecordUserDefinedToStr family). A top-level
// `fn toStr(p: Point)` is therefore legitimate Ry, not a silent shadow.

TEST_F(CodeGenTest, ReservedBuiltinTypeOf) {
    expectCompileError(
        "fn typeOf(x: int) -> int:\n"
        "    return x\n",
        "is reserved for a built-in");
}

TEST_F(CodeGenTest, ReservedBuiltinIter) {
    expectCompileError(
        "fn iter(x: int) -> int:\n"
        "    return x\n",
        "is reserved for a built-in");
}

TEST_F(CodeGenTest, ReservedBuiltinReverseMutating) {
    expectCompileError(
        "fn reverse!(x: int) -> int:\n"
        "    return x\n",
        "is reserved for a built-in");
}

// The error message must name the specific function so the user
// can fix the right declaration when many fns share a file.
TEST_F(CodeGenTest, ReservedBuiltinErrorNamesTheFunction) {
    expectCompileError(
        "fn sum(a: int, b: int) -> int:\n"
        "    return a + b\n",
        "'sum'");
}

// Generic fn templates also flow through reserved-name rejection.
// `generic_fn_templates_` is a flat top-level map, so shadowing is
// just as silent without the guard.
TEST_F(CodeGenTest, ReservedBuiltinGenericMap) {
    expectCompileError(
        "fn map<T, U>(x: T) -> U:\n"
        "    return x as U\n",
        "is reserved for a built-in");
}

TEST_F(CodeGenTest, ReservedBuiltinGenericMin) {
    expectCompileError(
        "fn min<T>(x: T) -> T:\n"
        "    return x\n",
        "is reserved for a built-in");
}

TEST_F(CodeGenTest, ReservedBuiltinGenericFilter) {
    expectCompileError(
        "fn filter<T>(x: T) -> T:\n"
        "    return x\n",
        "is reserved for a built-in");
}

// ============================================================
// #1889 positive cases: the guard MUST NOT over-fire. False
// positives would block legitimate code, so each accept-path is
// pinned by a positive test that proves the input compiles.
// ============================================================

// Non-collision names compile cleanly even when they share a prefix
// or suffix with a reserved name.
TEST_F(CodeGenTest, ReservedBuiltinAllowsDistinctName) {
    EXPECT_NO_THROW(runSource(
        "fn mySum(x: int) -> int:\n"
        "    return x\n"
        "print(mySum(5))\n"
    ));
}

// Nested fns are exempt from the reserved-name reject — the
// declaration is accepted at codegen time. For hardcoded-dispatch
// builtins like `sum`, the call site silently dispatches to stdlib
// instead of the nested fn (a pre-existing limitation outside #1889's
// scope). This test verifies declaration acceptance only.
TEST_F(CodeGenTest, ReservedBuiltinAllowsNestedShadow) {
    EXPECT_NO_THROW(runSource(
        "fn outer() -> int:\n"
        "    fn sum(x: int) -> int:\n"
        "        return x + 1\n"
        "    return 0\n"
        "print(outer())\n"
    ));
}

// `toStr` is excluded from the reserved set (see
// `kReservedBuiltinFunctionNames` in include/ry/builtin_names.hpp),
// so a `fn toStr(p: Point)` override is legal at any scope. This test
// pins the nested form; the top-level form is exercised by the
// `RecordUserDefinedToStr` family in tests/test_codegen_record.cpp.
TEST_F(CodeGenTest, NestedToStrOverrideWorks) {
    EXPECT_NO_THROW(runSource(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "fn outer() -> str:\n"
        "    fn toStr(p: Point) -> str:\n"
        "        return f\"({p.x}, {p.y})\"\n"
        "    p = Point(1, 2)\n"
        "    return toStr(p)\n"
        "print(outer())\n"
    ));
}

// Fall-through builtin names (the 154 names NOT in the empirically
// filtered 84-name reserved set) keep working — declaring them as
// user fns is the long-standing override pattern and the 84-name
// list was chosen to preserve that exactly.
TEST_F(CodeGenTest, ReservedBuiltinAllowsFallthroughAdd) {
    EXPECT_NO_THROW(runSource(
        "fn add(x: int, y: int) -> int:\n"
        "    return x + y\n"
        "print(add(2, 3))\n"
    ));
}

TEST_F(CodeGenTest, ReservedBuiltinAllowsFallthroughAbs) {
    EXPECT_NO_THROW(runSource(
        "fn abs(x: int) -> int:\n"
        "    if x < 0:\n"
        "        return -x\n"
        "    return x\n"
        "print(abs(-7))\n"
    ));
}

// `@native` declarations are exempt by design — the stdlib itself
// declares reserved names like `@native fn sum(...)` and `@native fn
// load<T>(...)`. These are exercised indirectly every time a test
// loads `share/std/higher_order.ry` (Guard 1 path) or
// `share/std/json/json.ry` (Guard 2 path), but explicit coverage
// makes the intent obvious and prevents regression. `compileSource`
// is used (not `runSource`) because `@native` declarations have no
// body — there is nothing to JIT-execute.
TEST_F(CodeGenTest, ReservedBuiltinAllowsNativeDeclaration) {
    EXPECT_NO_THROW(compileSource(
        "@native\n"
        "fn sum(a: int, b: int) -> int\n"
    ));
}

TEST_F(CodeGenTest, ReservedBuiltinAllowsNativeGenericDeclaration) {
    EXPECT_NO_THROW(compileSource(
        "@native\n"
        "fn map<T, U>(xs: List<T>, f: fn(T) -> U) -> List<U>\n"
    ));
}
