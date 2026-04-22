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

TEST_F(CodeGenTest, HelperReturnRejectsIncompatibleThreadResultMetadataAcrossBranches) {
    expectCompileError(
        "@native(\"thread\")\n"
        "function thread_spawn(body: function() -> any) -> Thread\n"
        "function mk_thread(flag: bool) -> Thread:\n"
        "  if flag:\n"
        "    return thread_spawn(() => 1)\n"
        "  return thread_spawn(() => true)\n",
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
        "function f() -> Result<bool, MyErr1>:\n"
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
        "function g() -> Result<i8, Error>:\n"
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
        "function mk() -> Result<int, Error>:\n"
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
        "function mk() -> Result<int, str>:\n"
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
        {"enum 'Tree'", "self-referential", "List<Tree>"});
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
        "function unwrap_or(opt: MyOpt, default: int) -> int:\n"
        "  return default\n",
        {"generic enum 'MyOpt'", "without type arguments", "MyOpt<Item>"});
}

// #1203: optional-wrapped self reference (`Tree?`) must be rejected with the
// same wrapper diagnostic — an `Option<T>` payload is inlined, not boxed,
// so the layout would still be infinite. The previous substring-based check
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
        "function mk() -> Outer<int>:\n"
        "  return Outer<int>::Wrap(Inner<int>::In(1))\n"));
}
