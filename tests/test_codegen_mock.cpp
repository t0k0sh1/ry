#include "test_codegen_common.hpp"


using namespace ry;
// ============================================================
// Basic mock: function return value is replaced
// ============================================================

TEST_F(CodeGenTest, MockBasicReplace) {
    EXPECT_EQ(runTestSource(withStdlibDirectiveDecls(
        "fn greet() -> str:\n"
        "    return \"hello\"\n"
        "\n"
        "@describe(\"mock basic\")\n"
        "fn mockBasic():\n"
        "    @it(\"replaces function\")\n"
        "    fn replacesFunction():\n"
        "        mock(greet, () => \"mocked\")\n"
        "        expect(greet()).toEq(\"mocked\")\n"
    )), "mock basic\n  \033[32m+ replaces function\033[0m\n\n1 passed, 0 failed\n");
}

// ============================================================
// Mock with arguments
// ============================================================

TEST_F(CodeGenTest, MockWithArgs) {
    EXPECT_EQ(runTestSource(withStdlibDirectiveDecls(
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "\n"
        "@describe(\"mock with args\")\n"
        "fn mockWithArgs():\n"
        "    @it(\"replaces function with args\")\n"
        "    fn replacesFunctionWithArgs():\n"
        "        mock(add, (a: int, b: int) => a * b)\n"
        "        expect(add(3, 4)).toEq(12)\n"
    )), "mock with args\n  \033[32m+ replaces function with args\033[0m\n\n1 passed, 0 failed\n");
}

// ============================================================
// Auto-restore between it blocks
// ============================================================

TEST_F(CodeGenTest, MockAutoRestore) {
    EXPECT_EQ(runTestSource(withStdlibDirectiveDecls(
        "fn greet() -> str:\n"
        "    return \"hello\"\n"
        "\n"
        "@describe(\"mock restore\")\n"
        "fn mockRestore():\n"
        "    @it(\"mocks\")\n"
        "    fn mocks():\n"
        "        mock(greet, () => \"mocked\")\n"
        "        expect(greet()).toEq(\"mocked\")\n"
        "\n"
        "    @it(\"auto-restores\")\n"
        "    fn autoRestores():\n"
        "        expect(greet()).toEq(\"hello\")\n"
    )), "mock restore\n  \033[32m+ mocks\033[0m\n  \033[32m+ auto-restores\033[0m\n\n2 passed, 0 failed\n");
}

// ============================================================
// verify() returns call count
// ============================================================

TEST_F(CodeGenTest, MockVerifyCallCount) {
    EXPECT_EQ(runTestSource(withStdlibDirectiveDecls(
        "fn greet() -> str:\n"
        "    return \"hello\"\n"
        "\n"
        "@describe(\"verify\")\n"
        "fn verifyGroup():\n"
        "    @it(\"counts calls\")\n"
        "    fn countsCalls():\n"
        "        mock(greet, () => \"mocked\")\n"
        "        greet()\n"
        "        greet()\n"
        "        greet()\n"
        "        expect(verify(\"greet\")).toEq(3)\n"
    )), "verify\n  \033[32m+ counts calls\033[0m\n\n1 passed, 0 failed\n");
}

// ============================================================
// Mock function used as expression / return value
// ============================================================

TEST_F(CodeGenTest, MockFunctionUsedAsExpr) {
    // verify() works when mock is called and function result is used
    EXPECT_EQ(runTestSource(withStdlibDirectiveDecls(
        "fn getValue() -> int:\n"
        "    return 100\n"
        "\n"
        "@describe(\"mock expr\")\n"
        "fn mockExpr():\n"
        "    @it(\"tracks calls in expressions\")\n"
        "    fn tracksCallsInExpressions():\n"
        "        mock(getValue, () => 999)\n"
        "        x = getValue()\n"
        "        expect(x).toEq(999)\n"
        "        expect(verify(\"getValue\")).toEq(1)\n"
    )), "mock expr\n  \033[32m+ tracks calls in expressions\033[0m\n\n1 passed, 0 failed\n");
}

// ============================================================
// mock() outside test mode -> compile error
// ============================================================

TEST_F(CodeGenTest, MockOutsideTestModeError) {
    EXPECT_THROW(runSource(
        "fn greet() -> str:\n"
        "    return \"hello\"\n"
        "mock(greet, () => \"mocked\")\n"
    ), std::exception);
}

// ============================================================
// mock() on non-existent function -> compile error
// ============================================================

TEST_F(CodeGenTest, MockNonExistentFunctionError) {
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "@describe(\"error\")\n"
        "fn errorGroup():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(no_such_fn, () => \"x\")\n"
    )), std::exception);
}

// ============================================================
// mock() with type mismatch -> compile error
// ============================================================

TEST_F(CodeGenTest, MockTypeMismatchError) {
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "fn greet() -> str:\n"
        "    return \"hello\"\n"
        "\n"
        "@describe(\"error\")\n"
        "fn errorGroup():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(greet, () => 42)\n"
    )), std::exception);
}

TEST_F(CodeGenTest, MockedFunctionStillChecksRequire) {
    std::string src = withStdlibDirectiveDecls(
        "fn deposit(amount: int, balance: int) -> int:\n"
        "    require:\n"
        "        amount > 0\n"
        "        balance >= 0\n"
        "    return balance + amount\n"
        "\n"
        "@describe(\"mock contracts\")\n"
        "fn mockContracts():\n"
        "    @it(\"checks require on mocked calls\")\n"
        "    fn checksRequireOnMockedCalls():\n"
        "        mock(deposit, (amount: int, balance: int) => balance + amount)\n"
        "        deposit(-100, -200)\n"
    );
    EXPECT_EXIT(runTestSource(src), ::testing::ExitedWithCode(1), "");
}

TEST_F(CodeGenTest, MockedFunctionStillChecksEnsure) {
    std::string src = withStdlibDirectiveDecls(
        "fn deposit(amount: int, balance: int) -> int:\n"
        "    require:\n"
        "        amount > 0\n"
        "        balance >= 0\n"
        "    ensure v:\n"
        "        v > balance\n"
        "    return balance + amount\n"
        "\n"
        "@describe(\"mock contracts\")\n"
        "fn mockContracts():\n"
        "    @it(\"checks ensure on mocked calls\")\n"
        "    fn checksEnsureOnMockedCalls():\n"
        "        mock(deposit, (amount: int, balance: int) => -999)\n"
        "        deposit(10, 20)\n"
    );
    EXPECT_EXIT(runTestSource(src), ::testing::ExitedWithCode(1), "");
}

// ============================================================
// verifyCalledWith() rejection tests (#1677)
// ============================================================

TEST_F(CodeGenTest, VerifyCalledWithUnmockedFunctionError) {
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "fn compute(x: int) -> int:\n"
        "    return x\n"
        "\n"
        "@describe(\"vcw error\")\n"
        "fn vcwError():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        expect(verifyCalledWith(\"compute\", 5)).toEq(0)\n"
    )), std::exception);
}

TEST_F(CodeGenTest, VerifyCalledWithUnknownFunctionError) {
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "@describe(\"vcw error\")\n"
        "fn vcwError():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        expect(verifyCalledWith(\"no_such_fn\", 5)).toEq(0)\n"
    )), std::exception);
}

TEST_F(CodeGenTest, VerifyCalledWithArityMismatchError) {
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "fn compute(x: int) -> int:\n"
        "    return x\n"
        "\n"
        "@describe(\"vcw arity\")\n"
        "fn vcwArity():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(compute, (x: int) => x)\n"
        "        compute(5)\n"
        "        expect(verifyCalledWith(\"compute\", 5, 6)).toEq(0)\n"
    )), std::exception);
}

TEST_F(CodeGenTest, VerifyCalledWithTypeMismatchError) {
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "fn compute(x: int) -> int:\n"
        "    return x\n"
        "\n"
        "@describe(\"vcw type\")\n"
        "fn vcwType():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(compute, (x: int) => x)\n"
        "        compute(5)\n"
        "        expect(verifyCalledWith(\"compute\", \"five\")).toEq(0)\n"
    )), std::exception);
}

TEST_F(CodeGenTest, VerifyCalledWithListIntArgAccepted) {
    // List<int> arg now accepted (#1703). 1 matching call -> count = 1.
    EXPECT_EQ(runTestSource(withStdlibDirectiveDecls(
        "fn takesList(xs: List<int>) -> int:\n"
        "    return len(xs)\n"
        "\n"
        "@describe(\"vcw list\")\n"
        "fn vcwList():\n"
        "    @it(\"counts list arg\")\n"
        "    fn countsListArg():\n"
        "        mock(takesList, (xs: List<int>) => len(xs))\n"
        "        takesList([1, 2])\n"
        "        expect(verifyCalledWith(\"takesList\", [1, 2])).toEq(1)\n"
    )), "vcw list\n  \033[32m+ counts list arg\033[0m\n\n1 passed, 0 failed\n");
}

TEST_F(CodeGenTest, VerifyCalledWithListStrArgAccepted) {
    // List<str> arg exercises the ARC retain/release path on snapshot.
    EXPECT_EQ(runTestSource(withStdlibDirectiveDecls(
        "fn takesList(xs: List<str>) -> int:\n"
        "    return len(xs)\n"
        "\n"
        "@describe(\"vcw list str\")\n"
        "fn vcwListStr():\n"
        "    @it(\"counts list str arg\")\n"
        "    fn countsListStrArg():\n"
        "        mock(takesList, (xs: List<str>) => len(xs))\n"
        "        takesList([\"a\", \"b\"])\n"
        "        expect(verifyCalledWith(\"takesList\", [\"a\", \"b\"])).toEq(1)\n"
    )), "vcw list str\n  \033[32m+ counts list str arg\033[0m\n\n1 passed, 0 failed\n");
}

TEST_F(CodeGenTest, VerifyCalledWithNestedListArgUnsupportedError) {
    // List<List<int>> remains unsupported in v1; the parameter-side gate rejects.
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "fn takesNested(xs: List<List<int>>) -> int:\n"
        "    return len(xs)\n"
        "\n"
        "@describe(\"vcw nested list\")\n"
        "fn vcwNestedList():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(takesNested, (xs: List<List<int>>) => len(xs))\n"
        "        takesNested([[1, 2]])\n"
        "        expect(verifyCalledWith(\"takesNested\", [[1, 2]])).toEq(0)\n"
    )), std::exception);
}

TEST_F(CodeGenTest, VerifyCalledWithMapStrIntArgAccepted) {
    // #1705: Map<str, int> arg matches a recorded call with the same {k -> v} pairs.
    // Flipped from VerifyCalledWithMapArgUnsupportedError per
    // .claude/rules/tests-rejection-tdd.md (relaxing rejection: flip THROW -> NO_THROW).
    EXPECT_EQ(runTestSource(withStdlibDirectiveDecls(
        "fn takesMap(m: Map<str, int>) -> int:\n"
        "    return len(m)\n"
        "\n"
        "@describe(\"vcw map\")\n"
        "fn vcwMap():\n"
        "    @it(\"counts map arg\")\n"
        "    fn countsMapArg():\n"
        "        mock(takesMap, (m: Map<str, int>) => len(m))\n"
        "        takesMap({\"a\": 1})\n"
        "        expect(verifyCalledWith(\"takesMap\", {\"a\": 1})).toEq(1)\n"
    )), "vcw map\n  \033[32m+ counts map arg\033[0m\n\n1 passed, 0 failed\n");
}

TEST_F(CodeGenTest, VerifyCalledWithListParameterRejectedWithPrimitiveArg) {
    // A List<int> parameter must reject a primitive (str) argument. Both lower
    // to ptrTy_ under opaque pointers and isStringValue would tag the str arg
    // as kind=4, silently returning 0 instead of erroring. The explicit
    // list-vs-scalar consistency check in emitVerifyCalledWithCall (#1703)
    // catches this.
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "fn takesList(xs: List<int>) -> int:\n"
        "    return len(xs)\n"
        "\n"
        "@describe(\"vcw list param\")\n"
        "fn vcwListParam():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(takesList, (xs: List<int>) => len(xs))\n"
        "        takesList([1, 2])\n"
        "        expect(verifyCalledWith(\"takesList\", \"x\")).toEq(0)\n"
    )), std::exception);
}

TEST_F(CodeGenTest, VerifyCalledWithScalarParameterRejectedWithListArg) {
    // A str parameter must reject a List<int> argument. Both lower to ptrTy_,
    // so the LLVM type check passes; the explicit list-vs-scalar consistency
    // check (#1703) must fire to reject "argument is a List but parameter has
    // scalar type".
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "fn takesStr(s: str) -> int:\n"
        "    return len(s)\n"
        "\n"
        "@describe(\"vcw scalar param list arg\")\n"
        "fn vcwScalarParamListArg():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(takesStr, (s: str) => len(s))\n"
        "        takesStr(\"hi\")\n"
        "        expect(verifyCalledWith(\"takesStr\", [1, 2])).toEq(0)\n"
    )), std::exception);
}

TEST_F(CodeGenTest, VerifyCalledWithListParamRejectedWithListOfDifferentElementType) {
    // List<int> parameter must reject a List<str> argument (element type
    // mismatch). Both lower to a List ARC header (ptrTy_), so the LLVM type
    // check passes; the explicit element-type comparison must fire.
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "fn takesIntList(xs: List<int>) -> int:\n"
        "    return len(xs)\n"
        "\n"
        "@describe(\"vcw list elem mismatch\")\n"
        "fn vcwListElemMismatch():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(takesIntList, (xs: List<int>) => len(xs))\n"
        "        takesIntList([1, 2])\n"
        "        expect(verifyCalledWith(\"takesIntList\", [\"a\", \"b\"])).toEq(0)\n"
    )), std::exception);
}

// ============================================================
// verifyCalledWith(): Set<T> argument support (#1704)
// ============================================================

TEST_F(CodeGenTest, VerifyCalledWithSetIntArgAccepted) {
    // Set<int> arg matches a recorded call with the same elements.
    EXPECT_EQ(runTestSource(withStdlibDirectiveDecls(
        "fn takesSet(xs: Set<int>) -> int:\n"
        "    return len(xs)\n"
        "\n"
        "@describe(\"vcw set\")\n"
        "fn vcwSet():\n"
        "    @it(\"counts set arg\")\n"
        "    fn countsSetArg():\n"
        "        mock(takesSet, (xs: Set<int>) => len(xs))\n"
        "        takesSet({1, 2, 3})\n"
        "        expect(verifyCalledWith(\"takesSet\", {1, 2, 3})).toEq(1)\n"
    )), "vcw set\n  \033[32m+ counts set arg\033[0m\n\n1 passed, 0 failed\n");
}

TEST_F(CodeGenTest, VerifyCalledWithSetStrArgAccepted) {
    // Set<str> arg exercises the ARC retain/release path on the snapshot.
    EXPECT_EQ(runTestSource(withStdlibDirectiveDecls(
        "fn takesSet(xs: Set<str>) -> int:\n"
        "    return len(xs)\n"
        "\n"
        "@describe(\"vcw set str\")\n"
        "fn vcwSetStr():\n"
        "    @it(\"counts set str arg\")\n"
        "    fn countsSetStrArg():\n"
        "        mock(takesSet, (xs: Set<str>) => len(xs))\n"
        "        takesSet({\"a\", \"b\"})\n"
        "        expect(verifyCalledWith(\"takesSet\", {\"a\", \"b\"})).toEq(1)\n"
    )), "vcw set str\n  \033[32m+ counts set str arg\033[0m\n\n1 passed, 0 failed\n");
}

TEST_F(CodeGenTest, VerifyCalledWithSetUnorderedMatch) {
    // Sets compare unordered: recording {1, 2, 3} matches verifying {3, 2, 1}.
    EXPECT_EQ(runTestSource(withStdlibDirectiveDecls(
        "fn takesSet(xs: Set<int>) -> int:\n"
        "    return len(xs)\n"
        "\n"
        "@describe(\"vcw set unordered\")\n"
        "fn vcwSetUnordered():\n"
        "    @it(\"matches unordered\")\n"
        "    fn matchesUnordered():\n"
        "        mock(takesSet, (xs: Set<int>) => len(xs))\n"
        "        takesSet({1, 2, 3})\n"
        "        expect(verifyCalledWith(\"takesSet\", {3, 2, 1})).toEq(1)\n"
    )), "vcw set unordered\n  \033[32m+ matches unordered\033[0m\n\n1 passed, 0 failed\n");
}

TEST_F(CodeGenTest, VerifyCalledWithSetParameterRejectedWithPrimitiveArg) {
    // Set<int> parameter must reject a primitive (str) argument. Both lower
    // to ptrTy_ under opaque pointers; the explicit set-vs-scalar consistency
    // check must fire.
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "fn takesSet(xs: Set<int>) -> int:\n"
        "    return len(xs)\n"
        "\n"
        "@describe(\"vcw set param scalar arg\")\n"
        "fn vcwSetParamScalarArg():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(takesSet, (xs: Set<int>) => len(xs))\n"
        "        takesSet({1, 2})\n"
        "        expect(verifyCalledWith(\"takesSet\", \"x\")).toEq(0)\n"
    )), std::exception);
}

TEST_F(CodeGenTest, VerifyCalledWithScalarParameterRejectedWithSetArg) {
    // A str parameter must reject a Set<int> argument. Both lower to ptrTy_,
    // so the LLVM type check passes; the set-vs-scalar consistency check
    // must fire.
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "fn takesStr(s: str) -> int:\n"
        "    return len(s)\n"
        "\n"
        "@describe(\"vcw scalar param set arg\")\n"
        "fn vcwScalarParamSetArg():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(takesStr, (s: str) => len(s))\n"
        "        takesStr(\"hi\")\n"
        "        expect(verifyCalledWith(\"takesStr\", {1, 2})).toEq(0)\n"
    )), std::exception);
}

TEST_F(CodeGenTest, VerifyCalledWithSetParamRejectedWithSetOfDifferentElementType) {
    // Set<int> parameter must reject a Set<str> argument (element type
    // mismatch). Both lower to a Set ARC header (ptrTy_), so the LLVM type
    // check passes; the explicit element-type comparison must fire.
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "fn takesIntSet(xs: Set<int>) -> int:\n"
        "    return len(xs)\n"
        "\n"
        "@describe(\"vcw set elem mismatch\")\n"
        "fn vcwSetElemMismatch():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(takesIntSet, (xs: Set<int>) => len(xs))\n"
        "        takesIntSet({1, 2})\n"
        "        expect(verifyCalledWith(\"takesIntSet\", {\"a\", \"b\"})).toEq(0)\n"
    )), std::exception);
}

TEST_F(CodeGenTest, VerifyCalledWithListParamRejectedWithSetArg) {
    // List<int> parameter must reject a Set<int> argument. Even with the same
    // element type, kind (List vs Set) must match.
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "fn takesIntList(xs: List<int>) -> int:\n"
        "    return len(xs)\n"
        "\n"
        "@describe(\"vcw list param set arg\")\n"
        "fn vcwListParamSetArg():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(takesIntList, (xs: List<int>) => len(xs))\n"
        "        takesIntList([1, 2])\n"
        "        expect(verifyCalledWith(\"takesIntList\", {1, 2})).toEq(0)\n"
    )), std::exception);
}

TEST_F(CodeGenTest, VerifyCalledWithSetParamRejectedWithListArg) {
    // Set<int> parameter must reject a List<int> argument. Even with the same
    // element type, kind (Set vs List) must match.
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "fn takesIntSet(xs: Set<int>) -> int:\n"
        "    return len(xs)\n"
        "\n"
        "@describe(\"vcw set param list arg\")\n"
        "fn vcwSetParamListArg():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(takesIntSet, (xs: Set<int>) => len(xs))\n"
        "        takesIntSet({1, 2})\n"
        "        expect(verifyCalledWith(\"takesIntSet\", [1, 2])).toEq(0)\n"
    )), std::exception);
}

// ============================================================
// verifyCalledWith(): Map<K, V> argument support (#1705)
// ============================================================

TEST_F(CodeGenTest, VerifyCalledWithMapIntIntArgAccepted) {
    // Map<int, int>: integer key + integer value path (no ARC retain needed).
    EXPECT_EQ(runTestSource(withStdlibDirectiveDecls(
        "fn takesMap(m: Map<int, int>) -> int:\n"
        "    return len(m)\n"
        "\n"
        "@describe(\"vcw map int int\")\n"
        "fn vcwMapIntInt():\n"
        "    @it(\"counts map arg\")\n"
        "    fn countsMapArg():\n"
        "        mock(takesMap, (m: Map<int, int>) => len(m))\n"
        "        takesMap({1: 10, 2: 20})\n"
        "        expect(verifyCalledWith(\"takesMap\", {1: 10, 2: 20})).toEq(1)\n"
    )), "vcw map int int\n  \033[32m+ counts map arg\033[0m\n\n1 passed, 0 failed\n");
}

TEST_F(CodeGenTest, VerifyCalledWithMapStrStrArgAccepted) {
    // Map<str, str>: exercises ARC retain/release on BOTH keys and values.
    EXPECT_EQ(runTestSource(withStdlibDirectiveDecls(
        "fn takesMap(m: Map<str, str>) -> int:\n"
        "    return len(m)\n"
        "\n"
        "@describe(\"vcw map str str\")\n"
        "fn vcwMapStrStr():\n"
        "    @it(\"counts map arg\")\n"
        "    fn countsMapArg():\n"
        "        mock(takesMap, (m: Map<str, str>) => len(m))\n"
        "        takesMap({\"a\": \"x\", \"b\": \"y\"})\n"
        "        expect(verifyCalledWith(\"takesMap\", {\"a\": \"x\", \"b\": \"y\"})).toEq(1)\n"
    )), "vcw map str str\n  \033[32m+ counts map arg\033[0m\n\n1 passed, 0 failed\n");
}

TEST_F(CodeGenTest, VerifyCalledWithMapUnorderedMatch) {
    // Maps compare unordered: insertion order must not affect the match.
    EXPECT_EQ(runTestSource(withStdlibDirectiveDecls(
        "fn takesMap(m: Map<str, int>) -> int:\n"
        "    return len(m)\n"
        "\n"
        "@describe(\"vcw map unordered\")\n"
        "fn vcwMapUnordered():\n"
        "    @it(\"matches unordered\")\n"
        "    fn matchesUnordered():\n"
        "        mock(takesMap, (m: Map<str, int>) => len(m))\n"
        "        takesMap({\"a\": 1, \"b\": 2, \"c\": 3})\n"
        "        expect(verifyCalledWith(\"takesMap\", {\"c\": 3, \"a\": 1, \"b\": 2})).toEq(1)\n"
    )), "vcw map unordered\n  \033[32m+ matches unordered\033[0m\n\n1 passed, 0 failed\n");
}

TEST_F(CodeGenTest, VerifyCalledWithMapStrFloatArgAccepted) {
    // Map<str, float>: exercises value kind=2 (float) in mockArgEqual.
    EXPECT_EQ(runTestSource(withStdlibDirectiveDecls(
        "fn takesMap(m: Map<str, float>) -> int:\n"
        "    return len(m)\n"
        "\n"
        "@describe(\"vcw map str float\")\n"
        "fn vcwMapStrFloat():\n"
        "    @it(\"counts map arg\")\n"
        "    fn countsMapArg():\n"
        "        mock(takesMap, (m: Map<str, float>) => len(m))\n"
        "        takesMap({\"a\": 1.5, \"b\": 2.5})\n"
        "        expect(verifyCalledWith(\"takesMap\", {\"a\": 1.5, \"b\": 2.5})).toEq(1)\n"
    )), "vcw map str float\n  \033[32m+ counts map arg\033[0m\n\n1 passed, 0 failed\n");
}

TEST_F(CodeGenTest, VerifyCalledWithMapStrBoolArgAccepted) {
    // Map<str, bool>: exercises value kind=3 (bool) in mockArgEqual.
    EXPECT_EQ(runTestSource(withStdlibDirectiveDecls(
        "fn takesMap(m: Map<str, bool>) -> int:\n"
        "    return len(m)\n"
        "\n"
        "@describe(\"vcw map str bool\")\n"
        "fn vcwMapStrBool():\n"
        "    @it(\"counts map arg\")\n"
        "    fn countsMapArg():\n"
        "        mock(takesMap, (m: Map<str, bool>) => len(m))\n"
        "        takesMap({\"a\": true, \"b\": false})\n"
        "        expect(verifyCalledWith(\"takesMap\", {\"a\": true, \"b\": false})).toEq(1)\n"
    )), "vcw map str bool\n  \033[32m+ counts map arg\033[0m\n\n1 passed, 0 failed\n");
}

TEST_F(CodeGenTest, VerifyCalledWithMapParameterRejectedWithPrimitiveArg) {
    // Map<str, int> parameter must reject a primitive (str) argument.
    // Both lower to ptrTy_; the explicit map-vs-scalar consistency check
    // must fire (#1705).
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "fn takesMap(m: Map<str, int>) -> int:\n"
        "    return len(m)\n"
        "\n"
        "@describe(\"vcw map param scalar arg\")\n"
        "fn vcwMapParamScalarArg():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(takesMap, (m: Map<str, int>) => len(m))\n"
        "        takesMap({\"a\": 1})\n"
        "        expect(verifyCalledWith(\"takesMap\", \"x\")).toEq(0)\n"
    )), std::exception);
}

TEST_F(CodeGenTest, VerifyCalledWithScalarParameterRejectedWithMapArg) {
    // A str parameter must reject a Map<str, int> argument. Both lower to
    // ptrTy_; the map-vs-scalar consistency check must fire (#1705).
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "fn takesStr(s: str) -> int:\n"
        "    return len(s)\n"
        "\n"
        "@describe(\"vcw scalar param map arg\")\n"
        "fn vcwScalarParamMapArg():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(takesStr, (s: str) => len(s))\n"
        "        takesStr(\"hi\")\n"
        "        expect(verifyCalledWith(\"takesStr\", {\"a\": 1})).toEq(0)\n"
    )), std::exception);
}

TEST_F(CodeGenTest, VerifyCalledWithMapParamRejectedWithDifferentKeyType) {
    // Map<str, int> parameter must reject a Map<int, int> argument
    // (key type mismatch). Element-type comparison must fire (#1705).
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "fn takesStrIntMap(m: Map<str, int>) -> int:\n"
        "    return len(m)\n"
        "\n"
        "@describe(\"vcw map key mismatch\")\n"
        "fn vcwMapKeyMismatch():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(takesStrIntMap, (m: Map<str, int>) => len(m))\n"
        "        takesStrIntMap({\"a\": 1})\n"
        "        expect(verifyCalledWith(\"takesStrIntMap\", {1: 1})).toEq(0)\n"
    )), std::exception);
}

TEST_F(CodeGenTest, VerifyCalledWithMapParamRejectedWithDifferentValueType) {
    // Map<str, int> parameter must reject a Map<str, str> argument
    // (value type mismatch). Element-type comparison must fire (#1705).
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "fn takesStrIntMap(m: Map<str, int>) -> int:\n"
        "    return len(m)\n"
        "\n"
        "@describe(\"vcw map val mismatch\")\n"
        "fn vcwMapValMismatch():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(takesStrIntMap, (m: Map<str, int>) => len(m))\n"
        "        takesStrIntMap({\"a\": 1})\n"
        "        expect(verifyCalledWith(\"takesStrIntMap\", {\"a\": \"x\"})).toEq(0)\n"
    )), std::exception);
}

TEST_F(CodeGenTest, VerifyCalledWithMapParamRejectedWithListArg) {
    // Map<str, int> parameter must reject a List<int> argument.
    // Even though both lower to ptrTy_, kind (Map vs List) must match (#1705).
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "fn takesMap(m: Map<str, int>) -> int:\n"
        "    return len(m)\n"
        "\n"
        "@describe(\"vcw map param list arg\")\n"
        "fn vcwMapParamListArg():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(takesMap, (m: Map<str, int>) => len(m))\n"
        "        takesMap({\"a\": 1})\n"
        "        expect(verifyCalledWith(\"takesMap\", [1, 2])).toEq(0)\n"
    )), std::exception);
}

TEST_F(CodeGenTest, VerifyCalledWithMapParamRejectedWithSetArg) {
    // Map<str, int> parameter must reject a Set<int> argument.
    // Kind (Map vs Set) must match (#1705).
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "fn takesMap(m: Map<str, int>) -> int:\n"
        "    return len(m)\n"
        "\n"
        "@describe(\"vcw map param set arg\")\n"
        "fn vcwMapParamSetArg():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(takesMap, (m: Map<str, int>) => len(m))\n"
        "        takesMap({\"a\": 1})\n"
        "        expect(verifyCalledWith(\"takesMap\", {1, 2})).toEq(0)\n"
    )), std::exception);
}

TEST_F(CodeGenTest, VerifyCalledWithListParamRejectedWithMapArg) {
    // List<int> parameter must reject a Map<str, int> argument.
    // Kind (List vs Map) must match (#1705).
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "fn takesIntList(xs: List<int>) -> int:\n"
        "    return len(xs)\n"
        "\n"
        "@describe(\"vcw list param map arg\")\n"
        "fn vcwListParamMapArg():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(takesIntList, (xs: List<int>) => len(xs))\n"
        "        takesIntList([1, 2])\n"
        "        expect(verifyCalledWith(\"takesIntList\", {\"a\": 1})).toEq(0)\n"
    )), std::exception);
}

TEST_F(CodeGenTest, VerifyCalledWithSetParamRejectedWithMapArg) {
    // Set<int> parameter must reject a Map<str, int> argument.
    // Kind (Set vs Map) must match (#1705).
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "fn takesIntSet(xs: Set<int>) -> int:\n"
        "    return len(xs)\n"
        "\n"
        "@describe(\"vcw set param map arg\")\n"
        "fn vcwSetParamMapArg():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(takesIntSet, (xs: Set<int>) => len(xs))\n"
        "        takesIntSet({1, 2})\n"
        "        expect(verifyCalledWith(\"takesIntSet\", {\"a\": 1})).toEq(0)\n"
    )), std::exception);
}

// =============================================================================
// verifyCalledWith(): record argument support (#1706)
// =============================================================================

TEST_F(CodeGenTest, VerifyCalledWithRecordArgAccepted) {
    // Record argument with primitive fields: kind=9 path with field-by-field compare.
    EXPECT_EQ(runTestSource(withStdlibDirectiveDecls(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "\n"
        "fn takesPoint(p: Point) -> int:\n"
        "    return p.x\n"
        "\n"
        "@describe(\"vcw record\")\n"
        "fn vcwRecord():\n"
        "    @it(\"counts record arg\")\n"
        "    fn countsRecordArg():\n"
        "        mock(takesPoint, (p: Point) => p.x)\n"
        "        takesPoint(Point(1, 2))\n"
        "        expect(verifyCalledWith(\"takesPoint\", Point(1, 2))).toEq(1)\n"
    )), "vcw record\n  \033[32m+ counts record arg\033[0m\n\n1 passed, 0 failed\n");
}

TEST_F(CodeGenTest, VerifyCalledWithRecordParamDifferentRecordTypeRejected) {
    // Two structurally identical records (Point, Vec): both (int, int).
    // verifyCalledWith must reject cross-record-name calls at compile time
    // because the parameter is declared `Point` but the argument is `Vec`.
    // Stage 2 of emitVerifyCalledWithCall enforces declared-type identity.
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "\n"
        "record Vec:\n"
        "    x: int\n"
        "    y: int\n"
        "\n"
        "fn takesPoint(p: Point) -> int:\n"
        "    return p.x\n"
        "\n"
        "@describe(\"vcw record cross\")\n"
        "fn vcwRecordCross():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(takesPoint, (p: Point) => p.x)\n"
        "        takesPoint(Point(1, 2))\n"
        "        expect(verifyCalledWith(\"takesPoint\", Vec(1, 2))).toEq(0)\n"
    )), std::exception);
}

TEST_F(CodeGenTest, VerifyCalledWithRecordWithUnsupportedFieldTypeRejected) {
    // A record whose fields include a non-primitive/non-str type (List<int>)
    // must be rejected at compile time by Stage 1 (param-side validation).
    // Initial #1706 supports only int/float/bool/str fields; nested collections
    // are out of scope and tracked separately.
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "record Bag:\n"
        "    items: List<int>\n"
        "\n"
        "fn takesBag(b: Bag) -> int:\n"
        "    return len(b.items)\n"
        "\n"
        "@describe(\"vcw record bad field\")\n"
        "fn vcwRecordBadField():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(takesBag, (b: Bag) => len(b.items))\n"
        "        takesBag(Bag([1, 2]))\n"
        "        expect(verifyCalledWith(\"takesBag\", Bag([1, 2]))).toEq(0)\n"
    )), std::exception);
}

TEST_F(CodeGenTest, VerifyCalledWithRecordParamRejectedWithTupleArg) {
    // Record parameter must reject a tuple argument even if shape coincides.
    // Stage 2 cross-shape compile error.
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "\n"
        "fn takesPoint(p: Point) -> int:\n"
        "    return p.x\n"
        "\n"
        "@describe(\"vcw record tuple\")\n"
        "fn vcwRecordTuple():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(takesPoint, (p: Point) => p.x)\n"
        "        takesPoint(Point(1, 2))\n"
        "        expect(verifyCalledWith(\"takesPoint\", (1, 2))).toEq(0)\n"
    )), std::exception);
}

// =============================================================================
// verifyCalledWith(): tuple argument support (#1706)
// =============================================================================

TEST_F(CodeGenTest, VerifyCalledWithTupleArgAccepted) {
    // Tuple argument with primitive elements: kind=10 path with arity+kinds compare.
    EXPECT_EQ(runTestSource(withStdlibDirectiveDecls(
        "fn takesPair(p: (int, int)) -> int:\n"
        "    return 0\n"
        "\n"
        "@describe(\"vcw tuple\")\n"
        "fn vcwTuple():\n"
        "    @it(\"counts tuple arg\")\n"
        "    fn countsTupleArg():\n"
        "        mock(takesPair, (p: (int, int)) => 0)\n"
        "        takesPair((1, 2))\n"
        "        expect(verifyCalledWith(\"takesPair\", (1, 2))).toEq(1)\n"
    )), "vcw tuple\n  \033[32m+ counts tuple arg\033[0m\n\n1 passed, 0 failed\n");
}

TEST_F(CodeGenTest, VerifyCalledWithTupleArityMismatchRejected) {
    // Tuple parameter with arity=2 must reject a tuple argument with arity=3.
    // Stage 2 of emitVerifyCalledWithCall checks that the arg's tuple shape
    // matches the parameter's declared tuple sig.
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "fn takesPair(p: (int, int)) -> int:\n"
        "    return 0\n"
        "\n"
        "@describe(\"vcw tuple arity\")\n"
        "fn vcwTupleArity():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(takesPair, (p: (int, int)) => 0)\n"
        "        takesPair((1, 2))\n"
        "        expect(verifyCalledWith(\"takesPair\", (1, 2, 3))).toEq(0)\n"
    )), std::exception);
}

TEST_F(CodeGenTest, VerifyCalledWithTupleWithUnsupportedElementTypeRejected) {
    // A tuple parameter whose element types include a non-primitive/non-str
    // (List<int>) must be rejected at compile time by Stage 1.
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "fn takesPair(p: (int, List<int>)) -> int:\n"
        "    return 0\n"
        "\n"
        "@describe(\"vcw tuple bad elem\")\n"
        "fn vcwTupleBadElem():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(takesPair, (p: (int, List<int>)) => 0)\n"
        "        takesPair((1, [2]))\n"
        "        expect(verifyCalledWith(\"takesPair\", (1, [2]))).toEq(0)\n"
    )), std::exception);
}

TEST_F(CodeGenTest, VerifyCalledWithTupleElementTypeMismatchRejected) {
    // #1706 Stage 2 per-element check: a tuple parameter with element types
    // (int, str) must reject a tuple argument whose second element has a
    // different type (here List<int>) even though both ptr-backed types
    // collapse to ptr at the LLVM struct level. The check reads the
    // source_type_name metadata stamped on the tuple literal and rejects
    // before Stage 3 builds the snapshot.
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "fn takesIntStrPair(p: (int, str)) -> int:\n"
        "    return 0\n"
        "\n"
        "@describe(\"vcw tuple elem mismatch\")\n"
        "fn vcwTupleElemMismatch():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(takesIntStrPair, (p: (int, str)) => 0)\n"
        "        takesIntStrPair((1, \"a\"))\n"
        "        expect(verifyCalledWith(\"takesIntStrPair\", (1, [2]))).toEq(0)\n"
    )), std::exception);
}

TEST_F(CodeGenTest, VerifyCalledWithTupleParamRejectedWithRecordArg) {
    // Tuple parameter must reject a record argument even if shape coincides.
    // Stage 2 cross-shape compile error.
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "record Point:\n"
        "    x: int\n"
        "    y: int\n"
        "\n"
        "fn takesPair(p: (int, int)) -> int:\n"
        "    return 0\n"
        "\n"
        "@describe(\"vcw tuple record\")\n"
        "fn vcwTupleRecord():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(takesPair, (p: (int, int)) => 0)\n"
        "        takesPair((1, 2))\n"
        "        expect(verifyCalledWith(\"takesPair\", Point(1, 2))).toEq(0)\n"
    )), std::exception);
}

TEST_F(CodeGenTest, VerifyCalledWithEmptyArgsError) {
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "@describe(\"vcw empty\")\n"
        "fn vcwEmpty():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        expect(verifyCalledWith()).toEq(0)\n"
    )), std::exception);
}

TEST_F(CodeGenTest, VerifyCalledWithFirstArgNotStringLiteralError) {
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "fn compute(x: int) -> int:\n"
        "    return x\n"
        "\n"
        "@describe(\"vcw first\")\n"
        "fn vcwFirst():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(compute, (x: int) => x)\n"
        "        compute(5)\n"
        "        name = \"compute\"\n"
        "        expect(verifyCalledWith(name, 5)).toEq(0)\n"
    )), std::exception);
}

TEST_F(CodeGenTest, VerifyCalledWithOutsideTestModeError) {
    // verifyCalledWith requires test mode. Without `from testing import`, the
    // call resolves through normal name lookup and fails as undefined.
    EXPECT_THROW(runSource(
        "fn compute(x: int) -> int:\n"
        "    return x\n"
        "verifyCalledWith(\"compute\", 5)\n"
    ), std::exception);
}
