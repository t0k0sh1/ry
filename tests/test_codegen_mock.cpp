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

TEST_F(CodeGenTest, VerifyCalledWithListArgUnsupportedError) {
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "fn takesList(xs: List<int>) -> int:\n"
        "    return len(xs)\n"
        "\n"
        "@describe(\"vcw list\")\n"
        "fn vcwList():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(takesList, (xs: List<int>) => len(xs))\n"
        "        takesList([1, 2])\n"
        "        expect(verifyCalledWith(\"takesList\", [1, 2])).toEq(0)\n"
    )), std::exception);
}

TEST_F(CodeGenTest, VerifyCalledWithMapArgUnsupportedError) {
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "fn takesMap(m: Map<str, int>) -> int:\n"
        "    return len(m)\n"
        "\n"
        "@describe(\"vcw map\")\n"
        "fn vcwMap():\n"
        "    @it(\"errors\")\n"
        "    fn errors():\n"
        "        mock(takesMap, (m: Map<str, int>) => len(m))\n"
        "        takesMap({\"a\": 1})\n"
        "        expect(verifyCalledWith(\"takesMap\", {\"a\": 1})).toEq(0)\n"
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
