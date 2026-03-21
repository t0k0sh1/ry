#include "test_codegen_common.hpp"

// ============================================================
// Basic mock: function return value is replaced
// ============================================================

TEST_F(CodeGenTest, MockBasicReplace) {
    EXPECT_EQ(runTestSource(
        "fn greet() -> str:\n"
        "    return \"hello\"\n"
        "\n"
        "describe(\"mock basic\", fn():\n"
        "    it(\"replaces function\", fn():\n"
        "        mock(greet, fn(): \"mocked\")\n"
        "        expect(greet()).to_eq(\"mocked\")\n"
        "    )\n"
        ")\n"
    ), "mock basic\n  \033[32m+ replaces function\033[0m\n\n1 passed, 0 failed\n");
}

// ============================================================
// Mock with arguments
// ============================================================

TEST_F(CodeGenTest, MockWithArgs) {
    EXPECT_EQ(runTestSource(
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "\n"
        "describe(\"mock with args\", fn():\n"
        "    it(\"replaces function with args\", fn():\n"
        "        mock(add, fn(a: int, b: int): a * b)\n"
        "        expect(add(3, 4)).to_eq(12)\n"
        "    )\n"
        ")\n"
    ), "mock with args\n  \033[32m+ replaces function with args\033[0m\n\n1 passed, 0 failed\n");
}

// ============================================================
// Auto-restore between it blocks
// ============================================================

TEST_F(CodeGenTest, MockAutoRestore) {
    EXPECT_EQ(runTestSource(
        "fn greet() -> str:\n"
        "    return \"hello\"\n"
        "\n"
        "describe(\"mock restore\", fn():\n"
        "    it(\"mocks\", fn():\n"
        "        mock(greet, fn(): \"mocked\")\n"
        "        expect(greet()).to_eq(\"mocked\")\n"
        "    )\n"
        "    it(\"auto-restores\", fn():\n"
        "        expect(greet()).to_eq(\"hello\")\n"
        "    )\n"
        ")\n"
    ), "mock restore\n  \033[32m+ mocks\033[0m\n  \033[32m+ auto-restores\033[0m\n\n2 passed, 0 failed\n");
}

// ============================================================
// verify() returns call count
// ============================================================

TEST_F(CodeGenTest, MockVerifyCallCount) {
    EXPECT_EQ(runTestSource(
        "fn greet() -> str:\n"
        "    return \"hello\"\n"
        "\n"
        "describe(\"verify\", fn():\n"
        "    it(\"counts calls\", fn():\n"
        "        mock(greet, fn(): \"mocked\")\n"
        "        greet()\n"
        "        greet()\n"
        "        greet()\n"
        "        expect(verify(greet)).to_eq(3)\n"
        "    )\n"
        ")\n"
    ), "verify\n  \033[32m+ counts calls\033[0m\n\n1 passed, 0 failed\n");
}

// ============================================================
// Mock function used as expression / return value
// ============================================================

TEST_F(CodeGenTest, MockFunctionUsedAsExpr) {
    // verify() works when mock is called and function result is used
    EXPECT_EQ(runTestSource(
        "fn get_value() -> int:\n"
        "    return 100\n"
        "\n"
        "describe(\"mock expr\", fn():\n"
        "    it(\"tracks calls in expressions\", fn():\n"
        "        mock(get_value, fn(): 999)\n"
        "        let x = get_value()\n"
        "        expect(x).to_eq(999)\n"
        "        expect(verify(get_value)).to_eq(1)\n"
        "    )\n"
        ")\n"
    ), "mock expr\n  \033[32m+ tracks calls in expressions\033[0m\n\n1 passed, 0 failed\n");
}

// ============================================================
// mock() outside test mode -> compile error
// ============================================================

TEST_F(CodeGenTest, MockOutsideTestModeError) {
    EXPECT_THROW(runSource(
        "fn greet() -> str:\n"
        "    return \"hello\"\n"
        "mock(greet, fn(): \"mocked\")\n"
    ), std::exception);
}

// ============================================================
// mock() on non-existent function -> compile error
// ============================================================

TEST_F(CodeGenTest, MockNonExistentFunctionError) {
    EXPECT_THROW(runTestSource(
        "describe(\"error\", fn():\n"
        "    it(\"errors\", fn():\n"
        "        mock(no_such_fn, fn(): \"x\")\n"
        "    )\n"
        ")\n"
    ), std::exception);
}

// ============================================================
// mock() with type mismatch -> compile error
// ============================================================

TEST_F(CodeGenTest, MockTypeMismatchError) {
    EXPECT_THROW(runTestSource(
        "fn greet() -> str:\n"
        "    return \"hello\"\n"
        "\n"
        "describe(\"error\", fn():\n"
        "    it(\"errors\", fn():\n"
        "        mock(greet, fn(): 42)\n"
        "    )\n"
        ")\n"
    ), std::exception);
}
