#include "test_codegen_common.hpp"

// ===== require (precondition) tests =====

TEST_F(CodeGenTest, RequireSatisfied) {
    std::string src =
        "fn deposit(amount: int) -> int:\n"
        "    require:\n"
        "        amount > 0\n"
        "    return amount\n"
        "print(deposit(100))";
    EXPECT_EQ(runSource(src), "100\n");
}

TEST_F(CodeGenTest, RequireViolation) {
    std::string src =
        "fn deposit(amount: int) -> int:\n"
        "    require:\n"
        "        amount > 0\n"
        "    return amount\n"
        "print(deposit(-1))";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

TEST_F(CodeGenTest, RequireMultipleConditions) {
    std::string src =
        "fn add(a: int, b: int) -> int:\n"
        "    require:\n"
        "        a >= 0\n"
        "        b >= 0\n"
        "    return a + b\n"
        "print(add(3, 4))";
    EXPECT_EQ(runSource(src), "7\n");
}

TEST_F(CodeGenTest, RequireMultipleConditionsSecondFails) {
    std::string src =
        "fn add(a: int, b: int) -> int:\n"
        "    require:\n"
        "        a >= 0\n"
        "        b >= 0\n"
        "    return a + b\n"
        "print(add(3, -1))";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

// ===== ensure (postcondition) + result tests =====

TEST_F(CodeGenTest, EnsureSatisfied) {
    std::string src =
        "fn abs(x: int) -> int:\n"
        "    ensure:\n"
        "        result >= 0\n"
        "    if x < 0:\n"
        "        return -x\n"
        "    return x\n"
        "print(abs(-5))";
    EXPECT_EQ(runSource(src), "5\n");
}

TEST_F(CodeGenTest, EnsureViolation) {
    std::string src =
        "fn bad() -> int:\n"
        "    ensure:\n"
        "        result > 0\n"
        "    return -1\n"
        "print(bad())";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

TEST_F(CodeGenTest, EnsureWithOld) {
    std::string src =
        "fn inc(x: int) -> int:\n"
        "    ensure:\n"
        "        result == old(x) + 1\n"
        "    return x + 1\n"
        "print(inc(5))";
    EXPECT_EQ(runSource(src), "6\n");
}

TEST_F(CodeGenTest, EnsureWithOldViolation) {
    std::string src =
        "fn inc(x: int) -> int:\n"
        "    ensure:\n"
        "        result == old(x) + 1\n"
        "    return x + 2\n"
        "print(inc(5))";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

TEST_F(CodeGenTest, RequireAndEnsureCombined) {
    std::string src =
        "fn deposit(amount: int, balance: int) -> int:\n"
        "    require:\n"
        "        amount > 0\n"
        "        balance >= 0\n"
        "    ensure:\n"
        "        result >= 0\n"
        "        result == old(balance) + amount\n"
        "    new_balance: int = balance + amount\n"
        "    return new_balance\n"
        "print(deposit(100, 500))";
    EXPECT_EQ(runSource(src), "600\n");
}

// ===== invariant tests =====

TEST_F(CodeGenTest, InvariantSatisfiedOnConstruction) {
    std::string src =
        "record Account:\n"
        "    balance: int\n"
        "    min_balance: int\n"
        "    invariant:\n"
        "        balance >= min_balance\n"
        "a = Account(100, 0)\n"
        "print(a.balance)";
    EXPECT_EQ(runSource(src), "100\n");
}

TEST_F(CodeGenTest, InvariantViolatedOnConstruction) {
    std::string src =
        "record Account:\n"
        "    balance: int\n"
        "    min_balance: int\n"
        "    invariant:\n"
        "        balance >= min_balance\n"
        "a = Account(-1, 0)\n"
        "print(a.balance)";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

TEST_F(CodeGenTest, InvariantSatisfiedAfterFieldAssign) {
    std::string src =
        "record Account:\n"
        "    balance: int\n"
        "    min_balance: int\n"
        "    invariant:\n"
        "        balance >= min_balance\n"
        "a = Account(100, 0)\n"
        "a.balance = 50\n"
        "print(a.balance)";
    EXPECT_EQ(runSource(src), "50\n");
}

TEST_F(CodeGenTest, InvariantViolatedAfterFieldAssign) {
    std::string src =
        "record Account:\n"
        "    balance: int\n"
        "    min_balance: int\n"
        "    invariant:\n"
        "        balance >= min_balance\n"
        "a = Account(100, 0)\n"
        "a.balance = -1\n"
        "print(a.balance)";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}

// ===== Nested function with contract: FnScope protects contract state =====

TEST_F(CodeGenTest, NestedFnWithEnsurePreservesOuterContract) {
    std::string src =
        "fn outer(x: int) -> int:\n"
        "    ensure:\n"
        "        result >= 0\n"
        "    fn inner(y: int) -> int:\n"
        "        ensure:\n"
        "            result > 0\n"
        "        return y + 1\n"
        "    return inner(x)\n"
        "print(outer(5))";
    EXPECT_EQ(runSource(src), "6\n");
}

TEST_F(CodeGenTest, NestedFnWithEnsureViolation) {
    std::string src =
        "fn outer(x: int) -> int:\n"
        "    ensure:\n"
        "        result >= 0\n"
        "    fn inner(y: int) -> int:\n"
        "        ensure:\n"
        "            result > 0\n"
        "        return y + 1\n"
        "    return inner(x)\n"
        "print(outer(-1))";
    EXPECT_EXIT(runSource(src), ::testing::ExitedWithCode(1), "");
}
