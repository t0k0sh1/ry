#include <gtest/gtest.h>
#include "ry/parser.hpp"

static Program parseStr(const std::string &src) {
    Lexer lex(src);
    Parser parser(lex);
    return parser.parseProgram();
}

TEST(ParserTest, SimpleIntAssign) {
    Program prog = parseStr("x = 42");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<AssignStmt>(prog[0]));
    const auto &assign = std::get<AssignStmt>(prog[0]);
    EXPECT_EQ(assign.name, "x");
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(assign.value->data));
    EXPECT_EQ(std::get<NumberExpr>(assign.value->data).value, 42);
}

TEST(ParserTest, SimpleFloatAssign) {
    Program prog = parseStr("x = 3.14");
    ASSERT_EQ(prog.size(), 1u);
    const auto &assign = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<FloatExpr>(assign.value->data));
    EXPECT_DOUBLE_EQ(std::get<FloatExpr>(assign.value->data).value, 3.14);
}

TEST(ParserTest, VariableRHS) {
    Program prog = parseStr("y = x");
    const auto &assign = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<VariableExpr>(assign.value->data));
    EXPECT_EQ(std::get<VariableExpr>(assign.value->data).name, "x");
}

TEST(ParserTest, PrintCall) {
    Program prog = parseStr("print(42)");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<CallStmt>(prog[0]));
    const auto &call = std::get<CallStmt>(prog[0]);
    EXPECT_EQ(call.callee, "print");
    ASSERT_EQ(call.args.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(call.args[0]->data));
    EXPECT_EQ(std::get<NumberExpr>(call.args[0]->data).value, 42);
}

TEST(ParserTest, MulOverAdd) {
    // "x = 1 + 2 * 3" → +(1, *(2, 3))
    Program prog = parseStr("x = 1 + 2 * 3");
    const auto &assign = std::get<AssignStmt>(prog[0]);
    const auto &outer = std::get<std::unique_ptr<BinaryExpr>>(assign.value->data);
    EXPECT_EQ(outer->op, "+");
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(outer->lhs->data));
    EXPECT_EQ(std::get<NumberExpr>(outer->lhs->data).value, 1);
    const auto &rhs = std::get<std::unique_ptr<BinaryExpr>>(outer->rhs->data);
    EXPECT_EQ(rhs->op, "*");
    EXPECT_EQ(std::get<NumberExpr>(rhs->lhs->data).value, 2);
    EXPECT_EQ(std::get<NumberExpr>(rhs->rhs->data).value, 3);
}

TEST(ParserTest, PowerRightAssociative) {
    // "x = 2 ** 3 ** 2" → **(2, **(3, 2))
    Program prog = parseStr("x = 2 ** 3 ** 2");
    const auto &assign = std::get<AssignStmt>(prog[0]);
    const auto &outer = std::get<std::unique_ptr<BinaryExpr>>(assign.value->data);
    EXPECT_EQ(outer->op, "**");
    EXPECT_EQ(std::get<NumberExpr>(outer->lhs->data).value, 2);
    const auto &rhs = std::get<std::unique_ptr<BinaryExpr>>(outer->rhs->data);
    EXPECT_EQ(rhs->op, "**");
    EXPECT_EQ(std::get<NumberExpr>(rhs->lhs->data).value, 3);
    EXPECT_EQ(std::get<NumberExpr>(rhs->rhs->data).value, 2);
}

TEST(ParserTest, NotRightAssociative) {
    // "x = not not true" → not(not(true))
    Program prog = parseStr("x = not not true");
    const auto &assign = std::get<AssignStmt>(prog[0]);
    const auto &outer = std::get<std::unique_ptr<UnaryExpr>>(assign.value->data);
    EXPECT_EQ(outer->op, "not");
    const auto &inner = std::get<std::unique_ptr<UnaryExpr>>(outer->operand->data);
    EXPECT_EQ(inner->op, "not");
    EXPECT_TRUE(std::holds_alternative<BoolExpr>(inner->operand->data));
    EXPECT_TRUE(std::get<BoolExpr>(inner->operand->data).value);
}

TEST(ParserTest, BoolTrueAssign) {
    Program prog = parseStr("x = true");
    const auto &assign = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<BoolExpr>(assign.value->data));
    EXPECT_TRUE(std::get<BoolExpr>(assign.value->data).value);
}

TEST(ParserTest, BoolFalseAssign) {
    Program prog = parseStr("x = false");
    const auto &assign = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<BoolExpr>(assign.value->data));
    EXPECT_FALSE(std::get<BoolExpr>(assign.value->data).value);
}

TEST(ParserTest, ComparisonOverAdd) {
    // "x = 1 + 2 == 3" → ==(+(1,2), 3)
    Program prog = parseStr("x = 1 + 2 == 3");
    const auto &assign = std::get<AssignStmt>(prog[0]);
    const auto &outer = std::get<std::unique_ptr<BinaryExpr>>(assign.value->data);
    EXPECT_EQ(outer->op, "==");
    const auto &lhs = std::get<std::unique_ptr<BinaryExpr>>(outer->lhs->data);
    EXPECT_EQ(lhs->op, "+");
    EXPECT_EQ(std::get<NumberExpr>(outer->rhs->data).value, 3);
}

TEST(ParserTest, LogicalPrecedence) {
    // "x = a or b and not c" → or(a, and(b, not(c)))
    Program prog = parseStr("x = a or b and not c");
    const auto &assign = std::get<AssignStmt>(prog[0]);
    const auto &outer = std::get<std::unique_ptr<BinaryExpr>>(assign.value->data);
    EXPECT_EQ(outer->op, "or");
    EXPECT_EQ(std::get<VariableExpr>(outer->lhs->data).name, "a");
    const auto &and_node = std::get<std::unique_ptr<BinaryExpr>>(outer->rhs->data);
    EXPECT_EQ(and_node->op, "and");
    EXPECT_EQ(std::get<VariableExpr>(and_node->lhs->data).name, "b");
    const auto &not_node = std::get<std::unique_ptr<UnaryExpr>>(and_node->rhs->data);
    EXPECT_EQ(not_node->op, "not");
    EXPECT_EQ(std::get<VariableExpr>(not_node->operand->data).name, "c");
}

TEST(ParserTest, UnaryMinus) {
    Program prog = parseStr("x = -42");
    const auto &assign = std::get<AssignStmt>(prog[0]);
    const auto &unary = std::get<std::unique_ptr<UnaryExpr>>(assign.value->data);
    EXPECT_EQ(unary->op, "-");
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(unary->operand->data));
    EXPECT_EQ(std::get<NumberExpr>(unary->operand->data).value, 42);
}

TEST(ParserTest, UnaryPlus) {
    Program prog = parseStr("x = +5");
    const auto &assign = std::get<AssignStmt>(prog[0]);
    const auto &unary = std::get<std::unique_ptr<UnaryExpr>>(assign.value->data);
    EXPECT_EQ(unary->op, "+");
    EXPECT_EQ(std::get<NumberExpr>(unary->operand->data).value, 5);
}

TEST(ParserTest, Parentheses) {
    // "x = (1 + 2) * 3" → *(+(1,2), 3)
    Program prog = parseStr("x = (1 + 2) * 3");
    const auto &assign = std::get<AssignStmt>(prog[0]);
    const auto &outer = std::get<std::unique_ptr<BinaryExpr>>(assign.value->data);
    EXPECT_EQ(outer->op, "*");
    const auto &lhs = std::get<std::unique_ptr<BinaryExpr>>(outer->lhs->data);
    EXPECT_EQ(lhs->op, "+");
    EXPECT_EQ(std::get<NumberExpr>(outer->rhs->data).value, 3);
}

TEST(ParserTest, MultipleStatements) {
    Program prog = parseStr("x = 1\ny = 2");
    ASSERT_EQ(prog.size(), 2u);
    EXPECT_EQ(std::get<AssignStmt>(prog[0]).name, "x");
    EXPECT_EQ(std::get<AssignStmt>(prog[1]).name, "y");
}

TEST(ParserTest, InvalidSyntaxThrows) {
    // 数値から始まる文はエラー
    EXPECT_THROW(parseStr("42 = x"), std::runtime_error);
}
