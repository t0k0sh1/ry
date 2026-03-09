#include <gtest/gtest.h>
#include "ry/parser.hpp"

static Program parseStr(const std::string &src) {
    Lexer lex(src);
    Parser parser(lex);
    return parser.parseProgram();
}

TEST(ParserTest, LetSimpleInt) {
    Program prog = parseStr("let x = 42");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<LetStmt>(prog[0]));
    const auto &s = std::get<LetStmt>(prog[0]);
    EXPECT_EQ(s.name, "x");
    EXPECT_FALSE(s.type_annotation.has_value());
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(s.value->data));
    EXPECT_EQ(std::get<NumberExpr>(s.value->data).value, 42);
}

TEST(ParserTest, ConstSimpleInt) {
    Program prog = parseStr("const x = 42");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<ConstStmt>(prog[0]));
    const auto &s = std::get<ConstStmt>(prog[0]);
    EXPECT_EQ(s.name, "x");
    EXPECT_FALSE(s.type_annotation.has_value());
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(s.value->data));
    EXPECT_EQ(std::get<NumberExpr>(s.value->data).value, 42);
}

TEST(ParserTest, LetWithTypeAnnotation) {
    Program prog = parseStr("let x: int = 42");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<LetStmt>(prog[0]);
    EXPECT_EQ(s.name, "x");
    ASSERT_TRUE(s.type_annotation.has_value());
    EXPECT_EQ(*s.type_annotation, "int");
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(s.value->data));
    EXPECT_EQ(std::get<NumberExpr>(s.value->data).value, 42);
}

TEST(ParserTest, ConstWithTypeAnnotation) {
    Program prog = parseStr("const y: float = 3.14");
    const auto &s = std::get<ConstStmt>(prog[0]);
    EXPECT_EQ(s.name, "y");
    ASSERT_TRUE(s.type_annotation.has_value());
    EXPECT_EQ(*s.type_annotation, "float");
    ASSERT_TRUE(std::holds_alternative<FloatExpr>(s.value->data));
}

TEST(ParserTest, LetFloat) {
    Program prog = parseStr("let x = 3.14");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<LetStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<FloatExpr>(s.value->data));
    EXPECT_DOUBLE_EQ(std::get<FloatExpr>(s.value->data).value, 3.14);
}

TEST(ParserTest, AssignStmt) {
    Program prog = parseStr("let x = 1\nx = 2");
    ASSERT_EQ(prog.size(), 2u);
    ASSERT_TRUE(std::holds_alternative<LetStmt>(prog[0]));
    ASSERT_TRUE(std::holds_alternative<AssignStmt>(prog[1]));
    const auto &assign = std::get<AssignStmt>(prog[1]);
    EXPECT_EQ(assign.name, "x");
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(assign.value->data));
    EXPECT_EQ(std::get<NumberExpr>(assign.value->data).value, 2);
}

TEST(ParserTest, VariableRHS) {
    Program prog = parseStr("let y = x");
    const auto &s = std::get<LetStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<VariableExpr>(s.value->data));
    EXPECT_EQ(std::get<VariableExpr>(s.value->data).name, "x");
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
    // "let x = 1 + 2 * 3" → +(1, *(2, 3))
    Program prog = parseStr("let x = 1 + 2 * 3");
    const auto &s = std::get<LetStmt>(prog[0]);
    const auto &outer = std::get<std::unique_ptr<BinaryExpr>>(s.value->data);
    EXPECT_EQ(outer->op, "+");
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(outer->lhs->data));
    EXPECT_EQ(std::get<NumberExpr>(outer->lhs->data).value, 1);
    const auto &rhs = std::get<std::unique_ptr<BinaryExpr>>(outer->rhs->data);
    EXPECT_EQ(rhs->op, "*");
    EXPECT_EQ(std::get<NumberExpr>(rhs->lhs->data).value, 2);
    EXPECT_EQ(std::get<NumberExpr>(rhs->rhs->data).value, 3);
}

TEST(ParserTest, PowerRightAssociative) {
    // "let x = 2 ** 3 ** 2" → **(2, **(3, 2))
    Program prog = parseStr("let x = 2 ** 3 ** 2");
    const auto &s = std::get<LetStmt>(prog[0]);
    const auto &outer = std::get<std::unique_ptr<BinaryExpr>>(s.value->data);
    EXPECT_EQ(outer->op, "**");
    EXPECT_EQ(std::get<NumberExpr>(outer->lhs->data).value, 2);
    const auto &rhs = std::get<std::unique_ptr<BinaryExpr>>(outer->rhs->data);
    EXPECT_EQ(rhs->op, "**");
    EXPECT_EQ(std::get<NumberExpr>(rhs->lhs->data).value, 3);
    EXPECT_EQ(std::get<NumberExpr>(rhs->rhs->data).value, 2);
}

TEST(ParserTest, NotRightAssociative) {
    // "let x = not not true" → not(not(true))
    Program prog = parseStr("let x = not not true");
    const auto &s = std::get<LetStmt>(prog[0]);
    const auto &outer = std::get<std::unique_ptr<UnaryExpr>>(s.value->data);
    EXPECT_EQ(outer->op, "not");
    const auto &inner = std::get<std::unique_ptr<UnaryExpr>>(outer->operand->data);
    EXPECT_EQ(inner->op, "not");
    EXPECT_TRUE(std::holds_alternative<BoolExpr>(inner->operand->data));
    EXPECT_TRUE(std::get<BoolExpr>(inner->operand->data).value);
}

TEST(ParserTest, BoolTrueAssign) {
    Program prog = parseStr("let x = true");
    const auto &s = std::get<LetStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<BoolExpr>(s.value->data));
    EXPECT_TRUE(std::get<BoolExpr>(s.value->data).value);
}

TEST(ParserTest, BoolFalseAssign) {
    Program prog = parseStr("let x = false");
    const auto &s = std::get<LetStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<BoolExpr>(s.value->data));
    EXPECT_FALSE(std::get<BoolExpr>(s.value->data).value);
}

TEST(ParserTest, ComparisonOverAdd) {
    // "let x = 1 + 2 == 3" → ==(+(1,2), 3)
    Program prog = parseStr("let x = 1 + 2 == 3");
    const auto &s = std::get<LetStmt>(prog[0]);
    const auto &outer = std::get<std::unique_ptr<BinaryExpr>>(s.value->data);
    EXPECT_EQ(outer->op, "==");
    const auto &lhs = std::get<std::unique_ptr<BinaryExpr>>(outer->lhs->data);
    EXPECT_EQ(lhs->op, "+");
    EXPECT_EQ(std::get<NumberExpr>(outer->rhs->data).value, 3);
}

TEST(ParserTest, LogicalPrecedence) {
    // "let x = a or b and not c" → or(a, and(b, not(c)))
    Program prog = parseStr("let x = a or b and not c");
    const auto &s = std::get<LetStmt>(prog[0]);
    const auto &outer = std::get<std::unique_ptr<BinaryExpr>>(s.value->data);
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
    Program prog = parseStr("let x = -42");
    const auto &s = std::get<LetStmt>(prog[0]);
    const auto &unary = std::get<std::unique_ptr<UnaryExpr>>(s.value->data);
    EXPECT_EQ(unary->op, "-");
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(unary->operand->data));
    EXPECT_EQ(std::get<NumberExpr>(unary->operand->data).value, 42);
}

TEST(ParserTest, UnaryPlus) {
    Program prog = parseStr("let x = +5");
    const auto &s = std::get<LetStmt>(prog[0]);
    const auto &unary = std::get<std::unique_ptr<UnaryExpr>>(s.value->data);
    EXPECT_EQ(unary->op, "+");
    EXPECT_EQ(std::get<NumberExpr>(unary->operand->data).value, 5);
}

TEST(ParserTest, Parentheses) {
    // "let x = (1 + 2) * 3" → *(+(1,2), 3)
    Program prog = parseStr("let x = (1 + 2) * 3");
    const auto &s = std::get<LetStmt>(prog[0]);
    const auto &outer = std::get<std::unique_ptr<BinaryExpr>>(s.value->data);
    EXPECT_EQ(outer->op, "*");
    const auto &lhs = std::get<std::unique_ptr<BinaryExpr>>(outer->lhs->data);
    EXPECT_EQ(lhs->op, "+");
    EXPECT_EQ(std::get<NumberExpr>(outer->rhs->data).value, 3);
}

TEST(ParserTest, MultipleStatements) {
    Program prog = parseStr("let x = 1\nlet y = 2");
    ASSERT_EQ(prog.size(), 2u);
    EXPECT_EQ(std::get<LetStmt>(prog[0]).name, "x");
    EXPECT_EQ(std::get<LetStmt>(prog[1]).name, "y");
}

TEST(ParserTest, InvalidSyntaxThrows) {
    // 数値から始まる文はエラー
    EXPECT_THROW(parseStr("42 = x"), std::runtime_error);
}

TEST(ParserTest, TypeAnnotationInt) {
    Program prog = parseStr("let x: int = 42");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<LetStmt>(prog[0]);
    EXPECT_EQ(s.name, "x");
    ASSERT_TRUE(s.type_annotation.has_value());
    EXPECT_EQ(*s.type_annotation, "int");
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(s.value->data));
    EXPECT_EQ(std::get<NumberExpr>(s.value->data).value, 42);
}

TEST(ParserTest, TypeAnnotationFloat) {
    Program prog = parseStr("let y: float = 3.14");
    const auto &s = std::get<LetStmt>(prog[0]);
    EXPECT_EQ(s.name, "y");
    ASSERT_TRUE(s.type_annotation.has_value());
    EXPECT_EQ(*s.type_annotation, "float");
    ASSERT_TRUE(std::holds_alternative<FloatExpr>(s.value->data));
}

TEST(ParserTest, TypeAnnotationBool) {
    Program prog = parseStr("let z: bool = true");
    const auto &s = std::get<LetStmt>(prog[0]);
    EXPECT_EQ(s.name, "z");
    ASSERT_TRUE(s.type_annotation.has_value());
    EXPECT_EQ(*s.type_annotation, "bool");
    ASSERT_TRUE(std::holds_alternative<BoolExpr>(s.value->data));
}

TEST(ParserTest, TypeAnnotationUnknownTypeThrows) {
    EXPECT_THROW(parseStr("let x: string = 42"), std::runtime_error);
}

TEST(ParserTest, TypeAnnotationMissingEqualsThrows) {
    EXPECT_THROW(parseStr("let x: int 42"), std::runtime_error);
}

TEST(ParserTest, TypeAnnotationMixedWithInference) {
    Program prog = parseStr("let a: int = 1\nlet b = 2");
    ASSERT_EQ(prog.size(), 2u);
    EXPECT_TRUE(std::get<LetStmt>(prog[0]).type_annotation.has_value());
    EXPECT_FALSE(std::get<LetStmt>(prog[1]).type_annotation.has_value());
}

TEST(ParserTest, BareAssignmentWithoutDeclaration) {
    // x = 10 はパーサーでは許容（codegen でエラー）
    Program prog = parseStr("x = 10");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<AssignStmt>(prog[0]));
    EXPECT_EQ(std::get<AssignStmt>(prog[0]).name, "x");
}

TEST(ParserTest, TypeAnnotationWithoutLetOrConstThrows) {
    // x: int = 10 はエラー（let/const が必要）
    EXPECT_THROW(parseStr("x: int = 10"), std::runtime_error);
}
