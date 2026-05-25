#include <gtest/gtest.h>
#include "ry/parser.hpp"
#include "ry/diagnostic.hpp"


using namespace ry;
static Program parseStr(const std::string &src) {
    Lexer lex(src);
    Parser parser(lex);
    return parser.parseProgram();
}

TEST(ParserTest, LetSimpleInt) {
    Program prog = parseStr("x = 42");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<AssignStmt>(prog[0]));
    const auto &s = std::get<AssignStmt>(prog[0]);
    EXPECT_EQ(s.name, "x");
    EXPECT_EQ(s.type_annotation, nullptr);
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(s.value->data));
    EXPECT_EQ(std::get<NumberExpr>(s.value->data).value, 42);
}

TEST(ParserTest, VarSimpleInt) {
    Program prog = parseStr("x = 42");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<AssignStmt>(prog[0]));
    const auto &s = std::get<AssignStmt>(prog[0]);
    EXPECT_EQ(s.name, "x");
    EXPECT_EQ(s.type_annotation, nullptr);
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(s.value->data));
    EXPECT_EQ(std::get<NumberExpr>(s.value->data).value, 42);
}

TEST(ParserTest, LetWithTypeAnnotation) {
    Program prog = parseStr("x: int = 42");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    EXPECT_EQ(s.name, "x");
    ASSERT_TRUE(s.type_annotation != nullptr);
    EXPECT_EQ(s.type_annotation->toString(),"int");
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(s.value->data));
    EXPECT_EQ(std::get<NumberExpr>(s.value->data).value, 42);
}

TEST(ParserTest, VarWithTypeAnnotation) {
    Program prog = parseStr("y: float = 3.14");
    const auto &s = std::get<AssignStmt>(prog[0]);
    EXPECT_EQ(s.name, "y");
    ASSERT_TRUE(s.type_annotation != nullptr);
    EXPECT_EQ(s.type_annotation->toString(),"float");
    ASSERT_TRUE(std::holds_alternative<FloatExpr>(s.value->data));
}

TEST(ParserTest, LetFloat) {
    Program prog = parseStr("x = 3.14");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<FloatExpr>(s.value->data));
    EXPECT_DOUBLE_EQ(std::get<FloatExpr>(s.value->data).value, 3.14);
}

TEST(ParserTest, LeadingDotFloat) {
    Program prog = parseStr("x = .5");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<FloatExpr>(s.value->data));
    EXPECT_DOUBLE_EQ(std::get<FloatExpr>(s.value->data).value, 0.5);
}

TEST(ParserTest, LeadingDotFloatInExpression) {
    Program prog = parseStr("x = .5 + .25");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<BinaryExpr>>(s.value->data));
}

TEST(ParserTest, LeadingDotFloatRejectsIntSuffix) {
    EXPECT_THROW(parseStr("x = .5i32"), std::runtime_error);
}

TEST(ParserTest, AssignStmt) {
    Program prog = parseStr("x = 1\nx = 2");
    ASSERT_EQ(prog.size(), 2u);
    ASSERT_TRUE(std::holds_alternative<AssignStmt>(prog[0]));
    ASSERT_TRUE(std::holds_alternative<AssignStmt>(prog[1]));
    const auto &assign = std::get<AssignStmt>(prog[1]);
    EXPECT_EQ(assign.name, "x");
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(assign.value->data));
    EXPECT_EQ(std::get<NumberExpr>(assign.value->data).value, 2);
}

TEST(ParserTest, VariableRHS) {
    Program prog = parseStr("y = x");
    const auto &s = std::get<AssignStmt>(prog[0]);
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
    // "x = 1 + 2 * 3" → +(1, *(2, 3))
    Program prog = parseStr("x = 1 + 2 * 3");
    const auto &s = std::get<AssignStmt>(prog[0]);
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
    // "x = 2 ** 3 ** 2" → **(2, **(3, 2))
    Program prog = parseStr("x = 2 ** 3 ** 2");
    const auto &s = std::get<AssignStmt>(prog[0]);
    const auto &outer = std::get<std::unique_ptr<BinaryExpr>>(s.value->data);
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
    const auto &s = std::get<AssignStmt>(prog[0]);
    const auto &outer = std::get<std::unique_ptr<UnaryExpr>>(s.value->data);
    EXPECT_EQ(outer->op, "not");
    const auto &inner = std::get<std::unique_ptr<UnaryExpr>>(outer->operand->data);
    EXPECT_EQ(inner->op, "not");
    EXPECT_TRUE(std::holds_alternative<BoolExpr>(inner->operand->data));
    EXPECT_TRUE(std::get<BoolExpr>(inner->operand->data).value);
}

TEST(ParserTest, BoolTrueAssign) {
    Program prog = parseStr("x = true");
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<BoolExpr>(s.value->data));
    EXPECT_TRUE(std::get<BoolExpr>(s.value->data).value);
}

TEST(ParserTest, BoolFalseAssign) {
    Program prog = parseStr("x = false");
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<BoolExpr>(s.value->data));
    EXPECT_FALSE(std::get<BoolExpr>(s.value->data).value);
}

TEST(ParserTest, ComparisonOverAdd) {
    // "x = 1 + 2 == 3" → ==(+(1,2), 3)
    Program prog = parseStr("x = 1 + 2 == 3");
    const auto &s = std::get<AssignStmt>(prog[0]);
    const auto &outer = std::get<std::unique_ptr<BinaryExpr>>(s.value->data);
    EXPECT_EQ(outer->op, "==");
    const auto &lhs = std::get<std::unique_ptr<BinaryExpr>>(outer->lhs->data);
    EXPECT_EQ(lhs->op, "+");
    EXPECT_EQ(std::get<NumberExpr>(outer->rhs->data).value, 3);
}

TEST(ParserTest, LogicalPrecedence) {
    // "x = a or b and not c" → or(a, and(b, not(c)))
    Program prog = parseStr("x = a or b and not c");
    const auto &s = std::get<AssignStmt>(prog[0]);
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
    Program prog = parseStr("x = -42");
    const auto &s = std::get<AssignStmt>(prog[0]);
    const auto &unary = std::get<std::unique_ptr<UnaryExpr>>(s.value->data);
    EXPECT_EQ(unary->op, "-");
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(unary->operand->data));
    EXPECT_EQ(std::get<NumberExpr>(unary->operand->data).value, 42);
}

TEST(ParserTest, UnaryPlus) {
    Program prog = parseStr("x = +5");
    const auto &s = std::get<AssignStmt>(prog[0]);
    const auto &unary = std::get<std::unique_ptr<UnaryExpr>>(s.value->data);
    EXPECT_EQ(unary->op, "+");
    EXPECT_EQ(std::get<NumberExpr>(unary->operand->data).value, 5);
}

TEST(ParserTest, Parentheses) {
    // "x = (1 + 2) * 3" → *(+(1,2), 3)
    Program prog = parseStr("x = (1 + 2) * 3");
    const auto &s = std::get<AssignStmt>(prog[0]);
    const auto &outer = std::get<std::unique_ptr<BinaryExpr>>(s.value->data);
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

TEST(ParserTest, UnexpectedTokenMessage) {
    try {
        parseStr("; 1");
        FAIL() << "expected exception";
    } catch (const DiagnosticError &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("unexpected token ';'"), std::string::npos);
        // 'expect' should not be listed as a keyword
        EXPECT_EQ(msg.find("'expect'"), std::string::npos);
    }
}

TEST(ParserTest, TypeAnnotationInt) {
    Program prog = parseStr("x: int = 42");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    EXPECT_EQ(s.name, "x");
    ASSERT_TRUE(s.type_annotation != nullptr);
    EXPECT_EQ(s.type_annotation->toString(),"int");
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(s.value->data));
    EXPECT_EQ(std::get<NumberExpr>(s.value->data).value, 42);
}

TEST(ParserTest, TypeAnnotationFloat) {
    Program prog = parseStr("y: float = 3.14");
    const auto &s = std::get<AssignStmt>(prog[0]);
    EXPECT_EQ(s.name, "y");
    ASSERT_TRUE(s.type_annotation != nullptr);
    EXPECT_EQ(s.type_annotation->toString(),"float");
    ASSERT_TRUE(std::holds_alternative<FloatExpr>(s.value->data));
}

TEST(ParserTest, TypeAnnotationBool) {
    Program prog = parseStr("z: bool = true");
    const auto &s = std::get<AssignStmt>(prog[0]);
    EXPECT_EQ(s.name, "z");
    ASSERT_TRUE(s.type_annotation != nullptr);
    EXPECT_EQ(s.type_annotation->toString(),"bool");
    ASSERT_TRUE(std::holds_alternative<BoolExpr>(s.value->data));
}

TEST(ParserTest, TypeAnnotationAcceptsUserDefinedType) {
    Program prog = parseStr("x: Point = p");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(s.type_annotation != nullptr);
    EXPECT_EQ(s.type_annotation->toString(),"Point");
}

// ===== type パーサーテスト =====

TEST(ParserTest, TypeDefinition) {
    Program prog = parseStr("record Point:\n    x: int\n    y: int");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<RecordStmt>(prog[0]));
    const auto &ts = std::get<RecordStmt>(prog[0]);
    EXPECT_EQ(ts.name, "Point");
    ASSERT_EQ(ts.fields.size(), 2u);
    EXPECT_EQ(ts.fields[0].name, "x");
    EXPECT_EQ(ts.fields[0].type->toString(), "int");
    EXPECT_EQ(ts.fields[1].name, "y");
    EXPECT_EQ(ts.fields[1].type->toString(), "int");
}

TEST(ParserTest, RecordSubtyping) {
    Program prog = parseStr("record HttpError < Error:\n    status: int\n    url: str");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<RecordStmt>(prog[0]));
    const auto &ts = std::get<RecordStmt>(prog[0]);
    EXPECT_EQ(ts.name, "HttpError");
    ASSERT_TRUE(ts.parent_name.has_value());
    EXPECT_EQ(*ts.parent_name, "Error");
    ASSERT_EQ(ts.fields.size(), 2u);
    EXPECT_EQ(ts.fields[0].name, "status");
    EXPECT_EQ(ts.fields[0].type->toString(), "int");
    EXPECT_EQ(ts.fields[1].name, "url");
    EXPECT_EQ(ts.fields[1].type->toString(), "str");
}

TEST(ParserTest, RecordWithoutParent) {
    Program prog = parseStr("record Point:\n    x: int\n    y: int");
    ASSERT_EQ(prog.size(), 1u);
    const auto &ts = std::get<RecordStmt>(prog[0]);
    EXPECT_EQ(ts.name, "Point");
    EXPECT_FALSE(ts.parent_name.has_value());
}

TEST(ParserTest, FieldAccessSimple) {
    Program prog = parseStr("x = p.x");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<FieldAccessExpr>>(s.value->data));
    const auto &fa = *std::get<std::unique_ptr<FieldAccessExpr>>(s.value->data);
    EXPECT_EQ(fa.field, "x");
    ASSERT_TRUE(std::holds_alternative<VariableExpr>(fa.object->data));
    EXPECT_EQ(std::get<VariableExpr>(fa.object->data).name, "p");
}

TEST(ParserTest, FieldAccessChained) {
    Program prog = parseStr("x = a.b.c");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    // a.b.c → FieldAccess(FieldAccess(a, b), c)
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<FieldAccessExpr>>(s.value->data));
    const auto &outer = *std::get<std::unique_ptr<FieldAccessExpr>>(s.value->data);
    EXPECT_EQ(outer.field, "c");
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<FieldAccessExpr>>(outer.object->data));
    const auto &inner = *std::get<std::unique_ptr<FieldAccessExpr>>(outer.object->data);
    EXPECT_EQ(inner.field, "b");
    ASSERT_TRUE(std::holds_alternative<VariableExpr>(inner.object->data));
    EXPECT_EQ(std::get<VariableExpr>(inner.object->data).name, "a");
}

TEST(ParserTest, LetStringLiteral) {
    Program prog = parseStr("s = \"hello\"");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    EXPECT_EQ(s.name, "s");
    ASSERT_TRUE(std::holds_alternative<StringExpr>(s.value->data));
    EXPECT_EQ(std::get<StringExpr>(s.value->data).value, "hello");
}

TEST(ParserTest, LetStringWithTypeAnnotation) {
    Program prog = parseStr("s: str = \"world\"");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    EXPECT_EQ(s.name, "s");
    ASSERT_TRUE(s.type_annotation != nullptr);
    EXPECT_EQ(s.type_annotation->toString(),"str");
    ASSERT_TRUE(std::holds_alternative<StringExpr>(s.value->data));
    EXPECT_EQ(std::get<StringExpr>(s.value->data).value, "world");
}

TEST(ParserTest, TypeAnnotationMissingEqualsThrows) {
    EXPECT_THROW(parseStr("x: int 42"), std::runtime_error);
}

TEST(ParserTest, TypeAnnotationMixedWithInference) {
    Program prog = parseStr("a: int = 1\nb = 2");
    ASSERT_EQ(prog.size(), 2u);
    EXPECT_TRUE(std::get<AssignStmt>(prog[0]).type_annotation != nullptr);
    EXPECT_EQ(std::get<AssignStmt>(prog[1]).type_annotation, nullptr);
}

TEST(ParserTest, BareAssignmentWithoutDeclaration) {
    // x = 10 はパーサーでは許容（codegen でエラー）
    Program prog = parseStr("x = 10");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<AssignStmt>(prog[0]));
    EXPECT_EQ(std::get<AssignStmt>(prog[0]).name, "x");
}

TEST(ParserTest, TypeAnnotationWithoutValue) {
    // x: int = 10 is now valid (typed variable declaration)
    Program prog = parseStr("x: int = 10");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    EXPECT_EQ(s.name, "x");
    ASSERT_TRUE(s.type_annotation != nullptr);
    EXPECT_EQ(s.type_annotation->toString(),"int");
}

// ===== if / when パーサーテスト =====

TEST(ParserTest, IfSimple) {
    Program prog = parseStr("if true:\n    print(1)");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<IfStmt>>(prog[0]));
    const auto &ifStmt = *std::get<std::unique_ptr<IfStmt>>(prog[0]);
    EXPECT_TRUE(ifStmt.else_body.empty());
    ASSERT_EQ(ifStmt.branch.body.size(), 1u);
    EXPECT_TRUE(std::holds_alternative<CallStmt>(ifStmt.branch.body[0]));
}

TEST(ParserTest, IfElse) {
    Program prog = parseStr("if true:\n    print(1)\nelse:\n    print(2)");
    ASSERT_EQ(prog.size(), 1u);
    const auto &ifStmt = *std::get<std::unique_ptr<IfStmt>>(prog[0]);
    ASSERT_EQ(ifStmt.else_body.size(), 1u);
}

TEST(ParserTest, IfExpressionColonInlineParsesAsIfBlockExpr) {
    Program prog = parseStr("x = if true: 1 else: 2");
    ASSERT_EQ(prog.size(), 1u);
    const auto &assign = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(assign.value != nullptr);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<IfBlockExpr>>(assign.value->data));
    const auto &ifExpr = *std::get<std::unique_ptr<IfBlockExpr>>(assign.value->data);
    ASSERT_EQ(ifExpr.then_body.size(), 1u);
    ASSERT_EQ(ifExpr.else_body.size(), 1u);
    EXPECT_TRUE(std::holds_alternative<ExprStmt>(ifExpr.then_body[0]));
    EXPECT_TRUE(std::holds_alternative<ExprStmt>(ifExpr.else_body[0]));
}

TEST(ParserTest, IfExpressionColonAllowsInlineThenBlockElse) {
    Program prog = parseStr("x = if true: 1 else:\n    2");
    ASSERT_EQ(prog.size(), 1u);
    const auto &assign = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(assign.value != nullptr);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<IfBlockExpr>>(assign.value->data));
    const auto &ifExpr = *std::get<std::unique_ptr<IfBlockExpr>>(assign.value->data);
    ASSERT_EQ(ifExpr.then_body.size(), 1u);
    ASSERT_EQ(ifExpr.else_body.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<ExprStmt>(ifExpr.then_body[0]));
    ASSERT_TRUE(std::holds_alternative<ExprStmt>(ifExpr.else_body[0]));
    const auto &thenStmt = std::get<ExprStmt>(ifExpr.then_body[0]);
    const auto &elseStmt = std::get<ExprStmt>(ifExpr.else_body[0]);
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(thenStmt.expr->data));
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(elseStmt.expr->data));
    EXPECT_EQ(std::get<NumberExpr>(thenStmt.expr->data).value, 1);
    EXPECT_EQ(std::get<NumberExpr>(elseStmt.expr->data).value, 2);
}

TEST(ParserTest, IfExpressionColonAllowsBlockThenInlineElse) {
    Program prog = parseStr("x = if true:\n    1\nelse: 2");
    ASSERT_EQ(prog.size(), 1u);
    const auto &assign = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(assign.value != nullptr);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<IfBlockExpr>>(assign.value->data));
    const auto &ifExpr = *std::get<std::unique_ptr<IfBlockExpr>>(assign.value->data);
    ASSERT_EQ(ifExpr.then_body.size(), 1u);
    ASSERT_EQ(ifExpr.else_body.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<ExprStmt>(ifExpr.then_body[0]));
    ASSERT_TRUE(std::holds_alternative<ExprStmt>(ifExpr.else_body[0]));
    const auto &thenStmt = std::get<ExprStmt>(ifExpr.then_body[0]);
    const auto &elseStmt = std::get<ExprStmt>(ifExpr.else_body[0]);
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(thenStmt.expr->data));
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(elseStmt.expr->data));
    EXPECT_EQ(std::get<NumberExpr>(thenStmt.expr->data).value, 1);
    EXPECT_EQ(std::get<NumberExpr>(elseStmt.expr->data).value, 2);
}

TEST(ParserTest, IfExpressionColonRejectsMissingThenExpr) {
    EXPECT_THROW(parseStr("x = if true: else: 2"), std::runtime_error);
}

TEST(ParserTest, IfExpressionColonRejectsMissingElseExpr) {
    EXPECT_THROW(parseStr("x = if true: 1 else:"), std::runtime_error);
}

TEST(ParserTest, IfElifRejected) {
    EXPECT_THROW(parseStr("if true:\n    print(1)\nelif false:\n    print(2)\nelse:\n    print(3)"),
                 std::runtime_error);
}

TEST(ParserTest, CaseConditionStatement) {
    Program prog = parseStr("case:\n    true:\n        print(1)\n    _:\n        print(2)");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CaseCondStmt>>(prog[0]));
    const auto &caseStmt = *std::get<std::unique_ptr<CaseCondStmt>>(prog[0]);
    ASSERT_EQ(caseStmt.arms.size(), 1u);
    ASSERT_EQ(caseStmt.else_body.size(), 1u);
}

TEST(ParserTest, CaseCondWildcardMustBeLast) {
    EXPECT_THROW(parseStr("case:\n    _:\n        print(0)\n    true:\n        print(1)"),
                 std::runtime_error);
}

TEST(ParserTest, CaseCondExprWildcardMustBeLast) {
    EXPECT_THROW(parseStr("x = case:\n    _ : 0\n    true : 1"),
                 std::runtime_error);
}

TEST(ParserTest, IfBlockMultipleStatements) {
    Program prog = parseStr("if true:\n    x = 1\n    print(x)");
    const auto &ifStmt = *std::get<std::unique_ptr<IfStmt>>(prog[0]);
    ASSERT_EQ(ifStmt.branch.body.size(), 2u);
    EXPECT_TRUE(std::holds_alternative<AssignStmt>(ifStmt.branch.body[0]));
    EXPECT_TRUE(std::holds_alternative<CallStmt>(ifStmt.branch.body[1]));
}

TEST(ParserTest, IfMissingColonThrows) {
    EXPECT_THROW(parseStr("if true\n    print(1)"), std::runtime_error);
}

TEST(ParserTest, IfEmptyBlockThrows) {
    EXPECT_THROW(parseStr("if true:\nprint(1)"), std::runtime_error);
}

// ===== while パーサーテスト =====

TEST(ParserTest, WhileSimple) {
    Program prog = parseStr("while true:\n    print(1)");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<WhileStmt>>(prog[0]));
    const auto &ws = *std::get<std::unique_ptr<WhileStmt>>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<BoolExpr>(ws.condition->data));
    ASSERT_EQ(ws.body.size(), 1u);
    EXPECT_TRUE(std::holds_alternative<CallStmt>(ws.body[0]));
}

TEST(ParserTest, WhileBlockMultipleStatements) {
    Program prog = parseStr("while true:\n    x = 1\n    print(x)");
    const auto &ws = *std::get<std::unique_ptr<WhileStmt>>(prog[0]);
    ASSERT_EQ(ws.body.size(), 2u);
    EXPECT_TRUE(std::holds_alternative<AssignStmt>(ws.body[0]));
    EXPECT_TRUE(std::holds_alternative<CallStmt>(ws.body[1]));
}

TEST(ParserTest, WhileMissingColonThrows) {
    EXPECT_THROW(parseStr("while true\n    print(1)"), std::runtime_error);
}

// ===== using statement (#1817) =====

TEST(ParserTest, UsingSimple) {
    Program prog = parseStr("using f = open(p, \"r\"):\n    readAll(f)");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<UsingStmt>>(prog[0]));
    const auto &us = *std::get<std::unique_ptr<UsingStmt>>(prog[0]);
    EXPECT_EQ(us.name, "f");
    ASSERT_EQ(us.body.size(), 1u);
}

TEST(ParserTest, UsingMissingNameThrows) {
    // `using = expr:` → expected variable name after 'using'
    EXPECT_THROW(parseStr("using = open(p, \"r\"):\n    readAll(f)"),
                 std::runtime_error);
}

TEST(ParserTest, UsingMissingEqualsThrows) {
    // `using f open(...):` → expected '=' after using variable name
    EXPECT_THROW(parseStr("using f open(p, \"r\"):\n    readAll(f)"),
                 std::runtime_error);
}

TEST(ParserTest, UsingMissingColonThrows) {
    // `using f = expr` (no colon) → expected ':' after init expression
    EXPECT_THROW(parseStr("using f = open(p, \"r\")\n    readAll(f)"),
                 std::runtime_error);
}

// ===== function / return / CallExpr パーサーテスト =====

TEST(ParserTest, FnSimple) {
    Program prog = parseStr("fn add(a: int, b: int) -> int:\n    return a + b");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<FnStmt>>(prog[0]));
    const auto &function = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(function.name, "add");
    ASSERT_EQ(function.params.size(), 2u);
    EXPECT_EQ(function.params[0].name, "a");
    EXPECT_EQ(function.params[0].type->toString(), "int");
    EXPECT_EQ(function.params[1].name, "b");
    EXPECT_EQ(function.params[1].type->toString(), "int");
    EXPECT_EQ(function.return_type->toString(), "int");
    ASSERT_EQ(function.body.size(), 1u);
    EXPECT_TRUE(std::holds_alternative<ReturnStmt>(function.body[0]));
}

TEST(ParserTest, LegacyFunctionKeywordRejectedInDeclaration) {
    EXPECT_THROW(parseStr("function add(a: int, b: int) -> int:\n    return a + b"), std::runtime_error);
}

TEST(ParserTest, LegacyFunctionKeywordRejectedInFnType) {
    EXPECT_THROW(parseStr("type Callback = function(int, int) -> int"), std::runtime_error);
}

TEST(ParserTest, FnIdentifierIsRejected) {
    EXPECT_THROW(parseStr("fn = 1\nx = fn + 2"), std::runtime_error);
}

TEST(ParserTest, ReturnStatement) {
    Program prog = parseStr("fn f() -> int:\n    return 42");
    const auto &function = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    ASSERT_EQ(function.body.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<ReturnStmt>(function.body[0]));
    const auto &ret = std::get<ReturnStmt>(function.body[0]);
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(ret.value->data));
    EXPECT_EQ(std::get<NumberExpr>(ret.value->data).value, 42);
}

TEST(ParserTest, CallExprInLet) {
    Program prog = parseStr("x = add(1, 2)");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CallExpr>>(s.value->data));
    const auto &call = *std::get<std::unique_ptr<CallExpr>>(s.value->data);
    EXPECT_EQ(call.callee, "add");
    ASSERT_EQ(call.args.size(), 2u);
}

TEST(ParserTest, FnMissingColonThrows) {
    EXPECT_THROW(parseStr("fn f() -> int\n    return 1"), std::runtime_error);
}

TEST(ParserTest, FnMissingArrowWithTypeThrows) {
    // function f() int: → missing '->' before type, "int" is not ':'
    EXPECT_THROW(parseStr("fn f() int:\n    return 1"), std::runtime_error);
}

TEST(ParserTest, FnReturnTypeOmitted) {
    // function f(): → return type defaults to "" (inferred at codegen)
    Program prog = parseStr("fn f():\n    return");
    ASSERT_EQ(prog.size(), 1u);
    const auto &function = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(function.name, "f");
    EXPECT_EQ(function.return_type, nullptr);
    ASSERT_EQ(function.body.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<ReturnStmt>(function.body[0]));
    const auto &ret = std::get<ReturnStmt>(function.body[0]);
    EXPECT_EQ(ret.value, nullptr);
}

TEST(ParserTest, FnExplicitUnitReturn) {
    Program prog = parseStr("fn f() -> Unit:\n    return");
    const auto &function = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(function.return_type->toString(), "Unit");
}

TEST(ParserTest, LambdaReturnTypeOmitted) {
    // (x: int) => x + 1 → return type is empty (inferred at codegen time)
    Program prog = parseStr("f = (x: int) => x + 1");
    ASSERT_EQ(prog.size(), 1u);
    const auto &assign = std::get<AssignStmt>(prog[0]);
    const auto &lambda = std::get<std::unique_ptr<LambdaExpr>>(assign.value->data);
    EXPECT_EQ(lambda->return_type, nullptr);
}

TEST(ParserTest, LambdaExplicitReturnType) {
    // (x: int) -> int => x + 1 → return type is "int"
    Program prog = parseStr("f = (x: int) -> int => x + 1");
    ASSERT_EQ(prog.size(), 1u);
    const auto &assign = std::get<AssignStmt>(prog[0]);
    const auto &lambda = std::get<std::unique_ptr<LambdaExpr>>(assign.value->data);
    EXPECT_EQ(lambda->return_type->toString(), "int");
}

TEST(ParserTest, LambdaOptionAWithParenPrefix) {
    Program prog = parseStr("f = (x: int) -> int => x + 1");
    ASSERT_EQ(prog.size(), 1u);
    const auto &assign = std::get<AssignStmt>(prog[0]);
    const auto &lambda = std::get<std::unique_ptr<LambdaExpr>>(assign.value->data);
    ASSERT_EQ(lambda->params.size(), 1u);
    EXPECT_EQ(lambda->params[0].name, "x");
    ASSERT_TRUE(lambda->return_type != nullptr);
    EXPECT_EQ(lambda->return_type->toString(), "int");
    ASSERT_TRUE(lambda->expr_body != nullptr);
}

TEST(ParserTest, TypeAnnotationOptionInt) {
    Program prog = parseStr("x: Option<int> = None");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(s.type_annotation != nullptr);
    EXPECT_EQ(s.type_annotation->toString(),"Option<int>");
}

TEST(ParserTest, FnParamOptionType) {
    Program prog = parseStr("fn f(x: Option<int>) -> int:\n    return 0");
    const auto &function = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    ASSERT_EQ(function.params.size(), 1u);
    EXPECT_EQ(function.params[0].type->toString(), "Option<int>");
}

TEST(ParserTest, FnReturnOptionType) {
    Program prog = parseStr("fn f() -> Option<int>:\n    return Some(1)");
    const auto &function = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(function.return_type->toString(), "Option<int>");
}

// ===== import パーサーテスト =====

TEST(ParserTest, ImportAll) {
    Program prog = parseStr("from math");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<ImportStmt>(prog[0]));
    const auto &imp = std::get<ImportStmt>(prog[0]);
    EXPECT_EQ(imp.module_path, "math");
    EXPECT_TRUE(imp.names.empty());
}

TEST(ParserTest, ImportSingleFunction) {
    Program prog = parseStr("from math import add");
    ASSERT_EQ(prog.size(), 1u);
    const auto &imp = std::get<ImportStmt>(prog[0]);
    EXPECT_EQ(imp.module_path, "math");
    ASSERT_EQ(imp.names.size(), 1u);
    EXPECT_EQ(imp.names[0].name, "add");
    EXPECT_FALSE(imp.names[0].alias.has_value());
}

TEST(ParserTest, ImportMultipleFunctions) {
    Program prog = parseStr("from math import add, sub");
    const auto &imp = std::get<ImportStmt>(prog[0]);
    EXPECT_EQ(imp.module_path, "math");
    ASSERT_EQ(imp.names.size(), 2u);
    EXPECT_EQ(imp.names[0].name, "add");
    EXPECT_FALSE(imp.names[0].alias.has_value());
    EXPECT_EQ(imp.names[1].name, "sub");
    EXPECT_FALSE(imp.names[1].alias.has_value());
}

TEST(ParserTest, ImportDotPath) {
    Program prog = parseStr("from utils.math import add");
    const auto &imp = std::get<ImportStmt>(prog[0]);
    EXPECT_EQ(imp.module_path, "utils/math");
    ASSERT_EQ(imp.names.size(), 1u);
    EXPECT_EQ(imp.names[0].name, "add");
}

TEST(ParserTest, ImportExpectedModuleName) {
    EXPECT_THROW(parseStr("from 42"), std::runtime_error);
}

TEST(ParserTest, ImportInBlockThrows) {
    EXPECT_THROW(parseStr("if true:\n    from math import add"), std::runtime_error);
}

// ===== relative import tests =====

TEST(ParserTest, RelativeImportDot) {
    Program prog = parseStr("from . import add");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<ImportStmt>(prog[0]));
    const auto &imp = std::get<ImportStmt>(prog[0]);
    EXPECT_EQ(imp.module_path, ".");
    ASSERT_EQ(imp.names.size(), 1u);
    EXPECT_EQ(imp.names[0].name, "add");
}

TEST(ParserTest, RelativeImportDotMultiple) {
    Program prog = parseStr("from . import add, sub");
    const auto &imp = std::get<ImportStmt>(prog[0]);
    EXPECT_EQ(imp.module_path, ".");
    ASSERT_EQ(imp.names.size(), 2u);
    EXPECT_EQ(imp.names[0].name, "add");
    EXPECT_EQ(imp.names[1].name, "sub");
}

TEST(ParserTest, RelativeImportDotAll) {
    Program prog = parseStr("from .");
    ASSERT_EQ(prog.size(), 1u);
    const auto &imp = std::get<ImportStmt>(prog[0]);
    EXPECT_EQ(imp.module_path, ".");
    EXPECT_TRUE(imp.names.empty());
}

TEST(ParserTest, RelativeImportDotSubmodule) {
    Program prog = parseStr("from .utils import helper");
    const auto &imp = std::get<ImportStmt>(prog[0]);
    EXPECT_EQ(imp.module_path, "./utils");
    ASSERT_EQ(imp.names.size(), 1u);
    EXPECT_EQ(imp.names[0].name, "helper");
}

TEST(ParserTest, RelativeImportDotNestedSubmodule) {
    Program prog = parseStr("from .utils.calc import add");
    const auto &imp = std::get<ImportStmt>(prog[0]);
    EXPECT_EQ(imp.module_path, "./utils/calc");
    ASSERT_EQ(imp.names.size(), 1u);
    EXPECT_EQ(imp.names[0].name, "add");
}

TEST(ParserTest, ImportHyphenError) {
    try {
        parseStr("from ry-tutorial import add");
        FAIL() << "Expected exception";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("hyphens"), std::string::npos)
            << "Error message should mention hyphens: " << msg;
        EXPECT_NE(msg.find("module names"), std::string::npos)
            << "Error message should say 'module names' (not 'package names'): " << msg;
    }
}

TEST(ParserTest, ImportRelativeHyphenError) {
    try {
        parseStr("from .-mod import add");
        FAIL() << "Expected exception";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("hyphens"), std::string::npos)
            << "Error message should mention hyphens: " << msg;
        EXPECT_NE(msg.find("module names"), std::string::npos)
            << "Error message should say 'module names' (not 'package names'): " << msg;
    }
}

TEST(ParserTest, ImportFromMissingModuleNameError) {
    try {
        parseStr("from 123 import add");
        FAIL() << "Expected exception";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("expected module name after 'from'"), std::string::npos)
            << "Error message should say 'expected module name after from' (not 'package name'): "
            << msg;
    }
}

TEST(ParserTest, ImportParentDirError) {
    EXPECT_THROW(parseStr("from .. import add"), std::runtime_error);
}

// `expect` is the only Ry keyword (TokenKind::Expect) accepted at the
// import-name position; it names the testing intrinsic exposed by
// `from testing import expect` (#712). Other keywords are still rejected
// here, including `fn` (TokenKind::Fn). The pair below locks both halves
// of the narrow widening so a future relaxation that accidentally allows
// any keyword at this position is caught.
TEST(ParserTest, ImportExpectKeywordAccepted) {
    Program prog = parseStr("from testing import expect");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<ImportStmt>(prog[0]));
    const auto &imp = std::get<ImportStmt>(prog[0]);
    EXPECT_EQ(imp.module_path, "testing");
    ASSERT_EQ(imp.names.size(), 1u);
    EXPECT_EQ(imp.names[0].name, "expect");
}

TEST(ParserTest, ImportFnKeywordRejected) {
    try {
        parseStr("from testing import fn");
        FAIL() << "Expected parser to reject 'fn' at import-name position";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("expected function name after 'import'"), std::string::npos)
            << "Error message should match the import-name diagnostic: " << msg;
    }
}

TEST(ParserTest, ImportFnKeywordRejectedAfterComma) {
    try {
        parseStr("from testing import expect, fn");
        FAIL() << "Expected parser to reject 'fn' after comma in import list";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("expected function name after ','"), std::string::npos)
            << "Error message should match the post-comma import-name diagnostic: " << msg;
    }
}

// ===== wildcard import rejection (#1748) =====
//
// `*` is not accepted at any import-name position. All four sites
// (non-braced head, braced head, non-braced post-comma, braced post-comma)
// produce the same actionable diagnostic. Wildcard import is intentionally
// unsupported; selective or braced forms must be used instead.

TEST(ParserTest, ImportWildcardNonBracedRejected) {
    try {
        parseStr("from math import *");
        FAIL() << "Expected parser to reject '*' as wildcard import";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("wildcards"), std::string::npos)
            << "Error message should mention wildcards: " << msg;
    }
}

TEST(ParserTest, ImportWildcardBracedRejected) {
    try {
        parseStr("from math import {*}");
        FAIL() << "Expected parser to reject '*' inside braced import";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("wildcards"), std::string::npos)
            << "Error message should mention wildcards: " << msg;
    }
}

TEST(ParserTest, ImportWildcardAfterCommaRejected) {
    try {
        parseStr("from math import a, *");
        FAIL() << "Expected parser to reject '*' after comma in non-braced import";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("wildcards"), std::string::npos)
            << "Error message should mention wildcards: " << msg;
    }
}

TEST(ParserTest, ImportWildcardBracedAfterCommaRejected) {
    try {
        parseStr("from math import {a, *}");
        FAIL() << "Expected parser to reject '*' after comma in braced import";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("wildcards"), std::string::npos)
            << "Error message should mention wildcards: " << msg;
    }
}

// ===== alias tests (#1721) =====

TEST(ParserTest, ImportSingleAlias) {
    Program prog = parseStr("from math import add as plus");
    ASSERT_EQ(prog.size(), 1u);
    const auto &imp = std::get<ImportStmt>(prog[0]);
    ASSERT_EQ(imp.names.size(), 1u);
    EXPECT_EQ(imp.names[0].name, "add");
    ASSERT_TRUE(imp.names[0].alias.has_value());
    EXPECT_EQ(*imp.names[0].alias, "plus");
}

TEST(ParserTest, ImportMixedAlias) {
    Program prog = parseStr("from math import a, b as B, c");
    const auto &imp = std::get<ImportStmt>(prog[0]);
    ASSERT_EQ(imp.names.size(), 3u);
    EXPECT_EQ(imp.names[0].name, "a");
    EXPECT_FALSE(imp.names[0].alias.has_value());
    EXPECT_EQ(imp.names[1].name, "b");
    ASSERT_TRUE(imp.names[1].alias.has_value());
    EXPECT_EQ(*imp.names[1].alias, "B");
    EXPECT_EQ(imp.names[2].name, "c");
    EXPECT_FALSE(imp.names[2].alias.has_value());
}

// Self-alias (`foo as foo`) is parser-normalized to no alias so AST and
// formatter output stay canonical (a redundant binding is never generated).
TEST(ParserTest, ImportSelfAliasNormalized) {
    Program prog = parseStr("from math import add as add");
    const auto &imp = std::get<ImportStmt>(prog[0]);
    ASSERT_EQ(imp.names.size(), 1u);
    EXPECT_EQ(imp.names[0].name, "add");
    EXPECT_FALSE(imp.names[0].alias.has_value());
}

TEST(ParserTest, ImportExpectKeywordAsAlias) {
    // `expect` is accepted as an alias target the same way it is accepted as
    // an import name (#712 keyword carve-out). Lock both halves so a future
    // narrowing of one path keeps the other consistent.
    Program prog = parseStr("from testing import fail as expect");
    const auto &imp = std::get<ImportStmt>(prog[0]);
    ASSERT_EQ(imp.names.size(), 1u);
    EXPECT_EQ(imp.names[0].name, "fail");
    ASSERT_TRUE(imp.names[0].alias.has_value());
    EXPECT_EQ(*imp.names[0].alias, "expect");
}

TEST(ParserTest, ImportAsRequiresIdent) {
    try {
        parseStr("from math import add as 123");
        FAIL() << "Expected parser to reject non-identifier alias";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("expected identifier after 'as'"), std::string::npos)
            << "Error message should match the alias diagnostic: " << msg;
    }
}

TEST(ParserTest, ImportAsRequiresAliasNotKeyword) {
    // 'fn' is a keyword; using it as an alias target must be rejected, mirroring
    // the existing `from m import fn` rejection at the name position.
    EXPECT_THROW(parseStr("from math import add as fn"), std::runtime_error);
}

// ===== braced selective import tests (#1722) =====

TEST(ParserTest, ImportBracedSingleItem) {
    Program prog = parseStr("from math import { add }");
    ASSERT_EQ(prog.size(), 1u);
    const auto &imp = std::get<ImportStmt>(prog[0]);
    EXPECT_EQ(imp.module_path, "math");
    ASSERT_EQ(imp.names.size(), 1u);
    EXPECT_EQ(imp.names[0].name, "add");
    EXPECT_FALSE(imp.names[0].alias.has_value());
}

TEST(ParserTest, ImportBracedMultipleItems) {
    Program prog = parseStr("from math import { add, sub }");
    const auto &imp = std::get<ImportStmt>(prog[0]);
    ASSERT_EQ(imp.names.size(), 2u);
    EXPECT_EQ(imp.names[0].name, "add");
    EXPECT_EQ(imp.names[1].name, "sub");
}

TEST(ParserTest, ImportBracedSingleTrailingComma) {
    Program prog = parseStr("from math import { add, }");
    const auto &imp = std::get<ImportStmt>(prog[0]);
    ASSERT_EQ(imp.names.size(), 1u);
    EXPECT_EQ(imp.names[0].name, "add");
}

TEST(ParserTest, ImportBracedTrailingComma) {
    Program prog = parseStr("from math import { a, b, }");
    const auto &imp = std::get<ImportStmt>(prog[0]);
    ASSERT_EQ(imp.names.size(), 2u);
    EXPECT_EQ(imp.names[0].name, "a");
    EXPECT_EQ(imp.names[1].name, "b");
}

TEST(ParserTest, ImportBracedWithAlias) {
    // Combines with #1721 symbol alias.
    Program prog = parseStr("from math import { add as plus, sub }");
    const auto &imp = std::get<ImportStmt>(prog[0]);
    ASSERT_EQ(imp.names.size(), 2u);
    EXPECT_EQ(imp.names[0].name, "add");
    ASSERT_TRUE(imp.names[0].alias.has_value());
    EXPECT_EQ(*imp.names[0].alias, "plus");
    EXPECT_EQ(imp.names[1].name, "sub");
    EXPECT_FALSE(imp.names[1].alias.has_value());
}

TEST(ParserTest, ImportBracedMultiLine) {
    Program prog = parseStr("from math import {\n  add,\n  sub,\n}\n");
    const auto &imp = std::get<ImportStmt>(prog[0]);
    ASSERT_EQ(imp.names.size(), 2u);
    EXPECT_EQ(imp.names[0].name, "add");
    EXPECT_EQ(imp.names[1].name, "sub");
}

TEST(ParserTest, ImportBracedExpectKeyword) {
    // `expect` keyword carve-out works inside braces as well (mirrors #712).
    Program prog = parseStr("from testing import { expect }");
    const auto &imp = std::get<ImportStmt>(prog[0]);
    ASSERT_EQ(imp.names.size(), 1u);
    EXPECT_EQ(imp.names[0].name, "expect");
}

TEST(ParserTest, ImportBracedRejectsEmpty) {
    try {
        parseStr("from math import {}");
        FAIL() << "Expected parser to reject empty braced import list";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("expected import name after '{'"), std::string::npos)
            << "Error message should match the braced empty diagnostic: " << msg;
    }
}

TEST(ParserTest, ImportBracedRejectsUnclosed) {
    EXPECT_THROW(parseStr("from math import { add"), std::runtime_error);
}

TEST(ParserTest, ImportBracedRejectsMissingComma) {
    EXPECT_THROW(parseStr("from math import { a b }"), std::runtime_error);
}

TEST(ParserTest, ImportBracedRejectsBadAlias) {
    // Alias rule (#1721) applies inside braces too.
    EXPECT_THROW(parseStr("from math import { add as 123 }"), std::runtime_error);
}

// ===== qualified import tests (#1723) =====

TEST(ParserTest, QualifiedImportSingleModule) {
    Program prog = parseStr("import math");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<QualifiedImportStmt>>(prog[0]));
    const auto &qi = *std::get<std::unique_ptr<QualifiedImportStmt>>(prog[0]);
    EXPECT_EQ(qi.module_name, "math");
    EXPECT_FALSE(qi.alias.has_value());
}

TEST(ParserTest, QualifiedImportDottedPathRejected) {
    // AC6: 'import a.b' (dot-separated) is rejected with clear error.
    try {
        parseStr("import a.b");
        FAIL() << "Expected parser to reject dotted module path in qualified import";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("dotted module paths"), std::string::npos)
            << "Error should mention dotted module paths: " << msg;
    }
}

TEST(ParserTest, QualifiedImportDuplicateRejected) {
    // AC7: duplicate 'import math' in same file is a compile error.
    // The same-module-duplicate branch and the alias-collision branch
    // emit deliberately distinct wordings; assert this case takes the
    // non-alias branch so a future refactor that collapses them is caught.
    try {
        parseStr("import math\nimport math");
        FAIL() << "Expected parser to reject duplicate qualified import";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("duplicate qualified import"), std::string::npos)
            << "Error should mention duplicate qualified import: " << msg;
        EXPECT_NE(msg.find("'import math'"), std::string::npos)
            << "Same-module branch should quote the full 'import <mod>' form: " << msg;
        EXPECT_EQ(msg.find("alias collision"), std::string::npos)
            << "Same-module branch must not use alias-collision wording: " << msg;
    }
}

TEST(ParserTest, QualifiedImportAsRegistersAlias) {
    // #1724: 'import math as m' parses successfully and stores the alias on
    // the QualifiedImportStmt while preserving the original module name. This
    // is the positive sibling of the (formerly rejecting) test that locked
    // in the pre-#1724 spec — flipped per
    // .claude/rules/tests-rejection-tdd.md "Relaxing a rejection branch
    // requires flipping (not deleting) existing EXPECT_THROW tests".
    Program prog = parseStr("import math as m");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<QualifiedImportStmt>>(prog[0]));
    const auto &qi = *std::get<std::unique_ptr<QualifiedImportStmt>>(prog[0]);
    EXPECT_EQ(qi.module_name, "math");
    ASSERT_TRUE(qi.alias.has_value());
    EXPECT_EQ(*qi.alias, "m");
}

TEST(ParserTest, QualifiedImportAliasOnlyRegistersAliasNotOriginal) {
    // Python-style semantic: `import math as m` makes `m` (and only `m`) a
    // qualified-import handle. Bare `math.sqrt(2.0)` after the alias must
    // fall through to UFCS dispatch (CallExpr with receiver prepended,
    // qualified_module unset) — the qualified-call path requires
    // `imported_modules_.count(name) > 0`, which only the alias satisfies.
    Program prog = parseStr("import math as m\nx = math.sqrt(2.0)\n");
    ASSERT_EQ(prog.size(), 2u);
    const auto &assign = std::get<AssignStmt>(prog[1]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CallExpr>>(assign.value->data));
    const auto &call = *std::get<std::unique_ptr<CallExpr>>(assign.value->data);
    EXPECT_EQ(call.callee, "sqrt");
    EXPECT_FALSE(call.qualified_module.has_value());
    // UFCS prepends the receiver, so the call has 2 args (math, 2.0).
    ASSERT_EQ(call.args.size(), 2u);
    ASSERT_TRUE(std::holds_alternative<VariableExpr>(call.args[0]->data));
    EXPECT_EQ(std::get<VariableExpr>(call.args[0]->data).name, "math");
}

TEST(ParserTest, QualifiedImportAliasCollisionRejected) {
    // Two distinct modules aliased to the same effective name must collide
    // at parse time, even though they originate from different module names.
    // The wording is intentionally distinct from the same-module-duplicate
    // branch (see QualifiedImportDuplicateRejected) so the two cases stay
    // separable across future refactors.
    try {
        parseStr("import math as m\nimport path as m");
        FAIL() << "Expected parser to reject duplicate alias 'm'";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("duplicate qualified import"), std::string::npos)
            << "Error should mention duplicate qualified import: " << msg;
        EXPECT_NE(msg.find("name 'm'"), std::string::npos)
            << "Alias branch should quote the colliding effective name as `name 'm'`: " << msg;
        EXPECT_NE(msg.find("alias collision"), std::string::npos)
            << "Alias branch should mention 'alias collision' to differentiate from the same-module case: " << msg;
    }
}

TEST(ParserTest, QualifiedImportSelfAliasIsAccepted) {
    // Degenerate `import math as math` is accepted and behaves identically
    // to bare `import math` — the effective name is 'math' in both cases.
    Program prog = parseStr("import math as math");
    ASSERT_EQ(prog.size(), 1u);
    const auto &qi = *std::get<std::unique_ptr<QualifiedImportStmt>>(prog[0]);
    EXPECT_EQ(qi.module_name, "math");
    ASSERT_TRUE(qi.alias.has_value());
    EXPECT_EQ(*qi.alias, "math");
}

TEST(ParserTest, QualifiedImportAliasRejectsSnakeCase) {
    // Per .claude/rules/parser-conventions.md "Module-global typed-decl ...
    // enforces camelCase", every binding site rejects snake_case.
    try {
        parseStr("import math as my_alias");
        FAIL() << "Expected parser to reject snake_case alias name";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("'my_alias'"), std::string::npos)
            << "Error should quote the rejected alias name: " << msg;
        EXPECT_NE(msg.find("camelCase"), std::string::npos)
            << "Error should mention camelCase requirement: " << msg;
    }
}

TEST(ParserTest, QualifiedImportAliasShadowingRejected) {
    // The shadow-check at parser.cpp consults `imported_modules_`, which
    // (post-#1724) is keyed by the alias when present. Binding the alias as
    // a local must trigger the same diagnostic as bare-import shadowing.
    try {
        parseStr("import math as m\nm: int = 1\n");
        FAIL() << "Expected parser to reject local binding that shadows alias";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("cannot shadow imported module 'm'"), std::string::npos)
            << "Error should mention shadow rejection of alias 'm': " << msg;
    }
}

TEST(ParserTest, QualifiedImportAliasFieldAccessRouting) {
    // After `import math as m`, the postfix parser must recognize `m.PI`
    // as a qualified field access (qualified_module == "m"), not as UFCS
    // on a local `m`. The alias must propagate through the
    // `imported_modules_.count(ve->name)` gate in src/parser_expr.cpp.
    Program prog = parseStr("import math as m\nx = m.PI");
    ASSERT_EQ(prog.size(), 2u);
    const auto &assign = std::get<AssignStmt>(prog[1]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<FieldAccessExpr>>(assign.value->data));
    const auto &fa = *std::get<std::unique_ptr<FieldAccessExpr>>(assign.value->data);
    EXPECT_EQ(fa.field, "PI");
    ASSERT_TRUE(fa.qualified_module.has_value());
    EXPECT_EQ(*fa.qualified_module, "m");
}

TEST(ParserTest, QualifiedImportInBlockThrows) {
    // 'import' is only allowed at top level, mirroring the existing 'from'
    // block-context rejection.
    EXPECT_THROW(parseStr("fn f() -> Unit:\n    import math\n    return"),
                 std::runtime_error);
}

TEST(ParserTest, QualifiedImportAsExpectsIdentifierAfterAs) {
    // Sibling-branch test: when 'as' is present but the next token is not
    // an identifier, the parser must reject with the dedicated message
    // BEFORE the "not yet supported" diagnostic fires.
    try {
        parseStr("import math as 42");
        FAIL() << "Expected parser to reject non-identifier after 'as'";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("expected identifier after 'as'"), std::string::npos)
            << "Error should mention expected identifier after 'as': " << msg;
    }
}

TEST(ParserTest, QualifiedImportExpectedModuleName) {
    try {
        parseStr("import 42");
        FAIL() << "Expected parser to reject non-identifier after 'import'";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("expected module name after 'import'"), std::string::npos)
            << "Error should mention expected module name: " << msg;
    }
}

TEST(ParserTest, QualifiedImportCoexistsWithSelectiveImport) {
    // AC2: 'import math' and 'from math import PI' can coexist in the same
    // file. The parser must not reject the combination.
    Program prog = parseStr("import math\nfrom math import PI");
    ASSERT_EQ(prog.size(), 2u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<QualifiedImportStmt>>(prog[0]));
    ASSERT_TRUE(std::holds_alternative<ImportStmt>(prog[1]));
}

TEST(ParserTest, QualifiedImportDotCallProducesQualifiedCallExpr) {
    // AC1: 'math.sqrt(2.0)' after 'import math' becomes a CallExpr with
    // qualified_module set and NO prepended receiver (unlike UFCS).
    Program prog = parseStr("import math\nx = math.sqrt(2.0)");
    ASSERT_EQ(prog.size(), 2u);
    const auto &assign = std::get<AssignStmt>(prog[1]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CallExpr>>(assign.value->data));
    const auto &call = *std::get<std::unique_ptr<CallExpr>>(assign.value->data);
    EXPECT_EQ(call.callee, "sqrt");
    ASSERT_TRUE(call.qualified_module.has_value());
    EXPECT_EQ(*call.qualified_module, "math");
    // Qualified call must NOT prepend the receiver; the only arg is 2.0.
    ASSERT_EQ(call.args.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<FloatExpr>(call.args[0]->data));
}

TEST(ParserTest, QualifiedImportDotFieldProducesQualifiedFieldAccess) {
    // qualified const access: math.PI → FieldAccessExpr with qualified_module
    Program prog = parseStr("import math\nx = math.PI");
    ASSERT_EQ(prog.size(), 2u);
    const auto &assign = std::get<AssignStmt>(prog[1]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<FieldAccessExpr>>(assign.value->data));
    const auto &fa = *std::get<std::unique_ptr<FieldAccessExpr>>(assign.value->data);
    EXPECT_EQ(fa.field, "PI");
    ASSERT_TRUE(fa.qualified_module.has_value());
    EXPECT_EQ(*fa.qualified_module, "math");
}

TEST(ParserTest, QualifiedImportDoesNotBreakUFCS) {
    // AC4: when the LHS is NOT a qualified-imported module name, the dot
    // still produces UFCS (CallExpr with receiver prepended, NO
    // qualified_module). 'hello' is a local binding, not an imported module.
    Program prog = parseStr("import math\nhello = \"x\"\nn = hello.length()");
    ASSERT_EQ(prog.size(), 3u);
    const auto &nAssign = std::get<AssignStmt>(prog[2]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CallExpr>>(nAssign.value->data));
    const auto &call = *std::get<std::unique_ptr<CallExpr>>(nAssign.value->data);
    EXPECT_EQ(call.callee, "length");
    EXPECT_FALSE(call.qualified_module.has_value());
    // UFCS prepends the receiver.
    ASSERT_EQ(call.args.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<VariableExpr>(call.args[0]->data));
}

TEST(ParserTest, QualifiedImportShadowingByBareAssignRejected) {
    // 'import math' makes 'math' a module namespace; binding 'math' to a
    // value would create a confusing namespace conflict, so we reject it
    // at parse time with a clear diagnostic. (Conservative v0.0.23 behavior
    // per plan #1723; may be relaxed later.)
    try {
        parseStr("import math\nmath = 42");
        FAIL() << "Expected parser to reject shadowing of imported module";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("cannot shadow imported module 'math'"), std::string::npos)
            << "Error should mention shadow rejection: " << msg;
    }
}

TEST(ParserTest, QualifiedImportShadowingByTypedAssignRejected) {
    EXPECT_THROW(parseStr("import math\nmath: int = 42"), std::runtime_error);
}

TEST(ParserTest, QualifiedImportShadowingByTupleDestructRejected) {
    // First-position shadowing in tuple destructure.
    EXPECT_THROW(parseStr("import math\nmath, y = (1, 2)"), std::runtime_error);
}

TEST(ParserTest, QualifiedImportShadowingByTupleDestructRestRejected) {
    // Rest-position shadowing in tuple destructure (mirrors the bare/first
    // position; both sites must be guarded).
    EXPECT_THROW(parseStr("import math\nx, math = (1, 2)"), std::runtime_error);
}

TEST(ParserTest, QualifiedImportShadowingByCompoundAssignRejected) {
    EXPECT_THROW(parseStr("import math\nmath += 1"), std::runtime_error);
}

TEST(ParserTest, QualifiedImportShadowingByIncrementRejected) {
    EXPECT_THROW(parseStr("import math\nmath++"), std::runtime_error);
}

TEST(ParserTest, QualifiedImportShadowingByDecrementRejected) {
    // Mirror of the increment test — both ++ and -- bind a name locally
    // and must be guarded the same way.
    EXPECT_THROW(parseStr("import math\nmath--"), std::runtime_error);
}

TEST(ParserTest, QualifiedImportShadowingByForLoopVarRejected) {
    // 'for math in ...' shadows the imported module inside the loop body,
    // which then routes 'math.sqrt(...)' to the qualified call rather than
    // to the loop variable — silently the wrong result. Reject at parse time.
    try {
        parseStr("import math\nfor math in [1, 2]:\n    print(math)\n");
        FAIL() << "Expected parser to reject for-loop binding that shadows imported module";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("cannot shadow imported module 'math'"), std::string::npos)
            << "Error should mention shadow rejection: " << msg;
    }
}

TEST(ParserTest, QualifiedImportShadowingByForLoopTupleElementRejected) {
    // Rest-position shadowing in for-loop tuple destructure. Both first and
    // rest positions must be guarded, mirroring the regular tuple-destruct
    // tests above.
    EXPECT_THROW(
        parseStr("import math\nfor i, math in [(1, 2)]:\n    print(math)\n"),
        std::runtime_error);
}

TEST(ParserTest, QualifiedImportShadowingByForLoopTupleFirstRejected) {
    EXPECT_THROW(
        parseStr("import math\nfor math, j in [(1, 2)]:\n    print(math)\n"),
        std::runtime_error);
}

TEST(ParserTest, QualifiedImportShadowingByFnParamRejected) {
    // Function parameter named 'math' would silently shadow the import
    // inside the body but still route 'math.sqrt(...)' to the qualified
    // call, ignoring the argument. Reject at parse time.
    try {
        parseStr("import math\nfn f(math: int) -> int:\n    return math\n");
        FAIL() << "Expected parser to reject fn parameter that shadows imported module";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("cannot shadow imported module 'math'"), std::string::npos)
            << "Error should mention shadow rejection: " << msg;
    }
}

TEST(ParserTest, QualifiedImportShadowingByLambdaParamRejected) {
    // Lambda parameter named 'math'. The lambda parser uses the commit-flag
    // pattern to defer hard errors past the speculative try/catch, so this
    // throw must surface as a user-visible diagnostic, not get swallowed
    // and re-emitted as a generic 'expected =' error from the outer stmt
    // parser.
    try {
        parseStr("import math\nf = (math: int) => math + 1\n");
        FAIL() << "Expected parser to reject lambda parameter that shadows imported module";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("cannot shadow imported module 'math'"), std::string::npos)
            << "Error should mention shadow rejection: " << msg;
    }
}

TEST(ParserTest, QualifiedImportShadowingByBareLambdaParamRejected) {
    // Bare-paren-omitted single-param lambda `math => expr` (#1572 form)
    // must also be guarded, mirroring the parenthesized lambda above.
    // The bare form has no try/catch wrapping so the throw surfaces
    // directly — but the test makes the guard explicit so a future
    // refactor that drops the call doesn't silently re-allow shadowing.
    try {
        parseStr("import math\nf = math => math + 1\n");
        FAIL() << "Expected parser to reject bare lambda param that shadows imported module";
    } catch (const std::runtime_error &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("cannot shadow imported module 'math'"), std::string::npos)
            << "Error should mention shadow rejection: " << msg;
    }
}

TEST(ParserTest, QualifiedImportShadowingByParenTupleDestructFirstRejected) {
    // Parenthesized tuple destructure first-position binding. The bare-form
    // path was already guarded (QualifiedImportShadowingByTupleDestructFirstRejected
    // above) but the parenthesized variant lives in a separate code path
    // (parser.cpp, looksLikeParenthesizedTupleDestructure branch) and must
    // be guarded independently.
    EXPECT_THROW(parseStr("import math\n(math, y) = (1, 2)"), std::runtime_error);
}

TEST(ParserTest, QualifiedImportShadowingByParenTupleDestructRestRejected) {
    // Rest-position shadowing in the parenthesized form — mirror of the
    // first-position guard. Both first and rest sites must be checked.
    EXPECT_THROW(parseStr("import math\n(x, math) = (1, 2)"), std::runtime_error);
}

TEST(ParserTest, QualifiedImportChainedQualifiedFieldAccessAtStmt) {
    // AC1 / CR-review (#1729): `math.PI.toStr()` at statement position must
    // set qualified_module on the FIRST FieldAccessExpr so codegen routes
    // PI through the namespace lookup before the trailing UFCS hop. Without
    // the fix the multi-hop chain in parser.cpp built the FieldAccessExpr
    // without qualified_module and codegen errored with "undefined
    // variable: math".
    Program prog = parseStr("import math\nmath.PI.toStr()");
    // [QualifiedImportStmt, ExprStmt] — the qualified import counts as a
    // statement and lives at prog[0].
    ASSERT_EQ(prog.size(), 2u);
    const auto &es = std::get<ExprStmt>(prog.back());
    // Chain shape: CallExpr{toStr, args=[FieldAccessExpr{math, PI, qm=math}]}
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CallExpr>>(es.expr->data));
    const auto &call = *std::get<std::unique_ptr<CallExpr>>(es.expr->data);
    EXPECT_EQ(call.callee, "toStr");
    ASSERT_EQ(call.args.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<FieldAccessExpr>>(call.args[0]->data));
    const auto &fa = *std::get<std::unique_ptr<FieldAccessExpr>>(call.args[0]->data);
    EXPECT_EQ(fa.field, "PI");
    ASSERT_TRUE(fa.qualified_module.has_value());
    EXPECT_EQ(*fa.qualified_module, "math");
}

TEST(ParserTest, QualifiedImportChainedQualifiedCallAtStmt) {
    // CR-review (#1729): statement-side dot fast path used to short-circuit
    // back to ExprStmt right after the qualified 1-hop call, dropping any
    // postfix tail. `math.sqrt(2.0).toStr()` at statement position must now
    // chain through parsePostfixContinuation so the trailing `.toStr()`
    // wraps the qualified CallExpr as a UFCS first-arg.
    Program prog = parseStr("import math\nmath.sqrt(2.0).toStr()");
    ASSERT_EQ(prog.size(), 2u);
    const auto &es = std::get<ExprStmt>(prog.back());
    // Outer call is the UFCS hop `.toStr()`, inner first-arg is the
    // qualified call `math.sqrt(2.0)`.
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CallExpr>>(es.expr->data));
    const auto &outer = *std::get<std::unique_ptr<CallExpr>>(es.expr->data);
    EXPECT_EQ(outer.callee, "toStr");
    EXPECT_FALSE(outer.qualified_module.has_value());
    ASSERT_FALSE(outer.args.empty());
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CallExpr>>(outer.args[0]->data));
    const auto &inner = *std::get<std::unique_ptr<CallExpr>>(outer.args[0]->data);
    EXPECT_EQ(inner.callee, "sqrt");
    ASSERT_TRUE(inner.qualified_module.has_value());
    EXPECT_EQ(*inner.qualified_module, "math");
}

TEST(ParserTest, QualifiedImportKeywordMemberAtStmt) {
    // CR-review (#1729): statement-side dot fast path used to require
    // TokenKind::Ident after `.`, rejecting keyword tokens like `expect`,
    // `and`, etc. that arrive from the lexer's keyword_map. The expression
    // side already accepted them via isKeywordAfterDot — the statement
    // side must now match.
    //
    // Use a synthetic stdlib-shaped module that does not exist so codegen
    // would reject downstream, but parse must succeed and produce the
    // qualified CallExpr. We import `testing` (a real importable name) and
    // call its `expect` member, which yields TokenKind::Expect from the
    // lexer.
    Program prog = parseStr("import testing\ntesting.expect(1)");
    ASSERT_EQ(prog.size(), 2u);
    const auto &es = std::get<ExprStmt>(prog.back());
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CallExpr>>(es.expr->data));
    const auto &call = *std::get<std::unique_ptr<CallExpr>>(es.expr->data);
    EXPECT_EQ(call.callee, "expect");
    ASSERT_TRUE(call.qualified_module.has_value());
    EXPECT_EQ(*call.qualified_module, "testing");
}

TEST(ParserTest, QualifiedImportDoesNotBreakStructFieldAccess) {
    // AC5: regular struct field access (p.x where p is a value, not a
    // module) still produces FieldAccessExpr with NO qualified_module.
    Program prog = parseStr(
        "import math\nrecord Point:\n    x: int\np = Point(3)\nv = p.x");
    const auto &vAssign = std::get<AssignStmt>(prog.back());
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<FieldAccessExpr>>(vAssign.value->data));
    const auto &fa = *std::get<std::unique_ptr<FieldAccessExpr>>(vAssign.value->data);
    EXPECT_EQ(fa.field, "x");
    EXPECT_FALSE(fa.qualified_module.has_value());
}

TEST(ParserTest, DuplicateFieldNameThrows) {
    EXPECT_THROW(parseStr("record Point:\n    x: int\n    x: int"), std::runtime_error);
}

// ===== タプル パーサーテスト =====

TEST(ParserTest, TupleLiteral) {
    Program prog = parseStr("t = (1, 2)");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<TupleExpr>>(s.value->data));
    const auto &tuple = *std::get<std::unique_ptr<TupleExpr>>(s.value->data);
    ASSERT_EQ(tuple.elements.size(), 2u);
    EXPECT_EQ(std::get<NumberExpr>(tuple.elements[0]->data).value, 1);
    EXPECT_EQ(std::get<NumberExpr>(tuple.elements[1]->data).value, 2);
}

TEST(ParserTest, TupleMixedTypes) {
    Program prog = parseStr("t = (1, 3.14)");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<TupleExpr>>(s.value->data));
    const auto &tuple = *std::get<std::unique_ptr<TupleExpr>>(s.value->data);
    ASSERT_EQ(tuple.elements.size(), 2u);
    EXPECT_TRUE(std::holds_alternative<NumberExpr>(tuple.elements[0]->data));
    EXPECT_TRUE(std::holds_alternative<FloatExpr>(tuple.elements[1]->data));
}

TEST(ParserTest, TupleThreeElements) {
    Program prog = parseStr("t = (1, 2, 3)");
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<TupleExpr>>(s.value->data));
    const auto &tuple = *std::get<std::unique_ptr<TupleExpr>>(s.value->data);
    ASSERT_EQ(tuple.elements.size(), 3u);
}

TEST(ParserTest, TupleTypeAnnotation) {
    Program prog = parseStr("t: (int, float) = (1, 3.14)");
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(s.type_annotation != nullptr);
    EXPECT_EQ(s.type_annotation->toString(),"(int, float)");
}

TEST(ParserTest, TupleIndexAccess) {
    // t.0 → FieldAccessExpr with field "0"
    Program prog = parseStr("x = t.0");
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<FieldAccessExpr>>(s.value->data));
    const auto &fa = *std::get<std::unique_ptr<FieldAccessExpr>>(s.value->data);
    EXPECT_EQ(fa.field, "0");
    ASSERT_TRUE(std::holds_alternative<VariableExpr>(fa.object->data));
    EXPECT_EQ(std::get<VariableExpr>(fa.object->data).name, "t");
}

TEST(ParserTest, FnReturnTupleType) {
    Program prog = parseStr("fn swap(a: int, b: int) -> (int, int):\n    return (b, a)");
    const auto &function = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(function.return_type->toString(), "(int, int)");
}

TEST(ParserTest, ParenGroupingStillWorks) {
    // Single expression in parens is still grouping, not tuple
    Program prog = parseStr("x = (1 + 2) * 3");
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<BinaryExpr>>(s.value->data));
    const auto &outer = *std::get<std::unique_ptr<BinaryExpr>>(s.value->data);
    EXPECT_EQ(outer.op, "*");
}

TEST(ParserTest, TupleSingleElement) {
    Program prog = parseStr("t = (42,)");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<TupleExpr>>(s.value->data));
    const auto &tuple = *std::get<std::unique_ptr<TupleExpr>>(s.value->data);
    ASSERT_EQ(tuple.elements.size(), 1u);
    EXPECT_EQ(std::get<NumberExpr>(tuple.elements[0]->data).value, 42);
}

TEST(ParserTest, TupleTrailingComma) {
    Program prog = parseStr("t = (1, 2,)");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<TupleExpr>>(s.value->data));
    const auto &tuple = *std::get<std::unique_ptr<TupleExpr>>(s.value->data);
    ASSERT_EQ(tuple.elements.size(), 2u);
    EXPECT_EQ(std::get<NumberExpr>(tuple.elements[0]->data).value, 1);
    EXPECT_EQ(std::get<NumberExpr>(tuple.elements[1]->data).value, 2);
}

TEST(ParserTest, TupleSingleElementString) {
    Program prog = parseStr("t = (\"hello\",)");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<TupleExpr>>(s.value->data));
    const auto &tuple = *std::get<std::unique_ptr<TupleExpr>>(s.value->data);
    ASSERT_EQ(tuple.elements.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<StringExpr>(tuple.elements[0]->data));
}

// ===== Parenthesized tuple destructuring assignment (#1189) =====

TEST(ParserTest, ParenTupleDestructBasic) {
    Program prog = parseStr("(a, b) = (10, 20)");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<TupleDestructStmt>(prog[0]));
    const auto &s = std::get<TupleDestructStmt>(prog[0]);
    ASSERT_EQ(s.names.size(), 2u);
    EXPECT_EQ(s.names[0], "a");
    EXPECT_EQ(s.names[1], "b");
    EXPECT_FALSE(s.is_immutable);
}

TEST(ParserTest, ParenTupleDestructThree) {
    Program prog = parseStr("(a, b, c) = (1, 2, 3)");
    const auto &s = std::get<TupleDestructStmt>(prog[0]);
    ASSERT_EQ(s.names.size(), 3u);
    EXPECT_EQ(s.names[2], "c");
}

TEST(ParserTest, ParenTupleDestructWildcard) {
    Program prog = parseStr("(_, b) = (10, 20)");
    const auto &s = std::get<TupleDestructStmt>(prog[0]);
    ASSERT_EQ(s.names.size(), 2u);
    EXPECT_EQ(s.names[0], "_");
    EXPECT_EQ(s.names[1], "b");
}

TEST(ParserTest, ParenTupleDestructConst) {
    Program prog = parseStr("@const\n(a, b) = (1, 2)");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<TupleDestructStmt>(prog[0]));
    const auto &s = std::get<TupleDestructStmt>(prog[0]);
    EXPECT_TRUE(s.is_immutable);
    ASSERT_EQ(s.names.size(), 2u);
    EXPECT_EQ(s.names[0], "a");
}

TEST(ParserTest, ParenTupleDestructConstSameLine) {
    // Same-line form exercises the `parseDirectives` LParen-deferral guard:
    // without it, `@const (a, b)` would be parsed as `@const(a, b)` directive
    // args and the trailing `=` would trip the directive-not-supported error.
    Program prog = parseStr("@const (a, b) = (1, 2)");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<TupleDestructStmt>(prog[0]));
    const auto &s = std::get<TupleDestructStmt>(prog[0]);
    EXPECT_TRUE(s.is_immutable);
    ASSERT_EQ(s.names.size(), 2u);
    EXPECT_EQ(s.names[0], "a");
    EXPECT_EQ(s.names[1], "b");
}

TEST(ParserTest, ParenTupleDestructConstSameLineTrailingCommaRejected) {
    // Same-line `@const (a,)` must still reject: lookahead returns false (no
    // second Ident after comma), parseDirectives falls back to treating `(..)`
    // as directive args, and the statement parser raises parserError.
    EXPECT_THROW(parseStr("@const (a,) = (1,)"), std::runtime_error);
}

TEST(ParserTest, ParenTupleDestructSingleNameIsGroupingExpr) {
    // `(a) = expr` must NOT be treated as tuple destructuring.
    // The lookahead requires ≥1 comma (≥2 names). `(a)` alone falls through
    // to expression-statement parsing, which then treats '=' as unexpected.
    EXPECT_THROW(parseStr("(a) = 42"), std::runtime_error);
}

TEST(ParserTest, ParenTupleDestructRejectsArbitraryExprLhs) {
    // `(a + b) = expr` is not a tuple destructure — the lookahead rejects
    // non-ident tokens and falls through to expression parsing.
    EXPECT_THROW(parseStr("(a + b) = 42"), std::runtime_error);
}

TEST(ParserTest, ParenTupleAsExpressionStatement) {
    // Bare `(a, b)` (without `=`) stays an expression statement producing a
    // TupleExpr — ensuring the lookahead did not consume tokens speculatively.
    Program prog = parseStr("a = 1\nb = 2\n(a, b)");
    ASSERT_EQ(prog.size(), 3u);
    ASSERT_TRUE(std::holds_alternative<ExprStmt>(prog[2]));
    const auto &es = std::get<ExprStmt>(prog[2]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<TupleExpr>>(es.expr->data));
}

TEST(ParserTest, ParenTupleDestructTrailingCommaRejected) {
    // Single-name with trailing comma: the lookahead requires an identifier
    // after each comma, so `(a,)` is not a valid destructure LHS.
    EXPECT_THROW(parseStr("(a,) = (1,)"), std::runtime_error);
}

TEST(ParserTest, ParenTupleDestructRejectsSnakeCase) {
    // #1450: parenthesized tuple-destructure LHS must enforce camelCase on
    // every name (both the first and any rest names).
    EXPECT_THROW(parseStr("(my_x, my_y) = (1, 2)"), std::runtime_error);
    EXPECT_THROW(parseStr("(a, my_b) = (1, 2)"), std::runtime_error);
    EXPECT_THROW(parseStr("(my_a, b) = (1, 2)"), std::runtime_error);
}

TEST(ParserTest, BareTupleDestructRejectsSnakeCaseFirst) {
    // #1450: bare tuple-destructure form must reject snake_case on the first
    // name (consumed by the outer Ident dispatch before reaching the comma
    // branch).
    EXPECT_THROW(parseStr("my_a, b = (1, 2)"), std::runtime_error);
}

TEST(ParserTest, BareTupleDestructRejectsSnakeCaseRest) {
    // #1450: bare tuple-destructure form must reject snake_case on a rest
    // name (consumed inside the comma loop).
    EXPECT_THROW(parseStr("a, my_b = (1, 2)"), std::runtime_error);
}

TEST(ParserTest, BareTupleDestructAcceptsCamelCase) {
    // #1450 positive baseline: camelCase names parse cleanly in bare form.
    Program prog = parseStr("myA, myB = (1, 2)");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<TupleDestructStmt>(prog[0]));
    const auto &s = std::get<TupleDestructStmt>(prog[0]);
    ASSERT_EQ(s.names.size(), 2u);
    EXPECT_EQ(s.names[0], "myA");
    EXPECT_EQ(s.names[1], "myB");
}

TEST(ParserTest, BareTupleDestructAcceptsUnderscore) {
    // #1450: `_` placeholder must remain accepted in bare form (and at any
    // position).
    Program prog = parseStr("_, b = (1, 2)");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<TupleDestructStmt>(prog[0]);
    ASSERT_EQ(s.names.size(), 2u);
    EXPECT_EQ(s.names[0], "_");
    EXPECT_EQ(s.names[1], "b");

    Program prog2 = parseStr("a, _ = (1, 2)");
    const auto &s2 = std::get<TupleDestructStmt>(prog2[0]);
    ASSERT_EQ(s2.names.size(), 2u);
    EXPECT_EQ(s2.names[0], "a");
    EXPECT_EQ(s2.names[1], "_");
}

// ===== UFCS パーサーテスト =====

TEST(ParserTest, UFCSBasic) {
    // a.f(b) → CallExpr{f, [a, b]}
    Program prog = parseStr("x = a.f(b)");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CallExpr>>(s.value->data));
    const auto &call = *std::get<std::unique_ptr<CallExpr>>(s.value->data);
    EXPECT_EQ(call.callee, "f");
    ASSERT_EQ(call.args.size(), 2u);
    ASSERT_TRUE(std::holds_alternative<VariableExpr>(call.args[0]->data));
    EXPECT_EQ(std::get<VariableExpr>(call.args[0]->data).name, "a");
    ASSERT_TRUE(std::holds_alternative<VariableExpr>(call.args[1]->data));
    EXPECT_EQ(std::get<VariableExpr>(call.args[1]->data).name, "b");
}

TEST(ParserTest, UFCSNoArgs) {
    // a.f() → CallExpr{f, [a]}
    Program prog = parseStr("x = a.f()");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CallExpr>>(s.value->data));
    const auto &call = *std::get<std::unique_ptr<CallExpr>>(s.value->data);
    EXPECT_EQ(call.callee, "f");
    ASSERT_EQ(call.args.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<VariableExpr>(call.args[0]->data));
    EXPECT_EQ(std::get<VariableExpr>(call.args[0]->data).name, "a");
}

TEST(ParserTest, UFCSChained) {
    // a.f(b).g(c) → CallExpr{g, [CallExpr{f, [a, b]}, c]}
    Program prog = parseStr("x = a.f(b).g(c)");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CallExpr>>(s.value->data));
    const auto &outer = *std::get<std::unique_ptr<CallExpr>>(s.value->data);
    EXPECT_EQ(outer.callee, "g");
    ASSERT_EQ(outer.args.size(), 2u);
    // first arg is CallExpr{f, [a, b]}
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CallExpr>>(outer.args[0]->data));
    const auto &inner = *std::get<std::unique_ptr<CallExpr>>(outer.args[0]->data);
    EXPECT_EQ(inner.callee, "f");
    ASSERT_EQ(inner.args.size(), 2u);
    EXPECT_EQ(std::get<VariableExpr>(inner.args[0]->data).name, "a");
    EXPECT_EQ(std::get<VariableExpr>(inner.args[1]->data).name, "b");
    // second arg is c
    EXPECT_EQ(std::get<VariableExpr>(outer.args[1]->data).name, "c");
}

TEST(ParserTest, UFCSWithFieldAccess) {
    // p.x.f() → CallExpr{f, [FieldAccessExpr{p, x}]}
    Program prog = parseStr("r = p.x.f()");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CallExpr>>(s.value->data));
    const auto &call = *std::get<std::unique_ptr<CallExpr>>(s.value->data);
    EXPECT_EQ(call.callee, "f");
    ASSERT_EQ(call.args.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<FieldAccessExpr>>(call.args[0]->data));
    const auto &fa = *std::get<std::unique_ptr<FieldAccessExpr>>(call.args[0]->data);
    EXPECT_EQ(fa.field, "x");
    ASSERT_TRUE(std::holds_alternative<VariableExpr>(fa.object->data));
    EXPECT_EQ(std::get<VariableExpr>(fa.object->data).name, "p");
}

// ===== Map パーステスト =====

TEST(ParserTest, MapLiteral) {
    Program prog = parseStr("m = {\"a\": 1, \"b\": 2}");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    EXPECT_EQ(s.name, "m");
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<MapExpr>>(s.value->data));
    const auto &map = *std::get<std::unique_ptr<MapExpr>>(s.value->data);
    ASSERT_EQ(map.keys.size(), 2u);
    ASSERT_EQ(map.values.size(), 2u);
    ASSERT_TRUE(std::holds_alternative<StringExpr>(map.keys[0]->data));
    EXPECT_EQ(std::get<StringExpr>(map.keys[0]->data).value, "a");
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(map.values[0]->data));
    EXPECT_EQ(std::get<NumberExpr>(map.values[0]->data).value, 1);
}

TEST(ParserTest, IndexAssignStmt) {
    Program prog = parseStr("m = {\"a\": 1}\nm[\"b\"] = 2");
    ASSERT_EQ(prog.size(), 2u);
    ASSERT_TRUE(std::holds_alternative<IndexAssignStmt>(prog[1]));
    const auto &s = std::get<IndexAssignStmt>(prog[1]);
    ASSERT_TRUE(std::holds_alternative<VariableExpr>(s.object->data));
    EXPECT_EQ(std::get<VariableExpr>(s.object->data).name, "m");
    ASSERT_TRUE(std::holds_alternative<StringExpr>(s.indices[0]->data));
    EXPECT_EQ(std::get<StringExpr>(s.indices[0]->data).value, "b");
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(s.value->data));
    EXPECT_EQ(std::get<NumberExpr>(s.value->data).value, 2);
}

TEST(ParserTest, MapTypeAnnotation) {
    Program prog = parseStr("m: Map<str, int> = {\"a\": 1}");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(s.type_annotation != nullptr);
    EXPECT_EQ(s.type_annotation->toString(),"Map<str, int>");
}

// ===== Operator overloading =====

TEST(ParserTest, OperatorFnBinaryPlus) {
    std::string src =
        "fn operator+(a: Vec2, b: Vec2) -> Vec2:\n"
        "    return a\n";
    Program prog = parseStr(src);
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<FnStmt>>(prog[0]));
    const auto &function = std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(function->name, "operator+");
    EXPECT_TRUE(function->is_operator);
    ASSERT_EQ(function->params.size(), 2u);
    EXPECT_EQ(function->params[0].name, "a");
    EXPECT_EQ(function->params[0].type->toString(), "Vec2");
    EXPECT_EQ(function->params[1].name, "b");
    EXPECT_EQ(function->params[1].type->toString(), "Vec2");
    EXPECT_EQ(function->return_type->toString(), "Vec2");
}

TEST(ParserTest, OperatorFnUnaryMinus) {
    std::string src =
        "fn operator-(a: Vec2) -> Vec2:\n"
        "    return a\n";
    Program prog = parseStr(src);
    ASSERT_EQ(prog.size(), 1u);
    const auto &function = std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(function->name, "operator-");
    EXPECT_TRUE(function->is_operator);
    ASSERT_EQ(function->params.size(), 1u);
}

TEST(ParserTest, OperatorFnEqEq) {
    std::string src =
        "fn operator==(a: Vec2, b: Vec2) -> bool:\n"
        "    return true\n";
    Program prog = parseStr(src);
    const auto &function = std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(function->name, "operator==");
    EXPECT_TRUE(function->is_operator);
}

TEST(ParserTest, OperatorFnTildeUnaryOnly) {
    // ~ with 2 params should fail
    std::string src =
        "fn operator~(a: int, b: int) -> int:\n"
        "    return a\n";
    EXPECT_THROW(parseStr(src), std::runtime_error);
}

TEST(ParserTest, OperatorFnInvalidParamCount) {
    // Binary operator with 0 params should fail
    std::string src =
        "fn operator+() -> int:\n"
        "    return 0\n";
    EXPECT_THROW(parseStr(src), std::runtime_error);
}

// ===== Operator return type constraint =====

TEST(ParserTest, OperatorEqMustReturnBool) {
    EXPECT_THROW(parseStr(
        "fn operator==(a: int, b: int) -> int:\n"
        "    return 42\n"), std::runtime_error);
}

TEST(ParserTest, OperatorNeqMustReturnBool) {
    EXPECT_THROW(parseStr(
        "fn operator!=(a: int, b: int) -> str:\n"
        "    return \"no\"\n"), std::runtime_error);
}

TEST(ParserTest, OperatorLessMustReturnBool) {
    EXPECT_THROW(parseStr(
        "fn operator<(a: int, b: int) -> int:\n"
        "    return 0\n"), std::runtime_error);
}

TEST(ParserTest, OperatorLessEqMustReturnBool) {
    EXPECT_THROW(parseStr(
        "fn operator<=(a: int, b: int) -> int:\n"
        "    return 0\n"), std::runtime_error);
}

TEST(ParserTest, OperatorGreaterMustReturnBool) {
    EXPECT_THROW(parseStr(
        "fn operator>(a: int, b: int) -> int:\n"
        "    return 0\n"), std::runtime_error);
}

TEST(ParserTest, OperatorGreaterEqMustReturnBool) {
    EXPECT_THROW(parseStr(
        "fn operator>=(a: int, b: int) -> int:\n"
        "    return 0\n"), std::runtime_error);
}

TEST(ParserTest, OperatorNotMustReturnBool) {
    EXPECT_THROW(parseStr(
        "fn operator not(a: int) -> int:\n"
        "    return 0\n"), std::runtime_error);
}

TEST(ParserTest, OperatorAndMustReturnBool) {
    EXPECT_THROW(parseStr(
        "fn operator and(a: int, b: int) -> int:\n"
        "    return 0\n"), std::runtime_error);
}

TEST(ParserTest, OperatorOrMustReturnBool) {
    EXPECT_THROW(parseStr(
        "fn operator or(a: int, b: int) -> int:\n"
        "    return 0\n"), std::runtime_error);
}

TEST(ParserTest, OperatorEqReturnBoolOk) {
    Program prog = parseStr(
        "fn operator==(a: int, b: int) -> bool:\n"
        "    return true\n");
    EXPECT_EQ(prog.size(), 1u);
}

TEST(ParserTest, OperatorPlusReturnsNonBoolOk) {
    Program prog = parseStr(
        "fn operator+(a: int, b: int) -> int:\n"
        "    return a + b\n");
    EXPECT_EQ(prog.size(), 1u);
}

TEST(ParserTest, OperatorPlusRejectsSpaceForm) {
    EXPECT_THROW(parseStr(
        "fn operator +(a: int, b: int) -> int:\n"
        "    return a + b\n"), std::runtime_error);
}

TEST(ParserTest, OperatorEqEqRejectsSpaceForm) {
    EXPECT_THROW(parseStr(
        "fn operator ==(a: int, b: int) -> bool:\n"
        "    return true\n"), std::runtime_error);
}

TEST(ParserTest, OperatorPlusEqRejectsSpaceForm) {
    EXPECT_THROW(parseStr(
        "fn operator +=(a: int, b: int) -> int:\n"
        "    return a + b\n"), std::runtime_error);
}

TEST(ParserTest, OperatorTildeRejectsSpaceForm) {
    EXPECT_THROW(parseStr(
        "fn operator ~(a: int) -> int:\n"
        "    return a\n"), std::runtime_error);
}

TEST(ParserTest, OperatorAndKeywordAcceptsSpaceForm) {
    Program prog = parseStr(
        "fn operator and(a: bool, b: bool) -> bool:\n"
        "    return a\n");
    EXPECT_EQ(prog.size(), 1u);
}

TEST(ParserTest, OperatorInKeywordAcceptsSpaceForm) {
    Program prog = parseStr(
        "fn operator in(a: int, b: int) -> bool:\n"
        "    return true\n");
    EXPECT_EQ(prog.size(), 1u);
}

TEST(ParserTest, OperatorOrKeywordAcceptsSpaceForm) {
    Program prog = parseStr(
        "fn operator or(a: bool, b: bool) -> bool:\n"
        "    return a\n");
    EXPECT_EQ(prog.size(), 1u);
}

TEST(ParserTest, OperatorNotKeywordAcceptsSpaceForm) {
    Program prog = parseStr(
        "fn operator not(a: bool) -> bool:\n"
        "    return a\n");
    EXPECT_EQ(prog.size(), 1u);
}

TEST(ParserTest, OperatorAsKeywordAcceptsSpaceForm) {
    Program prog = parseStr(
        "fn operator as(a: int) -> bool:\n"
        "    return true\n");
    EXPECT_EQ(prog.size(), 1u);
}

// ===== Compound assignment operator tests =====

TEST(ParserTest, CompoundAssignPreservesOp) {
    Program prog = parseStr("x += 1\n");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    EXPECT_EQ(s.name, "x");
    ASSERT_TRUE(s.compound_op.has_value());
    EXPECT_EQ(*s.compound_op, "+");
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(s.value->data));
    EXPECT_EQ(std::get<NumberExpr>(s.value->data).value, 1);
}

TEST(ParserTest, CompoundAssignAllOperators) {
    std::vector<std::pair<std::string, std::string>> cases = {
        {"x += 1\n", "+"}, {"x -= 1\n", "-"}, {"x *= 1\n", "*"},
        {"x /= 1\n", "/"}, {"x %= 1\n", "%"}, {"x //= 1\n", "//"},
        {"x **= 1\n", "**"}, {"x &= 1\n", "&"}, {"x |= 1\n", "|"},
        {"x ^= 1\n", "^"}, {"x <<= 1\n", "<<"}, {"x >>= 1\n", ">>"},
    };
    for (auto &[src, expected_op] : cases) {
        Program prog = parseStr(src);
        ASSERT_EQ(prog.size(), 1u) << "Failed for: " << src;
        const auto &s = std::get<AssignStmt>(prog[0]);
        ASSERT_TRUE(s.compound_op.has_value()) << "Failed for: " << src;
        EXPECT_EQ(*s.compound_op, expected_op) << "Failed for: " << src;
    }
}

TEST(ParserTest, IncrementPreservesCompoundOp) {
    Program prog = parseStr("x++\n");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    EXPECT_EQ(s.name, "x");
    ASSERT_TRUE(s.compound_op.has_value());
    EXPECT_EQ(*s.compound_op, "+");
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(s.value->data));
    EXPECT_EQ(std::get<NumberExpr>(s.value->data).value, 1);
}

TEST(ParserTest, DecrementPreservesCompoundOp) {
    Program prog = parseStr("x--\n");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    EXPECT_EQ(s.name, "x");
    ASSERT_TRUE(s.compound_op.has_value());
    EXPECT_EQ(*s.compound_op, "-");
}

TEST(ParserTest, OperatorFnCompoundPlusEq) {
    std::string src =
        "fn operator+=(a: Vec2, b: Vec2) -> Vec2:\n"
        "    return a\n";
    Program prog = parseStr(src);
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<FnStmt>>(prog[0]));
    const auto &function = std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(function->name, "operator+=");
    EXPECT_TRUE(function->is_operator);
    ASSERT_EQ(function->params.size(), 2u);
    EXPECT_EQ(function->params[0].name, "a");
    EXPECT_EQ(function->params[0].type->toString(), "Vec2");
    EXPECT_EQ(function->return_type->toString(), "Vec2");
}

TEST(ParserTest, OperatorFnCompoundAssignRequiresTwoParams) {
    EXPECT_THROW(
        parseStr("fn operator+=(a: Vec2) -> Vec2:\n    return a\n"),
        std::runtime_error);
}

TEST(ParserTest, PlainAssignHasNoCompoundOp) {
    Program prog = parseStr("x = 1\n");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    EXPECT_FALSE(s.compound_op.has_value());
}

// ===== Set パーサーテスト =====

TEST(ParserTest, SetLiteral) {
    Program prog = parseStr("s = {1, 2, 3}");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<SetExpr>>(s.value->data));
    const auto &set = *std::get<std::unique_ptr<SetExpr>>(s.value->data);
    ASSERT_EQ(set.elements.size(), 3u);
    EXPECT_EQ(std::get<NumberExpr>(set.elements[0]->data).value, 1);
    EXPECT_EQ(std::get<NumberExpr>(set.elements[1]->data).value, 2);
    EXPECT_EQ(std::get<NumberExpr>(set.elements[2]->data).value, 3);
}

TEST(ParserTest, SetSingleElement) {
    Program prog = parseStr("s = {42}");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<SetExpr>>(s.value->data));
    const auto &set = *std::get<std::unique_ptr<SetExpr>>(s.value->data);
    ASSERT_EQ(set.elements.size(), 1u);
    EXPECT_EQ(std::get<NumberExpr>(set.elements[0]->data).value, 42);
}

TEST(ParserTest, SetTypeAnnotation) {
    Program prog = parseStr("s: Set<int> = {1}");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(s.type_annotation != nullptr);
    EXPECT_EQ(s.type_annotation->toString(),"Set<int>");
}

TEST(ParserTest, InOperator) {
    Program prog = parseStr("r = x in s");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<BinaryExpr>>(s.value->data));
    const auto &bin = *std::get<std::unique_ptr<BinaryExpr>>(s.value->data);
    EXPECT_EQ(bin.op, "in");
    EXPECT_EQ(std::get<VariableExpr>(bin.lhs->data).name, "x");
    EXPECT_EQ(std::get<VariableExpr>(bin.rhs->data).name, "s");
}

// ===== Enum パーサーテスト =====

TEST(ParserTest, EnumDefinition) {
    Program prog = parseStr("enum Color:\n    Red\n    Green\n    Blue");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<EnumStmt>(prog[0]));
    const auto &es = std::get<EnumStmt>(prog[0]);
    EXPECT_EQ(es.name, "Color");
    ASSERT_EQ(es.variants.size(), 3u);
    EXPECT_EQ(es.variants[0].name, "Red");
    EXPECT_EQ(es.variants[1].name, "Green");
    EXPECT_EQ(es.variants[2].name, "Blue");
}

TEST(ParserTest, EnumAccess) {
    Program prog = parseStr("c = Color::Red");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<EnumAccessExpr>(s.value->data));
    const auto &ea = std::get<EnumAccessExpr>(s.value->data);
    EXPECT_EQ(ea.enum_name, "Color");
    EXPECT_EQ(ea.variant_name, "Red");
}

TEST(ParserTest, EnumComparison) {
    Program prog = parseStr("r = c == Color::Green");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<BinaryExpr>>(s.value->data));
    const auto &bin = *std::get<std::unique_ptr<BinaryExpr>>(s.value->data);
    EXPECT_EQ(bin.op, "==");
    EXPECT_EQ(std::get<VariableExpr>(bin.lhs->data).name, "c");
    ASSERT_TRUE(std::holds_alternative<EnumAccessExpr>(bin.rhs->data));
    const auto &ea = std::get<EnumAccessExpr>(bin.rhs->data);
    EXPECT_EQ(ea.enum_name, "Color");
    EXPECT_EQ(ea.variant_name, "Green");
}

// ===== Union 型パーサーテスト =====

TEST(ParserTest, LetUnionTypeAnnotation) {
    Program prog = parseStr("x: int | str = 42");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    EXPECT_EQ(s.name, "x");
    ASSERT_TRUE(s.type_annotation != nullptr);
    EXPECT_EQ(s.type_annotation->toString(),"int | str");
}

TEST(ParserTest, FnUnionParam) {
    Program prog = parseStr("fn f(x: int | str) -> int:\n    return 0");
    const auto &function = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    ASSERT_EQ(function.params.size(), 1u);
    EXPECT_EQ(function.params[0].type->toString(), "int | str");
}

TEST(ParserTest, FnUnionReturn) {
    Program prog = parseStr("fn f() -> int | str:\n    return 0");
    const auto &function = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(function.return_type->toString(), "int | str");
}

// ===== >>> パーサーテスト =====

TEST(ParserTest, LogicalRightShift) {
    Program prog = parseStr("x = a >>> b");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<BinaryExpr>>(s.value->data));
    const auto &bin = *std::get<std::unique_ptr<BinaryExpr>>(s.value->data);
    EXPECT_EQ(bin.op, ">>>");
    EXPECT_EQ(std::get<VariableExpr>(bin.lhs->data).name, "a");
    EXPECT_EQ(std::get<VariableExpr>(bin.rhs->data).name, "b");
}

// ===== not in パーサーテスト =====

TEST(ParserTest, NotInOperator) {
    Program prog = parseStr("r = x not in s");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<BinaryExpr>>(s.value->data));
    const auto &bin = *std::get<std::unique_ptr<BinaryExpr>>(s.value->data);
    EXPECT_EQ(bin.op, "not in");
    EXPECT_EQ(std::get<VariableExpr>(bin.lhs->data).name, "x");
    EXPECT_EQ(std::get<VariableExpr>(bin.rhs->data).name, "s");
}

TEST(ParserTest, NotStillWorksAfterNotIn) {
    // "not x" should still parse as UnaryExpr
    Program prog = parseStr("r = not x");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<UnaryExpr>>(s.value->data));
    const auto &unary = *std::get<std::unique_ptr<UnaryExpr>>(s.value->data);
    EXPECT_EQ(unary.op, "not");
}

TEST(ParserTest, UnionThreeTypes) {
    Program prog = parseStr("x: int | float | str = 42");
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(s.type_annotation != nullptr);
    EXPECT_EQ(s.type_annotation->toString(),"int | float | str");
}

// ===== Contract (Design by Contract) tests =====

TEST(ParserTest, FnWithRequire) {
    std::string src =
        "fn deposit(amount: int) -> int:\n"
        "    require:\n"
        "        amount > 0\n"
        "    return amount";
    Program prog = parseStr(src);
    ASSERT_EQ(prog.size(), 1u);
    auto &function = std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(function->name, "deposit");
    EXPECT_EQ(function->preconditions.size(), 1u);
    EXPECT_EQ(function->postconditions.size(), 0u);
    EXPECT_EQ(function->body.size(), 1u);
}

TEST(ParserTest, FnWithEnsure) {
    std::string src =
        "fn abs(x: int) -> int:\n"
        "    ensure v:\n"
        "        v >= 0\n"
        "    if x < 0:\n"
        "        return -x\n"
        "    return x";
    Program prog = parseStr(src);
    auto &function = std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(function->preconditions.size(), 0u);
    EXPECT_EQ(function->postconditions.size(), 1u);
    ASSERT_EQ(function->ensure_bindings.size(), 1u);
    EXPECT_EQ(function->ensure_bindings[0], "v");
}

TEST(ParserTest, FnWithRequireAndEnsure) {
    std::string src =
        "fn add(a: int, b: int) -> int:\n"
        "    require:\n"
        "        a >= 0\n"
        "        b >= 0\n"
        "    ensure v:\n"
        "        v >= 0\n"
        "    return a + b";
    Program prog = parseStr(src);
    auto &function = std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(function->preconditions.size(), 2u);
    EXPECT_EQ(function->postconditions.size(), 1u);
    EXPECT_EQ(function->body.size(), 1u);
    ASSERT_EQ(function->ensure_bindings.size(), 1u);
    EXPECT_EQ(function->ensure_bindings[0], "v");
}

TEST(ParserTest, FnWithoutContract) {
    std::string src =
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b";
    Program prog = parseStr(src);
    auto &function = std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(function->preconditions.size(), 0u);
    EXPECT_EQ(function->postconditions.size(), 0u);
}

TEST(ParserTest, TypeWithInvariant) {
    std::string src =
        "record Account:\n"
        "    balance: int\n"
        "    minBalance: int\n"
        "    invariant:\n"
        "        balance >= minBalance";
    Program prog = parseStr(src);
    ASSERT_EQ(prog.size(), 1u);
    auto &ts = std::get<RecordStmt>(prog[0]);
    EXPECT_EQ(ts.name, "Account");
    EXPECT_EQ(ts.fields.size(), 2u);
    EXPECT_EQ(ts.invariants.size(), 1u);
}

TEST(ParserTest, TypeWithoutInvariant) {
    std::string src =
        "record Point:\n"
        "    x: int\n"
        "    y: int";
    Program prog = parseStr(src);
    auto &ts = std::get<RecordStmt>(prog[0]);
    EXPECT_EQ(ts.invariants.size(), 0u);
}

TEST(ParserTest, EnsureVariableBinding) {
    std::string src =
        "fn inc(x: int) -> int:\n"
        "    ensure v:\n"
        "        v == x + 1\n"
        "    return x + 1";
    Program prog = parseStr(src);
    auto &function = std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(function->postconditions.size(), 1u);
    ASSERT_EQ(function->ensure_bindings.size(), 1u);
    EXPECT_EQ(function->ensure_bindings[0], "v");
    // The postcondition expression should be v == x + 1
    auto &postExpr = function->postconditions[0];
    auto *bin = std::get_if<std::unique_ptr<BinaryExpr>>(&postExpr->data);
    ASSERT_TRUE(bin != nullptr);
    // lhs is VariableExpr("v")
    auto *lhsVar = std::get_if<VariableExpr>(&(*bin)->lhs->data);
    ASSERT_TRUE(lhsVar != nullptr);
    EXPECT_EQ(lhsVar->name, "v");
}

TEST(ParserTest, EnsureTupleBinding) {
    std::string src =
        "fn divide(a: int, b: int) -> (int, int):\n"
        "    ensure q, r:\n"
        "        q >= 0\n"
        "    return (a // b, a % b)";
    Program prog = parseStr(src);
    auto &function = std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(function->postconditions.size(), 1u);
    ASSERT_EQ(function->ensure_bindings.size(), 2u);
    EXPECT_EQ(function->ensure_bindings[0], "q");
    EXPECT_EQ(function->ensure_bindings[1], "r");
}

// ===== record キーワード =====

TEST(ParserTest, RecordKeyword) {
    Program prog = parseStr("record Point:\n    x: int\n    y: int");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<RecordStmt>(prog[0]));
    auto &ts = std::get<RecordStmt>(prog[0]);
    EXPECT_EQ(ts.name, "Point");
    EXPECT_EQ(ts.fields.size(), 2u);
}

// ===== type エイリアス =====

TEST(ParserTest, TypeAlias) {
    Program prog = parseStr("type MyInt = int");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<TypeAliasStmt>(prog[0]));
    auto &ta = std::get<TypeAliasStmt>(prog[0]);
    EXPECT_EQ(ta.name, "MyInt");
    EXPECT_EQ(ta.target_type->toString(), "int");
}

// `@public` may be applied to a type alias declaration so that it can be
// imported across package boundaries (#1544).
TEST(ParserTest, TypeAliasAcceptsPublicDirective) {
    Program prog = parseStr("@public\ntype MyInt = int");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<TypeAliasStmt>(prog[0]));
    const auto &ta = std::get<TypeAliasStmt>(prog[0]);
    EXPECT_EQ(ta.name, "MyInt");
    ASSERT_EQ(ta.directives.size(), 1u);
    EXPECT_EQ(ta.directives[0].name, "public");
}

// `@public` may be applied to an enum declaration so that it can be imported
// across package boundaries (#1544).
TEST(ParserTest, EnumAcceptsPublicDirective) {
    Program prog = parseStr("@public\nenum Color:\n    Red\n    Green");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<EnumStmt>(prog[0]));
    const auto &es = std::get<EnumStmt>(prog[0]);
    EXPECT_EQ(es.name, "Color");
    ASSERT_EQ(es.directives.size(), 1u);
    EXPECT_EQ(es.directives[0].name, "public");
}

// ===== for k, v in map =====

TEST(ParserTest, ForKVParsing) {
    Program prog = parseStr("for k, v in m:\n    print(k)");
    ASSERT_EQ(prog.size(), 1u);
    auto &fs = std::get<std::unique_ptr<ForStmt>>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<TuplePattern>>(fs->binding));
    const auto &tp = *std::get<std::unique_ptr<TuplePattern>>(fs->binding);
    ASSERT_EQ(tp.elements.size(), 2u);
    ASSERT_TRUE(std::holds_alternative<VariablePattern>(tp.elements[0]));
    ASSERT_TRUE(std::holds_alternative<VariablePattern>(tp.elements[1]));
    EXPECT_EQ(std::get<VariablePattern>(tp.elements[0]).name, "k");
    EXPECT_EQ(std::get<VariablePattern>(tp.elements[1]).name, "v");
}

TEST(ParserTest, ForThreeVariableDestructuring) {
    Program prog = parseStr("for a, b, c in xs:\n    print(a)");
    ASSERT_EQ(prog.size(), 1u);
    auto &fs = std::get<std::unique_ptr<ForStmt>>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<TuplePattern>>(fs->binding));
    const auto &tp = *std::get<std::unique_ptr<TuplePattern>>(fs->binding);
    ASSERT_EQ(tp.elements.size(), 3u);
    ASSERT_TRUE(std::holds_alternative<VariablePattern>(tp.elements[0]));
    ASSERT_TRUE(std::holds_alternative<VariablePattern>(tp.elements[1]));
    ASSERT_TRUE(std::holds_alternative<VariablePattern>(tp.elements[2]));
    EXPECT_EQ(std::get<VariablePattern>(tp.elements[0]).name, "a");
    EXPECT_EQ(std::get<VariablePattern>(tp.elements[1]).name, "b");
    EXPECT_EQ(std::get<VariablePattern>(tp.elements[2]).name, "c");
}

TEST(ParserTest, ForNestedTupleDestructuring) {
    Program prog = parseStr("for i, (k, v) in enumerate(xs):\n    print(k)");
    ASSERT_EQ(prog.size(), 1u);
    auto &fs = std::get<std::unique_ptr<ForStmt>>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<TuplePattern>>(fs->binding));
    const auto &outer = *std::get<std::unique_ptr<TuplePattern>>(fs->binding);
    ASSERT_EQ(outer.elements.size(), 2u);
    ASSERT_TRUE(std::holds_alternative<VariablePattern>(outer.elements[0]));
    EXPECT_EQ(std::get<VariablePattern>(outer.elements[0]).name, "i");
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<TuplePattern>>(outer.elements[1]));
    const auto &inner = *std::get<std::unique_ptr<TuplePattern>>(outer.elements[1]);
    ASSERT_EQ(inner.elements.size(), 2u);
    ASSERT_TRUE(std::holds_alternative<VariablePattern>(inner.elements[0]));
    ASSERT_TRUE(std::holds_alternative<VariablePattern>(inner.elements[1]));
    EXPECT_EQ(std::get<VariablePattern>(inner.elements[0]).name, "k");
    EXPECT_EQ(std::get<VariablePattern>(inner.elements[1]).name, "v");
}

TEST(ParserTest, ForBindingRejectsUnsupportedPatterns) {
    EXPECT_THROW(parseStr("for Some(x) in xs:\n    print(x)"), std::runtime_error);
    EXPECT_THROW(parseStr("for Point(x, y) in xs:\n    print(x)"), std::runtime_error);
}

TEST(ParserTest, ForChannelParsing) {
    Program prog = parseStr("for x in ch:\n    print(x)");
    ASSERT_EQ(prog.size(), 1u);
    auto &fs = std::get<std::unique_ptr<ForStmt>>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<VariablePattern>(fs->binding));
    EXPECT_EQ(std::get<VariablePattern>(fs->binding).name, "x");
    auto *iter = std::get_if<VariableExpr>(&fs->iterable->data);
    ASSERT_NE(iter, nullptr);
    EXPECT_EQ(iter->name, "ch");
}

TEST(ParserTest, ParallelForDirectiveParsing) {
    Program prog = parseStr("@parallel\nfor i in range(4):\n    print(i)");
    ASSERT_EQ(prog.size(), 1u);
    auto &fs = std::get<std::unique_ptr<ForStmt>>(prog[0]);
    ASSERT_EQ(fs->directives.size(), 1u);
    EXPECT_EQ(fs->directives[0].name, "parallel");
}

TEST(ParserTest, AsyncFnParsing) {
    Program prog = parseStr("async fn add(a: int, b: int) -> int:\n    return a + b");
    ASSERT_EQ(prog.size(), 1u);
    auto &function = std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_TRUE(function->is_async);
    EXPECT_EQ(function->name, "add");
    EXPECT_EQ(function->return_type->toString(), "int");
}

TEST(ParserTest, AsyncFnCanonicalParsing) {
    Program prog = parseStr("async fn add(a: int, b: int) -> int:\n    return a + b");
    ASSERT_EQ(prog.size(), 1u);
    auto &function = std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_TRUE(function->is_async);
    EXPECT_EQ(function->name, "add");
    EXPECT_EQ(function->return_type->toString(), "int");
}

TEST(ParserTest, AwaitExprParsing) {
    // await inside async fn should parse successfully
    Program prog = parseStr(
        "async fn doFetch() -> int:\n"
        "    x = await fetch()\n"
        "    return x");
    ASSERT_EQ(prog.size(), 1u);
    auto &function = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    ASSERT_TRUE(function.is_async);
    ASSERT_GE(function.body.size(), 1u);
    auto &let = std::get<AssignStmt>(function.body[0]);
    auto *await = std::get_if<std::unique_ptr<AwaitExpr>>(&let.value->data);
    ASSERT_NE(await, nullptr);
    auto *call = std::get_if<std::unique_ptr<CallExpr>>(&(*await)->operand->data);
    ASSERT_NE(call, nullptr);
    EXPECT_EQ((*call)->callee, "fetch");
}

TEST(ParserTest, AwaitStatementParsing) {
    // await as statement inside async fn
    Program prog = parseStr(
        "async fn doFetch() -> Unit:\n"
        "    await fetch()");
    ASSERT_EQ(prog.size(), 1u);
    auto &function = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    ASSERT_TRUE(function.is_async);
    ASSERT_EQ(function.body.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<AwaitStmt>(function.body[0]));
    auto &await = std::get<AwaitStmt>(function.body[0]);
    auto *call = std::get_if<std::unique_ptr<CallExpr>>(&await.operand->data);
    ASSERT_NE(call, nullptr);
    EXPECT_EQ((*call)->callee, "fetch");
}

TEST(ParserTest, AwaitOutsideAsyncFnRejected) {
    // await outside async fn should fail
    EXPECT_THROW(parseStr("x = await fetch()"), std::runtime_error);
    EXPECT_THROW(parseStr("await fetch()"), std::runtime_error);
    // await inside lambda within async fn should also fail (lambda is not async)
    EXPECT_THROW(parseStr(
        "async fn foo() -> int:\n"
        "    f = (x: int) => await bar()\n"
        "    return 1"), std::runtime_error);
}

TEST(ParserTest, ParallelDirectiveRejectedOnWhile) {
    EXPECT_THROW(parseStr("@parallel\nwhile true:\n    print(1)"), std::runtime_error);
}

TEST(ParserTest, AsyncWithoutFnRejected) {
    EXPECT_THROW(parseStr("async let x = 1"), std::runtime_error);
}

// ===== .. 演算子 =====

TEST(ParserTest, RangeExpr) {
    Program prog = parseStr("xs = 1 .. 5");
    ASSERT_EQ(prog.size(), 1u);
    auto &let = std::get<AssignStmt>(prog[0]);
    auto *range = std::get_if<std::unique_ptr<RangeExpr>>(&let.value->data);
    ASSERT_TRUE(range != nullptr);
}

// ===== ?? 演算子 =====

TEST(ParserTest, NullCoalesceExpr) {
    Program prog = parseStr("x = a ?? 0");
    ASSERT_EQ(prog.size(), 1u);
    auto &let = std::get<AssignStmt>(prog[0]);
    auto *bin = std::get_if<std::unique_ptr<BinaryExpr>>(&let.value->data);
    ASSERT_TRUE(bin != nullptr);
    EXPECT_EQ((*bin)->op, "??");
}

// ===== none キーワード =====

TEST(ParserTest, NoneExpr) {
    Program prog = parseStr("x = none");
    ASSERT_EQ(prog.size(), 1u);
    auto &let = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<NoneExpr>(let.value->data));
}

// ===== 命名規約チェック =====

TEST(ParserTest, VariableAssignmentAcceptsCamelCase) {
    // Variable names are not checked at parser level
    Program prog = parseStr("myVar = 1");
    ASSERT_EQ(prog.size(), 1u);
    EXPECT_EQ(std::get<AssignStmt>(prog[0]).name, "myVar");
}

TEST(ParserTest, CamelCaseFunctionAccepted) {
    Program prog = parseStr("fn myFunc() -> int:\n    return 1");
    ASSERT_EQ(prog.size(), 1u);
    auto &function = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(function.name, "myFunc");
}

TEST(ParserTest, SnakeCaseFunctionRejected) {
    EXPECT_THROW(parseStr("fn my_func() -> int:\n    return 1"), std::runtime_error);
}

TEST(ParserTest, UnderscorePrefixCamelCaseFunctionAccepted) {
    // A leading `_` is just an identifier prefix as far as the parser is
    // concerned — it carries no visibility meaning (visibility is determined
    // by `@public` and the package boundary of the caller).
    // The body after the prefix must still be camelCase.
    Program prog = parseStr("fn _privateFn() -> int:\n    return 1");
    ASSERT_EQ(prog.size(), 1u);
    auto &function = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(function.name, "_privateFn");
}

TEST(ParserTest, UnderscorePrefixSnakeBodyFunctionRejected) {
    // The `_` prefix does not exempt the body from the camelCase rule.
    EXPECT_THROW(parseStr("fn _private_fn() -> int:\n    return 1"), std::runtime_error);
}

TEST(ParserTest, BareUnderscoreFunctionRejected) {
    // A lone underscore has no camelCase body and must be rejected.
    EXPECT_THROW(parseStr("fn _() -> int:\n    return 1"), std::runtime_error);
}

TEST(ParserTest, BangSuffixFunctionAccepted) {
    Program prog = parseStr("@native\nfn sort!(values: List<int>) -> Unit");
    ASSERT_EQ(prog.size(), 1u);
    auto &function = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(function.name, "sort!");
}

TEST(ParserTest, UnderscorePrefixCamelCaseParamAccepted) {
    // Module-private convention also applies to function parameters: a `_`
    // prefix is allowed as long as the body is camelCase.
    Program prog = parseStr("fn use(_myParam: int) -> int:\n    return 1");
    ASSERT_EQ(prog.size(), 1u);
    auto &function = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    ASSERT_EQ(function.params.size(), 1u);
    EXPECT_EQ(function.params[0].name, "_myParam");
}

TEST(ParserTest, UnderscorePrefixSnakeCaseParamRejected) {
    // The `_` prefix on a parameter does not exempt the body from camelCase.
    EXPECT_THROW(parseStr("fn use(_my_param: int) -> int:\n    return 1"), std::runtime_error);
}

TEST(ParserTest, BangSuffixNonNativeFunctionAccepted) {
    Program prog = parseStr("fn clear!(xs: List<int>) -> Unit:\n    ...");
    ASSERT_EQ(prog.size(), 1u);
    auto &function = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(function.name, "clear!");
}

TEST(ParserTest, CamelCaseNativeFunctionAccepted) {
    Program prog = parseStr("@native\nfn availableParallelism() -> int");
    ASSERT_EQ(prog.size(), 1u);
    auto &function = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(function.name, "availableParallelism");
}

TEST(ParserTest, CamelCaseNonNativeFunctionAccepted) {
    Program prog = parseStr("fn availableParallelism() -> int:\n    return 1");
    ASSERT_EQ(prog.size(), 1u);
    auto &function = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(function.name, "availableParallelism");
}

TEST(ParserTest, ScreamingSnakeCaseNonNativeFunctionRejected) {
    EXPECT_THROW(parseStr("fn MY_FUNC() -> int:\n    return 1"), std::runtime_error);
}

// #1470: typed-declaration form (`name: Type = value`) is the last user-binding
// site that previously accepted snake_case. The parser must enforce camelCase
// at this site too — both for module-globals and for function-local variables,
// since `parseStatement` is shared between top-level and block contexts.

TEST(ParserTest, TypedDeclRejectsSnakeCase) {
    // Module-global form (issue #1470 reproduction).
    EXPECT_THROW(parseStr("snake_global: int = 42"), std::runtime_error);
}

TEST(ParserTest, TypedDeclAcceptsCamelCase) {
    Program prog = parseStr("myGlobal: int = 42");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    EXPECT_EQ(s.name, "myGlobal");
    ASSERT_TRUE(s.type_annotation != nullptr);
    EXPECT_EQ(s.type_annotation->toString(), "int");
}

TEST(ParserTest, TypedDeclAcceptsUnderscorePrefix) {
    // Module-private convention: `_camelCase` is allowed (matches
    // `isCamelCase` semantics for fn names and lambda params).
    Program prog = parseStr("_myGlobal: int = 42");
    ASSERT_EQ(prog.size(), 1u);
    EXPECT_EQ(std::get<AssignStmt>(prog[0]).name, "_myGlobal");
}

TEST(ParserTest, TypedDeclRejectsBareUnderscore) {
    // A lone `_` is not a camelCase binding name (variable position has no
    // wildcard semantics, unlike tuple destructure).
    EXPECT_THROW(parseStr("_: int = 42"), std::runtime_error);
}

TEST(ParserTest, TypedDeclRejectsUnderscorePrefixSnakeBody) {
    // The `_` prefix does not exempt the body from camelCase, mirroring fn /
    // parameter / lambda enforcement.
    EXPECT_THROW(parseStr("_my_var: int = 42"), std::runtime_error);
}

TEST(ParserTest, TypedDeclRejectsScreamingSnakeWithoutNativeOrConst) {
    // Without @native or @const, SCREAMING_SNAKE_CASE is not allowed.
    EXPECT_THROW(parseStr("PI: float = 3.14"), std::runtime_error);
}

TEST(ParserTest, NativeTypedDeclAcceptsScreamingSnakeCase) {
    // @native + SCREAMING_SNAKE_CASE is the established convention for
    // built-in constants (see fn-name handling in parser_decl.cpp:130).
    Program prog = parseStr("@native @const PI: float");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    EXPECT_EQ(s.name, "PI");
    EXPECT_EQ(s.value, nullptr);
}

TEST(ParserTest, NativeTypedDeclRejectsSnakeCase) {
    // @native does not relax the casing rule beyond SCREAMING_SNAKE_CASE.
    EXPECT_THROW(parseStr("@native @const my_pi: float"), std::runtime_error);
}

TEST(ParserTest, ConstTypedDeclAcceptsScreamingSnakeCase) {
    // @const + SCREAMING_SNAKE_CASE is the established Ry convention for
    // module-level constants (docs/reference/functions.md:188-202).
    Program prog = parseStr("@const\nPI: float = 3.14");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    EXPECT_EQ(s.name, "PI");
    ASSERT_TRUE(s.type_annotation != nullptr);
    EXPECT_EQ(s.type_annotation->toString(), "float");
}

TEST(ParserTest, ConstTypedDeclRejectsSnakeCase) {
    // @const does not relax the casing rule beyond SCREAMING_SNAKE_CASE;
    // snake_case is still rejected.
    EXPECT_THROW(parseStr("@const\nmy_pi: float = 3.14"), std::runtime_error);
}

TEST(ParserTest, TypedDeclRejectsSnakeCaseInsideFunction) {
    // parseStatement is shared between top-level and block contexts; the same
    // camelCase rule must therefore apply to function-local declarations.
    EXPECT_THROW(
        parseStr("fn main() -> int:\n    snake_local: int = 1\n    return snake_local"),
        std::runtime_error);
}

TEST(ParserTest, TypedDeclAcceptsCamelCaseInsideFunction) {
    Program prog = parseStr("fn main() -> int:\n    myLocal: int = 1\n    return myLocal");
    ASSERT_EQ(prog.size(), 1u);
}

TEST(ParserTest, PascalCaseRecordRequired) {
    EXPECT_THROW(parseStr("record my_point:\n    x: int"), std::runtime_error);
}

TEST(ParserTest, PascalCaseEnumRequired) {
    EXPECT_THROW(parseStr("enum my_color:\n    Red\n    Green"), std::runtime_error);
}

TEST(ParserTest, PascalCaseEnumVariantRequired) {
    EXPECT_THROW(parseStr("enum Color:\n    red\n    Green"), std::runtime_error);
}

// ===== Explicit enum value parser tests =====

TEST(ParserTest, EnumExplicitValues) {
    Program prog = parseStr("enum HttpStatus:\n    Ok = 200\n    NotFound = 404\n    InternalError = 500");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<EnumStmt>(prog[0]));
    const auto &es = std::get<EnumStmt>(prog[0]);
    EXPECT_EQ(es.name, "HttpStatus");
    ASSERT_EQ(es.variants.size(), 3u);
    EXPECT_EQ(es.variants[0].name, "Ok");
    ASSERT_TRUE(es.variants[0].explicit_value.has_value());
    EXPECT_EQ(es.variants[0].explicit_value.value(), 200);
    EXPECT_EQ(es.variants[1].name, "NotFound");
    ASSERT_TRUE(es.variants[1].explicit_value.has_value());
    EXPECT_EQ(es.variants[1].explicit_value.value(), 404);
    EXPECT_EQ(es.variants[2].name, "InternalError");
    ASSERT_TRUE(es.variants[2].explicit_value.has_value());
    EXPECT_EQ(es.variants[2].explicit_value.value(), 500);
}

TEST(ParserTest, EnumExplicitNegativeValue) {
    Program prog = parseStr("enum Temp:\n    Cold = -10\n    Hot = 40");
    const auto &es = std::get<EnumStmt>(prog[0]);
    ASSERT_TRUE(es.variants[0].explicit_value.has_value());
    EXPECT_EQ(es.variants[0].explicit_value.value(), -10);
    ASSERT_TRUE(es.variants[1].explicit_value.has_value());
    EXPECT_EQ(es.variants[1].explicit_value.value(), 40);
}

TEST(ParserTest, EnumExplicitValueMixedError) {
    EXPECT_THROW(parseStr("enum Bad:\n    A = 1\n    B\n    C = 3"), std::runtime_error);
}

TEST(ParserTest, EnumExplicitValueOnADTError) {
    EXPECT_THROW(parseStr("enum Bad:\n    A(int) = 1\n    B = 2"), std::runtime_error);
}

// ===== Named fields in ADT enum variants =====

TEST(ParserTest, EnumNamedFields) {
    Program prog = parseStr("enum Shape:\n    Circle(radius: float)\n    Rect(width: float, height: float)\n    Point");
    ASSERT_EQ(prog.size(), 1u);
    const auto &es = std::get<EnumStmt>(prog[0]);
    EXPECT_EQ(es.name, "Shape");
    ASSERT_EQ(es.variants.size(), 3u);
    // Circle
    EXPECT_EQ(es.variants[0].name, "Circle");
    ASSERT_EQ(es.variants[0].field_types.size(), 1u);
    EXPECT_EQ(es.variants[0].field_types[0]->toString(), "float");
    ASSERT_EQ(es.variants[0].field_names.size(), 1u);
    EXPECT_EQ(es.variants[0].field_names[0], "radius");
    // Rect
    EXPECT_EQ(es.variants[1].name, "Rect");
    ASSERT_EQ(es.variants[1].field_types.size(), 2u);
    EXPECT_EQ(es.variants[1].field_types[0]->toString(), "float");
    EXPECT_EQ(es.variants[1].field_types[1]->toString(), "float");
    ASSERT_EQ(es.variants[1].field_names.size(), 2u);
    EXPECT_EQ(es.variants[1].field_names[0], "width");
    EXPECT_EQ(es.variants[1].field_names[1], "height");
    // Point (no fields)
    EXPECT_TRUE(es.variants[2].field_types.empty());
    EXPECT_TRUE(es.variants[2].field_names.empty());
}

TEST(ParserTest, EnumUnnamedFieldsRegression) {
    Program prog = parseStr("enum Shape:\n    Circle(float)\n    Rect(float, float)");
    const auto &es = std::get<EnumStmt>(prog[0]);
    ASSERT_EQ(es.variants[0].field_types.size(), 1u);
    EXPECT_TRUE(es.variants[0].field_names.empty());
    ASSERT_EQ(es.variants[1].field_types.size(), 2u);
    EXPECT_TRUE(es.variants[1].field_names.empty());
}

TEST(ParserTest, EnumMixedFieldsError) {
    EXPECT_THROW(parseStr("enum Bad:\n    Bar(x: int, float)"), std::runtime_error);
}

TEST(ParserTest, EnumDuplicateFieldNameError) {
    EXPECT_THROW(parseStr("enum Bad:\n    Bar(x: int, x: float)"), std::runtime_error);
}

TEST(ParserTest, EnumNonCamelCaseFieldError) {
    EXPECT_THROW(parseStr("enum Bad:\n    Bar(Radius: float)"), std::runtime_error);
}

TEST(ParserTest, EnumSnakeCaseFieldRejected) {
    EXPECT_THROW(parseStr("enum Bad:\n    Bar(my_radius: float)"), std::runtime_error);
}

TEST(ParserTest, EnumCamelCaseFieldAccepted) {
    Program prog = parseStr("enum Shape:\n    Circle(myRadius: float)");
    ASSERT_EQ(prog.size(), 1u);
    const auto &es = std::get<EnumStmt>(prog[0]);
    ASSERT_EQ(es.variants.size(), 1u);
    ASSERT_EQ(es.variants[0].field_names.size(), 1u);
    EXPECT_EQ(es.variants[0].field_names[0], "myRadius");
}

TEST(ParserTest, EnumNamedFieldsGeneric) {
    Program prog = parseStr("enum Option<T>:\n    Some(value: T)\n    None");
    const auto &es = std::get<EnumStmt>(prog[0]);
    EXPECT_EQ(es.variants[0].name, "Some");
    ASSERT_EQ(es.variants[0].field_names.size(), 1u);
    EXPECT_EQ(es.variants[0].field_names[0], "value");
    ASSERT_EQ(es.variants[0].field_types.size(), 1u);
    EXPECT_EQ(es.variants[0].field_types[0]->toString(), "T");
}

TEST(ParserTest, PascalCaseTypeAliasRequired) {
    EXPECT_THROW(parseStr("type my_int = int"), std::runtime_error);
}

TEST(ParserTest, TypeAliasFnType) {
    Program prog = parseStr("type Callback = fn(int, int) -> int");
    auto &ta = std::get<TypeAliasStmt>(prog[0]);
    EXPECT_EQ(ta.name, "Callback");
    EXPECT_EQ(ta.target_type->toString(), "fn(int, int) -> int");
}

TEST(ParserTest, TypeAliasCanonicalFnType) {
    Program prog = parseStr("type Callback = fn(int, int) -> int");
    auto &ta = std::get<TypeAliasStmt>(prog[0]);
    EXPECT_EQ(ta.name, "Callback");
    EXPECT_EQ(ta.target_type->toString(), "fn(int, int) -> int");
}

TEST(ParserTest, CamelCaseForLoopVariableAccepted) {
    Program prog = parseStr("for myVar in xs:\n    print(myVar)");
    ASSERT_EQ(prog.size(), 1u);
}

TEST(ParserTest, SnakeCaseForLoopVariableRejected) {
    EXPECT_THROW(parseStr("for my_var in xs:\n    print(my_var)"), std::runtime_error);
}

TEST(ParserTest, CamelCaseParamAccepted) {
    Program prog = parseStr("fn add(myNum: int) -> int:\n    return myNum");
    ASSERT_EQ(prog.size(), 1u);
    auto &function = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    ASSERT_EQ(function.params.size(), 1u);
    EXPECT_EQ(function.params[0].name, "myNum");
}

TEST(ParserTest, SnakeCaseParamRejected) {
    EXPECT_THROW(parseStr("fn add(my_num: int) -> int:\n    return my_num"), std::runtime_error);
}

TEST(ParserTest, BangSuffixParamRejected) {
    EXPECT_THROW(parseStr("fn add(x!: int) -> int:\n    return x!"), std::runtime_error);
}

TEST(ParserTest, CamelCaseRecordFieldAccepted) {
    Program prog = parseStr("record Point:\n    myX: int\n    myY: int");
    ASSERT_EQ(prog.size(), 1u);
    const auto &rs = std::get<RecordStmt>(prog[0]);
    ASSERT_EQ(rs.fields.size(), 2u);
    EXPECT_EQ(rs.fields[0].name, "myX");
    EXPECT_EQ(rs.fields[1].name, "myY");
}

TEST(ParserTest, SnakeCaseRecordFieldRejected) {
    EXPECT_THROW(parseStr("record Point:\n    my_x: int"), std::runtime_error);
}

// ===== trailing block syntax =====

TEST(ParserTest, TrailingBlockNoArgs) {
    Program prog = parseStr("foo():\n    bar()");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<CallStmt>(prog[0]));
    const auto &s = std::get<CallStmt>(prog[0]);
    EXPECT_EQ(s.callee, "foo");
    ASSERT_EQ(s.args.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<LambdaExpr>>(s.args[0]->data));
}

TEST(ParserTest, TrailingBlockWithArgs) {
    Program prog = parseStr("foo(\"a\", 1):\n    bar()");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<CallStmt>(prog[0]));
    const auto &s = std::get<CallStmt>(prog[0]);
    EXPECT_EQ(s.callee, "foo");
    ASSERT_EQ(s.args.size(), 3u);
    ASSERT_TRUE(std::holds_alternative<StringExpr>(s.args[0]->data));
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(s.args[1]->data));
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<LambdaExpr>>(s.args[2]->data));
}

TEST(ParserTest, TrailingBlockUFCS) {
    Program prog = parseStr("x.each():\n    print(1)");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<CallStmt>(prog[0]));
    const auto &s = std::get<CallStmt>(prog[0]);
    EXPECT_EQ(s.callee, "each");
    // First arg is the UFCS receiver (x), second is the trailing lambda
    ASSERT_EQ(s.args.size(), 2u);
    ASSERT_TRUE(std::holds_alternative<VariableExpr>(s.args[0]->data));
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<LambdaExpr>>(s.args[1]->data));
}

// ===== @native fn tests =====

TEST(ParserTest, NativeFnDeclaration) {
    Program prog = parseStr("@native\nfn contains(s: str, sub: str) -> bool\n");
    ASSERT_EQ(prog.size(), 1u);
    auto &fs = std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(fs->name, "contains");
    EXPECT_EQ(fs->params.size(), 2u);
    EXPECT_EQ(fs->return_type->toString(), "bool");
    EXPECT_TRUE(fs->body.empty());
    ASSERT_EQ(fs->directives.size(), 1u);
    EXPECT_EQ(fs->directives[0].name, "native");
}

TEST(ParserTest, NativeFnOperatorDeclaration) {
    Program prog = parseStr("@native\nfn operator+(a: int, b: int) -> int\n");
    ASSERT_EQ(prog.size(), 1u);
    auto &fs = std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(fs->name, "operator+");
    EXPECT_TRUE(fs->is_operator);
    EXPECT_TRUE(fs->body.empty());
    ASSERT_EQ(fs->directives.size(), 1u);
    EXPECT_EQ(fs->directives[0].name, "native");
}

TEST(ParserTest, EllipsisTopLevel) {
    Program prog = parseStr("...");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<EllipsisStmt>(prog[0]));
}

TEST(ParserTest, EllipsisInFnBody) {
    Program prog = parseStr("fn stub():\n    ...\n");
    ASSERT_EQ(prog.size(), 1u);
    auto &fs = std::get<std::unique_ptr<FnStmt>>(prog[0]);
    ASSERT_EQ(fs->body.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<EllipsisStmt>(fs->body[0]));
}

TEST(ParserTest, EllipsisInIfBody) {
    Program prog = parseStr("if true:\n    ...\n");
    ASSERT_EQ(prog.size(), 1u);
    auto &is = std::get<std::unique_ptr<IfStmt>>(prog[0]);
    ASSERT_EQ(is->branch.body.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<EllipsisStmt>(is->branch.body[0]));
}

TEST(ParserTest, NativeFnWithColonError) {
    EXPECT_THROW({
        parseStr("@native\nfn bad() -> int:\n    return 1\n");
    }, std::runtime_error);
}

// ===== @public directive tests (#1545) =====
// Parser-level acceptance only — visibility semantics are deferred to #1544.

TEST(ParserTest, PublicOnFn) {
    Program prog = parseStr("@public\nfn answer() -> int:\n    return 42\n");
    ASSERT_EQ(prog.size(), 1u);
    auto &fs = std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(fs->name, "answer");
    ASSERT_EQ(fs->directives.size(), 1u);
    EXPECT_EQ(fs->directives[0].name, "public");
}

TEST(ParserTest, PublicOnLet) {
    Program prog = parseStr("@public\nx = 42\n");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<AssignStmt>(prog[0]));
    const auto &s = std::get<AssignStmt>(prog[0]);
    EXPECT_EQ(s.name, "x");
    ASSERT_EQ(s.directives.size(), 1u);
    EXPECT_EQ(s.directives[0].name, "public");
}

TEST(ParserTest, PublicOnRecord) {
    Program prog = parseStr("@public\nrecord Foo:\n    x: int\n");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<RecordStmt>(prog[0]));
    const auto &ts = std::get<RecordStmt>(prog[0]);
    EXPECT_EQ(ts.name, "Foo");
    ASSERT_EQ(ts.directives.size(), 1u);
    EXPECT_EQ(ts.directives[0].name, "public");
}

// ===== @directive (DirectiveDefStmt) rejection tests (#708) =====

TEST(DirectiveDefParserTest, RejectsSnakeCaseDirectiveName) {
    EXPECT_THROW(parseStr("@directive(target=[\"function\"])\nfn my_directive()\n"),
                 DiagnosticError);
}

TEST(DirectiveDefParserTest, AcceptsCamelCaseDirectiveName) {
    Program prog = parseStr("@directive(target=[\"function\"])\nfn myDirective()\n");
    ASSERT_EQ(prog.size(), 1u);
}

TEST(DirectiveDefParserTest, RejectsSnakeCaseDirectiveParam) {
    EXPECT_THROW(parseStr("@directive(target=[\"function\"])\nfn d(my_level: str = \"info\")\n"),
                 DiagnosticError);
}

TEST(DirectiveDefParserTest, AcceptsCamelCaseDirectiveParam) {
    Program prog = parseStr("@directive(target=[\"function\"])\nfn d(myLevel: str = \"info\")\n");
    ASSERT_EQ(prog.size(), 1u);
}

TEST(DirectiveDefParserTest, RejectsLetAfterDirectiveAnnotation) {
    EXPECT_THROW(parseStr("@directive(target=[\"function\"])\nx = 1\n"),
                 DiagnosticError);
}

TEST(DirectiveDefParserTest, RejectsRecordAfterDirectiveAnnotation) {
    EXPECT_THROW(parseStr("@directive(target=[\"function\"])\nrecord Foo:\n    x: int\n"),
                 DiagnosticError);
}

TEST(DirectiveDefParserTest, RejectsAsyncFnAfterDirectiveAnnotation) {
    EXPECT_THROW(parseStr("@directive(target=[\"function\"])\nasync fn it():\n    ...\n"),
                 DiagnosticError);
}

TEST(DirectiveDefParserTest, RejectsBodyOnDirectiveDef) {
    EXPECT_THROW(parseStr("@directive(target=[\"function\"])\nfn it(d: str):\n    d\n"),
                 DiagnosticError);
}

TEST(DirectiveDefParserTest, RejectsReturnTypeOnDirectiveDef) {
    EXPECT_THROW(parseStr("@directive(target=[\"function\"])\nfn it() -> int\n"),
                 DiagnosticError);
}

TEST(DirectiveDefParserTest, RejectsMissingTargetArgument) {
    EXPECT_THROW(parseStr("@directive()\nfn it()\n"),
                 DiagnosticError);
}

// #1408: `stage` is no longer a valid argument and is rejected as unknown.
TEST(DirectiveDefParserTest, RejectsStageArgument) {
    EXPECT_THROW(parseStr("@directive(target=[\"function\"], stage=\"compile\")\nfn it()\n"),
                 DiagnosticError);
}

TEST(DirectiveDefParserTest, RejectsUnknownTargetValue) {
    EXPECT_THROW(parseStr("@directive(target=[\"banana\"])\nfn it()\n"),
                 DiagnosticError);
}

TEST(DirectiveDefParserTest, RejectsNonStringTarget) {
    EXPECT_THROW(parseStr("@directive(target=42)\nfn it()\n"),
                 DiagnosticError);
}

TEST(DirectiveDefParserTest, RejectsNonStringElementsInTargetList) {
    EXPECT_THROW(parseStr("@directive(target=[1, 2])\nfn it()\n"),
                 DiagnosticError);
}

TEST(DirectiveDefParserTest, RejectsEmptyTargetList) {
    EXPECT_THROW(parseStr("@directive(target=[])\nfn it()\n"),
                 DiagnosticError);
}

TEST(DirectiveDefParserTest, RejectsDuplicateTargetInList) {
    EXPECT_THROW(parseStr("@directive(target=[\"function\", \"function\"])\nfn it()\n"),
                 DiagnosticError);
}

TEST(DirectiveDefParserTest, RejectsDuplicateTargetArgument) {
    EXPECT_THROW(parseStr(
        "@directive(target=[\"function\"], target=[\"record\"])\nfn it()\n"),
        DiagnosticError);
}

TEST(DirectiveDefParserTest, RejectsUnknownNamedArgument) {
    EXPECT_THROW(parseStr("@directive(target=[\"function\"], extra=\"x\")\nfn it()\n"),
                 DiagnosticError);
}

TEST(DirectiveDefParserTest, RejectsPositionalArgumentForDirectiveAnnotation) {
    EXPECT_THROW(parseStr("@directive(\"function\")\nfn it()\n"),
                 DiagnosticError);
}

TEST(DirectiveDefParserTest, RejectsCombiningDirectiveWithDeprecatedBefore) {
    EXPECT_THROW(parseStr("@deprecated\n@directive(target=[\"function\"])\nfn it()\n"),
                 DiagnosticError);
}

TEST(DirectiveDefParserTest, RejectsCombiningDirectiveWithDeprecatedAfter) {
    EXPECT_THROW(parseStr("@directive(target=[\"function\"])\n@deprecated\nfn it()\n"),
                 DiagnosticError);
}

// ===== @directive acceptance tests (#708) =====

TEST(DirectiveDefParserTest, AcceptsBasicDirectiveDef) {
    Program prog = parseStr("@directive(target=[\"function\"])\nfn it(description: str)\n");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<DirectiveDefStmt>(prog[0]));
    const auto &d = std::get<DirectiveDefStmt>(prog[0]);
    EXPECT_EQ(d.name, "it");
    ASSERT_EQ(d.targets.size(), 1u);
    EXPECT_EQ(d.targets[0], "function");
    ASSERT_EQ(d.params.size(), 1u);
    EXPECT_EQ(d.params[0].name, "description");
    ASSERT_TRUE(d.params[0].type != nullptr);
    EXPECT_EQ(d.params[0].type->toString(), "str");
}

TEST(DirectiveDefParserTest, AcceptsBareStringTargetSugar) {
    Program prog = parseStr("@directive(target=\"function\")\nfn it(d: str)\n");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<DirectiveDefStmt>(prog[0]));
    const auto &d = std::get<DirectiveDefStmt>(prog[0]);
    ASSERT_EQ(d.targets.size(), 1u);
    EXPECT_EQ(d.targets[0], "function");
}

TEST(DirectiveDefParserTest, AcceptsMultipleTargets) {
    Program prog = parseStr("@directive(target=[\"function\", \"record\"])\nfn cacheable()\n");
    ASSERT_EQ(prog.size(), 1u);
    const auto &d = std::get<DirectiveDefStmt>(prog[0]);
    ASSERT_EQ(d.targets.size(), 2u);
    EXPECT_EQ(d.targets[0], "function");
    EXPECT_EQ(d.targets[1], "record");
    EXPECT_TRUE(d.params.empty());
}

TEST(DirectiveDefParserTest, AcceptsConstAsName) {
    Program prog = parseStr("@directive(target=[\"function\"])\nfn const()\n");
    ASSERT_EQ(prog.size(), 1u);
    const auto &d = std::get<DirectiveDefStmt>(prog[0]);
    EXPECT_EQ(d.name, "const");
}

TEST(DirectiveDefParserTest, AcceptsParamWithDefaultValue) {
    Program prog = parseStr("@directive(target=[\"function\"])\nfn inline(mode: str = \"always\")\n");
    ASSERT_EQ(prog.size(), 1u);
    const auto &d = std::get<DirectiveDefStmt>(prog[0]);
    ASSERT_EQ(d.params.size(), 1u);
    EXPECT_EQ(d.params[0].name, "mode");
    EXPECT_NE(d.params[0].default_value, nullptr);
}

TEST(DirectiveDefParserTest, AcceptsStatementTarget) {
    Program prog = parseStr("@directive(target=[\"statement\"])\nfn debug()\n");
    ASSERT_EQ(prog.size(), 1u);
    const auto &d = std::get<DirectiveDefStmt>(prog[0]);
    ASSERT_EQ(d.targets.size(), 1u);
    EXPECT_EQ(d.targets[0], "statement");
}

TEST(DirectiveDefParserTest, AcceptsForTarget) {
    Program prog = parseStr("@directive(target=[\"for\"])\nfn parallel()\n");
    ASSERT_EQ(prog.size(), 1u);
    const auto &d = std::get<DirectiveDefStmt>(prog[0]);
    ASSERT_EQ(d.targets.size(), 1u);
    EXPECT_EQ(d.targets[0], "for");
}

// ===== @directive + @public combination tests (#1546) =====

TEST(DirectiveDefParserTest, AcceptsPublicBeforeDirective) {
    Program prog = parseStr("@public\n@directive(target=[\"function\"])\nfn it(description: str)\n");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<DirectiveDefStmt>(prog[0]));
    const auto &d = std::get<DirectiveDefStmt>(prog[0]);
    EXPECT_EQ(d.name, "it");
    ASSERT_EQ(d.directives.size(), 1u);
    EXPECT_EQ(d.directives[0].name, "public");
}

TEST(DirectiveDefParserTest, AcceptsPublicAfterDirective) {
    Program prog = parseStr("@directive(target=[\"function\"])\n@public\nfn it(description: str)\n");
    ASSERT_EQ(prog.size(), 1u);
    const auto &d = std::get<DirectiveDefStmt>(prog[0]);
    EXPECT_EQ(d.name, "it");
    ASSERT_EQ(d.directives.size(), 1u);
    EXPECT_EQ(d.directives[0].name, "public");
}

TEST(DirectiveDefParserTest, AcceptsBareDirectiveHasEmptyDirectives) {
    Program prog = parseStr("@directive(target=[\"function\"])\nfn it(d: str)\n");
    ASSERT_EQ(prog.size(), 1u);
    const auto &d = std::get<DirectiveDefStmt>(prog[0]);
    EXPECT_TRUE(d.directives.empty());
}

TEST(DirectiveDefParserTest, RejectsCombiningDirectiveWithInlineBefore) {
    EXPECT_THROW(parseStr("@inline\n@directive(target=[\"function\"])\nfn it()\n"),
                 DiagnosticError);
}

TEST(DirectiveDefParserTest, RejectsCombiningDirectiveWithInlineAfter) {
    EXPECT_THROW(parseStr("@directive(target=[\"function\"])\n@inline\nfn it()\n"),
                 DiagnosticError);
}

// ===== OR pattern binding check =====

TEST(ParserTest, OrPatternRejectsVariableBinding) {
    EXPECT_THROW({
        parseStr("case x:\n    a | b:\n        print(a)\n");
    }, std::runtime_error);
}

TEST(ParserTest, OrPatternRejectsSomeBinding) {
    EXPECT_THROW({
        parseStr("case x:\n    Some(a) | Some(b):\n        print(a)\n");
    }, std::runtime_error);
}

TEST(ParserTest, OrPatternRejectsOkBinding) {
    EXPECT_THROW({
        parseStr("case x:\n    Ok(a) | Ok(b):\n        print(a)\n");
    }, std::runtime_error);
}

TEST(ParserTest, OrPatternRejectsErrBinding) {
    EXPECT_THROW({
        parseStr("case x:\n    Err(a) | Err(b):\n        print(a)\n");
    }, std::runtime_error);
}

TEST(ParserTest, OrPatternRejectsOkAsAlternative) {
    EXPECT_THROW({
        parseStr("case x:\n    1 | Ok(a):\n        print(a)\n");
    }, std::runtime_error);
}

TEST(ParserTest, OrPatternRejectsErrAsAlternative) {
    EXPECT_THROW({
        parseStr("case x:\n    1 | Err(e):\n        print(e)\n");
    }, std::runtime_error);
}

TEST(ParserTest, OrPatternRejectsEnumConstructorBinding) {
    EXPECT_THROW({
        parseStr("case x:\n    Foo::Bar(a) | Foo::Baz(b):\n        print(a)\n");
    }, std::runtime_error);
}

TEST(ParserTest, OrPatternRejectsNestedTupleBindingInEnumConstructor) {
    // Nested tuple variable bindings inside constructor payloads must also be
    // rejected in OR patterns, exercising the recursive patternHasBinding path.
    EXPECT_THROW({
        parseStr("case x:\n    Foo::Bar((a, b)) | Foo::Baz((c, d)):\n        print(a)\n");
    }, std::runtime_error);
}

TEST(ParserTest, OrPatternAllowsWildcardBindings) {
    EXPECT_NO_THROW({
        parseStr("case x:\n    Ok(_) | Err(_):\n        print(\"done\")\n");
    });
    EXPECT_NO_THROW({
        parseStr("case x:\n    Some(_) | None:\n        print(\"done\")\n");
    });
    EXPECT_NO_THROW({
        parseStr("case x:\n    Foo::Bar(_) | Foo::Baz(_):\n        print(\"done\")\n");
    });
}

TEST(ParserTest, DeepNestingThrows) {
    // 300 nested parentheses should exceed MAX_RECURSION_DEPTH (256)
    std::string deep = "x = " + std::string(300, '(') + "1" + std::string(300, ')');
    EXPECT_THROW(parseStr(deep), DiagnosticError);
}

TEST(ParserTest, ModerateNestingSucceeds) {
    // 50 nested parentheses in an assignment expression should be fine
    std::string moderate = "x = " + std::string(50, '(') + "1" + std::string(50, ')');
    EXPECT_NO_THROW(parseStr(moderate));
}

// ===== Default arguments =====

TEST(ParserTest, DefaultArgBasic) {
    Program prog = parseStr("fn f(x: int, y: int = 10) -> int:\n    return x + y");
    const auto &function = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    ASSERT_EQ(function.params.size(), 2u);
    EXPECT_EQ(function.params[0].name, "x");
    EXPECT_FALSE(function.params[0].default_value);
    EXPECT_EQ(function.params[1].name, "y");
    EXPECT_TRUE(function.params[1].default_value);
}

TEST(ParserTest, DefaultArgMultiple) {
    Program prog = parseStr("fn f(a: int, b: int = 1, c: int = 2) -> int:\n    return a");
    const auto &function = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    ASSERT_EQ(function.params.size(), 3u);
    EXPECT_FALSE(function.params[0].default_value);
    EXPECT_TRUE(function.params[1].default_value);
    EXPECT_TRUE(function.params[2].default_value);
}

TEST(ParserTest, DefaultArgNonTrailingError) {
    // default arg followed by non-default arg is a parse error
    EXPECT_THROW(parseStr("fn f(a: int = 0, b: int) -> int:\n    return a"), std::runtime_error);
}

TEST(ParserTest, DefaultArgNoTypeError) {
    // default arg without explicit type annotation is a parse error
    EXPECT_THROW(parseStr("fn f(x = 10) -> int:\n    return x"), std::runtime_error);
}

TEST(ParserTest, DefaultArgLambdaError) {
    // default args in lambda are not supported
    EXPECT_THROW(parseStr("f = (x: int = 10) => x"), std::runtime_error);
}

TEST(ParserTest, RejectOldColonSingleExprLambda) {
    // old `:` syntax for single-expression lambdas is rejected with guidance
    EXPECT_THROW(parseStr("f = (x: int): x + 1"), std::runtime_error);
    EXPECT_THROW(parseStr("f = (): 42"), std::runtime_error);
    EXPECT_THROW(parseStr("f = (x: int) -> int: x + 1"), std::runtime_error);
}

TEST(ParserTest, RejectAnonymousFunctionLambda) {
    EXPECT_THROW(parseStr("f = fn(x: int) => x + 1"), std::runtime_error);
}

TEST(ParserTest, RejectAnonymousFunctionLambdaWithReturnType) {
    EXPECT_THROW(parseStr("f = fn(x: int) -> int => x + 1"), std::runtime_error);
}

TEST(ParserTest, RejectAnonymousFunctionLambdaBlock) {
    EXPECT_THROW(parseStr("f = fn(x: int):\n    return x + 1"), std::runtime_error);
}

TEST(ParserTest, RejectAnonymousFunctionLambdaNoParams) {
    EXPECT_THROW(parseStr("f = fn() => 42"), std::runtime_error);
    EXPECT_THROW(parseStr("f = fn() => 42"), std::runtime_error);
}

TEST(ParserTest, LambdaParamRejectsSnakeCase) {
    EXPECT_THROW(parseStr("f = (my_x, my_y) => my_x + my_y"), std::runtime_error);
    EXPECT_THROW(parseStr("f = (my_x: int) -> int => my_x + 1"), std::runtime_error);
    EXPECT_THROW(parseStr("f = (my_x: int):\n    return my_x + 1\n"), std::runtime_error);
}

TEST(ParserTest, LambdaParamAcceptsCamelCase) {
    Program prog = parseStr("f = (myX, myY) => myX + myY");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<LambdaExpr>>(s.value->data));
    const auto &lambda = *std::get<std::unique_ptr<LambdaExpr>>(s.value->data);
    ASSERT_EQ(lambda.params.size(), 2u);
    EXPECT_EQ(lambda.params[0].name, "myX");
    EXPECT_EQ(lambda.params[1].name, "myY");
}

TEST(ParserTest, LambdaParamAcceptsUnderscorePrefix) {
    // _camelCase is module-private convention and must be accepted
    Program prog = parseStr("f = (_a, _b) => _a + _b");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<LambdaExpr>>(s.value->data));
    const auto &lambda = *std::get<std::unique_ptr<LambdaExpr>>(s.value->data);
    ASSERT_EQ(lambda.params.size(), 2u);
    EXPECT_EQ(lambda.params[0].name, "_a");
    EXPECT_EQ(lambda.params[1].name, "_b");
}

TEST(ParserTest, NestedLambdaRejectsInnerSnakeCase) {
    // Outer lambda is camelCase-valid and committed; inner lambda's snake_case
    // param must still be rejected (validates prev_committed save/restore).
    EXPECT_THROW(parseStr("f = (myX) => (my_y) => myX + my_y"), std::runtime_error);
}

TEST(ParserTest, NestedLambdaAcceptsCamelCase) {
    Program prog = parseStr("f = (myX) => (myY) => myX + myY");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<LambdaExpr>>(s.value->data));
    const auto &outer = *std::get<std::unique_ptr<LambdaExpr>>(s.value->data);
    ASSERT_EQ(outer.params.size(), 1u);
    EXPECT_EQ(outer.params[0].name, "myX");
    ASSERT_TRUE(outer.expr_body != nullptr);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<LambdaExpr>>(outer.expr_body->data));
    const auto &inner = *std::get<std::unique_ptr<LambdaExpr>>(outer.expr_body->data);
    ASSERT_EQ(inner.params.size(), 1u);
    EXPECT_EQ(inner.params[0].name, "myY");
}

// ===== Bare-paren-omitted single-param lambda (#1572) =====

TEST(ParserTest, BareLambdaSingleParamAccepts) {
    Program prog = parseStr("f = s => s + 1");
    ASSERT_EQ(prog.size(), 1u);
    const auto &assign = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<LambdaExpr>>(assign.value->data));
    const auto &lambda = *std::get<std::unique_ptr<LambdaExpr>>(assign.value->data);
    ASSERT_EQ(lambda.params.size(), 1u);
    EXPECT_EQ(lambda.params[0].name, "s");
    EXPECT_EQ(lambda.return_type, nullptr);
    ASSERT_TRUE(lambda.expr_body != nullptr);
    EXPECT_TRUE(lambda.body.empty());
    EXPECT_TRUE(std::holds_alternative<std::unique_ptr<BinaryExpr>>(lambda.expr_body->data));
}

TEST(ParserTest, BareLambdaPreservesIfExpressionWithBareIdentCond) {
    // The bare lambda dispatch must not preempt `if cond => then else else`
    // when `cond` is a bare ident — this is a positive sibling test for the
    // `in_if_cond_` flag.
    Program prog = parseStr("f = (b: bool) => if b => 1 else 2");
    ASSERT_EQ(prog.size(), 1u);
    const auto &assign = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<LambdaExpr>>(assign.value->data));
    const auto &outer = *std::get<std::unique_ptr<LambdaExpr>>(assign.value->data);
    ASSERT_TRUE(outer.expr_body != nullptr);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<IfExpr>>(outer.expr_body->data));
    const auto &ifExpr = *std::get<std::unique_ptr<IfExpr>>(outer.expr_body->data);
    ASSERT_TRUE(ifExpr.condition != nullptr);
    EXPECT_TRUE(std::holds_alternative<VariableExpr>(ifExpr.condition->data));
}

TEST(ParserTest, BareLambdaRejectsSnakeCase) {
    EXPECT_THROW(parseStr("f = my_x => my_x"), std::runtime_error);
}

TEST(ParserTest, BareLambdaRejectsMultiArg) {
    // Multi-arg paren-omission is forbidden (tuple destructure ambiguity).
    EXPECT_THROW(parseStr("f = s, t => s + t"), std::runtime_error);
}

TEST(ParserTest, BareLambdaRejectsAnnotatedParam) {
    // `s: str => s` is parsed as a typed module-global declaration, not a lambda.
    EXPECT_THROW(parseStr("f = s: str => s"), std::runtime_error);
}

TEST(ParserTest, BareLambdaRejectsBlockBody) {
    // Block body requires paren wrapping; bare form is single-expression only.
    EXPECT_THROW(parseStr("f = s =>\n    return s\n"), std::runtime_error);
}

TEST(ParserTest, BareLambdaNestedRightAssoc) {
    Program prog = parseStr("f = x => y => x + y");
    ASSERT_EQ(prog.size(), 1u);
    const auto &assign = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<LambdaExpr>>(assign.value->data));
    const auto &outer = *std::get<std::unique_ptr<LambdaExpr>>(assign.value->data);
    ASSERT_EQ(outer.params.size(), 1u);
    EXPECT_EQ(outer.params[0].name, "x");
    ASSERT_TRUE(outer.expr_body != nullptr);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<LambdaExpr>>(outer.expr_body->data));
    const auto &inner = *std::get<std::unique_ptr<LambdaExpr>>(outer.expr_body->data);
    ASSERT_EQ(inner.params.size(), 1u);
    EXPECT_EQ(inner.params[0].name, "y");
}

TEST(ParserTest, BareLambdaInIfThenElse) {
    // Bare lambda inside if-expression then/else arms must work.
    Program prog = parseStr("f = (b: bool) => if b => x => x else y => y * 2");
    ASSERT_EQ(prog.size(), 1u);
    const auto &assign = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<LambdaExpr>>(assign.value->data));
    const auto &outer = *std::get<std::unique_ptr<LambdaExpr>>(assign.value->data);
    ASSERT_TRUE(outer.expr_body != nullptr);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<IfExpr>>(outer.expr_body->data));
    const auto &ifExpr = *std::get<std::unique_ptr<IfExpr>>(outer.expr_body->data);
    ASSERT_TRUE(ifExpr.then_value != nullptr);
    ASSERT_TRUE(ifExpr.else_value != nullptr);
    EXPECT_TRUE(std::holds_alternative<std::unique_ptr<LambdaExpr>>(ifExpr.then_value->data));
    EXPECT_TRUE(std::holds_alternative<std::unique_ptr<LambdaExpr>>(ifExpr.else_value->data));
}

TEST(ParserTest, BareLambdaBodyDoesNotInheritAsyncContext) {
    // `await` inside a bare lambda body within async fn must be rejected
    // (lambda body parses with in_async_fn_ = false).
    EXPECT_THROW(
        parseStr("async fn outer() -> int:\n    f = s => await s\n    return 1\n"),
        std::runtime_error);
}

// ===== Cast expression with generic types (#490) =====

TEST(ParserTest, CastSimpleType) {
    Program prog = parseStr("x = 42 as float");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CastExpr>>(s.value->data));
    const auto &cast = *std::get<std::unique_ptr<CastExpr>>(s.value->data);
    EXPECT_EQ(cast.target_type->toString(), "float");
}

TEST(ParserTest, CastGenericType) {
    Program prog = parseStr("x = y as Option<int>");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CastExpr>>(s.value->data));
    const auto &cast = *std::get<std::unique_ptr<CastExpr>>(s.value->data);
    EXPECT_EQ(cast.target_type->toString(), "Option<int>");
}

TEST(ParserTest, CastGenericTwoParams) {
    Program prog = parseStr("x = y as Map<str, int>");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CastExpr>>(s.value->data));
    const auto &cast = *std::get<std::unique_ptr<CastExpr>>(s.value->data);
    EXPECT_EQ(cast.target_type->toString(), "Map<str, int>");
}

TEST(ParserTest, CastFollowedByComparison) {
    Program prog = parseStr("x = y as float > 3.0");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<BinaryExpr>>(s.value->data));
    const auto &bin = *std::get<std::unique_ptr<BinaryExpr>>(s.value->data);
    EXPECT_EQ(bin.op, ">");
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CastExpr>>(bin.lhs->data));
    const auto &cast = *std::get<std::unique_ptr<CastExpr>>(bin.lhs->data);
    EXPECT_EQ(cast.target_type->toString(), "float");
}

TEST(ParserTest, CastChained) {
    Program prog = parseStr("x = 42 as float as str");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CastExpr>>(s.value->data));
    const auto &outer = *std::get<std::unique_ptr<CastExpr>>(s.value->data);
    EXPECT_EQ(outer.target_type->toString(), "str");
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CastExpr>>(outer.value->data));
    const auto &inner = *std::get<std::unique_ptr<CastExpr>>(outer.value->data);
    EXPECT_EQ(inner.target_type->toString(), "float");
}

// ===== case <subject> 式パーサーテスト =====

TEST(ParserTest, CaseExprBasic) {
    Program prog = parseStr("x = case y:\n    1 : 10\n    _ : 0");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<AssignStmt>(prog[0]));
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CaseExpr>>(s.value->data));
    const auto &me = *std::get<std::unique_ptr<CaseExpr>>(s.value->data);
    ASSERT_EQ(me.arms.size(), 2u);
    EXPECT_TRUE(std::holds_alternative<LiteralPattern>(me.arms[0].pattern));
    EXPECT_TRUE(std::holds_alternative<WildcardPattern>(me.arms[1].pattern));
}

TEST(ParserTest, CaseExprWithGuard) {
    Program prog = parseStr("x = case y:\n    n if n > 0 : n\n    _ : 0");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    const auto &me = *std::get<std::unique_ptr<CaseExpr>>(s.value->data);
    ASSERT_EQ(me.arms.size(), 2u);
    EXPECT_TRUE(std::holds_alternative<VariablePattern>(me.arms[0].pattern));
    EXPECT_TRUE(me.arms[0].guard != nullptr);
    EXPECT_TRUE(me.arms[1].guard == nullptr);
}

TEST(ParserTest, CaseExprOrPattern) {
    Program prog = parseStr("x = case y:\n    1 | 2 | 3 : 10\n    _ : 0");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    const auto &me = *std::get<std::unique_ptr<CaseExpr>>(s.value->data);
    ASSERT_EQ(me.arms.size(), 2u);
    EXPECT_TRUE(std::holds_alternative<std::unique_ptr<OrPattern>>(me.arms[0].pattern));
}

TEST(ParserTest, CaseExprEmptyThrows) {
    EXPECT_THROW(parseStr("x = case y:\n"), std::runtime_error);
}

TEST(ParserTest, CaseCondExprFatArrowRejected) {
    EXPECT_THROW(parseStr("x = case:\n    true => 1\n    _ : 0"),
                 std::runtime_error);
}

TEST(ParserTest, CaseCondExprWildcardFatArrowRejected) {
    EXPECT_THROW(parseStr("x = case:\n    true : 1\n    _ => 0"),
                 std::runtime_error);
}

TEST(ParserTest, CaseExprFatArrowRejected) {
    EXPECT_THROW(parseStr("x = case y:\n    1 => 10\n    _ : 0"),
                 std::runtime_error);
}

TEST(ParserTest, IntLiteralInt64Max) {
    // INT64_MAX (9223372036854775807) should parse successfully
    Program prog = parseStr("x = 9223372036854775807");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    const auto &n = std::get<NumberExpr>(s.value->data);
    EXPECT_EQ(n.value, INT64_MAX);
}

TEST(ParserTest, IntLiteralInt64MaxPlus1AcceptedAsBitPattern) {
    // After #807 the parser accepts up to UINT64_MAX by storing the value as
    // a bit pattern in int64_t. INT64_MAX+1 is stored as INT64_MIN; codegen
    // is responsible for rejecting it when the target type is i64 or bare
    // int (see test_codegen.cpp).
    Program prog = parseStr("x = 9223372036854775808");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    const auto &n = std::get<NumberExpr>(s.value->data);
    EXPECT_EQ(static_cast<uint64_t>(n.value),
              static_cast<uint64_t>(INT64_MAX) + 1);
    EXPECT_TRUE(n.suffix.empty());
}

TEST(ParserTest, IntLiteralU64MaxSuffixed) {
    // #807: 18446744073709551615u64 must parse without error and store the
    // bit pattern in NumberExpr.value.
    Program prog = parseStr("x = 18446744073709551615u64");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    const auto &n = std::get<NumberExpr>(s.value->data);
    EXPECT_EQ(n.suffix, "u64");
    EXPECT_EQ(static_cast<uint64_t>(n.value), UINT64_MAX);
}

TEST(ParserTest, IntLiteralU64MaxHexSuffixed) {
    Program prog = parseStr("x = 0xFFFFFFFFFFFFFFFFu64");
    const auto &n = std::get<NumberExpr>(std::get<AssignStmt>(prog[0]).value->data);
    EXPECT_EQ(n.suffix, "u64");
    EXPECT_EQ(static_cast<uint64_t>(n.value), UINT64_MAX);
}

TEST(ParserTest, IntLiteralUint64OverflowThrows) {
    // Magnitudes strictly greater than UINT64_MAX must still be rejected at
    // parse time (strtoull returns ERANGE).
    try {
        parseStr("x = 18446744073709551616");  // UINT64_MAX + 1
        FAIL() << "expected exception";
    } catch (const DiagnosticError &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("integer literal out of range"), std::string::npos);
    }
}

TEST(ParserTest, FloatLiteralScientificBasic) {
    Program prog = parseStr("x = 1e10");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<FloatExpr>(s.value->data));
    const auto &f = std::get<FloatExpr>(s.value->data);
    EXPECT_DOUBLE_EQ(f.value, 1e10);
    EXPECT_TRUE(f.suffix.empty());
}

TEST(ParserTest, FloatLiteralScientificNegExponent) {
    Program prog = parseStr("x = 1.5e-3");
    const auto &f = std::get<FloatExpr>(std::get<AssignStmt>(prog[0]).value->data);
    EXPECT_DOUBLE_EQ(f.value, 1.5e-3);
}

TEST(ParserTest, FloatLiteralScientificUppercaseE) {
    Program prog = parseStr("x = 2.5E+2");
    const auto &f = std::get<FloatExpr>(std::get<AssignStmt>(prog[0]).value->data);
    EXPECT_DOUBLE_EQ(f.value, 250.0);
}

TEST(ParserTest, FloatLiteralScientificWithUnderscore) {
    Program prog = parseStr("x = 1_000e3");
    const auto &f = std::get<FloatExpr>(std::get<AssignStmt>(prog[0]).value->data);
    EXPECT_DOUBLE_EQ(f.value, 1e6);
}

TEST(ParserTest, FloatLiteralScientificF32Suffix) {
    Program prog = parseStr("x = 1e10f32");
    const auto &f = std::get<FloatExpr>(std::get<AssignStmt>(prog[0]).value->data);
    EXPECT_EQ(f.suffix, "f32");
}

TEST(ParserTest, ScientificWithIntSuffixRejected) {
    // `1e10` is a float literal; an integer suffix is an error.
    EXPECT_THROW(parseStr("x = 1e10i32"), DiagnosticError);
}

TEST(ParserTest, ScientificMissingExponentThrows) {
    // `1e` has no exponent digits; lexer keeps `e` as the start of an
    // identifier which the "invalid character after numeric literal"
    // guard then rejects.
    EXPECT_THROW(parseStr("x = 1e"), std::runtime_error);
}

TEST(ParserTest, ScientificSignedMissingExponentThrows) {
    // `1e-` has a sign but no digits; same fallthrough as `1e`.
    EXPECT_THROW(parseStr("x = 1e-"), std::runtime_error);
}

TEST(ParserTest, FloatLiteralLeadingDotScientific) {
    // `.5e10` is also a valid float with exponent — the leading-dot lexer
    // branch must reuse the same exponent scan as the digit-prefixed one.
    Program prog = parseStr("x = .5e10");
    const auto &f = std::get<FloatExpr>(std::get<AssignStmt>(prog[0]).value->data);
    EXPECT_DOUBLE_EQ(f.value, 0.5e10);
}

TEST(ParserTest, IdentifierStartingWithENotStolen) {
    // Regression guard: `1exp` must not swallow `e` as an exponent.
    EXPECT_THROW(parseStr("x = 1exp"), std::runtime_error);
}

// ===== Chained LHS assignment (#812) =====

TEST(ParserTest, ChainedLhsListFieldAssign) {
    // `list[i].field = v` parses as FieldAssignStmt with IndexExpr object.
    Program prog = parseStr("pts[0].x = 99");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<FieldAssignStmt>(prog[0]));
    const auto &s = std::get<FieldAssignStmt>(prog[0]);
    EXPECT_EQ(s.field, "x");
    EXPECT_FALSE(s.compound_op.has_value());
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<IndexExpr>>(s.object->data));
}

TEST(ParserTest, ChainedLhsRecordFieldListAssign) {
    // `record.field[i] = v` parses as IndexAssignStmt with FieldAccessExpr object.
    Program prog = parseStr("b.items[1] = 99");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<IndexAssignStmt>(prog[0]));
    const auto &s = std::get<IndexAssignStmt>(prog[0]);
    ASSERT_EQ(s.indices.size(), 1u);
    EXPECT_FALSE(s.compound_op.has_value());
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<FieldAccessExpr>>(s.object->data));
}

TEST(ParserTest, ChainedLhsNestedListAssign) {
    // `grid[0][1] = 99` parses as IndexAssignStmt with IndexExpr object.
    Program prog = parseStr("grid[0][1] = 99");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<IndexAssignStmt>(prog[0]));
    const auto &s = std::get<IndexAssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<IndexExpr>>(s.object->data));
}

TEST(ParserTest, ChainedLhsDeepFieldAssign) {
    // `d.a.inner.val = 99` parses as FieldAssignStmt with FieldAccessExpr object.
    Program prog = parseStr("d.a.inner.val = 99");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<FieldAssignStmt>(prog[0]));
    const auto &s = std::get<FieldAssignStmt>(prog[0]);
    EXPECT_EQ(s.field, "val");
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<FieldAccessExpr>>(s.object->data));
}

TEST(ParserTest, ChainedLhsIndexCompoundAssign) {
    // `xs[0] += 10` sets compound_op = "+".
    Program prog = parseStr("xs[0] += 10");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<IndexAssignStmt>(prog[0]));
    const auto &s = std::get<IndexAssignStmt>(prog[0]);
    ASSERT_TRUE(s.compound_op.has_value());
    EXPECT_EQ(*s.compound_op, "+");
}

TEST(ParserTest, ChainedLhsFieldCompoundAssign) {
    // `pts[0].x -= 5` on a list-of-records sets compound_op = "-".
    Program prog = parseStr("pts[0].x -= 5");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<FieldAssignStmt>(prog[0]));
    const auto &s = std::get<FieldAssignStmt>(prog[0]);
    ASSERT_TRUE(s.compound_op.has_value());
    EXPECT_EQ(*s.compound_op, "-");
}

TEST(ParserTest, ChainedLhsIndexPostfixIncrement) {
    // `xs[0]++` on a list element desugars to compound_op = "+" with rhs = 1.
    Program prog = parseStr("xs[0]++");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<IndexAssignStmt>(prog[0]));
    const auto &s = std::get<IndexAssignStmt>(prog[0]);
    ASSERT_TRUE(s.compound_op.has_value());
    EXPECT_EQ(*s.compound_op, "+");
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(s.value->data));
    EXPECT_EQ(std::get<NumberExpr>(s.value->data).value, 1);
}

TEST(ParserTest, ChainedLhsFieldPostfixDecrement) {
    // `pts[0].x--` on a list-of-records desugars to compound_op = "-" with rhs = 1.
    Program prog = parseStr("pts[0].x--");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<FieldAssignStmt>(prog[0]));
    const auto &s = std::get<FieldAssignStmt>(prog[0]);
    ASSERT_TRUE(s.compound_op.has_value());
    EXPECT_EQ(*s.compound_op, "-");
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(s.value->data));
    EXPECT_EQ(std::get<NumberExpr>(s.value->data).value, 1);
}

TEST(ParserTest, ChainedLhsIndexMissingEqualsThrows) {
    // Preserving the old strict error: a chain ending in `[i]` without an
    // assignment operator must still raise "expected '=' after index
    // expression" rather than silently becoming an expression statement.
    EXPECT_THROW(parseStr("xs[0]\n"), std::runtime_error);
}

TEST(ParserTest, ChainedLhsFieldMissingEqualsThrows) {
    // Same guard for field chains without an assignment operator.
    EXPECT_THROW(parseStr("rec.field\n"), std::runtime_error);
}

// ===== Trailing comma tests (#832) =====

TEST(ParserTest, ListTrailingComma) {
    Program prog = parseStr("x = [1, 2, 3,]");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<ListExpr>>(s.value->data));
    const auto &list = *std::get<std::unique_ptr<ListExpr>>(s.value->data);
    ASSERT_EQ(list.elements.size(), 3u);
    EXPECT_EQ(std::get<NumberExpr>(list.elements[0]->data).value, 1);
    EXPECT_EQ(std::get<NumberExpr>(list.elements[1]->data).value, 2);
    EXPECT_EQ(std::get<NumberExpr>(list.elements[2]->data).value, 3);
}

TEST(ParserTest, ListSingleTrailingComma) {
    Program prog = parseStr("x = [1,]");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<ListExpr>>(s.value->data));
    const auto &list = *std::get<std::unique_ptr<ListExpr>>(s.value->data);
    ASSERT_EQ(list.elements.size(), 1u);
    EXPECT_EQ(std::get<NumberExpr>(list.elements[0]->data).value, 1);
}

TEST(ParserTest, MapTrailingComma) {
    Program prog = parseStr("m = {\"a\": 1, \"b\": 2,}");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<MapExpr>>(s.value->data));
    const auto &map = *std::get<std::unique_ptr<MapExpr>>(s.value->data);
    ASSERT_EQ(map.keys.size(), 2u);
    ASSERT_EQ(map.values.size(), 2u);
    EXPECT_EQ(std::get<StringExpr>(map.keys[0]->data).value, "a");
    EXPECT_EQ(std::get<StringExpr>(map.keys[1]->data).value, "b");
}

TEST(ParserTest, SetTrailingComma) {
    Program prog = parseStr("s = {1, 2, 3,}");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<SetExpr>>(s.value->data));
    const auto &set = *std::get<std::unique_ptr<SetExpr>>(s.value->data);
    ASSERT_EQ(set.elements.size(), 3u);
    EXPECT_EQ(std::get<NumberExpr>(set.elements[0]->data).value, 1);
    EXPECT_EQ(std::get<NumberExpr>(set.elements[1]->data).value, 2);
    EXPECT_EQ(std::get<NumberExpr>(set.elements[2]->data).value, 3);
}

TEST(ParserTest, FunctionCallTrailingComma) {
    Program prog = parseStr("x = f(1, 2,)");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CallExpr>>(s.value->data));
    const auto &call = *std::get<std::unique_ptr<CallExpr>>(s.value->data);
    EXPECT_EQ(call.callee, "f");
    ASSERT_EQ(call.args.size(), 2u);
    EXPECT_EQ(std::get<NumberExpr>(call.args[0]->data).value, 1);
    EXPECT_EQ(std::get<NumberExpr>(call.args[1]->data).value, 2);
}

TEST(ParserTest, FunctionParamTrailingComma) {
    Program prog = parseStr("fn foo(a: int, b: int,) -> int:\n    return a\n");
    ASSERT_EQ(prog.size(), 1u);
    const auto &fn = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(fn.name, "foo");
    ASSERT_EQ(fn.params.size(), 2u);
    EXPECT_EQ(fn.params[0].name, "a");
    EXPECT_EQ(fn.params[1].name, "b");
}

TEST(ParserTest, LambdaParamTrailingComma) {
    Program prog = parseStr("f = (a: int, b: int,) => a + b");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<AssignStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<LambdaExpr>>(s.value->data));
    const auto &lambda = *std::get<std::unique_ptr<LambdaExpr>>(s.value->data);
    ASSERT_EQ(lambda.params.size(), 2u);
    EXPECT_EQ(lambda.params[0].name, "a");
    EXPECT_EQ(lambda.params[1].name, "b");
}

TEST(ParserTest, GenericTypeParamTrailingComma) {
    Program prog = parseStr("fn foo<T, U,>(x: T) -> U:\n    return x as U\n");
    ASSERT_EQ(prog.size(), 1u);
    const auto &fn = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    ASSERT_EQ(fn.type_params.size(), 2u);
    EXPECT_EQ(fn.type_params[0].name, "T");
    EXPECT_EQ(fn.type_params[1].name, "U");
}

TEST(ParserTest, EnumVariantFieldTrailingComma) {
    Program prog = parseStr("enum Shape:\n    Rect(float, float,)\n");
    ASSERT_EQ(prog.size(), 1u);
    const auto &es = std::get<EnumStmt>(prog[0]);
    ASSERT_EQ(es.variants.size(), 1u);
    ASSERT_EQ(es.variants[0].field_types.size(), 2u);
    EXPECT_EQ(es.variants[0].field_types[0]->toString(), "float");
    EXPECT_EQ(es.variants[0].field_types[1]->toString(), "float");
}

TEST(ParserTest, FnTypeTrailingComma) {
    Program prog = parseStr("type Callback = fn(int, int,) -> int");
    ASSERT_EQ(prog.size(), 1u);
    auto &ta = std::get<TypeAliasStmt>(prog[0]);
    EXPECT_EQ(ta.target_type->toString(), "fn(int, int) -> int");
}

TEST(ParserTest, GenericTypeArgTrailingComma) {
    Program prog = parseStr("fn foo(x: List<int,>) -> int:\n    return 0\n");
    ASSERT_EQ(prog.size(), 1u);
    const auto &fn = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    ASSERT_EQ(fn.params.size(), 1u);
    EXPECT_EQ(fn.params[0].type->toString(), "List<int>");
}

TEST(ParserTest, MapTypeArgTrailingComma) {
    Program prog = parseStr("fn foo(x: Map<str, int,>) -> int:\n    return 0\n");
    ASSERT_EQ(prog.size(), 1u);
    const auto &fn = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    ASSERT_EQ(fn.params.size(), 1u);
    EXPECT_EQ(fn.params[0].type->toString(), "Map<str, int>");
}

TEST(ParserTest, EnumConstructorPatternTrailingComma) {
    Program prog = parseStr("case x:\n    Foo::Bar(a, b,):\n        print(a)\n");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CaseStmt>>(prog[0]));
    const auto &cs = *std::get<std::unique_ptr<CaseStmt>>(prog[0]);
    ASSERT_EQ(cs.arms.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<EnumConstructorPattern>>(cs.arms[0].pattern));
    const auto &ep = *std::get<std::unique_ptr<EnumConstructorPattern>>(cs.arms[0].pattern);
    ASSERT_EQ(ep.bindings.size(), 2u);
    ASSERT_TRUE(std::holds_alternative<VariablePattern>(ep.bindings[0]));
    ASSERT_TRUE(std::holds_alternative<VariablePattern>(ep.bindings[1]));
    EXPECT_EQ(std::get<VariablePattern>(ep.bindings[0]).name, "a");
    EXPECT_EQ(std::get<VariablePattern>(ep.bindings[1]).name, "b");
}

TEST(ParserTest, EnumConstructorPatternNestedTuple) {
    Program prog = parseStr("case e:\n    Event::Click((x, y)):\n        print(x)\n");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CaseStmt>>(prog[0]));
    const auto &cs = *std::get<std::unique_ptr<CaseStmt>>(prog[0]);
    ASSERT_EQ(cs.arms.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<EnumConstructorPattern>>(cs.arms[0].pattern));
    const auto &ep = *std::get<std::unique_ptr<EnumConstructorPattern>>(cs.arms[0].pattern);
    EXPECT_EQ(ep.enum_name, "Event");
    EXPECT_EQ(ep.variant_name, "Click");
    ASSERT_EQ(ep.bindings.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<TuplePattern>>(ep.bindings[0]));
    const auto &tp = *std::get<std::unique_ptr<TuplePattern>>(ep.bindings[0]);
    ASSERT_EQ(tp.elements.size(), 2u);
    ASSERT_TRUE(std::holds_alternative<VariablePattern>(tp.elements[0]));
    ASSERT_TRUE(std::holds_alternative<VariablePattern>(tp.elements[1]));
    EXPECT_EQ(std::get<VariablePattern>(tp.elements[0]).name, "x");
    EXPECT_EQ(std::get<VariablePattern>(tp.elements[1]).name, "y");
}

TEST(ParserTest, EnumConstructorPatternNestedLiteral) {
    Program prog = parseStr("case w:\n    Wrap::Val(42):\n        print(42)\n");
    ASSERT_EQ(prog.size(), 1u);
    const auto &cs = *std::get<std::unique_ptr<CaseStmt>>(prog[0]);
    ASSERT_EQ(cs.arms.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<EnumConstructorPattern>>(cs.arms[0].pattern));
    const auto &ep = *std::get<std::unique_ptr<EnumConstructorPattern>>(cs.arms[0].pattern);
    ASSERT_EQ(ep.bindings.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<LiteralPattern>(ep.bindings[0]));
    const auto &lp = std::get<LiteralPattern>(ep.bindings[0]);
    const auto *ne = std::get_if<NumberExpr>(&lp.value->data);
    ASSERT_NE(ne, nullptr);
    EXPECT_EQ(ne->value, 42);
}

TEST(ParserTest, EnumConstructorPatternNestedWildcard) {
    Program prog = parseStr("case w:\n    Wrap::Val(_, y):\n        print(y)\n");
    ASSERT_EQ(prog.size(), 1u);
    const auto &cs = *std::get<std::unique_ptr<CaseStmt>>(prog[0]);
    ASSERT_EQ(cs.arms.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<EnumConstructorPattern>>(cs.arms[0].pattern));
    const auto &ep = *std::get<std::unique_ptr<EnumConstructorPattern>>(cs.arms[0].pattern);
    ASSERT_EQ(ep.bindings.size(), 2u);
    EXPECT_TRUE(std::holds_alternative<WildcardPattern>(ep.bindings[0]));
    ASSERT_TRUE(std::holds_alternative<VariablePattern>(ep.bindings[1]));
    EXPECT_EQ(std::get<VariablePattern>(ep.bindings[1]).name, "y");
}

TEST(ParserTest, DoubleTrailingCommaError) {
    EXPECT_THROW(parseStr("x = [1, 2,,]"), std::runtime_error);
}

TEST(ParserTest, GenericTypeArgDoubleCommaError) {
    EXPECT_THROW(parseStr("fn foo(x: List<int,,>) -> int:\n    return 0\n"), std::runtime_error);
}

// ============================================================
// Named arguments in function calls (#747)
// ============================================================

TEST(ParserTest, NamedArgInCallStmt) {
    Program prog = parseStr("print(\"hello\", end=\"\")\n");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<CallStmt>(prog[0]));
    const auto &s = std::get<CallStmt>(prog[0]);
    EXPECT_EQ(s.callee, "print");
    ASSERT_EQ(s.args.size(), 1u);
    ASSERT_EQ(s.named_args.size(), 1u);
    EXPECT_EQ(s.named_args[0].name, "end");
}

TEST(ParserTest, NamedArgOnlyInCallStmt) {
    Program prog = parseStr("print(end=\"\\n\")\n");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<CallStmt>(prog[0]);
    EXPECT_EQ(s.args.size(), 0u);
    ASSERT_EQ(s.named_args.size(), 1u);
    EXPECT_EQ(s.named_args[0].name, "end");
}

TEST(ParserTest, MultipleNamedArgsInCallStmt) {
    Program prog = parseStr("print(\"a\", sep=\"-\", end=\"!\\n\")\n");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<CallStmt>(prog[0]);
    ASSERT_EQ(s.args.size(), 1u);
    ASSERT_EQ(s.named_args.size(), 2u);
    EXPECT_EQ(s.named_args[0].name, "sep");
    EXPECT_EQ(s.named_args[1].name, "end");
}

TEST(ParserTest, PositionalAfterNamedArgError) {
    EXPECT_THROW(parseStr("print(end=\"\", \"hello\")\n"), std::runtime_error);
}

TEST(ParserTest, DuplicateNamedArgError) {
    EXPECT_THROW(parseStr("print(end=\"\\n\", end=\"!\")\n"), std::runtime_error);
}

// ============================================================
// Tuple patterns in case (#834)
// ============================================================

TEST(ParserTest, TuplePatternZeroTupleRejected) {
    // '()' as a pattern is not supported
    EXPECT_THROW(parseStr("case x:\n    ():\n        print(0)\n"), std::runtime_error);
}

TEST(ParserTest, TuplePatternUnclosedError) {
    // Missing closing ')' in tuple pattern
    EXPECT_THROW(parseStr("case x:\n    (a, b:\n        print(0)\n"), std::runtime_error);
}

TEST(ParserTest, TuplePatternOrWithBindingRejected) {
    // OR-pattern containing a binding inside a tuple should be rejected
    EXPECT_THROW(parseStr("case x:\n    (1, y) | (2, z):\n        print(0)\n"), std::runtime_error);
}

TEST(ParserTest, TuplePattern2ElementParsed) {
    Program prog = parseStr("case t:\n    (a, b):\n        print(a)\n");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CaseStmt>>(prog[0]));
    const auto &cs = *std::get<std::unique_ptr<CaseStmt>>(prog[0]);
    ASSERT_EQ(cs.arms.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<TuplePattern>>(cs.arms[0].pattern));
    const auto &tp = *std::get<std::unique_ptr<TuplePattern>>(cs.arms[0].pattern);
    ASSERT_EQ(tp.elements.size(), 2u);
    EXPECT_TRUE(std::holds_alternative<VariablePattern>(tp.elements[0]));
    EXPECT_TRUE(std::holds_alternative<VariablePattern>(tp.elements[1]));
}

TEST(ParserTest, TuplePattern1TupleParsed) {
    Program prog = parseStr("case t:\n    (v,):\n        print(v)\n");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CaseStmt>>(prog[0]));
    const auto &cs = *std::get<std::unique_ptr<CaseStmt>>(prog[0]);
    ASSERT_EQ(cs.arms.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<TuplePattern>>(cs.arms[0].pattern));
    const auto &tp = *std::get<std::unique_ptr<TuplePattern>>(cs.arms[0].pattern);
    ASSERT_EQ(tp.elements.size(), 1u);
}

TEST(ParserTest, TuplePatternGroupingUnwrapped) {
    // (p) with no comma is grouping — the inner pattern is returned, not a TuplePattern
    Program prog = parseStr("case x:\n    (42):\n        print(0)\n");
    ASSERT_EQ(prog.size(), 1u);
    const auto &cs = *std::get<std::unique_ptr<CaseStmt>>(prog[0]);
    ASSERT_EQ(cs.arms.size(), 1u);
    EXPECT_FALSE(std::holds_alternative<std::unique_ptr<TuplePattern>>(cs.arms[0].pattern));
    EXPECT_TRUE(std::holds_alternative<LiteralPattern>(cs.arms[0].pattern));
}

TEST(ParserTest, TuplePatternGroupingUnclosedError) {
    // (42: — single element grouping with missing ')' hits the distinct rejection branch
    EXPECT_THROW(parseStr("case x:\n    (42:\n        print(0)\n"), std::runtime_error);
}

// ===== Record Pattern parsing =====

TEST(ParserTest, RecordPatternBasic) {
    // Point(a, b) parses to RecordPattern with two VariablePattern elements
    Program prog = parseStr("case p:\n    Point(a, b):\n        print(a)\n");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CaseStmt>>(prog[0]));
    const auto &cs = *std::get<std::unique_ptr<CaseStmt>>(prog[0]);
    ASSERT_EQ(cs.arms.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<RecordPattern>>(cs.arms[0].pattern));
    const auto &rp = *std::get<std::unique_ptr<RecordPattern>>(cs.arms[0].pattern);
    EXPECT_EQ(rp.name, "Point");
    ASSERT_EQ(rp.elements.size(), 2u);
    EXPECT_TRUE(std::holds_alternative<VariablePattern>(rp.elements[0]));
    EXPECT_TRUE(std::holds_alternative<VariablePattern>(rp.elements[1]));
    EXPECT_EQ(std::get<VariablePattern>(rp.elements[0]).name, "a");
    EXPECT_EQ(std::get<VariablePattern>(rp.elements[1]).name, "b");
}

TEST(ParserTest, RecordPatternWithLiteralAndWildcard) {
    // Point(0, _) — literal element and wildcard element
    Program prog = parseStr("case p:\n    Point(0, _):\n        print(0)\n");
    ASSERT_EQ(prog.size(), 1u);
    const auto &cs = *std::get<std::unique_ptr<CaseStmt>>(prog[0]);
    ASSERT_EQ(cs.arms.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<RecordPattern>>(cs.arms[0].pattern));
    const auto &rp = *std::get<std::unique_ptr<RecordPattern>>(cs.arms[0].pattern);
    ASSERT_EQ(rp.elements.size(), 2u);
    EXPECT_TRUE(std::holds_alternative<LiteralPattern>(rp.elements[0]));
    EXPECT_TRUE(std::holds_alternative<WildcardPattern>(rp.elements[1]));
}

TEST(ParserTest, RecordPatternNested) {
    // Outer(Point(a, b), _) — nested record head in element position
    Program prog = parseStr("case s:\n    Outer(Point(a, b), _):\n        print(a)\n");
    ASSERT_EQ(prog.size(), 1u);
    const auto &cs = *std::get<std::unique_ptr<CaseStmt>>(prog[0]);
    ASSERT_EQ(cs.arms.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<RecordPattern>>(cs.arms[0].pattern));
    const auto &outer = *std::get<std::unique_ptr<RecordPattern>>(cs.arms[0].pattern);
    EXPECT_EQ(outer.name, "Outer");
    ASSERT_EQ(outer.elements.size(), 2u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<RecordPattern>>(outer.elements[0]));
    const auto &inner = *std::get<std::unique_ptr<RecordPattern>>(outer.elements[0]);
    EXPECT_EQ(inner.name, "Point");
    ASSERT_EQ(inner.elements.size(), 2u);
}

TEST(ParserTest, RecordPatternEmptyRejected) {
    // Point() — must have at least one field
    EXPECT_THROW(parseStr("case p:\n    Point():\n        print(0)\n"), std::runtime_error);
}

TEST(ParserTest, RecordPatternUnclosedRejected) {
    // Point(a, b — missing closing ')'
    EXPECT_THROW(parseStr("case p:\n    Point(a, b:\n        print(0)\n"), std::runtime_error);
}

TEST(ParserTest, RecordPatternOrWithBindingRejected) {
    // OR-pattern cannot contain variable bindings
    EXPECT_THROW(parseStr("case p:\n    Point(a, b) | Point(c, d):\n        print(0)\n"), std::runtime_error);
}

TEST(ParserTest, RecordPatternTrailingComma) {
    // Point(a, b,) — trailing comma is accepted and yields the same two-element RecordPattern
    Program prog = parseStr("case p:\n    Point(a, b,):\n        print(0)\n");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CaseStmt>>(prog[0]));
    const auto &cs = *std::get<std::unique_ptr<CaseStmt>>(prog[0]);
    ASSERT_EQ(cs.arms.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<RecordPattern>>(cs.arms[0].pattern));
    const auto &rp = *std::get<std::unique_ptr<RecordPattern>>(cs.arms[0].pattern);
    EXPECT_EQ(rp.name, "Point");
    ASSERT_EQ(rp.elements.size(), 2u);  // trailing comma must NOT produce a phantom element
    EXPECT_TRUE(std::holds_alternative<VariablePattern>(rp.elements[0]));
    EXPECT_TRUE(std::holds_alternative<VariablePattern>(rp.elements[1]));
}

// ---- Array type T[N] parser tests (regression for #1259) ----

TEST(ParserArrayType, HappyPath_SmallSize) {
    Program prog = parseStr("x: int[10] = 0");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<AssignStmt>(prog[0]));
    const auto &s = std::get<AssignStmt>(prog[0]);
    EXPECT_EQ(s.name, "x");
    ASSERT_TRUE(s.type_annotation != nullptr);
    EXPECT_EQ(s.type_annotation->toString(), "int[10]");
}

TEST(ParserArrayType, RejectsOverflowingSize) {
    EXPECT_THROW(parseStr(
        "x: int[999999999999999999999999999999999999999999999999999999999999999999999999999999999999] = 0"),
        DiagnosticError);
}

TEST(ParserArrayType, RejectsHexLiteralSize) {
    EXPECT_THROW(parseStr("x: int[0xFF] = 0"), DiagnosticError);
}

TEST(ParserArrayType, RejectsUnderscoreInSize) {
    EXPECT_THROW(parseStr("x: int[1_000] = 0"), DiagnosticError);
}

// ---- Angle-bracket generic call rejection (regression for #1885) ----
//
// `f<T>(args)` at expression position used to be silently parsed as a
// comparison-chain (`f < T > (args)`), causing the misleading
// "undefined variable: f" error. The canonical generic-call syntax is
// `f[T](args)` (docs/reference/functions.md §Generic Functions); the
// parser must emit a clear diagnostic for the `<T>(...)` form instead.

TEST(ParserTest, AngleBracketGenericCallRejectsFlat) {
    try {
        parseStr("x = loadAs<int>(\"1\")");
        FAIL() << "expected DiagnosticError";
    } catch (const DiagnosticError &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("f[T](args)"), std::string::npos)
            << "should suggest [T] syntax: " << msg;
    }
}

TEST(ParserTest, AngleBracketGenericCallRejectsNested) {
    try {
        parseStr("x = loadAs<Map<str, int>>(\"{}\")");
        FAIL() << "expected DiagnosticError";
    } catch (const DiagnosticError &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("f[T](args)"), std::string::npos)
            << "should suggest [T] syntax: " << msg;
    }
}

TEST(ParserTest, AngleBracketGenericCallRejectsInCaseExpr) {
    try {
        parseStr(
            "case loadAs<int>(\"1\"):\n"
            "    Ok(v):\n"
            "        print(v)\n"
            "    Err(e):\n"
            "        print(e.message)\n");
        FAIL() << "expected DiagnosticError";
    } catch (const DiagnosticError &e) {
        std::string msg = e.what();
        EXPECT_NE(msg.find("f[T](args)"), std::string::npos)
            << "should suggest [T] syntax: " << msg;
    }
}

TEST(ParserTest, AngleBracketGenericCallPreservesComparisonChain) {
    // `a < b > c` must still parse as a chained comparison (not a generic call).
    Program prog = parseStr("x = a < b > c");
    ASSERT_EQ(prog.size(), 1u);
}

TEST(ParserTest, AngleBracketGenericCallPreservesIdentLessExpr) {
    // Plain `f < n` (no closing `>`) is a comparison and must keep parsing.
    Program prog = parseStr("x = f < n");
    ASSERT_EQ(prog.size(), 1u);
}

TEST(ParserTest, SquareBracketGenericCallStillWorks) {
    // The canonical `f[T](args)` syntax remains accepted unchanged.
    Program prog = parseStr("x = loadAs[int](\"1\")");
    ASSERT_EQ(prog.size(), 1u);
}

TEST(ParserTest, EnumConstructorWithGenericTypeStillWorks) {
    // `Foo<T>::Variant(args)` enum-constructor path must remain accepted —
    // it is the sibling branch of the new `<T>(...)` rejection.
    Program prog = parseStr("x = MyOption<int>::MySome(42)");
    ASSERT_EQ(prog.size(), 1u);
}
