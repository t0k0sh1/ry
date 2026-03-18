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

TEST(ParserTest, VarSimpleInt) {
    Program prog = parseStr("var x = 42");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<VarStmt>(prog[0]));
    const auto &s = std::get<VarStmt>(prog[0]);
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

TEST(ParserTest, VarWithTypeAnnotation) {
    Program prog = parseStr("var y: float = 3.14");
    const auto &s = std::get<VarStmt>(prog[0]);
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
    Program prog = parseStr("var x = 1\nx = 2");
    ASSERT_EQ(prog.size(), 2u);
    ASSERT_TRUE(std::holds_alternative<VarStmt>(prog[0]));
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

TEST(ParserTest, TypeAnnotationAcceptsUserDefinedType) {
    Program prog = parseStr("let x: Point = p");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<LetStmt>(prog[0]);
    ASSERT_TRUE(s.type_annotation.has_value());
    EXPECT_EQ(*s.type_annotation, "Point");
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
    EXPECT_EQ(ts.fields[0].type, "int");
    EXPECT_EQ(ts.fields[1].name, "y");
    EXPECT_EQ(ts.fields[1].type, "int");
}

TEST(ParserTest, FieldAccessSimple) {
    Program prog = parseStr("let x = p.x");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<LetStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<FieldAccessExpr>>(s.value->data));
    const auto &fa = *std::get<std::unique_ptr<FieldAccessExpr>>(s.value->data);
    EXPECT_EQ(fa.field, "x");
    ASSERT_TRUE(std::holds_alternative<VariableExpr>(fa.object->data));
    EXPECT_EQ(std::get<VariableExpr>(fa.object->data).name, "p");
}

TEST(ParserTest, FieldAccessChained) {
    Program prog = parseStr("let x = a.b.c");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<LetStmt>(prog[0]);
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
    Program prog = parseStr("let s = \"hello\"");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<LetStmt>(prog[0]);
    EXPECT_EQ(s.name, "s");
    ASSERT_TRUE(std::holds_alternative<StringExpr>(s.value->data));
    EXPECT_EQ(std::get<StringExpr>(s.value->data).value, "hello");
}

TEST(ParserTest, LetStringWithTypeAnnotation) {
    Program prog = parseStr("let s: str = \"world\"");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<LetStmt>(prog[0]);
    EXPECT_EQ(s.name, "s");
    ASSERT_TRUE(s.type_annotation.has_value());
    EXPECT_EQ(*s.type_annotation, "str");
    ASSERT_TRUE(std::holds_alternative<StringExpr>(s.value->data));
    EXPECT_EQ(std::get<StringExpr>(s.value->data).value, "world");
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

TEST(ParserTest, TypeAnnotationWithoutLetOrVarThrows) {
    // x: int = 10 はエラー（let/var が必要）
    EXPECT_THROW(parseStr("x: int = 10"), std::runtime_error);
}

// ===== if/elif/else パーサーテスト =====

TEST(ParserTest, IfSimple) {
    Program prog = parseStr("if true:\n    print(1)");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<IfStmt>>(prog[0]));
    const auto &ifStmt = *std::get<std::unique_ptr<IfStmt>>(prog[0]);
    ASSERT_EQ(ifStmt.branches.size(), 1u);
    EXPECT_TRUE(ifStmt.else_body.empty());
    ASSERT_EQ(ifStmt.branches[0].body.size(), 1u);
    EXPECT_TRUE(std::holds_alternative<CallStmt>(ifStmt.branches[0].body[0]));
}

TEST(ParserTest, IfElse) {
    Program prog = parseStr("if true:\n    print(1)\nelse:\n    print(2)");
    ASSERT_EQ(prog.size(), 1u);
    const auto &ifStmt = *std::get<std::unique_ptr<IfStmt>>(prog[0]);
    ASSERT_EQ(ifStmt.branches.size(), 1u);
    ASSERT_EQ(ifStmt.else_body.size(), 1u);
}

TEST(ParserTest, IfElifElse) {
    Program prog = parseStr("if true:\n    print(1)\nelif false:\n    print(2)\nelse:\n    print(3)");
    ASSERT_EQ(prog.size(), 1u);
    const auto &ifStmt = *std::get<std::unique_ptr<IfStmt>>(prog[0]);
    ASSERT_EQ(ifStmt.branches.size(), 2u);
    ASSERT_EQ(ifStmt.else_body.size(), 1u);
}

TEST(ParserTest, IfBlockMultipleStatements) {
    Program prog = parseStr("if true:\n    let x = 1\n    print(x)");
    const auto &ifStmt = *std::get<std::unique_ptr<IfStmt>>(prog[0]);
    ASSERT_EQ(ifStmt.branches[0].body.size(), 2u);
    EXPECT_TRUE(std::holds_alternative<LetStmt>(ifStmt.branches[0].body[0]));
    EXPECT_TRUE(std::holds_alternative<CallStmt>(ifStmt.branches[0].body[1]));
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
    Program prog = parseStr("while true:\n    let x = 1\n    print(x)");
    const auto &ws = *std::get<std::unique_ptr<WhileStmt>>(prog[0]);
    ASSERT_EQ(ws.body.size(), 2u);
    EXPECT_TRUE(std::holds_alternative<LetStmt>(ws.body[0]));
    EXPECT_TRUE(std::holds_alternative<CallStmt>(ws.body[1]));
}

TEST(ParserTest, WhileMissingColonThrows) {
    EXPECT_THROW(parseStr("while true\n    print(1)"), std::runtime_error);
}

// ===== fn / return / CallExpr パーサーテスト =====

TEST(ParserTest, FnSimple) {
    Program prog = parseStr("fn add(a: int, b: int) -> int:\n    return a + b");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<FnStmt>>(prog[0]));
    const auto &fn = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(fn.name, "add");
    ASSERT_EQ(fn.params.size(), 2u);
    EXPECT_EQ(fn.params[0].name, "a");
    EXPECT_EQ(fn.params[0].type, "int");
    EXPECT_EQ(fn.params[1].name, "b");
    EXPECT_EQ(fn.params[1].type, "int");
    EXPECT_EQ(fn.return_type, "int");
    ASSERT_EQ(fn.body.size(), 1u);
    EXPECT_TRUE(std::holds_alternative<ReturnStmt>(fn.body[0]));
}

TEST(ParserTest, ReturnStatement) {
    Program prog = parseStr("fn f() -> int:\n    return 42");
    const auto &fn = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    ASSERT_EQ(fn.body.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<ReturnStmt>(fn.body[0]));
    const auto &ret = std::get<ReturnStmt>(fn.body[0]);
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(ret.value->data));
    EXPECT_EQ(std::get<NumberExpr>(ret.value->data).value, 42);
}

TEST(ParserTest, CallExprInLet) {
    Program prog = parseStr("let x = add(1, 2)");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<LetStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CallExpr>>(s.value->data));
    const auto &call = *std::get<std::unique_ptr<CallExpr>>(s.value->data);
    EXPECT_EQ(call.callee, "add");
    ASSERT_EQ(call.args.size(), 2u);
}

TEST(ParserTest, FnMissingColonThrows) {
    EXPECT_THROW(parseStr("fn f() -> int\n    return 1"), std::runtime_error);
}

TEST(ParserTest, FnMissingArrowWithTypeThrows) {
    // fn f() int: → missing '->' before type, "int" is not ':'
    EXPECT_THROW(parseStr("fn f() int:\n    return 1"), std::runtime_error);
}

TEST(ParserTest, FnReturnTypeOmitted) {
    // fn f(): → return type defaults to "Unit"
    Program prog = parseStr("fn f():\n    return");
    ASSERT_EQ(prog.size(), 1u);
    const auto &fn = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(fn.name, "f");
    EXPECT_EQ(fn.return_type, "Unit");
    ASSERT_EQ(fn.body.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<ReturnStmt>(fn.body[0]));
    const auto &ret = std::get<ReturnStmt>(fn.body[0]);
    EXPECT_EQ(ret.value, nullptr);
}

TEST(ParserTest, FnExplicitUnitReturn) {
    Program prog = parseStr("fn f() -> Unit:\n    return");
    const auto &fn = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(fn.return_type, "Unit");
}

TEST(ParserTest, TypeAnnotationOptionInt) {
    Program prog = parseStr("let x: Option<int> = None");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<LetStmt>(prog[0]);
    ASSERT_TRUE(s.type_annotation.has_value());
    EXPECT_EQ(*s.type_annotation, "Option<int>");
}

TEST(ParserTest, FnParamOptionType) {
    Program prog = parseStr("fn f(x: Option<int>) -> int:\n    return 0");
    const auto &fn = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    ASSERT_EQ(fn.params.size(), 1u);
    EXPECT_EQ(fn.params[0].type, "Option<int>");
}

TEST(ParserTest, FnReturnOptionType) {
    Program prog = parseStr("fn f() -> Option<int>:\n    return Some(1)");
    const auto &fn = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(fn.return_type, "Option<int>");
}

// ===== import パーサーテスト =====

TEST(ParserTest, ImportAll) {
    Program prog = parseStr("from math");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<ImportStmt>(prog[0]));
    const auto &imp = std::get<ImportStmt>(prog[0]);
    EXPECT_EQ(imp.module_path, "math.ry");
    EXPECT_TRUE(imp.names.empty());
}

TEST(ParserTest, ImportSingleFunction) {
    Program prog = parseStr("from math import add");
    ASSERT_EQ(prog.size(), 1u);
    const auto &imp = std::get<ImportStmt>(prog[0]);
    EXPECT_EQ(imp.module_path, "math.ry");
    ASSERT_EQ(imp.names.size(), 1u);
    EXPECT_EQ(imp.names[0], "add");
}

TEST(ParserTest, ImportMultipleFunctions) {
    Program prog = parseStr("from math import add, sub");
    const auto &imp = std::get<ImportStmt>(prog[0]);
    EXPECT_EQ(imp.module_path, "math.ry");
    ASSERT_EQ(imp.names.size(), 2u);
    EXPECT_EQ(imp.names[0], "add");
    EXPECT_EQ(imp.names[1], "sub");
}

TEST(ParserTest, ImportDotPath) {
    Program prog = parseStr("from utils.math import add");
    const auto &imp = std::get<ImportStmt>(prog[0]);
    EXPECT_EQ(imp.module_path, "utils/math.ry");
    ASSERT_EQ(imp.names.size(), 1u);
    EXPECT_EQ(imp.names[0], "add");
}

TEST(ParserTest, ImportExpectedModuleName) {
    EXPECT_THROW(parseStr("from 42"), std::runtime_error);
}

TEST(ParserTest, ImportInBlockThrows) {
    EXPECT_THROW(parseStr("if true:\n    from math import add"), std::runtime_error);
}

TEST(ParserTest, DuplicateFieldNameThrows) {
    EXPECT_THROW(parseStr("record Point:\n    x: int\n    x: int"), std::runtime_error);
}

// ===== タプル パーサーテスト =====

TEST(ParserTest, TupleLiteral) {
    Program prog = parseStr("let t = (1, 2)");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<LetStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<TupleExpr>>(s.value->data));
    const auto &tuple = *std::get<std::unique_ptr<TupleExpr>>(s.value->data);
    ASSERT_EQ(tuple.elements.size(), 2u);
    EXPECT_EQ(std::get<NumberExpr>(tuple.elements[0]->data).value, 1);
    EXPECT_EQ(std::get<NumberExpr>(tuple.elements[1]->data).value, 2);
}

TEST(ParserTest, TupleMixedTypes) {
    Program prog = parseStr("let t = (1, 3.14)");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<LetStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<TupleExpr>>(s.value->data));
    const auto &tuple = *std::get<std::unique_ptr<TupleExpr>>(s.value->data);
    ASSERT_EQ(tuple.elements.size(), 2u);
    EXPECT_TRUE(std::holds_alternative<NumberExpr>(tuple.elements[0]->data));
    EXPECT_TRUE(std::holds_alternative<FloatExpr>(tuple.elements[1]->data));
}

TEST(ParserTest, TupleThreeElements) {
    Program prog = parseStr("let t = (1, 2, 3)");
    const auto &s = std::get<LetStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<TupleExpr>>(s.value->data));
    const auto &tuple = *std::get<std::unique_ptr<TupleExpr>>(s.value->data);
    ASSERT_EQ(tuple.elements.size(), 3u);
}

TEST(ParserTest, TupleTypeAnnotation) {
    Program prog = parseStr("let t: (int, float) = (1, 3.14)");
    const auto &s = std::get<LetStmt>(prog[0]);
    ASSERT_TRUE(s.type_annotation.has_value());
    EXPECT_EQ(*s.type_annotation, "(int, float)");
}

TEST(ParserTest, TupleIndexAccess) {
    // t.0 → FieldAccessExpr with field "0"
    Program prog = parseStr("let x = t.0");
    const auto &s = std::get<LetStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<FieldAccessExpr>>(s.value->data));
    const auto &fa = *std::get<std::unique_ptr<FieldAccessExpr>>(s.value->data);
    EXPECT_EQ(fa.field, "0");
    ASSERT_TRUE(std::holds_alternative<VariableExpr>(fa.object->data));
    EXPECT_EQ(std::get<VariableExpr>(fa.object->data).name, "t");
}

TEST(ParserTest, FnReturnTupleType) {
    Program prog = parseStr("fn swap(a: int, b: int) -> (int, int):\n    return (b, a)");
    const auto &fn = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(fn.return_type, "(int, int)");
}

TEST(ParserTest, ParenGroupingStillWorks) {
    // Single expression in parens is still grouping, not tuple
    Program prog = parseStr("let x = (1 + 2) * 3");
    const auto &s = std::get<LetStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<BinaryExpr>>(s.value->data));
    const auto &outer = *std::get<std::unique_ptr<BinaryExpr>>(s.value->data);
    EXPECT_EQ(outer.op, "*");
}

// ===== UFCS パーサーテスト =====

TEST(ParserTest, UFCSBasic) {
    // a.f(b) → CallExpr{f, [a, b]}
    Program prog = parseStr("let x = a.f(b)");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<LetStmt>(prog[0]);
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
    Program prog = parseStr("let x = a.f()");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<LetStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<CallExpr>>(s.value->data));
    const auto &call = *std::get<std::unique_ptr<CallExpr>>(s.value->data);
    EXPECT_EQ(call.callee, "f");
    ASSERT_EQ(call.args.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<VariableExpr>(call.args[0]->data));
    EXPECT_EQ(std::get<VariableExpr>(call.args[0]->data).name, "a");
}

TEST(ParserTest, UFCSChained) {
    // a.f(b).g(c) → CallExpr{g, [CallExpr{f, [a, b]}, c]}
    Program prog = parseStr("let x = a.f(b).g(c)");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<LetStmt>(prog[0]);
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
    Program prog = parseStr("let r = p.x.f()");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<LetStmt>(prog[0]);
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
    Program prog = parseStr("let m = {\"a\": 1, \"b\": 2}");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<LetStmt>(prog[0]);
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
    Program prog = parseStr("let m = {\"a\": 1}\nm[\"b\"] = 2");
    ASSERT_EQ(prog.size(), 2u);
    ASSERT_TRUE(std::holds_alternative<IndexAssignStmt>(prog[1]));
    const auto &s = std::get<IndexAssignStmt>(prog[1]);
    ASSERT_TRUE(std::holds_alternative<VariableExpr>(s.object->data));
    EXPECT_EQ(std::get<VariableExpr>(s.object->data).name, "m");
    ASSERT_TRUE(std::holds_alternative<StringExpr>(s.index->data));
    EXPECT_EQ(std::get<StringExpr>(s.index->data).value, "b");
    ASSERT_TRUE(std::holds_alternative<NumberExpr>(s.value->data));
    EXPECT_EQ(std::get<NumberExpr>(s.value->data).value, 2);
}

TEST(ParserTest, MapTypeAnnotation) {
    Program prog = parseStr("let m: Map<str, int> = {\"a\": 1}");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<LetStmt>(prog[0]);
    ASSERT_TRUE(s.type_annotation.has_value());
    EXPECT_EQ(*s.type_annotation, "Map<str, int>");
}

// ===== Operator overloading =====

TEST(ParserTest, OperatorFnBinaryPlus) {
    std::string src =
        "fn operator+(a: Vec2, b: Vec2) -> Vec2:\n"
        "    return a\n";
    Program prog = parseStr(src);
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<FnStmt>>(prog[0]));
    const auto &fn = std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(fn->name, "operator+");
    EXPECT_TRUE(fn->is_operator);
    ASSERT_EQ(fn->params.size(), 2u);
    EXPECT_EQ(fn->params[0].name, "a");
    EXPECT_EQ(fn->params[0].type, "Vec2");
    EXPECT_EQ(fn->params[1].name, "b");
    EXPECT_EQ(fn->params[1].type, "Vec2");
    EXPECT_EQ(fn->return_type, "Vec2");
}

TEST(ParserTest, OperatorFnUnaryMinus) {
    std::string src =
        "fn operator-(a: Vec2) -> Vec2:\n"
        "    return a\n";
    Program prog = parseStr(src);
    ASSERT_EQ(prog.size(), 1u);
    const auto &fn = std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(fn->name, "operator-");
    EXPECT_TRUE(fn->is_operator);
    ASSERT_EQ(fn->params.size(), 1u);
}

TEST(ParserTest, OperatorFnEqEq) {
    std::string src =
        "fn operator==(a: Vec2, b: Vec2) -> bool:\n"
        "    return true\n";
    Program prog = parseStr(src);
    const auto &fn = std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(fn->name, "operator==");
    EXPECT_TRUE(fn->is_operator);
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

// ===== Set パーサーテスト =====

TEST(ParserTest, SetLiteral) {
    Program prog = parseStr("let s = {1, 2, 3}");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<LetStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<SetExpr>>(s.value->data));
    const auto &set = *std::get<std::unique_ptr<SetExpr>>(s.value->data);
    ASSERT_EQ(set.elements.size(), 3u);
    EXPECT_EQ(std::get<NumberExpr>(set.elements[0]->data).value, 1);
    EXPECT_EQ(std::get<NumberExpr>(set.elements[1]->data).value, 2);
    EXPECT_EQ(std::get<NumberExpr>(set.elements[2]->data).value, 3);
}

TEST(ParserTest, SetSingleElement) {
    Program prog = parseStr("let s = {42}");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<LetStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<SetExpr>>(s.value->data));
    const auto &set = *std::get<std::unique_ptr<SetExpr>>(s.value->data);
    ASSERT_EQ(set.elements.size(), 1u);
    EXPECT_EQ(std::get<NumberExpr>(set.elements[0]->data).value, 42);
}

TEST(ParserTest, SetTypeAnnotation) {
    Program prog = parseStr("let s: Set<int> = {1}");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<LetStmt>(prog[0]);
    ASSERT_TRUE(s.type_annotation.has_value());
    EXPECT_EQ(*s.type_annotation, "Set<int>");
}

TEST(ParserTest, InOperator) {
    Program prog = parseStr("let r = x in s");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<LetStmt>(prog[0]);
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
    Program prog = parseStr("let c = Color::Red");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<LetStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<EnumAccessExpr>(s.value->data));
    const auto &ea = std::get<EnumAccessExpr>(s.value->data);
    EXPECT_EQ(ea.enum_name, "Color");
    EXPECT_EQ(ea.variant_name, "Red");
}

TEST(ParserTest, EnumComparison) {
    Program prog = parseStr("let r = c == Color::Green");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<LetStmt>(prog[0]);
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
    Program prog = parseStr("let x: int | str = 42");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<LetStmt>(prog[0]);
    EXPECT_EQ(s.name, "x");
    ASSERT_TRUE(s.type_annotation.has_value());
    EXPECT_EQ(*s.type_annotation, "int | str");
}

TEST(ParserTest, FnUnionParam) {
    Program prog = parseStr("fn f(x: int | str) -> int:\n    return 0");
    const auto &fn = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    ASSERT_EQ(fn.params.size(), 1u);
    EXPECT_EQ(fn.params[0].type, "int | str");
}

TEST(ParserTest, FnUnionReturn) {
    Program prog = parseStr("fn f() -> int | str:\n    return 0");
    const auto &fn = *std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(fn.return_type, "int | str");
}

// ===== >>> パーサーテスト =====

TEST(ParserTest, LogicalRightShift) {
    Program prog = parseStr("let x = a >>> b");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<LetStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<BinaryExpr>>(s.value->data));
    const auto &bin = *std::get<std::unique_ptr<BinaryExpr>>(s.value->data);
    EXPECT_EQ(bin.op, ">>>");
    EXPECT_EQ(std::get<VariableExpr>(bin.lhs->data).name, "a");
    EXPECT_EQ(std::get<VariableExpr>(bin.rhs->data).name, "b");
}

// ===== not in パーサーテスト =====

TEST(ParserTest, NotInOperator) {
    Program prog = parseStr("let r = x not in s");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<LetStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<BinaryExpr>>(s.value->data));
    const auto &bin = *std::get<std::unique_ptr<BinaryExpr>>(s.value->data);
    EXPECT_EQ(bin.op, "not in");
    EXPECT_EQ(std::get<VariableExpr>(bin.lhs->data).name, "x");
    EXPECT_EQ(std::get<VariableExpr>(bin.rhs->data).name, "s");
}

TEST(ParserTest, NotStillWorksAfterNotIn) {
    // "not x" should still parse as UnaryExpr
    Program prog = parseStr("let r = not x");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<LetStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<UnaryExpr>>(s.value->data));
    const auto &unary = *std::get<std::unique_ptr<UnaryExpr>>(s.value->data);
    EXPECT_EQ(unary.op, "not");
}

TEST(ParserTest, UnionThreeTypes) {
    Program prog = parseStr("let x: int | float | str = 42");
    const auto &s = std::get<LetStmt>(prog[0]);
    ASSERT_TRUE(s.type_annotation.has_value());
    EXPECT_EQ(*s.type_annotation, "int | float | str");
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
    auto &fn = std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(fn->name, "deposit");
    EXPECT_EQ(fn->preconditions.size(), 1u);
    EXPECT_EQ(fn->postconditions.size(), 0u);
    EXPECT_EQ(fn->body.size(), 1u);
}

TEST(ParserTest, FnWithEnsure) {
    std::string src =
        "fn abs(x: int) -> int:\n"
        "    ensure:\n"
        "        result >= 0\n"
        "    if x < 0:\n"
        "        return -x\n"
        "    return x";
    Program prog = parseStr(src);
    auto &fn = std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(fn->preconditions.size(), 0u);
    EXPECT_EQ(fn->postconditions.size(), 1u);
}

TEST(ParserTest, FnWithRequireAndEnsure) {
    std::string src =
        "fn add(a: int, b: int) -> int:\n"
        "    require:\n"
        "        a >= 0\n"
        "        b >= 0\n"
        "    ensure:\n"
        "        result >= 0\n"
        "    return a + b";
    Program prog = parseStr(src);
    auto &fn = std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(fn->preconditions.size(), 2u);
    EXPECT_EQ(fn->postconditions.size(), 1u);
    EXPECT_EQ(fn->body.size(), 1u);
}

TEST(ParserTest, FnWithoutContract) {
    std::string src =
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b";
    Program prog = parseStr(src);
    auto &fn = std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(fn->preconditions.size(), 0u);
    EXPECT_EQ(fn->postconditions.size(), 0u);
}

TEST(ParserTest, TypeWithInvariant) {
    std::string src =
        "record Account:\n"
        "    balance: int\n"
        "    min_balance: int\n"
        "    invariant:\n"
        "        balance >= min_balance";
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

TEST(ParserTest, OldExprParse) {
    std::string src =
        "fn inc(x: int) -> int:\n"
        "    ensure:\n"
        "        result == old(x) + 1\n"
        "    return x + 1";
    Program prog = parseStr(src);
    auto &fn = std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(fn->postconditions.size(), 1u);
    // The postcondition expression should contain an OldExpr
    auto &postExpr = fn->postconditions[0];
    auto *bin = std::get_if<std::unique_ptr<BinaryExpr>>(&postExpr->data);
    ASSERT_TRUE(bin != nullptr);
    // lhs is ResultExpr
    ASSERT_TRUE(std::holds_alternative<ResultExpr>((*bin)->lhs->data));
    // rhs is BinaryExpr (old(x) + 1)
    auto *rhsBin = std::get_if<std::unique_ptr<BinaryExpr>>(&(*bin)->rhs->data);
    ASSERT_TRUE(rhsBin != nullptr);
    // rhs.lhs is OldExpr
    ASSERT_TRUE(std::holds_alternative<std::unique_ptr<OldExpr>>((*rhsBin)->lhs->data));
}

TEST(ParserTest, ResultExprParse) {
    std::string src =
        "fn double(x: int) -> int:\n"
        "    ensure:\n"
        "        result >= 0\n"
        "    return x * 2";
    Program prog = parseStr(src);
    auto &fn = std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(fn->postconditions.size(), 1u);
    // The postcondition should contain result >= 0
    auto &postExpr = fn->postconditions[0];
    auto *bin = std::get_if<std::unique_ptr<BinaryExpr>>(&postExpr->data);
    ASSERT_TRUE(bin != nullptr);
    ASSERT_TRUE(std::holds_alternative<ResultExpr>((*bin)->lhs->data));
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
    EXPECT_EQ(ta.target_type, "int");
}

// ===== for k, v in map =====

TEST(ParserTest, ForKVParsing) {
    Program prog = parseStr("for k, v in m:\n    print(k)");
    ASSERT_EQ(prog.size(), 1u);
    auto &fs = std::get<std::unique_ptr<ForStmt>>(prog[0]);
    EXPECT_EQ(fs->var_name, "k");
    ASSERT_TRUE(fs->var_name2.has_value());
    EXPECT_EQ(*fs->var_name2, "v");
}

// ===== .. 演算子 =====

TEST(ParserTest, RangeExpr) {
    Program prog = parseStr("let xs = 1 .. 5");
    ASSERT_EQ(prog.size(), 1u);
    auto &let = std::get<LetStmt>(prog[0]);
    auto *range = std::get_if<std::unique_ptr<RangeExpr>>(&let.value->data);
    ASSERT_TRUE(range != nullptr);
}

// ===== ?? 演算子 =====

TEST(ParserTest, NullCoalesceExpr) {
    Program prog = parseStr("let x = a ?? 0");
    ASSERT_EQ(prog.size(), 1u);
    auto &let = std::get<LetStmt>(prog[0]);
    auto *bin = std::get_if<std::unique_ptr<BinaryExpr>>(&let.value->data);
    ASSERT_TRUE(bin != nullptr);
    EXPECT_EQ((*bin)->op, "??");
}

// ===== none キーワード =====

TEST(ParserTest, NoneExpr) {
    Program prog = parseStr("let x = none");
    ASSERT_EQ(prog.size(), 1u);
    auto &let = std::get<LetStmt>(prog[0]);
    ASSERT_TRUE(std::holds_alternative<NoneExpr>(let.value->data));
}

// ===== 命名規約チェック =====

TEST(ParserTest, SnakeCaseVariableRequired) {
    EXPECT_THROW(parseStr("let myVar = 1"), std::runtime_error);
}

TEST(ParserTest, SnakeCaseFunctionRequired) {
    EXPECT_THROW(parseStr("fn myFunc() -> int:\n    return 1"), std::runtime_error);
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

TEST(ParserTest, PascalCaseTypeAliasRequired) {
    EXPECT_THROW(parseStr("type my_int = int"), std::runtime_error);
}

TEST(ParserTest, TypeAliasFnType) {
    Program prog = parseStr("type Callback = fn(int, int) -> int");
    auto &ta = std::get<TypeAliasStmt>(prog[0]);
    EXPECT_EQ(ta.name, "Callback");
    EXPECT_EQ(ta.target_type, "fn(int, int) -> int");
}

TEST(ParserTest, SnakeCaseForLoopVariable) {
    EXPECT_THROW(parseStr("for myVar in xs:\n    print(myVar)"), std::runtime_error);
}

TEST(ParserTest, SnakeCaseParamRequired) {
    EXPECT_THROW(parseStr("fn add(myNum: int) -> int:\n    return myNum"), std::runtime_error);
}

// ===== expect マッチャー =====

TEST(ParserTest, ExpectToNotEq) {
    Program prog = parseStr("describe \"test\":\n    it \"t\":\n        expect(1).to_not_eq(2)");
    ASSERT_EQ(prog.size(), 1u);
}

TEST(ParserTest, ExpectToBeSome) {
    Program prog = parseStr("describe \"test\":\n    it \"t\":\n        expect(1).to_be_some()");
    ASSERT_EQ(prog.size(), 1u);
}

TEST(ParserTest, ExpectToContain) {
    Program prog = parseStr("describe \"test\":\n    it \"t\":\n        expect(1).to_contain(1)");
    ASSERT_EQ(prog.size(), 1u);
}

// ===== @native fn tests =====

TEST(ParserTest, NativeFnDeclaration) {
    Program prog = parseStr("@native\nfn contains(s: str, sub: str) -> bool\n");
    ASSERT_EQ(prog.size(), 1u);
    auto &fs = std::get<std::unique_ptr<FnStmt>>(prog[0]);
    EXPECT_EQ(fs->name, "contains");
    EXPECT_EQ(fs->params.size(), 2u);
    EXPECT_EQ(fs->return_type, "bool");
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

TEST(ParserTest, NativeFnWithColonError) {
    EXPECT_THROW({
        parseStr("@native\nfn bad() -> int:\n    return 1\n");
    }, std::runtime_error);
}
