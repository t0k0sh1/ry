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

TEST(ParserTest, TypeAnnotationAcceptsUserDefinedType) {
    Program prog = parseStr("let x: Point = p");
    ASSERT_EQ(prog.size(), 1u);
    const auto &s = std::get<LetStmt>(prog[0]);
    ASSERT_TRUE(s.type_annotation.has_value());
    EXPECT_EQ(*s.type_annotation, "Point");
}

// ===== type パーサーテスト =====

TEST(ParserTest, TypeDefinition) {
    Program prog = parseStr("type Point:\n    x: int\n    y: int");
    ASSERT_EQ(prog.size(), 1u);
    ASSERT_TRUE(std::holds_alternative<TypeStmt>(prog[0]));
    const auto &ts = std::get<TypeStmt>(prog[0]);
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

TEST(ParserTest, TypeAnnotationWithoutLetOrConstThrows) {
    // x: int = 10 はエラー（let/const が必要）
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
    EXPECT_THROW(parseStr("type Point:\n    x: int\n    x: int"), std::runtime_error);
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
