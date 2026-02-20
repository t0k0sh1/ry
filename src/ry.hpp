#pragma once

// ===== Section 1: includes =====
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/ExecutionEngine/Orc/ExecutorProcessControl.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/InitLLVM.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>

#include <cstdint>
#include <functional>
#include <memory>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

using namespace llvm;
using namespace llvm::orc;

// ===== Section 2: Lexer =====

enum class TokenKind {
    Number, Float, Ident, Plus, Minus, Star, Slash, Equals,
    LParen, RParen, Comma, Newline, Eof, Error,
    Percent,     // %
    StarStar,    // **
    SlashSlash,  // //
    EqEq,        // ==
    BangEq,      // !=
    Less,        // <
    LessEq,      // <=
    Greater,     // >
    GreaterEq,   // >=
    And,         // and
    Or,          // or
    Not          // not
};

struct Token {
    TokenKind kind;
    std::string value;
    int line;
};

class Lexer {
public:
    explicit Lexer(std::string src) : src_(std::move(src)), pos_(0), line_(1) {
        current_ = readToken();
    }

    Token peek() const { return current_; }

    Token next() {
        Token t = current_;
        current_ = readToken();
        return t;
    }

private:
    std::string src_;
    size_t pos_;
    int line_;
    Token current_;

    Token readToken() {
        // skip spaces/tabs
        while (pos_ < src_.size() && (src_[pos_] == ' ' || src_[pos_] == '\t'))
            ++pos_;

        if (pos_ >= src_.size())
            return {TokenKind::Eof, "", line_};

        char c = src_[pos_];

        if (c == '\n') { ++pos_; return {TokenKind::Newline, "\n", line_++}; }
        if (c == '\r') {
            ++pos_;
            if (pos_ < src_.size() && src_[pos_] == '\n') ++pos_;
            return {TokenKind::Newline, "\n", line_++};
        }
        if (c == '+') { ++pos_; return {TokenKind::Plus,   "+", line_}; }
        if (c == '-') { ++pos_; return {TokenKind::Minus,  "-", line_}; }
        if (c == '%') { ++pos_; return {TokenKind::Percent, "%", line_}; }
        if (c == '*') {
            ++pos_;
            if (pos_ < src_.size() && src_[pos_] == '*') {
                ++pos_; return {TokenKind::StarStar, "**", line_};
            }
            return {TokenKind::Star, "*", line_};
        }
        if (c == '/') {
            ++pos_;
            if (pos_ < src_.size() && src_[pos_] == '/') {
                ++pos_; return {TokenKind::SlashSlash, "//", line_};
            }
            return {TokenKind::Slash, "/", line_};
        }
        if (c == '=') {
            ++pos_;
            if (pos_ < src_.size() && src_[pos_] == '=') {
                ++pos_; return {TokenKind::EqEq, "==", line_};
            }
            return {TokenKind::Equals, "=", line_};
        }
        if (c == '(') { ++pos_; return {TokenKind::LParen, "(", line_}; }
        if (c == ')') { ++pos_; return {TokenKind::RParen, ")", line_}; }
        if (c == ',') { ++pos_; return {TokenKind::Comma,  ",", line_}; }
        if (c == '!') {
            ++pos_;
            if (pos_ < src_.size() && src_[pos_] == '=') {
                ++pos_; return {TokenKind::BangEq, "!=", line_};
            }
            return {TokenKind::Error, "!", line_};
        }
        if (c == '<') {
            ++pos_;
            if (pos_ < src_.size() && src_[pos_] == '=') {
                ++pos_; return {TokenKind::LessEq, "<=", line_};
            }
            return {TokenKind::Less, "<", line_};
        }
        if (c == '>') {
            ++pos_;
            if (pos_ < src_.size() && src_[pos_] == '=') {
                ++pos_; return {TokenKind::GreaterEq, ">=", line_};
            }
            return {TokenKind::Greater, ">", line_};
        }

        if (std::isdigit(c)) {
            std::string num;
            while (pos_ < src_.size() && std::isdigit(src_[pos_]))
                num += src_[pos_++];
            if (pos_ < src_.size() && src_[pos_] == '.') {
                num += src_[pos_++];
                while (pos_ < src_.size() && std::isdigit(src_[pos_]))
                    num += src_[pos_++];
                return {TokenKind::Float, num, line_};
            }
            return {TokenKind::Number, num, line_};
        }

        if (std::isalpha(c) || c == '_') {
            std::string id;
            while (pos_ < src_.size() && (std::isalnum(src_[pos_]) || src_[pos_] == '_'))
                id += src_[pos_++];
            if (id == "and") return {TokenKind::And, "and", line_};
            if (id == "or")  return {TokenKind::Or,  "or",  line_};
            if (id == "not") return {TokenKind::Not, "not", line_};
            return {TokenKind::Ident, id, line_};
        }

        ++pos_;
        return {TokenKind::Error, std::string(1, c), line_};
    }
};

// ===== Section 3: AST =====

struct NumberExpr   { int64_t value; };
struct FloatExpr    { double value; };
struct VariableExpr { std::string name; };
struct BinaryExpr;
struct UnaryExpr;

struct ExprNode {
    std::variant<NumberExpr, FloatExpr, VariableExpr,
                 std::unique_ptr<BinaryExpr>,
                 std::unique_ptr<UnaryExpr>> data;
};
using ExprPtr = std::unique_ptr<ExprNode>;

struct BinaryExpr {
    std::string op;
    ExprPtr lhs, rhs;
};

struct UnaryExpr {
    std::string op;    // "not"
    ExprPtr operand;
};

struct AssignStmt { std::string name; ExprPtr value; };
struct CallStmt   { std::string callee; std::vector<ExprPtr> args; };
using StmtNode = std::variant<AssignStmt, CallStmt>;
using Program  = std::vector<StmtNode>;

// ===== Section 4: Parser =====

class Parser {
public:
    explicit Parser(Lexer &lex) : lex_(lex) {}

    Program parseProgram() {
        Program prog;
        skipNewlines();
        while (lex_.peek().kind != TokenKind::Eof) {
            prog.push_back(parseStatement());
            // expect newline or EOF after statement
            if (lex_.peek().kind == TokenKind::Newline)
                lex_.next();
            else if (lex_.peek().kind != TokenKind::Eof)
                throw std::runtime_error("line " + std::to_string(lex_.peek().line) +
                                         ": expected newline, got '" + lex_.peek().value + "'");
            skipNewlines();
        }
        return prog;
    }

private:
    Lexer &lex_;

    void skipNewlines() {
        while (lex_.peek().kind == TokenKind::Newline) lex_.next();
    }

    StmtNode parseStatement() {
        Token id = lex_.peek();
        if (id.kind != TokenKind::Ident)
            throw std::runtime_error("line " + std::to_string(id.line) +
                                     ": expected identifier, got '" + id.value + "'");
        lex_.next(); // consume ident

        Token next = lex_.peek();
        if (next.kind == TokenKind::Equals) {
            lex_.next(); // consume '='
            AssignStmt s;
            s.name  = id.value;
            s.value = parseLogicalOr();
            return s;
        } else if (next.kind == TokenKind::LParen) {
            lex_.next(); // consume '('
            CallStmt s;
            s.callee = id.value;
            if (lex_.peek().kind != TokenKind::RParen) {
                s.args.push_back(parseLogicalOr());
                while (lex_.peek().kind == TokenKind::Comma) {
                    lex_.next();
                    s.args.push_back(parseLogicalOr());
                }
            }
            if (lex_.peek().kind != TokenKind::RParen)
                throw std::runtime_error("line " + std::to_string(lex_.peek().line) +
                                         ": expected ')'");
            lex_.next(); // consume ')'
            return s;
        }
        throw std::runtime_error("line " + std::to_string(next.line) +
                                 ": expected '=' or '(' after identifier");
    }

    ExprPtr parseExpr() {
        ExprPtr lhs = parseTerm();
        while (lex_.peek().kind == TokenKind::Plus || lex_.peek().kind == TokenKind::Minus) {
            std::string op = lex_.next().value;
            ExprPtr rhs = parseTerm();
            auto bin = std::make_unique<BinaryExpr>();
            bin->op  = op;
            bin->lhs = std::move(lhs);
            bin->rhs = std::move(rhs);
            auto node = std::make_unique<ExprNode>();
            node->data = std::move(bin);
            lhs = std::move(node);
        }
        return lhs;
    }

    ExprPtr parseTerm() {
        ExprPtr lhs = parsePower();
        while (lex_.peek().kind == TokenKind::Star   ||
               lex_.peek().kind == TokenKind::Slash  ||
               lex_.peek().kind == TokenKind::SlashSlash ||
               lex_.peek().kind == TokenKind::Percent) {
            std::string op = lex_.next().value;
            ExprPtr rhs = parsePower();
            auto bin = std::make_unique<BinaryExpr>();
            bin->op  = op;
            bin->lhs = std::move(lhs);
            bin->rhs = std::move(rhs);
            auto node = std::make_unique<ExprNode>();
            node->data = std::move(bin);
            lhs = std::move(node);
        }
        return lhs;
    }

    ExprPtr parsePower() {
        ExprPtr lhs = parsePrimary();
        if (lex_.peek().kind == TokenKind::StarStar) {
            std::string op = lex_.next().value;
            ExprPtr rhs = parsePower();  // 右結合: 再帰呼び出し
            auto bin = std::make_unique<BinaryExpr>();
            bin->op  = op;
            bin->lhs = std::move(lhs);
            bin->rhs = std::move(rhs);
            auto node = std::make_unique<ExprNode>();
            node->data = std::move(bin);
            return node;
        }
        return lhs;
    }

    ExprPtr parsePrimary() {
        Token t = lex_.peek();
        // 単項 + / -
        if (t.kind == TokenKind::Plus || t.kind == TokenKind::Minus) {
            lex_.next();
            ExprPtr operand = parsePrimary(); // 右結合
            auto unary = std::make_unique<UnaryExpr>();
            unary->op = t.value;
            unary->operand = std::move(operand);
            auto node = std::make_unique<ExprNode>();
            node->data = std::move(unary);
            return node;
        }
        if (t.kind == TokenKind::Number) {
            lex_.next();
            auto node = std::make_unique<ExprNode>();
            node->data = NumberExpr{std::stoll(t.value)};
            return node;
        }
        if (t.kind == TokenKind::Float) {
            lex_.next();
            auto node = std::make_unique<ExprNode>();
            node->data = FloatExpr{std::stod(t.value)};
            return node;
        }
        if (t.kind == TokenKind::Ident) {
            lex_.next();
            auto node = std::make_unique<ExprNode>();
            node->data = VariableExpr{t.value};
            return node;
        }
        if (t.kind == TokenKind::LParen) {
            lex_.next();
            ExprPtr e = parseLogicalOr();
            if (lex_.peek().kind != TokenKind::RParen)
                throw std::runtime_error("line " + std::to_string(lex_.peek().line) +
                                         ": expected ')'");
            lex_.next();
            return e;
        }
        throw std::runtime_error("line " + std::to_string(t.line) +
                                 ": unexpected token '" + t.value + "'");
    }

    ExprPtr parseComparison() {
        ExprPtr lhs = parseExpr();
        for (;;) {
            TokenKind k = lex_.peek().kind;
            if (k != TokenKind::EqEq    && k != TokenKind::BangEq  &&
                k != TokenKind::Less    && k != TokenKind::LessEq  &&
                k != TokenKind::Greater && k != TokenKind::GreaterEq)
                break;
            std::string op = lex_.next().value;
            ExprPtr rhs = parseExpr();
            auto bin = std::make_unique<BinaryExpr>();
            bin->op  = op;
            bin->lhs = std::move(lhs);
            bin->rhs = std::move(rhs);
            auto node = std::make_unique<ExprNode>();
            node->data = std::move(bin);
            lhs = std::move(node);
        }
        return lhs;
    }

    ExprPtr parseLogicalNot() {
        if (lex_.peek().kind == TokenKind::Not) {
            lex_.next(); // consume 'not'
            ExprPtr operand = parseLogicalNot(); // 右結合（not not a が動く）
            auto unary = std::make_unique<UnaryExpr>();
            unary->op = "not";
            unary->operand = std::move(operand);
            auto node = std::make_unique<ExprNode>();
            node->data = std::move(unary);
            return node;
        }
        return parseComparison();
    }

    ExprPtr parseLogicalAnd() {
        ExprPtr lhs = parseLogicalNot();
        while (lex_.peek().kind == TokenKind::And) {
            std::string op = lex_.next().value;
            ExprPtr rhs = parseLogicalNot();
            auto bin = std::make_unique<BinaryExpr>();
            bin->op  = op;
            bin->lhs = std::move(lhs);
            bin->rhs = std::move(rhs);
            auto node = std::make_unique<ExprNode>();
            node->data = std::move(bin);
            lhs = std::move(node);
        }
        return lhs;
    }

    ExprPtr parseLogicalOr() {
        ExprPtr lhs = parseLogicalAnd();
        while (lex_.peek().kind == TokenKind::Or) {
            std::string op = lex_.next().value;
            ExprPtr rhs = parseLogicalAnd();
            auto bin = std::make_unique<BinaryExpr>();
            bin->op  = op;
            bin->lhs = std::move(lhs);
            bin->rhs = std::move(rhs);
            auto node = std::make_unique<ExprNode>();
            node->data = std::move(bin);
            lhs = std::move(node);
        }
        return lhs;
    }
};

// ===== Section 5: CodeGen =====

class CodeGen {
public:
    CodeGen() : ctx_(std::make_unique<LLVMContext>()),
                mod_(std::make_unique<Module>("ry", *ctx_)),
                builder_(*ctx_) {
        i64Ty_ = Type::getInt64Ty(*ctx_);
        i32Ty_ = Type::getInt32Ty(*ctx_);
        f64Ty_ = Type::getDoubleTy(*ctx_);
        i1Ty_  = Type::getInt1Ty(*ctx_);

        // Register built-in functions
        builtins_["print"] = [this](const std::vector<ExprPtr> &args) { emitPrint(args); };
    }

    ThreadSafeModule compile(Program &prog) {
        // Create entry function: i32 @__ry_main__()
        FunctionType *ft = FunctionType::get(i32Ty_, false);
        fn_ = Function::Create(ft, Function::ExternalLinkage, "__ry_main__", *mod_);
        BasicBlock *bb = BasicBlock::Create(*ctx_, "entry", fn_);
        builder_.SetInsertPoint(bb);

        for (auto &stmt : prog) {
            std::visit([this](auto &s) { emitStmt(s); }, stmt);
        }

        builder_.CreateRet(ConstantInt::get(i32Ty_, 0));

        std::string err;
        raw_string_ostream errStream(err);
        if (verifyFunction(*fn_, &errStream))
            throw std::runtime_error("IR verify error: " + err);

        return ThreadSafeModule(std::move(mod_), std::move(ctx_));
    }

private:
    std::unique_ptr<LLVMContext> ctx_;
    std::unique_ptr<Module> mod_;
    IRBuilder<> builder_;
    Function *fn_ = nullptr;
    Type *i64Ty_, *i32Ty_, *f64Ty_, *i1Ty_;
    std::unordered_map<std::string, AllocaInst*> vars_;
    using BuiltinFn = std::function<void(const std::vector<ExprPtr>&)>;
    std::unordered_map<std::string, BuiltinFn> builtins_;

    // Insert alloca at entry block header (mem2reg-friendly)
    AllocaInst *getOrCreateVar(const std::string &name, Type *ty) {
        auto it = vars_.find(name);
        if (it != vars_.end()) {
            if (it->second->getAllocatedType() == ty)
                return it->second;
        }
        IRBuilder<> entryBuilder(&fn_->getEntryBlock(),
                                  fn_->getEntryBlock().begin());
        AllocaInst *alloca = entryBuilder.CreateAlloca(ty, nullptr, name);
        vars_[name] = alloca;
        return alloca;
    }

    void emitStmt(AssignStmt &s) {
        Value *val = emitExpr(*s.value);
        AllocaInst *ptr = getOrCreateVar(s.name, val->getType());
        builder_.CreateStore(val, ptr);
    }

    void emitStmt(CallStmt &s) {
        auto it = builtins_.find(s.callee);
        if (it == builtins_.end())
            throw std::runtime_error("unknown function: " + s.callee);
        it->second(s.args);
    }

    Value *emitExpr(const ExprNode &node) {
        return std::visit([this](const auto &e) -> Value* { return emitExprVariant(e); },
                          node.data);
    }

    Value *emitExprVariant(const NumberExpr &e) {
        return ConstantInt::get(i64Ty_, e.value, true);
    }

    Value *emitExprVariant(const FloatExpr &e) {
        return ConstantFP::get(f64Ty_, e.value);
    }

    Value *emitExprVariant(const VariableExpr &e) {
        if (e.name == "true")  return ConstantInt::get(i1Ty_, 1, false);
        if (e.name == "false") return ConstantInt::get(i1Ty_, 0, false);
        auto it = vars_.find(e.name);
        if (it == vars_.end())
            throw std::runtime_error("undefined variable: " + e.name);
        AllocaInst *alloca = it->second;
        Type *ty = alloca->getAllocatedType();
        return builder_.CreateLoad(ty, alloca, e.name);
    }

    Value *emitExprVariant(const std::unique_ptr<UnaryExpr> &e) {
        Value *val = emitExpr(*e->operand);
        if (e->op == "+") {
            return val;
        }
        if (e->op == "-") {
            if (val->getType()->isDoubleTy())
                return builder_.CreateFNeg(val, "fneg");
            if (val->getType() == i1Ty_)
                val = builder_.CreateZExt(val, i64Ty_, "boolext");
            return builder_.CreateNeg(val, "neg");
        }
        if (e->op == "not") {
            auto toBool = [this](Value *v) -> Value* {
                if (v->getType() == i1Ty_)
                    return v;
                if (v->getType()->isDoubleTy())
                    return builder_.CreateFCmpONE(
                        v, ConstantFP::get(f64Ty_, 0.0), "ftobool");
                return builder_.CreateICmpNE(
                    v, ConstantInt::get(v->getType(), 0), "itobool");
            };
            Value *boolVal = toBool(val);
            return builder_.CreateNot(boolVal, "not"); // LLVM: xor i1 %v, true
        }
        throw std::runtime_error("unknown unary operator: " + e->op);
    }

    Value *emitExprVariant(const std::unique_ptr<BinaryExpr> &e) {
        Value *lhs = emitExpr(*e->lhs);
        Value *rhs = emitExpr(*e->rhs);
        const std::string &op = e->op;

        // ===== 比較演算子 =====
        if (op == "==" || op == "!=" || op == "<" ||
            op == "<=" || op == ">"  || op == ">=") {

            // i1（bool）は先に i64 に ZExt
            if (lhs->getType() == i1Ty_) lhs = builder_.CreateZExt(lhs, i64Ty_, "lhs_i");
            if (rhs->getType() == i1Ty_) rhs = builder_.CreateZExt(rhs, i64Ty_, "rhs_i");

            bool lf = lhs->getType()->isDoubleTy();
            bool rf = rhs->getType()->isDoubleTy();
            if (lf || rf) {
                if (!lf) lhs = builder_.CreateSIToFP(lhs, f64Ty_, "cmp_lf");
                if (!rf) rhs = builder_.CreateSIToFP(rhs, f64Ty_, "cmp_rf");
                CmpInst::Predicate pred;
                if      (op == "==") pred = CmpInst::FCMP_OEQ;
                else if (op == "!=") pred = CmpInst::FCMP_ONE;
                else if (op == "<")  pred = CmpInst::FCMP_OLT;
                else if (op == "<=") pred = CmpInst::FCMP_OLE;
                else if (op == ">")  pred = CmpInst::FCMP_OGT;
                else                 pred = CmpInst::FCMP_OGE;
                return builder_.CreateFCmp(pred, lhs, rhs, "fcmp");
            }
            CmpInst::Predicate pred;
            if      (op == "==") pred = CmpInst::ICMP_EQ;
            else if (op == "!=") pred = CmpInst::ICMP_NE;
            else if (op == "<")  pred = CmpInst::ICMP_SLT;
            else if (op == "<=") pred = CmpInst::ICMP_SLE;
            else if (op == ">")  pred = CmpInst::ICMP_SGT;
            else                 pred = CmpInst::ICMP_SGE;
            return builder_.CreateICmp(pred, lhs, rhs, "icmp");
        }
        // ===== 比較演算子ここまで =====

        // ===== 論理演算子 =====
        if (op == "and" || op == "or") {
            auto toBool = [this](Value *v) -> Value* {
                if (v->getType() == i1Ty_)
                    return v;
                if (v->getType()->isDoubleTy())
                    return builder_.CreateFCmpONE(
                        v, ConstantFP::get(f64Ty_, 0.0), "ftobool");
                return builder_.CreateICmpNE(
                    v, ConstantInt::get(v->getType(), 0), "itobool");
            };
            Value *lhsBool = toBool(lhs);
            Value *rhsBool = toBool(rhs);
            if (op == "and")
                return builder_.CreateAnd(lhsBool, rhsBool, "and");
            else
                return builder_.CreateOr(lhsBool, rhsBool, "or");
        }
        // ===== 論理演算子ここまで =====

        // ** 累乗: 常にf64、libmのpow()を呼ぶ
        if (op == "**") {
            if (lhs->getType()->isIntegerTy()) lhs = builder_.CreateSIToFP(lhs, f64Ty_, "lhs_f");
            if (rhs->getType()->isIntegerTy()) rhs = builder_.CreateSIToFP(rhs, f64Ty_, "rhs_f");
            FunctionType *powTy = FunctionType::get(f64Ty_, {f64Ty_, f64Ty_}, false);
            FunctionCallee powFn = mod_->getOrInsertFunction("pow", powTy);
            return builder_.CreateCall(powFn, {lhs, rhs}, "pow");
        }

        // // 整数除算: f64入力はi64に変換してからsdiv
        if (op == "//") {
            if (lhs->getType() == i1Ty_) lhs = builder_.CreateZExt(lhs, i64Ty_, "lhs_i");
            if (rhs->getType() == i1Ty_) rhs = builder_.CreateZExt(rhs, i64Ty_, "rhs_i");
            if (lhs->getType()->isDoubleTy()) lhs = builder_.CreateFPToSI(lhs, i64Ty_, "lhs_i");
            if (rhs->getType()->isDoubleTy()) rhs = builder_.CreateFPToSI(rhs, i64Ty_, "rhs_i");
            return builder_.CreateSDiv(lhs, rhs, "idiv");
        }

        // / 除算: 常にf64
        if (op == "/") {
            if (lhs->getType()->isIntegerTy()) lhs = builder_.CreateSIToFP(lhs, f64Ty_, "lhs_f");
            if (rhs->getType()->isIntegerTy()) rhs = builder_.CreateSIToFP(rhs, f64Ty_, "rhs_f");
            return builder_.CreateFDiv(lhs, rhs, "div");
        }

        // % 剰余: 片方f64ならfrem、両方i64ならsrem
        if (op == "%") {
            if (lhs->getType() == i1Ty_) lhs = builder_.CreateZExt(lhs, i64Ty_, "lhs_i");
            if (rhs->getType() == i1Ty_) rhs = builder_.CreateZExt(rhs, i64Ty_, "rhs_i");
            bool lf = lhs->getType()->isDoubleTy();
            bool rf = rhs->getType()->isDoubleTy();
            if (lf || rf) {
                if (!lf) lhs = builder_.CreateSIToFP(lhs, f64Ty_, "lhs_f");
                if (!rf) rhs = builder_.CreateSIToFP(rhs, f64Ty_, "rhs_f");
                return builder_.CreateFRem(lhs, rhs, "frem");
            }
            return builder_.CreateSRem(lhs, rhs, "srem");
        }

        // +/-/*: 片方f64なら浮動小数点命令
        if (lhs->getType() == i1Ty_) lhs = builder_.CreateZExt(lhs, i64Ty_, "lhs_i");
        if (rhs->getType() == i1Ty_) rhs = builder_.CreateZExt(rhs, i64Ty_, "rhs_i");
        bool lf = lhs->getType()->isDoubleTy();
        bool rf = rhs->getType()->isDoubleTy();
        if (lf || rf) {
            if (!lf) lhs = builder_.CreateSIToFP(lhs, f64Ty_, "lhs_f");
            if (!rf) rhs = builder_.CreateSIToFP(rhs, f64Ty_, "rhs_f");
            if (op == "+") return builder_.CreateFAdd(lhs, rhs, "fadd");
            if (op == "-") return builder_.CreateFSub(lhs, rhs, "fsub");
            if (op == "*") return builder_.CreateFMul(lhs, rhs, "fmul");
            throw std::runtime_error("unknown operator: " + op);
        }
        if (op == "+") return builder_.CreateAdd(lhs, rhs, "add");
        if (op == "-") return builder_.CreateSub(lhs, rhs, "sub");
        if (op == "*") return builder_.CreateMul(lhs, rhs, "mul");
        throw std::runtime_error("unknown operator: " + op);
    }

    void emitPrint(const std::vector<ExprPtr> &args) {
        if (args.size() != 1)
            throw std::runtime_error("print() takes exactly 1 argument");

        // Declare printf
        FunctionType *printfTy = FunctionType::get(
            i32Ty_, {PointerType::getUnqual(*ctx_)}, /*isVarArg=*/true);
        FunctionCallee printfFn = mod_->getOrInsertFunction("printf", printfTy);

        Value *val = emitExpr(*args[0]);

        // Bool 出力
        if (val->getType() == i1Ty_) {
            Constant *trueStr  = builder_.CreateGlobalString("true\n",  ".fmt_true");
            Constant *falseStr = builder_.CreateGlobalString("false\n", ".fmt_false");
            Value *fmtPtr = builder_.CreateSelect(val, trueStr, falseStr, "bool_fmt");
            builder_.CreateCall(printfFn, {fmtPtr});
            return;
        }

        Constant *fmt;
        if (val->getType()->isDoubleTy())
            fmt = builder_.CreateGlobalString("%g\n", ".fmt_f");
        else
            fmt = builder_.CreateGlobalString("%ld\n", ".fmt_i");

        builder_.CreateCall(printfFn, {fmt, val});
    }
};
