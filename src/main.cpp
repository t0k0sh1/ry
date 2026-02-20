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
    Number, Ident, Plus, Minus, Star, Equals,
    LParen, RParen, Comma, Newline, Eof, Error
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
        if (c == '*') { ++pos_; return {TokenKind::Star,   "*", line_}; }
        if (c == '=') { ++pos_; return {TokenKind::Equals, "=", line_}; }
        if (c == '(') { ++pos_; return {TokenKind::LParen, "(", line_}; }
        if (c == ')') { ++pos_; return {TokenKind::RParen, ")", line_}; }
        if (c == ',') { ++pos_; return {TokenKind::Comma,  ",", line_}; }

        if (std::isdigit(c)) {
            std::string num;
            while (pos_ < src_.size() && std::isdigit(src_[pos_]))
                num += src_[pos_++];
            return {TokenKind::Number, num, line_};
        }

        if (std::isalpha(c) || c == '_') {
            std::string id;
            while (pos_ < src_.size() && (std::isalnum(src_[pos_]) || src_[pos_] == '_'))
                id += src_[pos_++];
            return {TokenKind::Ident, id, line_};
        }

        ++pos_;
        return {TokenKind::Error, std::string(1, c), line_};
    }
};

// ===== Section 3: AST =====

struct NumberExpr   { int64_t value; };
struct VariableExpr { std::string name; };
struct BinaryExpr;

struct ExprNode {
    std::variant<NumberExpr, VariableExpr, std::unique_ptr<BinaryExpr>> data;
};
using ExprPtr = std::unique_ptr<ExprNode>;

struct BinaryExpr {
    char op;
    ExprPtr lhs, rhs;
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
            s.value = parseExpr();
            return s;
        } else if (next.kind == TokenKind::LParen) {
            lex_.next(); // consume '('
            CallStmt s;
            s.callee = id.value;
            if (lex_.peek().kind != TokenKind::RParen) {
                s.args.push_back(parseExpr());
                while (lex_.peek().kind == TokenKind::Comma) {
                    lex_.next();
                    s.args.push_back(parseExpr());
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
            char op = lex_.next().value[0];
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
        ExprPtr lhs = parsePrimary();
        while (lex_.peek().kind == TokenKind::Star) {
            char op = lex_.next().value[0];
            ExprPtr rhs = parsePrimary();
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

    ExprPtr parsePrimary() {
        Token t = lex_.peek();
        if (t.kind == TokenKind::Number) {
            lex_.next();
            auto node = std::make_unique<ExprNode>();
            node->data = NumberExpr{std::stoll(t.value)};
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
            ExprPtr e = parseExpr();
            if (lex_.peek().kind != TokenKind::RParen)
                throw std::runtime_error("line " + std::to_string(lex_.peek().line) +
                                         ": expected ')'");
            lex_.next();
            return e;
        }
        throw std::runtime_error("line " + std::to_string(t.line) +
                                 ": unexpected token '" + t.value + "'");
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
    Type *i64Ty_, *i32Ty_;
    std::unordered_map<std::string, AllocaInst*> vars_;
    using BuiltinFn = std::function<void(const std::vector<ExprPtr>&)>;
    std::unordered_map<std::string, BuiltinFn> builtins_;

    // Insert alloca at entry block header (mem2reg-friendly)
    AllocaInst *getOrCreateVar(const std::string &name) {
        auto it = vars_.find(name);
        if (it != vars_.end()) return it->second;
        IRBuilder<> entryBuilder(&fn_->getEntryBlock(),
                                  fn_->getEntryBlock().begin());
        AllocaInst *alloca = entryBuilder.CreateAlloca(i64Ty_, nullptr, name);
        vars_[name] = alloca;
        return alloca;
    }

    void emitStmt(AssignStmt &s) {
        Value *val = emitExpr(*s.value);
        AllocaInst *ptr = getOrCreateVar(s.name);
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

    Value *emitExprVariant(const VariableExpr &e) {
        auto it = vars_.find(e.name);
        if (it == vars_.end())
            throw std::runtime_error("undefined variable: " + e.name);
        return builder_.CreateLoad(i64Ty_, it->second, e.name);
    }

    Value *emitExprVariant(const std::unique_ptr<BinaryExpr> &e) {
        Value *lhs = emitExpr(*e->lhs);
        Value *rhs = emitExpr(*e->rhs);
        switch (e->op) {
        case '+': return builder_.CreateAdd(lhs, rhs, "add");
        case '-': return builder_.CreateSub(lhs, rhs, "sub");
        case '*': return builder_.CreateMul(lhs, rhs, "mul");
        default:  throw std::runtime_error(std::string("unknown operator: ") + e->op);
        }
    }

    void emitPrint(const std::vector<ExprPtr> &args) {
        if (args.size() != 1)
            throw std::runtime_error("print() takes exactly 1 argument");

        // Declare printf
        FunctionType *printfTy = FunctionType::get(
            i32Ty_, {PointerType::getUnqual(*ctx_)}, /*isVarArg=*/true);
        FunctionCallee printfFn = mod_->getOrInsertFunction("printf", printfTy);

        // Format string "%ld\n"
        Constant *fmt = builder_.CreateGlobalString("%ld\n", ".fmt");

        Value *val = emitExpr(*args[0]);
        builder_.CreateCall(printfFn, {fmt, val});
    }
};

// ===== Section 6: main =====

int main(int argc, char *argv[]) {
    InitLLVM X(argc, argv);

    if (argc != 2) {
        errs() << "Usage: ry <file.ry>\n";
        return 1;
    }

    // Initialize native target
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();

    // Read source file
    auto bufOrErr = MemoryBuffer::getFile(argv[1]);
    if (!bufOrErr) {
        errs() << "Error reading file: " << argv[1] << "\n";
        return 1;
    }
    std::string src = (*bufOrErr)->getBuffer().str();

    try {
        // Lex -> Parse
        Lexer  lexer(src);
        Parser parser(lexer);
        Program prog = parser.parseProgram();

        // CodeGen -> ThreadSafeModule
        CodeGen cg;
        ThreadSafeModule tsm = cg.compile(prog);

        // Build LLJIT
        auto jitOrErr = LLJITBuilder().create();
        if (!jitOrErr) {
            errs() << "Failed to create JIT: ";
            logAllUnhandledErrors(jitOrErr.takeError(), errs());
            return 1;
        }
        auto &jit = *jitOrErr;

        // Expose process symbols (for printf etc.)
        auto &es = jit->getExecutionSession();
        auto &mainJD = jit->getMainJITDylib();
        auto dlsg = DynamicLibrarySearchGenerator::GetForCurrentProcess(
            jit->getDataLayout().getGlobalPrefix());
        if (!dlsg) {
            errs() << "Failed to create DynamicLibrarySearchGenerator: ";
            logAllUnhandledErrors(dlsg.takeError(), errs());
            return 1;
        }
        mainJD.addGenerator(std::move(*dlsg));

        // Add module
        if (auto err = jit->addIRModule(std::move(tsm))) {
            errs() << "Failed to add IR module: ";
            logAllUnhandledErrors(std::move(err), errs());
            return 1;
        }

        // Lookup and run __ry_main__
        auto symOrErr = jit->lookup("__ry_main__");
        if (!symOrErr) {
            errs() << "Failed to lookup __ry_main__: ";
            logAllUnhandledErrors(symOrErr.takeError(), errs());
            return 1;
        }
        auto *fn = symOrErr->toPtr<int(*)()>();
        fn();

    } catch (const std::exception &e) {
        errs() << "Error: " << e.what() << "\n";
        return 1;
    }

    return 0;
}
