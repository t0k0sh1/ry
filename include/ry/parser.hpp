#pragma once

#include "ry/lexer.hpp"
#include "ry/ast.hpp"
#include "ry/source_manager.hpp"

#include <cctype>
#include <cstring>
#include <initializer_list>
#include <utility>


namespace ry {

class Parser {
public:
    explicit Parser(Lexer &lex, const SourceManager *sm = nullptr, int fileId = 0)
        : lex_(lex), sm_(sm), file_id_(fileId) {}

    Program parseProgram();

private:
    Lexer &lex_;
    const SourceManager *sm_ = nullptr;
    int file_id_ = 0;
    int recursion_depth_ = 0;
    bool in_async_fn_ = false;
    static constexpr int MAX_RECURSION_DEPTH = 256;

    struct RecursionGuard {
        Parser &p_;
        explicit RecursionGuard(Parser &p) : p_(p) {
            if (p_.recursion_depth_ >= MAX_RECURSION_DEPTH)
                p_.parseError("expression nesting too deep (limit: " +
                              std::to_string(MAX_RECURSION_DEPTH) + ")");
            ++p_.recursion_depth_;
        }
        ~RecursionGuard() { --p_.recursion_depth_; }
        RecursionGuard(const RecursionGuard &) = delete;
        RecursionGuard &operator=(const RecursionGuard &) = delete;
    };

    // Error helpers
    [[noreturn]] void parseError(int line, const std::string &msg);
    [[noreturn]] void parseError(const std::string &msg);

    // Naming convention helpers
    static bool isSnakeCase(const std::string &name);
    static bool isMutationFnName(const std::string &name);
    static bool isScreamingSnakeCase(const std::string &name);
    static bool isPascalCase(const std::string &name);
    static void coerceFirstArgToString(std::vector<ExprPtr> &args);

    SourceLocation locFromToken(const Token &t) const { return {t.line, t.col, file_id_}; }

    std::vector<Directive> parseDirectives();
    void skipNewlines();
    void skipStructuralTokens();
    StmtNode parseImportStatement();
    StmtNode parseStatement();


    StmtNode parseIfStatement();
    StmtNode parseWhileStatement();
    StmtNode parseForStatement();
    StmtNode parseFnStatement(const std::vector<Directive> &directives, bool is_async = false);
    StmtNode parseRecordStatement();
    StmtNode parseTypeAliasStatement();
    StmtNode parseEnumStatement();
    StmtNode parseReturnStatement();
    StmtNode parseExpectStatement();
    StmtNode parseWhenStatement();
    StmtNode parseMatchStatement();
    void tryParseTrailingBlock(CallStmt &s);
    ExprPtr parseTrailingBlockAsLambda();
    ExprPtr parseWhenExpr();
    ExprPtr parseMatchExpr();
    Pattern parsePattern();
    void parseOrPattern(Pattern &pat);
    static bool patternHasBinding(const Pattern &p);
    TypeParam parseOneTypeParam();
    TypeNodePtr parseTypeName();
    TypeNodePtr parseTypeNameSingle();
    TypeNodePtr parseFnType();
    std::vector<StmtNode> parseBlock();
    std::vector<StmtNode> parseBlockOrInline();
    std::vector<ExprPtr> parseArgList();
    void parseContractClause(const std::string &clauseName, std::vector<ExprPtr> &out);
    void parseEnsureClause(FnStmt &fn);

    // Compound assignment helper: x op= rhs
    AssignStmt makeCompoundAssign(const Token &nameTok, const std::string &op, ExprPtr rhs);

    // Binary expression helper (left-associative)
    using ParseFn = ExprPtr (Parser::*)();
    ExprPtr parseBinaryLeft(ParseFn operand, std::initializer_list<TokenKind> ops);

    ExprPtr parseExpr();
    ExprPtr parseTerm();
    ExprPtr parsePower();
    ExprPtr parseCast();
    ExprPtr parsePostfix();
    ExprPtr makeErrorPropagateExpr(ExprPtr operand, const Token &tok);
    ExprPtr parsePrimary();
    ExprPtr parseComparison();
    ExprPtr parseBitwiseOr();
    ExprPtr parseBitwiseXor();
    ExprPtr parseBitwiseAnd();
    ExprPtr parseShift();
    ExprPtr parseLogicalNot();
    ExprPtr parseLogicalAnd();
    ExprPtr parseLogicalOr();
    ExprPtr parseConditional();
    ExprPtr parseNullCoalesce();
    ExprPtr parseRange();
    ExprPtr parseParenLambdaExpr();
    bool couldBeLambda();
    bool couldBeGenericEnum();
    TypeNodePtr parseCastTypeName();
    ExprPtr parseAwaitExpr();
};

// --- Shared numeric literal helpers ---

inline constexpr const char *kNumericSuffixes[] = {
    "i8", "i16", "i32", "i64",
    "u8", "u16", "u32", "u64",
    "f32", "f64"
};

inline std::pair<std::string, std::string> splitNumericSuffix(const std::string &s) {
    bool isHex = s.size() > 2 && s[0] == '0' && (s[1] == 'x' || s[1] == 'X');
    for (const char *suf : kNumericSuffixes) {
        // Skip f32/f64 for hex literals — 'f' is a valid hex digit
        if (isHex && suf[0] == 'f') continue;
        size_t len = std::strlen(suf);
        if (s.size() > len && s.compare(s.size() - len, len, suf) == 0) {
            char before = s[s.size() - len - 1];
            if (std::isxdigit(static_cast<unsigned char>(before)))
                return {s.substr(0, s.size() - len), suf};
        }
    }
    return {s, ""};
}

inline std::string stripUnderscores(const std::string &s) {
    std::string r;
    r.reserve(s.size());
    for (char c : s)
        if (c != '_') r += c;
    return r;
}

// Returns true if s is a valid in-range int64_t literal; sets *out on success.
inline bool tryParseIntLiteral(const std::string &s, int64_t *out) {
    std::string clean = stripUnderscores(s);
    errno = 0;
    char *end = nullptr;
    int base = 10;
    const char *p = clean.c_str();
    if (clean.size() > 2 && clean[0] == '0') {
        if (clean[1] == 'x' || clean[1] == 'X') base = 16;
        else if (clean[1] == 'b' || clean[1] == 'B') { base = 2; p += 2; }
    }
    long long val = std::strtoll(p, &end, base);
    if (errno == ERANGE || (end && *end != '\0')) return false;
    *out = static_cast<int64_t>(val);
    return true;
}

inline int64_t parseIntLiteral(const std::string &s) {
    int64_t val;
    if (!tryParseIntLiteral(s, &val))
        throw std::out_of_range("integer literal out of range for int: " + s);
    return val;
}

inline double parseFloatLiteral(const std::string &s) {
    return std::stod(stripUnderscores(s));
}

} // namespace ry
