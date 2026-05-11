#pragma once

#include "ry/lexer.hpp"
#include "ry/ast.hpp"
#include "ry/source_manager.hpp"

#include <cctype>
#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <initializer_list>
#include <stdexcept>
#include <unordered_set>
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
    // Set by parseParenLambdaExpr after the lambda is unambiguously committed
    // (i.e., past the closing ')' followed by '->', '=>', or ':'). Diagnostics
    // raised after this point must not be swallowed by the speculative
    // try/catch in parsePrimary's lambda dispatch.
    bool lambda_committed_ = false;
    // True while parsing the condition of an `if` expression. Suppresses the
    // bare-lambda dispatch (`Ident FatArrow`) in parsePrimary so the `=>` is
    // recognised as the if-expression then-arm, not consumed as a lambda body.
    bool in_if_cond_ = false;
    // Module names introduced by `import xxx` (qualified import) at the top
    // level of the current source. Used by parsePrimary's Dot dispatch to
    // disambiguate `mod.f(...)` qualified calls from UFCS / field access, and
    // by parseStatement to reject `let mod = ...` shadowing.
    std::unordered_set<std::string> imported_modules_;
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
    static bool isMutationFnName(const std::string &name);
    static bool isScreamingSnakeCase(const std::string &name);
    static bool isPascalCase(const std::string &name);
    static bool isCamelCase(const std::string &name);
    static void coerceFirstArgToString(std::vector<ExprPtr> &args);

    // After '.', a field/method name token may be an Ident, a numeric tuple
    // index, or any keyword from the lexer's keyword_map (e.g. `.and()`,
    // `.expect(...)`, `.case`). Shared between the expression-side
    // continuation (`parsePostfixContinuation`) and the statement-side dot
    // fast path in `parseStatement` so `import testing\ntesting.expect(...)`
    // parses identically in both positions.
    static bool isFieldNameTokenKind(TokenKind k);

    SourceLocation locFromToken(const Token &t) const { return {t.line, t.col, file_id_}; }

    std::vector<Directive> parseDirectives();
    void skipNewlines();
    void skipStructuralTokens();
    StmtNode parseImportStatement();
    StmtNode parseQualifiedImportStatement();
    void rejectImportShadowing(const Token &nameTok);
    StmtNode parseStatement();


    StmtNode parseIfStatement();
    StmtNode parseWhileStatement();
    StmtNode parseForStatement();
    StmtNode parseFnStatement(const std::vector<Directive> &directives, bool is_async = false);
    StmtNode parseDirectiveDefStatement(const Directive *dirAnnot,
                                        std::vector<Directive> directives);
    StmtNode parseRecordStatement();
    StmtNode parseTypeAliasStatement();
    StmtNode parseEnumStatement();
    StmtNode parseReturnStatement();
    StmtNode parseExpectStatement();
    StmtNode parseCaseStatement();
    StmtNode parseCaseStatementNoSubject(const Token &caseTok);
    StmtNode parseCaseStatementWithSubject(const Token &caseTok);
    void tryParseTrailingBlock(CallStmt &s);
    ExprPtr parseTrailingBlockAsLambda();
    ExprPtr parseCaseExpr();
    ExprPtr parseCaseExprNoSubject(const Token &caseTok);
    ExprPtr parseCaseExprWithSubject(const Token &caseTok);
    ExprPtr parseIfExpression();
    std::vector<StmtNode> parseIfExpressionBranchBody();
    Pattern parsePattern();
    void parseOrPattern(Pattern &pat);
    static bool patternHasBinding(const Pattern &p);
    void validateForBindingPattern(const Pattern &p);
    TypeParam parseOneTypeParam();
    TypeNodePtr parseTypeName();
    TypeNodePtr parseTypeNameSingle();
    TypeNodePtr parseFnType();
    std::vector<StmtNode> parseBlock();
    std::vector<StmtNode> parseBlockOrInline();
    std::vector<ExprPtr> parseArgList(std::vector<NamedArg> *named_out = nullptr);
    void parseContractClause(const std::string &clauseName, std::vector<ExprPtr> &out);
    void parseEnsureClause(FnStmt &fn);

    // Compound assignment helper: x op= rhs
    AssignStmt makeCompoundAssign(const Token &nameTok, const std::string &op, ExprPtr rhs);

    // Finalize a chained LHS (postfix chain rooted at an Ident) into an
    // IndexAssignStmt / FieldAssignStmt / AssignStmt / ExprStmt based on the
    // trailing token and the chain tail node. Used by parseStatement for the
    // new `LBracket` / `Dot` paths that may traverse multiple postfix hops.
    StmtNode finishChainedLhs(ExprPtr chain, const Token &first);

    // Binary expression helper (left-associative)
    using ParseFn = ExprPtr (Parser::*)();
    ExprPtr parseBinaryLeft(ParseFn operand, std::initializer_list<TokenKind> ops);

    ExprPtr parseExpr();
    ExprPtr parseTerm();
    ExprPtr parsePower();
    ExprPtr parseCast();
    ExprPtr parsePostfix();
    ExprPtr parsePostfixContinuation(ExprPtr expr);
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
    // Lookahead predicate for `(a, b, ...) = expr` statement form (#1189).
    // Returns true only when the current '(' is followed by `Ident (Comma Ident)+ RParen Equals`,
    // requiring at least two names to avoid ambiguity with grouping and single-name forms.
    bool looksLikeParenthesizedTupleDestructure();
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

// Parses a non-negative integer literal in the range [0, UINT64_MAX].
// The value is stored in `*out` as a bit pattern reinterpreted into int64_t
// (e.g. UINT64_MAX is stored as int64_t(-1)); codegen re-interprets the bit
// pattern according to the literal's suffix or surrounding annotation type.
// Negative literals arrive as UnaryExpr(-, NumberExpr{...}), so this function
// only sees the unsigned magnitude.
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
    unsigned long long val = std::strtoull(p, &end, base);
    if (errno == ERANGE || (end && *end != '\0')) return false;
    *out = static_cast<int64_t>(static_cast<uint64_t>(val));
    return true;
}

inline int64_t parseIntLiteral(const std::string &s) {
    int64_t val;
    if (!tryParseIntLiteral(s, &val))
        throw std::out_of_range("integer literal out of range for int: " + s);
    return val;
}

inline double parseFloatLiteral(const std::string &s) {
    // Use strtod (not stod) so overflow yields +/-HUGE_VAL instead of
    // throwing out_of_range — this matches C99 and lets `1e400` surface as
    // +Inf consistent with the runtime `to_float`.
    std::string clean = stripUnderscores(s);
    errno = 0;
    char *end = nullptr;
    double val = std::strtod(clean.c_str(), &end);
    // Any trailing garbage indicates a malformed literal (a lexer bug).
    if (end == clean.c_str() || (end && *end != '\0'))
        throw std::out_of_range("invalid float literal: " + s);
    return val;
}

} // namespace ry
