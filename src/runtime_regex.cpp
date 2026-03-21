#include "ry/runtime_regex.hpp"
#include <cstdio>
#include <cstdlib>
#include <cctype>
#include <cstring>
#include <string>
#include <vector>
#include <memory>

// ============================================================
// Regex AST
// ============================================================

namespace {

enum class RegexNodeKind {
    Literal,       // single character
    Dot,           // .
    CharClass,     // [abc], [a-z], [^0-9]
    Anchor,        // ^ or $
    WordBoundary,  // \b, \B
    Concat,        // AB
    Alternation,   // A|B
    Repeat,        // A*, A+, A?
    Group,         // (A)
};

struct RegexNode {
    RegexNodeKind kind;

    // Literal / Anchor
    char ch = 0;

    // CharClass
    bool negated = false;
    std::vector<std::pair<char, char>> ranges; // inclusive ranges

    // Repeat
    int repeatMin = 0;   // minimum repetitions
    int repeatMax = -1;  // maximum repetitions (-1 = unlimited)
    bool greedy = true;  // false = non-greedy (lazy)

    // Children
    std::vector<std::unique_ptr<RegexNode>> children;
};

using RegexNodePtr = std::unique_ptr<RegexNode>;

// Shared word-character ranges used by \w, \W, \b, \B
static const std::vector<std::pair<char, char>> WORD_CHAR_RANGES =
    {{'a','z'},{'A','Z'},{'0','9'},{'_','_'}};

// ============================================================
// Regex Parser (recursive descent)
// ============================================================

class RegexParser {
public:
    explicit RegexParser(const char *pattern) : src_(pattern), len_(strlen(pattern)), pos_(0) {}

    bool caseInsensitive() const { return caseInsensitive_; }

    RegexNodePtr parse() {
        auto node = parseAlternation();
        if (pos_ < len_) {
            fprintf(stderr, "regex error: unexpected character '%c' at position %zu in pattern '%s'\n",
                    src_[pos_], pos_, src_);
            exit(1);
        }
        return node;
    }

private:
    const char *src_;
    size_t len_;
    size_t pos_;
    bool caseInsensitive_ = false;

    char peek() const {
        if (pos_ >= len_) return '\0';
        return src_[pos_];
    }

    char advance() {
        return src_[pos_++];
    }

    bool atEnd() const {
        return pos_ >= len_;
    }

    RegexNodePtr parseAlternation() {
        auto left = parseConcat();
        if (peek() == '|') {
            auto node = std::make_unique<RegexNode>();
            node->kind = RegexNodeKind::Alternation;
            node->children.push_back(std::move(left));
            while (peek() == '|') {
                advance(); // consume '|'
                node->children.push_back(parseConcat());
            }
            return node;
        }
        return left;
    }

    RegexNodePtr parseConcat() {
        std::vector<RegexNodePtr> parts;
        while (!atEnd() && peek() != '|' && peek() != ')') {
            parts.push_back(parseRepeat());
        }
        if (parts.empty()) {
            // Empty concat = match empty string (literal with special handling)
            auto node = std::make_unique<RegexNode>();
            node->kind = RegexNodeKind::Concat;
            return node;
        }
        if (parts.size() == 1) return std::move(parts[0]);
        auto node = std::make_unique<RegexNode>();
        node->kind = RegexNodeKind::Concat;
        for (auto &p : parts) node->children.push_back(std::move(p));
        return node;
    }

    RegexNodePtr parseRepeat() {
        auto atom = parseAtom();
        if (atEnd()) return atom;

        int rmin = -1, rmax = -1;
        bool hasQuantifier = false;

        char c = peek();
        if (c == '*') {
            advance();
            rmin = 0; rmax = -1;
            hasQuantifier = true;
        } else if (c == '+') {
            advance();
            rmin = 1; rmax = -1;
            hasQuantifier = true;
        } else if (c == '?') {
            advance();
            rmin = 0; rmax = 1;
            hasQuantifier = true;
        } else if (c == '{') {
            size_t saved = pos_;
            if (parseQuantifierBrace(rmin, rmax)) {
                hasQuantifier = true;
            } else {
                pos_ = saved; // fallback: treat '{' as literal
            }
        }

        if (hasQuantifier) {
            auto node = std::make_unique<RegexNode>();
            node->kind = RegexNodeKind::Repeat;
            node->repeatMin = rmin;
            node->repeatMax = rmax;
            node->greedy = true;
            // Non-greedy suffix '?'
            if (!atEnd() && peek() == '?') {
                advance();
                node->greedy = false;
            }
            node->children.push_back(std::move(atom));
            return node;
        }
        return atom;
    }

    // Try to parse {n}, {n,}, {n,m}. Returns true on success.
    bool parseQuantifierBrace(int &rmin, int &rmax) {
        advance(); // consume '{'
        if (atEnd() || !std::isdigit(static_cast<unsigned char>(peek()))) return false;
        int n = parseNumber();
        if (n > 1000) {
            fprintf(stderr, "regex error: quantifier value %d exceeds maximum (1000) in pattern '%s'\n", n, src_);
            exit(1);
        }
        if (atEnd()) return false;
        if (peek() == '}') {
            advance();
            rmin = n; rmax = n;
            return true;
        }
        if (peek() == ',') {
            advance();
            if (atEnd()) return false;
            if (peek() == '}') {
                advance();
                rmin = n; rmax = -1;
                return true;
            }
            if (!std::isdigit(static_cast<unsigned char>(peek()))) return false;
            int m = parseNumber();
            if (m > 1000) {
                fprintf(stderr, "regex error: quantifier value %d exceeds maximum (1000) in pattern '%s'\n", m, src_);
                exit(1);
            }
            if (atEnd() || peek() != '}') return false;
            advance();
            if (n > m) {
                fprintf(stderr, "regex error: invalid quantifier {%d,%d} in pattern '%s'\n", n, m, src_);
                exit(1);
            }
            rmin = n; rmax = m;
            return true;
        }
        return false;
    }

    int parseNumber() {
        int val = 0;
        while (!atEnd() && std::isdigit(static_cast<unsigned char>(peek()))) {
            val = val * 10 + (advance() - '0');
            if (val > 1000) return val; // early exit on overflow
        }
        return val;
    }

    RegexNodePtr parseAtom() {
        char c = peek();
        if (c == '(') {
            advance(); // consume '('
            // Check for inline flags (?i)
            if (peek() == '?' && pos_ + 1 < len_ && src_[pos_ + 1] == 'i') {
                advance(); // consume '?'
                advance(); // consume 'i'
                if (peek() != ')') {
                    fprintf(stderr, "regex error: expected ')' after '(?i' in pattern '%s'\n", src_);
                    exit(1);
                }
                advance(); // consume ')'
                caseInsensitive_ = true;
                // Return next atom (flag is stateful, not a node)
                return parseAtom();
            }
            auto inner = parseAlternation();
            if (peek() != ')') {
                fprintf(stderr, "regex error: unmatched '(' in pattern '%s'\n", src_);
                exit(1);
            }
            advance(); // consume ')'
            auto node = std::make_unique<RegexNode>();
            node->kind = RegexNodeKind::Group;
            node->children.push_back(std::move(inner));
            return node;
        }
        if (c == '[') {
            return parseCharClass();
        }
        if (c == '.') {
            advance();
            auto node = std::make_unique<RegexNode>();
            node->kind = RegexNodeKind::Dot;
            return node;
        }
        if (c == '^' || c == '$') {
            advance();
            auto node = std::make_unique<RegexNode>();
            node->kind = RegexNodeKind::Anchor;
            node->ch = c;
            return node;
        }
        if (c == '\\') {
            advance(); // consume backslash
            if (atEnd()) {
                fprintf(stderr, "regex error: trailing backslash in pattern '%s'\n", src_);
                exit(1);
            }
            char escaped = advance();
            // Handle word boundary
            if (escaped == 'b' || escaped == 'B') {
                auto node = std::make_unique<RegexNode>();
                node->kind = RegexNodeKind::WordBoundary;
                node->negated = (escaped == 'B');
                return node;
            }
            // Handle shorthand character classes
            if (escaped == 'd' || escaped == 'D' ||
                escaped == 'w' || escaped == 'W' ||
                escaped == 's' || escaped == 'S') {
                return parseShorthandClass(escaped);
            }
            auto node = std::make_unique<RegexNode>();
            node->kind = RegexNodeKind::Literal;
            node->ch = escaped;
            return node;
        }
        if (c == '*' || c == '+' || c == '?') {
            fprintf(stderr, "regex error: nothing to repeat at position %zu in pattern '%s'\n", pos_, src_);
            exit(1);
        }
        if (atEnd() || c == '|' || c == ')') {
            fprintf(stderr, "regex error: unexpected end or character in pattern '%s'\n", src_);
            exit(1);
        }
        // Regular literal
        advance();
        auto node = std::make_unique<RegexNode>();
        node->kind = RegexNodeKind::Literal;
        node->ch = c;
        return node;
    }

    RegexNodePtr parseShorthandClass(char code) {
        auto node = std::make_unique<RegexNode>();
        node->kind = RegexNodeKind::CharClass;
        switch (code) {
            case 'd': node->negated = false; node->ranges = {{'0','9'}}; break;
            case 'D': node->negated = true;  node->ranges = {{'0','9'}}; break;
            case 'w': node->negated = false; node->ranges = WORD_CHAR_RANGES; break;
            case 'W': node->negated = true;  node->ranges = WORD_CHAR_RANGES; break;
            case 's': node->negated = false; node->ranges = {{' ',' '},{'\t','\t'},{'\n','\n'},{'\r','\r'},{'\f','\f'}}; break;
            case 'S': node->negated = true;  node->ranges = {{' ',' '},{'\t','\t'},{'\n','\n'},{'\r','\r'},{'\f','\f'}}; break;
        }
        return node;
    }

    RegexNodePtr parseCharClass() {
        advance(); // consume '['
        auto node = std::make_unique<RegexNode>();
        node->kind = RegexNodeKind::CharClass;
        node->negated = false;
        if (peek() == '^') {
            node->negated = true;
            advance();
        }
        while (!atEnd() && peek() != ']') {
            char lo = parseClassChar();
            if (peek() == '-' && pos_ + 1 < len_ && src_[pos_ + 1] != ']') {
                advance(); // consume '-'
                char hi = parseClassChar();
                node->ranges.push_back({lo, hi});
            } else {
                node->ranges.push_back({lo, lo});
            }
        }
        if (peek() != ']') {
            fprintf(stderr, "regex error: unmatched '[' in pattern '%s'\n", src_);
            exit(1);
        }
        advance(); // consume ']'
        return node;
    }

    char parseClassChar() {
        if (peek() == '\\') {
            advance();
            if (atEnd()) {
                fprintf(stderr, "regex error: trailing backslash in character class in pattern '%s'\n", src_);
                exit(1);
            }
            char c = advance();
            switch (c) {
                case 'n': return '\n';
                case 't': return '\t';
                case 'r': return '\r';
                default: return c;
            }
        }
        return advance();
    }
};

// ============================================================
// NFA
// ============================================================

struct NFAState {
    enum Kind { Match, Split, Char, Dot, CharClass, Anchor, WordBoundary };
    Kind kind;

    // Char
    char ch = 0;

    // CharClass
    bool negated = false;
    std::vector<std::pair<char, char>> ranges;

    // Transitions
    NFAState *out1 = nullptr;
    NFAState *out2 = nullptr; // only for Split

    // Generation counter for epsilon closure visited check (O(1) lookup)
    int visitGeneration = 0;
};

struct NFAFragment {
    NFAState *start;
    std::vector<NFAState **> danglingOuts; // pointers to patch
};

class NFABuilder {
public:
    NFABuilder() = default;
    ~NFABuilder() {
        for (auto *s : states_) delete s;
    }
    NFABuilder(NFABuilder &&other) noexcept : states_(std::move(other.states_)) {
        other.states_.clear();
    }
    NFABuilder &operator=(NFABuilder &&other) noexcept {
        if (this != &other) {
            for (auto *s : states_) delete s;
            states_ = std::move(other.states_);
            other.states_.clear();
        }
        return *this;
    }
    NFABuilder(const NFABuilder &) = delete;
    NFABuilder &operator=(const NFABuilder &) = delete;

    NFAState *newState(NFAState::Kind kind) {
        auto *s = new NFAState();
        s->kind = kind;
        states_.push_back(s);
        return s;
    }

    // Thompson's construction
    NFAFragment build(const RegexNode &node) {
        switch (node.kind) {
        case RegexNodeKind::Literal: {
            auto *s = newState(NFAState::Char);
            s->ch = node.ch;
            return {s, {&s->out1}};
        }
        case RegexNodeKind::Dot: {
            auto *s = newState(NFAState::Dot);
            return {s, {&s->out1}};
        }
        case RegexNodeKind::CharClass: {
            auto *s = newState(NFAState::CharClass);
            s->negated = node.negated;
            s->ranges = node.ranges;
            return {s, {&s->out1}};
        }
        case RegexNodeKind::Anchor: {
            auto *s = newState(NFAState::Anchor);
            s->ch = node.ch;
            return {s, {&s->out1}};
        }
        case RegexNodeKind::WordBoundary: {
            auto *s = newState(NFAState::WordBoundary);
            s->negated = node.negated;
            return {s, {&s->out1}};
        }
        case RegexNodeKind::Concat: {
            if (node.children.empty()) {
                // Empty concat: epsilon
                auto *s = newState(NFAState::Split);
                return {s, {&s->out1}};
            }
            auto frag = build(*node.children[0]);
            for (size_t i = 1; i < node.children.size(); ++i) {
                auto next = build(*node.children[i]);
                patch(frag, next.start);
                frag.danglingOuts = std::move(next.danglingOuts);
            }
            return frag;
        }
        case RegexNodeKind::Alternation: {
            if (node.children.size() == 1) return build(*node.children[0]);
            // Build pairwise: combine first two, then add rest
            auto left = build(*node.children[0]);
            for (size_t i = 1; i < node.children.size(); ++i) {
                auto right = build(*node.children[i]);
                auto *split = newState(NFAState::Split);
                split->out1 = left.start;
                split->out2 = right.start;
                std::vector<NFAState **> outs;
                outs.insert(outs.end(), left.danglingOuts.begin(), left.danglingOuts.end());
                outs.insert(outs.end(), right.danglingOuts.begin(), right.danglingOuts.end());
                left = {split, std::move(outs)};
            }
            return left;
        }
        case RegexNodeKind::Repeat: {
            int rmin = node.repeatMin;
            int rmax = node.repeatMax;
            bool gr = node.greedy;

            // Helper: configure split for greedy/non-greedy.
            // Sets the "preferred" out to inner, returns pointer to the "skip" out.
            auto configureSplit = [](NFAState *split, NFAState *inner, bool greedy) -> NFAState ** {
                if (greedy) {
                    split->out1 = inner;
                    return &split->out2;
                } else {
                    split->out2 = inner;
                    return &split->out1;
                }
            };

            // Step 1: Build min required copies concatenated
            NFAFragment result;
            bool hasResult = false;
            for (int i = 0; i < rmin; ++i) {
                auto copy = build(*node.children[0]);
                if (!hasResult) {
                    result = std::move(copy);
                    hasResult = true;
                } else {
                    patch(result, copy.start);
                    result.danglingOuts = std::move(copy.danglingOuts);
                }
            }

            if (rmax == -1) {
                // Step 2a: Unlimited → append Star loop
                auto loopInner = build(*node.children[0]);
                auto *split = newState(NFAState::Split);
                NFAState **skipOut = configureSplit(split, loopInner.start, gr);
                patch(loopInner, split);

                if (!hasResult) {
                    return {split, {skipOut}};
                } else {
                    patch(result, split);
                    result.danglingOuts = {skipOut};
                    return result;
                }
            } else {
                // Step 2b: Finite max → append (max - min) optional copies
                int optCount = rmax - rmin;
                for (int i = 0; i < optCount; ++i) {
                    auto optInner = build(*node.children[0]);
                    auto *split = newState(NFAState::Split);
                    NFAState **skipOut = configureSplit(split, optInner.start, gr);

                    if (!hasResult) {
                        std::vector<NFAState **> outs = {skipOut};
                        outs.insert(outs.end(), optInner.danglingOuts.begin(), optInner.danglingOuts.end());
                        result = {split, std::move(outs)};
                        hasResult = true;
                    } else {
                        patch(result, split);
                        result.danglingOuts = {skipOut};
                        result.danglingOuts.insert(result.danglingOuts.end(),
                            optInner.danglingOuts.begin(), optInner.danglingOuts.end());
                    }
                }

                if (!hasResult) {
                    // {0,0} = match empty
                    auto *s = newState(NFAState::Split);
                    return {s, {&s->out1}};
                }
                return result;
            }
        }
        case RegexNodeKind::Group:
            return build(*node.children[0]);
        }
        // unreachable
        fprintf(stderr, "regex internal error: unknown node kind\n");
        exit(1);
    }

    void patch(NFAFragment &frag, NFAState *target) {
        for (auto *p : frag.danglingOuts) *p = target;
        frag.danglingOuts.clear();
    }

private:
    std::vector<NFAState *> states_;
};

// ============================================================
// NFA Simulation
// ============================================================

class NFASimulator {
public:
    NFASimulator(NFAState *start, NFAState *matchState, bool caseInsensitive = false)
        : start_(start), matchState_(matchState), generation_(0), caseInsensitive_(caseInsensitive) {}

    // Try to match text[startPos..] returning the end position of the match,
    // or -1 if no match. fullMatch requires matching entire text.
    // preferShortest: return earliest match (for non-greedy patterns).
    int64_t simulate(const char *text, size_t textLen, size_t startPos,
                     bool fullMatch, bool preferShortest = false) {
        current_.clear();
        next_.clear();
        int64_t lastMatch = -1;

        ++generation_;
        addState(current_, start_, text, textLen, startPos);

        if (fullMatch) {
            if (stateSetContains(current_, matchState_)) {
                if (startPos == textLen) return (int64_t)startPos;
            }
        } else {
            if (stateSetContains(current_, matchState_)) {
                lastMatch = (int64_t)startPos;
                if (preferShortest) return lastMatch;
            }
        }

        for (size_t i = startPos; i < textLen; ++i) {
            char c = text[i];
            next_.clear();
            ++generation_;

            for (NFAState *s : current_) {
                if (s == matchState_) continue;
                bool matches = false;
                switch (s->kind) {
                case NFAState::Char:
                    if (caseInsensitive_) {
                        matches = (std::tolower((unsigned char)s->ch) == std::tolower((unsigned char)c));
                    } else {
                        matches = (s->ch == c);
                    }
                    break;
                case NFAState::Dot:
                    matches = (c != '\n');
                    break;
                case NFAState::CharClass:
                    matches = charClassMatches(s, c, caseInsensitive_);
                    break;
                default:
                    break;
                }
                if (matches && s->out1) {
                    addState(next_, s->out1, text, textLen, i + 1);
                }
            }

            current_.swap(next_);

            if (stateSetContains(current_, matchState_)) {
                if (fullMatch) {
                    if (i + 1 == textLen) return (int64_t)(i + 1);
                } else {
                    lastMatch = (int64_t)(i + 1);
                    if (preferShortest) return lastMatch;
                }
            }

            if (current_.empty()) break;
        }

        if (fullMatch) return -1;
        return lastMatch;
    }

private:
    NFAState *start_;
    NFAState *matchState_;
    std::vector<NFAState *> current_;
    std::vector<NFAState *> next_;
    int generation_;
    bool caseInsensitive_;

    void addState(std::vector<NFAState *> &stateSet, NFAState *s,
                  const char *text, size_t textLen, size_t pos) {
        if (!s || s->visitGeneration == generation_) return;
        s->visitGeneration = generation_;

        if (s->kind == NFAState::Split) {
            addState(stateSet, s->out1, text, textLen, pos);
            addState(stateSet, s->out2, text, textLen, pos);
            return;
        }
        if (s->kind == NFAState::Anchor) {
            if (s->ch == '^') {
                if (pos == 0) addState(stateSet, s->out1, text, textLen, pos);
            } else { // '$'
                if (pos == textLen) addState(stateSet, s->out1, text, textLen, pos);
            }
            return;
        }
        if (s->kind == NFAState::WordBoundary) {
            bool prevIsWord = (pos > 0) && isWordChar(text[pos - 1]);
            bool currIsWord = (pos < textLen) && isWordChar(text[pos]);
            bool atBoundary = (prevIsWord != currIsWord);
            if (atBoundary != s->negated) {
                addState(stateSet, s->out1, text, textLen, pos);
            }
            return;
        }
        stateSet.push_back(s);
    }

    static bool isWordChar(char c) {
        for (auto &[lo, hi] : WORD_CHAR_RANGES) {
            if (c >= lo && c <= hi) return true;
        }
        return false;
    }

    static bool stateSetContains(const std::vector<NFAState *> &stateSet, NFAState *target) {
        for (auto *s : stateSet) {
            if (s == target) return true;
        }
        return false;
    }

    static bool charInRange(char c, char lo, char hi, bool caseInsensitive) {
        if (c >= lo && c <= hi) return true;
        if (caseInsensitive) {
            char cl = std::tolower((unsigned char)c);
            char cu = std::toupper((unsigned char)c);
            return (cl >= lo && cl <= hi) || (cu >= lo && cu <= hi);
        }
        return false;
    }

    static bool charClassMatches(NFAState *s, char c, bool caseInsensitive = false) {
        bool inRange = false;
        for (auto &[lo, hi] : s->ranges) {
            if (charInRange(c, lo, hi, caseInsensitive)) { inRange = true; break; }
        }
        return s->negated ? !inRange : inRange;
    }
};

// ============================================================
// Compiled Regex wrapper
// ============================================================

struct CompiledRegex {
    NFABuilder builder;
    NFAState *matchState;
    NFAState *start;
    bool hasLazy_ = false;
    bool caseInsensitive_ = false;

    static bool detectLazy(const RegexNode &node) {
        if (node.kind == RegexNodeKind::Repeat && !node.greedy) return true;
        for (auto &child : node.children) {
            if (detectLazy(*child)) return true;
        }
        return false;
    }

    static CompiledRegex compile(const char *pattern) {
        RegexParser parser(pattern);
        auto ast = parser.parse();

        CompiledRegex cr;
        cr.hasLazy_ = detectLazy(*ast);
        cr.caseInsensitive_ = parser.caseInsensitive();
        auto frag = cr.builder.build(*ast);
        cr.matchState = cr.builder.newState(NFAState::Match);
        cr.builder.patch(frag, cr.matchState);
        cr.start = frag.start;
        return cr;
    }

    // Full match
    bool fullMatch(const char *text) {
        size_t len = strlen(text);
        NFASimulator sim(start, matchState, caseInsensitive_);
        return sim.simulate(text, len, 0, true) >= 0;
    }

    // Search: find first match, return {startPos, endPos} or {-1, -1}
    std::pair<int64_t, int64_t> search(const char *text) {
        size_t len = strlen(text);
        NFASimulator sim(start, matchState, caseInsensitive_);
        for (size_t i = 0; i <= len; ++i) {
            int64_t endPos = sim.simulate(text, len, i, false, hasLazy_);
            if (endPos >= 0) {
                return {(int64_t)i, endPos};
            }
        }
        return {-1, -1};
    }

    // Find all non-overlapping matches
    std::vector<std::pair<int64_t, int64_t>> findAll(const char *text) {
        size_t len = strlen(text);
        std::vector<std::pair<int64_t, int64_t>> results;
        NFASimulator sim(start, matchState, caseInsensitive_);
        size_t pos = 0;
        while (pos <= len) {
            int64_t endPos = sim.simulate(text, len, pos, false, hasLazy_);
            if (endPos >= 0) {
                results.push_back({(int64_t)pos, endPos});
                if ((size_t)endPos == pos) {
                    ++pos; // avoid infinite loop on zero-length match
                } else {
                    pos = (size_t)endPos;
                }
            } else {
                ++pos;
            }
        }
        return results;
    }
};

// ============================================================
// ListHeader layout: {i64 len, i64 cap, ptr data}
// ============================================================

struct ListHeader {
    int64_t len;
    int64_t cap;
    char **data;
};

static char *dupString(const char *s, size_t n) {
    char *buf = (char *)malloc(n + 1);
    memcpy(buf, s, n);
    buf[n] = '\0';
    return buf;
}

static ListHeader *makeStringList(const std::vector<std::string> &items) {
    auto *header = (ListHeader *)malloc(sizeof(ListHeader));
    header->len = (int64_t)items.size();
    header->cap = (int64_t)items.size();
    header->data = (char **)malloc(sizeof(char *) * items.size());
    for (size_t i = 0; i < items.size(); ++i) {
        header->data[i] = dupString(items[i].c_str(), items[i].size());
    }
    return header;
}

} // anonymous namespace

// ============================================================
// Public C API
// ============================================================

extern "C" {

int64_t __ry_regex_match(const char *pattern, const char *text) {
    auto cr = CompiledRegex::compile(pattern);
    return cr.fullMatch(text) ? 1 : 0;
}

int64_t __ry_regex_search(const char *pattern, const char *text) {
    auto cr = CompiledRegex::compile(pattern);
    auto result = cr.search(text);
    return result.first;
}

const char *__ry_regex_replace(const char *pattern, const char *text,
                                const char *replacement) {
    auto cr = CompiledRegex::compile(pattern);
    auto matches = cr.findAll(text);
    if (matches.empty()) {
        return dupString(text, strlen(text));
    }

    size_t textLen = strlen(text);
    size_t repLen = strlen(replacement);
    std::string result;
    result.reserve(textLen + matches.size() * repLen);
    size_t lastEnd = 0;
    for (auto &[start, end] : matches) {
        result.append(text + lastEnd, (size_t)start - lastEnd);
        result.append(replacement, repLen);
        lastEnd = (size_t)end;
    }
    result.append(text + lastEnd);
    return dupString(result.c_str(), result.size());
}

void *__ry_regex_split(const char *pattern, const char *text) {
    auto cr = CompiledRegex::compile(pattern);
    auto matches = cr.findAll(text);

    std::vector<std::string> parts;
    size_t lastEnd = 0;
    for (auto &[start, end] : matches) {
        parts.emplace_back(text + lastEnd, (size_t)start - lastEnd);
        lastEnd = (size_t)end;
    }
    parts.emplace_back(text + lastEnd);
    return makeStringList(parts);
}

void *__ry_regex_find_all(const char *pattern, const char *text) {
    auto cr = CompiledRegex::compile(pattern);
    auto matches = cr.findAll(text);

    std::vector<std::string> items;
    for (auto &[start, end] : matches) {
        items.emplace_back(text + start, (size_t)(end - start));
    }
    return makeStringList(items);
}

} // extern "C"
