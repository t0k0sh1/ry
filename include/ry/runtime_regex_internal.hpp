#pragma once

#include <cstdio>
#include <cstdlib>
#include <cctype>
#include <cstring>
#include <memory>
#include <vector>


namespace ry {

// ============================================================
// Regex AST
// ============================================================

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

    // Group (1-based index assigned by parser; -1 = no capture)
    int groupIndex = -1;

    // Children
    std::vector<std::unique_ptr<RegexNode>> children;
};

using RegexNodePtr = std::unique_ptr<RegexNode>;

// Shared word-character ranges used by \w, \W, \b, \B
inline const std::vector<std::pair<char, char>> WORD_CHAR_RANGES =
    {{'a','z'},{'A','Z'},{'0','9'},{'_','_'}};

// ============================================================
// Regex Parser (recursive descent)
// ============================================================

class RegexParser {
public:
    RegexParser(const char *pattern, size_t len);

    bool caseInsensitive() const { return caseInsensitive_; }
    int groupCount() const { return groupCount_; }

    RegexNodePtr parse();

private:
    const char *src_;
    size_t len_;
    size_t pos_;
    bool caseInsensitive_ = false;
    int groupDepth_ = 0;
    int groupCount_ = 0;
    static constexpr int MAX_GROUP_DEPTH = 50;

    char peek() const;
    char advance();
    bool atEnd() const;

    RegexNodePtr parseAlternation();
    RegexNodePtr parseConcat();
    RegexNodePtr parseRepeat();
    bool parseQuantifierBrace(int &rmin, int &rmax);
    int parseNumber();
    RegexNodePtr parseAtom();
    RegexNodePtr parseShorthandClass(char code);
    RegexNodePtr parseCharClass();
    char parseClassChar();
};

} // namespace ry
