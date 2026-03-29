#include "ry/runtime_regex.hpp"
#include "ry/runtime_regex_internal.hpp"
#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <cctype>
#include <cstring>
#include <string>
#include <vector>
#include <memory>

namespace {

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
    int64_t visitGeneration = 0;

    // For single-pass search: tracks the text position where this match attempt started
    size_t matchStartPos = 0;
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
    static constexpr int64_t MAX_STEPS = 10'000'000;

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

        int64_t steps = 0;
        for (size_t i = startPos; i < textLen; ++i) {
            char c = text[i];
            next_.clear();
            ++generation_;

            for (NFAState *s : current_) {
                if (++steps > MAX_STEPS) return -1;
                if (s == matchState_) continue;
                if (stateMatchesChar(s, c) && s->out1) {
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

    // Single-pass search: find the first (leftmost) match in O(n*s) time
    // instead of O(n^2) by injecting start state at every position during
    // a single forward scan.
    std::pair<int64_t, int64_t> searchSinglePass(
            const char *text, size_t textLen, bool preferShortest) {
        current_.clear();
        next_.clear();
        int64_t bestStart = -1, bestEnd = -1;
        int64_t steps = 0;

        for (size_t i = 0; i <= textLen; ++i) {
            // Inject start state for a new match attempt at position i.
            // Mark existing states as visited so addState won't duplicate them
            // (they have earlier/equal matchStartPos, which is preferred).
            ++generation_;
            for (NFAState *s : current_) {
                if (++steps > MAX_STEPS) return {bestStart, bestEnd};
                s->visitGeneration = generation_;
            }
            addState(current_, start_, text, textLen, i, i);

            // Check for match state in current set
            for (NFAState *s : current_) {
                if (s == matchState_) {
                    int64_t S = (int64_t)s->matchStartPos;
                    int64_t E = (int64_t)i;
                    if (bestStart == -1 || S < bestStart ||
                        (S == bestStart && !preferShortest && E > bestEnd)) {
                        bestStart = S;
                        bestEnd = E;
                    }
                    if (preferShortest && bestStart >= 0) {
                        // Only return once no active thread has an earlier
                        // start position — preserves leftmost-start semantics.
                        bool hasEarlierStart = false;
                        for (NFAState *t : current_) {
                            if (t != matchState_ &&
                                (int64_t)t->matchStartPos < bestStart) {
                                hasEarlierStart = true;
                                break;
                            }
                        }
                        if (!hasEarlierStart) {
                            return {bestStart, bestEnd};
                        }
                    }
                    break;
                }
            }

            // Early termination for greedy: once we have a leftmost match and
            // no thread from that start position remains, we're done.
            if (!preferShortest && bestStart >= 0) {
                if (!hasActiveThreadFrom((size_t)bestStart)) {
                    return {bestStart, bestEnd};
                }
            }

            if (i == textLen) break;

            // Step: consume text[i], build next state set
            char c = text[i];
            next_.clear();
            ++generation_;

            for (NFAState *s : current_) {
                if (++steps > MAX_STEPS) return {bestStart, bestEnd};
                if (s == matchState_) continue;
                if (stateMatchesChar(s, c) && s->out1) {
                    addState(next_, s->out1, text, textLen, i + 1,
                             s->matchStartPos);
                }
            }

            current_.swap(next_);

            if (current_.empty() && bestStart >= 0) {
                return {bestStart, bestEnd};
            }
        }

        return {bestStart, bestEnd};
    }

    // Single-pass findAll: find all non-overlapping matches in O(n*s) time.
    std::vector<std::pair<int64_t, int64_t>> findAllSinglePass(
            const char *text, size_t textLen, bool preferShortest) {
        std::vector<std::pair<int64_t, int64_t>> results;
        current_.clear();
        next_.clear();
        size_t discardBefore = 0;

        // Track pending match: the best match found for the current leftmost
        // start position, not yet emitted (for greedy, we wait for longest).
        int64_t pendingStart = -1, pendingEnd = -1;

        for (size_t i = 0; i <= textLen; ++i) {
            // Prune threads starting before discardBefore
            if (discardBefore > 0) {
                current_.erase(
                    std::remove_if(current_.begin(), current_.end(),
                        [&](NFAState *s) {
                            return s->matchStartPos < discardBefore;
                        }),
                    current_.end());
            }

            // Inject start state for position i (only if >= discardBefore)
            ++generation_;
            for (NFAState *s : current_) {
                s->visitGeneration = generation_;
            }
            if (i >= discardBefore) {
                addState(current_, start_, text, textLen, i, i);
            }

            // Check for match state
            for (NFAState *s : current_) {
                if (s == matchState_) {
                    int64_t S = (int64_t)s->matchStartPos;
                    int64_t E = (int64_t)i;

                    if (preferShortest) {
                        // Non-greedy: record as pending, emit only once
                        // no earlier-start thread is active (leftmost-first).
                        if (pendingStart == -1 || S < pendingStart) {
                            pendingStart = S;
                            pendingEnd = E;
                        }
                    } else {
                        // Greedy: record as pending, wait for longest
                        if (pendingStart == -1 || S < pendingStart ||
                            (S == pendingStart && E > pendingEnd)) {
                            pendingStart = S;
                            pendingEnd = E;
                        }
                    }
                    break;
                }
            }

            // Emit pending match once no earlier-start thread can
            // produce a better result (leftmost-first semantics).
            if (pendingStart >= 0) {
                bool canEmit;
                if (preferShortest) {
                    // Non-greedy: emit once no thread has an earlier start
                    bool hasEarlierStart = false;
                    for (NFAState *s : current_) {
                        if (s != matchState_ &&
                            (int64_t)s->matchStartPos < pendingStart) {
                            hasEarlierStart = true;
                            break;
                        }
                    }
                    canEmit = !hasEarlierStart;
                } else {
                    // Greedy: emit once no thread from the same start remains
                    canEmit = !hasActiveThreadFrom((size_t)pendingStart);
                }
                if (canEmit) {
                    results.push_back({pendingStart, pendingEnd});
                    if (pendingEnd == pendingStart) {
                        discardBefore = (size_t)pendingStart + 1;
                    } else {
                        discardBefore = (size_t)pendingEnd;
                    }
                    pendingStart = -1;
                    pendingEnd = -1;
                }
            }

            if (i == textLen) break;

            // Step: consume text[i]
            char c = text[i];
            next_.clear();
            ++generation_;

            for (NFAState *s : current_) {
                if (s == matchState_) continue;
                // Skip threads from already-emitted match regions
                // (discardBefore may have changed mid-iteration)
                if (s->matchStartPos < discardBefore) continue;
                if (stateMatchesChar(s, c) && s->out1) {
                    addState(next_, s->out1, text, textLen, i + 1,
                             s->matchStartPos);
                }
            }

            current_.swap(next_);
        }

        // Emit any remaining pending match
        if (pendingStart >= 0) {
            results.push_back({pendingStart, pendingEnd});
        }

        return results;
    }

private:
    NFAState *start_;
    NFAState *matchState_;
    std::vector<NFAState *> current_;
    std::vector<NFAState *> next_;
    int64_t generation_;
    bool caseInsensitive_;

    void addState(std::vector<NFAState *> &stateSet, NFAState *s,
                  const char *text, size_t textLen, size_t pos,
                  size_t matchStartPos = 0) {
        if (!s || s->visitGeneration == generation_) return;
        s->visitGeneration = generation_;
        s->matchStartPos = matchStartPos;

        if (s->kind == NFAState::Split) {
            addState(stateSet, s->out1, text, textLen, pos, matchStartPos);
            addState(stateSet, s->out2, text, textLen, pos, matchStartPos);
            return;
        }
        if (s->kind == NFAState::Anchor) {
            if (s->ch == '^') {
                if (pos == 0) addState(stateSet, s->out1, text, textLen, pos, matchStartPos);
            } else { // '$'
                if (pos == textLen) addState(stateSet, s->out1, text, textLen, pos, matchStartPos);
            }
            return;
        }
        if (s->kind == NFAState::WordBoundary) {
            bool prevIsWord = (pos > 0) && isWordChar(text[pos - 1]);
            bool currIsWord = (pos < textLen) && isWordChar(text[pos]);
            bool atBoundary = (prevIsWord != currIsWord);
            if (atBoundary != s->negated) {
                addState(stateSet, s->out1, text, textLen, pos, matchStartPos);
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

    bool stateMatchesChar(NFAState *s, char c) const {
        switch (s->kind) {
        case NFAState::Char:
            if (caseInsensitive_) {
                return std::tolower((unsigned char)s->ch) ==
                       std::tolower((unsigned char)c);
            }
            return s->ch == c;
        case NFAState::Dot:
            return c != '\n';
        case NFAState::CharClass:
            return charClassMatches(s, c, caseInsensitive_);
        default:
            return false;
        }
    }

    bool hasActiveThreadFrom(size_t startPos) const {
        for (NFAState *s : current_) {
            if (s != matchState_ && s->matchStartPos == startPos) {
                return true;
            }
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
        return sim.searchSinglePass(text, len, hasLazy_);
    }

    // Find all non-overlapping matches
    std::vector<std::pair<int64_t, int64_t>> findAll(const char *text) {
        size_t len = strlen(text);
        NFASimulator sim(start, matchState, caseInsensitive_);
        return sim.findAllSinglePass(text, len, hasLazy_);
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
    if (!pattern || !text) return 0;
    auto cr = CompiledRegex::compile(pattern);
    return cr.fullMatch(text) ? 1 : 0;
}

int64_t __ry_regex_search(const char *pattern, const char *text) {
    if (!pattern || !text) return -1;
    auto cr = CompiledRegex::compile(pattern);
    auto result = cr.search(text);
    return result.first;
}

const char *__ry_regex_replace(const char *pattern, const char *text,
                                const char *replacement) {
    if (!pattern || !text || !replacement) return text ? dupString(text, strlen(text)) : dupString("", 0);
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
    if (!pattern || !text) { std::vector<std::string> empty; return makeStringList(empty); }
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
