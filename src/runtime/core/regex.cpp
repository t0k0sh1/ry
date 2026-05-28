#include "ry/runtime/core/regex.hpp"
#include "ry/runtime/core/alloc.hpp"
#include "ry/runtime/core/regex_error_sentinels.hpp"
#include "ry/runtime/core/error.hpp"
#include "ry/runtime/core/regex_internal.hpp"
#include "ry/runtime/core/list.hpp"
#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <cctype>
#include <cstring>
#include <limits>
#include <set>
#include <string>
#include <vector>
#include <memory>
#include <stdexcept>


namespace ry {

DEFINE_LAST_ERROR(regex)

namespace {

// ============================================================
// NFA
// ============================================================

struct NFAState {
    enum class Kind { Match, Split, Char, Dot, CharClass, Anchor, WordBoundary,
                      GroupOpen, GroupClose };
    Kind kind = Kind::Match;

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

    // GroupOpen / GroupClose: 1-based capture group index
    int groupIndex = 0;
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
            auto *s = newState(NFAState::Kind::Char);
            s->ch = node.ch;
            return {s, {&s->out1}};
        }
        case RegexNodeKind::Dot: {
            auto *s = newState(NFAState::Kind::Dot);
            return {s, {&s->out1}};
        }
        case RegexNodeKind::CharClass: {
            auto *s = newState(NFAState::Kind::CharClass);
            s->negated = node.negated;
            s->ranges = node.ranges;
            return {s, {&s->out1}};
        }
        case RegexNodeKind::Anchor: {
            auto *s = newState(NFAState::Kind::Anchor);
            s->ch = node.ch;
            return {s, {&s->out1}};
        }
        case RegexNodeKind::WordBoundary: {
            auto *s = newState(NFAState::Kind::WordBoundary);
            s->negated = node.negated;
            return {s, {&s->out1}};
        }
        case RegexNodeKind::Concat: {
            if (node.children.empty()) {
                // Empty concat: epsilon
                auto *s = newState(NFAState::Kind::Split);
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
                auto *split = newState(NFAState::Kind::Split);
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
                auto *split = newState(NFAState::Kind::Split);
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
                    auto *split = newState(NFAState::Kind::Split);
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
                    auto *s = newState(NFAState::Kind::Split);
                    return {s, {&s->out1}};
                }
                return result;
            }
        }
        case RegexNodeKind::Group: {
            if (node.groupIndex > 0) {
                auto *open = newState(NFAState::Kind::GroupOpen);
                open->groupIndex = node.groupIndex;
                auto inner = build(*node.children[0]);
                auto *close = newState(NFAState::Kind::GroupClose);
                close->groupIndex = node.groupIndex;
                open->out1 = inner.start;
                patch(inner, close);
                return {open, {&close->out1}};
            }
            return build(*node.children[0]);
        }
        }
        // unreachable
        fprintf(stderr, "regex internal error: unknown node kind\n");
        ry_runtime_trap_exit();
    }

    void patch(NFAFragment &frag, NFAState *target) {
        for (auto *p : frag.danglingOuts) *p = target;
        frag.danglingOuts.clear();
    }

private:
    std::vector<NFAState *> states_;
};

// ============================================================
// Shared character-matching helpers (used by NFASimulator and CaptureBacktracker)
// ============================================================

static bool nfaIsWordChar(char c) {
    for (auto &[lo, hi] : WORD_CHAR_RANGES) {
        if (c >= lo && c <= hi) return true;
    }
    return false;
}

static bool nfaCharInRange(char c, char lo, char hi, bool caseInsensitive) {
    if (c >= lo && c <= hi) return true;
    if (caseInsensitive) {
        char cl = static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
        char cu = static_cast<char>(std::toupper(static_cast<unsigned char>(c)));
        return (cl >= lo && cl <= hi) || (cu >= lo && cu <= hi);
    }
    return false;
}

static bool nfaCharClassMatches(NFAState *s, char c, bool caseInsensitive) {
    bool inRange = false;
    for (auto &[lo, hi] : s->ranges) {
        if (nfaCharInRange(c, lo, hi, caseInsensitive)) { inRange = true; break; }
    }
    return s->negated ? !inRange : inRange;
}

static bool nfaStateMatchesChar(NFAState *s, char c, bool caseInsensitive) {
    switch (s->kind) {
    case NFAState::Kind::Char:
        if (caseInsensitive) {
            return std::tolower((unsigned char)s->ch) ==
                   std::tolower((unsigned char)c);
        }
        return s->ch == c;
    case NFAState::Kind::Dot:
        return c != '\n';
    case NFAState::Kind::CharClass:
        return nfaCharClassMatches(s, c, caseInsensitive);
    default:
        return false;
    }
}

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
                    if (i + 1 == textLen) return static_cast<int64_t>(i) + 1;
                } else {
                    lastMatch = static_cast<int64_t>(i) + 1;
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

        if (s->kind == NFAState::Kind::Split) {
            addState(stateSet, s->out1, text, textLen, pos, matchStartPos);
            addState(stateSet, s->out2, text, textLen, pos, matchStartPos);
            return;
        }
        if (s->kind == NFAState::Kind::GroupOpen ||
            s->kind == NFAState::Kind::GroupClose) {
            addState(stateSet, s->out1, text, textLen, pos, matchStartPos);
            return;
        }
        if (s->kind == NFAState::Kind::Anchor) {
            if (s->ch == '^') {
                if (pos == 0) addState(stateSet, s->out1, text, textLen, pos, matchStartPos);
            } else { // '$'
                if (pos == textLen) addState(stateSet, s->out1, text, textLen, pos, matchStartPos);
            }
            return;
        }
        if (s->kind == NFAState::Kind::WordBoundary) {
            bool prevIsWord = (pos > 0) && nfaIsWordChar(text[pos - 1]);
            bool currIsWord = (pos < textLen) && nfaIsWordChar(text[pos]);
            bool atBoundary = (prevIsWord != currIsWord);
            if (atBoundary != s->negated) {
                addState(stateSet, s->out1, text, textLen, pos, matchStartPos);
            }
            return;
        }
        stateSet.push_back(s);
    }

    bool stateMatchesChar(NFAState *s, char c) const {
        return nfaStateMatchesChar(s, c, caseInsensitive_);
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
    int groupCount_ = 0;
    int groupCount() const { return groupCount_; }

    static bool detectLazy(const RegexNode &node) {
        if (node.kind == RegexNodeKind::Repeat && !node.greedy) return true;
        for (auto &child : node.children) {
            if (detectLazy(*child)) return true;
        }
        return false;
    }

    static CompiledRegex compile(const char *pattern, size_t patternLen) {
        RegexParser parser(pattern, patternLen);
        auto ast = parser.parse();

        CompiledRegex cr;
        cr.hasLazy_ = detectLazy(*ast);
        cr.caseInsensitive_ = parser.caseInsensitive();
        cr.groupCount_ = parser.groupCount();
        auto frag = cr.builder.build(*ast);
        cr.matchState = cr.builder.newState(NFAState::Kind::Match);
        cr.builder.patch(frag, cr.matchState);
        cr.start = frag.start;
        return cr;
    }

    // Full match
    bool fullMatch(const char *text, size_t textLen) {
        NFASimulator sim(start, matchState, caseInsensitive_);
        return sim.simulate(text, textLen, 0, true) >= 0;
    }

    // Search: find first match, return {startPos, endPos} or {-1, -1}
    std::pair<int64_t, int64_t> search(const char *text, size_t textLen) {
        NFASimulator sim(start, matchState, caseInsensitive_);
        return sim.searchSinglePass(text, textLen, hasLazy_);
    }

    // Find all non-overlapping matches
    std::vector<std::pair<int64_t, int64_t>> findAll(const char *text, size_t textLen) {
        NFASimulator sim(start, matchState, caseInsensitive_);
        return sim.findAllSinglePass(text, textLen, hasLazy_);
    }
};

// ============================================================
// Backtracking capture extractor (used only when $N is in replacement)
// ============================================================

// Per-group capture span: {start, end} or {-1, -1} if not matched.
// captures[0] = whole match, captures[i] = group i (1-based).
using CaptureVec = std::vector<std::pair<int64_t, int64_t>>;

class CaptureBacktracker {
public:
    CaptureBacktracker(NFAState *start, NFAState *matchState,
                       int groupCount, bool caseInsensitive)
        : start_(start), matchState_(matchState),
          groupCount_(groupCount), caseInsensitive_(caseInsensitive) {}

    // Extract capture positions for a known match span [matchStart, matchEnd).
    // caps[0] = whole match, caps[i] = group i, or {-1,-1} if unmatched.
    // Returns true on success; false if no path was found.
    bool extractCaptures(const char *text, size_t textLen,
                         int64_t matchStart, int64_t matchEnd,
                         CaptureVec &caps) {
        caps.assign(static_cast<size_t>(groupCount_) + 1, {-1, -1});
        // onPath tracks (state, pos) pairs on the current DFS path to detect
        // epsilon cycles (states that loop without consuming input).
        DFSPath onPath;
        bool ok = backtrack(start_, text, textLen, static_cast<size_t>(matchStart),
                            static_cast<size_t>(matchEnd), caps, onPath);
        if (!ok) {
            fprintf(stderr,
                    "regex warning: capture extraction found no valid path for match "
                    "[%lld, %lld); backreferences may expand to empty\n",
                    static_cast<long long>(matchStart),
                    static_cast<long long>(matchEnd));
        }
        // backtrack may set caps[0] via GroupOpen/GroupClose on a wrapping group;
        // always overwrite with the authoritative bounds from findAll.
        caps[0] = {matchStart, matchEnd};
        return ok;
    }

private:
    NFAState *start_;
    NFAState *matchState_;
    int groupCount_;
    bool caseInsensitive_;

    // (state pointer, text position) — used to detect epsilon cycles in the DFS.
    using DFSPath = std::set<std::pair<NFAState *, size_t>>;

    // Recursive backtracking. Returns true if we reach matchState_ at pos == matchEnd.
    // onPath contains all (state, pos) pairs currently on the recursion stack;
    // any repeat signals an epsilon cycle and is pruned immediately.
    bool backtrack(NFAState *s, const char *text, size_t textLen,
                   size_t pos, size_t matchEnd, CaptureVec &caps, DFSPath &onPath) {
        if (!s) return false;

        if (s == matchState_) {
            return pos == matchEnd;
        }

        // Epsilon-cycle guard: if this (state, pos) pair is already on the current
        // DFS path we would loop forever — prune this branch.
        auto key = std::make_pair(s, pos);
        if (!onPath.insert(key).second) return false;

        bool result = false;
        switch (s->kind) {
        case NFAState::Kind::Split: {
            result = backtrack(s->out1, text, textLen, pos, matchEnd, caps, onPath) ||
                     backtrack(s->out2, text, textLen, pos, matchEnd, caps, onPath);
            break;
        }
        case NFAState::Kind::GroupOpen: {
            int64_t saved = caps[static_cast<size_t>(s->groupIndex)].first;
            caps[static_cast<size_t>(s->groupIndex)].first = static_cast<int64_t>(pos);
            result = backtrack(s->out1, text, textLen, pos, matchEnd, caps, onPath);
            if (!result) caps[static_cast<size_t>(s->groupIndex)].first = saved;
            break;
        }
        case NFAState::Kind::GroupClose: {
            int64_t saved = caps[static_cast<size_t>(s->groupIndex)].second;
            caps[static_cast<size_t>(s->groupIndex)].second = static_cast<int64_t>(pos);
            result = backtrack(s->out1, text, textLen, pos, matchEnd, caps, onPath);
            if (!result) caps[static_cast<size_t>(s->groupIndex)].second = saved;
            break;
        }
        case NFAState::Kind::Anchor: {
            bool pass = (s->ch == '^') ? (pos == 0) : (pos == textLen);
            if (pass) result = backtrack(s->out1, text, textLen, pos, matchEnd, caps, onPath);
            break;
        }
        case NFAState::Kind::WordBoundary: {
            bool prevIsWord = (pos > 0) && nfaIsWordChar(text[pos - 1]);
            bool currIsWord = (pos < textLen) && nfaIsWordChar(text[pos]);
            bool atBoundary = (prevIsWord != currIsWord);
            if (atBoundary != s->negated)
                result = backtrack(s->out1, text, textLen, pos, matchEnd, caps, onPath);
            break;
        }
        default:
            // Char / Dot / CharClass: consume one character
            if (pos < textLen && nfaStateMatchesChar(s, text[pos], caseInsensitive_))
                result = backtrack(s->out1, text, textLen, pos + 1, matchEnd, caps, onPath);
            break;
        }

        onPath.erase(key);
        return result;
    }
};

// ============================================================
// Replacement string utilities
// ============================================================

// Returns true if the replacement string contains any $N or ${digits} backreference.
static bool hasBackreferences(const char *repl, size_t len) {
    for (size_t i = 0; i + 1 < len; ++i) {
        if (repl[i] == '$') {
            char next = repl[i + 1];
            if (next == '$') { ++i; continue; } // skip $$
            if (std::isdigit((unsigned char)next)) return true;
            if (next == '{') {
                // Only ${digits} counts — scan ahead for at least one digit + '}'
                size_t j = i + 2;
                while (j < len && std::isdigit((unsigned char)repl[j])) ++j;
                if (j > i + 2 && j < len && repl[j] == '}') return true;
                // else: malformed ${...}, not a backreference — keep scanning
            }
        }
    }
    return false;
}

// Expand $N / ${N} / $$ / $0 in replacement using the provided captures.
// Captures[0] = whole match, captures[i] = group i.
// Out-of-range or unmatched groups expand to empty string.
static std::string expandReplacement(const char *repl, size_t repLen,
                                     const char *text,
                                     const CaptureVec &caps) {
    std::string out;
    out.reserve(repLen);
    for (size_t i = 0; i < repLen; ++i) {
        if (repl[i] != '$') {
            out += repl[i];
            continue;
        }
        ++i;
        if (i >= repLen) {
            out += '$';
            break;
        }
        char next = repl[i];
        if (next == '$') {
            out += '$';
        } else if (next == '{') {
            // ${N} syntax for multi-digit groups
            ++i;
            size_t numStart = i;
            while (i < repLen && std::isdigit((unsigned char)repl[i])) ++i;
            if (i < repLen && repl[i] == '}' && i > numStart) {
                size_t idx = 0;
                bool overflow = false;
                for (size_t j = numStart; j < i; ++j) {
                    const size_t digit = static_cast<size_t>(repl[j] - '0');
                    if (idx > (std::numeric_limits<size_t>::max() - digit) / 10) {
                        overflow = true;
                        break;
                    }
                    idx = idx * 10 + digit;
                    if (idx >= caps.size()) break;
                }
                if (!overflow && idx < caps.size()) {
                    auto [s, e] = caps[idx];
                    if (s >= 0 && e >= s) out.append(text + s, static_cast<size_t>(e - s));
                }
                // if '}' missing or no digits, skip and consume what we have
            } else {
                // malformed ${...} — emit literally
                out += '$';
                out += '{';
                for (size_t j = numStart; j < i; ++j) out += repl[j];
                if (i < repLen && repl[i] == '}') {
                    out += '}';
                } else {
                    --i;
                }
            }
        } else if (std::isdigit((unsigned char)next)) {
            int idx = next - '0';
            if (idx >= 0 && static_cast<size_t>(idx) < caps.size()) {
                auto [s, e] = caps[static_cast<size_t>(idx)];
                if (s >= 0 && e >= s) out.append(text + s, static_cast<size_t>(e - s));
            }
            // out-of-range → empty string (intentional)
        } else {
            // $ followed by non-digit, non-{ → literal $
            out += '$';
            out += next;
        }
    }
    return out;
}

} // anonymous namespace

// ============================================================
// Public C API
// ============================================================

extern "C" {

int64_t __ry_regex_match(const char *pattern, int64_t patternLen,
                          const char *text,    int64_t textLen) {
    if (!pattern || !text) return 0;
    try {
        auto cr = CompiledRegex::compile(pattern, static_cast<size_t>(patternLen));
        return cr.fullMatch(text, static_cast<size_t>(textLen)) ? 1 : 0;
    } catch (const std::runtime_error &e) {
        setLastError("%s", e.what());
        return kRegexMatchError;
    }
}

int64_t __ry_regex_search(const char *pattern, int64_t patternLen,
                           const char *text,    int64_t textLen) {
    if (!pattern || !text) return -1;
    try {
        auto cr = CompiledRegex::compile(pattern, static_cast<size_t>(patternLen));
        auto result = cr.search(text, static_cast<size_t>(textLen));
        return result.first;
    } catch (const std::runtime_error &e) {
        setLastError("%s", e.what());
        return kRegexSearchError;
    }
}

int64_t __ry_regex_is_match(const char *pattern, int64_t patternLen,
                             const char *text,    int64_t textLen) {
    int64_t result = __ry_regex_search(pattern, patternLen, text, textLen);
    if (result == kRegexSearchError) return kRegexMatchError;
    return result >= 0 ? 1 : 0;
}

const char *__ry_regex_replace(const char *pattern,     int64_t patternLen,
                                const char *text,        int64_t textLen,
                                const char *replacement, int64_t replacementLen) {
    if (!pattern || !text || !replacement)
        return text ? dupString(text, static_cast<size_t>(textLen)) : dupString("", 0);
    try {
        auto cr = CompiledRegex::compile(pattern, static_cast<size_t>(patternLen));
        auto matches = cr.findAll(text, static_cast<size_t>(textLen));
        if (matches.empty())
            return dupString(text, static_cast<size_t>(textLen));

        size_t tLen = static_cast<size_t>(textLen);
        size_t repLen = static_cast<size_t>(replacementLen);
        bool needsCaptures = hasBackreferences(replacement, repLen);

        std::string result;
        result.reserve(tLen + matches.size() * repLen);
        size_t lastEnd = 0;

        std::unique_ptr<CaptureBacktracker> bt;
        if (needsCaptures) {
            bt = std::make_unique<CaptureBacktracker>(
                cr.start, cr.matchState, cr.groupCount(), cr.caseInsensitive_);
        }

        for (auto &[start, end] : matches) {
            result.append(text + lastEnd, static_cast<size_t>(start) - lastEnd);
            if (needsCaptures) {
                CaptureVec caps;
                bt->extractCaptures(text, tLen, start, end, caps);
                result += expandReplacement(replacement, repLen, text, caps);
            } else {
                result.append(replacement, repLen);
            }
            lastEnd = static_cast<size_t>(end);
        }
        result.append(text + lastEnd, tLen - lastEnd);
        return dupString(result.c_str(), result.size());
    } catch (const std::runtime_error &e) {
        setLastError("%s", e.what());
        return nullptr;
    }
}

void *__ry_regex_split(const char *pattern, int64_t patternLen,
                        const char *text,    int64_t textLen) {
    if (!pattern || !text) return makeStringList({});
    try {
        auto cr = CompiledRegex::compile(pattern, static_cast<size_t>(patternLen));
        auto matches = cr.findAll(text, static_cast<size_t>(textLen));

        size_t tLen = static_cast<size_t>(textLen);
        std::vector<std::string> parts;
        size_t lastEnd = 0;
        for (auto &[start, end] : matches) {
            parts.emplace_back(text + lastEnd, static_cast<size_t>(start) - lastEnd);
            lastEnd = static_cast<size_t>(end);
        }
        parts.emplace_back(text + lastEnd, tLen - lastEnd);
        return makeStringList(parts);
    } catch (const std::runtime_error &e) {
        setLastError("%s", e.what());
        return nullptr;
    }
}

void *__ry_regex_find_all(const char *pattern, int64_t patternLen,
                           const char *text,    int64_t textLen) {
    if (!pattern || !text) return makeMatchList({});
    try {
        auto cr = CompiledRegex::compile(pattern, static_cast<size_t>(patternLen));
        auto matches = cr.findAll(text, static_cast<size_t>(textLen));

        size_t tLen = static_cast<size_t>(textLen);
        int numGroups = cr.groupCount();

        std::unique_ptr<CaptureBacktracker> bt;
        if (numGroups > 0) {
            bt = std::make_unique<CaptureBacktracker>(
                cr.start, cr.matchState, numGroups, cr.caseInsensitive_);
        }

        std::vector<MatchData> items;
        items.reserve(matches.size());
        CaptureVec caps;
        for (auto &[start, end] : matches) {
            MatchData md;
            md.full = dupString(text + start, static_cast<size_t>(end - start));
            if (numGroups > 0) {
                bt->extractCaptures(text, tLen, start, end, caps);
                std::vector<std::string> groupStrs;
                groupStrs.reserve(static_cast<size_t>(numGroups));
                for (int g = 1; g <= numGroups; ++g) {
                    auto [gs, ge] = caps[static_cast<size_t>(g)];
                    if (gs >= 0 && ge >= gs)
                        groupStrs.emplace_back(text + gs,
                                               static_cast<size_t>(ge - gs));
                    else
                        groupStrs.emplace_back();
                }
                md.groups = makeStringList(groupStrs);
            } else {
                md.groups = makeStringList({});
            }
            items.push_back(md);
        }
        return makeMatchList(items);
    } catch (const std::runtime_error &e) {
        setLastError("%s", e.what());
        return nullptr;
    }
}

} // extern "C"

} // namespace ry
