#pragma once

#include <cstdint>


namespace ry {

extern "C" {

// Full match: returns 1 if entire text matches pattern, 0 otherwise.
// patternLen / textLen are the byte lengths (NUL bytes included).
int64_t __ry_regex_match(const char *pattern, int64_t patternLen,
                          const char *text,    int64_t textLen);

// Search: returns char-index of first match start, or -1 if not found.
int64_t __ry_regex_search(const char *pattern, int64_t patternLen,
                           const char *text,    int64_t textLen);

// Like __ry_regex_match but unanchored (searches anywhere in text), not full-string.
int64_t __ry_regex_is_match(const char *pattern, int64_t patternLen,
                             const char *text,    int64_t textLen);

// Replace all occurrences: returns a StringHeader-managed string.
const char *__ry_regex_replace(const char *pattern,     int64_t patternLen,
                                const char *text,        int64_t textLen,
                                const char *replacement, int64_t replacementLen);

// Split by pattern: returns ListHeader* (List<str>)
void *__ry_regex_split(const char *pattern, int64_t patternLen,
                        const char *text,    int64_t textLen);

// Find all non-overlapping matches: returns ListHeader* (List<Match>)
void *__ry_regex_find_all(const char *pattern, int64_t patternLen,
                           const char *text,    int64_t textLen);

}

} // namespace ry
