#pragma once

#include <cstdint>


namespace ry {

extern "C" {

// Full match: returns 1 if entire text matches pattern, 0 otherwise
int64_t __ry_regex_match(const char *pattern, const char *text);

// Search: returns start index of first match, or -1 if not found
int64_t __ry_regex_search(const char *pattern, const char *text);

// Replace all occurrences: returns malloc'd string
const char *__ry_regex_replace(const char *pattern, const char *text,
                                const char *replacement);

// Split by pattern: returns ListHeader* (List<str>)
void *__ry_regex_split(const char *pattern, const char *text);

// Find all non-overlapping matches: returns ListHeader* (List<str>)
void *__ry_regex_find_all(const char *pattern, const char *text);

}

} // namespace ry
