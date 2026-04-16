#include "ry/runtime_string.hpp"
#include "ry/runtime_list.hpp"
#include <cctype>
#include <cstdint>
#include <cstring>

// Forward declaration: defined in runtime_utf8.cpp (C linkage, no namespace).
// Used by __ry_str_split when delimLen == 0.
extern "C" void *__ry_split_chars(const char *s, int64_t byte_len);

// ── StringHeader C-level exports called from JIT-compiled IR ─────────────
//
// Codegen emits CreateCall to these symbols when it needs to allocate Ry str
// values at runtime (string concat, repeat, f-string construction, etc.).
// C++ runtime code that returns str (runtime_io, runtime_path, ...) uses the
// inline ry::makeString / ry::makeStringUninit helpers from runtime_string.hpp
// directly instead of going through these symbols.

extern "C" {

// Allocate a StringHeader-managed string of `byte_len` bytes, uninitialized
// data (except for the null terminator at data[byte_len]).  Returns the handle
// (data pointer).  Called by codegen for string concat / repeat.
char *__ry_string_make_uninit(int64_t byte_len) {
    if (byte_len < 0) byte_len = 0;
    return ry::makeStringUninit(static_cast<size_t>(byte_len));
}

// Allocate a StringHeader-managed string by copying `byte_len` bytes from
// `src` (NUL bytes included).  Returns the handle.
char *__ry_string_make(const char *src, int64_t byte_len) {
    if (byte_len < 0) byte_len = 0;
    return ry::makeString(src, static_cast<size_t>(byte_len));
}

// Return the byte_len stored in the StringHeader for `handle`.
int64_t __ry_string_len(const char *handle) {
    return ry::stringByteLen(handle);
}

// NUL-safe string comparison: compares first min(la,lb) bytes with memcmp,
// then breaks ties by byte length.  Semantics match strcmp for NUL-free strings.
// Called by codegen for str == / != / < / <= / > / >= operators.
int32_t __ry_str_cmp(const char *a, int64_t la, const char *b, int64_t lb) {
    int64_t min_len = la < lb ? la : lb;
    int r = memcmp(a, b, static_cast<size_t>(min_len));
    if (r != 0) return r;
    if (la > lb) return 1;
    if (la < lb) return -1;
    return 0;
}

// ASCII case-insensitive fixed-length comparison: returns 1 if all len bytes
// of a and b match under tolower, 0 otherwise.  Shared by starts_with and ends_with.
static int32_t mem_eq_icase(const char *a, const char *b, int64_t len) {
    for (int64_t i = 0; i < len; ++i) {
        if (tolower(static_cast<unsigned char>(a[i])) !=
            tolower(static_cast<unsigned char>(b[i])))
            return 0;
    }
    return 1;
}

// NUL-safe starts_with: returns 1 if the first pl bytes of s match the pl bytes
// of p (case-sensitive or ASCII-folded), 0 otherwise.
// Called by codegen for starts_with() / _starts_with().
int32_t __ry_str_starts_with(const char *s, int64_t sl, const char *p, int64_t pl,
                              int32_t ignore_case) {
    if (pl > sl) return 0;
    if (ignore_case) return mem_eq_icase(s, p, pl);
    return memcmp(s, p, static_cast<size_t>(pl)) == 0 ? 1 : 0;
}

// NUL-safe ends_with: returns 1 if the last pl bytes of s match the pl bytes
// of suffix (case-sensitive or ASCII-folded), 0 otherwise.
// Called by codegen for ends_with() / _ends_with().
int32_t __ry_str_ends_with(const char *s, int64_t sl, const char *suffix, int64_t pl,
                            int32_t ignore_case) {
    if (pl > sl) return 0;
    const char *tail = s + (sl - pl);
    if (ignore_case) return mem_eq_icase(tail, suffix, pl);
    return memcmp(tail, suffix, static_cast<size_t>(pl)) == 0 ? 1 : 0;
}

// NUL-safe find: returns the byte offset of the first occurrence of needle
// (length nl) within haystack (length hl), or -1 if not found.
// Empty needle (nl == 0) returns 0, matching strstr() semantics.
// Case-insensitive path uses a naive O(hl*nl) ASCII-tolower loop (matching the
// existing strcasestr() behaviour which is also ASCII-only).
// Called by codegen for contains() and find().
int64_t __ry_str_find_byte(const char *s, int64_t hl, const char *p, int64_t nl,
                            int32_t ignore_case) {
    if (nl == 0) return 0;
    if (nl > hl) return -1;
    if (ignore_case) {
        for (int64_t i = 0; i <= hl - nl; ++i) {
            bool match = true;
            for (int64_t j = 0; j < nl; ++j) {
                if (static_cast<unsigned char>(tolower(static_cast<unsigned char>(s[i + j]))) !=
                    static_cast<unsigned char>(tolower(static_cast<unsigned char>(p[j])))) {
                    match = false;
                    break;
                }
            }
            if (match) return i;
        }
        return -1;
    }
    const void *found = memmem(s, static_cast<size_t>(hl), p, static_cast<size_t>(nl));
    if (!found) return -1;
    return static_cast<int64_t>(static_cast<const char *>(found) - s);
}

// NUL-safe replace: replaces every occurrence of oldStr (length oldLen) in s
// (length sLen) with newStr (length newLen).  All three arguments may contain
// embedded NUL bytes.  Empty needle (oldLen == 0) returns a fresh copy of s
// unchanged (#802); fresh copy ensures caller always owns the result.
// Called by codegen for replace(s, old, new).
const char *__ry_str_replace(const char *s, int64_t sLen,
                              const char *oldStr, int64_t oldLen,
                              const char *newStr, int64_t newLen) {
    if (oldLen == 0)
        return ry::makeString(s, static_cast<size_t>(sLen));

    int64_t count = 0;
    const char *cur = s;
    int64_t remain = sLen;
    while (remain >= oldLen) {
        const void *match = memmem(cur, static_cast<size_t>(remain), oldStr,
                                   static_cast<size_t>(oldLen));
        if (!match) break;
        ++count;
        int64_t advance = static_cast<const char *>(match) - cur + oldLen;
        cur += advance;
        remain -= advance;
    }

    if (count == 0)
        return ry::makeString(s, static_cast<size_t>(sLen));

    // Guard against int64_t overflow when newLen > oldLen.
    int64_t delta = newLen - oldLen;
    int64_t resultLen;
    if (delta > 0 && count > (INT64_MAX - sLen) / delta)
        resultLen = INT64_MAX;  // triggers OOM abort in makeStringUninit
    else
        resultLen = sLen + count * delta;
    char *out = ry::makeStringUninit(static_cast<size_t>(resultLen));

    cur = s;
    remain = sLen;
    char *dst = out;
    while (remain >= oldLen) {
        const void *match = memmem(cur, static_cast<size_t>(remain), oldStr,
                                   static_cast<size_t>(oldLen));
        if (!match) break;
        int64_t prefixLen = static_cast<const char *>(match) - cur;
        memcpy(dst, cur, static_cast<size_t>(prefixLen));
        dst += prefixLen;
        memcpy(dst, newStr, static_cast<size_t>(newLen));
        dst += newLen;
        cur = static_cast<const char *>(match) + oldLen;
        remain -= prefixLen + oldLen;
    }
    memcpy(dst, cur, static_cast<size_t>(remain));

    return out;
}

// NUL-safe split: splits s (length sLen) by delim (length delimLen) and
// returns a ListHeader* (List<str>) of the resulting segments.  All arguments
// may contain embedded NUL bytes.  Empty delimiter delegates to
// __ry_split_chars (one codepoint per element).
// Called by codegen for split(s, delim) when delim is non-empty.
void *__ry_str_split(const char *s, int64_t sLen,
                     const char *delim, int64_t delimLen) {
    if (delimLen == 0)
        return __ry_split_chars(s, sLen);

    // Pass 1: count occurrences of delim in s to determine element count.
    int64_t occurrences = 0;
    const char *cur = s;
    int64_t remain = sLen;
    while (remain >= delimLen) {
        const void *m = memmem(cur, static_cast<size_t>(remain),
                               delim, static_cast<size_t>(delimLen));
        if (!m) break;
        ++occurrences;
        int64_t advance = static_cast<const char *>(m) - cur + delimLen;
        cur += advance;
        remain -= advance;
    }

    int64_t elemCount = occurrences + 1;
    auto *header = static_cast<ry::ListHeader *>(ry::arc_alloc(sizeof(ry::ListHeader)));
    header->len = elemCount;
    header->cap = elemCount;
    header->data = static_cast<char **>(ry::checked_array_malloc(
        static_cast<size_t>(elemCount), sizeof(char *)));

    // Pass 2: fill element strings.
    cur = s;
    remain = sLen;
    int64_t idx = 0;
    while (remain >= delimLen) {
        const void *m = memmem(cur, static_cast<size_t>(remain),
                               delim, static_cast<size_t>(delimLen));
        if (!m) break;
        int64_t segLen = static_cast<const char *>(m) - cur;
        header->data[idx++] = ry::dupString(cur, static_cast<size_t>(segLen));
        cur = static_cast<const char *>(m) + delimLen;
        remain -= segLen + delimLen;
    }
    // Tail segment (may be empty if s ends with delim).
    header->data[idx] = ry::dupString(cur, static_cast<size_t>(remain));

    return header;
}

} // extern "C"
