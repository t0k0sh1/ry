#include "ry/runtime_string.hpp"
#include <cctype>
#include <cstdint>
#include <cstring>

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

} // extern "C"
