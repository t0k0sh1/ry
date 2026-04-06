#pragma once

#include <cstdio>
#include <cstdlib>
#include <cstring>

namespace ry {

[[noreturn]] inline void oom_abort(size_t n) {
    fprintf(stderr, "ry: out of memory (requested %zu bytes)\n", n);
    abort();
}

inline void *checked_malloc(size_t n) {
    void *p = malloc(n);
    if (!p && n > 0) oom_abort(n);
    return p;
}

inline void *checked_realloc(void *ptr, size_t n) {
    void *p = realloc(ptr, n);
    if (!p && n > 0) oom_abort(n);
    return p;
}

inline char *checked_strdup(const char *s) {
    if (!s) return nullptr;
    char *p = strdup(s);
    if (!p) oom_abort(strlen(s) + 1);
    return p;
}

inline char *checked_strndup(const char *s, size_t n) {
    if (!s) return nullptr;
    char *p = strndup(s, n);
    if (!p) oom_abort(n + 1);
    return p;
}

inline char *checked_memdup(const void *src, size_t len) {
    char *p = (char *)checked_malloc(len + 1);
    memcpy(p, src, len);
    p[len] = '\0';
    return p;
}

// Overflow-safe array allocation: aborts if count * elem_size overflows.
inline void *checked_array_malloc(size_t count, size_t elem_size) {
    size_t total;
    if (__builtin_mul_overflow(count, elem_size, &total)) {
        oom_abort(SIZE_MAX);
    }
    return checked_malloc(total);
}

// Overflow-safe array reallocation: aborts if count * elem_size overflows.
inline void *checked_array_realloc(void *ptr, size_t count, size_t elem_size) {
    size_t total;
    if (__builtin_mul_overflow(count, elem_size, &total)) {
        oom_abort(SIZE_MAX);
    }
    return checked_realloc(ptr, total);
}

} // namespace ry
