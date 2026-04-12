#include "ry/runtime_alloc.hpp"
#include "ry/runtime_error.hpp"

#include <cerrno>
#include <cstdint>
#include <cstdlib>
#include <cstring>


namespace ry {

DEFINE_LAST_ERROR(path)

// ===== Shared helpers =====

// Strip trailing slashes, returning the effective length.
// Root "/" returns 1.
static size_t strip_trailing(const char *p, size_t len) {
    while (len > 1 && p[len - 1] == '/')
        len--;
    return len;
}

// Find the start of the basename component within p[0..len).
// Returns pointer to the first char of the basename.
static const char *find_base(const char *p, size_t len) {
    if (len == 0) return p;
    const char *s = p + len - 1;
    while (s > p && *(s - 1) != '/')
        s--;
    return s;
}

// ===== Internal =====

// If b is absolute, returns a copy of b.
// Otherwise concatenates a + "/" + b, normalizing double slashes.
static char *join2_impl(const char *a, const char *b) {
    if (!a || !*a) return checked_strdup(b ? b : "");
    if (!b || !*b) return checked_strdup(a);
    if (b[0] == '/') return checked_strdup(b);

    size_t a_len = strip_trailing(a, strlen(a));
    size_t b_len = strlen(b);

    // If a ends with '/' (root path), don't insert an extra separator
    bool need_sep = (a[a_len - 1] != '/');
    size_t out_len = a_len + (need_sep ? 1 : 0) + b_len;
    char *out = (char *)checked_malloc(out_len + 1);
    memcpy(out, a, a_len);
    if (need_sep)
        out[a_len] = '/';
    memcpy(out + a_len + (need_sep ? 1 : 0), b, b_len);
    out[out_len] = '\0';
    return out;
}

// ===== Public API =====

extern "C" const char *__ry_path_join2(const char *a, const char *b) {
    return join2_impl(a, b);
}

extern "C" const char *__ry_path_join3(const char *a, const char *b, const char *c) {
    char *ab = join2_impl(a, b);
    char *result = join2_impl(ab, c);
    free(ab);
    return result;
}

extern "C" const char *__ry_path_join4(const char *a, const char *b, const char *c, const char *d) {
    char *ab = join2_impl(a, b);
    char *abc = join2_impl(ab, c);
    free(ab);
    char *result = join2_impl(abc, d);
    free(abc);
    return result;
}

extern "C" const char *__ry_path_basename(const char *p) {
    if (!p || !*p) return checked_strdup("");

    size_t len = strip_trailing(p, strlen(p));
    if (len == 1 && p[0] == '/') return checked_strdup("");

    const char *end = p + len;
    const char *base = find_base(p, len);

    // If base points to a '/', skip it
    if (*base == '/') base++;

    return checked_strndup(base, (size_t)(end - base));
}

extern "C" const char *__ry_path_dirname(const char *p) {
    if (!p || !*p) return checked_strdup(".");

    size_t len = strip_trailing(p, strlen(p));

    // Find last slash
    const char *slash = p + len - 1;
    while (slash > p && *slash != '/')
        slash--;

    if (slash == p) {
        return checked_strdup(*slash == '/' ? "/" : ".");
    }

    // Strip trailing slashes from dirname portion
    const char *dir_end = slash;
    while (dir_end > p && *(dir_end - 1) == '/')
        dir_end--;

    size_t dir_len = (size_t)(dir_end - p);
    if (dir_len == 0) return checked_strdup("/");

    return checked_strndup(p, dir_len);
}

extern "C" const char *__ry_path_extension(const char *p) {
    if (!p || !*p) return checked_strdup("");

    size_t len = strip_trailing(p, strlen(p));
    const char *end = p + len;
    const char *base = find_base(p, len);
    size_t base_len = (size_t)(end - base);

    // Scan backward from end for the last dot
    const char *dot = nullptr;
    for (size_t i = 0; i < base_len; i++) {
        if (base[i] == '.')
            dot = base + i;
    }

    // No dot, or dot is the first character (hidden file like .gitignore)
    if (!dot || dot == base) return checked_strdup("");

    return checked_strndup(dot, (size_t)(end - dot));
}

extern "C" const char *__ry_path_resolve(const char *p) {
    if (!p || !*p) {
        setLastError("cannot resolve path: empty path");
        return nullptr;
    }

    char *resolved = realpath(p, nullptr);
    if (!resolved) {
        setLastError("cannot resolve path '%s': %s", p, strerror(errno));
        return nullptr;
    }
    return resolved;
}

extern "C" int64_t __ry_path_is_absolute(const char *p) {
    if (!p) return 0;
    return p[0] == '/' ? 1 : 0;
}

} // namespace ry
