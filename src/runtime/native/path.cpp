#include "ry/runtime/core/error.hpp"
#include "ry/runtime/core/string.hpp"

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
// Returns nullptr (+ setLastError) if either argument contains an embedded NUL.
static char *join2_impl(const char *a, const char *b) {
    if (hasEmbeddedNul(a)) {
        setLastError("path.join: argument contains an embedded NUL byte");
        return nullptr;
    }
    if (hasEmbeddedNul(b)) {
        setLastError("path.join: argument contains an embedded NUL byte");
        return nullptr;
    }
    if (!a || !*a) return makeString(b ? b : "", b ? static_cast<size_t>(stringByteLen(b)) : 0);
    if (!b || !*b) return makeString(a, static_cast<size_t>(stringByteLen(a)));
    if (b[0] == '/') return makeString(b, static_cast<size_t>(stringByteLen(b)));

    size_t a_len = strip_trailing(a, static_cast<size_t>(stringByteLen(a)));
    size_t b_len = static_cast<size_t>(stringByteLen(b));

    // If a ends with '/' (root path), don't insert an extra separator
    bool need_sep = (a[a_len - 1] != '/');
    size_t out_len = a_len + (need_sep ? 1 : 0) + b_len;
    char *out = makeStringUninit(out_len);
    memcpy(out, a, a_len); // NOLINT(bugprone-not-null-terminated-result) — makeStringUninit appends NUL at out[out_len]
    if (need_sep)
        out[a_len] = '/';
    memcpy(out + a_len + (need_sep ? 1 : 0), b, b_len); // NOLINT(bugprone-not-null-terminated-result) — NUL already at out[out_len]
    // NUL at out[out_len] already written by makeStringUninit
    return out;
}

// ===== Public API =====

extern "C" const char *__ry_path_join2(const char *a, const char *b) {
    return join2_impl(a, b);
}

extern "C" const char *__ry_path_join3(const char *a, const char *b, const char *c) {
    char *ab = join2_impl(a, b);
    if (!ab) return nullptr;  // propagate NUL error from first pair
    char *result = join2_impl(ab, c);
    freeStringSlot(ab);
    return result;
}

extern "C" const char *__ry_path_join4(const char *a, const char *b, const char *c, const char *d) {
    char *ab = join2_impl(a, b);
    if (!ab) return nullptr;  // propagate NUL error from first pair
    char *abc = join2_impl(ab, c);
    freeStringSlot(ab);
    if (!abc) return nullptr;  // propagate NUL error from second pair
    char *result = join2_impl(abc, d);
    freeStringSlot(abc);
    return result;
}

extern "C" const char *__ry_path_basename(const char *p) {
    if (!p || !*p) return makeString("", 0);
    if (hasEmbeddedNul(p)) {
        setLastError("path.basename: argument contains an embedded NUL byte");
        return nullptr;
    }

    size_t len = strip_trailing(p, static_cast<size_t>(stringByteLen(p)));
    if (len == 1 && p[0] == '/') return makeString("", 0);

    const char *end = p + len;
    const char *base = find_base(p, len);

    // If base points to a '/', skip it
    if (*base == '/') base++;

    return makeString(base, (size_t)(end - base));
}

extern "C" const char *__ry_path_dirname(const char *p) {
    if (!p || !*p) return makeString(".", 1);
    if (hasEmbeddedNul(p)) {
        setLastError("path.dirname: argument contains an embedded NUL byte");
        return nullptr;
    }

    size_t len = strip_trailing(p, static_cast<size_t>(stringByteLen(p)));

    // Find last slash
    const char *slash = p + len - 1;
    while (slash > p && *slash != '/')
        slash--;

    if (slash == p) {
        return *slash == '/' ? makeString("/", 1) : makeString(".", 1);
    }

    // Strip trailing slashes from dirname portion
    const char *dir_end = slash;
    while (dir_end > p && *(dir_end - 1) == '/')
        dir_end--;

    size_t dir_len = (size_t)(dir_end - p);
    if (dir_len == 0) return makeString("/", 1);

    return makeString(p, dir_len);
}

extern "C" const char *__ry_path_ext(const char *p) {
    if (!p || !*p) return makeString("", 0);
    if (hasEmbeddedNul(p)) {
        setLastError("path.ext: argument contains an embedded NUL byte");
        return nullptr;
    }

    size_t len = strip_trailing(p, static_cast<size_t>(stringByteLen(p)));
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
    if (!dot || dot == base) return makeString("", 0);

    return makeString(dot, (size_t)(end - dot));
}

extern "C" const char *__ry_path_resolve(const char *p) {
    if (!p || !*p) {
        setLastError("cannot resolve path: empty path");
        return nullptr;
    }
    if (hasEmbeddedNul(p)) {
        setLastError("path.resolve: argument contains an embedded NUL byte");
        return nullptr;
    }

    char *resolved = realpath(p, nullptr);
    if (!resolved) {
        setLastError("cannot resolve path '%s': %s", p, strerror(errno));
        return nullptr;
    }
    const char *result = makeString(resolved, strlen(resolved));
    free(resolved);
    return result;
}

extern "C" int64_t __ry_path_isAbsolute(const char *p) {
    if (!p) return 0;
    return p[0] == '/' ? 1 : 0;
}

} // namespace ry
